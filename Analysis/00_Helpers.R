# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Shared helpers
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Sourced by the analysis scripts after their library block and before any model is
# fit. Requires dplyr and stringr (both come with tidyverse).
#
# prepare_analysis_data() is the single entry point: it repairs birth_fips and the
# variables derived from it, then derives migration status. Both steps are idempotent
# and derive from the FIPS codes rather than from stored labels, so they give the same
# answer whether or not the cleaned CSVs have been rebuilt since the fixes in
# Code/01_Numident.R and Analysis/01_Data_Cleaning.R.

# ------------------------------- birth_fips repair -------------------------------
# A county FIPS is exactly five digits: two for the state, three for the county. The
# raw CenSoc geography file stores them that way, correctly zero-padded and quoted
# ("01105"). They lose their leading zeros at ingest, because fread type-guesses those
# quoted strings to integer (01105 -> 1105). Both cleaning scripts compensated with
#
#   birth_fips = case_when(state %in% c("1","2","4","5","6","8","9") ~ paste0("0", birth_fips),
#                          ...                                      ~ as.character(birth_fips))
#
# where `state` comes from death_fips. That is the right state for death_fips, and the
# wrong one for birth_fips: whether a BIRTH code needs a zero is a fact about the birth
# state. So the code is padded on the death state's behalf, which leaves three groups:
#
#   4 chars ("1001")   born in a low-numbered state, died in a high-numbered one:
#                      the zero it needed was never added.
#   6 chars ("026163") born in a high-numbered state, died in a low-numbered one:
#                      a zero it did not need was prepended.
#   5 chars            correct.
#
# The corruption adds or removes exactly one leading zero from a five-character code,
# so it is losslessly reversible, and this repair is what reverses it. That matters
# because STATEFIP_b is str_sub(birth_fips, 1, 2): an unrepaired "1001" files an
# Alabama birth under state 10, Delaware, silently and without erroring. 13.0% of Black
# and 12.0% of White rows are affected.
#
# The repair is verified against `bpl`, the IPUMS birthplace code, which never passed
# through the padding logic: bpl %/% 100 is the birth state FIPS. Agreement is checked
# on every row (see check_birth_fips() below), so a silent recurrence fails loudly.
#
# Missing birth counties arrive as the literal string "0NA" -- paste0("0", NA) -- and
# become NA here.

# Valid state FIPS codes: 01-56, excluding the unassigned 03, 07, 14, 43 and 52.
VALID_STATEFIP <- sprintf("%02d", setdiff(1:56, c(3, 7, 14, 43, 52)))

# Census South, as used for the birth- and death-side indicators in 01_Data_Cleaning.R.
SOUTH_STATEFIP <- as.character(c("01", "05", "10", 11, 12, 13, 21, 22, 24, 28, 37,
                                 40, 45, 47, 48, 51, 54))

repair_birth_fips <- function(d) {
  d %>% dplyr::mutate(
    birth_fips = dplyr::case_when(
      is.na(birth_fips)                                    ~ NA_character_,
      stringr::str_detect(as.character(birth_fips), "NA")  ~ NA_character_,
      nchar(as.character(birth_fips)) == 4                 ~ paste0("0", as.character(birth_fips)),
      nchar(as.character(birth_fips)) == 6 &
        stringr::str_starts(as.character(birth_fips), "0") ~ stringr::str_sub(as.character(birth_fips), 2),
      TRUE                                                 ~ as.character(birth_fips)
    ),
    # death_fips is deliberately left alone. It is correct already -- its own padding
    # branch keys off the death state, which is the right state for it -- and it is a
    # join key against mechanism.csv. Rewriting it to a padded character column here
    # would break that join on a type mismatch, since the other side is read as a
    # number. add_migration_status() pads a copy for its comparison instead.
    STATEFIP_b     = stringr::str_sub(birth_fips, 1, 2),
    south_sample_b = dplyr::if_else(STATEFIP_b %in% SOUTH_STATEFIP, 1, NA_real_),
    born_in_south  = factor(
      dplyr::if_else(south_sample_b == 1 & !is.na(south_sample_b),
                     "Born South", "Not Born South"),
      levels = c("Not Born South", "Born South")
    )
  )
}

# Fails loudly rather than letting a bad code through as a valid-looking state.
check_birth_fips <- function(d, label = "") {
  b  <- d$birth_fips
  ok <- is.na(b) | nchar(b) == 5
  if (!all(ok)) {
    stop(sprintf("%s: %d birth_fips are neither NA nor 5 characters (e.g. %s)",
                 label, sum(!ok), paste(utils::head(unique(b[!ok]), 3), collapse = ", ")))
  }
  bad_state <- !is.na(d$STATEFIP_b) & !(d$STATEFIP_b %in% VALID_STATEFIP)
  if (any(bad_state)) {
    stop(sprintf("%s: %d rows have a STATEFIP_b outside the valid state set (e.g. %s)",
                 label, sum(bad_state),
                 paste(utils::head(unique(d$STATEFIP_b[bad_state]), 3), collapse = ", ")))
  }
  # Independent confirmation from the IPUMS birthplace code, where it is available.
  if ("bpl" %in% names(d)) {
    cmp <- !is.na(d$STATEFIP_b) & !is.na(d$bpl)
    disagree <- sum(as.integer(d$STATEFIP_b[cmp]) != d$bpl[cmp] %/% 100)
    if (disagree > 0) {
      stop(sprintf("%s: STATEFIP_b disagrees with bpl %%/%% 100 on %d of %d rows",
                   label, disagree, sum(cmp)))
    }
    message(sprintf("%s: birth_fips repair agrees with bpl on all %d comparable rows",
                    label, sum(cmp)))
  }
  invisible(d)
}

# ------------------------------- Migration status -------------------------------
# Recomputes `migrated` from birth_fips and death_fips, adds a `mover` factor, and
# drops anyone whose birth county is unrecorded.
#
# Why this is derived here rather than taken as given: `migrated` as written by
# 01_Data_Cleaning.R was
#   ifelse(as.character(birth_fips) == death_fips, "Migrated", "Did Not Migrate")
# which labelled people who died in their BIRTH county "Migrated" -- the two labels
# were swapped. The cleaning script has since been corrected, but the cleaned CSVs on
# disk may predate that fix, so every script that reads them recomputes it here.
#
# Migration status is undefined for people with no recorded birth county, so they are
# dropped rather than given a third category: every script that sources this helper
# therefore works from a sample in which birth county is always observed, and sample
# sizes agree across the descriptive, main, and migration-status tables. In db/dw they
# are 2,022 Black and 32,848 White rows.
#
# This runs after repair_birth_fips(), so both codes are already five characters. The
# split is unaffected by the repair either way: birth and death FIPS took the same
# padding branch whenever they were the same county, so no stayer was ever split apart,
# and the repair cannot manufacture one (a 6-character birth code only arises when the
# death county is in a low-numbered state and the birth county is not).
add_migration_status <- function(d) {
  d %>% dplyr::mutate(
    mover = dplyr::case_when(
      is.na(birth_fips) | is.na(death_fips) ~ NA_character_,
      birth_fips ==
        stringr::str_pad(as.character(death_fips), 5, pad = "0") ~ "Stayer",
      TRUE                                  ~ "Mover"
    ),
    mover = factor(mover, levels = c("Stayer", "Mover")),
    # "Did Not Migrate" stays the reference level so the coefficient keeps the name
    # `migratedMigrated` that every existing coef_map already refers to.
    migrated = factor(
      dplyr::if_else(mover == "Mover", "Migrated", "Did Not Migrate"),
      levels = c("Did Not Migrate", "Migrated")
    )
  ) %>%
    dplyr::filter(!is.na(mover))
}

# ------------------------------- Entry point -------------------------------
# What every analysis script calls on each data frame it reads.
prepare_analysis_data <- function(d, label = "") {
  d %>%
    repair_birth_fips() %>%
    check_birth_fips(label = label) %>%
    add_migration_status()
}

# ------------------------------- Shared axis range cache -------------------------------
# The main-estimate figures (OLS, IV-all, mechanism, migration status, birth-cohort,
# military service) are meant to share one axis so their vertical extents are directly
# comparable when read side by side in the manuscript. 00_run_all.R clears the R
# environment between scripts, so each figure's own estimate range is cached to disk;
# a figure built in an earlier script can then pick up the range of one built in a
# later script (and vice versa) once every script in the group has run at least once.
axis_cache_dir <- here::here("Data", "_Cleaned", "axis_cache")
if (!dir.exists(axis_cache_dir)) dir.create(axis_cache_dir, recursive = TRUE)

# Cache the [min(lo), max(hi)] of one figure's estimate interval, in whatever units
# that figure plots on its shared axis (years of life per 10-point rise in D for all
# of the pointrange figures; see the *10 rescale in the mechanism plot).
cache_estimate_range <- function(name, lo, hi) {
  saveRDS(c(lo = min(lo, na.rm = TRUE), hi = max(hi, na.rm = TRUE)),
          file.path(axis_cache_dir, paste0(name, ".rds")))
}

# Union the cached ranges for the named figures into one padded axis range. Returns
# NULL (i.e. auto-scale) until every named figure has cached at least once -- expected
# on a first run through a single script -- so re-source the analysis scripts a second
# time (or re-run 00_run_all.R) to have every figure in the group converge on the same
# final axis.
shared_axis_range <- function(names, pad = 0.06) {
  files <- file.path(axis_cache_dir, paste0(names, ".rds"))
  if (!all(file.exists(files))) return(NULL)
  vals    <- unlist(lapply(files, readRDS))
  rng     <- range(vals, na.rm = TRUE)
  pad_amt <- diff(rng) * pad
  c(rng[1] - pad_amt, rng[2] + pad_amt)
}

# Figures synced onto one shared "Change in Life Expectancy" axis (mechanism plot
# included via its *10-rescaled x-axis; see 04_Regression_Models.R).
MAIN_ESTIMATE_FIGS <- c("ols", "iv_all", "mechanism", "mig3", "bcohort", "military")
