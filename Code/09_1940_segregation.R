#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Title: 1940 County Segregation from Enumeration Districts
# Thelonious Goerz
# Method: Duncan & Duncan (1955) D, enumeration districts within county
# Data:   IPUMS 1940 100% full count (usa_00021)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ===========================================================
# WHAT THIS PRODUCES
# ===========================================================
# Black/White dissimilarity for the county an individual lived in in 1940, using
# enumeration districts (EDs) as the sub-county unit. This is the 1940 analogue of
# `county_dism` in Code/03_county_data.R, which is computed from NHGIS tract counts for
# the county of DEATH in 1980/1990/2000. The formula here is identical; only the unit
# (ED rather than tract) and the source (full-count microdata rather than NHGIS
# aggregates) differ.
#
# INPUT: Data/Census/usa_00021.dat.gz + usa_00021.ddi.xml
#   IPUMS USA 1940 100% database (SAMPLE 194002), fixed width, 100 bytes/record,
#   ~132M person records. Variables in the extract:
#     YEAR SAMPLE SERIAL HHWT STATEFIP COUNTYICP GQ ENUMDIST PERNUM PERWT RACE RACED
#     VERSIONHIST HISTID
#
#   Per the IPUMS documentation for ENUMDIST: "In 1930 and 1940, enumeration districts
#   are unique by county within states. [...] users must read ENUMDIST with one of the
#   STATE variables (STATEICP or STATEFIP) and the COUNTY variable (COUNTYICP) to
#   uniquely identify enumeration districts within and between states."
#   The ED key is therefore STATEFIP + COUNTYICP + ENUMDIST.
#
# OUTPUT: Data/_Cleaned/segregation_1940.csv, one row per 1940 county, keyed on
#   STATEICP + COUNTYICP (merges to the analytic data, which carries the ICPSR codes
#   from the usa_00007 controls extract) and carrying fips_1940 (merges to the
#   county-level instrument files, which are keyed on modern county FIPS).
#
# CACHE: Data/derived/ed_counts_1940.csv holds the ED x race counts. The full-count
#   pass takes a while, so it runs only when the cache is absent.
# ===========================================================

rm(list = ls())

library(tidyverse)
library(data.table)
library(magrittr)
library(readxl)
library(ipumsr)
library(segregation)
library(here)

ed_cache <- here("Data", "derived", "ed_counts_1940.csv")

# ===========================================================
# STEP 1: ED x RACE COUNTS FROM THE FULL COUNT
# ===========================================================
# Read in chunks and aggregate within each chunk. The full extract does not fit
# comfortably in memory, but the aggregate does: ~150k EDs x 2 race groups. RACE is
# restricted to 1 (White) and 2 (Black), matching the `RACE %in% 1:2` sample filter in
# Analysis/01_Data_Cleaning.R. PERWT is 1 for every record in a 100% file, so summing it
# is the same as counting rows; it is summed anyway so the code stays correct if this is
# ever re-pointed at a sample rather than the full count.

if (!file.exists(ed_cache)) {

  message("No ED cache found. Reading the 1940 full count -- this takes a while.")

  ddi <- read_ipums_ddi(here("Data", "Census", "usa_00021.ddi.xml"))

  accumulate_eds <- function(x, pos) {
    setDT(x)
    x[RACE %in% 1:2 & COUNTYICP > 0,
      .(n = sum(PERWT)),
      by = .(STATEFIP, COUNTYICP, ENUMDIST, RACE)]
  }

  ed_counts <- read_ipums_micro_chunked(
    ddi,
    vars     = c("STATEFIP", "COUNTYICP", "ENUMDIST", "RACE", "PERWT"),
    callback = IpumsDataFrameCallback$new(accumulate_eds),
    chunk_size = 5e6,
    verbose  = FALSE
  )

  # Chunk boundaries can split an ED, so re-aggregate across chunks.
  setDT(ed_counts)
  ed_counts <- ed_counts[, .(n = sum(n)), by = .(STATEFIP, COUNTYICP, ENUMDIST, RACE)]

  # One row per ED, with a white and a black count.
  ed_counts <- dcast(ed_counts, STATEFIP + COUNTYICP + ENUMDIST ~ RACE,
                     value.var = "n", fill = 0)
  setnames(ed_counts, c("1", "2"), c("n_white", "n_black"))

  fwrite(ed_counts, ed_cache)
  message("Wrote ED cache: ", nrow(ed_counts), " enumeration districts")

} else {
  message("Using existing ED cache: ", ed_cache)
  ed_counts <- fread(ed_cache)
}

stopifnot(all(c("STATEFIP", "COUNTYICP", "ENUMDIST", "n_white", "n_black") %in% names(ed_counts)))

# ===========================================================
# STEP 2: COUNTY DISSIMILARITY
# ===========================================================
# Same formula as Code/03_county_data.R, with EDs in place of tracts.

county_1940 <- ed_counts %>%
  as_tibble() %>%
  group_by(STATEFIP, COUNTYICP) %>%
  mutate(
    co_black = sum(n_black, na.rm = TRUE),
    co_white = sum(n_white, na.rm = TRUE),
    d        = abs(n_black / co_black - n_white / co_white),
    isob     = (n_black / co_black) * n_black / (n_white + n_black)
  ) %>%
  summarise(
    dism_1940   = 0.5 * sum(d, na.rm = TRUE),
    isolb_1940  = sum(isob, na.rm = TRUE),
    n_ed        = n(),
    co_black    = first(co_black),
    co_white    = first(co_white),
    .groups     = "drop"
  ) %>%
  mutate(
    pop_1940    = co_black + co_white,
    pblack_1940 = co_black / pop_1940
  )

# D is undefined where a county has no Black or no White residents at all: the shares
# above divide by zero and the index collapses to 0 or NaN rather than describing
# anything. These are set to NA explicitly rather than left to propagate.
county_1940 <- county_1940 %>%
  mutate(dism_1940  = if_else(co_black == 0 | co_white == 0, NA_real_, dism_1940),
         isolb_1940 = if_else(co_black == 0, NA_real_, isolb_1940))

message(sprintf("Computed D for %d counties (%d with a usable D)",
                nrow(county_1940), sum(!is.na(county_1940$dism_1940))))

# ===========================================================
# STEP 3: BIAS-ADJUSTED D
# ===========================================================
# EDs are far smaller than tracts (a few hundred people each), and D is upward-biased
# when units are small and one group is rare -- the index bias discussed in the draft
# (Fossett 2017). D_star_1940 is the bias-adjusted counterpart, computed exactly as in
# Code/03_county_data.R. `n_ed` is carried through to STEP 5 so downstream analysis can
# screen counties with too few units.

# Bootstrapped, so it is the slowest step by far (~100 iterations per county across ~3100
# counties). Cached for the same reason the ED counts are.

dstar_cache <- here("Data", "derived", "d_star_1940.csv")

if (!file.exists(dstar_cache)) {

  set.seed(9999)

  D_star_1940 <- ed_counts %>%
    as_tibble() %>%
    pivot_longer(names_to = "group", values_to = "weight",
                 cols = c(n_black, n_white)) %>%
    group_by(STATEFIP, COUNTYICP) %>%
    # Only counties with unit-level variation to work with.
    mutate(n = max(row_number())) %>%
    filter(n > 2) %>%
    group_modify(~ dissimilarity(
      data   = .,
      group  = "group",
      unit   = "ENUMDIST",
      weight = "weight",
      se     = TRUE
    )) %>%
    ungroup() %>%
    select(STATEFIP, COUNTYICP, D_star_1940 = est, se_1940 = se, bias_1940 = bias)

  write_csv(D_star_1940, dstar_cache)

} else {
  message("Using existing D* cache: ", dstar_cache)
  D_star_1940 <- read_csv(dstar_cache, show_col_types = FALSE)
}

county_1940 %<>% left_join(D_star_1940, by = c("STATEFIP", "COUNTYICP"))

# ===========================================================
# STEP 4: ICPSR -> FIPS CROSSWALK
# ===========================================================
# Data/Census/icpsrcnt.xls maps ICPSR state/county codes to state FIPS and county NAMES.
# It does NOT contain a county FIPS code, so the crosswalk has to be built.
#
# The tempting shortcut is COUNTYICP/10, which is the county FIPS for about 95% of rows
# but silently wrong for the rest: ICPSR retains defunct historical counties (Kansas
# "Arapahoe", Idaho "Alturas", Colorado "Greenwood"), which shifts the alphabetical
# sequence relative to FIPS. So names are matched first, with COUNTYICP/10 demoted to a
# tiebreaker and a last-resort fallback.
#
# Names cannot be normalised by simply stripping a trailing " County"/" city", because
# Virginia has both kinds of collision in both directions:
#   - "Charles City County" and "James City County" are COUNTIES whose name ends in
#     "City", so stripping the suffix turns them into "charles"/"james" and they stop
#     matching.
#   - "Alexandria city" (51510) is an independent city distinct from "Arlington County"
#     (51013), yet ICPSR records code 130 as the alternate "Arlington/Alexandria".
# So each name generates TWO candidate keys -- suffix kept and suffix stripped -- and a
# code that ends up with several candidate FIPS is resolved by the COUNTYICP/10 hint.
# That sends 130 to Arlington (51013), keeps Charles City at 51036, and still lets the
# independent cities (Alexandria 51510, Norfolk 51710) match on their stripped form.

# death_fips must be read as character: it is stored zero-padded ("01003") and readr
# would otherwise type-guess it to a number and silently drop the leading zero, which
# would then fail to match the state prefix and the instrument files' GEOID.
canonical <- read_csv(here("Data", "_Cleaned", "county_data.csv"),
                      col_types = cols(death_fips = col_character(),
                                       .default = col_guess())) %>%
  distinct(death_fips, STATE, COUNTY) %>%
  mutate(death_fips = str_pad(death_fips, 5, pad = "0"),
         st         = str_sub(death_fips, 1, 2))

COUNTY_SUFFIX <- regex(" (county|parish|census area|city and borough|borough|municipality|city)$",
                       ignore_case = TRUE)

base_normalise <- function(x) {
  x %>%
    str_replace(regex("^st[.]? ", ignore_case = TRUE), "saint ") %>%
    str_replace(regex("^mc ", ignore_case = TRUE), "mc") %>%
    tolower() %>%
    str_replace_all("[^a-z]", "")
}

# Both spellings of every name: as written, and with the geographic suffix removed.
name_keys <- function(x) {
  tibble(full = base_normalise(x), strip = base_normalise(str_remove(x, COUNTY_SUFFIX)))
}

canonical_keys <- canonical %>%
  mutate(k = name_keys(COUNTY)) %>%
  unpack(k) %>%
  pivot_longer(c(full, strip), values_to = "nm") %>%
  distinct(st, nm, death_fips)

icpsr <- read_excel(here("Data", "Census", "icpsrcnt.xls"), sheet = "icpsrcnt",
                    col_types = "text") %>%
  select(STATENAME, STATEICP, STATEFIP, COUNTYICP, COUNTYNAME) %>%
  mutate(across(c(STATEICP, STATEFIP, COUNTYICP), as.integer)) %>%
  filter(!is.na(COUNTYICP), COUNTYICP > 0) %>%
  mutate(
    st   = sprintf("%02d", STATEFIP),
    # The COUNTYICP/10 hint, defined only where the code is a clean multiple of 10.
    hint = if_else(COUNTYICP %% 10 == 0,
                   sprintf("%02d%03d", STATEFIP, as.integer(COUNTYICP / 10)),
                   NA_character_)
  )

# Alternates such as "Calhoun/Benton" are split; either name may match.
candidates <- icpsr %>%
  separate_longer_delim(COUNTYNAME, "/") %>%
  mutate(k = name_keys(COUNTYNAME)) %>%
  unpack(k) %>%
  pivot_longer(c(full, strip), values_to = "nm") %>%
  distinct(STATEICP, COUNTYICP, STATEFIP, hint, nm, st) %>%
  inner_join(canonical_keys, by = c("st", "nm"), relationship = "many-to-many") %>%
  distinct(STATEICP, COUNTYICP, STATEFIP, hint, fips = death_fips)

by_name <- candidates %>%
  group_by(STATEICP, COUNTYICP, STATEFIP) %>%
  summarise(
    fips_1940 = case_when(
      n() == 1          ~ first(fips),   # unambiguous
      any(fips == hint) ~ first(hint),   # several candidates, code hint picks one
      TRUE              ~ NA_character_  # genuinely ambiguous: drop rather than guess
    ),
    .groups = "drop"
  )

n_ambiguous <- sum(is.na(by_name$fips_1940))
by_name     <- by_name %>% filter(!is.na(fips_1940))

# The reverse collision: one modern FIPS claimed by two different 1940 counties. This
# happens where a county and an independent city share a name and only the city survived
# -- Norfolk County VA (abolished 1963, absorbed into Chesapeake) and Norfolk city both
# normalise to "norfolk" and both reach for 51710. The COUNTYICP/10 hint breaks the tie:
# Norfolk city's code 7100 points at 51710 and keeps it, Norfolk County's does not and is
# left unmatched, which is the right answer since it has no clean modern successor.
by_name <- by_name %>%
  left_join(icpsr %>% distinct(STATEICP, COUNTYICP, hint),
            by = c("STATEICP", "COUNTYICP")) %>%
  group_by(fips_1940) %>%
  filter(n() == 1 | (!is.na(hint) & fips_1940 == hint)) %>%
  # If a tie somehow survives the hint, drop the FIPS entirely rather than assign it
  # arbitrarily to one of the claimants.
  filter(n() == 1) %>%
  ungroup() %>%
  select(STATEICP, COUNTYICP, STATEFIP, fips_1940)

# -- Fallback: the hint on its own, accepted only where it names a real county that no
#    name match has already claimed.
claimed <- by_name$fips_1940

by_code <- icpsr %>%
  distinct(STATEICP, COUNTYICP, STATEFIP, hint) %>%
  anti_join(candidates, by = c("STATEICP", "COUNTYICP")) %>%
  filter(!is.na(hint), hint %in% canonical$death_fips, !hint %in% claimed) %>%
  transmute(STATEICP, COUNTYICP, STATEFIP, fips_1940 = hint)

crosswalk <- bind_rows(by_name, by_code)

n_icpsr <- nrow(distinct(icpsr, STATEICP, COUNTYICP))
message(sprintf(
  "Crosswalk: %d of %d ICPSR counties matched to a FIPS (%.1f%%) -- %d by name, %d by code, %d ambiguous and dropped",
  nrow(crosswalk), n_icpsr, 100 * nrow(crosswalk) / n_icpsr,
  nrow(by_name), nrow(by_code), n_ambiguous))

unmatched <- icpsr %>%
  distinct(STATENAME, STATEICP, COUNTYICP, COUNTYNAME) %>%
  anti_join(crosswalk, by = c("STATEICP", "COUNTYICP"))

if (nrow(unmatched) > 0) {
  message("Unmatched ICPSR counties by state:")
  print(unmatched %>% count(STATENAME, sort = TRUE), n = 30)
}

# ===========================================================
# STEP 5: MERGE AND SAVE
# ===========================================================
# The join is on STATEFIP + COUNTYICP because that is what the microdata carries.
# STATEICP comes from the crosswalk, and is the key the analytic data joins on.

segregation_1940 <- county_1940 %>%
  left_join(crosswalk, by = c("STATEFIP", "COUNTYICP")) %>%
  # Written zero-padded so the join against the instrument files' GEOID is a string
  # comparison rather than a leading-zero coin flip.
  mutate(fips_1940 = str_pad(fips_1940, 5, pad = "0")) %>%
  select(STATEICP, STATEFIP, COUNTYICP, fips_1940,
         dism_1940, D_star_1940, se_1940, bias_1940, isolb_1940,
         n_ed, pop_1940, co_black, co_white, pblack_1940)

stopifnot(all(is.na(segregation_1940$fips_1940) | nchar(segregation_1940$fips_1940) == 5))

# One modern county must not stand for two different 1940 counties: the analysis joins
# the instruments on fips_1940 and clusters on it, so a duplicate would silently pool two
# places into one cluster.
stopifnot(!any(duplicated(na.omit(segregation_1940$fips_1940))))

message(sprintf("Counties with a FIPS: %d of %d (%.1f%%); population covered: %.1f%%",
                sum(!is.na(segregation_1940$fips_1940)), nrow(segregation_1940),
                100 * mean(!is.na(segregation_1940$fips_1940)),
                100 * sum(segregation_1940$pop_1940[!is.na(segregation_1940$fips_1940)]) /
                  sum(segregation_1940$pop_1940)))

write_csv(segregation_1940, here("Data", "_Cleaned", "segregation_1940.csv"))
message("Saved: ", nrow(segregation_1940), " counties")
