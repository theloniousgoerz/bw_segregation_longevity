# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 1940 Segregation: Descriptives and IV Estimates
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Two things:
#   1. How does segregation in the county someone LIVED IN in 1940 relate to segregation
#      in the county they DIED IN in 1980-2000 (`county_dism`)?
#   2. What do the main RDI IV estimates look like when 1940 segregation is the
#      endogenous regressor instead of death-county segregation?
#
# `dism_1940` is built by Code/09_1940_segregation.R from the 1940 full count, using
# enumeration districts within county. Read that script's header for the construction.
#
# TWO CAVEATS THAT GOVERN HOW THESE NUMBERS SHOULD BE READ:
#
#   * EDs are much smaller than tracts (a few hundred people against a few thousand), and
#     D is upward-biased when units are small and one group is rare. `dism_1940` is
#     therefore NOT comparable in LEVEL to `county_dism`, only in rank and association.
#     `D_star_1940` (bias-adjusted) is carried alongside for exactly this reason, and the
#     models are re-run on it as a check.
#   * For movers the two measures describe DIFFERENT COUNTIES, so the descriptive
#     correlation below is split by `mover`. For stayers it is the same county measured
#     40-60 years apart; for movers it is origin against destination.
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## Packages
rm(list = ls())
library(tidyverse)
library(magrittr)
library(readr)
library(modelsummary)
library(cowplot)
library(fixest)
library(tinytable)
library(broom)
library(here)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Data
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
source(here("Analysis", "00_Helpers.R"))

db = read_csv(here("Data", "_Cleaned", "db.csv"))
dw = read_csv(here("Data", "_Cleaned", "dw.csv"))

# Repair birth_fips and derive migration status, exactly as every other analysis script
# does. `mover` comes from here and is used in the descriptive split below.
db %<>% prepare_analysis_data("db")
dw %<>% prepare_analysis_data("dw")

# ------------------------------- 1940 segregation -------------------------------
# Keyed on STATEICP + COUNTYICP, which db/dw carry from the usa_00007 controls extract.
seg40 = read_csv(here("Data", "_Cleaned", "segregation_1940.csv"),
                 col_types = cols(fips_1940 = col_character(), .default = col_guess()))

# `county_dism` is rescaled x100 in 03_Representivity_Table.R, so the coefficient reads
# as years of life per one-point move in D. The 1940 measures are put on the same scale
# so that the existing "per 10-point increase" framing carries over unchanged.
seg40 %<>% mutate(dism_1940   = dism_1940 * 100,
                  D_star_1940 = D_star_1940 * 100)

# ------------------------------- Small-unit bias screen -------------------------------
# This matters more here than it does for the tract-based death-county measure, and it is
# not a nuisance detail. D is mechanically near 1 when one group is tiny: a county with
# two Black residents spread over 30 EDs scores ~1.0 no matter how integrated it is.
# In these data mean D falls monotonically with the size of the Black population --
# 0.86 in counties with 1-10 Black residents, 0.73 at 11-100, 0.63 at 101-1,000, and
# 0.45 above 1,000 -- and the bias-adjusted D* does NOT fix it (0.90 in the smallest bin,
# and it leaves [0,1] for 127 counties). The unscreened "most segregated" counties in
# 1940 are rural counties with essentially no Black residents, which is an artefact.
#
# `d1940_ok` marks counties where the index is interpretable. Models are reported both
# ways below: the screen is not applied silently.
seg40 %<>% mutate(d1940_ok = co_black >= 100 & n_ed >= 10)

# ------------------------------- Instruments for the 1940 county -------------------
# The RDI instrumenting 1940 segregation is the RDI of the 1940 COUNTY OF RESIDENCE, not
# of the county of death, so that instrument and endogenous regressor refer to the same
# place. The rail file is keyed on modern county FIPS (GEOID), which is what fips_1940
# supplies. GEOID is read as character and padded because it is stored zero-padded and
# would otherwise lose the leading zero for states 01-09.
rdi_1940 = read_csv(here("Data", "derived", "atack_rail_county_instruments_1911.csv"),
                    col_types = cols(GEOID = col_character(), .default = col_guess())) %>%
  transmute(fips_1940            = str_pad(GEOID, 5, pad = "0"),
            rdi_1940             = rdi,
            rail_km_per_km2_1940 = rail_km_per_km2)

seg40 %<>% left_join(rdi_1940, by = "fips_1940")

merge_cols = c("STATEICP", "COUNTYICP", "fips_1940", "dism_1940", "D_star_1940",
               "isolb_1940", "n_ed", "pop_1940", "pblack_1940", "co_black", "d1940_ok",
               "rdi_1940", "rail_km_per_km2_1940")

# STATEICP is NA for the handful of 1940 counties the crosswalk could not place (all
# defunct Virginia counties: Elizabeth City, Norfolk, Princess Anne, Warwick). They are
# dropped before the join rather than after, because dplyr treats NA == NA as a MATCH:
# left as-is, every sample row with a missing STATEICP would join to all of them.
seg40_join = seg40 %>%
  filter(!is.na(STATEICP), !is.na(COUNTYICP)) %>%
  select(all_of(merge_cols))

stopifnot(!anyDuplicated(seg40_join[, c("STATEICP", "COUNTYICP")]))

db %<>% left_join(seg40_join, by = c("STATEICP", "COUNTYICP"))
dw %<>% left_join(seg40_join, by = c("STATEICP", "COUNTYICP"))

# ------------------------------- Three-way migration status -------------------------------
# The `mover` factor from 00_Helpers.R compares BIRTH county with DEATH county only, so it
# cannot see anyone who left and came back. The 1940 county is an intermediate observation
# that can, and it splits the "stayer" group in two:
#
#   Stayer  birth = 1940 = death   in the same county at all three observations
#   Return  birth = death != 1940  counted as a stayer by `mover`, but demonstrably
#                                  living somewhere else in 1940
#   Migrant birth != death
#
# This matters because the draft treats non-migrants as the group plausibly exposed to one
# county continuously. Return migrants are 8.6% of Black and 4.9% of White decedents, which
# is 32% and 16% of their respective `mover == "Stayer"` groups.
#
# Levels are one word so the fixest interaction coefficients are named `mig3Return` and
# `mig3Migrant` rather than carrying a space; the table's coef_map supplies the full labels.
# Stayer is the reference level, so in the interacted models "D" is the stayer effect and
# the interactions are differences from it -- the same convention as the movers/stayers
# table in 04_Regression_Models.R.
add_mig3 = function(d) {
  d %>%
    mutate(
      death_fips_pad = str_pad(as.character(death_fips), 5, pad = "0"),
      mig3 = case_when(
        is.na(birth_fips) | is.na(fips_1940) | is.na(death_fips_pad) ~ NA_character_,
        birth_fips == death_fips_pad & death_fips_pad == fips_1940   ~ "Stayer",
        birth_fips == death_fips_pad                                 ~ "Return",
        TRUE                                                         ~ "Migrant"
      ),
      mig3 = factor(mig3, levels = c("Stayer", "Return", "Migrant"))
    ) %>%
    select(-death_fips_pad)
}

db %<>% add_mig3()
dw %<>% add_mig3()

print(bind_rows(db %>% count(mig3) %>% mutate(Race = "Black"),
                dw %>% count(mig3) %>% mutate(Race = "White")) %>%
        group_by(Race) %>% mutate(pct = round(100 * n / sum(n), 1)) %>% ungroup())

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Merge coverage
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Reported rather than assumed: the crosswalk loses ~5% of ICPSR counties, and IPUMS
# leaves some 1940 counties unidentifiable (COUNTYICP == 0). Both show up here as rows
# with a missing dism_1940, and the share of ROWS lost is what matters for the models.

coverage = bind_rows(
  db %>% transmute(Race = "Black", dism_1940, rdi_1940),
  dw %>% transmute(Race = "White", dism_1940, rdi_1940)
) %>%
  group_by(Race) %>%
  summarise(
    n              = n(),
    has_d1940      = sum(!is.na(dism_1940)),
    pct_d1940      = 100 * mean(!is.na(dism_1940)),
    has_rdi1940    = sum(!is.na(rdi_1940)),
    pct_rdi1940    = 100 * mean(!is.na(rdi_1940)),
    pct_estimation = 100 * mean(!is.na(dism_1940) & !is.na(rdi_1940)),
    .groups = "drop"
  )

print(coverage)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Descriptive association: 1940 vs death-county segregation
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Columns are selected BEFORE the bind, not after: dw is a 2.1 GB file and binding the
# two in full would hold a second copy of it in memory for no reason.
desc_cols = function(d, race) {
  d %>%
    transmute(Race = race, dism_1940, D_star_1940, county_dism, death_decade, mover,
              death_fips, fips_1940, pblack_1940, n_ed) %>%
    filter(!is.na(dism_1940), !is.na(county_dism))
}

pooled = bind_rows(desc_cols(db, "Black"), desc_cols(dw, "White"))

# ------------------------------- Summary statistics -------------------------------
desc_stats = pooled %>%
  group_by(Race) %>%
  summarise(
    n           = n(),
    mean_d1940  = mean(dism_1940),
    sd_d1940    = sd(dism_1940),
    mean_ddeath = mean(county_dism),
    sd_ddeath   = sd(county_dism),
    r           = cor(dism_1940, county_dism),
    .groups = "drop"
  )

print(desc_stats)

# ------------------------------- Correlation by strata -------------------------------
# Overall, by decade of death, and by mover status. The mover split is the substantive
# one: for stayers this is the same county 40-60 years apart.
cor_by = function(d, ...) {
  d %>% group_by(Race, ...) %>%
    summarise(n = n(), r = cor(dism_1940, county_dism), .groups = "drop")
}

cor_overall = cor_by(pooled)               %>% mutate(stratum = "Overall",  level = "")
cor_decade  = cor_by(pooled, death_decade) %>% mutate(stratum = "Death decade",
                                                      level = as.character(death_decade))
cor_mover   = cor_by(pooled, mover)        %>% mutate(stratum = "Migration",
                                                      level = as.character(mover))

cor_table = bind_rows(cor_overall, cor_decade, cor_mover) %>%
  select(stratum, level, Race, n, r) %>%
  pivot_wider(names_from = Race, values_from = c(n, r)) %>%
  arrange(factor(stratum, levels = c("Overall", "Death decade", "Migration")), level)

print(cor_table)

# ------------------------------- Descriptives table -------------------------------
desc_out = bind_rows(
  desc_stats %>%
    transmute(Panel = "Distribution", Statistic = Race,
              `D 1940`  = sprintf("%.1f (%.1f)", mean_d1940,  sd_d1940),
              `D Death` = sprintf("%.1f (%.1f)", mean_ddeath, sd_ddeath),
              Correlation = sprintf("%.3f", r),
              N = format(n, big.mark = ",")),
  cor_table %>%
    transmute(Panel = stratum,
              Statistic = if_else(level == "", "All", level),
              `D 1940` = "", `D Death` = "",
              Correlation = sprintf("B %.3f / W %.3f", r_Black, r_White),
              N = format(n_Black + n_White, big.mark = ","))
)

tt(desc_out,
   caption = "1940 County Segregation and County-at-Death Segregation",
   notes = "Dissimilarity is expressed on a 0-100 scale. 1940 segregation is computed from enumeration districts within county using the 1940 full count; county-at-death segregation is computed from census tracts in the decennial census matching the decade of death. Because enumeration districts are smaller than tracts, the 1940 index is mechanically higher and the two are comparable in association rather than in level. Standard deviations in parentheses. The migration split compares people who died in their county of birth with those who did not.") %>%
  save_tt(here("FigTab", "seg_1940_descriptives_table.tex"), overwrite = TRUE)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Birth, 1940, and death county: pairwise agreement
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# The `mover`/`stayer` split used throughout the paper compares BIRTH county with DEATH
# county, and the draft treats stayers as the group plausibly exposed to one place
# continuously. The 1940 county is the first chance to check that reading against an
# intermediate observation, because it sits between the two.
#
# All three are put on 5-character county FIPS. birth_fips is already repaired by
# prepare_analysis_data(); death_fips is padded here (the helper deliberately leaves it
# as stored, since it is a numeric join key elsewhere); fips_1940 comes from the ICPSR
# crosswalk in Code/09_1940_segregation.R. County boundaries did shift between 1940 and
# 2000, so a small amount of disagreement is definitional rather than migration.

agree_cols = function(d, race) {
  d %>%
    transmute(Race  = race,
              Birth = birth_fips,
              `1940` = fips_1940,
              Death = str_pad(as.character(death_fips), 5, pad = "0")) %>%
    filter(!is.na(Birth), !is.na(`1940`), !is.na(Death))
}

agree = bind_rows(agree_cols(db, "Black"), agree_cols(dw, "White"))

pairwise_agreement = function(d) {
  v <- list(Birth = d$Birth, `1940` = d$`1940`, Death = d$Death)
  m <- outer(seq_along(v), seq_along(v),
             Vectorize(function(i, j) 100 * mean(v[[i]] == v[[j]])))
  dimnames(m) <- list(names(v), names(v))
  m
}

agree_mats = lapply(split(agree, agree$Race), pairwise_agreement)

agree_table = data.frame(
  rownames(agree_mats$Black),
  round(agree_mats$Black, 1),
  round(agree_mats$White, 1),
  row.names = NULL
)
# The two race blocks repeat the same three column labels; the spanners below say which
# is which. Trailing spaces keep the names unique in R and are invisible in LaTeX.
names(agree_table) = c("", "Birth", "1940", "Death", "Birth ", "1940 ", "Death ")

print(agree_table, row.names = FALSE)

tt(agree_table,
   caption = "Share of Individuals Whose Birth, 1940, and Death County Are the Same",
   notes = "Cells report the percentage of individuals for whom the county in the row and the county in the column are the same, expressed separately for Black and White decedents. County of birth and county of death are taken from the CenSoc Numident geography supplement; county of residence in 1940 is the ICPSR county in the 1940 full count, crosswalked to county FIPS. The sample is restricted to individuals with all three counties observed. The diagonal is 100 by construction, and the matrix is symmetric.") %>%
  group_tt(j = list("Black" = 2:4, "White" = 5:7)) %>%
  save_tt(here("FigTab", "county_agreement_table.tex"), overwrite = TRUE)

# ------------------------------- Binned scatter -------------------------------
# A raw scatter here is unreadable: there are hundreds of thousands of distinct
# origin-by-destination county pairs and the panel fills in solid. Binning on the
# x-variable is the standard fix and is what the first-stage figures in the paper
# already do (binsreg).
#
# The series is migration status, because that is the substantive contrast: for stayers
# the two axes are the SAME county measured 40-60 years apart, for movers they are
# origin against destination. Shape carries identity alongside the fill, so the panel
# survives grayscale printing.

N_BINS = 20

binned = pooled %>%
  filter(!is.na(mover)) %>%
  group_by(Race, mover) %>%
  mutate(bin = ntile(dism_1940, N_BINS)) %>%
  group_by(Race, mover, bin) %>%
  summarise(x = mean(dism_1940), y = mean(county_dism), n = n(), .groups = "drop")

seg_plot = binned %>%
  ggplot(aes(x = x, y = y, colour = mover, shape = mover)) +
  geom_point(size = 2.6) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.7) +
  facet_wrap(~ Race) +
  scale_colour_manual(values = c(Stayer = "grey15", Mover = "grey60"), name = NULL) +
  scale_shape_manual(values = c(Stayer = 16, Mover = 17), name = NULL) +
  coord_cartesian(ylim = c(30, 75)) +
  labs(x = "Dissimilarity in county of residence, 1940 (enumeration districts)",
       y = "Dissimilarity in county\nof death (tracts)") +
  theme_cowplot() +
  background_grid(major = "xy", size.major = 0.2) +
  theme(legend.position = "top")

ggsave(seg_plot, filename = here("FigTab", "seg_1940_corr_plot.jpeg"),
       width = 9, height = 4.5, dpi = 300)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Specification vocabulary
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Copied verbatim from Analysis/04_Regression_Models.R so the only thing that differs
# from the published RDI specification is the endogenous regressor, the county the
# instruments are measured in, and the clustering level.

cov_main <- "male + migrated + education + married + south"
fe_base  <- "byear + STATEFIP_b + urb_code"
fe_main  <- "byear + STATEFIP_b + urb_code + OCC"

# Clustering moves to the 1940 county, which is the level the endogenous regressor now
# varies at. (In 04_Regression_Models.R it is `~death_fips` for the same reason.)
CL40 <- ~fips_1940

instr_rdi_40 <- "rdi_1940 + rail_km_per_km2_1940"
iv_rdi_40    <- paste("dism_1940 ~", instr_rdi_40)
iv_rdi_40_ds <- paste("D_star_1940 ~", instr_rdi_40)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# First stage: does the RDI predict 1940 segregation?
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Estimated on distinct counties, matching how the death-county first-stage F is
# computed in 04_Regression_Models.R. The RDI is time-constant, so this is the honest
# denominator. If the F is weak, that is a result about the 1940 measure, not a knob.

# Restricted to the counties the estimation sample actually contains, so the first stage
# describes the same county set the IV models are fit on.
sample_counties = union(db$fips_1940, dw$fips_1940)

instrument_40 = seg40 %>%
  filter(fips_1940 %in% sample_counties, !is.na(rdi_1940), !is.na(dism_1940)) %>%
  distinct(fips_1940, dism_1940, D_star_1940, rdi_1940, rail_km_per_km2_1940)

fs_rdi_40 = feols(dism_1940 ~ .[instr_rdi_40], data = instrument_40, vcov = "white")

print(summary(fs_rdi_40))
cat("\nFirst-stage F (1940 RDI):\n")
print(fitstat(fs_rdi_40, type = "f"))

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# IV estimates
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

d1_b = feols(death_age ~ 1            | .[fe_base] | .[iv_rdi_40], data = db, vcov = CL40)
d1_w = feols(death_age ~ 1            | .[fe_base] | .[iv_rdi_40], data = dw, vcov = CL40)
d2_b = feols(death_age ~ .[cov_main]  | .[fe_main] | .[iv_rdi_40], data = db, vcov = CL40)
d2_w = feols(death_age ~ .[cov_main]  | .[fe_main] | .[iv_rdi_40], data = dw, vcov = CL40)

msummary(list("1st" = summary(d1_b, stage = 1),
              "IV"  = d1_b,
              "1st" = summary(d2_b, stage = 1),
              "IV"  = d2_b,
              "1st" = summary(d1_w, stage = 1),
              "IV"  = d1_w,
              "1st" = summary(d2_w, stage = 1),
              "IV"  = d2_w),
         fmt = 3,
         stars = T,
         coef_map = c(
           "fit_dism_1940" = "D (1940)",
           "male" = "Male",
           "education" = "Education",
           "migratedMigrated" = "Migrated",
           "married" = "Married in 1940",
           "south" = "Died in South",
           "rdi_1940" = "Railroad Division Index (1940 county)"),
         gof_map = c("nobs",
                     "r.squared",
                     "f"),
         align = "lcccccccc",
         notes = "This table describes the first-stage models and IV estimates of the effect of 1940 county segregation on longevity, instrumented with the railroad division index of the 1940 county of residence. Dissimilarity is computed from enumeration districts within county in the 1940 full count and is expressed on a 0-100 scale. Cluster-robust standard errors (by 1940 county) in parentheses.",
         title = "Estimates of the Effect of 1940 County Segregation on Longevity (RDI Instrument)",
         add_rows = data.frame(
           FE = c("Birth Year", "Birth State", "Urban-Rural Code", "Occupation"),
           m1_fs = c("-", "-", "-", "-"),
           m1 = c("X", "X", "X", "-"),
           m2_fs = c("-", "-", "-", "-"),
           m2 = c("X", "X", "X", "X"),
           m3_fs = c("-", "-", "-", "-"),
           m3 = c("X", "X", "X", "-"),
           m4_fs = c("-", "-", "-", "-"),
           m4 = c("X", "X", "X", "X")
         ),
         threeparttable = TRUE
) %>%
  group_tt(j = list("Base" = 2:3, "Controls" = 4:5, "Base" = 6:7, "Controls" = 8:9)) %>%
  group_tt(j = list("Black" = 2:5, "White" = 6:9)) %>%
  save_tt(., output = here("FigTab", "IV_results_table_1940.tex"), overwrite = T)


# ------------------------------- Main Specification in regression_models controlling for 1940 -------------------------------

d1_b_c = feols(death_age ~ 1           + dism_1940 | .[fe_base] | county_dism~rdi + rail_km_per_km2, data = db, vcov =~death_fips)
d1_w_c = feols(death_age ~ 1           + dism_1940 | .[fe_base] | county_dism~rdi + rail_km_per_km2, data = dw, vcov =~death_fips)
d2_b_c = feols(death_age ~ .[cov_main] + dism_1940 | .[fe_main] | county_dism~rdi + rail_km_per_km2, data = db, vcov =~death_fips)
d2_w_c = feols(death_age ~ .[cov_main] + dism_1940 | .[fe_main] | county_dism~rdi + rail_km_per_km2, data = dw, vcov =~death_fips)

# ------------------------------- Bias-adjusted D check -------------------------------
# The ED-level D is upward-biased where the Black population is small. If the estimates
# are an artefact of that bias, they should move when D* is substituted.

ds_b = feols(death_age ~ .[cov_main] | .[fe_main] | .[iv_rdi_40_ds], data = db, vcov = CL40)
ds_w = feols(death_age ~ .[cov_main] | .[fe_main] | .[iv_rdi_40_ds], data = dw, vcov = CL40)

# ------------------------------- Screened sample -------------------------------
# Same specification, restricted to counties where D is interpretable (see the screen
# defined with seg40 above). Reported rather than substituted.

sc_b = feols(death_age ~ .[cov_main] | .[fe_main] | .[iv_rdi_40],
             data = db %>% filter(d1940_ok), vcov = CL40)
sc_w = feols(death_age ~ .[cov_main] | .[fe_main] | .[iv_rdi_40],
             data = dw %>% filter(d1940_ok), vcov = CL40)

cat("\n================ SUMMARY ================\n")
cat("Share of rows in screened counties -- Black:",
    sprintf("%.1f%%", 100 * mean(db$d1940_ok, na.rm = TRUE)), " White:",
    sprintf("%.1f%%", 100 * mean(dw$d1940_ok, na.rm = TRUE)), "\n\n")
cat("Fully adjusted, D (1940), all counties:\n")
print(rbind(Black = coeftable(d2_b)["fit_dism_1940", ],
            White = coeftable(d2_w)["fit_dism_1940", ]))
cat("\nFully adjusted, D (1940), screened counties:\n")
print(rbind(Black = coeftable(sc_b)["fit_dism_1940", ],
            White = coeftable(sc_w)["fit_dism_1940", ]))
cat("\nFully adjusted, bias-adjusted D* (1940), all counties:\n")
print(rbind(Black = coeftable(ds_b)["fit_D_star_1940", ],
            White = coeftable(ds_w)["fit_D_star_1940", ]))

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Exclusion tests on the strict (three-way) non-migrant definition
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Appendix Tables A6 and A7 (Analysis/06_Robustness_Checks.R:213-269) regress 1940
# education and log wage income on the RDI for the full sample and for non-migrants. The
# footnote attached to them concedes its own weakness: "It is possible that there are
# exposures that I cannot observe between birth place and death place that occured
# between censuses. This serves as only a proxy of continous exposure."
#
# The 1940 county supplies that missing intermediate observation. These models repeat the
# non-migrant specification under a stricter definition -- same county at birth, in 1940,
# AND at death -- which removes the return migrants who satisfy birth = death but were
# living elsewhere in 1940.
#
# The RDI here is the DEATH-COUNTY RDI throughout, which is the instrument the exclusion
# restriction actually concerns. (For the strict non-migrants the death county is also the
# 1940 county and the birth county, so the distinction is moot within that column; it
# matters for the birth = death column, which still contains return migrants.) The
# specification is otherwise unchanged from 06_Robustness_Checks.R: same controls, same
# fixed effects, same clustering, RDI entered directly rather than as an instrument.
# `migrated` and `south` are dropped in both restricted samples because both are collinear
# once birth county equals death county.

db %<>% mutate(log_incwage = ifelse(INCWAGE %in% c(999998, 999999) | INCWAGE <= 0, NA, log(INCWAGE)))
dw %<>% mutate(log_incwage = ifelse(INCWAGE %in% c(999998, 999999) | INCWAGE <= 0, NA, log(INCWAGE)))

db_s2 = db %>% filter(mover == "Stayer")   # birth = death: the definition in the draft
dw_s2 = dw %>% filter(mover == "Stayer")
db_s3 = db %>% filter(mig3  == "Stayer")   # birth = 1940 = death: the strict definition
dw_s3 = dw %>% filter(mig3  == "Stayer")

FE_EXCL       <- "byear + STATEFIP_b + urb_code + OCC"
COV_EXCL_EDUC <- "male + married + rdi + rail_km_per_km2"
COV_EXCL_INC  <- "male + education + married + rdi + rail_km_per_km2"

ed_b2 = feols(education   ~ .[COV_EXCL_EDUC] | .[FE_EXCL], data = db_s2, vcov = ~death_fips)
ed_w2 = feols(education   ~ .[COV_EXCL_EDUC] | .[FE_EXCL], data = dw_s2, vcov = ~death_fips)
ed_b3 = feols(education   ~ .[COV_EXCL_EDUC] | .[FE_EXCL], data = db_s3, vcov = ~death_fips)
ed_w3 = feols(education   ~ .[COV_EXCL_EDUC] | .[FE_EXCL], data = dw_s3, vcov = ~death_fips)

in_b2 = feols(log_incwage ~ .[COV_EXCL_INC]  | .[FE_EXCL], data = db_s2, vcov = ~death_fips)
in_w2 = feols(log_incwage ~ .[COV_EXCL_INC]  | .[FE_EXCL], data = dw_s2, vcov = ~death_fips)
in_b3 = feols(log_incwage ~ .[COV_EXCL_INC]  | .[FE_EXCL], data = db_s3, vcov = ~death_fips)
in_w3 = feols(log_incwage ~ .[COV_EXCL_INC]  | .[FE_EXCL], data = dw_s3, vcov = ~death_fips)

msummary(
  list("Black\\newline (Birth=Death)" = ed_b2, "White\\newline (Birth=Death)" = ed_w2,
       "Black\\newline (All Three)"   = ed_b3, "White\\newline (All Three)"   = ed_w3,
       "Black\\newline (Birth=Death)" = in_b2, "White\\newline (Birth=Death)" = in_w2,
       "Black\\newline (All Three)"   = in_b3, "White\\newline (All Three)"   = in_w3),
  coef_map = c("rdi" = "RDI"),
  stars = T,
  gof_map = c("nobs", "r.squared"),
  fmt = 3,
  notes = "This table tests the association between the RDI and 1940 socioeconomic outcomes under two non-migrant definitions. 'Birth=Death' is the definition used in those tables: the individual died in their county of birth. 'All Three' additionally requires that they were living in that same county at the 1940 Census, which removes return migrants who satisfy the birth-equals-death definition but were observed elsewhere at the intermediate census. The RDI is measured in the county of death throughout. Controls: male, married, and (income models only) education. Fixed effects: birth year, birth state, urbanicity, occupation. Standard errors clustered on county of death.",
  title = "Association between the RDI and 1940 Socioeconomic Outcomes, Strict Non-Migrant Definitions",
  threeparttable = T,
  output = "tinytable"
) %>%
  group_tt(j = list("Years of Education" = 2:5, "Log Wage Income" = 6:9)) %>%
  save_tt(here("FigTab", "rdi_exclusion_three_way_table.tex"), overwrite = TRUE)

cat("\n=== RDI coefficient, strict vs. two-way non-migrant (death-county RDI) ===\n")
print(rbind(
  `Educ Black 2-way` = coeftable(ed_b2)["rdi", ], `Educ Black 3-way` = coeftable(ed_b3)["rdi", ],
  `Educ White 2-way` = coeftable(ed_w2)["rdi", ], `Educ White 3-way` = coeftable(ed_w3)["rdi", ],
  `Inc  Black 2-way` = coeftable(in_b2)["rdi", ], `Inc  Black 3-way` = coeftable(in_b3)["rdi", ],
  `Inc  White 2-way` = coeftable(in_w2)["rdi", ], `Inc  White 3-way` = coeftable(in_w3)["rdi", ]))
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Main RDI IV estimates by three-way migration status
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# The paper's MAIN specification -- death-county segregation instrumented by the
# death-county RDI, clustered on county of death -- re-estimated across the three-way
# split. Only the migration variable is new; `county_dism`, `rdi` and the clustering are
# exactly as in Analysis/04_Regression_Models.R, so these columns are directly comparable
# to the movers/stayers table (FigTab/IV_results_table_movers.tex).

CL           <- ~death_fips
cov_mig      <- "male + education + married + south"
cov_mig_stay <- "male + education + married"
instr_rdi    <- "rdi + rail_km_per_km2"
iv_rdi       <- paste("county_dism ~", instr_rdi)

interact_with <- function(instr, var) {
  paste(paste0(trimws(strsplit(instr, "\\+")[[1]]), "*", var), collapse = " + ")
}
iv_rdi_mig3 <- paste("county_dism*mig3 ~", interact_with(instr_rdi, "mig3"))

# `south` is dropped for stayers AND return migrants: both died in their birth county, so
# they died in their birth state and "died in the South" is a deterministic function of the
# birth-state fixed effect. It is retained in the migrant and pooled models.
m_b_stay = feols(death_age ~ .[cov_mig_stay] | .[fe_main] | .[iv_rdi], data = subset(db, mig3 == "Stayer"),  vcov = CL)
m_b_ret  = feols(death_age ~ .[cov_mig_stay] | .[fe_main] | .[iv_rdi], data = subset(db, mig3 == "Return"),  vcov = CL)
m_b_mig  = feols(death_age ~ .[cov_mig]      | .[fe_main] | .[iv_rdi], data = subset(db, mig3 == "Migrant"), vcov = CL)
m_w_stay = feols(death_age ~ .[cov_mig_stay] | .[fe_main] | .[iv_rdi], data = subset(dw, mig3 == "Stayer"),  vcov = CL)
m_w_ret  = feols(death_age ~ .[cov_mig_stay] | .[fe_main] | .[iv_rdi], data = subset(dw, mig3 == "Return"),  vcov = CL)
m_w_mig  = feols(death_age ~ .[cov_mig]      | .[fe_main] | .[iv_rdi], data = subset(dw, mig3 == "Migrant"), vcov = CL)

m_b_int = feols(death_age ~ .[cov_mig] | .[fe_main] | .[iv_rdi_mig3], data = db, vcov = CL)
m_w_int = feols(death_age ~ .[cov_mig] | .[fe_main] | .[iv_rdi_mig3], data = dw, vcov = CL)

cat("\n=== interacted model coefficient names (check coef_map) ===\n")
print(names(coef(m_b_int)))

msummary(
  list("Black\\newline (Stayer)"     = m_b_stay,
       "Black\\newline (Return)"     = m_b_ret,
       "Black\\newline (Migrant)"    = m_b_mig,
       "Black\\newline (Interacted)" = m_b_int,
       "White\\newline (Stayer)"     = m_w_stay,
       "White\\newline (Return)"     = m_w_ret,
       "White\\newline (Migrant)"    = m_w_mig,
       "White\\newline (Interacted)" = m_w_int),
  fmt = 3, stars = T,
  coef_map = c(
    "fit_county_dism"              = "D",
    "fit_county_dism:mig3Return"   = "D x Return Migrant",
    "fit_county_dism:mig3Migrant"  = "D x Migrant",
    "fit_mig3Return"               = "Return Migrant",
    "fit_mig3Migrant"              = "Migrant",
    "male"                         = "Male",
    "education"                    = "Education",
    "married"                      = "Married in 1940",
    "south"                        = "Died in South"),
  gof_map = c("nobs", "r.squared", "f"),
  notes = "This table describes IV estimates (Railroad Division Index instrument) of the effect of county-at-death segregation on longevity across a three-way migration classification that uses county of residence in the 1940 Census as an intermediate observation. A Stayer was in the same county at birth, in 1940, and at death. A Return Migrant died in their birth county but was living elsewhere in 1940, and so is classified as a non-migrant by the birth-versus-death definition used elsewhere in the paper. A Migrant died in a county other than their birth county. The Stayer, Return and Migrant columns split the sample, so every coefficient is free to differ. The Interacted columns pool the three groups and interact D with migration status, so 'D' is the effect among stayers and the interaction terms are differences from it. Dissimilarity is expressed on a 0-100 scale. Cluster-robust standard errors (by county of death) in parentheses.",
  title = "Estimates of the Effect of Segregation on Longevity by Three-Way Migration Status (RDI Instrument)",
  add_rows = data.frame(
    FE = c("Birth Year", "Birth State", "Urban-Rural Code", "Occupation"),
    m1 = c("X","X","X","X"), m2 = c("X","X","X","X"),
    m3 = c("X","X","X","X"), m4 = c("X","X","X","X"),
    m5 = c("X","X","X","X"), m6 = c("X","X","X","X"),
    m7 = c("X","X","X","X"), m8 = c("X","X","X","X")
  ),
  threeparttable = TRUE
) %>%
  group_tt(j = list("Black" = 2:5, "White" = 6:9)) %>%
  save_tt(here("FigTab", "IV_results_table_mig3.tex"), overwrite = T)

cat("\n=== D by three-way migration status (per 1 point of D) ===\n")
print(rbind(
  `Black Stayer`  = coeftable(m_b_stay)["fit_county_dism", ],
  `Black Return`  = coeftable(m_b_ret)["fit_county_dism", ],
  `Black Migrant` = coeftable(m_b_mig)["fit_county_dism", ],
  `White Stayer`  = coeftable(m_w_stay)["fit_county_dism", ],
  `White Return`  = coeftable(m_w_ret)["fit_county_dism", ],
  `White Migrant` = coeftable(m_w_mig)["fit_county_dism", ]))

# ------------------------------- Three-way migration status figure -------------------------------
# Same construction as the movers-vs-stayers figure in 04_Regression_Models.R
# (movers_plot_data / movers_plot): pointrange of the D coefficient from each sample-split
# model, rescaled to a 10-point increase in dissimilarity via the same `contrast` convention.
contrast = 10

mig3_plot_data = bind_rows(
  tidy(m_b_stay, conf.int = T) |> filter(term == "fit_county_dism") |> mutate(Race = "Black", Group = "Stayer"),
  tidy(m_b_ret,  conf.int = T) |> filter(term == "fit_county_dism") |> mutate(Race = "Black", Group = "Return Migrant"),
  tidy(m_b_mig,  conf.int = T) |> filter(term == "fit_county_dism") |> mutate(Race = "Black", Group = "Migrant"),
  tidy(m_w_stay, conf.int = T) |> filter(term == "fit_county_dism") |> mutate(Race = "White", Group = "Stayer"),
  tidy(m_w_ret,  conf.int = T) |> filter(term == "fit_county_dism") |> mutate(Race = "White", Group = "Return Migrant"),
  tidy(m_w_mig,  conf.int = T) |> filter(term == "fit_county_dism") |> mutate(Race = "White", Group = "Migrant")
) |>
  mutate(estimate  = estimate  * contrast,
         conf.high = conf.high * contrast,
         conf.low  = conf.low  * contrast,
         Group     = factor(Group, levels = c("Stayer", "Return Migrant", "Migrant")))

mig3_plot =
  ggplot(mig3_plot_data,
         aes(Group, estimate, ymin = conf.low, ymax = conf.high, color = Race)) +
  geom_pointrange(position = position_dodge2(width = .5), size = .75, lwd = .75, shape = 22) +
  geom_hline(yintercept = 0, linetype = "dashed", lwd = 1) +
  scale_color_manual(values = c("Black" = "darkgreen", "White" = "darkblue")) +
  labs(y = "Change in Life Expectancy", x = NULL,
       caption = str_wrap("IV estimates (Railroad Division Index instrument) of the effect of segregation on longevity,
       split by three-way migration status using county of residence in 1940 as an intermediate observation: a Stayer
       was in the same county at birth, in 1940, and at death; a Return Migrant died in their birth county but was
       living elsewhere in 1940; a Migrant died in a county other than their birth county.
       Models adjust for demographic controls and birth year, birth state, urban-rural, and occupation fixed effects.
       Estimates refer to a 10-point increase in Dissimilarity.", 100)) +
  theme_cowplot() +
  theme(plot.caption    = element_text(hjust = 0),
        legend.position = "bottom")

ggsave(mig3_plot, filename = here("FigTab", "mig3_estimates.jpeg"),
       width = 8, height = 6, dpi = 1000)
