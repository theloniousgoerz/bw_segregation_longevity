# # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Regression Models 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## Packages
rm(list=ls())
library(tidyverse)
library(magrittr)
library(readr)
library(modelsummary)
library(kableExtra)
library(gt)
library(cowplot)
library(fixest)
library(marginaleffects)
library(broom)
library(tinytable)
library(ggbrace)
library(gompertztrunc)
library(car)
library(binsreg)
library(ivDiag)
library(tidytext)
library(here)
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# The F-test helper used by the education section is ftest_vals(), defined just
# above that section.
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Data
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
source(here("Analysis","00_Helpers.R"))

data_a =   read_csv(here("Data","_Cleaned","data_a.csv"))
db =       read_csv(here("Data","_Cleaned","db.csv"))
dw =       read_csv(here("Data","_Cleaned","dw.csv"))
db_f=      read_csv(here("Data","_Cleaned","db_f.csv"))
dw_f=      read_csv(here("Data","_Cleaned","dw_f.csv"))
mechanism = read_csv(here("Data","_Cleaned","mechanism.csv"))
income_seg = read_csv(here("Data","_Cleaned","income_segregation_Hr.csv"))

# Repair birth_fips and derive migration status from the FIPS codes. The stored
# `migrated` has its two labels swapped, and STATEFIP_b -- the birth-state fixed effect
# in every model below -- was wrong for the rows whose birth_fips lost or gained a
# leading zero. See prepare_analysis_data() in 00_Helpers.R. This also adds the `mover`
# factor used by the movers-vs-stayers models below and drops anyone whose birth county
# is unrecorded, so every model here is fit on people with an observed migration status.
data_a %<>% prepare_analysis_data("data_a")
db     %<>% prepare_analysis_data("db")
dw     %<>% prepare_analysis_data("dw")
db_f   %<>% prepare_analysis_data("db_f")
dw_f   %<>% prepare_analysis_data("dw_f")

# educ_cat comes back from read_csv() as plain character, so any model that uses it
# (bare or interacted) would otherwise let feols/fixest coerce it to a factor on the
# fly and default to alphabetical levels -- making "College+" the omitted reference
# category instead of "Less than HS", since "College+" < "High School" < "Less than
# HS" < "Some College" alphabetically. That silently mislabels every education
# interaction coefficient below (e.g. "D x High School" would actually read as High
# School vs. College+, not vs. Less than HS) and drops College+ from any coef_map
# keyed on "...Less than HS" as the reference. Releveling explicitly here makes
# "Less than HS" the reference everywhere educ_cat is used in this script.
educ_levels <- c("Less than HS", "High School", "Some College", "College+")
db     %<>% mutate(educ_cat = factor(educ_cat, levels = educ_levels))
dw     %<>% mutate(educ_cat = factor(educ_cat, levels = educ_levels))
db_f   %<>% mutate(educ_cat = factor(educ_cat, levels = educ_levels))
dw_f   %<>% mutate(educ_cat = factor(educ_cat, levels = educ_levels))

# Merge
mechanisms = mechanism  %>% select(
  death_decade,
  death_fips, 
  starts_with("comp"),
  county_lib_index,
  lib_index_final,
  welf_direct_pc,
  health_pc,
  cash_asst_pc,
  medicaid_pc,
  taxes_pc,
  prop_tax_pc,
  Hr_all
) %>% distinct()


data_a %<>% left_join(.,mechanisms, by = c("death_decade","death_fips"))
db %<>% left_join(.,mechanisms, by = c("death_decade","death_fips"))
dw %<>% left_join(.,mechanisms, by = c("death_decade","death_fips"))

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Specification vocabulary
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Every model below is built from these pieces via fixest's .[ ] interpolation, so
# a covariate set, fixed-effect set, or instrument set is written down exactly once
# and changing it here changes it everywhere. Spelling these out at each call site
# is what previously let the rivers instrument and the vcov argument drift apart
# between the main models and their own robustness checks.

# All models cluster on county of death.
CL <- ~death_fips

# ---- Covariates ----
# Named for what they contain, not for who uses them: the gender splits drop `male`
# because each model is fit on one sex, and the education sets replace `education`
# with the categorical `educ_cat`.
cov_main    <- "male + migrated + education + married + south"  # main IV models
cov_nosouth <- "male + migrated + education + married"          # cov_main without south
cov_gender  <- "migrated + education + married + south"         # cov_main without male
cov_gen_sib <- "migrated + education + married"                 # without male or south
cov_ed      <- "male + migrated + married + south"              # education models
cov_ed_sib  <- "male + migrated + married"                      # education x sibling FE

# ---- Fixed effects ----
fe_base  <- "byear + STATEFIP_b + urb_code"
fe_main  <- "byear + STATEFIP_b + urb_code + OCC"
fe_sib_e <- "byear + STATEFIP_b + urb_code + OCC + sib_group_id_exact"
fe_sib_f <- "byear + STATEFIP_b + urb_code + OCC + sib_group_id_flexible"

# ---- Instruments ----
# Endogenous regressor is county_dism unless an alternative measure is substituted.
instr_rdi <- "rdi + rail_km_per_km2"
instr_riv <- "n_named_rivers + stream_km_per_km2"
iv_rdi    <- paste("county_dism ~", instr_rdi)
iv_riv    <- paste("county_dism ~", instr_riv)

# Interacted versions, derived from the same instrument sets so they cannot drift
# away from the main models. Generalized over the moderator variable so both the
# education interaction and the south interaction share one construction.
interact_with <- function(instr, var) {
  paste(paste0(trimws(strsplit(instr, "\\+")[[1]]), "*", var), collapse = " + ")
}
iv_rdi_ed    <- paste("county_dism*educ_cat ~", interact_with(instr_rdi, "educ_cat"))
iv_riv_ed    <- paste("county_dism*educ_cat ~", interact_with(instr_riv, "educ_cat"))
iv_rdi_south <- paste("county_dism*south ~",    interact_with(instr_rdi, "south"))

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Establish OLS relationship
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ------------------------------- Sibling models (defined here so the OLS and -------------------------------
# sibling FE tables below can both reference them; the sibling-sample OLS fits
# depend on the sibling FE fits via obs()).

# Exact Matches
sib_m2_b = feols(death_age~county_dism + .[cov_nosouth] |.[fe_sib_e], data = db_f, vcov = CL)
sib_m2_w = feols(death_age~county_dism + .[cov_nosouth] |.[fe_sib_e], data = dw_f, vcov = CL)

# Flexible Matches
sib_2_m2_b = feols(death_age~county_dism + .[cov_nosouth] |.[fe_sib_f], data = db_f, vcov = CL)
sib_2_m2_w = feols(death_age~county_dism + .[cov_nosouth] |.[fe_sib_f], data = dw_f, vcov = CL)

# OLS on the sibling samples, without the sibling group FE. These use cov_main
# (i.e. including `south`) so that every covariate-adjusted OLS fit carries the same
# control set as the IV models it is compared against.
ols_sib_e_b = feols(death_age~county_dism + .[cov_main] |.[fe_main], data = db_f[obs(sib_m2_b), ], vcov = CL)
ols_sib_e_w = feols(death_age~county_dism + .[cov_main] |.[fe_main], data = dw_f[obs(sib_m2_w), ], vcov = CL)
ols_sib_f_b = feols(death_age~county_dism + .[cov_main] |.[fe_main], data = db_f[obs(sib_2_m2_b), ], vcov = CL)
ols_sib_f_w = feols(death_age~county_dism + .[cov_main] |.[fe_main], data = dw_f[obs(sib_2_m2_w), ], vcov = CL)

# ------------------------------- OLS -------------------------------
# The Base columns take no covariates, matching the Base IV models (d1_*); the
# Controls columns take cov_main, matching the adjusted IV models (d2_*), so the
# OLS-IV contrast reported in the results section differs only in the estimator.
ols_m1_b = feols(death_age~county_dism |.[fe_base], data = db, vcov = CL)
ols_m1_w = feols(death_age~county_dism |.[fe_base], data = dw, vcov = CL)
ols_m2_b = feols(death_age~county_dism + .[cov_main] |.[fe_main], data = db, vcov = CL)
ols_m2_w = feols(death_age~county_dism + .[cov_main] |.[fe_main], data = dw, vcov = CL)

  msummary(list("Base"     = ols_m1_b,
                "Controls" = ols_m2_b,
                "Exact"    = ols_sib_e_b,
                "Flexible" = ols_sib_f_b,
                "Base"     = ols_m1_w,
                "Controls" = ols_m2_w,
                "Exact"    = ols_sib_e_w,
                "Flexible" = ols_sib_f_w),fmt =3, stars = T,
           coef_map = c("county_dism" = "D",
                        "male" = "Male",
                        "education" = "Education",
                        "migratedMigrated" = "Migrated",
                        "married" = "Married in 1940",
                        "south" = "Died in South"),
           gof_map = c("nobs",
                       "r.squared"
           ),
           notes = "Cluster-robust standard errors (by county of death) in parentheses. The Exact and Flexible columns are OLS estimated on the exact- and flexible-match sibling samples, without the sibling group fixed effect. The Base columns take no covariates and the remaining columns take the same covariate set as the adjusted instrumental variable models.",
           title = "Estimates of the Association Betwen Segregation and Longevity",
           add_rows = data.frame(
             FE = c("Birth Year","Birth State","Urban-Rural Code","Occupation"),
             m1 = c("X","X","X","-"),
             m2 = c("X","X","X","X"),
             m3 = c("X","X","X","X"),
             m4 = c("X","X","X","X"),
             m5 = c("X","X","X","-"),
             m6 = c("X","X","X","X"),
             m7 = c("X","X","X","X"),
             m8 = c("X","X","X","X")
           )
           ) %>%
    group_tt(j = list("Black" = 2:5, "White" = 6:9)) %>%
    save_tt(.,output = here("FigTab","OLS_results_table.tex"), overwrite = T)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## IV analysis
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ------------------------------- Estimate IV Models (RDI) -------------------------------
  
  d1_b = feols(death_age~1|.[fe_base] |.[iv_rdi], data = db, vcov = CL)
  d1_w = feols(death_age~1|.[fe_base] |.[iv_rdi], data = dw, vcov = CL)
  d2_b = feols(death_age~.[cov_main] |.[fe_main] |.[iv_rdi], data = db, vcov = CL)
  d2_w = feols(death_age~.[cov_main] |.[fe_main] |.[iv_rdi], data = dw, vcov = CL)
  
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
             "fit_county_dism" = "D",
             "male" = "Male",
             "education" = "Education",
             "migratedMigrated" = "Migrated",
             "married" = "Married in 1940",
             "south" = "Died in South",
             "rdi" = "Railroad Division Index",
             "rail_km_per_km2" = "Rail km per km$^2$"),
           gof_map = c("nobs",
                       "r.squared",
                       "f"),
           align = "lcccccccc",
           notes = "This table describes the first-stage models and IV estimates of the effect of segregation on longevity. Cluster-robust standard errors (by county of death) in parentheses.",
           title = "Estimates of the Effect of Segregation on Longevity (RDI Instrument)",
           add_rows = data.frame(
             FE = c("Birth Year","Birth State","Urban-Rural Code","Occupation"),
             m1_fs = c("-","-","-","-"),
             m1 = c("X","X","X","-"),
             m2_fs = c("-","-","-","-"),
             m2 = c("X","X","X","X"),
             m3_fs = c("-","-","-","-"),
             m3 = c("X","X","X","-"),
             m4_fs = c("-","-","-","-"),
             m4 = c("X","X","X","X")
           ),
           threeparttable = TRUE
  ) %>%
    group_tt(j = list("Base" = 2:3, "Controls" = 4:5, "Base" = 6:7, "Controls" = 8:9)) %>%
    group_tt(j = list("Black" = 2:5, "White" = 6:9)) %>%
    save_tt(., output = here("FigTab","IV_results_table_rdi.tex"), overwrite = T)
  
  
# ------------------------------- Estimate IV Models (Rivers) -------------------------------

r1_b = feols(death_age~1|.[fe_base] |.[iv_riv], data = db, vcov = CL)
r1_w = feols(death_age~1|.[fe_base] |.[iv_riv], data = dw, vcov = CL)
r2_b = feols(death_age~.[cov_main] |.[fe_main] |.[iv_riv], data = db, vcov = CL)
r2_w = feols(death_age~.[cov_main] |.[fe_main] |.[iv_riv], data = dw, vcov = CL)

msummary(list("1st" = summary(r1_b, stage = 1),
              "IV"  = r1_b,
              "1st" = summary(r2_b, stage = 1),
              "IV"  = r2_b,
              "1st" = summary(r1_w, stage = 1),
              "IV"  = r1_w,
              "1st" = summary(r2_w, stage = 1),
              "IV"  = r2_w),
         fmt = 3,
         stars = T,
         coef_map = c(
                      "fit_county_dism" = "D",
                      "male" = "Male",
                      "education" = "Education",
                      "migratedMigrated" = "Migrated",
                      "married" = "Married in 1940",
                      "south" = "Died in South",
                      "n_named_rivers" = "Named Rivers",
                      "stream_km_per_km2" = "Named Rivers (mi2)"),
         gof_map = c("nobs",
                     "r.squared",
                     "f"),
         align = "lcccccccc",
         notes = "This table describes the first-stage models and IV estimates of the effect of segregation on longevity. Cluster-robust standard errors (by county of death) in parentheses.",
         title = "Estimates of the Effect of Segregation on Longevity (Rivers Instrument)",
         add_rows = data.frame(
           FE = c("Birth Year","Birth State","Urban-Rural Code","Occupation"),
           m1_fs = c("-","-","-","-"),
           m1 = c("X","X","X","-"),
           m2_fs = c("-","-","-","-"),
           m2 = c("X","X","X","X"),
           m3_fs = c("-","-","-","-"),
           m3 = c("X","X","X","-"),
           m4_fs = c("-","-","-","-"),
           m4 = c("X","X","X","X")
         ),
         threeparttable = TRUE
         ) %>%
  group_tt(j = list("Base" = 2:3, "Controls" = 4:5, "Base" = 6:7, "Controls" = 8:9)) %>%
  group_tt(j = list("Black" = 2:5, "White" = 6:9)) %>%
  save_tt(., output = here("FigTab","IV_results_table_rivers.tex"), overwrite = T)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Sibling FE Robustness

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# The sibling FE models (sib_m2_*, sib_2_m2_*) and their sibling-sample OLS
# counterparts (ols_sib_*) are defined above, near the OLS table.

# ------------------------------- Sibling FE table (unpaired vs. paired) -------------------------------
# Within each sample x race pair, "Unpaired" is OLS without the family FE and
# "Paired" adds the sibling group FE, so the sample is held fixed.
# NOTE: the Unpaired columns now carry cov_main (including `south`) so they match the
# OLS table and the IV models, while the Paired columns still carry cov_nosouth. If the
# sibling FE models are moved to cov_main as well, this table returns to differing only
# in the family fixed effect.
msummary(list("Unpaired" = ols_sib_e_b,
              "Paired"    = sib_m2_b,
              "Unpaired" = ols_sib_e_w,
              "Paired"    = sib_m2_w,
              "Unpaired" = ols_sib_f_b,
              "Paired"    = sib_2_m2_b,
              "Unpaired" = ols_sib_f_w,
              "Paired"    = sib_2_m2_w),
         fmt = 3, stars = T,
         coef_map = c("county_dism" = "D",
                      "male" = "Male",
                      "education" = "Education",
                      "migratedMigrated" = "Migrated",
                      "married" = "Married in 1940",
                      "south" = "Died in South"),
         gof_map = c("nobs",
                     "r.squared"),
         notes = "Cluster-robust standard errors (by county of death) in parentheses. Within each sample, Unpaired is OLS without the sibling group fixed effect and Paired adds it, holding the estimation sample fixed. The Unpaired columns additionally control for dying in the South, matching the covariate set of the instrumental variable models; this control is absorbed by the family fixed effect for sibling pairs who died in the same region.",
         title = "Sibling Fixed Effects Estimates of the Effect of Segregation on Longevity",
         add_rows = data.frame(
           FE = c("Birth Year","Birth State","Urban-Rural Code","Occupation","Sibling FE"),
           m1 = c("X","X","X","X","-"),
           m2 = c("X","X","X","X","X"),
           m3 = c("X","X","X","X","-"),
           m4 = c("X","X","X","X","X"),
           m5 = c("X","X","X","X","-"),
           m6 = c("X","X","X","X","X"),
           m7 = c("X","X","X","X","-"),
           m8 = c("X","X","X","X","X")
         ),
         threeparttable = TRUE
         ) %>%
  group_tt(j = list("Black" = 2:3, "White" = 4:5, "Black" = 6:7, "White" = 8:9)) %>%
  group_tt(j = list("Exact Match" = 2:5, "Flexible Match" = 6:9)) %>%
  save_tt(., output = here("FigTab","sibling_fe_table.tex"), overwrite = T)


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Mechanisms
db_mech = db %>% filter(!is.na(taxes_pc))
dw_mech = dw %>% filter(!is.na(taxes_pc))

# The `_m` suffix here means "mechanism". Each model adds one policy channel to the
# main adjusted RDI IV specification, refit on the subsample with policy data. The
# `_all` models add every channel at once, so the table reads baseline -> one
# channel at a time -> all channels jointly.
#
# SNAP and property taxes are not reported. Segregation has no detectable effect on
# either in the first leg (see the OLS/IV mechanism table in 02_Mechanisms.R), so any
# movement in D from conditioning on them cannot be a mediated share. comp_snap in
# particular is measured at the state-decade level rather than the county level, so
# conditioning on it absorbs state policy regime and cohort variation unrelated to
# any given county's segregation.

# The joint mechanism control set, written once so the Black and White fits and the
# figure below cannot drift apart.
mech_all <- "taxes_pc + comp_medicaid + health_pc + welf_direct_pc"

# --------------------------
# Black
d2_b_m          = feols(death_age~.[cov_main]                  |.[fe_main] |.[iv_rdi], data = db_mech, vcov = CL)
d2_b_m_tax      = feols(death_age~.[cov_main] + taxes_pc       |.[fe_main] |.[iv_rdi], data = db_mech, vcov = CL)
d2_b_m_med      = feols(death_age~.[cov_main] + comp_medicaid  |.[fe_main] |.[iv_rdi], data = db_mech, vcov = CL)
d2_b_m_heal     = feols(death_age~.[cov_main] + health_pc      |.[fe_main] |.[iv_rdi], data = db_mech, vcov = CL)
d2_b_m_welf     = feols(death_age~.[cov_main] + welf_direct_pc |.[fe_main] |.[iv_rdi], data = db_mech, vcov = CL)
d2_b_m_all      = feols(death_age~.[cov_main] + .[mech_all]    |.[fe_main] |.[iv_rdi], data = db_mech, vcov = CL)

# --------------------------
# White
d2_w_m          = feols(death_age~.[cov_main]                  |.[fe_main] |.[iv_rdi], data = dw_mech, vcov = CL)
d2_w_m_tax      = feols(death_age~.[cov_main] + taxes_pc       |.[fe_main] |.[iv_rdi], data = dw_mech, vcov = CL)
d2_w_m_med      = feols(death_age~.[cov_main] + comp_medicaid  |.[fe_main] |.[iv_rdi], data = dw_mech, vcov = CL)
d2_w_m_heal     = feols(death_age~.[cov_main] + health_pc      |.[fe_main] |.[iv_rdi], data = dw_mech, vcov = CL)
d2_w_m_welf     = feols(death_age~.[cov_main] + welf_direct_pc |.[fe_main] |.[iv_rdi], data = dw_mech, vcov = CL)
d2_w_m_all      = feols(death_age~.[cov_main] + .[mech_all]    |.[fe_main] |.[iv_rdi], data = dw_mech, vcov = CL)

msummary(list(
  # Black
  "Black"                         = d2_b_m,
  "Black\\newline + Taxes"        = d2_b_m_tax,
  "Black\\newline + Medicaid"     = d2_b_m_med,
  "Black\\newline + Health" =       d2_b_m_heal,
  "Black\\newline + Welfare"      = d2_b_m_welf,
  "Black\\newline + All"          = d2_b_m_all,
  # White
  "White"                         = d2_w_m,
  "White\\newline + Taxes"        = d2_w_m_tax,
  "White\\newline + Medicaid"     = d2_w_m_med,
  "White\\newline + Health"   =     d2_w_m_heal,
  "White\\newline + Welfare"      = d2_w_m_welf,
  "White\\newline + All"          = d2_w_m_all
),
# 4 decimals (not 3): at 3dp the rounded D coefficients no longer reproduce the
# mediated shares reported in the text and in mechanism_coefplot.jpeg.
fmt   = 4,
stars = TRUE,
coef_map = c(
  "fit_county_dism"  = "D"
),
gof_map = c("nobs", "r.squared", "f"),
notes   = "IV estimates (RDI instrument). Each column adds one mechanism as a control, and the All columns add every mechanism jointly. Cluster-robust SEs (by county of death) in parentheses. The mediated shares reported in the text and in the mechanism figure are computed from the unrounded coefficients and will not reproduce exactly from the rounded entries here.",
title   = "Mechanism Estimates: Effect of Segregation on Longevity Controlling for Policy Channels",
threeparttable = TRUE
) %>%
  # Fixed column widths + smaller type so the 13-column table fits the landscape
  # text block (650pt); at natural width it overruns by ~390pt. The two Medicaid
  # columns get extra width so "+ Medicaid" stays on one line.
  style_tt(j = 1,
           tabularray_inner = "colsep=2pt, row{1-Z}={font=\\footnotesize}, colspec={Q[l,wd=1.5cm]Q[c,wd=1.55cm]Q[c,wd=1.55cm]Q[c,wd=1.72cm]Q[c,wd=1.55cm]Q[c,wd=1.55cm]Q[c,wd=1.55cm]Q[c,wd=1.55cm]Q[c,wd=1.55cm]Q[c,wd=1.72cm]Q[c,wd=1.55cm]Q[c,wd=1.55cm]Q[c,wd=1.55cm]}") %>%
  save_tt(., output = here("FigTab", "mechanism_table.tex"), overwrite = TRUE)

# -----------------------
# Mechanism coefficient plot
# -----------------------
black_models <- list(
  "Baseline"     = d2_b_m,
  "Taxes"        = d2_b_m_tax,
  "Medicaid"     = d2_b_m_med,
  "Health" = d2_b_m_heal,
  "Welfare"     = d2_b_m_welf,
  "All Mechanisms" = d2_b_m_all
)
white_models <- list(
  "Baseline"     = d2_w_m,
  "Taxes"        = d2_w_m_tax,
  "Medicaid"     = d2_w_m_med,
  "Health" = d2_w_m_heal,
  "Welfare"     = d2_w_m_welf,
  "All Mechanisms" = d2_w_m_all
)

coef_black <- map_dfr(black_models,
  \(m) broom::tidy(m, conf.int = TRUE) |> filter(term == "fit_county_dism"),
  .id = "mechanism"
) |> mutate(race = "Black")

coef_white <- map_dfr(white_models,
  \(m) broom::tidy(m, conf.int = TRUE) |> filter(term == "fit_county_dism"),
  .id = "mechanism"
) |> mutate(race = "White")

base_b <- coef_black |> filter(mechanism == "Baseline") |> pull(estimate)
base_w <- coef_white |> filter(mechanism == "Baseline") |> pull(estimate)

# Rescaled by 10 so the y-axis reads in the same "10-point rise in D" units as every
# other main-estimate figure (OLS, IV-all, migration status, birth cohort, military
# service), which is what makes this figure's shared axis (below) comparable to theirs.
# Mechanism order is fixed (not per-race sorted) so Black and White dodge to the same
# x position within each mechanism -- required now that estimate sits on the y-axis
# alongside every other main-estimate figure, rather than on its own flipped x-axis.
coef_all <- bind_rows(coef_black, coef_white) |>
  mutate(
    pct_baseline = case_when(
      race == "Black" ~ estimate / base_b * 100,
      race == "White" ~ estimate / base_w * 100
    ),
    pct_label = paste0(round(pct_baseline, 1), "%"),
    mechanism = factor(mechanism,
                        levels = c("Baseline", "Taxes", "Medicaid", "Health",
                                   "Welfare", "All Mechanisms")),
    estimate  = estimate  * 10,
    conf.low  = conf.low  * 10,
    conf.high = conf.high * 10
  )

baseline_lines <- data.frame(
  race       = c("Black", "White"),
  yintercept = c(base_b,  base_w) * 10
)

# The % labels sit past conf.high, so cache extra headroom on the upper end (the
# widest label is ~4 characters; a fixed data-unit buffer scaled to this axis's own
# spread stands in for their pixel width) or the shared axis clips the text.
mech_label_buffer <- diff(range(coef_all$conf.low, coef_all$conf.high)) * 0.12
cache_estimate_range("mechanism", coef_all$conf.low, coef_all$conf.high + mech_label_buffer)

mech_plot <-
  ggplot(coef_all, aes(x = mechanism, y = estimate,
                        ymin = conf.low, ymax = conf.high,
                        color = race, shape = race)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", lwd = 1) +
  geom_hline(data = baseline_lines,
             aes(yintercept = yintercept, color = race),
             linetype = "dotted", linewidth = 1, show.legend = FALSE) +
  geom_pointrange(position = position_dodge(width = 0.5), size = .75, lwd = .75) +
  geom_text(aes(y = conf.high, label = pct_label),
            position = position_dodge(width = 0.5),
            vjust = -0.7, size = 4, show.legend = FALSE) +
  scale_color_manual(values = c("Black" = "darkgreen", "White" = "darkblue")) +
  scale_shape_manual(values = c("Black" = 16, "White" = 17)) +
  # This figure only: fixed 0 to -1 year axis (not the shared MAIN_ESTIMATE_FIGS
  # range), per explicit request.
  coord_cartesian(ylim = c(-1, 0)) +
  labs(
    y     = "Change in Life Expectancy",
    x     = NULL,
    color = "Race",
    shape = "Race",
    title = NULL,
    subtitle = "Label shows coefficient as % of baseline model (no mechanism control)"
  ) +
  theme_cowplot() +
  theme(
    legend.position = "bottom",
    axis.text.x     = element_text(angle = 25, hjust = 1),
    # Sized so a 9.5pt note survives the ~0.46x shrink from this plot's 14in ggsave
    # width down to \textwidth (6.5in) on the printed page.
    plot.subtitle   = element_text(size = 20)
  )

ggsave(mech_plot, filename = here("FigTab", "mechanism_coefplot.jpeg"),
       width = 14, height = 6, dpi = 1000)



# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Figures

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ------------------------------- Instrument Figure -------------------------------
instrument = data_a %>%
  distinct(county_dism, death_decade, death_fips,
           rdi, rail_km_per_km2,
           n_named_rivers, n_named_rivers_sq, stream_km_per_km2)

# ------------------------------- Binscatter regressions -------------------------------
# Each panel bins segregation against the focal instrument, residualising on the
# remaining instrument(s) in that strategy via the binsreg `w` argument.
bins_rdi = binsreg(instrument$county_dism, instrument$rdi,
                   at = "mean",
                   w  = instrument$rail_km_per_km2)
rdi_data = as.data.frame(bins_rdi$data.plot)

bins_riv = binsreg(instrument$county_dism, instrument$n_named_rivers,
                   at = "mean",
                   w  = instrument$stream_km_per_km2)
riv_data = as.data.frame(bins_riv$data.plot)

# First-stage F-statistics, computed here so the captions cannot go stale. These use
# the same instrument sets as the IV models above, so the reported F describes the
# first stage the paper actually estimates.
fs_rdi = feols(county_dism~.[instr_rdi] | death_decade, data = instrument, vcov = CL)
fs_riv = feols(county_dism~.[instr_riv] | death_decade, data = instrument, vcov = CL)

f_rdi_val = unlist(fitstat(fs_rdi, type = "f"))[1]
f_riv_val = unlist(fitstat(fs_riv, type = "f"))[1]

# ------------------------------- Panel A: RDI -------------------------------
bs_rdi =
  ggplot(data = rdi_data, aes(Group.Full.Sample.data.dots.x, Group.Full.Sample.data.dots.fit)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm",
              alpha = 0,
              lwd = 2) +
  labs(x = "Railroad Division Index",
       y = "County Dissimilarity",
       subtitle = paste0("F-statistic = ", round(f_rdi_val, 2))) +
  theme_cowplot()

# ------------------------------- Panel B: Rivers -------------------------------
bs_riv =
  ggplot(data = riv_data, aes(Group.Full.Sample.data.dots.x, Group.Full.Sample.data.dots.fit)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm",
              alpha = 0,
              lwd = 2) +
  labs(x = "Number of Named Rivers",
       y = "County Dissimilarity",
       subtitle = paste0("F-statistic = ", round(f_riv_val, 2))) +
  theme_cowplot()

# ------------------------------- Combine Panels -------------------------------
fs_panels = plot_grid(bs_rdi, bs_riv,
                      labels = c("A", "B"),
                      nrow   = 1,
                      align  = "hv",
                      axis   = "tblr")

fs_caption = ggdraw() +
  # Sized so a 9.5pt note survives the ~0.54x shrink from this plot's 12in ggsave
  # width down to \textwidth (6.5in) on the printed page.
  draw_label(
    str_wrap("This figure displays the association between each instrument and segregation.
             Segregation is measured by the index of dissimilarity that measures how evenly distributed Black and White residents are within a county.
             Panel A displays the Railroad Division Index first stage, adjusting for railroad density.
             Panel B displays the named rivers first stage, adjusting for stream density.
             Dots represent means of bins at each level of the instrument.
             The blue line corresponds to the fitted OLS regression line of segregation on the instrument.", 88),
    x = 0, y = 1, hjust = 0, vjust = 1, size = 18
  ) +
  theme(plot.margin = margin(0, 0, 0, 7))

# rel_heights and the overall height below are sized for the caption's ~7 wrapped
# lines at size 18 (see draw_label above); anchoring the label to the top of its
# slot (y = 1, vjust = 1) keeps it from drifting into the panels above if the two
# ever fall slightly out of sync again.
fs_plot_all = plot_grid(fs_panels, fs_caption,
                        ncol        = 1,
                        rel_heights = c(1, 0.45))

# ------------------------------- Save -------------------------------
ggsave(fs_plot_all, filename = here("FigTab","fs_plot.jpeg"),
       width = 12,
       height = 8,
       dpi = 1000)

# ------------------------------- Create Plots -------------------------------
# Set Contrast for Plots
contrast = 10

# ------------------------------- All Estimates Plot -------------------------------

all_estimates_plot_data = bind_rows(
  # Rivers IV
  #tidy(r1_b, conf.int = T) |> filter(term == "fit_county_dism") |> mutate(Race = "Black", Model = "Unadjusted", Estimator = "Rivers IV"),
  #tidy(r1_w, conf.int = T) |> filter(term == "fit_county_dism") |> mutate(Race = "White", Model = "Unadjusted", Estimator = "Rivers IV"),
  tidy(r2_b, conf.int = T) |> filter(term == "fit_county_dism") |> mutate(Race = "Black", Model = "Controls",    Estimator = "Rivers IV"),
  tidy(r2_w, conf.int = T) |> filter(term == "fit_county_dism") |> mutate(Race = "White", Model = "Controls",    Estimator = "Rivers IV"),
  # RDI IV
  #tidy(d1_b, conf.int = T) |> filter(term == "fit_county_dism") |> mutate(Race = "Black", Model = "Unadjusted", Estimator = "RDI IV"),
  #tidy(d1_w, conf.int = T) |> filter(term == "fit_county_dism") |> mutate(Race = "White", Model = "Unadjusted", Estimator = "RDI IV"),
  tidy(d2_b, conf.int = T) |> filter(term == "fit_county_dism") |> mutate(Race = "Black", Model = "Controls",    Estimator = "RDI IV"),
  tidy(d2_w, conf.int = T) |> filter(term == "fit_county_dism") |> mutate(Race = "White", Model = "Controls",    Estimator = "RDI IV"),
  # Sibling FE — Exact match
  tidy(sib_m2_b, conf.int = T) |> filter(term == "county_dism") |> mutate(Race = "Black", Model = "Controls", Estimator = "Sib. FE (Exact)"),
  tidy(sib_m2_w, conf.int = T) |> filter(term == "county_dism") |> mutate(Race = "White", Model = "Controls", Estimator = "Sib. FE (Exact)"),
  # Sibling FE — Flexible match
  tidy(sib_2_m2_b, conf.int = T) |> filter(term == "county_dism") |> mutate(Race = "Black", Model = "Controls", Estimator = "Sib. FE (Flexible)"),
  tidy(sib_2_m2_w, conf.int = T) |> filter(term == "county_dism") |> mutate(Race = "White", Model = "Controls", Estimator = "Sib. FE (Flexible)")
) |>
  mutate(
    estimate  = estimate  * contrast,
    conf.high = conf.high * contrast,
    conf.low  = conf.low  * contrast,
    Model     = factor(Model, levels = c("Unadjusted", "Controls")),
    Estimator = factor(Estimator, levels = c("RDI IV","Rivers IV","Sib. FE (Flexible)","Sib. FE (Exact)"))
  )

cache_estimate_range("iv_all", all_estimates_plot_data$conf.low, all_estimates_plot_data$conf.high)

all_estimates_plot =
  ggplot(all_estimates_plot_data,
         aes(Estimator, estimate,
             ymin = conf.low,
             ymax = conf.high,
             color = Race,
             shape = Race)) +
  geom_pointrange(position = position_dodge2(width = .5),
                  size = .75,
                  lwd = .75) +
  labs(y = "Change in Life Expectancy",
       x = NULL,
       caption = str_wrap("This figure displays estimates from all specifications of the effect of racial segregation on longevity.
       IV models use named rivers (Rivers IV) and the Railroad Division Index (RDI IV) as instruments.
       Sibling FE models include birth year, urban-rural, birth state, and occupation fixed effects plus a sibling group fixed effect.
       All estimates refer to a 10-point increase in Dissimilarity.", 100)) +
  scale_color_manual(values = c("Black" = "darkgreen", "White" = "darkblue")) +
  scale_shape_manual(values = c("Black" = 16, "White" = 17)) +
  coord_cartesian(ylim = shared_axis_range(MAIN_ESTIMATE_FIGS)) +
  theme_cowplot() +
  geom_hline(yintercept = 0, linetype = "dashed", lwd = 1) +
  # Caption size is set so a 9.5pt note survives the ~0.46x shrink from this plot's
  # 14in ggsave width down to \textwidth (6.5in) on the printed page.
  theme(plot.caption    = element_text(hjust = 0, size = 20),
        legend.position = "bottom",
        axis.text.x     = element_text(angle = 25, hjust = 1))

ggsave(all_estimates_plot, filename = here("FigTab","iv_plot_all.jpeg"),
       width = 14,
       height = 6,
       dpi = 1000)

# ------------------------------- Per-strategy adjusted vs. unadjusted figures -------------------------------
# Each figure isolates one identification strategy and contrasts the unadjusted
# and adjusted specifications for Black and White, mirroring all_estimates_plot.

# Shared plot skeleton for the adjusted/unadjusted contrasts.
adj_contrast_plot = function(dat, caption_text) {
  ggplot(dat,
         aes(Model, estimate,
             ymin  = conf.low,
             ymax  = conf.high,
             color = Race)) +
    geom_pointrange(position = position_dodge2(width = .5),
                    size = .75, lwd = .75, shape = 22) +
    geom_hline(yintercept = 0, linetype = "dashed", lwd = 1) +
    scale_color_manual(values = c("Black" = "darkgreen", "White" = "darkblue")) +
    labs(y = "Change in Life Expectancy", x = NULL,
         caption = str_wrap(caption_text, 100)) +
    theme_cowplot() +
    theme(plot.caption    = element_text(hjust = 0),
          legend.position = "bottom")
}

scale_est = function(d) {
  d |> mutate(estimate  = estimate  * contrast,
              conf.high = conf.high * contrast,
              conf.low  = conf.low  * contrast,
              Model     = factor(Model, levels = c("Unadjusted", "Adjusted")))
}

# ------------------------------- RDI IV figure -------------------------------
rdi_iv_plot_data = bind_rows(
  tidy(d1_b, conf.int = T) |> filter(term == "fit_county_dism") |> mutate(Race = "Black", Model = "Unadjusted"),
  tidy(d1_w, conf.int = T) |> filter(term == "fit_county_dism") |> mutate(Race = "White", Model = "Unadjusted"),
  tidy(d2_b, conf.int = T) |> filter(term == "fit_county_dism") |> mutate(Race = "Black", Model = "Adjusted"),
  tidy(d2_w, conf.int = T) |> filter(term == "fit_county_dism") |> mutate(Race = "White", Model = "Adjusted")
) |> scale_est()

rdi_iv_plot = adj_contrast_plot(
  rdi_iv_plot_data,
  "IV estimates of the effect of segregation on longevity using the Railroad Division Index instrument.
   Unadjusted models include birth year, birth state, and urban-rural fixed effects; Adjusted models add
   demographic controls and occupation fixed effects. Estimates refer to a 10-point increase in Dissimilarity."
)

ggsave(rdi_iv_plot, filename = here("FigTab","rdi_iv_estimates.jpeg"),
       width = 8, height = 6, dpi = 1000)

# ------------------------------- RDI x South marginal effects figure -------------------------------
# Disabled: this block calls avg_slopes() on d2_b_south/d2_w_south, the south-interacted
# models it says are "fit above", but no such models are ever fit anywhere in this
# script (only the interacted instrument formula iv_rdi_south exists) -- an orphaned,
# broken block from before that fit step was removed. south_marginal_effects.jpeg is
# not referenced anywhere in the manuscript, so this is left disabled rather than
# guessing at the intended model spec.
#
# extract_south_slopes <- function(model, data_ref, race_label) {
#   avg_slopes(model,
#              variables = "county_dism",
#              by        = "south",
#              newdata   = datagrid(
#                south       = unique(data_ref$south),
#                county_dism = unique(data_ref$county_dism)
#              )) |>
#     tidy(conf.int = TRUE) |>
#     mutate(Race = race_label)
# }
#
# south_slopes_plot_data = bind_rows(
#   extract_south_slopes(d2_b_south, db, "Black"),
#   extract_south_slopes(d2_w_south, dw, "White")
# ) |>
#   mutate(
#     south     = factor(south, levels = c(0, 1), labels = c("Non-South", "South")),
#     estimate  = estimate  * contrast,
#     conf.high = conf.high * contrast,
#     conf.low  = conf.low  * contrast
#   )
#
# south_slopes_plot =
#   ggplot(south_slopes_plot_data,
#          aes(Race, estimate, ymin = conf.low, ymax = conf.high, color = Race, shape = south)) +
#   geom_pointrange(position = position_dodge2(width = .4), size = .75, lwd = .75) +
#   geom_hline(yintercept = 0, linetype = "dashed", lwd = 1, color = "gray50") +
#   scale_color_manual(values = c("Black" = "darkgreen", "White" = "darkblue")) +
#   labs(y = "Change in Life Expectancy",
#        x = NULL,
#        caption = str_wrap("Marginal effects of segregation on longevity for those who died in the South
#        versus outside the South, from a model with D interacted with dying in the South (RDI instrument),
#        adjusting for demographic controls and fixed effects. Estimates refer to a 10-point increase in
#        Dissimilarity.", 100)) +
#   theme_cowplot() +
#   theme(plot.caption    = element_text(hjust = 0),
#         legend.position = "bottom")
#
# ggsave(south_slopes_plot, filename = here("FigTab","south_marginal_effects.jpeg"),
#        width = 8, height = 6, dpi = 1000)

# ------------------------------- Rivers IV figure -------------------------------
rivers_iv_plot_data = bind_rows(
  tidy(r1_b, conf.int = T) |> filter(term == "fit_county_dism") |> mutate(Race = "Black", Model = "Unadjusted"),
  tidy(r1_w, conf.int = T) |> filter(term == "fit_county_dism") |> mutate(Race = "White", Model = "Unadjusted"),
  tidy(r2_b, conf.int = T) |> filter(term == "fit_county_dism") |> mutate(Race = "Black", Model = "Adjusted"),
  tidy(r2_w, conf.int = T) |> filter(term == "fit_county_dism") |> mutate(Race = "White", Model = "Adjusted")
) |> scale_est()

rivers_iv_plot = adj_contrast_plot(
  rivers_iv_plot_data,
  "IV estimates of the effect of segregation on longevity using the named-rivers instrument.
   Unadjusted models include birth year, birth state, and urban-rural fixed effects; Adjusted models add
   demographic controls and occupation fixed effects. Estimates refer to a 10-point increase in Dissimilarity."
)

ggsave(rivers_iv_plot, filename = here("FigTab","rivers_iv_estimates.jpeg"),
       width = 8, height = 6, dpi = 1000)

# ------------------------------- Sibling FE figure -------------------------------
# Both specs keep the sibling group fixed effect; Unadjusted drops the demographic
# covariates and Adjusted keeps them. Exact and flexible matches are distinguished
# by point shape.
sib_m1_b   = feols(death_age~county_dism |.[fe_sib_e], data = db_f, vcov = CL)
sib_m1_w   = feols(death_age~county_dism |.[fe_sib_e], data = dw_f, vcov = CL)
sib_2_m1_b = feols(death_age~county_dism |.[fe_sib_f], data = db_f, vcov = CL)
sib_2_m1_w = feols(death_age~county_dism |.[fe_sib_f], data = dw_f, vcov = CL)

sibling_fe_plot_data = bind_rows(
  # Exact match
  tidy(sib_m1_b,   conf.int = T) |> filter(term == "county_dism") |> mutate(Race = "Black", Model = "Unadjusted", Match = "Exact"),
  tidy(sib_m1_w,   conf.int = T) |> filter(term == "county_dism") |> mutate(Race = "White", Model = "Unadjusted", Match = "Exact"),
  tidy(sib_m2_b,   conf.int = T) |> filter(term == "county_dism") |> mutate(Race = "Black", Model = "Adjusted",   Match = "Exact"),
  tidy(sib_m2_w,   conf.int = T) |> filter(term == "county_dism") |> mutate(Race = "White", Model = "Adjusted",   Match = "Exact"),
  # Flexible match
  tidy(sib_2_m1_b, conf.int = T) |> filter(term == "county_dism") |> mutate(Race = "Black", Model = "Unadjusted", Match = "Flexible"),
  tidy(sib_2_m1_w, conf.int = T) |> filter(term == "county_dism") |> mutate(Race = "White", Model = "Unadjusted", Match = "Flexible"),
  tidy(sib_2_m2_b, conf.int = T) |> filter(term == "county_dism") |> mutate(Race = "Black", Model = "Adjusted",   Match = "Flexible"),
  tidy(sib_2_m2_w, conf.int = T) |> filter(term == "county_dism") |> mutate(Race = "White", Model = "Adjusted",   Match = "Flexible")
) |>
  scale_est() |>
  mutate(Match = factor(Match, levels = c("Exact", "Flexible")))

sibling_fe_plot =
  ggplot(sibling_fe_plot_data,
         aes(Model, estimate,
             ymin  = conf.low,
             ymax  = conf.high,
             color = Race,
             shape = Match)) +
  geom_pointrange(position = position_dodge2(width = .6),
                  size = .75, lwd = .75) +
  geom_hline(yintercept = 0, linetype = "dashed", lwd = 1) +
  scale_color_manual(values = c("Black" = "darkgreen", "White" = "darkblue")) +
  scale_shape_manual(values = c("Exact" = 22, "Flexible" = 21)) +
  labs(y = "Change in Life Expectancy", x = NULL,
       caption = str_wrap("Sibling fixed effects estimates of the effect of segregation on longevity.
       All models include birth year, birth state, urban-rural, occupation, and sibling group fixed effects.
       Unadjusted models omit the demographic covariates that Adjusted models include. Exact and flexible
       sibling matches are shown separately. Estimates refer to a 10-point increase in Dissimilarity.", 100)) +
  theme_cowplot() +
  theme(plot.caption    = element_text(hjust = 0),
        legend.position = "bottom")

ggsave(sibling_fe_plot, filename = here("FigTab","sibling_fe_estimates.jpeg"),
       width = 8, height = 6, dpi = 1000)

# ------------------------------- All OLS Estimates Plot -------------------------------
# OLS analogue of the all-estimates plot: same samples, same controls and FE, but
# D enters directly rather than through an instrument, and the sibling group FE is
# dropped from the two sibling samples.

# Restrict each OLS fit to the rows its IV counterpart actually used, so the only
# thing that changes across estimators is the estimator itself.
ols_on_iv_sample = function(iv_mod, data) {
  feols(death_age~county_dism + .[cov_main] |.[fe_main],
        data = data[obs(iv_mod), ],
        vcov = CL)
}

# OLS on the IV estimation sample. The RDI and rivers IV models select exactly the
# same rows of db/dw, so a single OLS fit stands in for both rather than plotting
# two identical columns.
ols_iv_b = ols_on_iv_sample(d2_b, db)
ols_iv_w = ols_on_iv_sample(d2_w, dw)

# OLS on the sibling samples (ols_sib_*), without the sibling group FE, are
# defined above near the OLS table.

ols_estimates_plot_data = bind_rows(
  tidy(ols_iv_b,     conf.int = T) |> filter(term == "county_dism") |> mutate(Race = "Black", Model = "Controls", Estimator = "OLS (IV Sample)"),
  tidy(ols_iv_w,     conf.int = T) |> filter(term == "county_dism") |> mutate(Race = "White", Model = "Controls", Estimator = "OLS (IV Sample)"),
  tidy(ols_sib_e_b,  conf.int = T) |> filter(term == "county_dism") |> mutate(Race = "Black", Model = "Controls", Estimator = "OLS (Exact Sample)"),
  tidy(ols_sib_e_w,  conf.int = T) |> filter(term == "county_dism") |> mutate(Race = "White", Model = "Controls", Estimator = "OLS (Exact Sample)"),
  tidy(ols_sib_f_b,  conf.int = T) |> filter(term == "county_dism") |> mutate(Race = "Black", Model = "Controls", Estimator = "OLS (Flexible Sample)"),
  tidy(ols_sib_f_w,  conf.int = T) |> filter(term == "county_dism") |> mutate(Race = "White", Model = "Controls", Estimator = "OLS (Flexible Sample)")
) |>
  mutate(
    estimate  = estimate  * contrast,
    conf.high = conf.high * contrast,
    conf.low  = conf.low  * contrast,
    Model     = factor(Model, levels = c("Unadjusted", "Controls")),
    Estimator = factor(Estimator, levels = c("OLS (IV Sample)","OLS (Flexible Sample)","OLS (Exact Sample)"))
  )

cache_estimate_range("ols", ols_estimates_plot_data$conf.low, ols_estimates_plot_data$conf.high)

ols_estimates_all =
  ggplot(ols_estimates_plot_data,
         aes(Estimator, estimate,
             ymin = conf.low,
             ymax = conf.high,
             color = Race,
             shape = Race)) +
  geom_pointrange(position = position_dodge2(width = .5),
                  size = .75,
                  lwd = .75) +
  labs(y = "Change in Life Expectancy",
       x = NULL,
       caption = str_wrap("This figure displays OLS estimates of the association between racial segregation and longevity, estimated on the samples used by each specification in the corresponding IV figure.
       The first group is OLS fit to the IV estimation sample, with segregation entering directly rather than through an instrument; the Railroad Division Index and named rivers models share the same sample, so a single OLS column covers both.
       The final two groups are OLS fit to the sibling samples with birth year, urban-rural, birth state, and occupation fixed effects but without the sibling group fixed effect.
       All estimates refer to a 10-point increase in Dissimilarity.", 100)) +
  scale_color_manual(values = c("Black" = "darkgreen", "White" = "darkblue")) +
  scale_shape_manual(values = c("Black" = 16, "White" = 17)) +
  coord_cartesian(ylim = shared_axis_range(MAIN_ESTIMATE_FIGS)) +
  theme_cowplot() +
  geom_hline(yintercept = 0, linetype = "dashed", lwd = 1) +
  # Caption size is set so a 9.5pt note survives the ~0.46x shrink from this plot's
  # 14in ggsave width down to \textwidth (6.5in) on the printed page.
  theme(plot.caption    = element_text(hjust = 0, size = 20),
        legend.position = "bottom",
        axis.text.x     = element_text(angle = 25, hjust = 1))

ggsave(ols_estimates_all, filename = here("FigTab","ols_estimates_all.jpeg"),
       width = 14,
       height = 6,
       dpi = 1000)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Results by education
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Helper: joint significance of the D x educ_cat interaction, as a cluster-robust
# Wald test on the interacted model's own vcov (every model here is fit with
# vcov = CL, so the test needs to respect that clustering to be valid).

ftest_vals <- function(model) {
  w <- fixest::wald(model, keep = "county_dism:educ_cat")
  list(fstat = unname(w$stat), pval = unname(w$p))
}

# Helper: avg_slopes by educ_cat, tagged with strategy and F-test info
extract_ed_slopes <- function(model, data_ref, strategy, fstat, pval) {
  avg_slopes(model,
             variables = "county_dism",
             by        = "educ_cat",
             newdata   = datagrid(
               educ_cat    = unique(data_ref$educ_cat),
               county_dism = unique(data_ref$county_dism)
             )) |>
    tidy(conf.int = TRUE) |>
    mutate(
      strategy   = strategy,
      fstat      = fstat,
      ftest_pval = pval,
      ftest_sig  = case_when(
        pval < 0.001 ~ "p < 0.001",
        pval < 0.01  ~ "p < 0.01",
        pval < 0.05  ~ "p < 0.05",
        TRUE         ~ paste0("p = ", round(pval, 3))
      )
    )
}

# ------------------------------- RDI IV -------------------------------
w_ed_rdi   = feols(death_age~.[cov_ed]            |.[fe_base] |.[iv_rdi_ed], data = dw, vcov = CL)
b_ed_rdi   = feols(death_age~.[cov_ed]            |.[fe_base] |.[iv_rdi_ed], data = db, vcov = CL)

rdi_ft_w <- ftest_vals(w_ed_rdi)
rdi_ft_b <- ftest_vals(b_ed_rdi)

# ------------------------------- Rivers IV -------------------------------
w_ed_riv   = feols(death_age~.[cov_ed]            |.[fe_base] |.[iv_riv_ed], data = dw, vcov = CL)
b_ed_riv   = feols(death_age~.[cov_ed]            |.[fe_base] |.[iv_riv_ed], data = db, vcov = CL)

riv_ft_w <- ftest_vals(w_ed_riv)
riv_ft_b <- ftest_vals(b_ed_riv)

# ------------------------------- Sibling FE (Exact) -------------------------------
w_ed_sib_e   = feols(death_age~county_dism*educ_cat + .[cov_ed_sib]   |.[fe_sib_e], data = dw_f, vcov = CL)
b_ed_sib_e   = feols(death_age~county_dism*educ_cat + .[cov_ed_sib]   |.[fe_sib_e], data = db_f, vcov = CL)

sib_e_ft_w <- ftest_vals(w_ed_sib_e)
sib_e_ft_b <- ftest_vals(b_ed_sib_e)

# ------------------------------- Sibling FE (Flexible) -------------------------------
w_ed_sib_f   = feols(death_age~county_dism*educ_cat + .[cov_ed_sib]   |.[fe_sib_f], data = dw_f, vcov = CL)
b_ed_sib_f   = feols(death_age~county_dism*educ_cat + .[cov_ed_sib]   |.[fe_sib_f], data = db_f, vcov = CL)

sib_f_ft_w <- ftest_vals(w_ed_sib_f)
sib_f_ft_b <- ftest_vals(b_ed_sib_f)

# ------------------------------- Combine into ggplot-ready data frames -------------------------------

education_white <- bind_rows(
  extract_ed_slopes(w_ed_rdi,   dw,   "RDI IV",             rdi_ft_w$fstat,   rdi_ft_w$pval),
  extract_ed_slopes(w_ed_riv,   dw,   "Rivers IV",          riv_ft_w$fstat,   riv_ft_w$pval),
  extract_ed_slopes(w_ed_sib_e, dw_f, "Sib. FE (Exact)",    sib_e_ft_w$fstat, sib_e_ft_w$pval),
  extract_ed_slopes(w_ed_sib_f, dw_f, "Sib. FE (Flexible)", sib_f_ft_w$fstat, sib_f_ft_w$pval)
) |>
  mutate(
    educ_cat = factor(educ_cat, levels = c("Less than HS","High School","Some College","College+")),
    strategy = factor(strategy, levels = c("Rivers IV","RDI IV","Sib. FE (Exact)","Sib. FE (Flexible)"))
  )

education_black <- bind_rows(
  extract_ed_slopes(b_ed_rdi,   db,   "RDI IV",             rdi_ft_b$fstat,   rdi_ft_b$pval),
  extract_ed_slopes(b_ed_riv,   db,   "Rivers IV",          riv_ft_b$fstat,   riv_ft_b$pval),
  extract_ed_slopes(b_ed_sib_e, db_f, "Sib. FE (Exact)",    sib_e_ft_b$fstat, sib_e_ft_b$pval),
  extract_ed_slopes(b_ed_sib_f, db_f, "Sib. FE (Flexible)", sib_f_ft_b$fstat, sib_f_ft_b$pval)
) |>
  mutate(
    educ_cat = factor(educ_cat, levels = c("Less than HS","High School","Some College","College+")),
    strategy = factor(strategy, levels = c("Rivers IV","RDI IV","Sib. FE (Exact)","Sib. FE (Flexible)"))
  )

saveRDS(education_white, here("Data","_Cleaned","education_white.rds"))
saveRDS(education_black, here("Data","_Cleaned","education_black.rds"))

# ------------------------------- create figure ------------------------------- #
# Both panels show the RDI IV strategy only. The other strategies remain in
# education_white / education_black for reference.
educ_slope_plot = function(dat, race_label) {
  dat %>%
    filter(strategy == "RDI IV") %>%
    mutate(
      educ_cat  = factor(educ_cat, levels = c("Less than HS","High School","Some College","College+")),
      estimate  = estimate*contrast,
      conf.high = conf.high*contrast,
      conf.low  = conf.low*contrast
    ) %>%
    ggplot(aes(educ_cat, estimate, ymin = conf.low, ymax = conf.high)) +
    geom_pointrange(lwd = 2, size = .75) +
    labs(y = "Change in Life Expectancy",
         x = "Education Level",
         caption = str_wrap(paste0(
           "This figure displays results of a model with an interaction between education-level and segregation for ",
           race_label, " Americans. The model includes covariates and fixed effects.
           Estimates refer to a 10-point increase in Dissimilarity."), 100)) +
    theme_cowplot() +
    geom_hline(yintercept = 0, linetype = "dashed", lwd = 1, color = "gray") +
    theme(plot.caption = element_text(hjust = 0),
          legend.position = "bottom")
}

# ------------------------------- White
educ_plot_w = educ_slope_plot(education_white, "White")
ggsave(educ_plot_w, filename = here("FigTab","education_slopes_white.jpeg"),
       width = 8, height = 6, dpi = 1000)

# ------------------------------- Black
educ_plot_b = educ_slope_plot(education_black, "Black")
ggsave(educ_plot_b, filename = here("FigTab","education_slopes_black.jpeg"),
       width = 8, height = 6, dpi = 1000)

## ------------------------------- Create Table (RDI IV by education) ------------------------------- ##
msummary(list("Black" = b_ed_rdi,
              "White" = w_ed_rdi
              ),fmt =3, stars = T,
         coef_map = c("fit_county_dism" = "D",
                      "male" = "Male",
                      "migratedMigrated" = "Migrated",
                      "married" = "Married in 1940",
                      "south" = "Died in South",
                      "fit_educ_catHigh School" = "High School (Ref = Less than HS)",
                      "fit_educ_catSome College" = "Some College",
                      "fit_educ_catCollege+" = "College or Higher",
                      "fit_county_dism:educ_catHigh School" = "D x High School",
                      "fit_county_dism:educ_catSome College" = "D x Some College",
                      "fit_county_dism:educ_catCollege+" = "D x College or Higher"
                      ),
         gof_map = c("nobs",
                     "r.squared"),
         notes = "This table describes IV estimates of the effect of segregation on longevity interacted by education.
         First-stage regressions are supppressed for concision. Cluster-robust standard errors (by county of death) in parentheses.",
         title = "Estimates of the Effect of Segregation on Longevity by Education-Level",
         add_rows = data.frame(
           FE = c("Birth Year","Birth State","Urban-Rural Code","Occupation"),
           m1 = c("X","X","X","-"),
           m2 = c("X","X","X","-")
         )) %>%
  save_tt(.,output = here("FigTab","IV_by_Education_table.tex"), overwrite = T)

## ------------------------------- D x Education average marginal effects table (IV + flexible sibling FE) ------------------------------- ##
# Average marginal effect of D AT each education level (not the raw regression
# interaction coefficient, which instead gives the contrast between a level and the
# Less-than-HS reference -- see IV_education_second_diff_table.tex below for that).
# Pulled from the avg_slopes() results already computed above (education_black /
# education_white), for the two IV strategies plus the flexible-match sibling FE
# strategy, rescaled to a 10-point rise in D for consistency with the rest of the
# paper. The exact-match sibling FE strategy is left out, as requested.
#
# 22 of the 24 cells are significant at p < .05. The two exceptions are White Rivers
# IV, Some College (p = .106) and College+ (p = .134); every other cell -- including
# every Sib. FE (Flexible) cell for both races -- is significant. The note below
# states this rather than a blanket significance claim.
# Every note using this table's stars advertises a "+ p < 0.1" tier in its legend;
# this must actually produce one, or a marginal cell (e.g. the Sib. FE (Flexible)
# second difference for White decedents, p = .077) silently reads as indistinguishable
# from a cell nowhere near significant.
star_from_p <- function(p) case_when(
  p < .001 ~ "***", p < .01 ~ "**", p < .05 ~ "*", p < .1 ~ "+", TRUE ~ ""
)

ed_ame_strategies <- c("RDI IV", "Rivers IV", "Sib. FE (Flexible)")

ame_ed_iv <- bind_rows(
  education_black |> mutate(Race = "Black"),
  education_white |> mutate(Race = "White")
) |>
  filter(strategy %in% ed_ame_strategies) |>
  mutate(
    cell = sprintf("%.3f%s (%.3f)", estimate * 10, star_from_p(p.value), std.error * 10)
  ) |>
  select(Race, strategy, educ_cat, cell) |>
  pivot_wider(names_from = c(Race, strategy), values_from = cell)

ame_ed_iv_tab <- ame_ed_iv |>
  transmute(
    `Education Level` = as.character(educ_cat),
    `RDI IV`      = `Black_RDI IV`,
    `Rivers IV`   = `Black_Rivers IV`,
    `Sib. FE (Flexible)`  = `Black_Sib. FE (Flexible)`,
    `RDI IV `     = `White_RDI IV`,
    `Rivers IV `  = `White_Rivers IV`,
    `Sib. FE (Flexible) ` = `White_Sib. FE (Flexible)`
  )

# ------------------------------- D x College+ contrast (second difference) -------------------------------
# The D x College+ interaction coefficient already IS the (College+ minus Less than
# HS) contrast in the segregation slope, since Less than HS is the reference. Used
# both as an extra row below (comparing the education gradient itself, rather than
# the level effects, across strategies) and in the standalone second-differences
# table further down. IV models carry the endogenous term as "fit_county_dism:...";
# the sibling FE model does not, hence the `prefix` argument.
extract_college_contrast <- function(model, race, strategy, prefix = "fit_") {
  broom::tidy(model, conf.int = TRUE) |>
    filter(term == paste0(prefix, "county_dism:educ_catCollege+")) |>
    transmute(strategy = strategy, race = race,
              estimate = estimate * 10, se = std.error * 10,
              p.value  = 2 * pt(-abs(estimate / se), df = Inf))
}

college_contrasts <- bind_rows(
  extract_college_contrast(b_ed_rdi,   "Black", "RDI IV"),
  extract_college_contrast(w_ed_rdi,   "White", "RDI IV"),
  extract_college_contrast(b_ed_riv,   "Black", "Rivers IV"),
  extract_college_contrast(w_ed_riv,   "White", "Rivers IV"),
  extract_college_contrast(b_ed_sib_f, "Black", "Sib. FE (Flexible)", prefix = ""),
  extract_college_contrast(w_ed_sib_f, "White", "Sib. FE (Flexible)", prefix = "")
)

second_diff_row <- college_contrasts |>
  mutate(cell = sprintf("%.3f%s (%.3f)", estimate, star_from_p(p.value), se)) |>
  select(race, strategy, cell) |>
  pivot_wider(names_from = c(race, strategy), values_from = cell) |>
  transmute(
    `Education Level` = "Second Difference",
    `RDI IV`      = `Black_RDI IV`,
    `Rivers IV`   = `Black_Rivers IV`,
    `Sib. FE (Flexible)`  = `Black_Sib. FE (Flexible)`,
    `RDI IV `     = `White_RDI IV`,
    `Rivers IV `  = `White_Rivers IV`,
    `Sib. FE (Flexible) ` = `White_Sib. FE (Flexible)`
  )

ame_ed_iv_tab <- bind_rows(ame_ed_iv_tab, second_diff_row)

# Joint F-test that the full set of D x educ_cat interactions is zero (a
# cluster-robust Wald test via ftest_vals(), see above); reported here so the
# table's implied claim -- that the segregation effect varies by education -- has
# its own significance test, distinct from the per-cell AME significance already
# noted below. Built as a sig/not-sig sentence from the actual p-values rather than
# a hardcoded "significant for all N models" claim, so it can't silently go stale
# if the estimates change on a future re-run (as happened when this test was fixed:
# the previous, non-cluster-robust version of this test overstated significance for
# five of these six models).
ed_ftests <- bind_rows(
  education_black |> distinct(strategy, fstat, ftest_pval) |> mutate(Race = "Black"),
  education_white |> distinct(strategy, fstat, ftest_pval) |> mutate(Race = "White")
) |>
  filter(strategy %in% ed_ame_strategies) |>
  mutate(strategy = factor(strategy, levels = ed_ame_strategies)) |>
  arrange(strategy, Race) |>
  mutate(label = sprintf("%s %s (F = %.2f, p %s)", Race, strategy, fstat,
                          if_else(ftest_pval < .001, "< .001", sprintf("= %.3f", ftest_pval))),
         sig = ftest_pval < .05)

sig_note <- if (all(ed_ftests$sig)) {
  paste0("A joint F-test that the D x education-category interaction terms are jointly zero, from a cluster-robust Wald test on each model, is significant (p < .05) for all six models: ",
         paste(ed_ftests$label, collapse = "; "), ".")
} else if (!any(ed_ftests$sig)) {
  paste0("A joint F-test that the D x education-category interaction terms are jointly zero, from a cluster-robust Wald test on each model, is not significant for any of the six models: ",
         paste(ed_ftests$label, collapse = "; "), ".")
} else {
  paste0("A joint F-test that the D x education-category interaction terms are jointly zero, from a cluster-robust Wald test on each model, is significant (p < .05) only for ",
         paste(ed_ftests$label[ed_ftests$sig], collapse = "; "),
         "; it is not significant for ",
         paste(ed_ftests$label[!ed_ftests$sig], collapse = "; "), ".")
}

tt(ame_ed_iv_tab,
   caption = "Average Marginal Effect of Segregation on Longevity by Education Level, by Identification Strategy",
   notes = paste0(
     "Cells report the average marginal effect of D (years of life per 10-point rise in Dissimilarity) at each education level, estimated from the fully-adjusted models above (Sib. FE (Flexible) additionally includes a flexible-match sibling group fixed effect); this is the effect of segregation within that education group, not a contrast against a reference group. Cluster-robust standard errors (by county of death) in parentheses. Every cell is significant at p < .05 except White decedents under Rivers IV at Some College (p = .106) and College+ (p = .134). The final row is the second difference -- the D x College+ interaction coefficient, i.e. how much the segregation slope for the College+ group differs from the Less-than-HS group shown in the first row -- and is individually significant only for White decedents under RDI IV. ",
     sig_note, " (+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001)")) %>%
  group_tt(j = list("Black" = 2:4, "White" = 5:7)) %>%
  # Fixed column widths + smaller type so the 7-column table fits the landscape text
  # block (~650pt); at natural width (\small, unconstrained columns) it overran by
  # about 100pt even inside \begin{landscape}. Same pattern as mechanism_table.tex
  # and IV_results_weights_table.tex above.
  style_tt(j = 1,
           tabularray_inner = "colsep=3pt, row{1-Z}={font=\\footnotesize}, colspec={Q[l,wd=2.6cm]*{6}{Q[c,wd=2.3cm]}}") %>%
  save_tt(., output = here("FigTab","IV_education_interaction_only_table.tex"), overwrite = T)

## ------------------------------- Second differences: College+ vs. Less than HS, by race ------------------------------- ##
# The D x College+ coefficient already IS the (College+ minus Less than HS) contrast
# in the segregation slope, since Less than HS is the reference; this table reports
# just that within-race contrast for each strategy (no cross-race comparison), for
# the two IV strategies only. college_contrasts (with p.value already computed) and
# star_from_p() are defined above, in the AME table block, which now also carries
# this same contrast (plus Sib. FE (Flexible)) as its own row.
second_diff <- college_contrasts |>
  filter(strategy %in% c("RDI IV", "Rivers IV")) |>
  select(-p.value) |>
  pivot_wider(names_from = race, values_from = c(estimate, se)) |>
  mutate(
    p_Black = 2 * pt(-abs(estimate_Black / se_Black), df = Inf),
    p_White = 2 * pt(-abs(estimate_White / se_White), df = Inf)
  )

second_diff_tab <- second_diff |>
  transmute(
    Strategy = strategy,
    Black    = sprintf("%.3f%s (%.3f)", estimate_Black, star_from_p(p_Black), se_Black),
    White    = sprintf("%.3f%s (%.3f)", estimate_White, star_from_p(p_White), se_White)
  )

# Fixing the same College+ reference-level bug as above: this contrast could not be
# computed correctly before educ_cat was releveled, because College+ was silently the
# omitted reference category rather than a fitted interaction term.
tt(second_diff_tab,
   caption = "Second Differences: College+ vs. Less-than-HS Segregation Slope, by Race",
   notes = "Cells report the D x College+ interaction coefficient from the fully-adjusted IV models above -- i.e. how much the segregation slope for the College+ group differs from the Less-than-HS reference group, in years of life per 10-point rise in D -- with cluster-robust standard errors (by county of death) in parentheses. The contrast is individually significant only for White decedents under RDI IV. (+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001)") %>%
  save_tt(here("FigTab","IV_education_second_diff_table.tex"), overwrite = TRUE)

# ------------------------------- Male V Female -------------------------------
# Named _male / _female rather than _m / _f: _m previously collided with the
# mechanism models above (d2_b_m), so whichever block ran last silently won.
# `male` drops out of the covariates because each model is fit on a single sex.

# RDI IV
d2_b_male   = feols(death_age~.[cov_gender] |.[fe_main] |.[iv_rdi], data = subset(db, male == 1), vcov = CL)
d2_w_male   = feols(death_age~.[cov_gender] |.[fe_main] |.[iv_rdi], data = subset(dw, male == 1), vcov = CL)
d2_b_female = feols(death_age~.[cov_gender] |.[fe_main] |.[iv_rdi], data = subset(db, male == 0), vcov = CL)
d2_w_female = feols(death_age~.[cov_gender] |.[fe_main] |.[iv_rdi], data = subset(dw, male == 0), vcov = CL)

# Rivers IV
r2_b_male   = feols(death_age~.[cov_gender] |.[fe_main] |.[iv_riv], data = subset(db, male == 1), vcov = CL)
r2_w_male   = feols(death_age~.[cov_gender] |.[fe_main] |.[iv_riv], data = subset(dw, male == 1), vcov = CL)
r2_b_female = feols(death_age~.[cov_gender] |.[fe_main] |.[iv_riv], data = subset(db, male == 0), vcov = CL)
r2_w_female = feols(death_age~.[cov_gender] |.[fe_main] |.[iv_riv], data = subset(dw, male == 0), vcov = CL)

# Sibling FE (Exact)
sib_e_b_male   = feols(death_age~county_dism + .[cov_gen_sib] |.[fe_sib_e], data = subset(db_f, male == 1), vcov = CL)
sib_e_w_male   = feols(death_age~county_dism + .[cov_gen_sib] |.[fe_sib_e], data = subset(dw_f, male == 1), vcov = CL)
sib_e_b_female = feols(death_age~county_dism + .[cov_gen_sib] |.[fe_sib_e], data = subset(db_f, male == 0), vcov = CL)
sib_e_w_female = feols(death_age~county_dism + .[cov_gen_sib] |.[fe_sib_e], data = subset(dw_f, male == 0), vcov = CL)

# Sibling FE (Flexible)
sib_f_b_male   = feols(death_age~county_dism + .[cov_gen_sib] |.[fe_sib_f], data = subset(db_f, male == 1), vcov = CL)
sib_f_w_male   = feols(death_age~county_dism + .[cov_gen_sib] |.[fe_sib_f], data = subset(dw_f, male == 1), vcov = CL)
sib_f_b_female = feols(death_age~county_dism + .[cov_gen_sib] |.[fe_sib_f], data = subset(db_f, male == 0), vcov = CL)
sib_f_w_female = feols(death_age~county_dism + .[cov_gen_sib] |.[fe_sib_f], data = subset(dw_f, male == 0), vcov = CL)

# The appendix table reports the RDI specification only, matching the accompanying
# text and figure caption. The rivers and sibling FE gender models are still fit above
# because the gender plot below faceted them; they are not tabulated.
msummary(list("Black (Men) " = d2_b_male,
              "Black (Women)" = d2_b_female,
              "White (Men) " = d2_w_male,
              "White (Women)" = d2_w_female),fmt =3, stars = T,
         coef_map = c("fit_county_dism" = "D",
                      "education" = "Education",
                      "migratedMigrated" = "Migrated",
                      "married" = "Married in 1940",
                      "south" = "Died in South"),
         gof_map = c("nobs",
                     "r.squared"

         ),
         notes = "This table describes IV estimates (Railroad Division Index instrument) of the effect of segregation on longevity by gender. Models are estimated separately by race and gender, so `male` drops out of the covariate set. First-stage regressions are suppressed for concision. Cluster-robust standard errors (by county of death) in parentheses.",
         title = "Estimates of the Effect of Segregation on Longevity by Gender (RDI Instrument)",
         add_rows = data.frame(`FE` = c("Birth Year","Birth State","Urban Rural Code","OCC"),
                               m1 = c("Yes","Yes","Yes","Yes"),
                               m2 = c("Yes","Yes","Yes","Yes"),
                               m3 = c("Yes","Yes","Yes","Yes"),
                               m4 = c("Yes","Yes","Yes","Yes")),
         output = "tinytable") %>%
  save_tt(.,output = "./FigTab/IV_results_table_gender.tex", overwrite = T)


## Plot by Gender
gender_plot_data = bind_rows(
  # RDI IV
  tidy(d2_b_male, conf.int = T) %>% filter(term =="fit_county_dism") %>% mutate(Race = "Black", Model = "Men",   Estimator = "RDI IV"),
  tidy(d2_b_female, conf.int = T) %>% filter(term =="fit_county_dism") %>% mutate(Race = "Black", Model = "Women", Estimator = "RDI IV"),
  tidy(d2_w_male, conf.int = T) %>% filter(term =="fit_county_dism") %>% mutate(Race = "White", Model = "Men",   Estimator = "RDI IV"),
  tidy(d2_w_female, conf.int = T) %>% filter(term =="fit_county_dism") %>% mutate(Race = "White", Model = "Women", Estimator = "RDI IV"),
  # Rivers IV
  tidy(r2_b_male, conf.int = T) %>% filter(term =="fit_county_dism") %>% mutate(Race = "Black", Model = "Men",   Estimator = "Rivers IV"),
  tidy(r2_b_female, conf.int = T) %>% filter(term =="fit_county_dism") %>% mutate(Race = "Black", Model = "Women", Estimator = "Rivers IV"),
  tidy(r2_w_male, conf.int = T) %>% filter(term =="fit_county_dism") %>% mutate(Race = "White", Model = "Men",   Estimator = "Rivers IV"),
  tidy(r2_w_female, conf.int = T) %>% filter(term =="fit_county_dism") %>% mutate(Race = "White", Model = "Women", Estimator = "Rivers IV"),
  # Sibling FE (Exact)
  tidy(sib_e_b_male, conf.int = T) %>% filter(term =="county_dism") %>% mutate(Race = "Black", Model = "Men",   Estimator = "Sib. FE (Exact)"),
  tidy(sib_e_b_female, conf.int = T) %>% filter(term =="county_dism") %>% mutate(Race = "Black", Model = "Women", Estimator = "Sib. FE (Exact)"),
  tidy(sib_e_w_male, conf.int = T) %>% filter(term =="county_dism") %>% mutate(Race = "White", Model = "Men",   Estimator = "Sib. FE (Exact)"),
  tidy(sib_e_w_female, conf.int = T) %>% filter(term =="county_dism") %>% mutate(Race = "White", Model = "Women", Estimator = "Sib. FE (Exact)"),
  # Sibling FE (Flexible)
  tidy(sib_f_b_male, conf.int = T) %>% filter(term =="county_dism") %>% mutate(Race = "Black", Model = "Men",   Estimator = "Sib. FE (Flexible)"),
  tidy(sib_f_b_female, conf.int = T) %>% filter(term =="county_dism") %>% mutate(Race = "Black", Model = "Women", Estimator = "Sib. FE (Flexible)"),
  tidy(sib_f_w_male, conf.int = T) %>% filter(term =="county_dism") %>% mutate(Race = "White", Model = "Men",   Estimator = "Sib. FE (Flexible)"),
  tidy(sib_f_w_female, conf.int = T) %>% filter(term =="county_dism") %>% mutate(Race = "White", Model = "Women", Estimator = "Sib. FE (Flexible)")
)

### Make Plot ###
Iv_estimate_plot_g =
  gender_plot_data %>%
  mutate(
    # scale by contrast
    estimate = estimate*contrast,
    conf.high = conf.high*contrast,
    conf.low = conf.low*contrast,
    Model = factor(Model, levels = c("Men","Women")),
    Estimator = factor(Estimator, levels = c("RDI IV","Rivers IV","Sib. FE (Exact)","Sib. FE (Flexible)"))
  ) %>%
  ggplot(aes(Model,estimate,
             ymin = conf.low,
             ymax = conf.high,
             color = Race)) +
  geom_pointrange(position = position_dodge2(width =.5),
                  size = .75,
                  lwd = 1) +
  facet_wrap(~Estimator, nrow = 1) +
  labs(y = "Change in Years of Life",
       x = NULL,
       caption = str_wrap("This figure displays estimates of the effect of racial segregation on longevity by gender across identification strategies.
       IV models use the Railroad Division Index (RDI IV) and named rivers (Rivers IV) as instruments; sibling FE models compare siblings within a family.
       Estimates from models with all controls and fixed effects are presented.
       Estimates refer to a 10 point increase in Dissimilarity.", 120)
  ) +
  scale_color_manual(values = c("darkgreen","darkblue")) +
  theme_cowplot() +
  geom_hline(yintercept = 0,linetype = "dashed",lwd = 1)    +
  theme(plot.caption = element_text(hjust = 0),
        legend.position = "bottom",
        strip.background = element_rect(fill = "gray92"),
        strip.text = element_text(face = "bold"))

ggsave(Iv_estimate_plot_g,filename = here("FigTab","iv_plot_gender.jpeg"),
       width = 12,
       height = 5,
       dpi = 1000)

# ------------------------------- Movers V Stayers -------------------------------
# Whether the effect of county segregation is driven by people who spent their whole
# life in one county. A stayer died in the county they were born in, so their exposure
# to that county's segregation is lifelong; a mover was exposed only after arriving.
#
# The `mover` factor is built by prepare_analysis_data() at the top of this script,
# where it is derived from birth_fips and death_fips. Anyone without a recorded birth
# county has already been dropped there, so the two groups partition the sample.

# `migrated` drops out of the covariates: it is a deterministic function of the split
# and so is collinear within each subsample.
cov_mig <- "male + education + married + south"

# `south` additionally drops out of the stayer models. A stayer died in their birth
# county, so they died in their birth state, and "died in the South" is then a
# deterministic function of the birth-state fixed effect. Left in, fixest drops it by
# collinearity for Black stayers and returns a numerically degenerate standard error
# (order 1e5) for White stayers, so it is removed from the specification rather than
# left for the solver to discover.
cov_mig_stay <- "male + education + married"

# ------------------------------- Sample splits (RDI IV) -------------------------------
d2_b_move = feols(death_age~.[cov_mig]      |.[fe_main] |.[iv_rdi], data = subset(db, mover == "Mover"),  vcov = CL)
d2_b_stay = feols(death_age~.[cov_mig_stay] |.[fe_main] |.[iv_rdi], data = subset(db, mover == "Stayer"), vcov = CL)
d2_w_move = feols(death_age~.[cov_mig]      |.[fe_main] |.[iv_rdi], data = subset(dw, mover == "Mover"),  vcov = CL)
d2_w_stay = feols(death_age~.[cov_mig_stay] |.[fe_main] |.[iv_rdi], data = subset(dw, mover == "Stayer"), vcov = CL)

# ------------------------------- Pooled interaction (RDI IV) -------------------------------
# Splitting the sample lets every coefficient differ across movers and stayers but gives
# no test of whether the D coefficients themselves differ. This model interacts D with
# mover status -- treating `mover` and D x mover as endogenous and instrumenting with the
# same instrument set interacted with mover, exactly as the south-interacted models do --
# so "D x Mover" is the mover-stayer difference with a standard error on it.
#
# Sweeping `mover` into the endogenous block is the house pattern rather than a claim
# that mover status is endogenous; it makes no difference here. Refitting with mover
# exogenous and only D and D:mover instrumented reproduces every coefficient and
# standard error in this table to the printed precision.
#
# `south` stays in the pooled models but is identified off movers only, since it is
# absorbed by the birth-state FE for the stayers.
iv_rdi_mover <- paste("county_dism*mover ~", interact_with(instr_rdi, "mover"))

d2_b_mover_int = feols(death_age~.[cov_mig] |.[fe_main] |.[iv_rdi_mover], data = db, vcov = CL)
d2_w_mover_int = feols(death_age~.[cov_mig] |.[fe_main] |.[iv_rdi_mover], data = dw, vcov = CL)

msummary(list("Black\\newline (Movers)"      = d2_b_move,
              "Black\\newline (Stayers)"     = d2_b_stay,
              "Black\\newline (Interacted)"  = d2_b_mover_int,
              "White\\newline (Movers)"      = d2_w_move,
              "White\\newline (Stayers)"     = d2_w_stay,
              "White\\newline (Interacted)"  = d2_w_mover_int),
         fmt = 3, stars = T,
         coef_map = c("fit_county_dism" = "D",
                      "fit_moverMover" = "Mover",
                      "fit_county_dism:moverMover" = "D x Mover",
                      "male" = "Male",
                      "education" = "Education",
                      "married" = "Married in 1940",
                      "south" = "Died in South"),
         gof_map = c("nobs",
                     "r.squared",
                     "f"),
         notes = "This table describes IV estimates (Railroad Division Index instrument) of the effect of segregation on longevity for movers and stayers.
         A stayer died in the county they were born in; a mover died in a different county. The Movers and Stayers columns split the sample, so every coefficient is free to differ.
         The Interacted columns pool the two groups and interact D with mover status, so 'D' is the effect among stayers and 'D x Mover' is the mover-stayer difference.
         `migrated` is dropped from the covariates because it is collinear with the split. First-stage regressions are suppressed for concision.
         Cluster-robust standard errors (by county of death) in parentheses.",
         title = "Estimates of the Effect of Segregation on Longevity (Movers vs. Stayers, RDI Instrument)",
         add_rows = data.frame(
           FE = c("Birth Year","Birth State","Urban-Rural Code","Occupation"),
           m1 = c("X","X","X","X"),
           m2 = c("X","X","X","X"),
           m3 = c("X","X","X","X"),
           m4 = c("X","X","X","X"),
           m5 = c("X","X","X","X"),
           m6 = c("X","X","X","X")
         ),
         threeparttable = TRUE
         ) %>%
  group_tt(j = list("Black" = 2:4, "White" = 5:7)) %>%
  save_tt(., output = here("FigTab","IV_results_table_movers.tex"), overwrite = T)

# ------------------------------- Movers vs. stayers figure -------------------------------
movers_plot_data = bind_rows(
  tidy(d2_b_move, conf.int = T) |> filter(term == "fit_county_dism") |> mutate(Race = "Black", Group = "Movers"),
  tidy(d2_b_stay, conf.int = T) |> filter(term == "fit_county_dism") |> mutate(Race = "Black", Group = "Stayers"),
  tidy(d2_w_move, conf.int = T) |> filter(term == "fit_county_dism") |> mutate(Race = "White", Group = "Movers"),
  tidy(d2_w_stay, conf.int = T) |> filter(term == "fit_county_dism") |> mutate(Race = "White", Group = "Stayers")
) |>
  mutate(estimate  = estimate  * contrast,
         conf.high = conf.high * contrast,
         conf.low  = conf.low  * contrast,
         Group     = factor(Group, levels = c("Stayers", "Movers")))

movers_plot =
  ggplot(movers_plot_data,
         aes(Group, estimate, ymin = conf.low, ymax = conf.high, color = Race)) +
  geom_pointrange(position = position_dodge2(width = .5), size = .75, lwd = .75, shape = 22) +
  geom_hline(yintercept = 0, linetype = "dashed", lwd = 1) +
  scale_color_manual(values = c("Black" = "darkgreen", "White" = "darkblue")) +
  labs(y = "Change in Life Expectancy", x = NULL,
       caption = str_wrap("IV estimates (Railroad Division Index instrument) of the effect of segregation on longevity,
       split by whether a person died in the county they were born in (Stayers) or in a different county (Movers).
       Models adjust for demographic controls and birth year, birth state, urban-rural, and occupation fixed effects.
       Estimates refer to a 10-point increase in Dissimilarity.", 100)) +
  theme_cowplot() +
  theme(plot.caption    = element_text(hjust = 0),
        legend.position = "bottom")

ggsave(movers_plot, filename = here("FigTab","movers_stayers_estimates.jpeg"),
       width = 8, height = 6, dpi = 1000)


#------------------------------- Weights -------------------------------

# RDI IV
d2_b_wt = feols(death_age~.[cov_main] |.[fe_main] |.[iv_rdi], data = db, vcov = CL, weights = db$weight)
d2_w_wt = feols(death_age~.[cov_main] |.[fe_main] |.[iv_rdi], data = dw, vcov = CL, weights = dw$weight)

# Rivers IV
r2_b_wt = feols(death_age~.[cov_main] |.[fe_main] |.[iv_riv], data = db, vcov = CL, weights = db$weight)
r2_w_wt = feols(death_age~.[cov_main] |.[fe_main] |.[iv_riv], data = dw, vcov = CL, weights = dw$weight)

# Sibling FE (Exact)
sib_e_b_wt = feols(death_age~county_dism + .[cov_nosouth] |.[fe_sib_e], data = db_f, vcov = CL, weights = db_f$weight)
sib_e_w_wt = feols(death_age~county_dism + .[cov_nosouth] |.[fe_sib_e], data = dw_f, vcov = CL, weights = dw_f$weight)

# Sibling FE (Flexible)
sib_f_b_wt = feols(death_age~county_dism + .[cov_nosouth] |.[fe_sib_f], data = db_f, vcov = CL, weights = db_f$weight)
sib_f_w_wt = feols(death_age~county_dism + .[cov_nosouth] |.[fe_sib_f], data = dw_f, vcov = CL, weights = dw_f$weight)

msummary(list("Black\\newline RDI IV" = d2_b_wt,
              "White\\newline RDI IV" = d2_w_wt,
              "Black\\newline Rivers IV" = r2_b_wt,
              "White\\newline Rivers IV" = r2_w_wt,
              "Black\\newline Sib. FE\\newline (Exact)" = sib_e_b_wt,
              "White\\newline Sib. FE\\newline (Exact)" = sib_e_w_wt,
              "Black\\newline Sib. FE\\newline (Flex.)" = sib_f_b_wt,
              "White\\newline Sib. FE\\newline (Flex.)" = sib_f_w_wt),fmt =3, stars = T,
         coef_map = c("fit_county_dism" = "D",
                      "county_dism" = "D",
                      "male" = "Male",
                      "education" = "Education",
                      "migratedMigrated" = "Migrated",
                      "married" = "Married in 1940",
                      "south" = "Died in South"),
         gof_map = c("nobs",
                     "r.squared"

         ),
         notes = "This table describes estimates of the effect of segregation on longevity using post-stratification weights across identification strategies.
         IV models use the Railroad Division Index and named rivers as instruments; sibling FE models compare siblings within a family.
         First-stage regressions are supppressed for concision. Cluster-robust standard errors (by county of death) in parentheses.",
         title = "Estimates of the Effect of Segregation on Longevity (Weights)",
         add_rows = data.frame(`FE` = c("Birth Year","Birth State","Urban Rural Code","OCC","Sibling Group"),
                               m1 = c("Yes","Yes","Yes","Yes","No"),
                               m2 = c("Yes","Yes","Yes","Yes","No"),
                               m3 = c("Yes","Yes","Yes","Yes","No"),
                               m4 = c("Yes","Yes","Yes","Yes","No"),
                               m5 = c("Yes","Yes","Yes","Yes","Yes"),
                               m6 = c("Yes","Yes","Yes","Yes","Yes"),
                               m7 = c("Yes","Yes","Yes","Yes","Yes"),
                               m8 = c("Yes","Yes","Yes","Yes","Yes")),
         output = "tinytable") %>%
  # Fixed column widths + smaller type so the 9-column table fits the landscape
  # text block (650pt); at natural width it overruns by ~266pt.
  style_tt(j = 1,
           tabularray_inner = "colsep=3pt, row{1-Z}={font=\\footnotesize}, colspec={Q[l,wd=3.1cm]*{8}{Q[c,wd=2.05cm]}}") %>%
  save_tt(.,output = here("FigTab","IV_results_weights_table.tex"), overwrite = T)



# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Alternative Measures of D 

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Each alternative measure replaces county_dism as the endogenous regressor, but is
# instrumented by exactly the same sets as the main models.
iv_H_rdi  <- paste("H_bw ~",         instr_rdi); iv_H_riv  <- paste("H_bw ~",         instr_riv)
iv_I_rdi  <- paste("county_isolb ~", instr_rdi); iv_I_riv  <- paste("county_isolb ~", instr_riv)
iv_FD_rdi <- paste("D_star ~",       instr_rdi); iv_FD_riv <- paste("D_star ~",       instr_riv)

# ------------------------------- RDI IV -------------------------------
H_b_rdi  = feols(death_age~.[cov_main] |.[fe_main] |.[iv_H_rdi],  data = db, vcov = CL)
H_w_rdi  = feols(death_age~.[cov_main] |.[fe_main] |.[iv_H_rdi],  data = dw, vcov = CL)
I_b_rdi  = feols(death_age~.[cov_main] |.[fe_main] |.[iv_I_rdi],  data = db, vcov = CL)
I_w_rdi  = feols(death_age~.[cov_main] |.[fe_main] |.[iv_I_rdi],  data = dw, vcov = CL)
FD_b_rdi = feols(death_age~.[cov_main] |.[fe_main] |.[iv_FD_rdi], data = db, vcov = CL)
FD_w_rdi = feols(death_age~.[cov_main] |.[fe_main] |.[iv_FD_rdi], data = dw, vcov = CL)

# ------------------------------- Rivers IV -------------------------------
H_b_riv  = feols(death_age~.[cov_main] |.[fe_main] |.[iv_H_riv],  data = db, vcov = CL)
H_w_riv  = feols(death_age~.[cov_main] |.[fe_main] |.[iv_H_riv],  data = dw, vcov = CL)
I_b_riv  = feols(death_age~.[cov_main] |.[fe_main] |.[iv_I_riv],  data = db, vcov = CL)
I_w_riv  = feols(death_age~.[cov_main] |.[fe_main] |.[iv_I_riv],  data = dw, vcov = CL)
FD_b_riv = feols(death_age~.[cov_main] |.[fe_main] |.[iv_FD_riv], data = db, vcov = CL)
FD_w_riv = feols(death_age~.[cov_main] |.[fe_main] |.[iv_FD_riv], data = dw, vcov = CL)

# ------------------------------- Sibling FE (Exact) -------------------------------
# The alternative measures enter directly; identification comes from the sibling group FE.
H_b_sib_e  = feols(death_age~H_bw + .[cov_nosouth]         |.[fe_sib_e], data = db_f, vcov = CL)
H_w_sib_e  = feols(death_age~H_bw + .[cov_nosouth]         |.[fe_sib_e], data = dw_f, vcov = CL)
I_b_sib_e  = feols(death_age~county_isolb + .[cov_nosouth] |.[fe_sib_e], data = db_f, vcov = CL)
I_w_sib_e  = feols(death_age~county_isolb + .[cov_nosouth] |.[fe_sib_e], data = dw_f, vcov = CL)
FD_b_sib_e = feols(death_age~D_star + .[cov_nosouth]       |.[fe_sib_e], data = db_f, vcov = CL)
FD_w_sib_e = feols(death_age~D_star + .[cov_nosouth]       |.[fe_sib_e], data = dw_f, vcov = CL)

# ------------------------------- Sibling FE (Flexible) -------------------------------
H_b_sib_f  = feols(death_age~H_bw + .[cov_nosouth]         |.[fe_sib_f], data = db_f, vcov = CL)
H_w_sib_f  = feols(death_age~H_bw + .[cov_nosouth]         |.[fe_sib_f], data = dw_f, vcov = CL)
I_b_sib_f  = feols(death_age~county_isolb + .[cov_nosouth] |.[fe_sib_f], data = db_f, vcov = CL)
I_w_sib_f  = feols(death_age~county_isolb + .[cov_nosouth] |.[fe_sib_f], data = dw_f, vcov = CL)
FD_b_sib_f = feols(death_age~D_star + .[cov_nosouth]       |.[fe_sib_f], data = db_f, vcov = CL)
FD_w_sib_f = feols(death_age~D_star + .[cov_nosouth]       |.[fe_sib_f], data = dw_f, vcov = CL)

alt_coef_map = c("fit_county_dism" = "D",
                 "county_dism" = "D",
                 "fit_H_bw" = "H",
                 "H_bw" = "H",
                 "fit_county_isolb" = "I",
                 "county_isolb" = "I",
                 "fit_D_star" = "D-Adjusted",
                 "D_star" = "D-Adjusted")

alt_notes = "This table describes estimates of the effect of segregation on longevity for alternative measures of D.
         IV models use the Railroad Division Index and named rivers as instruments; sibling FE models compare siblings within a family, with each measure entering directly.
         First-stage regressions are supppressed for concision. Cluster-robust standard errors (by county of death) in parentheses.
         Models adjust for all covariates and FEs used in main analyses but are not shown in the model."

alt_add_rows = data.frame(
  FE = c("Birth Year","Birth State","Urban-Rural Code","Occupation"),
  m1 = c("X","X","X","X"),
  m2 = c("X","X","X","X"),
  m3 = c("X","X","X","X"),
  m4 = c("X","X","X","X"),
  m5 = c("X","X","X","X"),
  m6 = c("X","X","X","X"),
  m7 = c("X","X","X","X"),
  m8 = c("X","X","X","X")
)

# ------------------------------- RDI IV table -------------------------------
msummary(list("Black (D)" = d2_b,
              "Black (H)" = H_b_rdi,
              "Black (I)" = I_b_rdi,
              "Black (D*)" = FD_b_rdi,
              "White (D)" = d2_w,
              "White (H)" = H_w_rdi,
              "White (I)" = I_w_rdi,
              "White (D*)" = FD_w_rdi),fmt =3, stars = T,
         coef_map = alt_coef_map,
         gof_map = c("nobs",
                     "r.squared"

         ),
         notes = alt_notes,
         title = "Estimates of the Effect of Segregation on Longevity (Alternative Measures, RDI Instrument)",
         add_rows = alt_add_rows)  %>%
  save_tt(.,output = here("FigTab","IV_results_table_alt_measure.tex"), overwrite = T)

# ------------------------------- Rivers IV table -------------------------------
msummary(list("Black (D)" = r2_b,
              "Black (H)" = H_b_riv,
              "Black (I)" = I_b_riv,
              "Black (D*)" = FD_b_riv,
              "White (D)" = r2_w,
              "White (H)" = H_w_riv,
              "White (I)" = I_w_riv,
              "White (D*)" = FD_w_riv),fmt =3, stars = T,
         coef_map = alt_coef_map,
         gof_map = c("nobs",
                     "r.squared"

         ),
         notes = alt_notes,
         title = "Estimates of the Effect of Segregation on Longevity (Alternative Measures, Rivers Instrument)",
         add_rows = alt_add_rows)  %>%
  save_tt(.,output = here("FigTab","IV_results_table_alt_measure_rivers.tex"), overwrite = T)

# ------------------------------- Sibling FE table -------------------------------
msummary(list("Black\\newline (D)" = sib_m2_b,
              "Black\\newline (H)" = H_b_sib_e,
              "Black\\newline (I)" = I_b_sib_e,
              "Black\\newline (D*)" = FD_b_sib_e,
              "White\\newline (D)" = sib_m2_w,
              "White\\newline (H)" = H_w_sib_e,
              "White\\newline (I)" = I_w_sib_e,
              "White\\newline (D*)" = FD_w_sib_e,
              "Black\\newline (D)" = sib_2_m2_b,
              "Black\\newline (H)" = H_b_sib_f,
              "Black\\newline (I)" = I_b_sib_f,
              "Black\\newline (D*)" = FD_b_sib_f,
              "White\\newline (D)" = sib_2_m2_w,
              "White\\newline (H)" = H_w_sib_f,
              "White\\newline (I)" = I_w_sib_f,
              "White\\newline (D*)" = FD_w_sib_f),fmt =3, stars = T,
         coef_map = alt_coef_map,
         gof_map = c("nobs",
                     "r.squared"

         ),
         notes = "This table describes sibling fixed-effects estimates of the effect of segregation on longevity for alternative measures of D.
         Columns 1-8 use exact sibling matches and columns 9-16 use flexible sibling matches.
         Cluster-robust standard errors (by county of death) in parentheses.
         Models adjust for all covariates and FEs used in main analyses but are not shown in the model.",
         title = "Estimates of the Effect of Segregation on Longevity (Alternative Measures, Sibling FE)",
         add_rows = data.frame(
           FE = c("Birth Year","Birth State","Urban-Rural Code","Occupation","Sibling Group"),
           m1 = c("X","X","X","X","X"),
           m2 = c("X","X","X","X","X"),
           m3 = c("X","X","X","X","X"),
           m4 = c("X","X","X","X","X"),
           m5 = c("X","X","X","X","X"),
           m6 = c("X","X","X","X","X"),
           m7 = c("X","X","X","X","X"),
           m8 = c("X","X","X","X","X"),
           m9 = c("X","X","X","X","X"),
           m10 = c("X","X","X","X","X"),
           m11 = c("X","X","X","X","X"),
           m12 = c("X","X","X","X","X"),
           m13 = c("X","X","X","X","X"),
           m14 = c("X","X","X","X","X"),
           m15 = c("X","X","X","X","X"),
           m16 = c("X","X","X","X","X")
         ))  %>%
  # Fixed column widths + smaller type so the 17-column table fits the landscape
  # text block (650pt); at natural width it overruns by ~428pt. 16 estimate
  # columns leave no slack, so this needs scriptsize and near-zero colsep.
  style_tt(j = 1,
           tabularray_inner = "colsep=1pt, row{1-Z}={font=\\scriptsize}, colspec={Q[l,wd=1.70cm]*{16}{Q[c,wd=1.24cm]}}") %>%
  save_tt(.,output = here("FigTab","IV_results_table_alt_measure_sib.tex"), overwrite = T)
