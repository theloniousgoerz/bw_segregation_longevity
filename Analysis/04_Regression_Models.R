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
# ------------------------------- Helper Function -------------------------------
my_ftest <- function(modc, modnc) {
  ssr_c <- sum(residuals(modc)^2)
  ssr_nc <- sum(residuals(modnc)^2)
  
  df_c <- df.residual(modc)
  df_nc <- df.residual(modnc)
  
  df_dif <- df_nc - df_c   # restricted df minus full df
  
  fstat <- ((ssr_nc - ssr_c) / df_dif) / (ssr_c / df_c)
  pvf <- pf(fstat, df_dif, df_c, lower.tail = FALSE)
  
  print(
    paste0("The F-statistic is ", fstat,
           " and the p-value is ", pvf)
  )
}
`%notin%` = Negate(`%in%`)
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Data
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
data_a =   read_csv(here("Data","_Cleaned","data_a.csv"))                                                                        
db =       read_csv(here("Data","_Cleaned","db.csv"))                                                                              
dw =       read_csv(here("Data","_Cleaned","dw.csv"))   
db_f=      read_csv(here("Data","_Cleaned","db_f.csv"))                                                                              
dw_f=      read_csv(here("Data","_Cleaned","dw_f.csv"))   
mechanism = read_csv(here("Data","_Cleaned","mechanism.csv"))
income_seg = read_csv(here("Data","_Cleaned","income_segregation_Hr.csv"))

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
  Hr_all,
  gov_party_consistent,
  gov_party
) %>% distinct() 


data_a %<>% left_join(.,mechanisms, by = c("death_decade","death_fips"))
db %<>% left_join(.,mechanisms, by = c("death_decade","death_fips")) 
dw %<>% left_join(.,mechanisms, by = c("death_decade","death_fips"))
  
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Establish OLS relationship
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ------------------------------- Sibling models (defined here so the OLS and -------------------------------
# sibling FE tables below can both reference them; the sibling-sample OLS fits
# depend on the sibling FE fits via obs()).

# Exact Matches
sib_m2_b = feols(death_age~county_dism + male + migrated + education + married |byear + STATEFIP_b  + urb_code +OCC + sib_group_id_exact, data = db_f,vcov = ~death_fips)
sib_m2_w = feols(death_age~county_dism + male + migrated + education + married |byear + STATEFIP_b  + urb_code +OCC + sib_group_id_exact, data = dw_f,vcov = ~death_fips)

# Flexible Matches
sib_2_m2_b = feols(death_age~county_dism + male + migrated + education + married |byear + STATEFIP_b  + urb_code +OCC + sib_group_id_flexible, data = db_f,vcov = ~death_fips)
sib_2_m2_w = feols(death_age~county_dism + male + migrated + education + married |byear + STATEFIP_b  + urb_code +OCC + sib_group_id_flexible, data = dw_f,vcov = ~death_fips)

# OLS on the sibling samples, without the sibling group FE
ols_sib_e_b = feols(death_age~county_dism + male + migrated + education + married |byear + STATEFIP_b + urb_code + OCC, data = db_f[obs(sib_m2_b), ], vcov = ~death_fips)
ols_sib_e_w = feols(death_age~county_dism + male + migrated + education + married |byear + STATEFIP_b + urb_code + OCC, data = dw_f[obs(sib_m2_w), ], vcov = ~death_fips)
ols_sib_f_b = feols(death_age~county_dism + male + migrated + education + married |byear + STATEFIP_b + urb_code + OCC, data = db_f[obs(sib_2_m2_b), ], vcov = ~death_fips)
ols_sib_f_w = feols(death_age~county_dism + male + migrated + education + married |byear + STATEFIP_b + urb_code + OCC, data = dw_f[obs(sib_2_m2_w), ], vcov = ~death_fips)

# ------------------------------- OLS -------------------------------
ols_m1_b = feols(death_age~county_dism   |byear + STATEFIP_b + urb_code, data = db,vcov =~death_fips)
ols_m1_w = feols(death_age~county_dism   |byear + STATEFIP_b + urb_code, data = dw,vcov =~death_fips)
ols_m2_b = feols(death_age~county_dism + male + migrated + education + married |byear + STATEFIP_b  + urb_code +OCC, data = db,vcov = ~death_fips)
ols_m2_w = feols(death_age~county_dism + male + migrated + education + married |byear + STATEFIP_b  + urb_code +OCC, data = dw,vcov = ~death_fips)

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
                        "married" = "Married in 1940"),
           gof_map = c("nobs",
                       "r.squared"
           ),
           notes = "Heteroskedasiticty Robust Standard Errors in parentheses. The Exact and Flexible columns are OLS estimated on the exact- and flexible-match sibling samples, without the sibling group fixed effect.",
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
  
  d1_b = feols(death_age~1|byear + STATEFIP_b + urb_code |county_dism~rdi + rail_km_per_km2, data = db, vcov =~death_fips)
  d1_w = feols(death_age~1|byear + STATEFIP_b + urb_code |county_dism~rdi + rail_km_per_km2, data = dw, vcov =~death_fips)
  d2_b = feols(death_age~male + migrated + education + married + south|byear + STATEFIP_b + urb_code + OCC |county_dism~rdi + rail_km_per_km2, data = db, vcov =~death_fips)
  d2_w = feols(death_age~male + migrated + education + married + south|byear + STATEFIP_b + urb_code + OCC |county_dism~rdi + rail_km_per_km2, data = dw, vcov =~death_fips)
  
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
             "rdi" = "Racial Diversity Index"),
           gof_map = c("nobs",
                       "r.squared",
                       "f"),
           align = "lcccccccc",
           notes = "This table describes the first-stage models and IV estimates of the effect of segregation on longevity. Heteroskedasiticty Robust Standard Errors in parentheses.",
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

r1_b = feols(death_age~1|byear + STATEFIP_b + urb_code |county_dism~n_named_rivers + n_named_rivers_sq + stream_km_per_km2 , data = db, vcov = "white")
r1_w = feols(death_age~1|byear + STATEFIP_b + urb_code |county_dism~n_named_rivers + n_named_rivers_sq + stream_km_per_km2 , data = dw, vcov = "white")
r2_b = feols(death_age~male + migrated + education + married + south|byear + STATEFIP_b + urb_code + OCC |county_dism~n_named_rivers + n_named_rivers_sq +  stream_km_per_km2, data = db, vcov ="white")
r2_w = feols(death_age~male + migrated + education + married + south|byear + STATEFIP_b + urb_code + OCC |county_dism~n_named_rivers + n_named_rivers_sq +  stream_km_per_km2, data = dw, vcov ="white")

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
                      "n_named_rivers_sq" = "Named Rivers (Sq.)"),
         gof_map = c("nobs",
                     "r.squared",
                     "f"),
         align = "lcccccccc",
         notes = "This table describes the first-stage models and IV estimates of the effect of segregation on longevity. Heteroskedasiticty Robust Standard Errors in parentheses.",
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
# "Paired" adds the sibling group FE, so the sample is held fixed and the only
# difference is the family fixed effect.
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
                      "married" = "Married in 1940"),
         gof_map = c("nobs",
                     "r.squared"),
         notes = "Cluster-robust standard errors (by county of death) in parentheses. Within each sample, Unpaired is OLS without the sibling group fixed effect and Paired adds it, holding the estimation sample fixed.",
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
# Results for children ineligible for war 

## 1. Five-year birth-year bins spanning 1905–1920.
##    Last bin keeps your 1915:1920 definition, so it is 6 years wide.
bins <- list(
  "1905-1909" = 1905:1909,
  "1910-1914" = 1910:1914,
  "1915-1920" = 1915:1920
)

est_bin <- function(data, years, sex_val) {
  d <- data[data$byear %in% years & data$male == sex_val, ]   # explicit, no NSE
  feols(
    death_age ~ migrated + education + married + south |
      byear + STATEFIP_b + urb_code + OCC |
      county_dism ~ rdi + rail_km_per_km2,
    data  = d,
    vcov  = ~death_fips
  )
}

collect <- function(data, race_label, sex_val, sex_label) {
  do.call(rbind, lapply(names(bins), function(b) {
    m  <- est_bin(data, bins[[b]], sex_val)
    ct <- coeftable(m)["fit_county_dism", ]
    data.frame(
      bin      = b,
      sample   = race_label,
      sex      = sex_label,
      estimate = ct[["Estimate"]],
      se       = ct[["Std. Error"]],
      fstat    = tryCatch(fitstat(m, "ivf1")[[1]]$stat,
                          error = function(e) NA_real_),
      stringsAsFactors = FALSE
    )
  }))
}

## 4. All four Race × Sex combinations.
res <- rbind(
  collect(db, "Black", 1, "Men"),
  collect(db, "Black", 0, "Women"),
  collect(dw, "White", 1, "Men"),
  collect(dw, "White", 0, "Women")
)

res$lo  <- res$estimate - 1.96 * res$se
res$hi  <- res$estimate + 1.96 * res$se
res$bin <- factor(res$bin, levels = names(bins))
res$sex <- factor(res$sex, levels = c("Men", "Women"))

## 5. Figure: faceted by sex, colored by race.
res %>%
  mutate(estimate = estimate * 10,
         lo       = lo * 10,
         hi       = hi * 10) %>%
  ggplot(aes(bin, estimate, 
             color = sample,  
             shape = sex)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_pointrange(aes(ymin = lo, ymax = hi),
                  position = position_dodge(width = 0.35),
                  linewidth = 0.7, fatten = 2.5, 
                  size = 1) +
  labs(
    x = "Birth-year bin",
    y = "Years of Life",
    color = "Group"
  ) +
  theme_cowplot() +
  theme(legend.position = "top") +
  scale_color_manual(values = c("Black" = "#1b7837", "White" = "#2166ac"))

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Mechanisms
db_mech = db %>% filter(!is.na(taxes_pc))
dw_mech = dw %>% filter(!is.na(taxes_pc))

# Shared formula components
mech_fe  <- "byear + STATEFIP_b + urb_code + OCC"
mech_iv  <- "county_dism~rdi + rail_km_per_km2"
mech_cov <- "male + migrated + education + married + south"

# --------------------------
# Black

d2_b_m         = feols(death_age~.[mech_cov]                      |.[mech_fe] |.[mech_iv], data = db_mech, vcov = ~death_fips)
d2_b_m_tax     = feols(death_age~.[mech_cov] + taxes_pc           |.[mech_fe] |.[mech_iv], data = db_mech, vcov = ~death_fips)
d2_b_m_tax_p     = feols(death_age~.[mech_cov] + prop_tax_pc           |.[mech_fe] |.[mech_iv], data = db_mech, vcov = ~death_fips)
d2_b_m_med     = feols(death_age~.[mech_cov] + comp_medicaid      |.[mech_fe] |.[mech_iv], data = db_mech, vcov = ~death_fips)
d2_b_m_heal    = feols(death_age~.[mech_cov] + health_pc   |.[mech_fe] |.[mech_iv], data = db_mech, vcov = ~death_fips)
d2_b_m_welf    = feols(death_age~.[mech_cov] + welf_direct_pc     |.[mech_fe] |.[mech_iv], data = db_mech, vcov = ~death_fips)
d2_b_m_snap    = feols(death_age~.[mech_cov] + comp_snap      |.[mech_fe] |.[mech_iv], data = db_mech, vcov =~death_fips)

# --------------------------
# White
d2_w_m         = feols(death_age~.[mech_cov]                      |.[mech_fe] |.[mech_iv], data = dw_mech, vcov =~death_fips)
d2_w_m_tax     = feols(death_age~.[mech_cov] +  taxes_pc           |.[mech_fe] |.[mech_iv], data = dw_mech, vcov =~death_fips)
d2_w_m_tax_prop = feols(death_age~.[mech_cov] + prop_tax_pc           |.[mech_fe] |.[mech_iv], data = dw_mech, vcov =~death_fips)
d2_w_m_med     = feols(death_age~.[mech_cov] +  comp_medicaid      |.[mech_fe] |.[mech_iv], data = dw_mech, vcov =~death_fips)
d2_w_m_heal     = feols(death_age~.[mech_cov] +  health_pc   |.[mech_fe] |.[mech_iv], data = dw_mech, vcov =~death_fips)
d2_w_m_welf    = feols(death_age~.[mech_cov] +  welf_direct_pc     |.[mech_fe] |.[mech_iv], data = dw_mech, vcov =~death_fips)
d2_w_m_snap     = feols(death_age~.[mech_cov] + comp_snap      |.[mech_fe] |.[mech_iv], data = dw_mech, vcov =~death_fips)

msummary(list(
  # Black
  "Black"               = d2_b_m,
  "Black + Taxes"       = d2_b_m_tax,
  "Black + Prop Tax"    = d2_b_m_tax_p,
  "Black + Medicaid"    = d2_b_m_med,
  "Black + Health" =      d2_b_m_heal,
  "Black + Welfare"     = d2_b_m_welf,
  "Black + Snap"        = d2_b_m_snap,
  # White
  "White"               = d2_w_m,
  "White + Taxes"       = d2_w_m_tax,
  "White + Prop Tax"    = d2_w_m_tax_prop,
  "White + Medicaid"    = d2_w_m_med,
  "White + Health"   =    d2_w_m_heal,
  "White + Welfare"     = d2_w_m_welf,
  "White + Snap"        = d2_w_m_snap
),
fmt   = 3,
stars = TRUE,
coef_map = c(
  "fit_county_dism"  = "D"
),
gof_map = c("nobs", "r.squared", "f"),
notes   = "IV estimates (RDI instrument). Each column adds one mechanism as a control. Heteroskedasticity-robust SEs in parentheses.",
title   = "Mechanism Estimates: Effect of Segregation on Longevity Controlling for Policy Channels",
threeparttable = TRUE
) %>%
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
  "Snap"      = d2_b_m_snap
)
white_models <- list(
  "Baseline"     = d2_w_m,    
  "Taxes"        = d2_w_m_tax,
  "Medicaid"     = d2_w_m_med,
  "Health" = d2_w_m_heal,
  "Welfare"     = d2_w_m_welf,
  "Snap" = d2_w_m_snap
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

mech_levels <- c("Baseline",
                 "Health", "Taxes", "Medicaid",  "Welfare","Snap")

coef_all <- bind_rows(coef_black, coef_white) |>
  mutate(
    pct_baseline = case_when(
      race == "Black" ~ estimate / base_b * 100,
      race == "White" ~ estimate / base_w * 100
    ),
    pct_label = paste0(round(pct_baseline, 1), "%"),
    # Order mechanisms by coefficient (descending) independently within each race facet
    mechanism = reorder_within(mechanism, estimate, race)
  )

baseline_lines <- data.frame(
  race      = c("Black", "White"),
  xintercept = c(base_b,  base_w)
)


mech_plot <-
  ggplot(coef_all, aes(y = mechanism, x = estimate, color = race)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", lwd = 1) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 lwd = 1,
                 height = 0.25, position = position_dodge(width = 0.4)) +
  geom_point(size = 2.5, position = position_dodge(width = 0.6)) +
  geom_text(aes(x = conf.high, label = pct_label),
            position = position_dodge(width = 0.6),
            hjust = -0.15, size = 2.8, show.legend = FALSE) +
  scale_color_manual(values = c("Black" = "#1b7837", "White" = "#2166ac")) +
  geom_vline(data = baseline_lines,
              aes(xintercept = xintercept, color = race),
              linetype = "dotted", linewidth = 1, show.legend = FALSE) +
  #scale_x_continuous(expand = expansion(mult = c(0.05, 0.22))) +
  labs(
    x     = "Coefficient on D (Segregation)",
    y     = NULL,
    color = NULL,
    title = NULL,
    subtitle = "Label shows coefficient as % of baseline model (no mechanism control)"
  ) +
facet_wrap(~race, nrow = 2, scales = "free") +
  scale_y_reordered() +
  theme_cowplot(font_size = 11) +
  theme(
    legend.position = "none",
    strip.background = element_rect(fill = "gray92"),
    strip.text       = element_text(face = "bold")
  )

ggsave(mech_plot, filename = here("FigTab", "mechanism_coefplot.jpeg"),
       width = 12, height = 7, dpi = 1000)



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

# First-stage F-statistics, computed here so the captions cannot go stale.
fs_rdi = feols(county_dism~rdi + rail_km_per_km2 | death_decade, data = instrument, vcov = ~death_fips)
fs_riv = feols(county_dism~n_named_rivers + n_named_rivers_sq + stream_km_per_km2 | death_decade, data = instrument, vcov = ~death_fips)

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
  draw_label(
    str_wrap("This figure displays the association between each instrument and segregation.
             Segregation is measured by the index of dissimilarity that measures how evenly distributed Black and White residents are within a county.
             Panel A displays the Railroad Division Index first stage, adjusting for railroad density.
             Panel B displays the named rivers first stage, adjusting for stream density.
             Dots represent means of bins at each level of the instrument.
             The blue line corresponds to the fitted OLS regression line of segregation on the instrument.", 140),
    x = 0, hjust = 0, size = 10
  ) +
  theme(plot.margin = margin(0, 0, 0, 7))

fs_plot_all = plot_grid(fs_panels, fs_caption,
                        ncol        = 1,
                        rel_heights = c(1, 0.25))

# ------------------------------- Save -------------------------------
ggsave(fs_plot_all, filename = here("FigTab","fs_plot.jpeg"),
       width = 12,
       height = 6,
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

all_estimates_plot =
  ggplot(all_estimates_plot_data,
         aes(Estimator, estimate,
             ymin = conf.low,
             ymax = conf.high,
             color = Race)) +
  geom_pointrange(position = position_dodge2(width = .5),
                  size = .75,
                  lwd = .75,
                  shape = 22) +
  labs(y = "Change in Life Expectancy",
       x = NULL,
       caption = str_wrap("This figure displays estimates from all specifications of the effect of racial segregation on longevity.
       IV models use named rivers (Rivers IV) and the Railroad Diversity Index (RDI IV) as instruments.
       Sibling FE models include birth year, urban-rural, birth state, and occupation fixed effects plus a sibling group fixed effect.
       All estimates refer to a 10-point increase in Dissimilarity.", 120)) +
  scale_color_manual(values = c("darkgreen","darkblue")) +
  theme_cowplot() +
  geom_hline(yintercept = 0, linetype = "dashed", lwd = 1) +
  theme(plot.caption    = element_text(hjust = 0),
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
  "IV estimates of the effect of segregation on longevity using the Railroad Diversity Index instrument.
   Unadjusted models include birth year, birth state, and urban-rural fixed effects; Adjusted models add
   demographic controls and occupation fixed effects. Estimates refer to a 10-point increase in Dissimilarity."
)

ggsave(rdi_iv_plot, filename = here("FigTab","rdi_iv_estimates.jpeg"),
       width = 8, height = 6, dpi = 1000)

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
sib_m1_b   = feols(death_age~county_dism | byear + STATEFIP_b + urb_code + OCC + sib_group_id_exact,    data = db_f, vcov = ~death_fips)
sib_m1_w   = feols(death_age~county_dism | byear + STATEFIP_b + urb_code + OCC + sib_group_id_exact,    data = dw_f, vcov = ~death_fips)
sib_2_m1_b = feols(death_age~county_dism | byear + STATEFIP_b + urb_code + OCC + sib_group_id_flexible, data = db_f, vcov = ~death_fips)
sib_2_m1_w = feols(death_age~county_dism | byear + STATEFIP_b + urb_code + OCC + sib_group_id_flexible, data = dw_f, vcov = ~death_fips)

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
  feols(death_age~county_dism + male + migrated + education + married + south |
          byear + STATEFIP_b + urb_code + OCC,
        data = data[obs(iv_mod), ],
        vcov = ~death_fips)
}

# OLS on each IV sample
ols_rivers_b = ols_on_iv_sample(r2_b, db)
ols_rivers_w = ols_on_iv_sample(r2_w, dw)
ols_rdi_b    = ols_on_iv_sample(d2_b, db)
ols_rdi_w    = ols_on_iv_sample(d2_w, dw)

# OLS on the sibling samples (ols_sib_*), without the sibling group FE, are
# defined above near the OLS table.

ols_estimates_plot_data = bind_rows(
  tidy(ols_rivers_b, conf.int = T) |> filter(term == "county_dism") |> mutate(Race = "Black", Model = "Controls", Estimator = "OLS (Rivers Sample)"),
  tidy(ols_rivers_w, conf.int = T) |> filter(term == "county_dism") |> mutate(Race = "White", Model = "Controls", Estimator = "OLS (Rivers Sample)"),
  tidy(ols_rdi_b,    conf.int = T) |> filter(term == "county_dism") |> mutate(Race = "Black", Model = "Controls", Estimator = "OLS (RDI Sample)"),
  tidy(ols_rdi_w,    conf.int = T) |> filter(term == "county_dism") |> mutate(Race = "White", Model = "Controls", Estimator = "OLS (RDI Sample)"),
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
    Estimator = factor(Estimator, levels = c("OLS (RDI Sample)","OLS (Rivers Sample)","OLS (Flexible Sample)","OLS (Exact Sample)"))
  )

ols_estimates_all =
  ggplot(ols_estimates_plot_data,
         aes(Estimator, estimate,
             ymin = conf.low,
             ymax = conf.high,
             color = Race)) +
  geom_pointrange(position = position_dodge2(width = .5),
                  size = .75,
                  lwd = .75,
                  shape = 22) +
  labs(y = "Change in Life Expectancy",
       x = NULL,
       caption = str_wrap("This figure displays OLS estimates of the association between racial segregation and longevity, estimated on the samples used by each specification in the corresponding IV figure.
       The first two groups are OLS fit to the estimation samples of the named rivers (Rivers IV) and Railroad Diversity Index (RDI IV) models, with segregation entering directly rather than through an instrument.
       The final two groups are OLS fit to the sibling samples with birth year, urban-rural, birth state, and occupation fixed effects but without the sibling group fixed effect.
       All estimates refer to a 10-point increase in Dissimilarity.", 120)) +
  scale_color_manual(values = c("darkgreen","darkblue")) +
  theme_cowplot() +
  geom_hline(yintercept = 0, linetype = "dashed", lwd = 1) +
  theme(plot.caption    = element_text(hjust = 0),
        legend.position = "bottom",
        axis.text.x     = element_text(angle = 25, hjust = 1))

ggsave(ols_estimates_all, filename = here("FigTab","ols_estimates_all.jpeg"),
       width = 14,
       height = 6,
       dpi = 1000)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Results by education
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Helper: return F-test values as a list
ftest_vals <- function(modc, modnc) {
  ssr_c  <- sum(residuals(modc)^2)
  ssr_nc <- sum(residuals(modnc)^2)
  df_c   <- df.residual(modc)
  df_nc  <- df.residual(modnc)
  df_dif <- df_nc - df_c
  fstat  <- ((ssr_nc - ssr_c) / df_dif) / (ssr_c / df_c)
  pval   <- pf(fstat, df_dif, df_c, lower.tail = FALSE)
  list(fstat = fstat, pval = pval)
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
w_ed_c_rdi = feols(death_age~male + migrated + married + south + educ_cat |byear + STATEFIP_b +urb_code + OCC | county_dism~ rdi + rail_km_per_km2, data = dw, vcov = ~death_fips)
b_ed_c_rdi = feols(death_age~male + migrated + married + south + educ_cat |byear + STATEFIP_b +urb_code + OCC | county_dism~ rdi + rail_km_per_km2, data = db, vcov = ~death_fips)
w_ed_rdi   = feols(death_age~male + migrated + married + south |byear + STATEFIP_b +urb_code | county_dism*educ_cat~ rdi*educ_cat + rail_km_per_km2*educ_cat, data = dw, vcov = ~death_fips)
b_ed_rdi   = feols(death_age~male + migrated + married + south |byear + STATEFIP_b +urb_code | county_dism*educ_cat~ rdi*educ_cat + rail_km_per_km2*educ_cat, data = db, vcov = ~death_fips)

rdi_ft_w <- ftest_vals(w_ed_c_rdi, w_ed_rdi)
rdi_ft_b <- ftest_vals(b_ed_c_rdi, b_ed_rdi)

w_rdi_2d = avg_slopes(w_ed_rdi,
           variables = "county_dism",
           by        = "educ_cat",
           newdata   = datagrid(
             educ_cat    = unique(db$educ_cat),
             county_dism = unique(db$county_dism)
           ),
           hypothesis = "pairwise")

b_rdi_2d = avg_slopes(b_ed_rdi,
                      variables = "county_dism",
                      by        = "educ_cat",
                      newdata   = datagrid(
                        educ_cat    = unique(db$educ_cat),
                        county_dism = unique(db$county_dism)
                      ),
                      hypothesis = "pairwise")


# ------------------------------- Rivers IV -------------------------------
w_ed_c_riv = feols(death_age~male + migrated + married + south + educ_cat |byear + STATEFIP_b +urb_code + OCC | county_dism~ n_named_rivers + n_named_rivers_sq + stream_km_per_km2, data = dw, vcov =~death_fips)
b_ed_c_riv = feols(death_age~male + migrated + married + south + educ_cat |byear + STATEFIP_b +urb_code + OCC | county_dism~ n_named_rivers + n_named_rivers_sq + stream_km_per_km2, data = db, vcov =~death_fips)
w_ed_riv   = feols(death_age~male + migrated + married + south |byear + STATEFIP_b +urb_code | county_dism*educ_cat~ n_named_rivers*educ_cat + n_named_rivers_sq*educ_cat + stream_km_per_km2*educ_cat, data = dw, vcov = ~death_fips)
b_ed_riv   = feols(death_age~male + migrated + married + south |byear + STATEFIP_b +urb_code | county_dism*educ_cat~ n_named_rivers*educ_cat + n_named_rivers_sq*educ_cat + stream_km_per_km2*educ_cat, data = db, vcov = ~death_fips)

riv_ft_w <- ftest_vals(w_ed_c_riv, w_ed_riv)
riv_ft_b <- ftest_vals(b_ed_c_riv, b_ed_riv)

# ------------------------------- Sibling FE (Exact) -------------------------------
w_ed_c_sib_e = feols(death_age~county_dism + male + migrated + married + educ_cat |byear + STATEFIP_b + urb_code + OCC + sib_group_id_exact, data = dw_f, vcov = ~death_fips)
b_ed_c_sib_e = feols(death_age~county_dism + male + migrated + married + educ_cat |byear + STATEFIP_b + urb_code + OCC + sib_group_id_exact, data = db_f, vcov = ~death_fips)
w_ed_sib_e   = feols(death_age~county_dism*educ_cat + male + migrated + married |byear + STATEFIP_b + urb_code + OCC + sib_group_id_exact, data = dw_f, vcov = ~death_fips)
b_ed_sib_e   = feols(death_age~county_dism*educ_cat + male + migrated + married |byear + STATEFIP_b + urb_code + OCC + sib_group_id_exact, data = db_f, vcov = ~death_fips)

sib_e_ft_w <- ftest_vals(w_ed_c_sib_e, w_ed_sib_e)
sib_e_ft_b <- ftest_vals(b_ed_c_sib_e, b_ed_sib_e)

w_sbe_2d = avg_slopes(w_ed_sib_e,
                      variables = "county_dism",
                      by        = "educ_cat",
                      newdata   = datagrid(
                        educ_cat    = unique(db$educ_cat),
                        county_dism = unique(db$county_dism)
                      ),
                      hypothesis = "pairwise")

# ------------------------------- Sibling FE (Flexible) -------------------------------
w_ed_c_sib_f = feols(death_age~county_dism + male + migrated + married + educ_cat |byear + STATEFIP_b + urb_code + OCC + sib_group_id_flexible, data = dw_f, vcov = ~death_fips)
b_ed_c_sib_f = feols(death_age~county_dism + male + migrated + married + educ_cat |byear + STATEFIP_b + urb_code + OCC + sib_group_id_flexible, data = db_f, vcov = ~death_fips)
w_ed_sib_f   = feols(death_age~county_dism*educ_cat + male + migrated + married |byear + STATEFIP_b + urb_code + OCC + sib_group_id_flexible, data = dw_f, vcov = ~death_fips)
b_ed_sib_f   = feols(death_age~county_dism*educ_cat + male + migrated + married |byear + STATEFIP_b + urb_code + OCC + sib_group_id_flexible, data = db_f, vcov = ~death_fips)

sib_f_ft_w <- ftest_vals(w_ed_c_sib_f, w_ed_sib_f)
sib_f_ft_b <- ftest_vals(b_ed_c_sib_f, b_ed_sib_f)

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
# ------------------------------- White
education_white %>% 
mutate(
  educ_cat = factor(educ_cat, levels = c("Less than HS","High School","Some College","College+")),
  estimate = estimate*contrast,
  conf.high = conf.high*contrast,
  conf.low = conf.low*contrast
) %>% 
  filter(strategy == "RDI IV") %>%
  ggplot(aes(educ_cat,
             estimate,
             ymin = conf.low,
             ymax = conf.high,
             shape = strategy)) + 
  geom_pointrange(position = position_dodge2(width =.5),
                  lwd = 2,
                  size = .75) + 
  facet_wrap(~strategy,ncol = 3) + 
  labs(y = "Change in Life Expectancy",
       x = "Education Level",
       caption = str_wrap("This figure displays results of a model with an interaction between education-level and segregation for White Americans. 
       The model includes covariates and fixed effects",100)) +
  theme_cowplot() + 
  geom_hline(yintercept = 0,linetype = "dashed",lwd = 1, color = "gray") + 
  theme(plot.caption = element_text(hjust = 0),
        legend.position = "bottom") 

# ------------------------------- Black
education_black %>% 
  filter(strategy == "RDI IV") %>%
  mutate(
    educ_cat = factor(educ_cat, levels = c("Less than HS","High School","Some College","College+")),
    estimate = estimate*contrast,
    conf.high = conf.high*contrast,
    conf.low = conf.low*contrast
  ) %>% 
  ggplot(aes(educ_cat,
             estimate,
             ymin = conf.low,
             ymax = conf.high,
             shape = strategy)) + 
  geom_pointrange(position = position_dodge2(width =.5),
                  lwd = 2,
                  size = .75) + 
  facet_wrap(~strategy,ncol = 3) + 
  labs(y = "Change in Life Expectancy",
       x = "Education Level",
       caption = str_wrap("This figure displays results of a model with an interaction between education-level and segregation for White Americans. 
       The model includes covariates and fixed effects.",100)) +
  theme_cowplot() + 
  geom_hline(yintercept = 0,linetype = "dashed",lwd = 1, color = "gray") + 
  theme(plot.caption = element_text(hjust = 0),
        legend.position = "bottom") 

## ------------------------------- Create Table (Gov. IV) ------------------------------- ##
msummary(list("Black" = b_ed,
              "White" = w_ed
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
         First-stage regressions are supppressed for concision. Heteroskedasiticty Robust Standard Errors in parentheses.",
         title = "Estimates of the Effect of Segregation on Longevity by Education-Level",
         add_rows = data.frame(
           FE = c("Birth Year","Birth State","Urban-Rural Code","Occupation"),
           m1 = c("X","X","X","X"),
           m2 = c("X","X","X","X")
         )) %>%
  save_tt(.,output = here("FigTab","IV_by_Education_table.tex"), overwrite = T)

# ------------------------------- Male V Female -------------------------------
# RDI IV
d2_b_m = feols(death_age~migrated + education + married + south|byear + STATEFIP_b + urb_code + OCC  |county_dism~rdi + rail_km_per_km2, data = subset(db,male ==1),vcov = ~death_fips)
d2_w_m = feols(death_age~migrated + education + married + south|byear + STATEFIP_b + urb_code + OCC  |county_dism~rdi + rail_km_per_km2, data = subset(dw,male ==1),vcov = ~death_fips)
d2_b_f = feols(death_age~migrated + education + married + south|byear + STATEFIP_b + urb_code + OCC  |county_dism~rdi + rail_km_per_km2, data = subset(db,male ==0),vcov = ~death_fips)
d2_w_f = feols(death_age~migrated + education + married + south|byear + STATEFIP_b + urb_code + OCC  |county_dism~rdi + rail_km_per_km2, data = subset(dw,male ==0),vcov = ~death_fips)

# Rivers IV
r2_b_m = feols(death_age~migrated + education + married + south|byear + STATEFIP_b + urb_code + OCC  |county_dism~n_named_rivers + n_named_rivers_sq + stream_km_per_km2, data = subset(db,male ==1),vcov = "white")
r2_w_m = feols(death_age~migrated + education + married + south|byear + STATEFIP_b + urb_code + OCC  |county_dism~n_named_rivers + n_named_rivers_sq + stream_km_per_km2, data = subset(dw,male ==1),vcov = "white")
r2_b_f = feols(death_age~migrated + education + married + south|byear + STATEFIP_b + urb_code + OCC  |county_dism~n_named_rivers + n_named_rivers_sq + stream_km_per_km2, data = subset(db,male ==0),vcov = "white")
r2_w_f = feols(death_age~migrated + education + married + south|byear + STATEFIP_b + urb_code + OCC  |county_dism~n_named_rivers + n_named_rivers_sq + stream_km_per_km2, data = subset(dw,male ==0),vcov = "white")

# Sibling FE (Exact)
sib_e_b_m = feols(death_age~county_dism + migrated + education + married |byear + STATEFIP_b + urb_code + OCC + sib_group_id_exact, data = subset(db_f,male ==1),vcov = ~death_fips)
sib_e_w_m = feols(death_age~county_dism + migrated + education + married |byear + STATEFIP_b + urb_code + OCC + sib_group_id_exact, data = subset(dw_f,male ==1),vcov = ~death_fips)
sib_e_b_f = feols(death_age~county_dism + migrated + education + married |byear + STATEFIP_b + urb_code + OCC + sib_group_id_exact, data = subset(db_f,male ==0),vcov = ~death_fips)
sib_e_w_f = feols(death_age~county_dism + migrated + education + married |byear + STATEFIP_b + urb_code + OCC + sib_group_id_exact, data = subset(dw_f,male ==0),vcov = ~death_fips)

# Sibling FE (Flexible)
sib_f_b_m = feols(death_age~county_dism + migrated + education + married |byear + STATEFIP_b + urb_code + OCC + sib_group_id_flexible, data = subset(db_f,male ==1),vcov = ~death_fips)
sib_f_w_m = feols(death_age~county_dism + migrated + education + married |byear + STATEFIP_b + urb_code + OCC + sib_group_id_flexible, data = subset(dw_f,male ==1),vcov = ~death_fips)
sib_f_b_f = feols(death_age~county_dism + migrated + education + married |byear + STATEFIP_b + urb_code + OCC + sib_group_id_flexible, data = subset(db_f,male ==0),vcov = ~death_fips)
sib_f_w_f = feols(death_age~county_dism + migrated + education + married |byear + STATEFIP_b + urb_code + OCC + sib_group_id_flexible, data = subset(dw_f,male ==0),vcov = ~death_fips)

msummary(list("Black (Men) " = d2_b_m,
              "Black (Women)" = d2_b_f,
              "White (Men) " = d2_w_m,
              "White (Women)" = d2_w_f,
              "Black (Men) " = r2_b_m,
              "Black (Women)" = r2_b_f,
              "White (Men) " = r2_w_m,
              "White (Women)" = r2_w_f,
              "Black (Men) " = sib_e_b_m,
              "Black (Women)" = sib_e_b_f,
              "White (Men) " = sib_e_w_m,
              "White (Women)" = sib_e_w_f,
              "Black (Men) " = sib_f_b_m,
              "Black (Women)" = sib_f_b_f,
              "White (Men) " = sib_f_w_m,
              "White (Women)" = sib_f_w_f),fmt =3, stars = T,
         coef_map = c("fit_county_dism" = "D",
                      "county_dism" = "D",
                      "education" = "Education",
                      "migratedMigrated" = "Migrated",
                      "married" = "Married in 1940",
                      "south" = "Died in South"),
         gof_map = c("nobs",
                     "r.squared"

         ),
         notes = "This table describes estimates of the effect of segregation on longevity by gender across identification strategies.
         Columns 1-4 use the Railroad Diversity Index instrument, columns 5-8 the named rivers instrument, and columns 9-16 sibling fixed effects (exact and flexible matches).
         First-stage regressions are supppressed for concision. Heteroskedasiticty Robust Standard Errors in parentheses.",
         title = "Estimates of the Effect of Segregation on Longevity (By Gender)",
         add_rows = data.frame(`FE` = c("Birth Year","Birth State","Urban Rural Code","OCC","Sibling Group"),
                               m1 = c("Yes","Yes","Yes","Yes","No"),
                               m2 = c("Yes","Yes","Yes","Yes","No"),
                               m3 = c("Yes","Yes","Yes","Yes","No"),
                               m4 = c("Yes","Yes","Yes","Yes","No"),
                               m5 = c("Yes","Yes","Yes","Yes","No"),
                               m6 = c("Yes","Yes","Yes","Yes","No"),
                               m7 = c("Yes","Yes","Yes","Yes","No"),
                               m8 = c("Yes","Yes","Yes","Yes","No"),
                               m9 = c("Yes","Yes","Yes","Yes","Yes"),
                               m10 = c("Yes","Yes","Yes","Yes","Yes"),
                               m11 = c("Yes","Yes","Yes","Yes","Yes"),
                               m12 = c("Yes","Yes","Yes","Yes","Yes"),
                               m13 = c("Yes","Yes","Yes","Yes","Yes"),
                               m14 = c("Yes","Yes","Yes","Yes","Yes"),
                               m15 = c("Yes","Yes","Yes","Yes","Yes"),
                               m16 = c("Yes","Yes","Yes","Yes","Yes")),
         output = "tinytable") %>%
  save_tt(.,output = "./FigTab/IV_results_table_gender.tex", overwrite = T)


## Plot by Gender
gender_plot_data = bind_rows(
  # RDI IV
  tidy(d2_b_m, conf.int = T) %>% filter(term =="fit_county_dism") %>% mutate(Race = "Black", Model = "Men",   Estimator = "RDI IV"),
  tidy(d2_b_f, conf.int = T) %>% filter(term =="fit_county_dism") %>% mutate(Race = "Black", Model = "Women", Estimator = "RDI IV"),
  tidy(d2_w_m, conf.int = T) %>% filter(term =="fit_county_dism") %>% mutate(Race = "White", Model = "Men",   Estimator = "RDI IV"),
  tidy(d2_w_f, conf.int = T) %>% filter(term =="fit_county_dism") %>% mutate(Race = "White", Model = "Women", Estimator = "RDI IV"),
  # Rivers IV
  tidy(r2_b_m, conf.int = T) %>% filter(term =="fit_county_dism") %>% mutate(Race = "Black", Model = "Men",   Estimator = "Rivers IV"),
  tidy(r2_b_f, conf.int = T) %>% filter(term =="fit_county_dism") %>% mutate(Race = "Black", Model = "Women", Estimator = "Rivers IV"),
  tidy(r2_w_m, conf.int = T) %>% filter(term =="fit_county_dism") %>% mutate(Race = "White", Model = "Men",   Estimator = "Rivers IV"),
  tidy(r2_w_f, conf.int = T) %>% filter(term =="fit_county_dism") %>% mutate(Race = "White", Model = "Women", Estimator = "Rivers IV"),
  # Sibling FE (Exact)
  tidy(sib_e_b_m, conf.int = T) %>% filter(term =="county_dism") %>% mutate(Race = "Black", Model = "Men",   Estimator = "Sib. FE (Exact)"),
  tidy(sib_e_b_f, conf.int = T) %>% filter(term =="county_dism") %>% mutate(Race = "Black", Model = "Women", Estimator = "Sib. FE (Exact)"),
  tidy(sib_e_w_m, conf.int = T) %>% filter(term =="county_dism") %>% mutate(Race = "White", Model = "Men",   Estimator = "Sib. FE (Exact)"),
  tidy(sib_e_w_f, conf.int = T) %>% filter(term =="county_dism") %>% mutate(Race = "White", Model = "Women", Estimator = "Sib. FE (Exact)"),
  # Sibling FE (Flexible)
  tidy(sib_f_b_m, conf.int = T) %>% filter(term =="county_dism") %>% mutate(Race = "Black", Model = "Men",   Estimator = "Sib. FE (Flexible)"),
  tidy(sib_f_b_f, conf.int = T) %>% filter(term =="county_dism") %>% mutate(Race = "Black", Model = "Women", Estimator = "Sib. FE (Flexible)"),
  tidy(sib_f_w_m, conf.int = T) %>% filter(term =="county_dism") %>% mutate(Race = "White", Model = "Men",   Estimator = "Sib. FE (Flexible)"),
  tidy(sib_f_w_f, conf.int = T) %>% filter(term =="county_dism") %>% mutate(Race = "White", Model = "Women", Estimator = "Sib. FE (Flexible)")
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
       IV models use the Railroad Diversity Index (RDI IV) and named rivers (Rivers IV) as instruments; sibling FE models compare siblings within a family.
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


#------------------------------- Weights -------------------------------  

# RDI IV
d2_b_wt = feols(death_age~male + migrated + education + married + south|byear + STATEFIP_b + urb_code + OCC  |county_dism~rdi + rail_km_per_km2, data = db,vcov = ~death_fips,weights = db$weight)
d2_w_wt = feols(death_age~male + migrated + education + married + south|byear + STATEFIP_b + urb_code + OCC  |county_dism~rdi + rail_km_per_km2, data = dw,vcov = ~death_fips,weights = dw$weight)

# Rivers IV
r2_b_wt = feols(death_age~male + migrated + education + married + south|byear + STATEFIP_b + urb_code + OCC  |county_dism~n_named_rivers + n_named_rivers_sq + stream_km_per_km2, data = db,vcov = "white",weights = db$weight)
r2_w_wt = feols(death_age~male + migrated + education + married + south|byear + STATEFIP_b + urb_code + OCC  |county_dism~n_named_rivers + n_named_rivers_sq + stream_km_per_km2, data = dw,vcov = "white",weights = dw$weight)

# Sibling FE (Exact)
sib_e_b_wt = feols(death_age~county_dism + male + migrated + education + married |byear + STATEFIP_b + urb_code + OCC + sib_group_id_exact, data = db_f,vcov = ~death_fips,weights = db_f$weight)
sib_e_w_wt = feols(death_age~county_dism + male + migrated + education + married |byear + STATEFIP_b + urb_code + OCC + sib_group_id_exact, data = dw_f,vcov = ~death_fips,weights = dw_f$weight)

# Sibling FE (Flexible)
sib_f_b_wt = feols(death_age~county_dism + male + migrated + education + married |byear + STATEFIP_b + urb_code + OCC + sib_group_id_flexible, data = db_f,vcov = ~death_fips,weights = db_f$weight)
sib_f_w_wt = feols(death_age~county_dism + male + migrated + education + married |byear + STATEFIP_b + urb_code + OCC + sib_group_id_flexible, data = dw_f,vcov = ~death_fips,weights = dw_f$weight)

msummary(list("Black\\newline RDI IV" = d2_b_wt,
              "White\\newline RDI IV" = d2_w_wt,
              "Black\\newline Rivers IV" = r2_b_wt,
              "White\\newline Rivers IV" = r2_w_wt,
              "Black\\newline Sib. FE (Exact)" = sib_e_b_wt,
              "White\\newline Sib. FE (Exact)" = sib_e_w_wt,
              "Black\\newline Sib. FE (Flexible)" = sib_f_b_wt,
              "White\\newline Sib. FE (Flexible)" = sib_f_w_wt),fmt =3, stars = T,
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
         IV models use the Railroad Diversity Index and named rivers as instruments; sibling FE models compare siblings within a family.
         First-stage regressions are supppressed for concision. Heteroskedasiticty Robust Standard Errors in parentheses.",
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
  save_tt(.,output = here("FigTab","IV_results_weights_table.tex"), overwrite = T)



# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Alternative Measures of D 

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ------------------------------- RDI IV -------------------------------
H_b_rdi  = feols(death_age~male + migrated + education + married + south|byear + STATEFIP_b + urb_code + OCC  |H_bw~rdi + rail_km_per_km2, data = db,vcov = ~death_fips)
H_w_rdi  = feols(death_age~male + migrated + education + married + south|byear + STATEFIP_b + urb_code + OCC  |H_bw~rdi + rail_km_per_km2, data = dw,vcov = ~death_fips)
I_b_rdi  = feols(death_age~male + migrated + education + married + south|byear + STATEFIP_b + urb_code + OCC  |county_isolb~rdi + rail_km_per_km2, data = db,vcov = ~death_fips)
I_w_rdi  = feols(death_age~male + migrated + education + married + south|byear + STATEFIP_b + urb_code + OCC  |county_isolb~rdi + rail_km_per_km2, data = dw,vcov = ~death_fips)
FD_b_rdi = feols(death_age~male + migrated + education + married + south|byear + STATEFIP_b + urb_code + OCC  |D_star~rdi + rail_km_per_km2, data = db,vcov = ~death_fips)
FD_w_rdi = feols(death_age~male + migrated + education + married + south|byear + STATEFIP_b + urb_code + OCC  |D_star~rdi + rail_km_per_km2, data = dw,vcov = ~death_fips)

# ------------------------------- Rivers IV -------------------------------
H_b_riv  = feols(death_age~male + migrated + education + married + south|byear + STATEFIP_b + urb_code + OCC  |H_bw~n_named_rivers + n_named_rivers_sq + stream_km_per_km2, data = db,vcov = "white")
H_w_riv  = feols(death_age~male + migrated + education + married + south|byear + STATEFIP_b + urb_code + OCC  |H_bw~n_named_rivers + n_named_rivers_sq + stream_km_per_km2, data = dw,vcov = "white")
I_b_riv  = feols(death_age~male + migrated + education + married + south|byear + STATEFIP_b + urb_code + OCC  |county_isolb~n_named_rivers + n_named_rivers_sq + stream_km_per_km2, data = db,vcov = "white")
I_w_riv  = feols(death_age~male + migrated + education + married + south|byear + STATEFIP_b + urb_code + OCC  |county_isolb~n_named_rivers + n_named_rivers_sq + stream_km_per_km2, data = dw,vcov = "white")
FD_b_riv = feols(death_age~male + migrated + education + married + south|byear + STATEFIP_b + urb_code + OCC  |D_star~n_named_rivers + n_named_rivers_sq + stream_km_per_km2, data = db,vcov = "white")
FD_w_riv = feols(death_age~male + migrated + education + married + south|byear + STATEFIP_b + urb_code + OCC  |D_star~n_named_rivers + n_named_rivers_sq + stream_km_per_km2, data = dw,vcov = "white")

# ------------------------------- Sibling FE (Exact) -------------------------------
# The alternative measures enter directly; identification comes from the sibling group FE.
H_b_sib_e  = feols(death_age~H_bw + male + migrated + education + married |byear + STATEFIP_b + urb_code + OCC + sib_group_id_exact, data = db_f,vcov = ~death_fips)
H_w_sib_e  = feols(death_age~H_bw + male + migrated + education + married |byear + STATEFIP_b + urb_code + OCC + sib_group_id_exact, data = dw_f,vcov = ~death_fips)
I_b_sib_e  = feols(death_age~county_isolb + male + migrated + education + married |byear + STATEFIP_b + urb_code + OCC + sib_group_id_exact, data = db_f,vcov = ~death_fips)
I_w_sib_e  = feols(death_age~county_isolb + male + migrated + education + married |byear + STATEFIP_b + urb_code + OCC + sib_group_id_exact, data = dw_f,vcov = ~death_fips)
FD_b_sib_e = feols(death_age~D_star + male + migrated + education + married |byear + STATEFIP_b + urb_code + OCC + sib_group_id_exact, data = db_f,vcov = ~death_fips)
FD_w_sib_e = feols(death_age~D_star + male + migrated + education + married |byear + STATEFIP_b + urb_code + OCC + sib_group_id_exact, data = dw_f,vcov = ~death_fips)

# ------------------------------- Sibling FE (Flexible) -------------------------------
H_b_sib_f  = feols(death_age~H_bw + male + migrated + education + married |byear + STATEFIP_b + urb_code + OCC + sib_group_id_flexible, data = db_f,vcov = ~death_fips)
H_w_sib_f  = feols(death_age~H_bw + male + migrated + education + married |byear + STATEFIP_b + urb_code + OCC + sib_group_id_flexible, data = dw_f,vcov = ~death_fips)
I_b_sib_f  = feols(death_age~county_isolb + male + migrated + education + married |byear + STATEFIP_b + urb_code + OCC + sib_group_id_flexible, data = db_f,vcov = ~death_fips)
I_w_sib_f  = feols(death_age~county_isolb + male + migrated + education + married |byear + STATEFIP_b + urb_code + OCC + sib_group_id_flexible, data = dw_f,vcov = ~death_fips)
FD_b_sib_f = feols(death_age~D_star + male + migrated + education + married |byear + STATEFIP_b + urb_code + OCC + sib_group_id_flexible, data = db_f,vcov = ~death_fips)
FD_w_sib_f = feols(death_age~D_star + male + migrated + education + married |byear + STATEFIP_b + urb_code + OCC + sib_group_id_flexible, data = dw_f,vcov = ~death_fips)

alt_coef_map = c("fit_county_dism" = "D",
                 "county_dism" = "D",
                 "fit_H_bw" = "H",
                 "H_bw" = "H",
                 "fit_county_isolb" = "I",
                 "county_isolb" = "I",
                 "fit_D_star" = "D-Adjusted",
                 "D_star" = "D-Adjusted")

alt_notes = "This table describes estimates of the effect of segregation on longevity for alternative measures of D.
         IV models use the Railroad Diversity Index and named rivers as instruments; sibling FE models compare siblings within a family, with each measure entering directly.
         First-stage regressions are supppressed for concision. Heteroskedasiticty Robust Standard Errors in parentheses.
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
msummary(list("Black (D)" = sib_m2_b,
              "Black (H)" = H_b_sib_e,
              "Black (I)" = I_b_sib_e,
              "Black (D*)" = FD_b_sib_e,
              "White (D)" = sib_m2_w,
              "White (H)" = H_w_sib_e,
              "White (I)" = I_w_sib_e,
              "White (D*)" = FD_w_sib_e,
              "Black (D)" = sib_2_m2_b,
              "Black (H)" = H_b_sib_f,
              "Black (I)" = I_b_sib_f,
              "Black (D*)" = FD_b_sib_f,
              "White (D)" = sib_2_m2_w,
              "White (H)" = H_w_sib_f,
              "White (I)" = I_w_sib_f,
              "White (D*)" = FD_w_sib_f),fmt =3, stars = T,
         coef_map = alt_coef_map,
         gof_map = c("nobs",
                     "r.squared"

         ),
         notes = "This table describes sibling fixed-effects estimates of the effect of segregation on longevity for alternative measures of D.
         Columns 1-8 use exact sibling matches and columns 9-16 use flexible sibling matches.
         Heteroskedasiticty Robust Standard Errors in parentheses.
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
  save_tt(.,output = here("FigTab","IV_results_table_alt_measure_sib.tex"), overwrite = T)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Monotonicity Descriptive
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Sibling FE has no first stage, so this check covers the two instruments only.
data_m = data_a %>% distinct(rdi, rail_km_per_km2, n_named_rivers, n_named_rivers_sq, stream_km_per_km2,
                             death_fips, death_decade, county_dism)

# ------------------------------- RDI IV -------------------------------
mon_rdi_1 = lm(county_dism~rdi + rail_km_per_km2, subset(data_m, death_decade == 1980))
mon_rdi_2 = lm(county_dism~rdi + rail_km_per_km2, subset(data_m, death_decade == 1990))
mon_rdi_3 = lm(county_dism~rdi + rail_km_per_km2, subset(data_m, death_decade == 2000))

# ------------------------------- Rivers IV -------------------------------
mon_riv_1 = lm(county_dism~n_named_rivers + n_named_rivers_sq + stream_km_per_km2, subset(data_m, death_decade == 1980))
mon_riv_2 = lm(county_dism~n_named_rivers + n_named_rivers_sq + stream_km_per_km2, subset(data_m, death_decade == 1990))
mon_riv_3 = lm(county_dism~n_named_rivers + n_named_rivers_sq + stream_km_per_km2, subset(data_m, death_decade == 2000))

data.frame(
  Decade    = c(1980, 1990, 2000),
  RDI_R2    = c(summary(mon_rdi_1)$r.squared[1],
                summary(mon_rdi_2)$r.squared[1],
                summary(mon_rdi_3)$r.squared[1]),
  Rivers_R2 = c(summary(mon_riv_1)$r.squared[1],
                summary(mon_riv_2)$r.squared[1],
                summary(mon_riv_3)$r.squared[1])
)
