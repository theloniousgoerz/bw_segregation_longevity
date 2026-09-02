# # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Examine Mechanisms
## Packages
rm(list=ls())
library(tidyverse)
library(magrittr)
library(readr)
library(modelsummary)
library(kableExtra)
library(cowplot)
library(fixest)
library(marginaleffects)
library(broom)
library(tinytable)
library(binsreg)
library(here)
library(haven)
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
# Mechanisms Data
policy_data = read_csv(here("Data","_Cleaned","county_policy_data.csv"))
income_seg = read_csv(here("Data","_Cleaned","income_segregation_Hr.csv"))
# Analytic Data
source(here("Analysis","00_Helpers.R"))

data_a =   read_csv(here("Data","_Cleaned","data_a.csv"))
rivers =      read_csv(here("Data","derived","tiger_hydrography_county_instruments_2023.csv"))
rdi =   read_csv(here("Data","derived","atack_rail_county_instruments_1911.csv"))

# Repairs birth_fips (and STATEFIP_b, which is derived from it) and drops anyone whose
# birth county is unrecorded, so this script describes the same analytic sample as the
# regression scripts. See prepare_analysis_data() in 00_Helpers.R.
data_a %<>% prepare_analysis_data("data_a")
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Data Cleaning for Analysis
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Rescale D for analysis ------------------------------

#  -------------------------------
# Merge on rdi and river instruments
#  -------------------------------
rdi %<>% mutate(death_fips = GEOID)
rivers %<>% mutate(death_fips = GEOID)
# Merge
data_a %<>% left_join(rdi) %>% left_join(rivers)
income_seg %<>% mutate(death_fips = county_fips, death_decade = year)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Mechanisms
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
segregation_data =data_a %>% distinct(death_fips,county_dism,death_decade,ln_gov,gov_rev_share,pop,rdi,n_named_rivers,n_named_rivers_sq,south,urb_code,rail_km_per_km2,pblack) %>%
  left_join(income_seg, by = c("death_decade","death_fips")) %>%
  mutate(Hr_all = Hr_all*100)

policy_data %>% summary()

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Regression table: N_governments → county fiscal policy
# Panel: county × decade (1980/1990/2000), SEs clustered at county level
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
policy_panel <- policy_data %>%
  mutate(
         across(c(welf_direct_pc, health_pc, cash_asst_pc, medicaid_pc, taxes_pc),
                \(x) log(if_else(x == 0, 0.01, x)),
                .names = "log_{.col}"),
         death_decade = decade,
         death_fips = fips5) %>% 
  left_join(., segregation_data, by = c("death_fips","death_decade")) %>%
  mutate(county_dism = county_dism/100)

# cash assistance,
#    Medicaid vendor payments, direct welfare, health/hospital spending
# z_cash
# z_medical
# z_welf
# z_health

# The six channels below are exactly the ones carried into the longevity models in
# 04_Regression_Models.R (taxes_pc, prop_tax_pc, comp_medicaid, health_pc,
# welf_direct_pc, comp_snap). Keep the two scripts' mechanism sets in sync: the
# mediation reading in the draft needs this first leg for every channel it reports.
m_welf    = feols(welf_direct_pc    ~ county_dism   | death_decade + urb_code + south , data = policy_panel, vcov = ~death_fips)
m_cash    = feols(cash_asst_pc      ~ county_dism   | death_decade + urb_code + south , data = policy_panel, vcov = ~death_fips)
m_medicaid = feols(comp_medicaid    ~ county_dism  |  death_decade + urb_code + south, data = policy_panel, vcov = ~death_fips)
m_taxes   = feols(taxes_pc          ~ county_dism   | death_decade + urb_code + south , data = policy_panel, vcov = ~death_fips)
m_prop    = feols(prop_tax_pc       ~ county_dism   | death_decade + urb_code + south , data = policy_panel, vcov = ~death_fips)
m_snap    = feols(comp_snap         ~ county_dism   | death_decade + urb_code + south , data = policy_panel, vcov = ~death_fips)
m_health  = feols(health_pc         ~ county_dism   | death_decade + urb_code + south , data = policy_panel, vcov = ~death_fips)

# Same six outcomes as the OLS block, same conditioning set, so that the OLS and IV
# rows of a given table column differ only in the estimator.
m_welf_iv       = feols(welf_direct_pc   ~ 1  | death_decade + urb_code + south| county_dism~rdi + rail_km_per_km2, data = policy_panel, vcov = ~death_fips)
m_cash_iv       = feols(cash_asst_pc     ~ 1  | death_decade + urb_code + south| county_dism~rdi + rail_km_per_km2, data = policy_panel, vcov = ~death_fips)
m_medicaid_iv   = feols(comp_medicaid    ~ 1  | death_decade + urb_code + south| county_dism~rdi + rail_km_per_km2, data = policy_panel, vcov = ~death_fips)
m_taxes_iv      = feols(taxes_pc         ~ 1  | death_decade + urb_code + south| county_dism~rdi + rail_km_per_km2, data = policy_panel, vcov = ~death_fips)
m_prop_iv       = feols(prop_tax_pc      ~ 1  | death_decade + urb_code + south| county_dism~rdi + rail_km_per_km2, data = policy_panel, vcov = ~death_fips)
m_snap_iv       = feols(comp_snap        ~ 1  | death_decade + urb_code + south| county_dism~rdi + rail_km_per_km2, data = policy_panel, vcov = ~death_fips)
m_health_iv     = feols(health_pc        ~ 1  | death_decade + urb_code + south| county_dism~rdi + rail_km_per_km2, data = policy_panel, vcov = ~death_fips)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Combined OLS + IV Table: Effect of D on County Fiscal Policy Mechanisms
# Two D rows (OLS and IV) per mechanism, with a single observations row (IV sample).
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Helper: format one D coefficient (+ stars) and its SE from a fixest model
fmt_D <- function(model, term) {
  ct   <- coeftable(model)[term, ]
  est  <- ct[["Estimate"]]
  se   <- ct[["Std. Error"]]
  p    <- ct[["Pr(>|t|)"]]
  star <- dplyr::case_when(
    p < 0.001 ~ "***",
    p < 0.01  ~ "**",
    p < 0.05  ~ "*",
    p < 0.1   ~ "+",
    TRUE      ~ ""
  )
  list(coef = sprintf("%.3f%s", est, star),
       se   = sprintf("(%.3f)", se))
}

# Column order: label, OLS model, IV model.
# Restricted to the channels that 04_Regression_Models.R actually controls for, in the
# same order as the mechanism table there, so the two tables can be read side by side.
# SNAP is estimated above but excluded here and in 04: the coefficient on D rounds to
# zero at three decimals in both OLS and IV, which is what disqualifies it as a
# mediator downstream. comp_snap varies at the state-decade level, so there is almost
# no within-state variation for a county-level D to explain.
mech_cols <- list(
  list(label = "Taxes",      ols = m_taxes,    iv = m_taxes_iv),
  list(label = "Medicaid",   ols = m_medicaid, iv = m_medicaid_iv),
  list(label = "Health",     ols = m_health,   iv = m_health_iv),
  list(label = "Welfare",    ols = m_welf,     iv = m_welf_iv)
)

ols_D  <- lapply(mech_cols, \(x) fmt_D(x$ols, "county_dism"))
iv_D   <- lapply(mech_cols, \(x) fmt_D(x$iv,  "fit_county_dism"))
n_iv   <- vapply(mech_cols, \(x) format(nobs(x$iv), big.mark = "", trim = TRUE), character(1))
labels <- vapply(mech_cols, \(x) x$label, character(1))

ncol_data <- length(mech_cols)                 # number of estimate columns
col_ids   <- paste(seq_len(ncol_data + 1), collapse = ",")   # 1..(k+1) for tabularray specs
row_line  <- function(cells) paste(cells, collapse = " & ")

combined_tex <- c(
  "\\begin{table}",
  "\\centering",
  "\\begin{talltblr}[         %% tabularray outer open",
  "caption={Effect of Segregation on County Fiscal Policy Mechanisms: OLS and IV},",
  "note{}={+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001},",
  "note{ }={OLS and IV (RDI instrument) estimates of the effect of segregation (D) on each county fiscal policy mechanism. Each column is a separate outcome, and the columns are the same policy channels controlled for in the longevity models. All models include decade (1980, 1990, 2000), urban-rural, and region fixed effects. Standard errors clustered at the county level in parentheses. D is scaled 0--1 in these county-level models (not 0--100 as in the individual-level longevity models), so each coefficient is the contrast between a perfectly integrated and a perfectly segregated county; divide by ten to read it per 10-point rise in D.},",
  "]                     %% tabularray outer close",
  "{                     %% tabularray inner open",
  paste0("colspec={", paste(rep("Q[]", ncol_data + 1), collapse = ""), "},"),
  paste0("column{", paste(seq(2, ncol_data + 1), collapse = ","), "}={}{halign=c,},"),
  "column{1}={}{halign=l,},",
  paste0("hline{6}={", col_ids, "}{solid, black, 0.05em},"),
  "}                     %% tabularray inner close",
  "\\toprule",
  paste0(row_line(c("", labels)), " \\\\ \\midrule %% TinyTableHeader"),
  paste0(row_line(c("D (OLS)",  vapply(ols_D, \(x) x$coef, character(1)))), " \\\\"),
  paste0(row_line(c("",         vapply(ols_D, \(x) x$se,   character(1)))), " \\\\"),
  paste0(row_line(c("D (IV)",   vapply(iv_D,  \(x) x$coef, character(1)))), " \\\\"),
  paste0(row_line(c("",         vapply(iv_D,  \(x) x$se,   character(1)))), " \\\\"),
  paste0(row_line(c("Num.Obs.", n_iv)), " \\\\"),
  "\\bottomrule",
  "\\end{talltblr}",
  "\\end{table}"
)

writeLines(combined_tex, here("FigTab", "mechanism_ols_iv_table.tex"))

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Descriptive Statistics: Mechanism Outcomes by Decade
# Outcome names are read off mech_cols, so this table covers exactly the channels
# reported in the OLS/IV table and coefficient plot above.
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mech_vars <- vapply(mech_cols, \(x) all.vars(formula(x$ols)[[2]]), character(1))
names(mech_vars) <- labels

mech_desc_data <- policy_panel %>%
  filter(!is.na(death_decade)) %>%
  select(death_decade, all_of(mech_vars)) %>%
  mutate(Decade = factor(death_decade)) %>%
  select(-death_decade)

# One N row per decade: counties contributing to the moments (non-missing on every
# mechanism). Blank under the SD columns so the count is not printed twice.
mech_desc_n <- mech_desc_data %>%
  filter(if_all(all_of(names(mech_vars)), \(x) !is.na(x))) %>%
  count(Decade) %>%
  mutate(n = format(n, big.mark = ",", trim = TRUE))

datasummary(
  All(mech_desc_data) ~ Decade * (Mean + SD),
  data           = mech_desc_data,
  title          = "Descriptive Statistics for County Fiscal Policy Mechanisms by Decade",
  fmt            = 2,
  add_rows       = data.frame(t(c("Counties", as.vector(rbind(mech_desc_n$n, ""))))),
  notes          = "Mean and standard deviation of each county fiscal policy mechanism, by decade. Taxes, health, and welfare spending are measured per capita; Medicaid is the Medicaid generosity index. Counts are county-decade observations with non-missing values on all four mechanisms.",
  threeparttable = TRUE,
  output         = "tinytable"
) %>%
  save_tt(here("FigTab", "mechanism_descriptives_table.tex"), overwrite = TRUE)

# -----------------------
# Save mechanism data
# -----------------------
write_csv(policy_panel, here("Data","_Cleaned","mechanism.csv"))

