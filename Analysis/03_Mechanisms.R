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
vote_share_gov = read_csv(here("Data","_Cleaned","gubernatorial_returns_decades.csv"))
vote_share_pres = read_csv(here("Data","_Cleaned","presidential_returns_decades.csv"))
# Analytic Data 
data_a =   read_csv(here("Data","_Cleaned","data_a.csv"))                                                                        
rivers =      read_csv(here("Data","derived","tiger_hydrography_county_instruments_2023.csv"))
rdi =   read_csv(here("Data","derived","atack_rail_county_instruments_1911.csv"))
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Data Cleaning for Analysis
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Rescale D for analysis -------------------------------

data_a %<>% mutate(county_dism = county_dism*100)

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


vote = 
  vote_share_gov %>% 
  mutate(death_decade = decade,
         death_fips = fips) %>% 
    distinct(death_decade,death_fips,gov_party,gov_party_consistent) 

segregation_data %<>% left_join(vote)

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
  left_join(., segregation_data, by = c("death_fips","death_decade"))

m_lib     = feols(county_lib_index ~ county_dism + south |  death_decade + urb_code, data = policy_panel, vcov = ~death_fips)
m_lib_c   = feols(lib_index_final  ~ county_dism + south | death_decade + urb_code, data = policy_panel, vcov = ~death_fips)
m_welf    = feols(welf_direct_pc   ~ county_dism + south | death_decade + urb_code, data = policy_panel, vcov = ~death_fips)
m_health  = feols(health_pc        ~ county_dism + south | death_decade + urb_code, data = policy_panel, vcov = ~death_fips)
m_cash    = feols(cash_asst_pc     ~ county_dism + south | death_decade + urb_code, data = policy_panel, vcov = ~death_fips)
m_medical = feols(medicaid_pc      ~ county_dism + south | death_decade + urb_code, data = policy_panel, vcov = ~death_fips)
m_taxes   = feols(taxes_pc         ~ county_dism + south | death_decade + urb_code, data = policy_panel, vcov = ~death_fips)
m_inc_seg = feols(Hr_all           ~ county_dism + south  |  death_decade + urb_code, data =  policy_panel, vcov = ~death_fips)
m_tanf    = feols(comp_tanf        ~ county_dism + south | death_decade + urb_code, data = policy_panel, vcov = ~death_fips)
m_snap    = feols(comp_snap        ~ county_dism + south | death_decade + urb_code, data = policy_panel, vcov = ~death_fips)
m_eitc    = feols(comp_eitc        ~ county_dism + south | death_decade + urb_code, data = policy_panel, vcov = ~death_fips)
m_educ    = feols(educ_pc          ~ county_dism + south | death_decade + urb_code, data = policy_panel, vcov = ~death_fips)

msummary(
  list(
    "Lib. Index"    = m_lib,
    "Lib Index (c)"     =m_lib_c,
    "Welfare"       = m_welf,
    "Health"        = m_health,
    "Cash Asst."    = m_cash,
    "Medicaid"      = m_medical,
    "Taxes"         = m_taxes,
    "Income Seg"   = m_inc_seg
  ),
#  coef_map  = c("county_dism" = "D"),
  gof_map   = c("nobs", "r.squared"),
  stars     = TRUE,
  fmt       = 3,
  title     = "OLS Association Between Segregation and Mechanisms",
  notes     = "Standard errors clustered at the county level. All models include decade fixed effects (1980, 1990, 2000). Per-capita spending variables winsorized at the 99th percentile.",
 # add_rows  = data.frame(FE = "Decade FE",
 #                        m1 = "X", m2 = "X", m3 = "X",
 #                        m4 = "X", m5 = "X", m6 = "X"),
  #align     = "lcccccc",
  threeparttable = TRUE,
  output    = "tinytable"
) %>%
  save_tt(here("FigTab","gov_policy_table.tex"), overwrite = TRUE)


m_lib_iv        = feols(county_lib_index ~ 1 + south| death_decade + urb_code | county_dism~rdi + rail_km_per_km2, data = policy_panel, vcov = ~death_fips)
m_lib_c_iv      = feols(lib_index_final  ~ 1 + south| death_decade + urb_code | county_dism~rdi + rail_km_per_km2, data = policy_panel, vcov = ~death_fips)
m_welf_iv       = feols(welf_direct_pc   ~ 1 + south| death_decade + urb_code | county_dism~rdi + rail_km_per_km2, data = policy_panel, vcov = ~death_fips)
m_health_iv     = feols(health_pc        ~ 1 + south| death_decade + urb_code | county_dism~rdi + rail_km_per_km2, data = policy_panel, vcov = ~death_fips)
m_cash_iv       = feols(cash_asst_pc     ~ 1 + south| death_decade + urb_code | county_dism~rdi + rail_km_per_km2, data = policy_panel, vcov = ~death_fips)
m_medical_iv    = feols(medicaid_pc      ~ 1 + south| death_decade + urb_code | county_dism~rdi + rail_km_per_km2, data = policy_panel, vcov = ~death_fips)
m_taxes_iv      = feols(taxes_pc         ~ 1 + south| death_decade + urb_code | county_dism~rdi + rail_km_per_km2, data = policy_panel, vcov = ~death_fips)
m_income_seg_iv = feols(Hr_all           ~ 1 + south| death_decade + urb_code | county_dism~rdi + rail_km_per_km2, data = policy_panel, vcov = ~death_fips)
m_tanf_iv       = feols(comp_tanf        ~ 1 + south| death_decade + urb_code | county_dism~rdi + rail_km_per_km2, data = policy_panel, vcov = ~death_fips)
m_snap_iv       = feols(comp_snap        ~ 1 + south| death_decade + urb_code | county_dism~rdi + rail_km_per_km2, data = policy_panel, vcov = ~death_fips)
m_eitc_iv       = feols(comp_eitc        ~ 1 + south| death_decade + urb_code | county_dism~rdi + rail_km_per_km2, data = policy_panel, vcov = ~death_fips)
m_educ_iv       = feols(educ_pc          ~ 1 + south| death_decade + urb_code | county_dism~rdi + rail_km_per_km2, data = policy_panel, vcov = ~death_fips)

msummary(
  list(
    "Lib. Index"    = m_lib_iv,
    "Lib Index (c)"    = m_lib_c_iv,
    "Welfare"       = m_welf_iv,
    "Health"        = m_health_iv,
    "Cash Asst."    = m_cash_iv,
    "Medicaid"      = m_medical_iv,
    "Taxes"         = m_taxes_iv,
    "Income Seg"    = m_income_seg_iv
  ),
  coef_map  = c("fit_county_dism" = "D"),
  gof_map   = c("nobs", "r.squared"),
  stars     = TRUE,
  fmt       = 3,
  title     = "IV Estimates of D on County Fiscal Policy",
  notes     = "Standard errors clustered at the county level. All models include decade fixed effects (1980, 1990, 2000).",
  #add_rows  = data.frame(FE = "Decade FE",
  #                       m1 = "X", m2 = "X", m3 = "X",
  #                       m4 = "X", m5 = "X", m6 = "X"),
  #align     = "lcccccc",
  threeparttable = TRUE,
  output    = "tinytable"
) %>%
  save_tt(here("FigTab","gov_policy_table.tex"), overwrite = TRUE)

# -----------------------
# Combined OLS + IV coefficient plot across mechanism outcomes

coef_ols <- map_dfr(
  list("Lib. Index" = m_lib_c, "Welfare" = m_welf, "Health" = m_health,
       "Cash Asst." = m_cash, "Medicaid" = m_medical, "Taxes" = m_taxes,
       "Income Seg" = m_inc_seg, "TANF" = m_tanf, "SNAP" = m_snap,
       "EITC" = m_eitc, "Education" = m_educ),
  \(m) broom::tidy(m, conf.int = TRUE) |> filter(term == "county_dism"),
  .id = "outcome"
) |> mutate(estimator = "OLS")

coef_iv <- map_dfr(
  list("Lib. Index" = m_lib_c_iv, "Welfare" = m_welf_iv, "Health" = m_health_iv,
       "Cash Asst." = m_cash_iv, "Medicaid" = m_medical_iv, "Taxes" = m_taxes_iv,
       "Income Seg" = m_income_seg_iv, "TANF" = m_tanf_iv, "SNAP" = m_snap_iv,
       "EITC" = m_eitc_iv, "Education" = m_educ_iv),
  \(m) broom::tidy(m, conf.int = TRUE) |> filter(term == "fit_county_dism"),
  .id = "outcome"
) |> mutate(estimator = "IV")

coef_mech <- bind_rows(coef_ols, coef_iv) |>
  mutate(
    outcome   = factor(outcome, levels = c("Taxes", "Income Seg", "Cash Asst.",
                "Lib. Index", "Welfare", "Health", "Medicaid",
                "TANF", "SNAP", "EITC", "Education")),
    estimator = factor(estimator, levels = c("OLS", "IV"))
  )

mech_figure = ggplot(coef_mech, aes(x = estimator, y = estimate, color = estimator, shape = estimator)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.15, position = position_dodge(width = 0.2)) +
  geom_point(size = 3, position = position_dodge(width = 0.2)) +
  labs(
    x = NULL, y = "Coefficient on D (Segregation)",
    color = NULL, shape = NULL,
    title = "Effect of Segregation on County Fiscal Policy Mechanisms"
  ) +
  facet_wrap(~outcome, scales = "free_y", nrow = 4) + 
  theme_cowplot() +
  theme(
    legend.position = "bottom"
  )

ggsave(mech_figure, filename = here("FigTab","mechanism.jpeg"),
       width = 10,
       height = 10,
       dpi = 1000)

# -----------------------
# Save mechanism data 
# -----------------------
write_csv(policy_panel, here("Data","_Cleaned","mechanism.csv"))

