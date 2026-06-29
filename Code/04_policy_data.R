#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Title: County-Level Policy Data (GFD + SPPD hybrid)
# Thelonious Goerz
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Constructs a hybrid liberalism index following Montez et al. (2026,
# Milbank Quarterly). For each of the 11 SPPD index items, uses the
# county-level GFD analogue where one exists; falls back to the state-level
# SPPD variable where no county analogue is available.
#
# GFD county analogues (items with county-level data):
#   Item 1  - TANF/AFDC  : GFD categorical + general cash assistance per capita
#   Item 5  - Medicaid   : GFD vendor medical payments per capita
#             (no state fallback; medicaidexp is NA pre-2014 in SPPD)
#
# State-level SPPD (items without county analogues):
#   Item 2  - SNAP              (SNAP_2023)
#   Item 3  - EITC              (eitc)
#   Item 4  - Minimum wage      (state_minwage_2023)
#   Item 6  - Unemployment ins. (ui_max_2023)
#   Item 7  - Paid sick leave   (psl)
#   Item 8  - Right-to-work     (1 - rtw; reversed)
#   Item 9  - Labor preemption  (-preempt_total; reversed)
#   Item 10 - Tobacco tax       (tobaccotax_2023)
#   Item 11 - Firearm laws      (CAP + (1-SYG) + (1-RTC))
#
# Index construction (Montez et al. 2026):
#   (1) normalize each component to [0,1] globally across all county-decades
#   (2) sum available normalized scores per county-decade
#   (3) re-normalize sum to [0,1]
#
# GFD Type_Code:
#   0=State  1=County  2=Municipal  3=Township  4=Special  5=School  6=Federal
#
# Population denominators: NHGIS decennial census tract data (nhgis0017)
# aggregated to county via 03_county_data.R.
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
rm(list = ls())
library(tidyverse)
library(data.table)
library(magrittr)
library(haven)
library(here)

minmax <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (rng[2] == rng[1]) return(rep(0, length(x)))
  (x - rng[1]) / (rng[2] - rng[1])
}

# ============================================================
# 1. Load GFD
# ============================================================
gfd_raw <- fread(
  here("Data", "GFD", "Government Finance Database All Data",
       "The Government Finance Database_All Data.csv"),
  na.strings = c("", "NA", "."),
  colClasses = list(
    character = c("FIPS_Code_State", "FIPS_County", "FIPS_Place", "FIPS_Combined",
                  "GOVSid", "FIPSid", "Name")
  )
)

# ============================================================
# 2. Filter to county governments and target decade windows
# ============================================================
gfd_county <- gfd_raw %>%
  filter(Type_Code == 1) %>%
  filter(!is.na(Year4), Year4 > 0) %>%
  filter(!is.na(FIPS_Combined), FIPS_Combined != "") %>%
  mutate(
    fips5 = str_pad(FIPS_Combined, 5, pad = "0"),
    decade = case_when(
      Year4 >= 1980 & Year4 <= 1989 ~ 1980L,
      Year4 >= 1990 & Year4 <= 1999 ~ 1990L,
      Year4 >= 2000 & Year4 <= 2005 ~ 2000L,
      TRUE ~ NA_integer_
    )
  ) %>%
  filter(!is.na(decade))

# ============================================================
# 3. Define GFD spending variables
# ============================================================
welfare_vars <- c(
  "Public_Welf_Direct_Exp",
  "Welf_Categ_Cash_Assist",
  "Welf_Cash_Cash_Assist",
  "Welf_Vend_Pmts_Medical"
)
health_vars  <- c("Health_Direct_Expend", "Total_Hospital_Total_Exp")
educ_vars    <- c("Total_Educ_Direct_Exp")
tax_vars     <- c("Total_Taxes", "Property_Tax", "Individual_Income_Tax")

all_spend_vars <- c(welfare_vars, health_vars, educ_vars, tax_vars)

gfd_county %<>%
  mutate(across(all_of(all_spend_vars),
                ~ suppressWarnings(as.numeric(.)), .names = "{.col}")) %>%
  mutate(across(all_of(all_spend_vars),
                ~ ifelse(. < 0, NA_real_, .)))

# ============================================================
# 4. Aggregate GFD: sum sub-units per county-year, average within decade
# ============================================================
county_decade <- gfd_county %>%
  group_by(fips5, Year4, decade) %>%
  summarise(across(all_of(all_spend_vars), ~ sum(., na.rm = TRUE)),
            .groups = "drop") %>%
  group_by(fips5, decade) %>%
  summarise(across(all_of(all_spend_vars), ~ mean(., na.rm = TRUE)),
            n_years_observed = n(),
            .groups = "drop")

# ============================================================
# 5. Population denominators
# ============================================================
pop_data <- read_csv(
  here("Data", "_Cleaned", "county_data.csv"),
  show_col_types = FALSE
) %>%
  filter(death_decade %in% c(1980, 1990, 2000)) %>%
  select(FIPS_Combined, death_decade, pop) %>%
  mutate(fips5 = str_pad(as.character(FIPS_Combined), 5, pad = "0")) %>%
  rename(decade = death_decade) %>%
  distinct(fips5, decade, .keep_all = TRUE) %>%
  select(fips5, decade, pop)

# ============================================================
# 6. GFD per-capita measures ($ per 1,000 residents)
# ============================================================
county_pc <- county_decade %>%
  left_join(pop_data, by = c("fips5", "decade")) %>%
  filter(!is.na(pop), pop > 0) %>%
  mutate(
    welf_direct_pc = Public_Welf_Direct_Exp / pop * 1000,
    cash_asst_pc   = (Welf_Categ_Cash_Assist + Welf_Cash_Cash_Assist) / pop * 1000,
    medicaid_pc    = Welf_Vend_Pmts_Medical  / pop * 1000,
    health_pc      = (Health_Direct_Expend + Total_Hospital_Total_Exp) / pop * 1000,
    educ_pc        = Total_Educ_Direct_Exp   / pop * 1000,
    taxes_pc       = Total_Taxes             / pop * 1000,
    prop_tax_pc    = Property_Tax            / pop * 1000,
    inc_tax_pc     = Individual_Income_Tax   / pop * 1000
  )

# ============================================================
# 7. GFD-only county liberalism index (4 components: cash assistance,
#    Medicaid vendor payments, direct welfare, health/hospital spending)
#    Normalized globally across counties and decades with GFD coverage.
# ============================================================
county_pc <- county_pc %>%
  mutate(
    z_cash    = minmax(cash_asst_pc),
    z_medical = minmax(medicaid_pc),
    z_welf    = minmax(welf_direct_pc),
    z_health  = minmax(health_pc)
  ) %>%
  rowwise() %>%
  mutate(gfd_sum = sum(c(z_cash, z_medical, z_welf, z_health), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(county_lib_index = minmax(gfd_sum)) %>%
  select(-gfd_sum)

# ============================================================
# 8. SPPD state-level components averaged within decade windows
#    Components 2-4, 6-11 (no county analogue); also TANF as fallback
#    for item 1 when county GFD is missing.
#    Note: medicaidexp is NA pre-2014; no usable state fallback for item 5.
# ============================================================
sppd_raw <- read_dta(here("Data", "SPPD", "SPPD_V2.0.dta"))

sppd_decade <- sppd_raw %>%
  mutate(
    decade = case_when(
      year >= 1980 & year <= 1989 ~ 1980L,
      year >= 1990 & year <= 1999 ~ 1990L,
      year >= 2000 & year <= 2005 ~ 2000L,
      TRUE ~ NA_integer_
    ),
    # direction: higher = more liberal
    firearms_restrictive = CAP + (1 - SYG) + (1 - RTC),
    no_rtw               = 1 - rtw,
    less_preempt         = -preempt_total
  ) %>%
  filter(!is.na(decade), !is.na(state_fips)) %>%
  group_by(state_fips, decade) %>%
  summarise(
    sppd_tanf         = mean(TANF2023,           na.rm = TRUE),  # item 1 fallback
    sppd_snap         = mean(SNAP_2023,           na.rm = TRUE),  # item 2
    sppd_eitc         = mean(eitc,                na.rm = TRUE),  # item 3
    sppd_minwage      = mean(state_minwage_2023,  na.rm = TRUE),  # item 4
    sppd_ui           = mean(ui_max_2023,         na.rm = TRUE),  # item 6
    sppd_psl          = mean(psl,                 na.rm = TRUE),  # item 7
    sppd_no_rtw       = mean(no_rtw,              na.rm = TRUE),  # item 8
    sppd_less_preempt = mean(less_preempt,        na.rm = TRUE),  # item 9
    sppd_tobacco      = mean(tobaccotax_2023,     na.rm = TRUE),  # item 10
    sppd_firearms     = mean(firearms_restrictive, na.rm = TRUE), # item 11
    .groups = "drop"
  ) %>%
  mutate(statefips = str_pad(as.integer(state_fips), 2, pad = "0"))

# ============================================================
# 8. Assemble hybrid dataset: all county-decades from pop_data
# ============================================================
county_hybrid <- pop_data %>%
  left_join(
    county_pc %>% select(fips5, decade, welf_direct_pc, cash_asst_pc,
                         medicaid_pc, health_pc, educ_pc, taxes_pc,
                         prop_tax_pc, inc_tax_pc, n_years_observed,
                         county_lib_index),
    by = c("fips5", "decade")
  ) %>%
  mutate(statefips = str_sub(fips5, 1, 2)) %>%
  left_join(sppd_decade %>% select(-state_fips),
            by = c("statefips", "decade")) %>%
  mutate(
    # Item 1 - TANF: GFD cash assistance; SPPD as fallback
    comp_tanf     = if_else(!is.na(cash_asst_pc), cash_asst_pc, sppd_tanf),
    tanf_source   = case_when(
      !is.na(cash_asst_pc) ~ "county_gfd",
      !is.na(sppd_tanf)    ~ "state_sppd",
      TRUE                 ~ NA_character_
    ),
    # Item 2-4, 6-11: always state SPPD (no county analogue)
    comp_snap     = sppd_snap,
    comp_eitc     = sppd_eitc,
    comp_minwage  = sppd_minwage,
    # Item 5 - Medicaid: GFD vendor payments only; no state fallback pre-2014
    comp_medicaid = medicaid_pc,
    medicaid_source = if_else(!is.na(medicaid_pc), "county_gfd", NA_character_),
    comp_ui       = sppd_ui,
    comp_psl      = sppd_psl,
    comp_no_rtw   = sppd_no_rtw,
    comp_less_preempt = sppd_less_preempt,
    comp_tobacco  = sppd_tobacco,
    comp_firearms = sppd_firearms
  )

# ============================================================
# 9. Normalize all 11 components globally, sum, re-normalize
# ============================================================
comp_vars <- c("comp_tanf", "comp_snap", "comp_eitc", "comp_minwage",
               "comp_medicaid", "comp_ui", "comp_psl", "comp_no_rtw",
               "comp_less_preempt", "comp_tobacco", "comp_firearms")

county_policy <- county_hybrid %>%
  mutate(across(all_of(comp_vars), minmax, .names = "n_{.col}")) %>%
  rowwise() %>%
  mutate(
    lib_sum = sum(c_across(starts_with("n_comp_")), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    lib_index_final = minmax(lib_sum),
    lib_source = case_when(
      !is.na(tanf_source) & tanf_source == "county_gfd" ~ "county_gfd",
      !is.na(tanf_source) & tanf_source == "state_sppd" ~ "state_sppd",
      TRUE ~ NA_character_
    )
  )

# ============================================================
# 10. Descriptive check
# ============================================================
county_policy %>%
  group_by(decade, lib_source) %>%
  summarise(
    n          = n(),
    mean_index = mean(lib_index_final, na.rm = TRUE),
    sd_index   = sd(lib_index_final, na.rm = TRUE),
    pct_missing_medicaid = mean(is.na(comp_medicaid)),
    .groups = "drop"
  ) %>%
  print()

# ============================================================
# 11. Save
# ============================================================
county_policy %>%
  select(
    fips5, statefips, decade, pop,
    # GFD per-capita variables (stored for reference)
    welf_direct_pc, cash_asst_pc, medicaid_pc, health_pc,
    educ_pc, taxes_pc, prop_tax_pc, inc_tax_pc, n_years_observed,
    # Hybrid components (pre-normalization)
    comp_tanf, tanf_source,
    comp_snap, comp_eitc, comp_minwage,
    comp_medicaid, medicaid_source,
    comp_ui, comp_psl, comp_no_rtw, comp_less_preempt,
    comp_tobacco, comp_firearms,
    # Indexes
    county_lib_index, lib_sum, lib_index_final, lib_source
  ) %>%
  write_csv(here("Data", "_Cleaned", "county_policy_data.csv"))
