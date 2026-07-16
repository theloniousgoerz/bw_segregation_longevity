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

# ------------------------------- OLS -------------------------------
ols_m1_b = feols(death_age~county_dism   |byear + STATEFIP_b + urb_code, data = db,vcov = "white")
ols_m1_w = feols(death_age~county_dism   |byear + STATEFIP_b + urb_code, data = dw,vcov = "white")
ols_m2_b = feols(death_age~county_dism + male + migrated + education + married |byear + STATEFIP_b  + urb_code +OCC, data = db,vcov = "white")
ols_m2_w = feols(death_age~county_dism + male + migrated + education + married |byear + STATEFIP_b  + urb_code +OCC, data = dw,vcov = "white")

  msummary(list("Black" = ols_m1_b,
                "Black\\newline Controls" = ols_m2_b,
                "White" = ols_m1_w,
                "White\\newline Controls" = ols_m2_w),fmt =3, stars = T,
           coef_map = c("county_dism" = "D",
                        "male" = "Male",
                        "education" = "Education",
                        "migratedMigrated" = "Migrated",
                        "married" = "Married in 1940"),
           gof_map = c("nobs",
                       "r.squared"
           ),
           notes = "Heteroskedasiticty Robust Standard Errors in parentheses.",
           title = "Estimates of the Association Betwen Segregation and Longevity",
           add_rows = data.frame(
             FE = c("Birth Year","Birth State","Urban-Rural Code","Occupation"),
             m1 = c("X","X","X","-"),
             m2 = c("X","X","X","X"),
             m3 = c("X","X","X","-"),
             m4 = c("X","X","X","X")
           )
           ) %>% 
    save_tt(.,output = here("FigTab","OLS_results_table.tex"), overwrite = T)
  
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## IV analysis
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ------------------------------- Estimate IV Models (RDI) -------------------------------
  
  d1_b = feols(death_age~1|byear + STATEFIP_b + urb_code |county_dism~rdi + rail_km_per_km2, data = db, vcov =~death_fips)
  d1_w = feols(death_age~1|byear + STATEFIP_b + urb_code |county_dism~rdi + rail_km_per_km2, data = dw, vcov =~death_fips)
  d2_b = feols(death_age~male + migrated + education + married + south|byear + STATEFIP_b + urb_code + OCC |county_dism~rdi + rail_km_per_km2, data = db, vcov =~death_fips)
  d2_w = feols(death_age~male + migrated + education + married + south|byear + STATEFIP_b + urb_code + OCC |county_dism~rdi + rail_km_per_km2, data = dw, vcov =~death_fips)
  
  msummary(list("Black\\newline 1st Stage" = summary(d1_b, stage = 1),
                "Black" = d1_b,
                "Black\\newline Controls\\newline 1st Stage" = summary(d2_b, stage = 1),
                "Black\\newline Controls" = d2_b,
                "White\\newline 1st Stage" = summary(d1_w, stage = 1),
                "White" = d1_w,
                "White\\newline Controls\\newline 1st Stage" = summary(d2_w, stage = 1),
                "White\\newline Controls" = d2_w),
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
    save_tt(., output = here("FigTab","IV_results_table_rdi.tex"), overwrite = T)

# ------------------------------- Estimate IV Models (Government) -------------------------------

m1_b = feols(death_age~1|byear + STATEFIP_b + urb_code |county_dism~ln_gov + gov_rev_share_state, data = db, vcov = ~death_fips)
m1_w = feols(death_age~1|byear + STATEFIP_b + urb_code |county_dism~ln_gov + gov_rev_share_state, data = dw, vcov = ~death_fips)
m2_b = feols(death_age~male + migrated + education + married + south|byear + STATEFIP_b + urb_code + OCC  |county_dism~ln_gov + gov_rev_share_state, data = db,vcov = ~death_fips)
m2_w = feols(death_age~male + migrated + education + married + south|byear + STATEFIP_b + urb_code + OCC  |county_dism~ln_gov + gov_rev_share_state, data = dw,vcov = ~death_fips)

msummary(list("Black\\newline 1st Stage" = summary(m1_b,stage = 1),
              "Black" = m1_b,
              "Black\\newline Controls\\newline 1st Stage" = summary(m2_b,stage = 1),
              "Black\\newline Controls" = m2_b,
              "White\\newline 1st Stage" = summary(m1_w,stage = 1),
              "White" = m1_w,
              "White\\newline Controls\\newline 1st Stage" = summary(m2_w,stage = 1),
              "White\\newline Controls" = m2_w),
         fmt =3, 
         stars = T,
         coef_map = c(
                      "fit_county_dism" = "D",
                      "male" = "Male",
                      "education" = "Education",
                      "migratedMigrated" = "Migrated",
                      "married" = "Married in 1940",
                      "south" = "Died in South",
                      "ln_gov" = "Log(Governments)",
                      "gov_rev_share_state" = "Revenue Share"),
         gof_map = c("nobs",
                     "r.squared",
                     "f"
                     ),
         align = "lcccccccc",
         notes = "This table describes the first-stage models and IV estimates of the effect of segregation on longevity. Heteroskedasiticty Robust Standard Errors in parentheses.",
         title = "Estimates of the Effect of Segregation on Longevity",
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
  save_tt(.,output = here("FigTab","IV_results_table.tex"), overwrite = T)

# ------------------------------- Estimate IV Models (Rivers) -------------------------------

r1_b = feols(death_age~1|byear + STATEFIP_b + urb_code |county_dism~n_named_rivers + n_named_rivers_sq + stream_km_per_km2 , data = db, vcov = "white")
r1_w = feols(death_age~1|byear + STATEFIP_b + urb_code |county_dism~n_named_rivers + n_named_rivers_sq + stream_km_per_km2 , data = dw, vcov = "white")
r2_b = feols(death_age~male + migrated + education + married + south|byear + STATEFIP_b + urb_code + OCC |county_dism~n_named_rivers + n_named_rivers_sq +  stream_km_per_km2, data = db, vcov ="white")
r2_w = feols(death_age~male + migrated + education + married + south|byear + STATEFIP_b + urb_code + OCC |county_dism~n_named_rivers + n_named_rivers_sq +  stream_km_per_km2, data = dw, vcov ="white")

msummary(list("Black\\newline 1st Stage" = summary(r1_b, stage = 1),
              "Black" = r1_b,
              "Black\\newline Controls\\newline 1st Stage" = summary(r2_b, stage = 1),
              "Black\\newline Controls" = r2_b,
              "White\\newline 1st Stage" = summary(r1_w, stage = 1),
              "White" = r1_w,
              "White\\newline Controls\\newline 1st Stage" = summary(r2_w, stage = 1),
              "White\\newline Controls" = r2_w),
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
  save_tt(., output = here("FigTab","IV_results_table_rivers.tex"), overwrite = T)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Sibling FE Robustness 

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Exact Matches
sib_m2_b = feols(death_age~county_dism + male + migrated + education + married |byear + STATEFIP_b  + urb_code +OCC + sib_group_id_exact, data = db_f,vcov = ~death_fips)
sib_m2_w = feols(death_age~county_dism + male + migrated + education + married |byear + STATEFIP_b  + urb_code +OCC + sib_group_id_exact, data = dw_f,vcov = ~death_fips)

# Flexible Matches
sib_2_m2_b = feols(death_age~county_dism + male + migrated + education + married |byear + STATEFIP_b  + urb_code +OCC + sib_group_id_flexible, data = db_f,vcov = ~death_fips)
sib_2_m2_w = feols(death_age~county_dism + male + migrated + education + married |byear + STATEFIP_b  + urb_code +OCC + sib_group_id_flexible, data = dw_f,vcov = ~death_fips)

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
  distinct(county_dism ,death_decade,ln_gov,gov_rev_share_state,death_fips) 

# ------------------------------- Binscatter regressions ------------------------------- 
b_share = binsreg(instrument$county_dism,instrument$ln_gov,at = "mean",w = instrument$gov_rev_share_state)
i_data = as.data.frame(b_share$data.plot)

bs = 
  ggplot(data = i_data,aes(Group.Full.Sample.data.dots.x,Group.Full.Sample.data.dots.fit)) + 
  geom_point(size =2) +
  geom_smooth(method = "lm",
              alpha = 0, 
              lwd = 2) + 
  labs(x = "Ln(Governments)",
       y = "County Dissimilarity",
       caption = str_wrap("This figure displays the association between government fragmentation and segregation.
                          Segregation is measured by the index if dissimilarity that measures evenly distributed Black and White residents are within a county. 
                          Dots represent means of bins at each level of the instrument. 
                          The blue line corresponds to the fitted OLS regression line of segregation on the instruments. 
                          Ln(Governments) refers to the log of the number of governments: the focal measure of government fragmentation.
                          F-statistic = 236.53.",100)) + 
  theme_cowplot() + 
  theme(plot.caption = element_text(hjust = 0)) 
# ------------------------------- Save -------------------------------
ggsave(bs,filename = here("FigTab","fs_plot.jpeg"),
       width = 10, 
       height = 5,
       dpi = 1000)

# ------------------------------- Create Plots -------------------------------
# Set Contrast for Plots
contrast = 10


# -------------------------------  OLS ------------------------------- 
ols_m1 = ols_m1_b %>% tidy(conf.int = T) %>% filter(term == "county_dism")  %>%  mutate(Model = "Unadjusted", Race = "Black")
ols_m2 = ols_m1_w %>% tidy(conf.int = T) %>% filter(term == "county_dism")  %>%  mutate(Model = "Unadjusted", Race = "White")
ols_m3 = ols_m2_b %>% tidy(conf.int = T) %>% filter(term == "county_dism")  %>% mutate(Model = "Controls", Race = "Black")
ols_m4 = ols_m2_w %>% tidy(conf.int = T) %>% filter(term == "county_dism")  %>% mutate(Model = "Controls", Race = "White")

### Visualize cross-sample OLS comparison ### 
ols_result = rbind(
  ols_m1,
  ols_m2 ,
  ols_m3 ,
  ols_m4                    
) %>% mutate(Model = factor(Model,levels = unique(Model)),
             estimate = estimate*contrast,
             conf.low = conf.low*contrast,
             conf.high = conf.high*contrast)

### Visualize Results ### 
ols_res_fig = 
  ggplot(ols_result,aes(Model,
                        estimate,
                        ymin = conf.low,
                        ymax = conf.high, 
                        color = Race)) + 
  geom_pointrange(lwd = 2,
                  size =.75,
                  position = position_dodge2(width = .1)) + 
  theme_cowplot() + 
  geom_hline(yintercept = 0, linetype = "dashed", lwd = 1) + 
  theme(legend.position = "top") + 
  labs(
    caption = str_wrap("This figure displays estimates of the OLS association between racial segregation and longevity. 
    The unadjusted model contains birth year, urban-rural, and birth state fixed effects. The model with controls adds covariates and occupation fixed effects.   
    Estimates refer to a 10 point increase in Dissimilarity.",100),
    x = "Model",
    y = "Change in Life Expectancy") + 
  scale_color_manual(values = c("darkgreen","darkblue"))  + 
    theme(plot.caption = element_text(hjust = 0),
          legend.position = "bottom") 
  
### Save Figure ### 
ggsave(ols_res_fig,
       filename = here("FigTab","ols_figure.jpeg"),
       width = 8, 
       height = 5,
       dpi = 1000)

### Plots of Main Effects
m1_b_plot =   tidy(m1_b,conf.int = T) %>% filter(term =="fit_county_dism") %>% mutate(Race = "Black", Model = "Unadjusted")
m1_w_plot =   tidy(m1_w,conf.int = T) %>% filter(term =="fit_county_dism") %>% mutate(Race = "White", Model = "Unadjusted")
m2_b_plot =   tidy(m2_b,conf.int = T) %>% filter(term =="fit_county_dism") %>% mutate(Race = "Black", Model = "Controls")
m2_w_plot =   tidy(m2_w,conf.int = T) %>% filter(term =="fit_county_dism") %>% mutate(Race = "White", Model = "Controls")

iv_plot = rbind(m1_b_plot,
                m1_w_plot,
                m2_b_plot,
                m2_w_plot)

# ------------------------------- IV ------------------------------- 
Iv_estimate_plot = 
iv_plot %>% 
  mutate(
    # scale by contrast 
    estimate = estimate*contrast,
    conf.high = conf.high*contrast,
    conf.low = conf.low*contrast,
    Model = factor(Model, levels = unique(Model))
  ) %>% 
  ggplot(aes(Model,estimate,
             ymin = conf.low,
             ymax = conf.high, 
             color = Race)) + 
  geom_pointrange(position = position_dodge2(width =.1),
                  size = .75,
                  lwd = 2) + 
  labs(y = "Change in Life Expectancy",
       caption = str_wrap("This figure displays estimates from two-stage least squares regressions of the effect of racial segregation on longevity. 
        The unadjusted model contains birth year, urban-rural, and birth state fixed effects. The model with controls adds covariates and occupation fixed effects.
    Estimates refer to a 10 point increase in Dissimilarity.",100)
  ) + 
  scale_color_manual(values = c("darkgreen","darkblue")) + 
  theme_cowplot() + 
  geom_hline(yintercept = 0,linetype = "dashed",lwd = 1)    + 
  theme(plot.caption = element_text(hjust = 0),
        legend.position = "bottom") 

ggsave(Iv_estimate_plot,filename = here("FigTab","iv_plot.jpeg"),
       width = 8, 
       height = 5,
       dpi = 1000)

# ------------------------------- All Estimates Plot -------------------------------

all_estimates_plot_data = bind_rows(
  # Gov IV
 # tidy(m1_b, conf.int = T) |> filter(term == "fit_county_dism") |> mutate(Race = "Black", Model = "Unadjusted", Estimator = "Gov. IV"),
 # tidy(m1_w, conf.int = T) |> filter(term == "fit_county_dism") |> mutate(Race = "White", Model = "Unadjusted", Estimator = "Gov. IV"),
  tidy(m2_b, conf.int = T) |> filter(term == "fit_county_dism") |> mutate(Race = "Black", Model = "Controls",    Estimator = "Gov. IV"),
  tidy(m2_w, conf.int = T) |> filter(term == "fit_county_dism") |> mutate(Race = "White", Model = "Controls",    Estimator = "Gov. IV"),
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
    Estimator = factor(Estimator, levels = c("RDI IV","Gov. IV","Rivers IV","Sib. FE (Exact)","Sib. FE (Flexible)"))
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
       IV models use government fragmentation (Gov. IV), named rivers (Rivers IV), and the Railroad Diversity Index (RDI IV) as instruments.
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

# ------------------------------- Gov. IV -------------------------------
w_ed_c = feols(death_age~male + migrated + married + south + educ_cat |byear + STATEFIP_b +urb_code + OCC | county_dism~ ln_gov + gov_rev_share_state, data = dw, vcov = ~death_fips)
b_ed_c = feols(death_age~male + migrated + married + south + educ_cat |byear + STATEFIP_b +urb_code + OCC | county_dism~ ln_gov + gov_rev_share_state, data = db, vcov = ~death_fips)
w_ed   = feols(death_age~male + migrated + married + south |byear + STATEFIP_b +urb_code | county_dism*educ_cat~ ln_gov*educ_cat + gov_rev_share_state*educ_cat, data = dw, vcov = ~death_fips)
b_ed   = feols(death_age~male + migrated + married + south |byear + STATEFIP_b +urb_code | county_dism*educ_cat~ ln_gov*educ_cat + gov_rev_share_state*educ_cat, data = db, vcov = ~death_fips)

gov_ft_w <- ftest_vals(w_ed_c, w_ed)
gov_ft_b <- ftest_vals(b_ed_c, b_ed)

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
  extract_ed_slopes(w_ed,       dw,   "Gov. IV",            gov_ft_w$fstat,   gov_ft_w$pval),
  extract_ed_slopes(w_ed_rdi,   dw,   "RDI IV",             rdi_ft_w$fstat,   rdi_ft_w$pval),
  extract_ed_slopes(w_ed_riv,   dw,   "Rivers IV",          riv_ft_w$fstat,   riv_ft_w$pval),
  extract_ed_slopes(w_ed_sib_e, dw_f, "Sib. FE (Exact)",    sib_e_ft_w$fstat, sib_e_ft_w$pval),
  extract_ed_slopes(w_ed_sib_f, dw_f, "Sib. FE (Flexible)", sib_f_ft_w$fstat, sib_f_ft_w$pval)
) |>
  mutate(
    educ_cat = factor(educ_cat, levels = c("Less than HS","High School","Some College","College+")),
    strategy = factor(strategy, levels = c("Gov. IV","Rivers IV","RDI IV","Sib. FE (Exact)","Sib. FE (Flexible)"))
  )

education_black <- bind_rows(
  extract_ed_slopes(b_ed,       db,   "Gov. IV",            gov_ft_b$fstat,   gov_ft_b$pval),
  extract_ed_slopes(b_ed_rdi,   db,   "RDI IV",             rdi_ft_b$fstat,   rdi_ft_b$pval),
  extract_ed_slopes(b_ed_riv,   db,   "Rivers IV",          riv_ft_b$fstat,   riv_ft_b$pval),
  extract_ed_slopes(b_ed_sib_e, db_f, "Sib. FE (Exact)",    sib_e_ft_b$fstat, sib_e_ft_b$pval),
  extract_ed_slopes(b_ed_sib_f, db_f, "Sib. FE (Flexible)", sib_f_ft_b$fstat, sib_f_ft_b$pval)
) |>
  mutate(
    educ_cat = factor(educ_cat, levels = c("Less than HS","High School","Some College","College+")),
    strategy = factor(strategy, levels = c("Gov. IV","Rivers IV","RDI IV","Sib. FE (Exact)","Sib. FE (Flexible)"))
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
m2_b_m = feols(death_age~migrated + education + married + south|byear + STATEFIP_b + urb_code + OCC  |county_dism~ln_gov + gov_rev_share_state, data = subset(db,male ==1),vcov = "white")
m2_w_m = feols(death_age~migrated + education + married + south|byear + STATEFIP_b + urb_code + OCC  |county_dism~ln_gov + gov_rev_share_state, data = subset(dw,male ==1),vcov = "white")
m2_b_f = feols(death_age~migrated + education + married + south|byear + STATEFIP_b + urb_code + OCC  |county_dism~ln_gov + gov_rev_share_state, data = subset(db,male ==0),vcov = "white")
m2_w_f = feols(death_age~migrated + education + married + south|byear + STATEFIP_b + urb_code + OCC  |county_dism~ln_gov + gov_rev_share_state, data = subset(dw,male ==0),vcov = "white")

msummary(list("Black (Men) " = m2_b_m,
              "Black (Women)" = m2_b_f,
              "White (Men) " = m2_w_m,
              "White (Women)" = m2_w_f),fmt =3, stars = T,
         coef_map = c("fit_county_dism" = "D",
                      "education" = "Education",
                      "migratedMigrated" = "Migrated",
                      "married" = "Married in 1940",
                      "south" = "Died in South"),
         gof_map = c("nobs",
                     "r.squared"
                     
         ),
         notes = "This table describes IV estimates of the effect of segregation on longevity by gender. 
         First-stage regressions are supppressed for concision. Heteroskedasiticty Robust Standard Errors in parentheses.",
         title = "Estimates of the Effect of Segregation on Longevity (By Gender)",
         add_rows = data.frame(`FE` = c("Birth Year","Birth State","Urban Rural Code","OCC"),
                               m1 = c("Yes","Yes","Yes","Yes"),
                               m2 = c("Yes","Yes","Yes","Yes"),
                               m3 = c("Yes","Yes","Yes","Yes"),
                               m4 = c("Yes","Yes","Yes","Yes")),
         output = "tinytable") %>% 
  save_tt(.,output = "./FigTab/IV_results_table_gender.tex", overwrite = T)


## Plot by Gender
m_b_m_plot =   tidy(m2_b_m,conf.int = T) %>% filter(term =="fit_county_dism") %>% mutate(Race = "Black", Model = "Men")
m_b_w_plot =   tidy(m2_b_f,conf.int = T) %>% filter(term =="fit_county_dism") %>% mutate(Race = "Black", Model = "Women")
m_w_m_plot =   tidy(m2_w_m,conf.int = T) %>% filter(term =="fit_county_dism") %>% mutate(Race = "White", Model = "Men")
m_w_w_plot =   tidy(m2_w_f,conf.int = T) %>% filter(term =="fit_county_dism") %>% mutate(Race = "White", Model = "Women")

iv_plot_g = rbind(m_b_m_plot,
                m_b_w_plot,
                m_w_m_plot,
                m_w_w_plot)

### Make Plot ### 
Iv_estimate_plot_g = 
  iv_plot_g %>% 
  mutate(
    # scale by contrast 
    estimate = estimate*contrast,
    conf.high = conf.high*contrast,
    conf.low = conf.low*contrast,
    Model = factor(Model, levels = unique(Model))
  ) %>% 
  ggplot(aes(Model,estimate,
             ymin = conf.low,
             ymax = conf.high, 
             color = Race)) + 
  geom_pointrange(position = position_dodge2(width =.1),
                  size = .75,
                  lwd = 2) + 
  labs(y = "Change in Years of Life",
       caption = "This figure displays estimates from two-stage least squares regressions of the effect of racial segregation on longevity by gender. 
       Estimates from models with all controls and fixed effects are presented.
    Estimates refer to a 10 point increase in Dissimilarity."
  ) + 
  scale_color_manual(values = c("darkgreen","darkblue")) + 
  theme_cowplot() + 
  geom_hline(yintercept = 0,linetype = "dashed",lwd = 1)    + 
  theme(plot.caption = element_text(hjust = 0),
        legend.position = "bottom") 

ggsave(Iv_estimate_plot_g,filename = here("FigTab","iv_plot_gender.jpeg"),
       width = 8, 
       height = 5,
       dpi = 1000)


#------------------------------- Weights -------------------------------  

m1_b_w = feols(death_age~1|byear + STATEFIP_b + urb_code |county_dism~ln_gov + gov_rev_share_state, data = db,vcov = "white",weights = db$weight)
m1_w_w = feols(death_age~1|byear + STATEFIP_b + urb_code |county_dism~ln_gov + gov_rev_share_state, data = dw,vcov = "white",weights = dw$weight)
m2_b_w = feols(death_age~male + migrated + education + married + south|byear + STATEFIP_b + urb_code + OCC  |county_dism~ln_gov + gov_rev_share_state, data = db,vcov = "white",weights = db$weight)
m2_w_w = feols(death_age~male + migrated + education + married + south|byear + STATEFIP_b + urb_code + OCC  |county_dism~ln_gov + gov_rev_share_state, data = dw,vcov = "white",weights = dw$weight)

msummary(list("Black" = m1_b_w,
              "Black\\newline Controls" = m2_b_w,
              "White" = m1_w_w,
              "White\\newline Controls" = m2_w_w),fmt =3, stars = T,
         coef_map = c("fit_county_dism" = "D",
                      "male" = "Male",
                      "education" = "Education",
                      "migratedMigrated" = "Migrated",
                      "married" = "Married in 1940",
                      "south" = "Died in South"),
         gof_map = c("nobs",
                     "r.squared"
                     
         ),
         notes = "This table describes IV estimates of the effect of segregation on longevity using post-stratification weights. 
         First-stage regressions are supppressed for concision. Heteroskedasiticty Robust Standard Errors in parentheses.",
         title = "Estimates of the Effect of Segregation on Longevity (Weights)",
         add_rows = data.frame(`FE` = c("Birth Year","Birth State","Urban Rural Code","OCC"),
                               m1 = c("Yes","Yes","Yes","No"),
                               m2 = c("Yes","Yes","Yes","Yes"),
                               m4 = c("Yes","Yes","Yes","No"),
                               m5 = c("Yes","Yes","Yes","Yes")),
         output = "tinytable") %>% 
  save_tt(.,output = here("FigTab","IV_results_weights_table.tex"), overwrite = T) 



# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Alternative Measures of D 

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

H_b = feols(death_age~male + migrated + education + married + south|byear + STATEFIP_b + urb_code + OCC  |H_bw~ln_gov + gov_rev_share_state, data = db,vcov = "white")
H_w = feols(death_age~male + migrated + education + married + south|byear + STATEFIP_b + urb_code + OCC  |H_bw~ln_gov + gov_rev_share_state, data = dw,vcov = "white")
I_b = feols(death_age~male + migrated + education + married + south|byear + STATEFIP_b + urb_code + OCC  |county_isolb~ln_gov + gov_rev_share_state, data = db,vcov = "white")
I_w = feols(death_age~male + migrated + education + married + south|byear + STATEFIP_b + urb_code + OCC  |county_isolb~ln_gov + gov_rev_share_state, data = dw,vcov = "white")
FD_w = feols(death_age~male + migrated + education + married + south|byear + STATEFIP_b + urb_code + OCC  |D_star~ln_gov + gov_rev_share_state, data = dw,vcov = "white")
FD_b = feols(death_age~male + migrated + education + married + south|byear + STATEFIP_b + urb_code + OCC  |D_star~ln_gov + gov_rev_share_state, data = db,vcov = "white")

msummary(list("Black (D)" = m2_b,
              "Black (H)" = H_b,
              "Black (I)" = I_b,
              "Black (D*)" = FD_b,
              "White (D)" = m2_w,
              "White (H)" = H_w,
              "White (I)" = I_w,
              "White (D*)" = FD_w),fmt =3, stars = T,
         coef_map = c("fit_county_dism" = "D",
                      "fit_H_bw" = "H",
                      "fit_county_isolb" = "I",
                      "fit_D_star" = "D-Adjusted"),
         gof_map = c("nobs",
                     "r.squared"
                     
         ),
         notes = "This table describes IV estimates of the effect of segregation on longevity for alternative measures of D. 
         First-stage regressions are supppressed for concision.Heteroskedasiticty Robust Standard Errors in parentheses. 
         Models adjust for all covariates and FEs used in main analyses but are not shown in the model.",
         title = "Estimates of the Effect of Segregation on Longevity (Alternative Measures)",
         add_rows = data.frame(
           FE = c("Birth Year","Birth State","Urban-Rural Code","Occupation"),
           m1 = c("X","X","X","X"),
           m2 = c("X","X","X","X"),
           m3 = c("X","X","X","X"),
           m4 = c("X","X","X","X"),
           m5 = c("X","X","X","X"),
           m6 = c("X","X","X","X"),
           m7 = c("X","X","X","X"),
           m8 = c("X","X","X","X")
         ))  %>%
  save_tt(.,output = here("FigTab","IV_results_table_alt_measure.tex"), overwrite = T)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Monotonicity Descriptive
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

data_m = data_a %>% distinct(ln_gov, gov_rev_share_state,death_fips,death_decade,county_dism) 

mon_reg_1 = lm(county_dism~ln_gov +gov_rev_share_state,subset(data_m, death_decade == 1980))
mon_reg_2 = lm(county_dism~ln_gov +gov_rev_share_state,subset(data_m, death_decade == 1990))
mon_reg_3 = lm(county_dism~ln_gov +gov_rev_share_state,subset(data_m, death_decade == 2000))

mon_reg_1_r2 = summary(mon_reg_1)$r.squared[1]
mon_reg_2_r2 = summary(mon_reg_2)$r.squared[1]
mon_reg_3_r2 = summary(mon_reg_3)$r.squared[1]

data.frame(Decade = c(1980,1990,2000),
           R2 = c(mon_reg_1_r2,mon_reg_2_r2,mon_reg_3_r2))
# R2 Declines slightly over time
