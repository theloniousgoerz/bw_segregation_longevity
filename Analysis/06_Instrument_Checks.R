# # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# supplemental checks 
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

# Source: EPA county-level annual PM2.5, 1990-2010, 2132 counties (FIPS-coded)
# https://catalog.data.gov/dataset/annual-pm2-5-and-cardiovascular-mortality-rate-data-trends-modified-by-county-socioeconomi
# No PM2.5 monitoring/reconstruction exists at the county level before 1990, so the
# 1980 death_decade has no match here and is left NA.
pm25_decade = read_csv(here("Data","raw","pm25_epa","County_annual_PM25_CMR.csv")) %>%
  transmute(
    death_fips = str_pad(as.character(FIPS), 5, pad = "0"),
    death_decade = case_when(
      Year %in% 1990:1999 ~ 1990,
      Year %in% 2000:2010 ~ 2000
    ),
    PM2.5
  ) %>%
  filter(!is.na(death_decade)) %>%
  group_by(death_fips, death_decade) %>%
  summarise(pm25 = mean(PM2.5, na.rm = TRUE), .groups = "drop")


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Establish First-Stage Relationship 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# -------------------------------First-stage -------------------------------

instrument = data_a %>% 
  distinct(county_dism ,death_decade,
           ln_gov,gov_rev_share_state,
           rdi,rail_km_per_km2,
           n_named_rivers,n_named_rivers_sq,
           stream_km_per_km2,death_fips,
           south) 

# ------------------------------- Make Table -------------------------------
f_gov  =    feols(county_dism~ln_gov + gov_rev_share_state | death_decade, data = instrument, vcov = ~death_fips) 
f_rivers  = feols(county_dism~n_named_rivers + stream_per_km_sq | death_decade, data = instrument, vcov = ~death_fips) 
f_rdi  =    feols(county_dism~rdi | death_decade, data = instrument, vcov = ~death_fips) 

f_table = msummary(list(f_gov,f_rivers,f_rdi),stars = T,
                   gof_map = c("nobs","f"),
                   # coef_map = c("ln_gov" = "Ln(Number Governments)",
                   #              "gov_rev_share_state" = "County Share Revenue from Federal Gov."),
                   add_rows = data.frame(
                     term = "First-Stage F",
                     `(1)` = unlist(fitstat(f_gov, type = "f"))[1],
                     `(2)` = unlist(fitstat(f_rivers, type = "f"))[1],
                     `(3)` = unlist(fitstat(f_rdi, type = "f"))[1],
                     check.names = FALSE),
                   notes = "First stage relationship includes death decade fixed effects and heteroskedacticity SEs.",
                   title = "First Stage Regression of D on Instruments",
                   #align = "lc",
                   threeparttable = T, 
                   fmt = 2,   
                   output = "tinytable")
# ------------------------------- Save -------------------------------
save_tt(f_table,output = here("FigTab","f_table.tex"), overwrite = T)


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# How correlated are these instruments? 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
instrument_distinct = instrument %>% distinct(death_fips,ln_gov,n_named_rivers,rdi)

rdi_gov_corr = feols(rdi~ln_gov,data = instrument_distinct, vcov = "white")
rdi_rivers_corr = feols(rdi~n_named_rivers,data = instrument_distinct, vcov = "white")
gpv_rivers_corr = feols(ln_gov~n_named_rivers,data = instrument_distinct, vcov = "white")

msummary(
  list(
    "RDI-Gov" = rdi_gov_corr,
    "RDI-Rivers" = rdi_rivers_corr,
    "Gov-Rivers" = gpv_rivers_corr
  ),
  coef_map = c(
    "ln_gov" = "Ln(Gov)",
    "n_named_rivers" = "N Rivers"
  ),
  align = "lccc",
  gof_map = c("nobs","r.squared"),
  notes = "This table displays coefficients and R-squared statistics for bivariate models of the relationships between instruments.
  Because the instruments are constant, these regressions are run on a each of their distinct values rather than the full sample of county-year observations. 
  All models include robust standard errors.",
  stars = T,
  threeparttable = T, 
  output = "tinytable"
) %>% 
  save_tt(here("FigTab","instrument_correlation_table.tex"),overwrite = T)

corr_mat = instrument_distinct %>% select(-death_fips) %>%
  filter(
    !is.na(ln_gov) &
    !is.na(n_named_rivers) &
    !is.na(rdi)) %>%
  cor()

corr_mat[upper.tri(corr_mat, diag = TRUE)] = NA

#corr_plot = 
corr_mat %>%
  data.frame() %>%
  mutate(Var1 = factor(rownames(.), levels = rownames(corr_mat))) %>%
  pivot_longer(-Var1, names_to = "Var2", values_to = "r") %>%
  mutate(Var2 = factor(Var2, levels = colnames(corr_mat))) %>%
  filter(!is.na(r)) %>%
  ggplot(aes(x = Var2, y = Var1,fill = r)) +
  geom_tile() + 
  geom_label(aes(label = round(r, 2)), size = 6, fill = "white") +
  scale_fill_gradient2(low = "white", mid = "gray", high = "black",
                        midpoint = 0, limits = c(-1, 1), name = "Correlation") +
  labs(x = NULL, y = NULL) +
  theme_cowplot() + 
  coord_flip() + 
  theme(legend.position = "top")

ggsave(corr_plot, filename = here("FigTab", "instrument_corr_plot.jpeg"),
       width = 7, height = 6, dpi = 300)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# County-Level PM2.5 (1990/2000 death decades only)
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

instrument %<>% left_join(pm25_decade, by = c("death_fips","death_decade"))

instrument %>%
  group_by(death_decade) %>%
  summarise(match_rate = mean(!is.na(pm25)), n = n())

rdi_pollute = feols(pm25~rdi | death_decade, data = instrument, vcov = ~death_fips)

msummary(list("pm2.5/m3" = rdi_pollute),
         coef_map = c("rdi" = "RDI"),
         stars = T,
         gof_map = c("nobs","r.squared"),
         notes = "Models include decade fixed effects and adjust for track length. Modles use cluster-robust standard errors. ",
         title = "Association between county-level PM2.5 concentration (1990,2000)",
         output = "tinytable") %>% 
  save_tt(here("FigTab","pm25_rdi_table.tex"))

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# RDI ~ Education / Log(INCWAGE): reduced-form associations, sans segregation
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Same controls/FE as the d2_b/d2_w main specifications, but with county_dism
# (segregation) dropped and rdi entered directly as a regressor rather than as
# an instrument. "migrated" is omitted from the non-migrant subsample models
# since it is constant (and therefore collinear) once the sample is restricted.

db %<>% mutate(log_incwage = ifelse(INCWAGE %in% c(999998, 999999) | INCWAGE <= 0, NA, log(INCWAGE)))
dw %<>% mutate(log_incwage = ifelse(INCWAGE %in% c(999998, 999999) | INCWAGE <= 0, NA, log(INCWAGE)))

db_stay = db %>% filter(migrated != "Migrated")
dw_stay = dw %>% filter(migrated != "Migrated")

# ------------------------------- Education -------------------------------
educ_b       = feols(education~male + migrated + married + south + rdi + rail_km_per_km2 | byear + STATEFIP_b + urb_code + OCC, data = db,      vcov = ~death_fips)
educ_w       = feols(education~male + migrated + married + south + rdi + rail_km_per_km2 | byear + STATEFIP_b + urb_code + OCC, data = dw,      vcov = ~death_fips)
educ_b_stay  = feols(education~male +            married + south + rdi + rail_km_per_km2 | byear + STATEFIP_b + urb_code + OCC, data = db_stay, vcov = ~death_fips)
educ_w_stay  = feols(education~male +            married + south + rdi + rail_km_per_km2 | byear + STATEFIP_b + urb_code + OCC, data = dw_stay, vcov = ~death_fips)

educ_table = msummary(
  list("Black (Full)" = educ_b, "White (Full)" = educ_w,
       "Black (Non-Migrant)" = educ_b_stay, "White (Non-Migrant)" = educ_w_stay),
  coef_map = c("rdi" = "RDI"),
  stars = T,
  gof_map = c("nobs","r.squared"),
  notes = "Outcome is years of education. Controls: male, migrated (full sample only), married, south. Fixed effects: birth year, birth state, urbanicity, occupation. Standard errors clustered on death county.",
  title = "Association between RDI and Education, by Race and Migration Status",
  threeparttable = T,
  fmt = 2,
  output = "tinytable")
save_tt(educ_table, output = here("FigTab","rdi_education_exclusion_table.tex"), overwrite = T)

# ------------------------------- Log(INCWAGE) -------------------------------
inc_b       = feols(log_incwage~male + migrated + education + married + south + rdi + rail_km_per_km2 | byear + STATEFIP_b + urb_code + OCC, data = db,      vcov = ~death_fips)
inc_w       = feols(log_incwage~male + migrated + education + married + south + rdi + rail_km_per_km2 | byear + STATEFIP_b + urb_code + OCC, data = dw,      vcov = ~death_fips)
inc_b_stay  = feols(log_incwage~male +            education + married + south + rdi + rail_km_per_km2 | byear + STATEFIP_b + urb_code + OCC, data = db_stay, vcov = ~death_fips)
inc_w_stay  = feols(log_incwage~male +            education + married + south + rdi + rail_km_per_km2 | byear + STATEFIP_b + urb_code + OCC, data = dw_stay, vcov = ~death_fips)

incwage_table = msummary(
  list("Black (Full)" = inc_b, "White (Full)" = inc_w,
       "Black (Non-Migrant)" = inc_b_stay, "White (Non-Migrant)" = inc_w_stay),
  coef_map = c("rdi" = "RDI"),
  stars = T,
  gof_map = c("nobs","r.squared"),
  notes = "Outcome is log(INCWAGE), restricted to positive, non-missing wage income. Controls: male, migrated (full sample only), education, married, south. Fixed effects: birth year, birth state, urbanicity, occupation. Standard errors clustered on death county.",
  title = "Association between RDI and Log(INCWAGE), by Race and Migration Status",
  threeparttable = T,
  fmt = 2,
  output = "tinytable")
save_tt(incwage_table, output = here("FigTab","rdi_incwage_table.tex"), overwrite = T)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Test Lal and Collegues IV tests?
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%






