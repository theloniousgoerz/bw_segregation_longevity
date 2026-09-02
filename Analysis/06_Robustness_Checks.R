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
source(here("Analysis","00_Helpers.R"))

data_a =   read_csv(here("Data","_Cleaned","data_a.csv"))
db =       read_csv(here("Data","_Cleaned","db.csv"))
dw =       read_csv(here("Data","_Cleaned","dw.csv"))
db_f=      read_csv(here("Data","_Cleaned","db_f.csv"))
dw_f=      read_csv(here("Data","_Cleaned","dw_f.csv"))

# Repair birth_fips and recompute migration status from the FIPS codes. The stored
# `migrated` has its two labels swapped, so the non-migrant subsamples built below were
# previously the migrants; STATEFIP_b was wrong for the rows whose birth_fips lost or
# gained a leading zero. See prepare_analysis_data() in 00_Helpers.R, which also drops
# anyone whose birth county is unrecorded.
data_a %<>% prepare_analysis_data("data_a")
db     %<>% prepare_analysis_data("db")
dw     %<>% prepare_analysis_data("dw")
db_f   %<>% prepare_analysis_data("db_f")
dw_f   %<>% prepare_analysis_data("dw_f")

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
           n_named_rivers,
           stream_km_per_km2,death_fips,
           south) 

# ------------------------------- Make Table -------------------------------
# `stream_per_km_sq` does not exist in the data; the stream density variable is
# `stream_km_per_km2`, which is what the rivers instrument set uses everywhere else.
f_rivers  = feols(county_dism~n_named_rivers + stream_km_per_km2 | death_decade, data = instrument, vcov = ~death_fips)
f_rdi  =    feols(county_dism~rdi + rail_km_per_km2 | death_decade, data = instrument, vcov = ~death_fips)

f_table = msummary(list(f_rivers,f_rdi),stars = T,
                   gof_map = c("nobs","f"),
                   coef_map = c("n_named_rivers" = "Number of Named Rivers",
                                "stream_per_km_sq" = "Stream km per km$^2$",
                                "stream_km_per_km2" = "Stream km per km$^2$",
                                "rdi" = "RDI",
                                "rail_km_per_km2" = "Rail km per km$^2$"),
                   add_rows = data.frame(
                     term = "First-Stage F",
                     `(2)` = unlist(fitstat(f_rivers, type = "f"))[1],
                     `(3)` = unlist(fitstat(f_rdi, type = "f"))[1],
                     check.names = FALSE),
                   notes = "First stage relationship includes death decade fixed effects and cluster-robust standard errors (by county at death).",
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
# The two instrument sets used in the paper are the RDI and the rivers measures, so this
# table reports that one pairwise association only. `ln_gov` is not an instrument in any
# model, and including it also dropped the sample to the 1,760 counties with government
# finance data; leaving it out keeps all 2,955 counties with both instruments.
instrument_distinct = instrument %>% distinct(death_fips,n_named_rivers,rdi)

rdi_rivers_corr = feols(rdi~n_named_rivers,data = instrument_distinct, vcov = "white")

msummary(
  list(
    "RDI-Rivers" = rdi_rivers_corr
  ),
  coef_map = c(
    "n_named_rivers" = "N Rivers"
  ),
  align = "lc",
  # The coefficient is on the order of 1e-4, so the default 3 digits prints it as 0.000.
  fmt = 5,
  gof_map = c("nobs","r.squared"),
  notes = "This table displays the coefficient and R-squared statistic from a bivariate model of the relationship between the two instruments.
  Because the instruments are time-constant, this regression is run on their distinct county values rather than the full sample of county-year observations.
  The model includes robust standard errors.",
  title = "Correlation Between the RDI and Rivers Instruments",
  stars = T,
  threeparttable = T,
  output = "tinytable"
) %>%
  save_tt(here("FigTab","instrument_correlation_table.tex"),overwrite = T)

# Built from `instrument` rather than `instrument_distinct`, which no longer carries
# `ln_gov`. This figure is not referenced in the draft; it is kept as a diagnostic.
corr_mat = instrument %>%
  distinct(death_fips, ln_gov, n_named_rivers, rdi) %>%
  select(-death_fips) %>%
  filter(
    !is.na(ln_gov) &
    !is.na(n_named_rivers) &
    !is.na(rdi)) %>%
  cor()

corr_mat[upper.tri(corr_mat, diag = TRUE)] = NA

corr_plot = 
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
         notes = "Models include decade fixed effects and adjust for track length. Models use cluster-robust standard errors. ",
         title = "Association between county-level PM2.5 concentration (1990,2000)",
         output = "tinytable") %>% 
  save_tt(here("FigTab","pm25_rdi_table.tex"), overwrite = T)

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
mon_riv_1 = lm(county_dism~n_named_rivers + stream_km_per_km2, subset(data_m, death_decade == 1980))
mon_riv_2 = lm(county_dism~n_named_rivers + stream_km_per_km2, subset(data_m, death_decade == 1990))
mon_riv_3 = lm(county_dism~n_named_rivers + stream_km_per_km2, subset(data_m, death_decade == 2000))

mon_r2 <- data.frame(
  Decade    = c(1980, 1990, 2000),
  RDI_R2    = c(summary(mon_rdi_1)$r.squared[1],
                summary(mon_rdi_2)$r.squared[1],
                summary(mon_rdi_3)$r.squared[1]),
  Rivers_R2 = c(summary(mon_riv_1)$r.squared[1],
                summary(mon_riv_2)$r.squared[1],
                summary(mon_riv_3)$r.squared[1])
)

mono_plot <- mon_r2 %>%
  pivot_longer(cols = c(RDI_R2, Rivers_R2), names_to = "Instrument", values_to = "R2") %>%
  ggplot(aes(x = Decade, y = R2, color = Instrument, shape = Instrument)) +
  geom_line() +
  geom_point(size = 3) +
  scale_color_manual(values = c("RDI_R2" = "black", "Rivers_R2" = "grey70")) +
  theme_classic() +
  theme(legend.position = "top")

ggsave(mono_plot, filename = here("FigTab", "monotonicity_gifure.jpeg"),
       width = 9, height = 7, dpi = 300)


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Results for children of different cohorts

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

## 5. Figure: colored by race, shaped by sex. Estimates are rescaled to a 10-point
##    rise in D (see the mutate below), so the axis is labelled accordingly.
bcohort_plot_data <- res %>%
  mutate(estimate = estimate * 10,
         lo       = lo * 10,
         hi       = hi * 10)

cache_estimate_range("bcohort", bcohort_plot_data$lo, bcohort_plot_data$hi)

bcohort_plot <-
  bcohort_plot_data %>%
  ggplot(aes(bin, estimate,
             color = sample,
             shape = sex)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_pointrange(aes(ymin = lo, ymax = hi),
                  position = position_dodge(width = 0.35),
                  linewidth = 0.7, fatten = 2.5,
                  size = 1) +
  coord_cartesian(ylim = shared_axis_range(MAIN_ESTIMATE_FIGS)) +
  labs(
    x = "Birth-year bin",
    y = "Years of Life (per 10-point rise in D)",
    color = "Group",
    subtitle = "RDI IV estimates, stratified by five-year birth cohort",
    caption = str_wrap(
      "Note: Estimates are two-stage least squares coefficients using the Railroad Division Index (RDI)
      instrument set (RDI and railroad track density); the Rivers instruments are not used here. Each point
      is a separate model fit within a race, sex, and five-year birth-year bin, expressed as years of life
      per 10-point rise in dissimilarity (D). All models include controls for migration, education, marital
      status, and death in the South, and fixed effects for birth year, birth state, urbanicity, and
      occupation. Bars are 95% confidence intervals from standard errors clustered on county of death.", 44)
  ) +
  theme_cowplot() +
  # Sized so a 9.5pt note survives the ~0.36x shrink from this plot's 9in ggsave
  # width down to its half-\textwidth (3.25in) display size on the printed page.
  theme(legend.position = "top",
        plot.caption = element_text(hjust = 0, size = 26, face = "italic")) +
  scale_color_manual(values = c("Black" = "#1b7837", "White" = "#2166ac"))

ggsave(bcohort_plot, filename = here("FigTab", "bcohort_stratified_estimates.jpeg"),
       # Taller than the panel alone needs, so the larger caption text (set for print
       # legibility at this figure's half-\textwidth display size) has room below the
       # panel instead of compressing it.
       width = 9, height = 12, dpi = 300)


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Robustness: Military Service Split (WWII Army Enlistment, Dataverse 4)
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Re-estimate the headline specifications (OLS, RDI IV, Rivers IV) with segregation (D)
# INTERACTED with military service, rather than as stratified served/not-served samples.
# Each estimator is fit once per race with `county_dism`, `served`, and their interaction;
# the interaction term tests directly whether the segregation-longevity slope differs for
# men who served vs. men who did not. Servers are identified by merging the CenSoc WWII
# Army enlistment records on HISTID.
#
# Framing (men only): WWII Army enlistment is ~all-male, so involving women would confound
# service with sex. We restrict to men (male == 1, per the est_bin() convention above) and
# drop `male` from the controls since it is then constant/collinear (mirroring how
# `migrated` is dropped in the non-migrant subsamples earlier in this script).
#
# IV note: interacting the endogenous D with `served` creates a SECOND endogenous term,
# `cd_served = county_dism * served`. We instrument both D and cd_served, using the base
# instruments AND each instrument interacted with `served` (which is exogenous). Both
# first stages are strong (see fitstat "ivf" / the "f" GOF row), so the interaction IV is
# well identified -- the earlier degeneracy was a small-subsample artifact of splitting,
# which the interaction avoids by using the full sample.
#
# Caveats:
#  * `served` has one-directional measurement error: a man who enlisted but whose record
#    was not ABE-linked to the 1940 Census is a false negative, so "Not Served" reads as
#    "not observed to have served." (False positives are unlikely.)
#  * Enlistment eligibility varies across cohorts: men born 1905-1920 were ~21-40 at U.S.
#    WWII entry (1941), so older cohorts had lower enlistment probability.

# ------------------------------- Build the served flag -------------------------------
# We use the enlistment file linked to the 1940 Census because it shares our sample's
# ABE-to-1940-Census linkage universe and carries HISTID for the broadest set of
# enlistees (2.57M). The Numident-linked enlistment file (1.69M) would under-count
# servers, so it is not used for the flag.
enlist_histids <- read_csv(
  here("Data", "dataverse_files(4)", "censoc_enlistment_census_1940_v1.1.csv"),
  col_select = "HISTID"
)$HISTID %>% unique()

# Restrict to men, flag service, and pre-build the endogenous interaction (cd_served) and
# the instrument x service products used to instrument it. Building these as explicit
# columns keeps the fixest IV formula unambiguous.
add_served <- function(d) {
  d %>%
    filter(male == 1) %>%
    mutate(
      served      = as.integer(HISTID %in% enlist_histids),
      cd_served   = county_dism * served,           # endogenous interaction (D x Served)
      rdi_served  = rdi * served,                   # RDI instruments x service
      rail_served = rail_km_per_km2 * served,
      nr_served   = n_named_rivers * served,         # Rivers instruments x service
      strm_served = stream_km_per_km2 * served
    )
}

db_m <- add_served(db); dw_m <- add_served(dw)

# Service counts per sample (sanity check before estimating).
print(count(db_m, served)); print(count(dw_m, served))

# ------------------------------- Estimator functions (interaction) -------------------------------
# Full-controls (headline) variant of each estimator, with `male` dropped. The IV models
# instrument both endogenous terms (county_dism, cd_served) with the base instruments and
# their service interactions.
fit_ols_int <- function(d) {
  feols(death_age ~ county_dism * served + migrated + education + married |
          byear + STATEFIP_b + urb_code + OCC,
        data = d, vcov = ~death_fips)
}
fit_rdi_int <- function(d) {
  feols(death_age ~ served + migrated + education + married + south |
          byear + STATEFIP_b + urb_code + OCC |
          county_dism + cd_served ~ rdi + rail_km_per_km2 + rdi_served + rail_served,
        data = d, vcov = ~death_fips)
}
fit_rivers_int <- function(d) {
  feols(death_age ~ served + migrated + education + married + south |
          byear + STATEFIP_b + urb_code + OCC |
          county_dism + cd_served ~ n_named_rivers  + stream_km_per_km2 +
            nr_served +  strm_served,
        data = d, vcov = ~death_fips)
}

# Fit each estimator once per race.
ols_b <- fit_ols_int(db_m); ols_w <- fit_ols_int(dw_m)
rdi_b <- fit_rdi_int(db_m); rdi_w <- fit_rdi_int(dw_m)
riv_b <- fit_rivers_int(db_m); riv_w <- fit_rivers_int(dw_m)

# ------------------------------- Tables (one per estimator) -------------------------------
service_note <- "Men only. `served` = 1 if the person's HISTID matches the CenSoc WWII Army enlistment-to-1940-Census file (else 'not observed to have served'). Each model interacts segregation (D) with service: 'D' is the slope for non-servers, 'D x Served' the difference for servers, 'Served' the main service effect. Controls: migrated, education, married (and 'Died in South' for the IV models); `male` is dropped (constant within men). Fixed effects: birth year, birth state, urbanicity, occupation."

# OLS
msummary(
  list("Black" = ols_b, "White" = ols_w),
  fmt = 3, stars = TRUE,
  coef_map = c("county_dism" = "D", "county_dism:served" = "D x Served",
               "served" = "Served (=1)", "education" = "Education",
               "migratedMigrated" = "Migrated", "married" = "Married in 1940"),
  gof_map = c("nobs", "r.squared"),
  notes = service_note,
  title = "OLS: Segregation x Military Service Interaction (Men)",
  threeparttable = TRUE, output = "tinytable"
) %>%
  save_tt(here("FigTab", "military_service_ols_table.tex"), overwrite = TRUE)

# RDI IV
msummary(
  list("Black" = rdi_b, "White" = rdi_w),
  fmt = 3, stars = TRUE,
  coef_map = c("fit_county_dism" = "D", "fit_cd_served" = "D x Served",
               "served" = "Served (=1)", "education" = "Education",
               "migratedMigrated" = "Migrated", "married" = "Married in 1940",
               "south" = "Died in South"),
  gof_map = c("nobs", "r.squared", "f"),
  notes = paste(service_note, "IV instruments D and D x Served with the Railroad Division Index and track density, each also interacted with service."),
  title = "RDI IV: Segregation x Military Service Interaction (Men)",
  threeparttable = TRUE, output = "tinytable"
) %>%
  save_tt(here("FigTab", "military_service_rdi_table.tex"), overwrite = TRUE)

# Rivers IV
msummary(
  list("Black" = riv_b, "White" = riv_w),
  fmt = 3, stars = TRUE,
  coef_map = c("fit_county_dism" = "D", "fit_cd_served" = "D x Served",
               "served" = "Served (=1)", "education" = "Education",
               "migratedMigrated" = "Migrated", "married" = "Married in 1940",
               "south" = "Died in South"),
  gof_map = c("nobs", "r.squared", "f"),
  notes = paste(service_note, "IV instruments D and D x Served with the number of named rivers (and its square) and stream density, each also interacted with service."),
  title = "Rivers IV: Segregation x Military Service Interaction (Men)",
  threeparttable = TRUE, output = "tinytable"
) %>%
  save_tt(here("FigTab", "military_service_rivers_table.tex"), overwrite = TRUE)

# ------------------------------- Combined coefficient figure -------------------------------
# From each interaction model, derive the IMPLIED segregation slope for non-servers (the
# base D coefficient) and for servers (base + interaction), with the servers' SE from a
# linear combination of the model VCOV. IV models expose the terms as `fit_county_dism`
# and `fit_cd_served`; OLS as `county_dism` and `county_dism:served`.
implied_slopes <- function(m, base, inter, estimator, race) {
  b <- coef(m); V <- vcov(m)
  data.frame(
    estimator = estimator, race = race,
    served    = c("Not Served", "Served"),
    estimate  = c(b[[base]], b[[base]] + b[[inter]]),
    se        = c(sqrt(V[base, base]),
                  sqrt(V[base, base] + V[inter, inter] + 2 * V[base, inter])),
    row.names = NULL, stringsAsFactors = FALSE
  )
}

service_res <- rbind(
  implied_slopes(ols_b, "county_dism",     "county_dism:served", "OLS",       "Black"),
  implied_slopes(ols_w, "county_dism",     "county_dism:served", "OLS",       "White"),
  implied_slopes(rdi_b, "fit_county_dism", "fit_cd_served",      "RDI IV",    "Black"),
  implied_slopes(rdi_w, "fit_county_dism", "fit_cd_served",      "RDI IV",    "White"),
  implied_slopes(riv_b, "fit_county_dism", "fit_cd_served",      "Rivers IV", "Black"),
  implied_slopes(riv_w, "fit_county_dism", "fit_cd_served",      "Rivers IV", "White")
)

service_res <- service_res %>%
  mutate(
    lo        = estimate - 1.96 * se,
    hi        = estimate + 1.96 * se,
    estimator = factor(estimator, levels = c("OLS", "RDI IV", "Rivers IV")),
    served    = factor(served, levels = c("Not Served", "Served"))
  )

service_plot_data <- service_res %>%
  mutate(estimate = estimate * 10, lo = lo * 10, hi = hi * 10)   # per 10-point rise in D

cache_estimate_range("military", service_plot_data$lo, service_plot_data$hi)

service_plot <-
  service_plot_data %>%
  ggplot(aes(served, estimate, color = race, shape = served)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_pointrange(aes(ymin = lo, ymax = hi),
                  position = position_dodge(width = 0.35),
                  linewidth = 0.7, fatten = 2.5, size = 1) +
  # Fixed (not free_y) scales so the three estimator panels share one axis, matching
  # the shared axis this figure also carries across the manuscript's other main
  # estimate figures.
  facet_wrap(~estimator) +
  coord_cartesian(ylim = shared_axis_range(MAIN_ESTIMATE_FIGS)) +
  labs(x = NULL, y = "Years of Life (per 10-point rise in D)", color = "Group", shape = "Service",
       subtitle = str_wrap("Implied segregation slope for non-servers vs. servers, from D x Service interaction models", 50)) +
  theme_cowplot() +
  # Subtitle sized so a 9.5pt note survives the ~0.36x shrink from this plot's 9in
  # ggsave width down to its half-\textwidth (3.25in) display size on print.
  theme(legend.position = "top", axis.text.x = element_text(angle = 20, hjust = 1),
        plot.subtitle = element_text(size = 26)) +
  scale_color_manual(values = c("Black" = "#1b7837", "White" = "#2166ac"))

ggsave(service_plot, filename = here("FigTab", "military_service_estimates.jpeg"),
       width = 9, height = 7, dpi = 300)
