# # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Double Truncation
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
library(here)
library(lfe)
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Data
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
db =       read_csv(here("Data","_Cleaned","db.csv"))                                                                              
dw =       read_csv(here("Data","_Cleaned","dw.csv"))   
rivers =      read_csv(here("Data","derived","tiger_hydrography_county_instruments_2023.csv"))
rdi =   read_csv(here("Data","derived","atack_rail_county_instruments_1911.csv"))
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Data Cleaning for Analysis
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Rescale D for analysis -------------------------------
db %<>% mutate(county_dism = county_dism*100,
               county_isolb = county_isolb*100,
               H_bw = H_bw*100,
               D_star = D_star*100)

dw %<>% mutate(county_dism = county_dism*100,
               county_isolb = county_isolb*100,
               H_bw = H_bw*100,
               D_star = D_star*100
)
#  -------------------------------
# Merge on rdi and river instruments
#  -------------------------------
rdi %<>% mutate(death_fips = GEOID)
rivers %<>% mutate(death_fips = GEOID)

db %<>% left_join(rdi) %>% left_join(rivers)
dw %<>% left_join(rdi) %>% left_join(rivers)

db_rdi = db %>% filter(!is.na(rdi) & !is.na(rail_km_per_km2) & !is.na(byear) & !is.na(STATEFIP_b) & !is.na(urb_code)) %>% group_by(STATEFIP_b) %>%
  mutate(nrow = max(row_number())) %>% filter(nrow>1) %>% ungroup()
dw_rdi = dw %>% filter(!is.na(rdi) & !is.na(rail_km_per_km2) & !is.na(byear) & !is.na(STATEFIP_b) & !is.na(urb_code)) %>% group_by(STATEFIP_b) %>%
  mutate(nrow = max(row_number())) %>% filter(nrow>1) %>% ungroup()
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Manually create Instrument 
db_rdi$iv = fitted(feols(county_dism~rdi+rail_km_per_km2 |byear + urb_code + STATEFIP_b, vcov = ~death_fips,data = db_rdi))
dw_rdi$iv = fitted(feols(county_dism~rdi+rail_km_per_km2 |byear + urb_code + STATEFIP_b, vcov = ~death_fips,data = dw_rdi))

# Check Ests against base spec
b_g0 = feols(death_age~county_dism |byear + urb_code + STATEFIP_b,data = db_rdi, vcov = "white")
w_g0 = feols(death_age~county_dism |byear + urb_code + STATEFIP_b,data = dw_rdi, vcov = "white")

msummary(list(
  b_g0,
  w_g0
),
stars = T)


dw_rdi_g = dw_rdi %>% select(death_age,byear,dyear,county_dism,STATEFIP_b,urb_code) %>% mutate(dyear = as.numeric(dyear),byear = as.numeric(byear))
db_rdi_g = db_rdi %>% select(death_age,byear,dyear,county_dism,STATEFIP_b,urb_code) %>% mutate(dyear = as.numeric(dyear),byear = as.numeric(byear))

# ── FWL helper ────────────────────────────────────────────────────────────────
# Residualises a numeric vector on the three fixed effects using demeanlist()
# which handles multi-way within-transformation efficiently.

partial_out_fe <- function(df) {
  
  fe_list <- list(
    factor(df$byear),
    factor(df$STATEFIP_b),
    factor(df$urb_code)
  )
  
  # demeanlist() iterates the within-transformation until convergence
  # (Gauss-Seidel algorithm) — fast even for large N with many FE levels
  resids <- lfe::demeanlist(
    df %>% select(death_age, county_dism),   # matrix of variables to partial out
    fl    = fe_list
  )
  
  df %>%
    mutate(
      death_age_resid = resids[, "death_age"],
      county_dism_resid        = resids[, "county_dism"]
    )
}

# ── Apply to each race dataset ─────────────────────────────────────────────────
dw_rdi_g_fwl <- partial_out_fe(dw_rdi_g)
db_rdi_g_fwl <- partial_out_fe(db_rdi_g)

# ── Re-add the grand means so the scale is interpretable ──────────────────────
# FWL residuals are mean-zero; adding back the mean of death_age preserves
# the original mortality scale that gompertz_mle expects.
dw_rdi_g_fwl <- dw_rdi_g_fwl %>%
  mutate(
    death_age_resid = death_age_resid + mean(dw_rdi_g$death_age, na.rm = TRUE),
    county_dism_resid        = county_dism_resid        + mean(dw_rdi_g$county_dism,        na.rm = TRUE)
  )

db_rdi_g_fwl <- db_rdi_g_fwl %>%
  mutate(
    death_age_resid = death_age_resid + mean(db_rdi_g$death_age, na.rm = TRUE),
    county_dism_resid        = county_dism_resid        + mean(db_rdi_g$county_dism,        na.rm = TRUE)
  )

# ── Gompertz models on FWL residuals ──────────────────────────────────────────
gompertz_model_black_fwl <- gompertztrunc::gompertz_mle(
  data        = db_rdi_g_fwl,
  death_age_resid ~ county_dism_resid,
  left_trunc  = 1988,
  right_trunc = 2005
)

gompertz_model_white_fwl <- gompertztrunc::gompertz_mle(
  data        = dw_rdi_g_fwl,
  death_age_resid ~ county_dism_resid,
  left_trunc  = 1988,
  right_trunc = 2005
)

## convert gompertz model to hazards
black_est = gompertztrunc::convert_hazards_to_ex(gompertz_model_black_fwl$results, use_model_estimates = T)
white_est = gompertztrunc::convert_hazards_to_ex(gompertz_model_white_fwl$results, use_model_estimates = T)

# Pull e65 estimates from Gompertz models
gompertz_ests <- bind_rows(
  black_est |> select(e65, e65_lower, e65_upper) |> mutate(race = "Black", source = "(D) Gompertz"),
  white_est |> select(e65, e65_lower, e65_upper) |> mutate(race = "White", source = "(D) Gompertz")
) |> rename(estimate = e65, lower = e65_lower, upper = e65_upper)

# Pull IV estimates from fixest models
iv_ests <- bind_rows(
  tibble(
    race   = "Black",
    source = "(D) (Regression)",
    estimate = coef(b_g0)[["county_dism"]],
    lower    = confint(b_g0)["county_dism", "2.5 %"],
    upper    = confint(b_g0)["county_dism", "97.5 %"]
  ),
  tibble(
    race   = "White",
    source = "(D) (Regression)",
    estimate = coef(w_g0)[["county_dism"]],
    lower    = confint(w_g0)["county_dism", "2.5 %"],
    upper    = confint(w_g0)["county_dism", "97.5 %"]
  )
)

plot_df <- bind_rows(gompertz_ests, iv_ests) |>
  mutate(
    race   = factor(race, levels = c("Black", "White")),
    source = factor(source, levels = c("(D) Gompertz", "(D) (Regression)"))
  )

plot_df


gompertz_figure = ggplot(plot_df, aes(x = source, y = estimate, ymin = lower, ymax = upper,  color = race)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_pointrange(
    shape = 15,
    position = position_dodge(width = 0.4),
    size = 0.6 ) +
  labs(
    x     = NULL,
    y     = "Years of Longevity (years)",
    shape = "Race", 
    caption =  str_wrap("Regression estimates include birth state, birth year, and urban-rural code fixed effects. 
                        Gompertz estimates model death age as a function of segregation regressed and fixed effects. 
                        To overcome computational limitations with high-dimensional fixed effects, the birth state, birth year, and urban-rural fixed 
                        effects are residualized out of death age and segregation using the Frisch-Waugh Lovell theorem.",100),
    subtitle = "Gompertz/Regression (Black) = 1.12; Gompertz/Regression (White) = 1.06."
  ) +
  theme_cowplot()  + 
  scale_color_manual(values = c("darkgreen","darkblue")) + 
  coord_flip() 

# Bias 
plot_df %>% 
  pivot_wider(
  names_from = c("source"),
  values_from = c( "estimate","upper","lower")
  ) %>% 
  mutate(bias = `estimate_(D) Gompertz`/ `estimate_(D) (Regression)`) %>% 
  datasummary_df(fmt = 4)

# ── Save ──────────────────────────────────────────
ggsave(gompertz_figure,filename =here("FigTab","gompertz_figure.jpeg"),
       width = 8, 
       height = 8,
       dpi = 1000)




