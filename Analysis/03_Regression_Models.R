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

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Data Cleaning for Analysis
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Rescale D for analysis -------------------------------

data_a %<>% mutate(county_dism = county_dism*100)
db %<>% mutate(county_dism = county_dism*100,
               county_isolb = county_isolb*100,
         H_bw = H_bw*100,
         D_star = D_star*100)

dw %<>% mutate(county_dism = county_dism*100,
               county_isolb = county_isolb*100,
               H_bw = H_bw*100,
               D_star = D_star*100
               )

# Education multi-category -------------------------------
db %<>% 
  mutate(educ_cat = case_when(
    educ_years <12 ~ "Less than HS",
    educ_years == 12 ~ "High School",
    educ_years >12 & educ_years < 16 ~ "Some College",
    educ_years >=16 ~ "College+"
  ),
  educ_cat = factor(educ_cat, levels = c("Less than HS","High School","Some College","College+")))
dw %<>% 
  mutate(educ_cat = case_when(
    educ_years <12 ~ "Less than HS",
    educ_years == 12 ~ "High School",
    educ_years > 12 & educ_years < 16 ~ "Some College",
    educ_years >=16 ~ "College+"
  ),
  educ_cat = factor(educ_cat, levels = c("Less than HS","High School","Some College","College+")))


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Run Models
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# -------------------------------First-stage -------------------------------

# ------------------------------- Collapse ---------------------------------
instrument = data_a %>% 
  distinct(county_dism ,death_decade,ln_gov,gov_rev_share_state,death_fips) 

# ------------------------------- Make Table -------------------------------
f  = feols(county_dism~ln_gov + gov_rev_share_state | death_decade, data = instrument, vcov = "white") 
f_table = msummary(f,stars = T,
         gof_map = c("nobs","f"),
         coef_map = c("ln_gov" = "Ln(Number Governments)",
                      "gov_rev_share_state" = "County Share Revenue from Federal Gov."),
         add_rows = data.frame("F",s = unlist(fitstat(f, type = "f"))[1]),
         notes = "First stage relationship includes death decade fixed effects and heteroskedacticity SEs.",
         title = "First Stage Regression of D on Instruments",
         align = "lc",
         threeparttable = T, 
         fmt = 2, 
         output = "tinytable")
# ------------------------------- Save -------------------------------
save_tt(f_table,output = here("FigTab","f_table.tex"), overwrite = T)
  
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## OLS 
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

# ------------------------------- Estimate IV Models -------------------------------

m1_b = feols(death_age~1|byear + STATEFIP_b + urb_code |county_dism~ln_gov + gov_rev_share_state, data = db, vcov = "white")
m1_w = feols(death_age~1|byear + STATEFIP_b + urb_code |county_dism~ln_gov + gov_rev_share_state, data = dw, vcov = "white")
m2_b = feols(death_age~male + migrated + education + married + south|byear + STATEFIP_b + urb_code + OCC  |county_dism~ln_gov + gov_rev_share_state, data = db,vcov = "white")
m2_w = feols(death_age~male + migrated + education + married + south|byear + STATEFIP_b + urb_code + OCC  |county_dism~ln_gov + gov_rev_share_state, data = dw,vcov = "white")

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

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Subgroup Analysis
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ------------------------------- Education ------------------------------- 
w_ed_c = feols(death_age~male + migrated + married + south + educ_cat |byear + STATEFIP_b +urb_code + OCC | county_dism~ ln_gov + gov_rev_share_state, data = dw,vcov = "white")
b_ed_c = feols(death_age~male + migrated + married + south + educ_cat |byear + STATEFIP_b +urb_code + OCC | county_dism~ ln_gov + gov_rev_share_state, data = db,vcov = "white")
w_ed =   feols(death_age~male + migrated + married + south |byear + STATEFIP_b +urb_code | county_dism*educ_cat~ ln_gov*educ_cat + gov_rev_share_state*educ_cat, data = dw,vcov = "white") 
b_ed =   feols(death_age~male + migrated + married + south |byear + STATEFIP_b +urb_code | county_dism*educ_cat~ ln_gov*educ_cat + gov_rev_share_state*educ_cat, data = db,vcov = "white")

# my_ftest(w_ed_c,w_ed) p <.001
# my_ftest(b_ed_c,b_ed) p = .45

# -------------------------------  White ------------------------------- 

education_white =avg_slopes(w_ed, 
           variables = "county_dism",
           by = "educ_cat",
           newdata = datagrid(
             educ_cat = unique(dw$educ_cat),
             county_dism = unique(dw$county_dism)
           )) %>% 
            tidy(conf.int = T) 

avg_slopes(w_ed, 
           variables = "county_dism",
           by = "educ_cat",
           newdata = datagrid(
             educ_cat = unique(dw$educ_cat),
             county_dism = unique(dw$county_dism)
           ),
           hypothesis = "pairwise")

# ------------------------------- Calculate AMEs ------------------------------- 
w_ed_graph = education_white %>% 
  mutate(
    educ_cat = factor(educ_cat, levels = c("Less than HS","High School","Some College","College+")),
    estimate = estimate*contrast,
    conf.high = conf.high*contrast,
    conf.low = conf.low*contrast
  ) %>% 
  ggplot(aes(educ_cat,estimate,
             ymin = conf.low,
             ymax = conf.high)) + 
  geom_pointrange(position = position_dodge2(width =.1),
                  lwd = 2,
                  size = .75) + 
  labs(y = "Change in Life Expectancy",
       x = "Education Level",
       caption = str_wrap("This figure displays results of a model with an interaction between education-level and segregation for White Americans. \n 
       The model includes covariates and fixed effects.
       The interaction is significant (p <.001) based on an F-test between nested and unnested models.",100)) +
  theme_cowplot() + 
  geom_hline(yintercept = 0,linetype = "dashed",lwd = 1) + 
  theme(plot.caption = element_text(hjust = 0),
        legend.position = "bottom") 

# ------------------------------- Black-------------------------------   

education_black =avg_slopes(b_ed, 
                            variables = "county_dism",
                            by = "educ_cat",
                            newdata = datagrid(
                              educ_cat = unique(db$educ_cat),
                              county_dism = unique(db$county_dism)
                            )) %>% 
  tidy(conf.int = T) 

# ------------------------------- Calculate AMEs ------------------------------- 

b_ed_graph = education_black %>% 
  mutate(
    educ_cat = factor(educ_cat, levels = c("Less than HS","High School","Some College","College+")),
    estimate = estimate*contrast,
    conf.high = conf.high*contrast,
    conf.low = conf.low*contrast
  ) %>% 
  ggplot(aes(educ_cat,estimate,
             ymin = conf.low,
             ymax = conf.high)) + 
  geom_pointrange(position = position_dodge2(width =.1),
                  lwd = 2,
                  size = .75) + 
  labs(y = "Change in Life Expectancy",
       x = "Education Level",
       caption = str_wrap("This figure displays results of a model with an interaction between education-level and segregation for Black Americans. \n 
       The model includes covariates and fixed effects.
       The interaction is not significant (p =.45) based on an F-test between nested and unnested models.",100)) +
  theme_cowplot() + 
  geom_hline(yintercept = 0,linetype = "dashed",lwd = 1)   + 
  theme(plot.caption = element_text(hjust = 0),
        legend.position = "bottom") 
  ylim(-.55,.05)

# Save plots 
ggsave(w_ed_graph,filename = here("FigTab","White_Ed_Interaction.jpeg"),
       width = 8, 
       height = 5,
       dpi = 1000)

ggsave(b_ed_graph,filename = here("FigTab","Black_Ed_Interaction.jpeg"),
       width = 8, 
       height = 5,
       dpi = 1000)

## ------------------------------- Create Table ------------------------------- ## 
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
