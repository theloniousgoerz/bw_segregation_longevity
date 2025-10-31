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

# Options
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

setwd("/Users/theloniousgoerz/Academic/Projects/QP/Analysis/")

my_ftest <- function(modc, modnc)
{
  df_dif <- (degrees_freedom(modc, type="resid") - degrees_freedom(modnc, type="resid"))
  df_nc <- degrees_freedom(modnc, type="resid")
  fstat <- ((modc$ssr - modnc$ssr) / df_dif) / (modnc$ssr / df_nc)
  pvf <- pf(fstat, df_dif, df_nc, lower.tail = FALSE)
  print(paste(paste("The F-statistic is", fstat, sep=" "), paste("and the p-value is", pvf, sep=" "), sep=" "))
}

`%notin%` = Negate(`%in%`)
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Data
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
data = read_csv("../Data/_Cleaned/data.csv")
data_a = read_csv("../Data/_Cleaned/data_a.csv")                                                                        
db = read_csv("../Data/_Cleaned/db.csv")                                                                              
dw = read_csv("../Data/_Cleaned/dw.csv")   
county_d = read_csv("../Data/_Cleaned/county_data.csv")
#gov_fin = read_csv("../Data/GFD/Government Finance Database All Data/The Government Finance Database_All Data.csv")

## Rescale D for analysis
data %<>% mutate(county_dism = county_dism*100)
data_a %<>% mutate(county_dism = county_dism*100)
db %<>% mutate(county_dism = county_dism*100)
dw %<>% mutate(county_dism = county_dism*100)

## Merge isolation and H indices on 
seg_measures = county_d %>% select(death_decade = YEAR,death_fips,county_isolb,H_bw,D_star) %>% 
  mutate(county_isolb = county_isolb*100,
         H_bw = H_bw*100,
         D_star = D_star*100)

db %<>% left_join(.,seg_measures) %>% filter(!is.na(D_star))
dw %<>% left_join(.,seg_measures) %>% filter(!is.na(D_star))

# Education multi-category
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
# Analysis 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Show OLS at different sample 
ols_full_b = feols(death_age~county_dism + male |byear + STATEFIP_b + urb_code, data = subset(data,Race == "Black"),vcov = "white")
ols_full_w = feols(death_age~county_dism + male |byear + STATEFIP_b + urb_code, data = subset(data,Race == "White"),vcov = "white")
ols_analytic_b = feols(death_age~county_dism + male |byear + STATEFIP_b + urb_code, data = db,vcov = "white")
ols_analytic_w = feols(death_age~county_dism + male |byear + STATEFIP_b + urb_code, data = dw,vcov = "white")


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# First-stage 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
instrument = data_a %>% 
  distinct(county_dism ,death_decade,ln_gov,gov_rev_share_state,death_fips,land_sq_mi) 

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

## Save Table ## 

  save_tt(f_table,output = "./FigTab/f_table.tex", overwrite = T)
  
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Instrument Placebo Test
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# The idea: Use other measures of residential options that might not affect seg 
  # Special Districts 
special_dist = gov_fin %>% 
  filter(
    # Filter Special dist
    Type_Code == 4 & 
    Year4 == 1967
  ) %>% 
  mutate(death_fips = FIPS_Combined) %>%
  group_by(death_fips) %>% 
  summarise(n_special_dist = n())

unlist(fitstat(feols(county_dism~land_sq_mi | death_decade, data = instrument, cluster = "death_fips"),type = "f")) %>% 
  data.frame() %>% datasummary_df()
  
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## OLS 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ols_m1_b = feols(death_age~county_dism   |byear + STATEFIP_b + urb_code, data = db,vcov = "white")
ols_m1_w = feols(death_age~county_dism   |byear + STATEFIP_b + urb_code, data = dw,vcov = "white")
ols_m2_b = feols(death_age~county_dism + male + migrated + education + married |byear + STATEFIP_b  + urb_code +OCC, data = db,vcov = "white")
ols_m2_w = feols(death_age~county_dism + male + migrated + education + married |byear + STATEFIP_b  + urb_code +OCC, data = dw,vcov = "white")
ols_base_b = feols(death_age~county_dism + male + migrated + education  + married |byear  +OCC + STATEFIP_b  + urb_code + death_decade , data = db,vcov = "white")
ols_base_w = feols(death_age~county_dism + male + migrated + education  + married |byear  +OCC + STATEFIP_b  + urb_code + death_decade, data =  dw,vcov = "white")

  msummary(list("Black" = ols_m1_b,
                "Black + C" = ols_m2_b,
                "White" = ols_m1_w,
                "White + C" = ols_m2_w),fmt =3, stars = T,
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
    save_tt(.,output = "./FigTab/OLS_results_table.tex", overwrite = T)
  
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## IV analysis
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

### Estimate Models ### 
# Progressively layer controls, fixed effects, and period fixed effects 
# death_fips^byear lead to same conclusions
m1_b = feols(death_age~1|byear + STATEFIP_b + urb_code |county_dism~ln_gov + gov_rev_share_state, data = db,vcov = "white")
m1_w = feols(death_age~1|byear + STATEFIP_b + urb_code |county_dism~ln_gov + gov_rev_share_state, data = dw,vcov = "white")
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
  save_tt(.,output = "./FigTab/IV_results_table.tex", overwrite = T)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Figures
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Instrument # 
# Binscatter regressions 
b_share = binsreg(instrument$county_dism,instrument$ln_gov,at = "mean",w = instrument$gov_rev_share_state)
i_data = as.data.frame(b_share$data.plot)

bs = 
  ggplot(data = i_data,aes(Group.Full.Sample.data.dots.x,Group.Full.Sample.data.dots.fit)) + 
  geom_point(size =2) +
  geom_smooth(method = "lm",
              alpha = 0, 
              lwd = 2) + 
  labs(x = "Log(N Governments) + Revenue Share",
       y = "County Dissimilarity",
       caption = str_wrap("Dots represent means of bins at each level of x. 
                          The blue line corresponds to the fitted OLS regression line of dissimilarity on the instruments. 
                          F-statistic = 226.",100)) + 
  theme_cowplot() + 
  theme(plot.caption = element_text(hjust = 0)) 

ggsave(bs,filename = "./FigTab/fs_plot.jpeg",
       width = 10, 
       height = 5,
       dpi = 1000)


county_d %>% filter(death_fips %in% instrument$death_fips) %>%
  group_by(death_decade) %>% 
  summarise(weighted.mean(county_dism,w = pop))

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
contrast = 10
# Set Contrast for Plots
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# OLS Results # 

ols_m1 = ols_m1_b %>% tidy(conf.int = T) %>% filter(term == "county_dism") %>%  mutate(Model = "Unadjusted", Race = "Black")
ols_m2 = ols_m1_w %>% tidy(conf.int = T) %>% filter(term == "county_dism") %>%  mutate(Model = "Unadjusted", Race = "White")
ols_m3 = ols_m2_b %>% tidy(conf.int = T) %>% filter(term == "county_dism")  %>% mutate(Model = "Adjusted", Race = "Black")
ols_m4 = ols_m2_w %>% tidy(conf.int = T) %>% filter(term == "county_dism")  %>% mutate(Model = "Adjusted", Race = "White")

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
    caption = "This figure displays estimates of the OLS association between racial segregation and longevity. 
    Estimates refer to a 10 point increase in Dissimilarity.",
    x = "Model",
    y = "Change in Years of Life") + 
  scale_color_manual(values = c("darkgreen","darkblue"))  + 
    theme(plot.caption = element_text(hjust = 0),
          legend.position = "bottom") 
  
### Save Figure ### 
ggsave(ols_res_fig,
       filename = "./FigTab/ols_figure.jpeg",
       width = 8, 
       height = 5,
       dpi = 1000)

### Plots of Main Effects
m1_b_plot =   tidy(m1_b,conf.int = T) %>% filter(term =="fit_county_dism") %>% mutate(Race = "Black", Model = "Unadjusted")
m1_w_plot =   tidy(m1_w,conf.int = T) %>% filter(term =="fit_county_dism") %>% mutate(Race = "White", Model = "Unadjusted")
m2_b_plot =   tidy(m2_b,conf.int = T) %>% filter(term =="fit_county_dism") %>% mutate(Race = "Black", Model = "Adjusted")
m2_w_plot =   tidy(m2_w,conf.int = T) %>% filter(term =="fit_county_dism") %>% mutate(Race = "White", Model = "Adjusted")

iv_plot = rbind(m1_b_plot,
                m1_w_plot,
                m2_b_plot,
                m2_w_plot)

### Make Plot ### 
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
  labs(y = "Change in Years of Life",
       caption = "This figure displays estimates from two-stage least squares regressions of the effect of racial segregation on longevity. 
    Estimates refer to a 10 point increase in Dissimilarity."
  ) + 
  scale_color_manual(values = c("darkgreen","darkblue")) + 
  theme_cowplot() + 
  geom_hline(yintercept = 0,linetype = "dashed",lwd = 1)    + 
  theme(plot.caption = element_text(hjust = 0),
        legend.position = "bottom") 

ggsave(Iv_estimate_plot,filename = "./FigTab/iv_plot.jpeg",
       width = 8, 
       height = 5,
       dpi = 1000)
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Subgroup Analysis
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
############### Education ###############


w_ed_c = feols(death_age~male + migrated + married + south + educ_cat |byear + STATEFIP_b +urb_code + OCC | county_dism~ ln_gov + gov_rev_share_state, data = dw,vcov = "white")
b_ed_c = feols(death_age~male + migrated + married + south + educ_cat |byear + STATEFIP_b +urb_code + OCC | county_dism~ ln_gov + gov_rev_share_state, data = db,vcov = "white")

w_ed = feols(death_age~male + migrated + married + south |byear + STATEFIP_b +urb_code + OCC | county_dism*educ_cat~ ln_gov*educ_cat + gov_rev_share_state*educ_cat, data = dw,vcov = "white") 
b_ed = feols(death_age~male + migrated + married + south |byear + STATEFIP_b +urb_code + OCC | county_dism*educ_cat~ ln_gov*educ_cat + gov_rev_share_state*educ_cat, data = db,vcov = "white")

# White #

education_white =avg_slopes(w_ed, 
           variables = "county_dism",
           by = "educ_cat",
           newdata = datagrid(
             educ_cat = unique(dw$educ_cat),
             county_dism = unique(dw$county_dism)
           )) %>% 
            tidy(conf.int = T) 


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
  labs(y = "Change in Years of Life",
       x = "Education Level",
       caption = str_wrap("This figure displays results of a model with an interaction between education-level and segregation for White Americans. \n 
       The interaction is significant (p <.001) based on an F-test between nested and unnested models.",100)) +
  theme_cowplot() + 
  geom_hline(yintercept = 0,linetype = "dashed",lwd = 1) + 
  theme(plot.caption = element_text(hjust = 0),
        legend.position = "bottom") 

# Black # 

education_black =avg_slopes(b_ed, 
                            variables = "county_dism",
                            by = "educ_cat",
                            newdata = datagrid(
                              educ_cat = unique(db$educ_cat),
                              county_dism = unique(db$county_dism)
                            )) %>% 
  tidy(conf.int = T) 

# Black #

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
  labs(y = "Change in Years of Life",
       x = "Education Level",
       caption = str_wrap("This figure displays results of a model with an interaction between education-level and segregation for Black Americans. \n 
       The interaction is not significant based on an F-test between nested and unnested models.",100)) +
  theme_cowplot() + 
  geom_hline(yintercept = 0,linetype = "dashed",lwd = 1)   + 
  theme(plot.caption = element_text(hjust = 0),
        legend.position = "bottom") 
  ylim(-.55,.05)

# Save plots 
ggsave(w_ed_graph,filename = "./FigTab/White_Ed_Interaction.jpeg",
       width = 8, 
       height = 5,
       dpi = 1000)

ggsave(b_ed_graph,filename = "./FigTab/Black_Ed_Interaction.jpeg",
       width = 8, 
       height = 5,
       dpi = 1000)

# Table 
w_ed

## Create Table ## 
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
         notes = "This table describes IV estimates of the effect of segregation on longevity interacted by education. First-stage regressions are supppressed for concision. Heteroskedasiticty Robust Standard Errors in parentheses.",
         title = "Estimates of the Effect of Segregation on Longevity by Education-Level",
         add_rows = data.frame(
           FE = c("Birth Year","Birth State","Urban-Rural Code","Occupation"),
           m1 = c("X","X","X","X"),
           m2 = c("X","X","X","X")
         )) %>% 
  save_tt(.,output = "./FigTab/IV_by_Education_table.tex", overwrite = T)

############### Male V Female ###############

## M
m2_b_m = feols(death_age~migrated + education + married + south|byear + STATEFIP_b + urb_code + OCC  |county_dism~ln_gov + gov_rev_share_state, data = subset(db,male ==1),vcov = "white")
m2_w_m = feols(death_age~migrated + education + married + south|byear + STATEFIP_b + urb_code + OCC  |county_dism~ln_gov + gov_rev_share_state, data = subset(dw,male ==1),vcov = "white")
## F 
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
         notes = "This table describes IV estimates of the effect of segregation on longevity by gender. First-stage regressions are supppressed for concision.Heteroskedasiticty Robust Standard Errors in parentheses.",
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
       caption = "This figure displays estimates from two-stage least squares regressions of the effect of racial segregation on longevity by Gender. 
    Estimates refer to a 10 point increase in Dissimilarity."
  ) + 
  scale_color_manual(values = c("darkgreen","darkblue")) + 
  theme_cowplot() + 
  geom_hline(yintercept = 0,linetype = "dashed",lwd = 1)    + 
  theme(plot.caption = element_text(hjust = 0),
        legend.position = "bottom") 

ggsave(Iv_estimate_plot_g,filename = "./FigTab/iv_plot_gender.jpeg",
       width = 8, 
       height = 5,
       dpi = 1000)


# Weights 

m1_b_w = feols(death_age~1|byear + STATEFIP_b + urb_code |county_dism~ln_gov + gov_rev_share_state, data = db,vcov = "white",weights = db$weight)
m1_w_w = feols(death_age~1|byear + STATEFIP_b + urb_code |county_dism~ln_gov + gov_rev_share_state, data = dw,vcov = "white",weights = dw$weight)
m2_b_w = feols(death_age~male + migrated + education + married + south|byear + STATEFIP_b + urb_code + OCC  |county_dism~ln_gov + gov_rev_share_state, data = db,vcov = "white",weights = db$weight)
m2_w_w = feols(death_age~male + migrated + education + married + south|byear + STATEFIP_b + urb_code + OCC  |county_dism~ln_gov + gov_rev_share_state, data = dw,vcov = "white",weights = dw$weight)

msummary(list("Black" = m1_b_w,
              "Black + C" = m2_b_w,
              "White" = m1_w_w,
              "White + C" = m2_w_w),fmt =3, stars = T,
         coef_map = c("fit_county_dism" = "D",
                      "male" = "Male",
                      "education" = "Education",
                      "migratedMigrated" = "Migrated",
                      "married" = "Married in 1940",
                      "south" = "Died in South"),
         gof_map = c("nobs",
                     "r.squared"
                     
         ),
         notes = "This table describes IV estimates of the effect of segregation on longevity using post-stratification weights. First-stage regressions are supppressed for concision. Heteroskedasiticty Robust Standard Errors in parentheses.",
         title = "Estimates of the Effect of Segregation on Longevity (Weights)",
         add_rows = data.frame(`FE` = c("Birth Year","Birth State","Urban Rural Code","OCC"),
                               m1 = c("Yes","Yes","Yes","No"),
                               m2 = c("Yes","Yes","Yes","Yes"),
                               m4 = c("Yes","Yes","Yes","No"),
                               m5 = c("Yes","Yes","Yes","Yes")),
         output = "tinytable") %>% 
  save_tt(.,output = "./FigTab/IV_results_weights_table.tex", overwrite = T) 


## Stayers are similar
b_stay = feols(death_age~male +education + married + south |byear  + urb_code + OCC   |county_dism~ln_gov + gov_rev_share_state, data = subset(db,migrated != "Migrated"),vcov = "white")
w_stay = feols(death_age~male +education + married + south |byear  + urb_code + OCC   |county_dism~ln_gov + gov_rev_share_state, data = subset(dw,migrated != "Migrated"),vcov = "white")

b_move = feols(death_age~male +education + married + south |byear + urb_code + OCC + STATEFIP_b  |county_dism~ln_gov + gov_rev_share_state, data = subset(db,migrated == "Migrated"),vcov = "white")
w_move = feols(death_age~male +education + married + south |byear + urb_code + OCC + STATEFIP_b  |county_dism~ln_gov + gov_rev_share_state, data = subset(dw,migrated == "Migrated"),vcov = "white")

msummary(list("Black (Stayer)" =b_stay ,
              "Black (Mover)"= b_move,
              "White (Stayer)" = w_stay,
              "White (Mover)" = w_move),
               fmt =3,
               stars = T,
         coef_map = c("fit_county_dism" = "D",
                      "male" = "Male",
                      "education" = "Education",
                      "migratedMigrated" = "Migrated",
                      "married" = "Married in 1940",
                      "south" = "Died in South"),
         gof_map = c("nobs",
                     "r.squared"
                     
         ),
         notes = "Heteroskedasiticty Robust Standard Errors in parentheses. Estimates for stayers do not include birth state FEs because these are by definition constant.",
         title = "Estimates of the Effect of Segregation on Longevity (Stayers)",
         add_rows = data.frame(`FE` = c("Birth Year","Birth State","Urban Rural Code","OCC"),
                               m1 = c("Yes","-","Yes","Yes"),
                               m2 = c("Yes","Yes","Yes","Yes"),
                               m1 = c("Yes","-","Yes","Yes"),
                               m2 = c("Yes","Yes","Yes","Yes")))  %>% 
  save_tt(.,output = "./FigTab/IV_results_stayers_table.tex", overwrite = T) 
 

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Alternative Measures of D 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

H_b = feols(death_age~male + migrated + education + married + south|byear + STATEFIP_b + urb_code + OCC  |H_bw~ln_gov + gov_rev_share_state, data = db,vcov = "white")
H_w = feols(death_age~male + migrated + education + married + south|byear + STATEFIP_b + urb_code + OCC  |H_bw~ln_gov + gov_rev_share_state, data = dw,vcov = "white")
I_b = feols(death_age~male + migrated + education + married + south|byear + STATEFIP_b + urb_code + OCC  |county_isolb~ln_gov + gov_rev_share_state, data = subset(db),vcov = "white")
I_w = feols(death_age~male + migrated + education + married + south|byear + STATEFIP_b + urb_code + OCC  |county_isolb~ln_gov + gov_rev_share_state, data = subset(dw),vcov = "white")
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
         notes = "This table describes IV estimates of the effect of segregation on longevity for alternative measures of D. First-stage regressions are supppressed for concision.Heteroskedasiticty Robust Standard Errors in parentheses. Models adjust for all covariates and FEs used in main analyses but are not shown in the model.",
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
  save_tt(.,output = "./FigTab/IV_results_table_alt_measure.tex", overwrite = T)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Monotonicity Descriptive
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

data_m = data_a %>% distinct(ln_gov, gov_rev_share_state,death_fips,death_decade,county_dism) 

lm(county_dism~ln_gov +gov_rev_share_state,subset(data_m, death_decade == 1980)) %>% summary()
lm(county_dism~ln_gov +gov_rev_share_state,subset(data_m, death_decade == 1990)) %>% summary()
lm(county_dism~ln_gov +gov_rev_share_state,subset(data_m, death_decade == 2000)) %>% summary()
 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Supplementary Figures (OLS By Sample)
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ols_fb = ols_full_b %>% tidy(conf.int = T) %>% filter(term == "county_dism") %>% mutate(Data = "All", Race = "Black")
ols_fw = ols_full_w %>% tidy(conf.int = T) %>% filter(term == "county_dism") %>% mutate(Data = "All", Race = "White")
ols_ab = ols_analytic_b %>% tidy(conf.int = T) %>% filter(term == "county_dism")  %>% mutate(Data = "Analytic", Race = "Black")
ols_aw = ols_analytic_w %>% tidy(conf.int = T) %>% filter(term == "county_dism")  %>% mutate(Data = "Analytic", Race = "White")

# Rbind # 

### Visualize cross-sample OLS comparison ### 
ols_result_sample = rbind(ols_fb, 
                          ols_fw, 
                          ols_ab, 
                          ols_aw
) %>% mutate(Data = factor(Data,levels = unique(Data)),
             estimate = estimate*21.4,
             conf.low = conf.low*21.4,
             conf.high = conf.high*21.4)

### Visualize Results ### 
ols_res_fig = 
  ggplot(ols_result_sample,aes(Data,estimate,ymin = conf.low,ymax = conf.high, color = Race)) + 
  geom_pointrange(lwd = 1,position = position_dodge2(width = .1)) + 
  theme_cowplot() + 
  # ylim(-5,0) + 
  geom_hline(yintercept = 0, linetype = "dashed", lwd = 1) + 
  theme(legend.position = "top") + 
  labs(caption = str_wrap("Note: The All column refers to the association between segregation and longevity among all 20-35 years old Black and White Men and Women in the Numident sample and 1940 Census conditional on complete cases and living in counties with Black populations >0.
                          The Analytic sample is subsequently conditional on complete cases with coverage of the instrument.",100),
       #  title = "Association Between Segregation and Longevity (By Sample)",
       subtitle = "Contrast = ((Y|D = 72.5) - (Y|D = 51.1))",
       x = "Sample",
       y = "Estimate") + 
  scale_color_manual(values = c("darkgreen","darkblue")) 
### Save Figure ### 

ggsave(ols_res_fig,filename = "./FigTab/ols_by_sample.jpeg",width = 10, height = 6)

 