######################################################################################################################################################
# 02 Data Analysis 
# Date: 6/7/24
######################################################################################################################################################
# ------------# ------------# ------------# ------------# ------------# ------------# ------------# ------------# ------------
# Packages 
# ------------# ------------# ------------# ------------# ------------# ------------# ------------# ------------# ------------
rm(list = ls())
library(modelsummary)
library(tidyverse)
library(haven)
library(magrittr)
library(ivreg)
library(tidycensus)
library(striprtf)
library(readxl)
library(estimatr)
library(patchwork)
library(fixest)
library(marginaleffects)
library(ggeffects)
library(margins)
library(forcats)
options(scipen = 999)

# Need to obtain county contextal variables 

# ------------# ------------# ------------# ------------# ------------# ------------# ------------# ------------# ------------
## Load data ## 
# ------------# ------------# ------------# ------------# ------------# ------------# ------------# ------------# ------------

data = read_csv("Data/working_merged_data.csv")

data %<>% filter(educ < 18) %>% 
  mutate(statebth = as.character(statebth),
         samestate = ifelse(statebth == stateoc,1,0))
# Create education identifiers 

data %<>% mutate(lesshs = ifelse(educ <12,1,0),
                 hs = ifelse(educ == 12,1,0),
                 somecollege = ifelse(educ>12 & educ < 16,1,0),
                 ba = ifelse(educ ==16,1,0),
                 ba_plus = ifelse(educ> 16 & educ !=99,1,0),
                 Race = ifelse(black ==1,"Black","White"),
                 Quartile_Black_Share = case_when(pctbk1990  < 0.03626400 ~"Q1",
                                                  pctbk1990 >= 0.03626400  & pctbk1990 < 0.08254753 ~ "Q2",
                                                  pctbk1990  >=0.08254753 & pctbk1990 < 0.13857791 ~"Q3",
                                                  pctbk1990 >= 0.13857791 ~ "Q4"))
data %<>% mutate(datayear = datayear + 1900,
                yob = datayear-age) 

data %<>% mutate(
  fe = case_when(
   yob >=1867   & yob < 1877 ~ "decade_1fe",
   yob >= 1877  & yob < 1887 ~ "decade_2fe",
   yob >= 1887  & yob < 1897 ~ "decade_3fe",
   yob >= 1897  & yob < 1907 ~ "decade_4fe",
   yob >= 1907  & yob < 1917 ~ "decade_5fe",
   yob >= 1917  & yob < 1927 ~ "decade_6fe",
   yob >= 1927  & yob < 1937 ~ "decade_7fe",
   yob >= 1937  & yob < 1947 ~ "decade_8fe",
   yob >= 1947  & yob < 1957 ~ "decade_9fe",
   yob >= 1957 & yob  <1967 ~  "decade_10fe",
   yob >= 1967 & yob  <1977 ~  "decade_11fe",
   yob >= 1977 & yob  <1987 ~  "decade_12fe",
   yob >=1987 & yob < 1997 ~   "decade_13fe")
)

# Make five year FE 
data %<>% mutate(fe_5year = as.character(cut_number(yob,21)))

cov =  data %>% select(datayear,name) %>% distinct() %>% group_by(name) %>% summarise(N_years = n()) 

data %<>% left_join(.,cov)

# ------------# ------------# ------------# ------------# ------------# ------------# ------------# ------------# ------------
# Sample selection 
# ------------# ------------# ------------# ------------# ------------# ------------# ------------# ------------# ------------

data %>% group_by(name) %>% filter(datayear < 1995) %>% select(name,datayear) %>%
  distinct() %>% 
  # Create number of rows 
  mutate(n = max(row_number())) %>% 
  distinct() %>% filter(n ==5)

# check first stages between 76 msa and 106

# 76 F statistic is 22.21
data %>% filter(N_years ==10) %>% distinct(name,herf,dism1990) %>%
lm(herf~dism1990,.) %>% summary()
# 106 F stat is 21.06
data  %>% distinct(name,herf,dism1990) %>%
  lm(herf~dism1990,.) %>% summary()

# Filtering Criterion 
data %<>% filter(N_years == 10)
# Keep only those MSAs where there is 10 years of data. 
# ------------# ------------# ------------# ------------# ------------# ------------# ------------# ------------# ------------

# ------------# ------------# ------------# ------------# ------------# ------------# ------------# ------------# ------------
## Analysis ## 
# ------------# ------------# ------------# ------------# ------------# ------------# ------------# ------------# ------------
#
# FIRST STAGE
   # Note: Fist Stage is good in both cases 
# ------------# ------------# ------------
### COUNTY v MSA relationship ### 
# ------------# ------------# ------------
summary(lm(dism1990~county_dism,data))
data %>% select(county_dism,fipspmsa,county,dism1990) %>% unique() %>% 
  ggplot(aes(county_dism,dism1990)) + 
  geom_smooth(method = "lm") + 
  theme_bw() + 
  labs(x = "D (County)", y = "D (MSA)",
    title = "Association between County and MSA Dissimilarity") + 
  geom_point()

# ------------# ------------# ------------
# Association between instrument and county
# ------------# ------------# ------------

# i-level
# ------------# ------------# ------------
fs_i = data %>% select(state_abbr,county,county_dism,herf) %>% 
  lm(county_dism~herf,data=.)
# MSA level 
# ------------# ------------# ------------
fs_c= data %>% select(state_abbr,county,county_dism,herf) %>% distinct() %>%
 lm(county_dism~herf,data=.) 
# ------------# ------------# ------------
first_stage_f = data.frame(Fstat = "F",
                          I_level = summary(fs_i)$fstatistic[1],
                          MSA_level = summary(fs_c)$fstatistic[1])
                           
msummary(list("I-level" = fs_i,"County-level" = fs_c),
         title = "MSA and I-level First Stage (County)",stars = T,
         gof_map = c("nobs"),
         add_rows = first_stage_f, vcov = "HC0")

# ------------# ------------# ------------
# Association between instrument and msa
# ------------# ------------# ------------
# i-level
# ------------# ------------# ------------
fs_i_m = data %>%
  lm(dism1990~herf,data=.)
# MSA level 
# ------------# ------------# ------------
fs_m = data %>% select(fipspmsa,dism1990,herf) %>% distinct() %>%
  lm(dism1990~herf,data=.) 
# ------------# ------------# ------------
first_stage_f_m = data.frame(Fstat = "F",
                           I_level = summary(fs_i_m)$fstatistic[1],
                           MSA_level = summary(fs_m)$fstatistic[1])

msummary(list("I-level" = fs_i,"MSA-level" = fs_m),
         title = "MSA and I-level First Stage (MSA)",stars = T,
         gof_map = c("nobs"),
         add_rows = first_stage_f_m)
# ------------# ------------# ------------# ------------# ------------# ------------# ------------# ------------# ------------
# ------------# ------------# ------------# ------------# ------------# ------------# ------------# ------------# ------------
# Look at representativeness of the 76 MSAs 

data %>% 
  distinct(name,pop1990) %>% 
  summarise(Mean = mean(pop1990),
            total_pop = sum(pop1990))

# ------------# ------------# ------------# ------------# ------------# ------------# ------------# ------------# ------------

# ------------# ------------# ------------# ------------# ------------# ------------# ------------# ------------# ------------
## 1. Motivate the OLS relationship between segregation and mortality 
# ------------# ------------# ------------# ------------# ------------# ------------# ------------# ------------# ------------
# need to get county shares 

# ------------# ------------# ------------# ------------# ------------# ------------# ------------# ------------# ------------

f1_d_age =
  ggplot(data,aes(dism1990,age)) + 
  geom_smooth(method = "lm") + 
  theme_bw() + 
  labs(x = "D (MSA)",
       y =  "Age at Death",
       title = "Association between MSA Dissimilarity and Age at Death") 

# ------------# ------------# ------------# ------------# ------------# ------------# ------------# ------------# ------------
f2_d_age = 
  ggplot(data,aes(dism1990,age, color = Race)) + 
  geom_smooth(method = "lm") + 
  theme_bw() + 
  labs(x = "D (MSA)",
      y =  "Age at Death"
      #title = "Association between MSA Dissimilarity and Age at Death"
      )

f1_d_age /
f2_d_age
# ------------# ------------# ------------# ------------# ------------# ------------# ------------# ------------# ------------
# Age and seg net of year of birth 

data$age_r = residuals(feols(age~1 |yob,data))
data$dism1990_r = residuals(feols(dism1990~1 | yob,data))
# Pooled 

data %>%
  ggplot(aes(dism1990_r,age_r,color=Race)) + 
  geom_smooth(method = "lm") + 
  theme_bw() + 
  labs(x = "D (MSA)",
       y =  "Age at Death",
       title = "Association between MSA Dissimilarity and Age at Death (Adjusted for Birth Year)",
       subtitle ="Lines are residuals of regressions of D and Age on a birth year fixed effect."
  )

data %>%
  ggplot(aes(dism1990_r,age_r,color=Race)) + 
  geom_smooth(method = "lm") + 
  theme_bw() + 
  labs(x = "D (MSA)",
       y =  "Age at Death",
       title = "Association between MSA Dissimilarity and Age at Death (Adjusted for Birth Year)",
       subtitle ="Lines are residuals of regressions of D and Age on a birth year fixed effect."
  ) + 
  facet_wrap(~datayear)


# ------------# ------------# ------------# ------------# ------------# ------------# ------------# ------------# ------------
f1_d_age_w = data %>% filter(black == 0) %>%
  ggplot(aes(dism1990_r,age_r)) + 
  geom_smooth(method = "lm") + 
  theme_bw() + 
  labs(x = "D (MSA)",
       y =  "Age at Death",
       title = "Association between MSA Dissimilarity and Age at Death (White)")
f1_d_age_b = data %>% filter(black == 1) %>%
  ggplot(aes(dism1990_r,age_r)) + 
  geom_smooth(method = "lm") + 
  theme_bw() + 
  labs(x = "D (MSA)",
       y =  "Age at Death",
       title = "Association between MSA Dissimilarity and Age at Death (Black)")

f1_d_age_b /f1_d_age_w


# ------------# ------------# ------------# ------------# ------------# ------------# ------------# ------------# ------------
# ------------# ------------# ------------# ------------# ------------# ------------# ------------# ------------# ------------

#####
# OLS 
#####
# Modeling notes 
  # Because i have repeated cross sections, it seems that using the fixed effects for year are not necessary. 
# Could aggregate counties 

# ----
data %<>% mutate(yob_fe = as.character(yob),
                 dism19902 = dism1990*dism1990,
                 male = ifelse(sex == 1,1,0),
                 herf2 = herf*herf)

b1 = feols(age~dism1990 + male | 
                yob ,data[data$black ==1,], 
              cluster = ~yob + name)
w1 = feols(age~dism1990 + factor(sex) +datayear| 
             fe_5year ,data[data$black ==0,], 
           cluster = ~yob +name)


ols_df = data.frame(Coefficient = c("White","Black"),
                    Estimate = c(summary(w1)$coefficients[1],summary(b1)$coefficients[1]),
                    SE = c(summary(w1)$coeftable[1,2]),summary(b1)$coeftable[1,2])
ols_df %>% 
  ggplot(aes(x = Coefficient,y = Estimate)) + 
  geom_point(size =3) + 
  geom_errorbar(aes(ymin = Estimate - SE, 
                    ymax = Estimate + SE, 
                    color = Coefficient)) + 
  theme_bw() + 
  geom_hline(yintercept = 0,linetype = "dashed") + 
  labs(title = "OLS Estimates of The Between Racial Segregation and Age at Death",
       caption = "Data are pooled from 1990-99 and adjust for sex and year of birth.")

# ------------# ------------# ------------# ------------# ------------# ------------# ------------# ------------# ------------
# Adding additional covariates 

# test w 5 year and yob


# White 
b2 = summary(feols(age~dism1990 + factor(sex) |yob + statebth + marstat, data = data[data$black == 1,], cluster = ~yob + name))
b3 = feols(age~dism1990 + male + educ |
             yob + statebth + marstat + occup + state_abbr, data = data[data$black == 1,], cluster = ~yob + name)

msummary(list(b1,b2,b3),stars = T)



# WHite
w2 = summary(feols(age~dism1990 + factor(sex) + datayear |fe_5year + statebth + marstat, data = data[data$black == 0,], cluster = ~yob + name))
w3 = feols(age~dism1990 + factor(sex) + 
             educ +  datayear |
             fe_5year + statebth + marstat + occup + state_abbr , data = data[data$black == 0,], cluster = ~yob + name)

#IV 
# Three iv specifications 
 feols(age~1 | yob + state_abbr 
              | dism1990~herf, cluster = ~yob,data = data[data$black ==1 & data$N_years == 10,])

b_iv2 = feols(age~1 + factor(sex) + educ + pop1990 + pctbk1990 |
                yob + state_abbr
              | dism1990~herf,
              cluster = ~yob +name + state_abbr,data = data[data$black ==1,])

 feols(age~male +educ|
                yob + N_years + statebth + marstat + occup + state_abbr |dism1990~herf,
              cluster =~yob,data = data[data$black ==1,])
 feols(age~male +educ|
         yob + statebth + marstat + occup + state_abbr |dism1990~herf,
       cluster =~yob,data = data[data$black ==1 & data$N_years == 10,])

w_iv1 = feols(age~1 |
                fe_5year  + state_abbr + datayear
              | dism1990~herf,
              cluster = ~yob + name ,data = data[data$black ==0,])

w_iv2 = feols(age~1 + factor(sex) + educ|
                fe_5year + datayear + state_abbr
              | dism1990~herf,
              cluster = ~yob + name,data = data[data$black ==0,])

w_iv3 = feols(age~1 + factor(sex) + educ|
                fe_5year + statebth + marstat + occup + state_abbr + datayear
              | dism1990~herf,
              cluster = ~yob + name ,data = data[data$black ==0,])


msummary(list(b_iv1,b_iv2,b_iv3),stars = T)

msummary(list(w_iv1,w_iv2,w_iv3),stars = T)

#### working #####

data %<>% mutate(
  male = ifelse(sex == 1,1,0),
                         educ_bin = case_when(lesshs ==1 ~"Lesshs",
                                           hs == 1 ~ "HS",
                                           somecollege == 1~"somecoll",
                                           ba ==1 ~ "BA",
                                           ba_plus == 1 ~ "BA_p"))

# 1993 has the most complete msas 
data %>% filter(datayear == 1993) %>% distinct(name)


# -----------------------
# Educational covariates 
# -----------------------

# Less than HS Black 
# for all less than hs people segregation increases longevity, but for black and white individuals it is negative the more ed you get 
# spec inspired by casey breen https://github.com/caseybreen/homeownership_longevity/blob/main/code/07_homeownership_homevalue.Rmd


# WHAT IS GOING ON WITH EDUCATION? 

summary(feols(age~dism1990 | yob,data[data$black ==1,]))


data %>% group_by(datayear,male,black) %>% 
  summarise(E = mean(age)) %>% 
  ggplot(aes(datayear,E, color = factor(male))) + 
  geom_point() + 
  facet_wrap(~black)


educ_controls = feols(age~male + dism1990*fct_relevel(educ_bin,"Lesshs") |
                fe_5year + statebth + marstat + occup + state_abbr,
              cluster = ~yob + name ,data = data[data$black ==1 & data$datayear == "1990",])



## 2.1 Show the validity of the instrument 

# Reduced form 
summary(feols(age~herf,data, vcov = "white"))
summary(feols(age~herf + lenper,data, vcov = "white"))
summary(feols(age~herf,data[data$white == 1,], vcov = "white"))
summary(feols(age~herf,data[data$white == 0,], vcov = "white"))
# Y is associated with Z 

# First stage 

summary(feols(dism1990~herf,data,cluster = ~name))
# Subgroups
summary(feols(dism1990~herf,data[data$white ==1,],cluster = ~name))
summary(feols(dism1990~herf,data[data$white ==0,],cluster = ~name))




# Falsification 


fdata = data %>% select(herf,lenper,
                        dism1990,
                        area1910,
                        count1910,
                        ethseg10,
                        ethiso10,
                        black1910,
                        passpc,
                        black1920,
                        ctyliterate1920,
                        lfp1920,
                        ctytrade_wkrs1920,
                        ctymanuf_wkrs1920,
                        ctyrail_wkrs1920,
                        incseg) %>% unique()

f_1 = feols(dism1990~herf + lenper,vcov = "white",data = fdata)
f_2 = feols(area1910~herf + lenper,vcov = "white",data = fdata)
f_3 = feols(count1910~herf + lenper,vcov = "white",data = fdata)
f_4 = feols(ethseg10~herf + lenper,vcov = "white",data = fdata)
f_5 = feols(ethiso10~herf + lenper,vcov = "white",data = fdata)
f_6 = feols(black1910~herf + lenper,vcov = "white",data = fdata)
f_7 = feols(passpc~herf + lenper,vcov = "white",data = fdata)
f_8 = feols(black1920~herf + lenper,vcov = "white",data = fdata)
f_9 = feols(ctyliterate1920~herf + lenper,vcov = "white",data = fdata)
f_10 = feols(lfp1920~herf + lenper,vcov = "white",data = fdata)
f_11 = feols(ctytrade_wkrs1920~herf + lenper,vcov = "white",data = fdata)
f_12 = feols(ctymanuf_wkrs1920~herf + lenper,vcov = "white",data = fdata)
f_13 = feols(ctyrail_wkrs1920~herf + lenper,vcov = "white",data = fdata)
f_14 = feols(incseg~herf + lenper,vcov = "white",data = fdata)

msummary(list(f_1,
              f_2,
              f_3,
              f_4,
              f_5,
              f_6,
              f_7,
              f_8,
              f_9,
              f_10,
              f_11,
              f_12,
              f_13,
              f_14),stars = T, fmt = 2, title = "Falsification checks: Other City Characteristics")




