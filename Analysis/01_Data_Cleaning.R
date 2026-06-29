# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Title: Data Cleaning and Summary Statistics
# Thelonious Goerz 
# Purpose: This file cleans all necessary data for analysis. 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Packages
rm(list=ls())
library(tidyverse)
library(magrittr)
library(readr)
library(modelsummary)
library(kableExtra)
library(gt)
library(readxl)
library(cowplot)
library(fixest)
library(here)
`%notin%` = Negate(`%in%`)
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Read Data 
controls = read_csv(here("Data","_Cleaned","controls.csv"))
numident = read_csv(here("Data","_Cleaned","NUMIDENT.csv"))
county_d = read_csv(here("Data","_Cleaned","county_data.csv"))
gov =      read_csv(here("Data","_Cleaned","CensusGov.csv"))
## Rural Urban Codes 
urb_rural_8393 = read_excel(here("Data","Rural_Urban_codes","cd8393.xls"))
urb_rural_03 =   read_excel(here("Data","Rural_Urban_codes","ruralurbancodes2003.xls"))
# Sibling identifiers 
sbs =      read_csv(here("Data","censoc_numident_siblings_v2","censoc_numident_sibs_exact_match_v2.csv"))
sbs_f =    read_csv(here("Data","censoc_numident_siblings_v2","censoc_numident_sibs_flexible_match_v2.csv"))
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Clean data
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# NUMIDENT
data = numident %<>% 
  left_join(.,controls) 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# RUC Codes 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 urb_codes =
   urb_rural_03 %>% 
   mutate(FIPS = `FIPS Code`) %>%
   left_join(urb_rural_8393, by = "FIPS") %>%
   rename(death_fips = FIPS,
          code_93 = `1993 Rural-urban Continuum Code.y`,
          code_00 = `2003 Rural-urban Continuum Code`,
          code_83 = `1983 Rural-urban Continuum Code` ) %>%
   pivot_longer(names_to = "year",
                values_to = "urb_code",cols = starts_with("code")) %>% 
   select(death_fips,year,urb_code) %>% 
     mutate(death_decade = case_when(
       year == "code_00" ~ 2000,
       year == "code_93" ~ 1990,
       year == "code_83" ~ 1980
     )) %>% 
     mutate(urb_code = case_when(death_decade %in% 1980:1990 & urb_code %in% 0:1 ~ 1,
                                  death_decade == 2000 & urb_code == 1 ~ 1,
                                  urb_code == 2 ~ 2,
                                  urb_code == 3 ~ 3,
                                  urb_code == 4 ~ 4,
                                  urb_code == 5 ~ 5,
                                  urb_code == 6 ~ 6,
                                  urb_code == 7 ~ 7,
                                  urb_code == 8 ~ 8,
                                  urb_code == 9 ~ 9))

 county_d %<>% 
   left_join(urb_codes,by = c("death_fips","death_decade")) 

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Clearn Fips 
data %<>% mutate(
  death_fips = as.character(death_fips),
  state = ifelse(death_state %in%  c(
    "al",
    "ak",
    "az",
    "ar",
    "ca",
    "co",
    "ct"
  ),str_sub(death_fips,1,1),str_sub(death_fips,1,2)),
  birth_fips = case_when(
    
    state %in% c(     "1",
                      "2",
                      "4",
                      "5",
                      "6",
                      "8",
                      "9") ~paste0("0",birth_fips),
    state %notin% c(     "1",
                         "2",
                         "4",
                         "5",
                         "6",
                         "8",
                         "9")  ~ as.character(birth_fips)
  ),
  death_fips = case_when(
    state %in% c(      "1",
                       "2",
                       "4",
                       "5",
                       "6",
                       "8",
                       "9") ~paste0("0",as.character(death_fips)),
    state %notin% c("1","2", "4","5", "6", "8", "9") ~ as.character(death_fips)
  )) 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# County
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
county_d %<>% mutate(death_fips = as.character(death_fips))
gov %<>% mutate(has_gov_info = 1)

county = county_d %<>% 
  left_join(., gov,by = c("death_fips")) 

## Define South Samples for birth and death ## 
south_fips = data.frame(
  STATEFIP =as.character( c("01","05","10",11,12,13,21,22,24,28,37,40,45,47,48,51,54)),
  south_sample = 1
)
south_fips_b = data.frame(
  STATEFIP_b =as.character( c("01","05","10",11,12,13,21,22,24,28,37,40,45,47,48,51,54)),
  south_sample_b = 1
)
## Merge south on to County ## 
county %<>% mutate(STATEFIP = as.character(str_sub(death_fips,1,2)), death_fips = as.character(death_fips)) %>% 
  left_join(.,south_fips)

## Merge on data_a with fips ## 
data %<>% left_join(.,county,by = c("death_fips","death_decade"))

## Merge on data_a with sibling identifiers ##
data %<>% left_join(.,sbs,by = c("HISTID")) %>% left_join(.,sbs_f,by = c("HISTID"))



data %<>% 
  mutate(migrated = ifelse(as.character(birth_fips) == death_fips,"Migrated","Did Not Migrate"),
         STATEFIP_b = as.character(str_sub(birth_fips,1,2))) %>% 
  left_join(south_fips_b, by = "STATEFIP_b")  %>% 
  # Place of Birth South Indicator # 
  mutate(born_in_south = factor(case_when(south_sample_b == 1~"Born South",
                                                    is.na(south_sample_b) ~ "Not Born South")),
                   born_in_south = fct_relevel(born_in_south,"Not Born South")
)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# create 
# Compare sample v non sample 
county %<>% mutate(sample_counties = ifelse(death_fips %in% data$death_fips,"In Sample","Out of Sample"))

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Data Join
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

data_a = data %>% filter(byear %in% 1905:1920 & 
                           sex %in% 1:2 &
                           HISPAN == 0 &
                           RACE %in% 1:2 & 
                           !is.na(weight)) %>% 
  mutate(ln_gov = ifelse(n_governments == 0, log(0.01),log(n_governments)),
         Race = ifelse(RACE == 1, "White","Black"),
         pblack = nh_black/pop,
         south = case_when(south_sample == 1~1,
                           is.na(south_sample)~0),
         male = ifelse(sex ==1,1,0),
         educ_years = case_when(
           # Educ is the mean of each bracket 
           EDUC == 99 ~ NA,
           EDUC == 0 ~ 0,
           EDUC == 1 ~ 3,
           EDUC == 2 ~ 6.5,
           EDUC == 3 ~ 9, 
           EDUC == 4 ~ 10,
           EDUC == 5 ~ 11, 
           EDUC == 6 ~ 12,
           EDUC == 7 ~ 13,
           EDUC == 8 ~ 14,
           EDUC == 9 ~ 15,
           EDUC == 10 ~ 16,
           EDUC == 11 ~ 17
         ),
         hs = ifelse(educ_years >=12,1,0),
         married = case_when(MARST %in% 1:2 ~ 1,
                             MARST %in% 3:8 ~0),
         ownhome = ifelse(OWNERSHP ==1,1,0),
         employed = case_when(EMPSTAT == 1 ~ 1,
                              EMPSTAT == 2 ~ 0)) %>% 
           filter(!is.na(pblack) & 
                    !is.na(educ_years) & 
                    !is.na(married) & !is.na(ownhome) & 
                    !is.na(county_dism)
                  )


fulldata = data %>% 
  filter(byear %in% 1905:1920 & 
           sex %in% 1:2 &
           HISPAN == 0 &
           RACE %in% 1:2 & 
           !is.na(weight)) %>% 
  
  mutate(
         Race = ifelse(RACE == 1, "White","Black"),
         pblack = nh_black/pop,
         south = case_when(south_sample == 1~1,
                           is.na(south_sample)~0),
         male = ifelse(sex ==1,1,0),
         educ_years = case_when(
           # Educ is the mean of each bracket 
           EDUC == 99 ~ NA,
           EDUC == 0 ~ 0,
           EDUC == 1 ~ 3,
           EDUC == 2 ~ 6.5,
           EDUC == 3 ~ 9, 
           EDUC == 4 ~ 10,
           EDUC == 5 ~ 11, 
           EDUC == 6 ~ 12,
           EDUC == 7 ~ 13,
           EDUC == 8 ~ 14,
           EDUC == 9 ~ 15,
           EDUC == 10 ~ 16,
           EDUC == 11 ~ 17
         ),
         black = ifelse(Race == "Black",1,0),
         hs = ifelse(educ_years >=12,1,0),
         married = case_when(MARST %in% 1:2 ~ 1,
                             MARST %in% 3:8 ~0),
         ownhome = ifelse(OWNERSHP ==1,1,0),
         employed = case_when(EMPSTAT == 1 ~ 1,
                              EMPSTAT == 2 ~ 0))



# Save 
write_csv(fulldata, here("Data","_Cleaned","fulldata.csv"))
write_csv(data_a,   here("Data","_Cleaned","analytic_sample.csv"))
write_csv(county,   here("Data","_Cleaned","county.csv"))


