# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Title: Data Cleaning and Summary Statistics
# Thelonious Goerz 
# Purpose: This file cleans all necessary data for analysis files and creates summary statistics, and descriptive plots. 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Packages
rm(list=ls())
library(tidyverse)
library(magrittr)
library(readr)
library(modelsummary)
library(kableExtra)
library(gt)
library(cowplot)
library(fixest)

# Options

setwd("/Users/theloniousgoerz/Academic/Projects/QP/Analysis/")

my_ftest <- function(modc, modnc)
{
  df_dif <- (degrees_freedom(modc, type="resid") - degrees_freedom(modnc, type="resid"))
  df_nc <- degrees_freedom(modnc, type="resid")
  fstat <- ((modc$ssr - modnc$ssr) / df_dif) / (modnc$ssr / df_nc)
  pvf <- pf(fstat, df_dif, df_nc, lower.tail = FALSE)
  print(paste(paste("The F-statistic is", fstat, sep=" "), paste("and the p-value is", pvf, sep=" "), sep=" "))
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Read Data 
controls = read_csv("../Data/_Cleaned/controls.csv")
numident = read_csv("../Data/_Cleaned/NUMIDENT.csv")
county_d = read_csv("../Data/_Cleaned/county_data.csv")
gov = read_csv("../Data/_Cleaned/CensusGov.csv")
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Clean data
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# NUMIDENT

data = numident %<>% left_join(.,controls) 
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


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# County
county_d %<>% mutate(death_fips = as.character(death_fips))
gov %<>% mutate(has_gov_info = 1)

county = county_d %<>% left_join(., gov,by = c("death_fips")) 

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

data %<>% left_join(.,metro_codes) %>% 
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

# Save 
write_csv(county, "../Data/_Cleaned/county.csv")
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Data Join
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Data Description 
#1988-1924

data_a = data %>% filter(byear %in% 1905:1924 & 
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
# merge sibs
data_a %<>% left_join(sibs)

data_as = data_a %>% filter(!is.na(sib_group_id_exact))

data %<>% mutate(iv_sample = ifelse(HISTID %in% data_a$HISTID,"IV Sample","Non-IV Sample"),
                    sibling_sample = ifelse(HISTID %in% data_as$HISTID,"Sibling Sample","Non-Sibling Sample"))


fulldata = data %>% 
  filter(byear %in% 1905:1924 & 
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
write_csv(fulldata, "../Data/_Cleaned/fulldata.csv")
# Save 
write_csv(data_a, "../Data/_Cleaned/analytic_sample.csv")

rm(data)
rm(data_a)
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# BUNMD 
b = bunmd %>% filter(death_fips %in% county$death_fips)

county %<>% mutate(death_fips = as.numeric(death_fips))

b %<>% left_join(.,county, by = c("death_decade","death_fips"))

b %<>% filter(!is.na(county_dism))

b %<>% left_join(.,south_fips,by = c("STATEFIP"))

# Save 
write_csv(b, "../Data/_Cleaned/bunmd_clean.csv")

