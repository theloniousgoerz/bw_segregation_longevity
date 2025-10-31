#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Title: County-Level Data 
# Thelonious Goerz 
# Date: 
rm(list = ls())
library(tidyverse)
library(readr)
library(readxl)
library(data.table)
library(haven)
library(magrittr)
library(tidycensus)
library(ipumsr)
library(fixest)
library(segregation)
options(scipen = 999)
# ------------------------------------------------# ------------------------------------------------
# Census Data 
# ------------------------------------------------# ------------------------------------------------
## Rural Urban Codes 
urb_rural_8393 = read_excel("./Data/Rural_Urban_codes/cd8393.xls")
urb_rural_03 = read_excel("./Data/Rural_Urban_codes/ruralurbancodes2003.xls")

nhgis0017_ts_nominal_tract <- read.csv("./Data/Census/nhgis0017_csv/nhgis0017_ts_nominal_tract copy.csv")  %>% 
 rename(GEOID = GISJOIN,
                county = COUNTYNH,
                state = STATENH,
                pop = AV0AA,
                nh_white = B18AA,
                nh_black = B18AB,
                w_Under5 = AC4AA,
                w_5to14   = AC4AB,
                w_15to59  = AC4AC,
                w_60to64  = AC4AD,
                w_65p = AC4AE,
                b_Under5 = AD6AA,
                b_5to14   = AD6AB,
                b_15to59  = AD6AC,
                b_60to64  = AD6AD,
                b_65p = AD6AE) 

# ------------------------------------------------# ------------------------------------------------
# County information in 1990
# County data 1980-2000
## Death County Data 
# 1980-2000
# ------------------------------------------------# ------------------------------------------------
tract_d = nhgis0017_ts_nominal_tract

tract_d %<>% 
  mutate(
    STATEA = str_sub(state,1,2),
    COUNTYA = str_sub(county,1,3),
    STATEA =as.numeric(STATEA),
    death_fips = case_when(STATEFP  < 10 & COUNTYFP < 10 ~ paste0("0",STATEFP,"00",COUNTYFP),
                           STATEFP  < 10 & COUNTYFP >= 10 & COUNTYFP <100 ~ paste0("0",STATEFP,"0",COUNTYFP),
                           STATEFP  >=10 & COUNTYFP < 10~ paste0(STATEFP,"00",COUNTYFP),
                           STATEFP  >=10 & COUNTYFP >= 10 & COUNTYFP <100~ paste0(STATEFP,"0",COUNTYFP),
                           STATEFP  >=10 & COUNTYFP >=100~ paste0(STATEFP,COUNTYFP),
                           STATEFP  <10 & COUNTYFP >=100~ paste0("0",STATEFP,COUNTYFP)
                           )
  ) 

# ------------------------------------------------# ------------------------------------------------# ------------------------------------------------
### Calculate D ###
county_d <- tract_d %>% 
  group_by(YEAR, STATE,death_fips,COUNTY) %>%
  mutate(
    co_black = sum(nh_black, na.rm = TRUE),
    co_white = sum(nh_white, na.rm = TRUE),
    d = abs(nh_black / co_black - nh_white / co_white),
    isob=(nh_black/co_black) * nh_black/(nh_white + nh_black)
  ) %>%
  summarise(county_dism = 0.5 * sum(d, na.rm = TRUE), .groups = "drop",
            county_isolb = sum(isob,na.rm = T))

# ------------------------------------------------# ------------------------------------------------# ------------------------------------------------
### Calculate D*

D_star = tract_d %>% 
 distinct(YEAR, STATE,death_fips,COUNTY,nh_black,nh_white, GEOID) %>% 
  pivot_longer(names_to = "group",values_to = "weight",
               cols = c(nh_black,nh_white)) %>% 
  group_by(YEAR,death_fips) %>%
  # Filter to counties where we have unit-level variation
  mutate(n = max(row_number())) %>%
  filter(n>2) %>%
  group_modify(~dissimilarity(
    data = ., 
    group = "group",
    unit = "GEOID",
    weight = "weight",
    se = TRUE
  ))

# ------------------------------------------------# ------------------------------------------------# ------------------------------------------------
D_star %<>% select(YEAR, death_fips,D_star = est,se, bias)

### Calculate H ###
County_H = tract_d %>% 
  select(GEOID,death_fips,
         nh_white,nh_black,YEAR,TRACTA) %>% 
  pivot_longer(names_to = "race",values_to = "count", cols = c(nh_white,nh_black)) %>%
  mutual_within(
    data = .,
    group = "race",
    unit = "TRACTA",
    weight = "count",
    within = c("death_fips","YEAR"),
    wide = TRUE
  ) %>% select(death_fips,YEAR,H_bw = H)

county_d %<>% left_join(County_H)  %>% 
  left_join(D_star)
# ------------------------------------------------# ------------------------------------------------# ------------------------------------------------
pop_char = 
  tract_d %>% 
  group_by(YEAR,death_fips) %>% 

  summarise(
    pop = sum(pop, na.rm = T),
    nh_white = sum(nh_white,na.rm = T),
    nh_black = sum(nh_black,na.rm = T),
    w_Under5 = sum(w_Under5,na.rm = T),
    w_5to14 = sum(w_5to14,na.rm = T),
    w_15to59 = sum(w_15to59,na.rm = T),
    w_60to64 = sum(w_60to64,na.rm = T),
    w_65p = sum(w_65p,na.rm = T),
    b_Under5 = sum(b_Under5,na.rm = T),
    b_5to14 = sum(b_5to14,na.rm = T),
    b_15to59 = sum(b_15to59,na.rm = T),
    b_60to64 = sum(b_60to64,na.rm = T),
    b_65p = sum(b_65p,na.rm = T), 
    death_decade = YEAR,
    FIPS_Combined = as.character(death_fips)
    
  ) %>% distinct()

county_d %<>% mutate(Year4 = YEAR, 
                     FIPS_Combined = as.character(death_fips)) %>% 
  left_join(.,pop_char) %>% distinct()

# ------------------------------------------------# ------------------------------------------------# ------------------------------------------------
### cunstruct 1940-1970 bp growth 

bpop_1940_df = bpop_1940 %>% rename(bpop_40 = BYA003) %>% 
  mutate(death_fips = paste0(str_sub(STATEA,1,2),str_sub(COUNTYA,1,3))) %>% 
  select(death_fips,bpop_40)

tract_d %>% select(COUNTYFP,STATEFP,death_fips) %>% head()
 b_growth = 
tract_d %>% filter(YEAR == 1970) %>% 
 mutate(death_fips = paste0(STATEFP,COUNTYFP)) %>%
  group_by(death_fips) %>%
   mutate(bpop_70 = sum(nh_black,na.rm = T),
          wpop_70 = sum(nh_white,na.rm = T),
          pop_70 = sum(pop,na.rm=T)
                                  )  %>% 
             distinct(death_fips,wpop_70,bpop_70,pop_70) %>%
  left_join(.,bpop_1940_df, by = "death_fips") %>% 
  mutate(
         b_increase = bpop_40-bpop_70,
         b_growth = b_increase/pop_70)

 
county_d %<>% left_join(.,b_growth, by = "death_fips") 
write_csv(county_d,"./Data/_Cleaned/county_data.csv")
