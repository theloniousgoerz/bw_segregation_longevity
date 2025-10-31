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
library(stringr)
library(tibble)
options(scipen = 999)
# ------------------------------------------------# ------------------------------------------------
# Data for 1940 Analysis
# ------------------------------------------------
# Load 1940 data 
setwd("/Users/theloniousgoerz/Academic/Projects/QP/")
#census = read_csv("../Data/merged_data_1940_alt.csv")
# County information in 1940 
county_1940_rs_age <- read_csv("./Data/nhgis0015_csv/nhgis0015_ds77_1940_county.csv")
county_1940_total_pop = read_csv("./Data/nhgis0015_csv/nhgis0015_ds224_1940_county.csv")
tract_1940_race <- read_csv("./Data/nhgis0015_csv/nhgis0015_ds76_1940_tract.csv")
bpop_1940 = read_csv("/Users/theloniousgoerz/Academic/Projects/QP/Data/nhgis0025_csv/nhgis0025_ds78_1940_county.csv")

#https://www.kaggle.com/datasets/wbdill/us-county-landmass?resource=download
landarea = read_csv("./Data/county_landmass.csv")

nchs2fips_county1990 <- read_csv("./Data/nchs2fips_county1990.csv")
cbsa2005 = read_csv("./Data/cbsatocountycrosswalk2005.csv")

## Rural Urban Codes 
urb_rural_8393 = read_excel("/Users/theloniousgoerz/Academic/Projects/QP/Data/Rural_Urban_codes/cd8393.xls")
urb_rural_03 = read_excel("/Users/theloniousgoerz/Academic/Projects/QP/Data/Rural_Urban_codes/ruralurbancodes2003.xls")



#nhgis0017_ts_nominal_tract <- read.csv("./Data/nhgis0017_csv/nhgis0017_ts_nominal_tract copy.csv")  %>% 
#  rename(GEOID = GISJOIN,
#         county = COUNTYNH,
#         state = STATENH,
#         pop = AV0AA,
#         nh_white = B18AA,
#         nh_black = B18AB,
#         w_Under5 = AC4AA,
#         w_5to14   = AC4AB,
#         w_15to59  = AC4AC,
#         w_60to64  = AC4AD,
#         w_65p = AC4AE,
#         b_Under5 = AD6AA,
#         b_5to14   = AD6AB,
#         b_15to59  = AD6AC,
#         b_60to64  = AD6AD,
#         b_65p = AD6AE) %>%   mutate(
#           STATEA = str_sub(state,1,2),
#           COUNTYA = str_sub(county,1,3),
#           STATEA =as.numeric(STATEA)) 
#write_csv(nhgis0017_ts_nominal_tract,"./Data/nhgis0017_csv/tractdata.csv")
# ------------------------------------------------# ------------------------------------------------
# ------------------------------------------------# ------------------------------------------------
# County information in 1990
# County data 1980-2000
----------#
  ## Death County Data 
  # 1980-2000
  # ------------------#
  
  tractdata = read.csv("./Data/nhgis0017_csv/tractdata.csv") %>% 
  mutate(death_fips = case_when(STATEFP  < 10 & COUNTYFP < 10 ~ paste0("0",STATEFP,"00",COUNTYFP),
                                STATEFP  < 10 & COUNTYFP >= 10 & COUNTYFP <100 ~ paste0("0",STATEFP,"0",COUNTYFP),
                                STATEFP  >=10 & COUNTYFP < 10~ paste0(STATEFP,"00",COUNTYFP),
                                STATEFP  >=10 & COUNTYFP >= 10 & COUNTYFP <100~ paste0(STATEFP,"0",COUNTYFP),
                                STATEFP  >=10 & COUNTYFP >=100~ paste0(STATEFP,COUNTYFP),
                                STATEFP  <10 & COUNTYFP >=100~ paste0("0",STATEFP,COUNTYFP)
  )) 

pop_char = 
  tractdata %>% 
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

county_d <- tractdata %>% 
  group_by(YEAR,death_fips) %>% 
  mutate(co_black = sum(nh_black,na.rm = T),
         co_white = sum(nh_white,na.rm = T),
         d = abs(nh_black /co_black - nh_white/co_white)) %>%
  summarise(county_dism = .5*sum(d,na.rm = T)) %>% distinct()



county_d %<>% mutate(Year4 = YEAR, FIPS_Combined = as.character(death_fips)) %>% left_join(.,pop_char) %>% distinct()


### cunstruct 1940-1970 bp growth 

bpop_1940_df = bpop_1940 %>% rename(bpop_40 = BYA003) %>% 
  mutate(death_fips = paste0(str_sub(STATEA,1,2),str_sub(COUNTYA,1,3))) %>% select(death_fips,bpop_40)

b_growth = 
  tractdata %>% filter(YEAR == 1970) %>% 
  group_by(death_fips) %>%
  mutate(bpop_70 = sum(nh_black,na.rm = T),
         wpop_70 = sum(nh_white,na.rm = T),
         pop_70 = sum(pop,na.rm=T)
  )  %>% 
  distinct(death_fips,wpop_70,bpop_70,pop_70) %>%
  left_join(.,bpop_1940_df, by = "death_fips") %>% 
  mutate(
    b_increase = bpop_70-bpop_40,
    b_growth = b_increase/pop_70)



county_d %<>% left_join(.,b_growth, by = "death_fips") 

## Rural Urban Codes ##
# https://www.ers.usda.gov/data-products/rural-urban-continuum-codes

urb_rural_8393 %<>% select(
  death_fips = FIPS,
  urb_83 = `1983 Rural-urban Continuum Code`
)



urb_rural = urb_rural_03 %>% select(death_fips = `FIPS Code`,urb_93 = `1993 Rural-urban Continuum Code`,urb_03 = `2003 Rural-urban Continuum Code`,
                        desc_03 = `Description for 2003 codes`) %>% 
  left_join(.,urb_rural_8393, by = "death_fips")

county_d %<>% left_join(.,urb_rural, by = "death_fips") %>% 
  mutate(urban_rural_code = case_when(
    YEAR== 1980 ~ urb_83, 
    YEAR== 1990 ~ urb_93,
    YEAR== 2000 ~ urb_03
  ),
  urban_rural_desc = case_when(
    urban_rural_code == 1 ~ "Metro 1M or more",
    urban_rural_code == 2 ~ "Metro 250k-1M",
    urban_rural_code == 3 ~ "Metro <250k",
    urban_rural_code == 4 ~ "Non-Metro Urban 20k adj",
    urban_rural_code == 5 ~ "Non-Metro Urban 20k nadj",
    urban_rural_code == 6 ~ "Urban 5-20k adj",
    urban_rural_code == 7 ~" Urban 5-20k nadj",
    urban_rural_code == 8 ~ "Urban <5k adj",
    urban_rural_code == 9 ~ "Urban <5k nadj"
    
  ),
  urban_d = ifelse(urban_rural_code %in% c(1:5),"Urban","Rural")
  
  ) 
## Land Area 

landarea %<>% mutate(death_fips = FIPS)

county_d %<>% left_join(.,landarea) %>% 
  mutate(pop_dens = pop/land_sq_mi) %>% ungroup()

# ##############################################################################################################
### MSAs ---------------------------------------------
# use 1990 and 2005 delineartions. accept 2 years of error 1988-90
# ##############################################################################################################

### 1990 fips 

nchs2fips_county1990 %<>% mutate(death_fips =fipsco)

fips_80 = nchs2fips_county1990 %>% select(death_fips,fips_msa) %>% mutate(YEAR = 1980)
fips_90 = nchs2fips_county1990 %>% select(death_fips,fips_msa) %>% mutate(YEAR = 1990)

fips_00 = cbsa2005 %>% select(death_fips = fipscounty,fips_msa = msa) %>% 
  mutate(YEAR = 2000)

msa_names = cbsa2005 %>% select(fips_msa = msa,msaname) %>% distinct() %>% filter(nchar(fips_msa) == 4)

msa = rbind(fips_00,fips_80,fips_90)

msa %<>% filter(nchar(fips_msa) == 4) %>% 
  left_join(.,msa_names)

# ##############################################################################################################
# Combine data to merge 
# ##############################################################################################################

county_d %<>% left_join(msa, by = c("death_fips","YEAR")) 

write_csv(county_d,"./Data/_Cleaned/county_data.csv")