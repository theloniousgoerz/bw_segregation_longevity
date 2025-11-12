#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Title: NUMIDENT Construction 
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
library(ipumsr)
options(scipen = 999)

censoc =  fread(here("Data","Censoc","dataverse_files (2)", "censoc_numident_v3.csv"))
geog =    fread(here("Data","Censoc","dataverse_files (2)","censoc_numident_geography_supplement_v1.csv"))
census =  fread(here("Data","Census","usa_00006.csv"),header = T)
ddi = read_ipums_ddi(here("Data","Census","usa_00007.xml"))
controls = read_ipums_micro(ddi)


# Geog Processing 
`%notin%` = Negate(`%in%`)

geog %<>% mutate(
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
                   state %notin% c("1",
                                   "2",
                                   "4",
                                   "5",
                                   "6",
                                   "8",
                                   "9") ~ as.character(death_fips)
                 )) 


# 1940 data
# Filter censoc to only matching numident then merge 

censoc %<>% filter(HISTID %in% geog$HISTID)

census %<>% filter(HISTID %in% censoc$HISTID)

t = census %>% left_join(.,censoc, by = "HISTID")

t %<>% left_join(.,geog, by = "HISTID")

t %<>% mutate(death_decade = 
                case_when(dyear %in% 1980:1990 ~ 1980,
                          dyear %in% 1990:2000 ~ 1990,
                          dyear %in% 2000:2010 ~ 2000))

t %<>% mutate(FIPS_Combined = death_fips, Year4 = death_decade)

write_csv(t,here("Data","_Cleaned","NUMIDENT.csv"))

### 1940 Controls 
controls_m = controls %>% filter(HISTID %in% t$HISTID)
rm(controls)
write.csv(controls_m,file = here("Data","_Cleaned","controls.csv"))
