# BUNMD Processing 
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
options(scipen = 999)

bunmd_geog = fread("/Users/theloniousgoerz/Academic/Projects/QP/Data/BUNMD/bunmd_geography_supplement_v1.csv")
bunmd = fread("/Users/theloniousgoerz/Academic/Projects/QP/Data/BUNMD/bunmd_v2/bunmd_v2.csv")
sibs = fread("/Users/theloniousgoerz/Academic/Projects/QP/Data/BUNMD/bunmd_siblings_v2/bunmd_sibs_exact_match_v2.csv")
sibsf = fread("/Users/theloniousgoerz/Academic/Projects/QP/Data/BUNMD/bunmd_siblings_v2/bunmd_sibs_flexible_match_v2.csv")

# Filter BUNMD to Geographic supplement IDs 
bunmd %<>% filter(ssn %in% bunmd_geog$ssn) 

bunmd_geog %<>% left_join(.,bunmd, by = c("ssn")) %>% 
  mutate(death_decade = case_when(dyear %in% 1980:1990 ~ 1980,
                                  dyear %in% 1990:2000 ~ 1990,
                                  dyear %in% 2000:2010 ~ 2000))


bunmd_geog %<>% left_join(.,sibs, by = "ssn")
bunmd_geog %<>% left_join(.,sibsf, by = "ssn")

bunmd_geog %<>% filter(race_last %in% 1:2 & !is.na(death_fips))


write_csv(bunmd_geog,"./Data/_Cleaned/BUNMD.csv")