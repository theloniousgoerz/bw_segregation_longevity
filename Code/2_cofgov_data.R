#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Title: Census of Governments  
# Thelonious Goerz 
# Date: 
# https://my.willamette.edu/site/mba/public-datasets
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

setwd("/Users/theloniousgoerz/Academic/Projects/QP/")

data = read_csv("./Data/GFD/Government Finance Database Municipal Data/MunicipalData.csv")
data_t = read_csv("./Data/GFD/Government Finance Database Township Data/TownshipData.csv")


data %>% filter(Year4 == 1967) %>% view()

 c_of_gov_m = 
  data %>% filter(Year4 %in% c(1967) & Name != "NOT AVAILABLE") %>% 
  group_by(State_Code,FIPS_Combined) %>% 
  summarise(n_municipal_gov = n(),
            municipal_revenue =  sum(Total_Fed_IG_Revenue,na.rm = T),
            total_revenue_municipal = sum(Total_Revenue,na.rm = T))

c_of_gov_t = data_t %>% filter(Year4 %in% c(1967) & Name != "NOT AVAILABLE") %>% 
  group_by(State_Code,FIPS_Combined) %>% 
  summarise(n_township_gov = n(),
            township_revenue =  sum(Total_Fed_IG_Revenue,na.rm = T),
            total_revenue_town = sum(Total_Revenue,na.rm = T))

# merge 
# gov_rev_share 
gr_state = c_of_gov_m %>% left_join(.,c_of_gov_t, by = c("FIPS_Combined","State_Code")) %>% 
  group_by(State_Code) %>% 
  summarise(s_town = sum(township_revenue,na.rm = T),
            s_municipal = sum(municipal_revenue,na.rm = T),
            t_town = sum(total_revenue_town,na.rm = T),
            t_municipal = sum(total_revenue_municipal,na.rm = T)) %>% ungroup() %>%
  rowwise() %>% mutate(gov_rev_share_state = sum(s_town,s_municipal,na.rm = T) /sum(t_town,t_municipal,na.rm = T)) %>% ungroup() %>% 
  select(State_Code,gov_rev_share_state)

c_of_gov_m %<>% left_join(.,c_of_gov_t, by = c("FIPS_Combined","State_Code")) %>% 
  rowwise() %>%
  # create combined measure. 
  mutate(n_governments = sum(n_municipal_gov,n_township_gov,na.rm = T),
         revenue_share = sum(township_revenue,municipal_revenue,na.rm = T),
         total = sum(total_revenue_town,total_revenue_municipal,na.rm = T),
         gov_rev_share = (revenue_share/total)) %>% ungroup() %>% 
  select(-c(n_township_gov,township_revenue,total_revenue_town)) %>% 
  left_join(.,gr_state) %>% select(-State_Code)

c_of_gov_m %<>% rename(death_fips = FIPS_Combined)

# Save 
write_csv(c_of_gov_m,"./Data/_Cleaned/CensusGov.csv")
