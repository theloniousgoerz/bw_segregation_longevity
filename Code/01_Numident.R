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
# The FIPS columns are correctly zero-padded 5-character strings in the source file
# ("01105"), but fread type-guesses them to integer and the leading zeros are lost.
# Reading them as character is what keeps them intact; without this, every downstream
# script has to guess which codes lost a zero.
geog =    fread(here("Data","Censoc","dataverse_files (2)","censoc_numident_geography_supplement_v1.csv"),
                colClasses = list(character = c("birth_fips","death_fips")))
census =  fread(here("Data","Census","usa_00006.csv"),header = T)
ddi = read_ipums_ddi(here("Data","Census","usa_00007.xml"))
controls = read_ipums_micro(ddi)


# Geog Processing 
`%notin%` = Negate(`%in%`)

# Both FIPS codes now arrive as the 5-character strings the source file stores, so no
# zero-padding step is needed. The block that used to sit here re-added a leading zero
# to BOTH codes whenever the death state was one of the single-digit-FIPS states, which
# was right for death_fips and wrong for birth_fips: whether a birth code needs a zero
# is a fact about the birth state, not the death state. That mismatch put roughly an
# eighth of all birth counties in the wrong state.
geog %<>% mutate(
  birth_fips = as.character(birth_fips),
  death_fips = as.character(death_fips),
  state      = str_sub(death_fips, 1, 2)
)


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
