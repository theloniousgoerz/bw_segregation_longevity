######################################################################################################################################################
# 01 Data Construction 
  # Data construction for my qualifying paper 
# 6/7/24
######################################################################################################################################################
rm(list = ls())
library(tidyverse)
library(readr)
library(readxl)
library(data.table)
library(haven)
library(magrittr)
library(tidycensus)
library(ipumsr)
options(scipen = 999)

######################################################################################################################################################
# Load Data 
# Create Crosswalk
######################################################################################################################################################
# County information 
#countyrace <- read_csv("Data/nhgis0014_csv/nhgis0014_ds120_1990_tract.csv")
setwd("/Users/theloniousgoerz/Academic/Projects/QP/")
# MSA Identifiers 
X1990_ccc <- read_excel("./Data/ICPSR_38765 2/DS0001/38765-0001-Zipped_package/1990_ccc.xlsx")
name_fips_cw <- read_excel("Data/name_fips_cw.xlsx")
# Fips crosswalk dept of labor 
county_msa_cw = read_excel("Data/effective_may_16_2004_county_and_state_fips.xlsx", sheet = 2)

name_fips_cw
# # Mortality data 1990-99
 mort = read_csv("./Data/mort1990.csv")
# mort_91 = read_csv("./Data/mort1991.csv")
# mort_92 = read_csv("./Data/mort1992.csv")
# mort_93 = read_csv("./Data/mort1993.csv")
# mort_94 = read_csv("./Data/mort1994.csv")
# mort_95 = read_csv("./Data/mort1995.csv")
# mort_96 = read_csv("./Data/mort1996.csv")
# mort_97 = read_csv("./Data/mort1997.csv")
# mort_98 = read_csv("./Data/mort1998.csv")
# mort_99 = read_csv("./Data/mort1999.csv")

# Instrument
ananat = read_dta("./Data/aej_maindata.dta")

nchs2fips_county1990 <- read_csv("Data/nchs2fips_county1990.csv")

name_fips_cw %<>% mutate(name = str_sub(name,start = 2,end =-2))
######################################################################################################################################################
# 2004 crosswalk 

#inst = ananat %>% left_join(.,X1990_ccc) %>% mutate(msa = as.character(msafips))
#
#inst %<>% filter(!is.na(county_fips)) %>% mutate(death_fips = county_fips)
#
#inst %<>% left_join(.,county_msa_cw) %>% filter(!is.na(county_fips)) %>% mutate(death_fips = county_fips)

######################################################################################################################################################
######################################################################################################################################################
# Instrument 
######################################################################################################################################################
# MSA crosswalk 
ananat %<>% left_join(.,X1990_ccc, by = "name") %>% mutate(msafips = as.character(msafips))

ananat %<>% 
  mutate(
         state_abbr = str_to_title(toupper(str_sub(name,-2,-1))), 
         time = 1990) 

ananat %<>% mutate(fipspmsa = msafips)

ananat %<>% 
  mutate(fipspmsa = case_when(nchar(fipspmsa) == 2 ~ paste0("00",fipspmsa,sep = ""),
                              nchar(fipspmsa) ==1 ~ paste0("000",fipspmsa,sep = ""),
                              nchar(fipspmsa) ==3 ~ paste0("0",fipspmsa,sep = ""),
                              nchar(fipspmsa) > 3 ~ fipspmsa)) 


### FIX ANANAT FIPS with NCHS fips when applicable 
# Some of the MSA codes from the CGV data are not correct to the NBER mort MSAs. 
# Need to Check this 

nchs_msa=data.frame(
  name = c(
    "burlinvt",
    "elmirany",
    "grandfnd",
    "iowaciia",
    "manchenh",
    "newlonct",
    "pittsfma",
    "portlame",
    "portsmnh",
    "springma",
    "waterbct"
  ),
# MSA identifier compatible from the mort file 
  msa_fips_m = c("1303","2335","2985","3500","3740","4763","5523","6323","6403","6453","8003")
)

# NCHS identified areas 
# These are places that cannot be identified by the MSA but can be by NCHS codes 

# I should change the county to be the fips version 
  # more robust over time but the city code may be suscetible 
name_nchs_id_cw = data.frame(name = c("bridgect","fallrima","fitchbma","hartfoct","kankakil","lawrenma","newbedma","newhavct","norwalct","salemma","worcesma"),
           fipsctyr = c("09001","25005","25027","09003","17091","25009","25005","09009","09001","25009","25027"),
           cityrs = c("003","028","029","015","999","037","054","022","026","069","095"),
           metro = c(1,1,1,1,1,1,1,1,1,1,1))

# Merge identifiers to ananat 


ananat %<>% left_join(.,nchs_msa, by = "name")
ananat %<>% left_join(.,name_nchs_id_cw, by = "name")
######################################################################################################################################################
# Mortality data 
# Documentation https://data.nber.org/mortality/1990/dt90icd9.pdf
######################################################################################################################################################

# FIPSPMSA identifier in data refers to the PMSA of residence 
# All of the other identifiers are not FIPS but use the NCHS proprietary ids 

# Make unique identifier for both datasets to facilitate merging

unique_id = ananat %>% select(name,fipspmsa)

county_msa_cw %<>% mutate(fipspmsa = msa)

ananat %<>% left_join(.,county_msa_cw, by = c("fipspmsa")) %>% filter(!is.na(county_fips))



# # create unique identifier for name
# 
# unique_id %<>% left_join(.,nchs_msa) %>% 
#   mutate(ID = ifelse(is.na(msa_fips_m),fipspmsa,msa_fips_m)) %>% left_join(.,name_nchs_id_cw) %>% 
#   mutate(ID_2 = paste0(fipsctyr,cityrs,metro,sep =""))
# 
# unique_id %<>% mutate(metro = as.character(metro))
# # Merge version to NCHS identifiers 
# id_2 = unique_id %>% filter(ID_2 != "NANANA") %>% select(fipsctyr,cityrs,ID_2,name_2 = name)
# # Merge version to fips 
# id_3 = unique_id %>% select(name,fipspmsa = ID)
# 
# ######################################################################################################################################################
# 
# ######################################################################################################################################################
# # Merge mortality data with instrument and MSA info
# ######################################################################################################################################################
# mort %<>% select(stateoc,countyoc,countyrs,staters,fipspmsa,region,
#                 educ,age,sex,race,hispanic,restatus,statebth,marstat,occup,monthdth,ucod,datayear,cityrs,metro,fipsctyr)
# 
# mort = left_join(mort,id_2, by = c("cityrs","fipsctyr")) 
# mort = left_join(mort,id_3) 
# mort %<>% mutate(name = ifelse(is.na(name_2),name,name_2)) 
# 
# d = left_join(ananat,mort, by = "name") %>% filter(!is.na(age))
# 
# # Create  county to mSA 1990 mort identifier 
# id = mort %>% mutate(name = ifelse(is.na(name_2),name,name_2)) 
# 
# join = left_join(ananat,id, by = "name") 
# 
# 
# join %<>% select(fipspmsa.x,name,fipsctyr.y,staters) %>% distinct() %>% mutate(fipspmsa = as.numeric(fipspmsa.x),
#                                                                              county = as.numeric(fipsctyr.y)) %>% 
#   select(name,fipspmsa,county) %>% 
#   distinct() %>% 
#   mutate(state = toupper(str_sub(name,-2))) 
# 
# state_codes = fips_codes %>% distinct(state,state_code)
# 
# join %<>% left_join(.,state_codes, by = "state")
# 
# join %<>% 
#   # Mindful of leading 0
#   mutate(county = as.character(ifelse(state_code <10,paste0(0,county,sep = ""),county) ))
# 
# join
######################################################################################################################################################
### 1940 Data 
######################################################################################################################################################
# CenSoc data 
rm(X1990_ccc)
rm(name_fips_cw)
rm(mort)

censoc <- fread("./Data/dataverse_files (2)/censoc_numident_v3.csv")
geog = fread("./Data/dataverse_files (2)/censoc_numident_geography_supplement_v1.csv")
census = fread("./Data/usa_00006.csv",header = T)

# Geog Processing 

geog %<>% mutate(birth_fips = as.character(birth_fips),
                  death_fips = as.character(death_fips))

ananat %<>% mutate(death_fips = county_fips)

# Test merge of Ananat 

geog %<>% left_join(.,ananat, by = "death_fips")

geog %>% filter(!is.na(herf))



write_csv(geog, file ="Data/geog_1940_merge.csv")

geog %<>% filter(!is.na(name))

# 1940 data

# Filter censoc to only matching numident then merge 

censoc %<>% filter(HISTID %in% geog$HISTID)

census %<>% filter(HISTID %in% censoc$HISTID)

t = census %>% left_join(.,censoc, by = "HISTID")

t %<>% left_join(.,geog, by = "HISTID")

#t %<>% filter(!is.na(death_fips))
write_csv(t, file ="Data/merged_data_1940.csv")
# Save join 
write_csv(join,"Data/instrument_to_data_id.csv")



######################################################################################################################################################


# Output
# save working version of the data 
######################################################################################################################################################
#write_csv(d, file ="Data/working_merged_data.csv")

rm(list = ls())


