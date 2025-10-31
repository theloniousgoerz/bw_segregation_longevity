# Instrument MSA and county merge File

# This file cross-walks 1990 MSAs to counties from 1990 Vital Statistics Data 

######################################################################################################################################################
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
library(fixest)
library(striprtf)
library(sqldf)
library(readxl)
library(data.table)
options(scipen = 999)

######################################################################################################################################################
# Load Data 
# Create Crosswalk
######################################################################################################################################################
# County information 
#countyrace <- read_csv("Data/nhgis0014_csv/nhgis0014_ds120_1990_tract.csv")

# MSA Identifiers 
X1990_ccc <- read_excel("./Data/ICPSR_38765 2/DS0001/38765-0001-Zipped_package/1990_ccc.xlsx")
name_fips_cw <- read_excel("Data/name_fips_cw.xlsx")
# Mortality data 1990-99
mort = read_csv("./Data/mort1990.csv")

# Instrument
ananat = read_dta("./Data/aej_maindata.dta")

nchs2fips_county1990 <- read_csv("Data/nchs2fips_county1990.csv")

name_fips_cw %<>% mutate(name = str_sub(name,start = 2,end =-2))

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
# create unique identifier for name
unique_id %<>% left_join(.,nchs_msa) %>% 
  mutate(ID = ifelse(is.na(msa_fips_m),fipspmsa,msa_fips_m)) %>% left_join(.,name_nchs_id_cw) %>% 
  mutate(ID_2 = paste0(fipsctyr,cityrs,metro,sep =""))

unique_id %<>% mutate(metro = as.character(metro))
# Merge version to NCHS identifiers 
id_2 = unique_id %>% filter(ID_2 != "NANANA") %>% select(fipsctyr,cityrs,metro,ID_2,name_2 = name)
# Merge version to fips 
id_3 = unique_id %>% select(name,fipspmsa = ID)

mort_all = mort

mort_all = left_join(mort_all,id_2, by = c("cityrs","fipsctyr","metro")) 
mort_all = left_join(mort_all,id_3) 
mort_all %<>% mutate(name = ifelse(is.na(name_2),name,name_2)) 

d = left_join(ananat,mort_all, by = "name") %>% filter(!is.na(age))

# Create  county to mSA 1990 mort identifier 
mort = left_join(mort,id_2, by = c("cityrs","fipsctyr")) 
mort = left_join(mort,id_3) 
id = mort %>% mutate(name = ifelse(is.na(name_2),name,name_2)) 

join = left_join(ananat,id, by = "name") 

join %<>% select(fipspmsa.x,name,fipsstr,fipsctyr.y) %>% distinct() %>% mutate(fipspmsa = as.numeric(fipspmsa.x),
                                                                               county = as.numeric(fipsctyr.y),
                                                                               state = as.numeric(fipsstr)) %>% 
  select(name,fipspmsa,state,county)


write_csv(join,"Data/instrument_to_data_id.csv")


