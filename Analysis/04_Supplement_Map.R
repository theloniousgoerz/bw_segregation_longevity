## Map of Sample Counties 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## Packages
rm(list=ls())
library(tidyverse)
library(magrittr)
library(readr)
library(modelsummary)
library(kableExtra)
library(gt)
library(cowplot)
library(fixest)
library(marginaleffects)
library(broom)
library(tinytable)
library(ggbrace)
library(gompertztrunc)
library(tidycensus)

options(tigris_use_cache = TRUE)
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

# Data
data_a = read_csv("../Data/_Cleaned/analytic_sample.csv")
ananat = read_csv("../Data/_Cleaned/data.csv")
#county = read_csv("/Users/theloniousgoerz/Academic/Projects/QP/Data/1990_county_shapefiles/nhgis0023_ds120_1990_county.csv")
county_d = read_csv("../Data/_Cleaned/county_data.csv")
nchs2fips_county1990 <- read_csv("../Data/nchs2fips_county1990.csv")

county_sf = get_acs(geography = "county", 
               variables= c(medincome = "B19013_001"),
              year = 2009,
              survey = "acs5",
              geometry = T)

nchs2fips_county1990 %<>% mutate(death_fips = fipsco)



##### Get distinct FIPS ##### 

analytic_fips = data_a %>% filter(!is.na(n_governments) & 
                                  !is.na(county_dism) & 
                                  death_decade ==1990) %>%
  distinct(death_fips,pblack,death_msa_name)  %>% mutate(death_fips = as.character(death_fips))

d_2000 = county_d %>% filter(death_decade == 1990) %>% mutate(death_fips = as.character(death_fips))

##### Get Filter county_sf  ##### 

sf_a = county_sf %>% 
  mutate(death_fips = GEOID,
         analytic_sample = case_when(death_fips %in% analytic_fips$death_fips ~"In Sample",
                                     TRUE  ~"Not In Sample"),
         ananat_samp = case_when(death_fips %in% as.character(ananat$death_fips) ~ "RDI",
                                 TRUE ~ "Non_RDI")) %>% 
  left_join(.,d_2000) %>% 
  left_join(.,analytic_fips) %>% 
  left_join(.,nchs2fips_county1990)

##### Create_plot  #####
sf_a %<>% mutate(state = as.numeric(str_sub(GEOID,1,2)))


##### Ananat Plot #####

sf_a  %>% 
#  filter(state != 15 & state !=2)  %>%
ggplot() + 
  geom_sf(aes(fill = ananat_samp)) + 
  theme_bw()

##### Analytic Sample #####

data_a %<>% left_join(.,nchs2fips_county1990)

samp_non_samp = sf_a  %>% 
  filter(state != 15 & state !=2)  %>%
  ggplot() + 
  geom_sf(aes(fill = analytic_sample)) + 
  theme_bw() + 
  scale_fill_manual(values = c("black","white")) + 
  labs(title = "Sample V non-Sample Counties",
       caption = "Alaska and Hawaii omitted.") + 
  theme(legend.position = "top")

ggsave(samp_non_samp,filename = "./FigTab/map.jpeg",width = 10, height = 6)


### Geography of the Black Population in Sample ### 

sf_a  %>% filter(pblack >0) %>%
  filter(analytic_sample == "In Sample" & state != 2 & state !=15)  %>%
  ggplot() + 
  geom_sf(aes(fill = pblack)) + 
  theme_bw() + 
 # scale_fill_manual(values = c("black","white")) + 
  labs(title = "Sample V non-Sample Counties",
       caption = "Alaska and Hawaii omitted.") + 
  theme(legend.position = "top")

sf_a  %>%
  filter(analytic_sample == "Not In Sample" & state != 2)  %>%
  ggplot() + 
  geom_sf(aes(fill = n_governments)) + 
  theme_bw() + 
  # scale_fill_manual(values = c("black","white")) + 
  labs(title = "Sample V non-Sample Counties",
       caption = "Alaska and Hawaii omitted.") + 
  theme(legend.position = "top")



