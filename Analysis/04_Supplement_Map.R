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
library(tidycensus)
library(here)

options(tigris_use_cache = TRUE)

`%notin%` = Negate(`%in%`)
# Data
data_a = read_csv(here("Data","_Cleaned","data_a.csv"))
county = read_csv(here("Data","_Cleaned","county.csv"))
#nchs2fips_county1990 <- read_csv("../Data/nchs2fips_county1990.csv")

# Get shape for 2000 county boundaries
county_sf = get_acs(geography = "county", 
               variables= c(medincome = "B19013_001"),
              year = 2009,
              survey = "acs5",
              geometry = T)

##### Get distinct FIPS ##### 

analytic_fips = data_a %>% 
  filter(death_decade ==2000) %>%
  distinct(death_fips) %>% 
  mutate(death_fips = as.character(death_fips))

##### Get county_sf  ##### 
sf_a = county_sf %>% 
  mutate(death_fips = GEOID,
         analytic_sample = case_when(death_fips %in% analytic_fips$death_fips ~"In Sample",
                                     TRUE  ~"Not In Sample")) %>% 
  left_join(.,analytic_fips)

##### Create_plot  #####
sf_a %<>% mutate(state = as.numeric(str_sub(GEOID,1,2)))

state_fips_lower48 <- c(
  1, 4, 5, 6, 8, 9, 10, 11, 12, 13, 14, 16,
  17, 18, 19, 20, 21, 22, 23, 24, 25, 26,
  27, 28, 29, 30, 31, 32, 33, 34, 35, 36,
  37, 38, 39, 40, 41, 42, 44, 45, 46, 47,
  48, 49, 50, 51, 53, 54, 55, 56
)

sf_a %>% filter(state == 15)
##### Analytic Sample #####
samp_non_samp = sf_a  %>% 
  filter(state %in% state_fips_lower48)  %>%
  rename(Sample = analytic_sample) %>%
  ggplot() + 
  geom_sf(aes(fill = Sample)) + 
  scale_fill_manual(values = c("black","white")) + 
  labs(
       caption = "This plot describes counties in the U.S. and their inclusion in the sample.
2000 county boundaries are used.
Alaska and Hawaii omitted for visualization purposes.") + 
  theme_cowplot() + 
  theme(legend.position = "bottom",
        plot.caption = element_text(hjust = 0))


ggsave(samp_non_samp,filename = here("FigTab","map.jpeg"),width = 10, height = 6)


