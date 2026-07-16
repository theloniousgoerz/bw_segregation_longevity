# # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Representivity Tables 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rm(list=ls())
library(tidyverse)
library(magrittr)
library(readr)
library(modelsummary)
library(kableExtra)
library(gt)
library(cowplot)
library(fixest)
library(tinytable)
library(here)

# Data
data_a =    read_csv(here("Data", "_Cleaned","analytic_sample.csv"))
data =      read_csv(here("Data", "_Cleaned","fulldata.csv"))
county_d =  read_csv(here("Data", "_Cleaned","county_data.csv"))
rivers =      read_csv(here("Data","derived","tiger_hydrography_county_instruments_2023.csv"))
rdi =   read_csv(here("Data","derived","atack_rail_county_instruments_1911.csv"))

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

state_abbrevs <- data.frame(death_state = c("ct", "il", "in", "ia", "ks", "me", "ma", "mi", "mn", "mo", 
                                            "ne", "nh", "nj", "ny", "nd", "oh", "pa", "ri", "sd", "vt", "wi"), 
                            north_state_d = c("North"))

state_region <- data.frame(
  death_state = c(
    "me", "nh", "vt", "ma", "ri", "ct",
    "ny", "nj", "pa",
    "oh", "in", "il", "mi", "wi",
    "mn", "ia", "mo", "nd", "sd", "ne", "ks",
    "de", "md", "dc", "va", "wv", "nc", "sc", "ga", "fl",
    "ky", "tn", "ms", "al",
    "ok", "tx", "ar", "la",
    "mt", "id", "wy", "nv", "ut", "co", "az", "nm", 
    "ak", "wa", "or", "ca", "hi"
  ),
  census_region_d = c(
    rep("Northeast", 9),
    rep("Midwest", 12),
    rep("South", 17),
    rep("West", 13)
  )
)

#  -------------------------------
# Merge on rdi and river instruments
#  -------------------------------
rdi %<>% mutate(death_fips = GEOID)
rivers %<>% mutate(death_fips = GEOID)

data_a %<>% left_join(rdi,by = c("death_fips")) %>% left_join(rivers, by = c("death_fips")) 
data %<>% left_join(rdi,by = c("death_fips")) %>% left_join(rivers, by = c("death_fips")) 

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Filtering 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# all counties
counties = 
  data_a %>% filter(byear %in% 1905:1920) %>%
  distinct(death_fips,county_dism,
                  n_governments,
                  gov_rev_share_state,
                  nh_black,
                  death_decade,
                 rdi,
                 D_star)
c_1 = counties %>% distinct(death_fips) %>% nrow()
c_2 = counties %>% filter(nh_black >0) %>% distinct(death_fips) %>% nrow()
c_3 = counties %>% 
  filter(nh_black >-0 &
  !is.na(rdi)) %>% 
  distinct(death_fips) %>%
  nrow()

ind_1 =
  data_a %>% filter(
    byear %in% 1905:1920) %>% nrow()
ind_2 =
  data_a %>% filter(
    byear %in% 1905:1920 & 
      nh_black >0) %>% nrow()
ind_3 =
  data_a%>% filter(
    byear %in% 1905:1920 & 
      nh_black >0 & 
# Upstream Sample Selection
      !is.na(rdi) & 
      !is.na(rail_km_per_km2)) %>% nrow()
ind_4 =
  data_a %>% filter(
    byear %in% 1905:1920 & 
      nh_black >0 & 
      !is.na(rdi) & 
      !is.na(rail_km_per_km2) & 
      !is.na(male) &  
      !is.na(migrated) & 
      !is.na(educ_years) &  
      !is.na(urb_code) &
      !is.na(south) &
      !is.na(married) &
      !is.na(byear) & 
      !is.na(OCC) & 
      !is.na(STATEFIP_b) &
      !is.na(weight) & 
      !is.na(county_dism) &
      !is.na(D_star)) %>% nrow()

c_4 =   data_a %>% 
  filter(
    byear %in% 1905:1920 & 
    nh_black >0 & 
    !is.na(rdi) & 
    !is.na(rail_km_per_km2) &
    !is.na(male) &  
    !is.na(migrated) & 
    !is.na(educ_years) &  
    !is.na(urb_code) &
    !is.na(south) &
    !is.na(married) &
    !is.na(byear) & 
    !is.na(OCC) & 
    !is.na(STATEFIP_b) &
    !is.na(weight) & 
    !is.na(county_dism) &
    !is.na(D_star)) %>% 
  distinct(death_fips) %>% nrow()


### Assemble Data Frame with Stepwise Deletion
data.frame(
  `Sample` = c("All Counties",
               "Counties With >0 NH Black Pop",
               "Counties With non-Missing Government Revenue Share Data",
               "Filter non-Missing Individual-and County-Level Variables."),
  `N (Counties)` = c(c_1,c_2,c_3,c_4),
  `N (Persons)` = c(ind_1,ind_2,ind_3,ind_4),
  check.names = F
) %>% 
  datasummary_df(
    align = "lcc",
    notes = "Individual-level characteristics include demographics, education, and fixed effects. Individuals are also filtered by not missing post-stratification weights.",
  output = "tinytable",
  title = "Sample Filtering Criteria",
  fmt = 0
  ) %>% 
  save_tt(here("FigTab","Filtering_table.tex"),overwrite = T)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Filter Analytic Data ###
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

data_a =
  data_a %>% filter(
    byear %in% 1905:1920 & 
      nh_black >0 & 
      #!is.na(gov_rev_share_state) & 
      #  !is.na(n_governments)
      !is.na(rdi) & 
      !is.na(rail_km_per_km2) &
      !is.na(male) &  
      !is.na(migrated) & 
      !is.na(educ_years) &  
      !is.na(south) &
      !is.na(married) &
      !is.na(byear) & 
      !is.na(OCC) & 
      !is.na(STATEFIP_b) &
      !is.na(weight) & 
      !is.na(county_dism) &
      !is.na(D_star))



data %<>%  filter(
    byear %in% 1905:1920 & 
    !is.na(male) &  
    !is.na(migrated) & 
    !is.na(educ_years) &  
    !is.na(south) &
    !is.na(married) &
    !is.na(byear) & 
    !is.na(OCC) & 
    !is.na(STATEFIP_b) &
    !is.na(weight) & 
    !is.na(county_dism) &
    !is.na(D_star) &
    nh_black >0)

db =  data_a %>% filter(Race == "Black") %>% mutate(education = educ_years)
dw =  data_a %>% filter(Race == "White") %>% mutate(education = educ_years)
db_f =  data %>% filter(Race == "Black" & !is.na(sib_group_id_flexible)) %>% mutate(education = educ_years)
dw_f =  data %>% filter(Race == "White" & !is.na(sib_group_id_flexible)) %>% mutate(education = educ_years)

## Rescale D for analysis -------------------------------

data_a %<>% mutate(county_dism = county_dism*100)
db %<>% mutate(county_dism = county_dism*100,
               county_isolb = county_isolb*100,
               H_bw = H_bw*100,
               D_star = D_star*100)

dw %<>% mutate(county_dism = county_dism*100,
               county_isolb = county_isolb*100,
               H_bw = H_bw*100,
               D_star = D_star*100
)

db_f %<>% mutate(county_dism = county_dism*100,
                 county_isolb = county_isolb*100,
                 H_bw = H_bw*100,
                 D_star = D_star*100)

dw_f %<>% mutate(county_dism = county_dism*100,
                 county_isolb = county_isolb*100,
                 H_bw = H_bw*100,
                 D_star = D_star*100
)
#  -------------------------------
# Education multi-category
#  -------------------------------
db %<>% 
  mutate(educ_cat = case_when(
    educ_years <12 ~ "Less than HS",
    educ_years == 12 ~ "High School",
    educ_years >12 & educ_years < 16 ~ "Some College",
    educ_years >=16 ~ "College+"
  ),
  educ_cat = factor(educ_cat, levels = c("Less than HS","High School","Some College","College+")))
dw %<>% 
  mutate(educ_cat = case_when(
    educ_years <12 ~ "Less than HS",
    educ_years == 12 ~ "High School",
    educ_years > 12 & educ_years < 16 ~ "Some College",
    educ_years >=16 ~ "College+"
  ),
  educ_cat = factor(educ_cat, levels = c("Less than HS","High School","Some College","College+")))

db_f %<>% 
  mutate(educ_cat = case_when(
    educ_years <12 ~ "Less than HS",
    educ_years == 12 ~ "High School",
    educ_years >12 & educ_years < 16 ~ "Some College",
    educ_years >=16 ~ "College+"
  ),
  educ_cat = factor(educ_cat, levels = c("Less than HS","High School","Some College","College+")))
dw_f %<>% 
  mutate(educ_cat = case_when(
    educ_years <12 ~ "Less than HS",
    educ_years == 12 ~ "High School",
    educ_years > 12 & educ_years < 16 ~ "Some College",
    educ_years >=16 ~ "College+"
  ),
  educ_cat = factor(educ_cat, levels = c("Less than HS","High School","Some College","College+")))

db$county_dism

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Save Data for Analysis ###
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
write_csv(data,   here("Data", "_Cleaned","data.csv"))
write_csv(data_a, here("Data", "_Cleaned","data_a.csv"))
write_csv(db,     here("Data", "_Cleaned","db.csv")) 
write_csv(dw,     here("Data", "_Cleaned","dw.csv"))
write_csv(db_f,  here("Data", "_Cleaned","db_f.csv")) 
write_csv(dw_f,  here("Data", "_Cleaned","dw_f.csv"))
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Represntivity Table Comparing Full Data to Analytic Sample 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fulldata = data %>% 
  filter(byear %in% 1905:1920 & 
           sex %in% 1:2 &
           HISPAN == 0 &
           RACE %in% 1:2 & 
           !is.na(weight)) %>% 
  
  mutate(
    Race = ifelse(RACE == 1, "White","Black"),
    pblack = nh_black/pop,
    south = case_when(south_sample == 1~1,
                      is.na(south_sample)~0),
    male = ifelse(sex ==1,1,0),
    educ_years = case_when(
      # Educ is the mean of each bracket 
      EDUC == 99 ~ NA,
      EDUC == 0 ~ 0,
      EDUC == 1 ~ 3,
      EDUC == 2 ~ 6.5,
      EDUC == 3 ~ 9, 
      EDUC == 4 ~ 10,
      EDUC == 5 ~ 11, 
      EDUC == 6 ~ 12,
      EDUC == 7 ~ 13,
      EDUC == 8 ~ 14,
      EDUC == 9 ~ 15,
      EDUC == 10 ~ 16,
      EDUC == 11 ~ 17
    ),
    black = ifelse(Race == "Black",1,0),
    hs = ifelse(educ_years >=12,1,0),
    married = case_when(MARST %in% 1:2 ~ 1,
                        MARST %in% 3:8 ~0),
    ownhome = ifelse(OWNERSHP ==1,1,0),
    employed = case_when(EMPSTAT == 1 ~ 1,
                         EMPSTAT == 2 ~ 0))



# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### Balance Table ######
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fulldata %<>% mutate(iv_sample = ifelse(HISTID %in% data_a$HISTID,"IV Sample","Non-IV Sample"))

## IV-non-IV Table ## 
var_order_rt = c(
  "Age at Death ",
  "Male",
  "Black",
  "Education (Years)" , 
  "Married (In 1940)" ,
  "Migrated (Birth County - Death County)" ,
  "Reside in South at Death" ,
  "County D at Death"  ,
  "County Population",
  "County Pr. Black" ,
  "N"
)


# Full data summary
fd_summary = fulldata %>%
  filter(!is.na(weight) & !is.na(migrated)) %>%
  mutate(migrated = ifelse(migrated == "Migrated",1,0),
         black = ifelse(Race == "Black",1,0)) 

s_summary = fulldata %>%
  filter(!is.na(weight) & !is.na(migrated)) %>%
  mutate(migrated = ifelse(migrated == "Migrated",1,0),
         black = ifelse(Race == "Black",1,0)) %>% 
         filter(iv_sample == "IV Sample")

### T Values
var_map = c("death_age", "male","educ_years", "married","migrated","south","county_dism","pop","pblack","black")

results <- data.frame(
  variable = var_map,
  diff = rep(NA_real_,length(var_map)),
  p.value = rep(NA_real_,length(var_map)),
  stringsAsFactors = FALSE
)

# loop through each variable
for (i in seq_along(var_map)) {
  var_name <- var_map[i]
  
  # run t-test
  t_out <- t.test(fd_summary[[var_name]], s_summary[[var_name]], var.equal = FALSE)
  
  # store mean diff and p-value
  results$diff[i] <- mean(fd_summary[[var_name]], na.rm = TRUE) - mean(s_summary[[var_name]], na.rm = TRUE)
  results$p.value[i] <- t_out$p.value
}

fd_summary %<>%
  select(
    "Age at Death " =   death_age,
    "Male" =   male,
    "Education (Years)" =   educ_years, 
    "Married (In 1940)" =   married,
    "Migrated (Birth County - Death County)" = migrated,
    "Reside in South at Death" =   south,
    "County D at Death" =   county_dism,
    "County Population" = pop,
    "County Pr. Black" = pblack,
    "Black" = black
  ) %>%
  mutate(N = max(row_number())) %>% 
  ungroup() %>%
  gather(variable,value) %>%
  mutate(variable = factor(variable, levels = var_order_rt)) %>%
  group_by(variable) %>% summarise(
   `Mean (All)` = mean(value,na.rm = T))

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Sample Summary
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

s_summary %<>%
  select(
    "Age at Death " =   death_age,
    "Male" =   male,
    "Education (Years)" =   educ_years, 
    "Married (In 1940)" =   married,
    "Migrated (Birth County - Death County)" = migrated,
    "Reside in South at Death" =   south,
    "County D at Death" =   county_dism,
    "County Population" = pop,
    "County Pr. Black" = pblack,
    "Black" = black
  ) %>%
  mutate(N = max(row_number())) %>% 
  ungroup() %>%
  gather(variable,value) %>%
  mutate(variable = factor(variable, levels = var_order_rt)) %>%
  group_by(variable) %>% summarise(
   `Mean (Sample)` = mean(value,na.rm = T))

## Combine 
rep_table = left_join(fd_summary,s_summary) %>% 
  rename(Variable = variable)

results = rbind(results,data.frame(variable = "N",diff = 0,p.value = 0,check.names =F))

results %<>% 
  mutate(
   Variable = case_when(variable == "death_age" ~"Age at Death ",
   variable == "male" ~"Male",
   variable == "educ_years" ~"Education (Years)",
   variable == "married" ~"Married (In 1940)",
   variable == "migrated" ~"Migrated (Birth County - Death County)",
   variable == "south" ~"Reside in South at Death",
   variable == "county_dism" ~"County D at Death",
   variable == "pop" ~"County Population",
   variable == "pblack" ~"County Pr. Black",
   variable == "black"~"Black",
   variable == "N" ~ "N"
  )) %>% select(Variable,diff,p.value)

t = rep_table %>% left_join(.,results)

lrow = t[t$Variable == "N",] %>%
  mutate(diff = "-", 
         p.value = "-")

## Save
datasummary_df(t[t$Variable != "N",], 
               align = "lcccc", 
               fmt = 2, 
               add_rows = lrow,
               title = "Representativeness of Numident Analytic Sample V Full Sample",
               notes = "This table compares the descriptive statistics of the full Numident sample to the analytic sample subset of the sample used in analysis.",
               output = "tinytable"
               ) %>% 
  save_tt(output = here("FigTab","representivity_table.tex"), overwrite = T)
  

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Descriptive Statistics Table ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

data_table = fulldata %>% 
  filter(!is.na(weight) & !is.na(migrated) & iv_sample == "IV Sample") %>%
  mutate(migrated = ifelse(migrated == "Migrated",1,0)) %>% 
  select(
     "Age at Death " =   death_age,
     "Male" =   male,
     "Education (Years)" =   educ_years, 
     "Married (In 1940)" =   married,
     "Migrated (Birth County - Death County)" = migrated,
     "Reside in South at Death" =   south,
     "County D at Death" =   county_dism,
     "County Population" = pop,
     "County Pr. Black" = pblack,
     "Race" = Race,
  ) 

 datasummary(All(data_table)~(Mean)*Race,
             data =data_table,
             title = "Descriptive Statistics of Analytic Sample By Race",
             align = "lcc",
             add_rows = data.frame(r1 = c("N"),
                                   r2 = nrow(db),
                                   r3 = nrow(dw)),
             notes = "This table presents descriptive statistics for the analytic sample by racial group.",
             threeparttable = T,
             fmt = 2,
             output = "tinytable"
 ) %>%
   save_tt(output = here("FigTab","descriptive_table.tex"), overwrite = T)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Sibling Sample Representativeness Table ####
# Compares full IV analytic sample to exact sibling matches (sib_group_id_exact)
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sib_base <- fulldata %>%
  filter(!is.na(weight) & !is.na(migrated)) %>%
  mutate(
    migrated = ifelse(migrated == "Migrated", 1, 0),
    Sample   = factor(
      ifelse(!is.na(sib_group_id_exact), "Sibling Sample", "Full Sample"),
      levels = c("Full Sample", "Sibling Sample")
    )
  )

n_full <- nrow(sib_base)
n_sib  <- sum(!is.na(sib_base$sib_group_id_exact))

sib_table <- sib_base %>%
  select(
    "Age at Death"                           = death_age,
    "Male"                                   = male,
    "Education (Years)"                      = educ_years,
    "Married (In 1940)"                      = married,
    "Migrated (Birth County - Death County)" = migrated,
    "Reside in South at Death"               = south,
    "County D at Death"                      = county_dism,
    "County Population"                      = pop,
    "County Pr. Black"                       = pblack,
    "Sample"                                 = Sample
  )

datasummary(
  All(sib_table) ~ Mean * Sample,
  data         = sib_table,
  title        = "Representativeness of Exact Sibling Matches Relative to Full IV Sample",
  align        = "lcc",
  add_rows     = data.frame(r1 = "N", r2 = n_full, r3 = n_sib),
  notes        = "This table compares descriptive statistics for the full IV analytic sample to the subset matched into exact sibling groups. Means are unweighted.",
  threeparttable = TRUE,
  fmt          = 2,
  output       = "tinytable"
) %>%
  save_tt(output = here("FigTab", "sibling_representivity_table.tex"), overwrite = TRUE)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Representativeness Table By Race and Sample ####
# Black/White columns under each of: full data, IV sample, flexible sibling sample
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# County population is rescaled to thousands so the six columns fit the page width.
rep_base <- fulldata %>%
  filter(!is.na(weight) & !is.na(migrated)) %>%
  mutate(migrated = ifelse(migrated == "Migrated", 1, 0),
         pop      = pop/1000)

# The sibling sample is the flexible sibling match on the full data and carries no
# RDI restriction, so it is not a subset of the IV column. This matches db_f/dw_f.
samples <- bind_rows(
  rep_base %>% mutate(Sample = "Full Data"),
  rep_base %>% filter(iv_sample == "IV Sample") %>% mutate(Sample = "IV Sample"),
  rep_base %>% filter(!is.na(sib_group_id_flexible)) %>% mutate(Sample = "Sibling Sample")
) %>%
  mutate(
    Sample = factor(Sample, levels = c("Full Data", "IV Sample", "Sibling Sample")),
    Race   = factor(Race, levels = c("Black", "White"))
  )

race_var_labels <- c(
  death_age   = "Age at Death",
  male        = "Male",
  educ_years  = "Education (Years)",
  married     = "Married (In 1940)",
  migrated    = "Migrated (Birth to Death County)",
  south       = "Reside in South at Death",
  county_dism = "County D at Death",
  pop         = "County Population (1000s)",
  pblack      = "County Pr. Black"
)

race_col_order <- c(
  "Full Data__Black",      "Full Data__White",
  "IV Sample__Black",      "IV Sample__White",
  "Sibling Sample__Black", "Sibling Sample__White"
)

fmt_race_num <- function(x) formatC(x, format = "f", digits = 2, big.mark = ",")

race_stats <- samples %>%
  select(all_of(names(race_var_labels)), Race, Sample) %>%
  pivot_longer(all_of(names(race_var_labels)), names_to = "variable", values_to = "value") %>%
  group_by(variable, Sample, Race) %>%
  summarise(m = mean(value, na.rm = TRUE), s = sd(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(col = paste0(Sample, "__", Race))

race_means <- race_stats %>%
  mutate(val = fmt_race_num(m)) %>%
  select(variable, col, val) %>%
  pivot_wider(names_from = col, values_from = val)

race_sds <- race_stats %>%
  mutate(val = paste0("(", fmt_race_num(s), ")")) %>%
  select(variable, col, val) %>%
  pivot_wider(names_from = col, values_from = val)

# Interleave: mean row carries the label, SD row sits directly beneath it unlabelled.
race_body <- map_dfr(names(race_var_labels), function(v) {
  bind_rows(
    race_means %>% filter(variable == v) %>% mutate(Variable = race_var_labels[[v]]),
    race_sds   %>% filter(variable == v) %>% mutate(Variable = "")
  )
}) %>%
  select(Variable, all_of(race_col_order))

race_n <- samples %>%
  count(Sample, Race) %>%
  mutate(col = paste0(Sample, "__", Race),
         n   = formatC(n, format = "d", big.mark = ",")) %>%
  select(col, n) %>%
  pivot_wider(names_from = col, values_from = n) %>%
  mutate(Variable = "N") %>%
  select(Variable, all_of(race_col_order))

rep_race_table <- bind_rows(race_body, race_n)
names(rep_race_table) <- c("Variable", rep(c("Black", "White"), 3))

datasummary_df(
  rep_race_table,
  align  = "lcccccc",
  title  = "Representativeness of Analytic Samples by Race",
  notes  = "Cell entries are unweighted means with standard deviations in parentheses. The IV sample is restricted to counties with non-missing railroad division index and to individuals with non-missing analytic covariates. The sibling sample is the flexible sibling-group match on the full data and is not restricted to counties with non-missing railroad division index data, so it is not a subset of the IV sample.",
  threeparttable = TRUE,
  output = "tinytable"
) %>%
  group_tt(j = list("Full Data" = 2:3, "IV Sample" = 4:5, "Sibling Sample" = 6:7)) %>%
  save_tt(output = here("FigTab", "representivity_table_by_race.tex"), overwrite = TRUE)
