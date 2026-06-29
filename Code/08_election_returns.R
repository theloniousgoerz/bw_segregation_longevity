#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Title: County Election Returns - Closest Years to 1980/1990/2000
# Thelonious Goerz
# Source: Amlani & Algara (2021), "Partisanship & Nationalization in
#   American Elections," Harvard Dataverse doi:10.7910/DVN/DGUMFI
#   Presidential: dataverse_shareable_presidential_county_returns_1868_2020.Rdata
#   Gubernatorial: dataverse_shareable_gubernatorial_county_returns_1865_2020.Rdata
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ===========================================================
# DECADE MATCHING
# Presidential elections occur every 4 years and land exactly on
#   1980 and 2000. 1990 falls exactly between 1988 and 1992 (2
#   years each way), so both are kept and tagged to decade 1990.
# Gubernatorial elections are staggered by state (many states use
#   off-cycle or odd-year terms), so there is no single national
#   "closest year" - the nearest available election year is found
#   separately for each state, with ties (e.g. a state with
#   elections in 1978 and 1982, both 2 years from 1980) kept as
#   multiple rows tagged to the same decade.
# ===========================================================

rm(list = ls())

library(tidyverse)
library(here)

data_dir <- here("Data", "Electon_Returns")

load(file.path(data_dir, "dataverse_shareable_presidential_county_returns_1868_2020.Rdata"))
load(file.path(data_dir, "dataverse_shareable_gubernatorial_county_returns_1865_2020.Rdata"))

DECADES <- c(1980, 1990, 2000)

# ===========================================================
# PRESIDENTIAL
# ===========================================================
pres_years <- c(1980, 1988, 2000)

pres_decade <- pres_elections_release %>%
  filter(election_year %in% pres_years) %>%
  mutate(
    decade = case_when(
      election_year == 1980          ~ 1980,
      election_year %in% c(1988) ~ 1990,
      election_year == 2000          ~ 2000
    ),
    dem_share_two_party = democratic_raw_votes / pres_raw_county_vote_totals_two_party,
    rep_share_two_party = republican_raw_votes / pres_raw_county_vote_totals_two_party
  ) %>%
  select(
    decade, election_year, fips, county_name, state, sfips,
    seat_status, complete_county_cases,
    democratic_raw_votes, republican_raw_votes,
    pres_raw_county_vote_totals_two_party, raw_county_vote_totals,
    dem_share_two_party, rep_share_two_party
  )

# ===========================================================
# GUBERNATORIAL
# For each state, find the election year(s) closest to each decade
# marker among that state's available gubernatorial election years.
# ===========================================================
closest_years <- function(years, target) {
  d <- abs(years - target)
  unique(years[d == min(d)])
}

gov_years_by_state <- gov_elections_release %>%
  distinct(state, election_year)

gov_state_decade_years <- gov_years_by_state %>%
  group_by(state) %>%
  group_modify(~ map_dfr(DECADES, function(t) {
    tibble(decade = t, election_year = closest_years(.x$election_year, t))
  })) %>%
  ungroup()

gov_decade <- 
gov_elections_release %>%
  inner_join(gov_state_decade_years, by = c("state", "election_year")) %>%
  mutate(
    dem_share_two_party = democratic_raw_votes / gov_raw_county_vote_totals_two_party,
    rep_share_two_party = republican_raw_votes / gov_raw_county_vote_totals_two_party
  ) %>%
  group_by(state,election_year) %>% 
  mutate(
    state_dem_vote_share = sum(democratic_raw_votes,na.rm = T) / sum(gov_raw_county_vote_totals_two_party,na.rm = T),
    dem_governor = ifelse(state_dem_vote_share >.5,1,0)) %>% 
  ungroup() %>%
  select(
    decade, election_year, fips, county_name, state, sfips,
    seat_status,
    democratic_raw_votes, republican_raw_votes,
    dem_governor,
    gov_raw_county_vote_totals_two_party, raw_county_vote_totals,
    dem_share_two_party, rep_share_two_party
  )

# ===========================================================
# GOVERNOR PARTY INDICATORS
# Collapse to one observation per state-decade (most recent election year
# wins when two years tie for closest distance), derive a string party
# label, and flag states that held the same party across all three decades.
# ===========================================================

# One row per state-decade: take most-recent election year if ties exist
gov_state_decade <- gov_decade %>%
  distinct(state, sfips, decade, election_year, dem_governor) %>%
  group_by(state, decade) %>%
  slice_max(election_year, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(gov_party = case_when(
    dem_governor == 1 ~ "Dem",
    dem_governor == 0 ~ "Rep",
    TRUE              ~ NA_character_
  ))

# Wide-form to assess consistency across decades
gov_consistency <- gov_state_decade %>%
  pivot_wider(
    id_cols    = state,
    names_from = decade, values_from = gov_party,
    names_prefix = "gov_party_"
  ) %>%
  mutate(
    gov_party_consistent = case_when(
      gov_party_1980 == "Dem" & gov_party_1990 == "Dem" & gov_party_2000 == "Dem" ~ "Dem",
      gov_party_1980 == "Rep" & gov_party_1990 == "Rep" & gov_party_2000 == "Rep" ~ "Rep",
      TRUE ~ "Mixed"
    )
  )

# Merge back: per-decade label + cross-decade consistency flag
gov_decade <- gov_decade %>%
  left_join(
    gov_state_decade %>% select(state, decade, gov_party),
    by = c("state", "decade")
  ) %>%
  left_join(
    gov_consistency %>% select(state, gov_party_1980, gov_party_1990,
                               gov_party_2000, gov_party_consistent),
    by = "state"
  )

# Report how far gubernatorial matches landed from their target decade,
# since off-cycle states can be more than 2 years away.
dist_tab <- gov_decade %>%
  mutate(dist = abs(election_year - decade)) %>%
  count(decade, dist)
message(paste(capture.output(as.data.frame(dist_tab)), collapse = "\n"))

# ===========================================================
# SAVE
# ===========================================================
write_csv(pres_decade, here("Data", "_Cleaned", "presidential_returns_decades.csv"))
write_csv(gov_decade,  here("Data", "_Cleaned", "gubernatorial_returns_decades.csv"))

message("Presidential: ", nrow(pres_decade), " county-elections")
message("Gubernatorial: ", nrow(gov_decade), " county-elections")
