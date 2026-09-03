## Run All
# Thelonious Goerz
# This script runs all code to replicate figures and tables.
#
# Prerequisites: the raw data listed in README.md > Data must already be
# downloaded and placed at the paths given there. Code/05 and
# Code/06 also reach out over the network on their own (Census TIGER/Line and
# Jeremy Atack's railroad shapefile); see the notes above those two steps below.
#
# Order: every script in Code/ runs before any script in Analysis/, since the
# Analysis scripts read the _Cleaned/derived files Code/ writes. Within
# Analysis/, 03_Representivity_Table.R runs before 02_Mechanisms.R even though
# its number is higher -- see the note above that step.

library(here)
library(tictoc)

# ============================================================
# Code: data construction
# ============================================================
tictoc::tic("Code/01_Numident.R")
source(here::here("Code", "01_Numident.R"))
tictoc::toc()
rm(list = ls())

tictoc::tic("Code/02_cofgov_data.R")
source(here::here("Code", "02_cofgov_data.R"))
tictoc::toc()
rm(list = ls())

tictoc::tic("Code/03_county_data.R")
source(here::here("Code", "03_county_data.R"))
tictoc::toc()
rm(list = ls())

tictoc::tic("Code/04_policy_data.R")
source(here::here("Code", "04_policy_data.R"))
tictoc::toc()
rm(list = ls())

# Downloads Jeremy Atack's railroad shapefile on first run (~60-100MB) and cuts
# every CONUS county polygon along it: ~10-30 min on a first run, seconds on any
# re-run since Data/cache/rdi_checkpoint_1911.rds is already populated.
tictoc::tic("Code/05_atack_railroads_county.R")
source(here::here("Code", "05_atack_railroads_county.R"))
tictoc::toc()
rm(list = ls())

# Downloads Census TIGER/Line hydrography for ~3,100 counties one at a time:
# 1-3 hours on a first run, seconds on any re-run since
# Data/cache/hydro_checkpoint_2023.rds is already populated.
tictoc::tic("Code/06_tiger_hydrography_county.R")
source(here::here("Code", "06_tiger_hydrography_county.R"))
tictoc::toc()
rm(list = ls())

tictoc::tic("Code/07_1940_segregation.R")
source(here::here("Code", "07_1940_segregation.R"))
tictoc::toc()
rm(list = ls())

# ============================================================
# Analysis: cleaning, tables, and figures
# ============================================================
tictoc::tic("Analysis/01_Data_Cleaning.R")
source(here::here("Analysis", "01_Data_Cleaning.R"))
tictoc::toc()
rm(list = ls())

# Runs before 02_Mechanisms.R on purpose: it writes Data/_Cleaned/data_a.csv
# (and db/dw/db_f/dw_f.csv), which 02_Mechanisms.R reads back in. Running in
# filename order would fail on a from-scratch run with a file-not-found error.
tictoc::tic("Analysis/03_Representivity_Table.R")
source(here::here("Analysis", "03_Representivity_Table.R"))
tictoc::toc()
rm(list = ls())

tictoc::tic("Analysis/02_Mechanisms.R")
source(here::here("Analysis", "02_Mechanisms.R"))
tictoc::toc()
rm(list = ls())

tictoc::tic("Analysis/04_Regression_Models.R")
source(here::here("Analysis", "04_Regression_Models.R"))
tictoc::toc()
rm(list = ls())

tictoc::tic("Analysis/05_gompertz.R")
source(here::here("Analysis", "05_gompertz.R"))
tictoc::toc()
rm(list = ls())

tictoc::tic("Analysis/06_Robustness_Checks.R")
source(here::here("Analysis", "06_Robustness_Checks.R"))
tictoc::toc()
rm(list = ls())

tictoc::tic("Analysis/07_1940_segregation.R")
source(here::here("Analysis", "07_1940_segregation.R"))
tictoc::toc()
rm(list = ls())
