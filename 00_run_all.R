## Run All
# Thelonious Goerz
# This script runs all code to replicate figures and tables. 

library(here)
library(tictoc)

tictoc::tic()
# Data Processing
source(here::here("Code","01_Numident.R"))
tictoc::toc()
rm(list = ls())

tictoc::tic()
source(here::here("Code","02_cofgov_data.R"))
tictoc::toc()
rm(list = ls())

tictoc::tic()
source(here::here("Code","03_county_data.R"))
tictoc::toc()
rm(list = ls())

# Analysis
tictoc::tic()
source(here::here("Analysis", "01_Data_Cleaning.R"))
tictoc::toc()
rm(list = ls())
tictoc::tic()
source(here::here("Analysis", "02_Representivity_Table.R"))
tictoc::toc()
rm(list = ls())
tictoc::tic()
source(here::here("Analysis", "03_Regression_Models.R"))
tictoc::toc()
rm(list = ls())
tictoc::tic()
source(here::here("Analysis", "04_Supplment_Map.R"))
tictoc::toc()