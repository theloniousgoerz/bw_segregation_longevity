# ==============================================================================
# 00_setup.R
# Project: bw_segregation_longevity
# Purpose: Define project paths, install/load packages used by the
#          instrument-construction scripts (05 and 06).
# Author:  [Thelonious Goerz]
# ==============================================================================

# ---- Project paths -----------------------------------------------------------
PROJ_DIR <- "/Users/theloniousgoerz/Academic/Projects/bw_segregation_longevity"
CODE_DIR <- file.path(PROJ_DIR, "Code")
DATA_DIR <- file.path(PROJ_DIR, "Data")

RAW_DIR     <- file.path(DATA_DIR, "raw")
DERIVED_DIR <- file.path(DATA_DIR, "derived")
CACHE_DIR   <- file.path(DATA_DIR, "cache")   # tigris cache + restartable loops

for (d in c(RAW_DIR, DERIVED_DIR, CACHE_DIR)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

# ---- Packages ----------------------------------------------------------------
pkgs <- c(
  "sf",        # vector GIS
  "lwgeom",    # st_split() for cutting county polygons with rail lines
  "tigris",    # Census TIGER/Line downloads (counties, linear_water)
  "dplyr",
  "purrr",
  "readr",
  "units"
)

to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install) > 0) install.packages(to_install)
invisible(lapply(pkgs, library, character.only = TRUE))

# Cache TIGER downloads so re-runs don't re-download ~3,100 county files
options(tigris_use_cache = TRUE)
Sys.setenv(TIGRIS_CACHE_DIR = CACHE_DIR)

# Equal-area projection for all length/area calculations (CONUS Albers)
EA_CRS <- 5070

message("Setup complete. Data dirs under: ", DATA_DIR)
