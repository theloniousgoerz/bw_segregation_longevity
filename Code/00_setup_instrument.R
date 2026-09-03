# ==============================================================================
# 00_setup.R
# Project: bw_segregation_longevity
# Purpose: Define project paths, install/load packages used by the
#          instrument-construction scripts (05 and 06).
# Author:  [Thelonious Goerz]
# ==============================================================================

# ---- Packages ----------------------------------------------------------------
pkgs <- c(
  "here",
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

# ---- Project paths -----------------------------------------------------------
# here() anchors to the directory containing bw_segregation_longevity.Rproj (or,
# absent that, the nearest .git root), so this resolves correctly on any machine
# instead of only the original author's.
PROJ_DIR <- here::here()
CODE_DIR <- file.path(PROJ_DIR, "Code")
DATA_DIR <- file.path(PROJ_DIR, "Data")

RAW_DIR     <- file.path(DATA_DIR, "raw")
DERIVED_DIR <- file.path(DATA_DIR, "derived")
CACHE_DIR   <- file.path(DATA_DIR, "cache")   # tigris cache + restartable loops

for (d in c(RAW_DIR, DERIVED_DIR, CACHE_DIR)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

# Cache TIGER downloads so re-runs don't re-download ~3,100 county files
options(tigris_use_cache = TRUE)
Sys.setenv(TIGRIS_CACHE_DIR = CACHE_DIR)

# Equal-area projection for all length/area calculations (CONUS Albers)
EA_CRS <- 5070

message("Setup complete. Data dirs under: ", DATA_DIR)
