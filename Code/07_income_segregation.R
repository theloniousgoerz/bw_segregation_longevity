#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Title: Rank-Ordered Income Segregation (Hr)
# Thelonious Goerz
# Method: Reardon (2011) via OasisR::rankorderseg
# Data: NHGIS family income brackets by tract, 1980/1990/2000
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ===========================================================
# NHGIS FILES USED
# ===========================================================
# nhgis0068_ds107_1980_tract       1980 STF3
#   DIK001-017  Family Income in 1979, all families (NT73, 17 brackets)
#   DIM001-009  White families (NT75, 9 brackets)
#   DIM010-018  Black families (NT75, 9 brackets)
#
# nhgis0068_ds123_1990_tract       1990 STF3
#   E0Q001-025  Family Income in 1989, all families (NP107, 25 brackets)
#
# nhgis0068_ds125_1990_tract_141   1990 STF4b
#   FHIABR001-300  White non-Hispanic families × income × family type (NPB86)
#   FHIABS001-300  Black non-Hispanic families × income × family type (NPB86)
#   Structure: 25 income brackets × 12 family types = 300 columns each
#   → must sum every 12 consecutive columns to collapse to 25 income brackets
#
# nhgis0070_ds151_2000_tract       2000 SF3a
#   GNN001-016    Family Income in 1999, all families (NP076A, 16 brackets)
#
# nhgis0070_ds153_2000_tract       2000 SF4
#   H2CAAIV001-016  White alone, not Hispanic or Latino (NPCT112A, 16 brackets)
#   H2CAAIX001-016  Black alone, not Hispanic or Latino (NPCT112A, 16 brackets)
# ===========================================================

rm(list = ls())

library(tidyverse)
library(data.table)
library(magrittr)
library(OasisR)
library(here)

data_dir <- here("Data", "Census", "nhgis_income")

# ===========================================================
# LOAD DATA
# ===========================================================
d1980       <- fread(file.path(data_dir, "nhgis0068_ds107_1980_tract.csv"))
d1990_all   <- fread(file.path(data_dir, "nhgis0068_ds123_1990_tract.csv"))
d1990_race  <- fread(file.path(data_dir, "nhgis0068_ds125_1990_tract_141.csv"))
d2000       <- fread(file.path(data_dir, "nhgis0070_ds151_2000_tract.csv"))
d2000_race  <- fread(file.path(data_dir, "nhgis0070_ds153_2000_tract.csv"))

# ===========================================================
# FIPS CONSTRUCTION
# GISJOIN format: G + state(2) + 0 + county(3) + 0 + tract(...)
# ===========================================================
make_fips <- function(gisjoin) paste0(substr(gisjoin, 2, 3), substr(gisjoin, 5, 7))

d1980[,      county_fips := make_fips(GISJOIN)]
d1990_all[,  county_fips := make_fips(GISJOIN)]
d1990_race[, county_fips := make_fips(GISJOIN)]
d2000[,      county_fips := make_fips(GISJOIN)]
d2000_race[, county_fips := make_fips(GISJOIN)]

# ===========================================================
# 1990 RACE COLLAPSE
# FHI variables cycle through 12 family types within each income bracket:
#   cols 1-12   = bracket 1 (<$5k)
#   cols 13-24  = bracket 2 ($5-10k)
#   ...
#   cols 289-300 = bracket 25 ($150k+)
# Collapse to 25 bracket columns by summing across family types.
# ===========================================================
N_1990_BRACKETS <- 25
FAMILY_TYPES    <- 12   # family type sub-categories per bracket

collapse_fhi <- function(dt, prefix) {
  all_cols <- paste0(prefix, sprintf("%03d", 1:300))
  missing  <- setdiff(all_cols, names(dt))
  if (length(missing) > 0)
    stop("Missing columns for prefix '", prefix, "': ", paste(missing[1:min(3,length(missing))], collapse=", "), "...")

  mat <- as.matrix(dt[, ..all_cols])
  mat[is.na(mat)] <- 0L

  # Sum each group of FAMILY_TYPES columns to get bracket totals
  bracket_mat <- matrix(0L, nrow = nrow(mat), ncol = N_1990_BRACKETS)
  for (b in seq_len(N_1990_BRACKETS)) {
    idx <- ((b - 1L) * FAMILY_TYPES + 1L):(b * FAMILY_TYPES)
    bracket_mat[, b] <- rowSums(mat[, idx, drop = FALSE])
  }
  as.data.table(bracket_mat)
}

white_1990_brackets <- collapse_fhi(d1990_race, "FHIABR")
black_1990_brackets <- collapse_fhi(d1990_race, "FHIABS")

# Attach GISJOIN and county_fips for later use
white_1990_brackets[, `:=`(GISJOIN = d1990_race$GISJOIN, county_fips = d1990_race$county_fips)]
black_1990_brackets[, `:=`(GISJOIN = d1990_race$GISJOIN, county_fips = d1990_race$county_fips)]

# Min families per county to include race-specific Hr.
# Below this threshold the polynomial approximation overshoots [0,1].
MIN_FAMILIES <- 10000L

# ===========================================================
# HELPERS
# ===========================================================

# Scalar Hr from a tract × income-bracket matrix, or NA if insufficient data.
compute_Hr <- function(county_rows, bracket_cols) {
  if (nrow(county_rows) < 2L) return(NA_real_)

  mat <- as.matrix(county_rows[, ..bracket_cols])
  mat[is.na(mat)] <- 0

  mat <- mat[rowSums(mat) > 0, , drop = FALSE]
  if (nrow(mat) < 2L) return(NA_real_)

  result <- tryCatch(rankorderseg(mat), error = function(e) NULL)
  if (is.null(result)) return(NA_real_)

  hr <- result$Hr
  if (is.list(hr)) hr <- hr[[1L]]
  as.numeric(hr)[1L]
}

# Total families in a county for a given set of bracket columns.
county_total <- function(county_rows, bracket_cols) {
  mat <- as.matrix(county_rows[, ..bracket_cols])
  mat[is.na(mat)] <- 0
  sum(mat)
}

# ===========================================================
# PER-YEAR COMPUTATION FUNCTIONS
# ===========================================================

# --- 1980 ---
# All families:   DIK001-017 (17 brackets)
# White families: DIM001-009 (9 brackets)
# Black families: DIM010-018 (9 brackets)
run_1980 <- function() {
  all_cols   <- paste0("DIK", sprintf("%03d", 1:17))
  white_cols <- paste0("DIM", sprintf("%03d", 1:9))
  black_cols <- paste0("DIM", sprintf("%03d", 10:18))

  counties <- unique(d1980$county_fips)
  message("1980: ", length(counties), " counties")

  map_dfr(counties, function(fips) {
    sub <- d1980[county_fips == fips]
    n_white <- county_total(sub, white_cols)
    n_black <- county_total(sub, black_cols)
    meets_threshold <- n_white >= MIN_FAMILIES & n_black >= MIN_FAMILIES
    tibble(
      year        = 1980L,
      county_fips = fips,
      n_white_fam = n_white,
      n_black_fam = n_black,
      Hr_all   = compute_Hr(sub, all_cols),
      Hr_white = if (meets_threshold) compute_Hr(sub, white_cols) else NA_real_,
      Hr_black = if (meets_threshold) compute_Hr(sub, black_cols) else NA_real_
    )
  })
}

# --- 1990 ---
# All families: E0Q001-025 from ds123 (25 brackets)
# White/Black:  collapsed bracket columns from ds125
run_1990 <- function() {
  all_cols <- paste0("E0Q", sprintf("%03d", 1:25))
  w_cols   <- paste0("V", 1:25)   # V1..V25 from collapse_fhi()
  b_cols   <- paste0("V", 1:25)

  counties <- unique(d1990_all$county_fips)
  message("1990: ", length(counties), " counties")

  map_dfr(counties, function(fips) {
    sub_all   <- d1990_all[county_fips == fips]
    sub_white <- white_1990_brackets[county_fips == fips]
    sub_black <- black_1990_brackets[county_fips == fips]
    n_white <- county_total(sub_white, w_cols)
    n_black <- county_total(sub_black, b_cols)
    meets_threshold <- n_white >= MIN_FAMILIES & n_black >= MIN_FAMILIES
    tibble(
      year        = 1990L,
      county_fips = fips,
      n_white_fam = n_white,
      n_black_fam = n_black,
      Hr_all   = compute_Hr(sub_all,   all_cols),
      Hr_white = if (meets_threshold) compute_Hr(sub_white, w_cols) else NA_real_,
      Hr_black = if (meets_threshold) compute_Hr(sub_black, b_cols) else NA_real_
    )
  })
}

# --- 2000 ---
# All families:              GNN001-016     from ds151 (16 brackets)
# White alone, non-Hispanic: H2CAAIV001-016 from ds153 (16 brackets)
# Black alone, non-Hispanic: H2CAAIX001-016 from ds153 (16 brackets)
run_2000 <- function() {
  all_cols   <- paste0("GNN",     sprintf("%03d", 1:16))
  white_cols <- paste0("H2CAAIV", sprintf("%03d", 1:16))
  black_cols <- paste0("H2CAAIX", sprintf("%03d", 1:16))

  counties <- unique(d2000$county_fips)
  message("2000: ", length(counties), " counties")

  map_dfr(counties, function(fips) {
    sub      <- d2000[county_fips == fips]
    sub_race <- d2000_race[county_fips == fips]
    n_white  <- county_total(sub_race, white_cols)
    n_black  <- county_total(sub_race, black_cols)
    meets_threshold <- n_white >= MIN_FAMILIES & n_black >= MIN_FAMILIES
    tibble(
      year        = 2000L,
      county_fips = fips,
      n_white_fam = n_white,
      n_black_fam = n_black,
      Hr_all   = compute_Hr(sub,      all_cols),
      Hr_white = if (meets_threshold) compute_Hr(sub_race, white_cols) else NA_real_,
      Hr_black = if (meets_threshold) compute_Hr(sub_race, black_cols) else NA_real_
    )
  })
}

# ===========================================================
# RUN
# ===========================================================
results_1980 <- run_1980()
results_1990 <- run_1990()
results_2000 <- run_2000()

income_seg <- bind_rows(results_1980, results_1990, results_2000)
income_seg %>% summary()
# ===========================================================
# SAVE
# ===========================================================
write_csv(income_seg, here("Data", "_Cleaned", "income_segregation_Hr.csv"))
message("Saved: ", nrow(income_seg), " county-year observations")



