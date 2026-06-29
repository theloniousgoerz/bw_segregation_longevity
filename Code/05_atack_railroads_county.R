# ==============================================================================
# 01_atack_railroads_county.R
# Project: bw_segregation_longevity
#
# Purpose: Download Jeremy Atack's historical railroad GIS database
#          (1826-1911) and construct county-level instruments for
#          segregation based on pre-1911 railroad configurations:
#
#   (1) rail_any        - indicator: any pre-cutoff railroad in county
#   (2) rail_km         - total track length (km) in county
#   (3) rail_km_per_km2 - track density (km of track per km^2 of land)
#   (4) rdi             - Railroad Division Index (Ananat 2011, AEJ:Applied):
#                         RDI = 1 - sum_i (a_i / A)^2, where a_i are the areas
#                         of the subpolygons created by cutting the county
#                         with the railroad network and A is county area.
#                         0 = undivided; -> 1 = finely divided.
#   (5) n_rail_pieces   - number of subpolygons the rail network cuts the
#                         county into (1 = undivided)
#
# Data:  Jeremy Atack, "Historical Geographic Information Systems (GIS)
#        database of U.S. Railroads" (May 2016; revised Oct 31, 2023).
#        https://my.vanderbilt.edu/jeremyatack/data-downloads/
#        Attribute field `InOpBy` records the year by which each segment
#        was in operation; we keep segments with InOpBy <= YEAR_CUTOFF.
#
# References:
#   Ananat (2011) AEJ: Applied 3(2): 34-66.
#   Chyn, Haggag & Stuart (2022, NBER WP 30563) - county/city RDI as IV for
#     segregation; note their discussion of whether to control for track
#     length per km^2 (we output it so you can include/exclude it yourself).
#   Atack (2013) J. Econ. History 73: 313-338 (data documentation).
#
# Notes:
#   - Counties are MODERN (2020 vintage, cartographic boundary) so the output
#     merges on 5-digit FIPS to contemporary county outcome data. If you need
#     historical county boundaries, swap in NHGIS shapefiles at the marked
#     step; everything downstream is unchanged.
#   - The st_split step is the slow part (~10-30 min for all CONUS counties
#     on a laptop). Progress is checkpointed to Data/cache so the loop is
#     restartable.
# ==============================================================================

source("/Users/theloniousgoerz/Academic/Projects/bw_segregation_longevity/Code/00_setup_instrument.R")

YEAR_CUTOFF <- 1911   # "pre-1911": keep all track in operation by 1911.
                      # Set to 1910 (pre-Great Migration census) or 1900 for
                      # robustness; rerun from Step 3.

ATACK_URL <- "https://cdn.vanderbilt.edu/t2-my/my-prd/wp-content/uploads/sites/133/2024/09/RR1826-1911Modified103123.zip"

# ---- Step 1: Download and unzip Atack railroad shapefile ---------------------
rail_zip <- file.path(RAW_DIR, "atack_railroads_1826_1911.zip")
rail_dir <- file.path(RAW_DIR, "atack_railroads")

if (!file.exists(rail_zip)) {
  message("Downloading Atack railroad shapefile (~60-100MB)...")
  options(timeout = max(1200, getOption("timeout")))
  download.file(ATACK_URL, rail_zip, mode = "wb")
}
if (!dir.exists(rail_dir)) unzip(rail_zip, exdir = rail_dir)

shp_path <- list.files(rail_dir, pattern = "\\.shp$",
                       full.names = TRUE, recursive = TRUE)
stopifnot(length(shp_path) >= 1)
rail <- st_read(shp_path[1], quiet = TRUE)

# ---- Step 2: Restrict to pre-cutoff track and clean --------------------------
# Field `InOpBy` = year segment was in operation by (per Atack's metadata).
# Field names occasionally vary in case across releases; match defensively.
inop_field <- names(rail)[tolower(names(rail)) == "inopby"]
stopifnot(length(inop_field) == 1)

rail <- rail |>
  rename(InOpBy = all_of(inop_field)) |>
  mutate(InOpBy = suppressWarnings(as.integer(InOpBy))) |>
  filter(!is.na(InOpBy), InOpBy > 0, InOpBy <= YEAR_CUTOFF) |>
  st_transform(EA_CRS) |>
  st_make_valid()

message(sprintf("Kept %d rail segments in operation by %d.",
                nrow(rail), YEAR_CUTOFF))

# ---- Step 3: County polygons --------------------------------------------------
# [SWAP POINT for historical boundaries: replace this block with
#  st_read() of an NHGIS county shapefile and rename its FIPS column GEOID.]
cty <- counties(cb = TRUE, year = 2020, progress_bar = FALSE) |>
  filter(!STATEFP %in% c("02", "15", "60", "66", "69", "72", "78")) |>  # CONUS
  st_transform(EA_CRS) |>
  st_make_valid() |>
  select(GEOID, STATEFP, NAME, ALAND) |>
  mutate(county_km2 = ALAND / 1e6)

# Single unioned rail layer used for splitting
rail_union <- st_union(st_geometry(rail))

# ---- Step 4: Track length and density by county -------------------------------
message("Computing track length per county...")
rail_by_cty <- st_intersection(
  st_geometry(cty) |> st_sf(GEOID = cty$GEOID),
  rail_union
)
rail_by_cty$rail_km <- as.numeric(set_units(st_length(rail_by_cty), "km"))

len_tab <- rail_by_cty |>
  st_drop_geometry() |>
  group_by(GEOID) |>
  summarise(rail_km = sum(rail_km), .groups = "drop")

# ---- Step 5: Railroad Division Index (RDI) ------------------------------------
# For each county, cut the polygon with the rail network and compute
# 1 - Herfindahl of subpolygon area shares. Checkpointed + restartable.
ckpt <- file.path(CACHE_DIR, sprintf("rdi_checkpoint_%d.rds", YEAR_CUTOFF))
rdi_list <- if (file.exists(ckpt)) readRDS(ckpt) else list()

compute_rdi <- function(poly, lines_union) {
  lines_clip <- suppressWarnings(st_intersection(lines_union, poly))
  if (length(lines_clip) == 0 || all(st_is_empty(lines_clip))) {
    return(c(rdi = 0, n_pieces = 1))
  }
  pieces <- tryCatch(
    suppressWarnings(lwgeom::st_split(poly, lines_clip)) |>
      st_collection_extract("POLYGON"),
    error = function(e) NULL
  )
  if (is.null(pieces) || length(pieces) <= 1) return(c(rdi = 0, n_pieces = 1))
  a <- as.numeric(st_area(pieces))
  a <- a[a > 0]
  shares <- a / sum(a)
  c(rdi = 1 - sum(shares^2), n_pieces = length(a))
}

todo <- setdiff(cty$GEOID, names(rdi_list))
message(sprintf("RDI: %d counties remaining (checkpoint: %s)",
                length(todo), basename(ckpt)))

for (g in todo) {
  poly <- st_geometry(cty[cty$GEOID == g, ])
  rdi_list[[g]] <- compute_rdi(poly, rail_union)
  if (length(rdi_list) %% 100 == 0) {
    saveRDS(rdi_list, ckpt)
    message(sprintf("  ...%d / %d counties done", length(rdi_list), nrow(cty)))
  }
}
saveRDS(rdi_list, ckpt)

rdi_tab <- tibble(
  GEOID    = names(rdi_list),
  rdi      = vapply(rdi_list, `[[`, numeric(1), "rdi"),
  n_rail_pieces = vapply(rdi_list, `[[`, numeric(1), "n_pieces")
)

# ---- Step 6: Assemble and save ------------------------------------------------
out <- cty |>
  st_drop_geometry() |>
  select(GEOID, STATEFP, county_name = NAME, county_km2) |>
  left_join(len_tab, by = "GEOID") |>
  left_join(rdi_tab, by = "GEOID") |>
  mutate(
    rail_km         = coalesce(rail_km, 0),
    rail_any        = as.integer(rail_km > 0),
    rail_km_per_km2 = rail_km / county_km2,
    year_cutoff     = YEAR_CUTOFF
  )

out_path <- file.path(DERIVED_DIR,
                      sprintf("atack_rail_county_instruments_%d.csv", YEAR_CUTOFF))
write_csv(out, out_path)
message("Saved: ", out_path)

# Quick sanity checks ------------------------------------------------------------
message(sprintf("Share of counties with any pre-%d rail: %.2f",
                YEAR_CUTOFF, mean(out$rail_any)))
print(summary(out$rdi))
