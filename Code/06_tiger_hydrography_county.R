# ==============================================================================
# 02_tiger_hydrography_county.R
# Project: bw_segregation_longevity
#
# Purpose: Download the Census TIGER/Line Linear Hydrography files (one file
#          per county, via the tigris package) and construct county-level
#          river/stream instruments in the spirit of the topographical
#          instruments in Cutler & Glaeser (1997, QJE) / Hoxby (1994):
#
#   (1) n_stream_features  - count of stream/river line features
#   (2) n_named_rivers     - count of DISTINCT named rivers/streams
#                            (closest analogue to C&G's "number of rivers";
#                            unnamed intermittent reaches are excluded)
#   (3) stream_km          - total stream/river length (km)
#   (4) stream_km_per_km2  - stream density
#   (5) n_named_rivers_sq  - square of (2), since C&G use quadratic terms
#                            to capture nonlinearity (QJE 1997, p. 853)
#
# Data:  Census TIGER/Line Linear Hydrography (LINEARWATER), county-based
#        shapefiles. "The linear hydrography shapefile includes streams/
#        rivers, braided streams, canals, ditches, artificial paths, and
#        aqueducts" (Census TIGER technical documentation; accessed via
#        tigris::linear_water()).
#
# Feature filter (MTFCC codes):
#   H3010 Stream/River; H3013 Braided Stream.
#   We EXCLUDE H3020 canals/ditches (man-made; endogenous to development)
#   and H1100 connectors/artificial paths. Artificial paths through wide
#   rivers carry the river's FULLNAME, so major rivers are still counted
#   in n_named_rivers via their named stream segments.
#
# Notes:
#   - ~3,100 downloads; tigris caching is on, and results are checkpointed
#     per county so the loop is restartable after any network failure.
#   - Expect 1-3 hours on a typical connection for the first full run.
# ==============================================================================

source(here::here("Code", "00_setup_instrument.R"))

TIGER_YEAR <- 2023
STREAM_MTFCC <- c("H3010", "H3013")

# ---- Step 1: County universe ---------------------------------------------------
cty <- counties(cb = TRUE, year = 2020, progress_bar = FALSE) |>
  filter(!STATEFP %in% c("02", "15", "60", "66", "69", "72", "78")) |>  # CONUS
  st_drop_geometry() |>
  select(GEOID, STATEFP, COUNTYFP, NAME, ALAND) |>
  mutate(county_km2 = ALAND / 1e6) |>
  arrange(GEOID)

# ---- Step 2: Loop over counties, checkpointed -----------------------------------
ckpt <- file.path(CACHE_DIR, sprintf("hydro_checkpoint_%d.rds", TIGER_YEAR))
hydro_list <- if (file.exists(ckpt)) readRDS(ckpt) else list()

summarise_county_water <- function(stfp, cofp) {
  lw <- tryCatch(
    suppressMessages(
      linear_water(state = stfp, county = cofp,
                   year = TIGER_YEAR, progress_bar = FALSE)
    ),
    error = function(e) NULL
  )
  if (is.null(lw) || nrow(lw) == 0) {
    return(tibble(n_stream_features = 0, n_named_rivers = 0, stream_km = 0,
                  download_failed = is.null(lw)))
  }
  streams <- lw |> filter(MTFCC %in% STREAM_MTFCC)
  if (nrow(streams) == 0) {
    return(tibble(n_stream_features = 0, n_named_rivers = 0, stream_km = 0,
                  download_failed = FALSE))
  }
  streams <- st_transform(streams, EA_CRS)
  km <- as.numeric(units::set_units(st_length(streams), "km"))
  named <- streams$FULLNAME[!is.na(streams$FULLNAME) & streams$FULLNAME != ""]
  tibble(
    n_stream_features = nrow(streams),
    n_named_rivers    = dplyr::n_distinct(named),
    stream_km         = sum(km),
    download_failed   = FALSE
  )
}

todo <- cty |> filter(!GEOID %in% names(hydro_list))
message(sprintf("Hydrography: %d counties remaining (checkpoint: %s)",
                nrow(todo), basename(ckpt)))

for (i in seq_len(nrow(todo))) {
  g <- todo$GEOID[i]
  hydro_list[[g]] <- summarise_county_water(todo$STATEFP[i], todo$COUNTYFP[i])
  if (length(hydro_list) %% 50 == 0) {
    saveRDS(hydro_list, ckpt)
    message(sprintf("  ...%d / %d counties done", length(hydro_list), nrow(cty)))
  }
}
saveRDS(hydro_list, ckpt)

# ---- Step 3: Assemble, retry failures once, and save ----------------------------
hydro_tab <- bind_rows(hydro_list, .id = "GEOID")

failed <- hydro_tab |> filter(download_failed) |> pull(GEOID)
if (length(failed) > 0) {
  message(sprintf("Retrying %d failed downloads...", length(failed)))
  for (g in failed) {
    row <- cty[cty$GEOID == g, ]
    hydro_list[[g]] <- summarise_county_water(row$STATEFP, row$COUNTYFP)
  }
  saveRDS(hydro_list, ckpt)
  hydro_tab <- bind_rows(hydro_list, .id = "GEOID")
}
if (any(hydro_tab$download_failed)) {
  warning("Some counties still failed to download; flagged in output. ",
          "Re-run this script to retry (checkpoint preserved).")
}

out <- cty |>
  select(GEOID, STATEFP, county_name = NAME, county_km2) |>
  left_join(hydro_tab, by = "GEOID") |>
  mutate(
    stream_km_per_km2 = stream_km / county_km2,
    n_named_rivers_sq = n_named_rivers^2,
    tiger_year        = TIGER_YEAR
  )

out_path <- file.path(DERIVED_DIR,
                      sprintf("tiger_hydrography_county_instruments_%d.csv", TIGER_YEAR))
write_csv(out, out_path)
message("Saved: ", out_path)

print(summary(out |> select(n_stream_features, n_named_rivers, stream_km)))
