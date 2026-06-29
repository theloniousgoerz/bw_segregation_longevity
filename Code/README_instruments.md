# Instrument construction: pre-1911 railroads (RDI) and rivers/streams

Place these three scripts in:
  /Users/theloniousgoerz/Academic/Projects/bw_segregation_longevity/Code

All data lands in:
  /Users/theloniousgoerz/Academic/Projects/bw_segregation_longevity/Data
    raw/      downloaded source files (Atack zip + shapefiles)
    cache/    tigris cache + restartable checkpoints
    derived/  final county-level CSVs (merge on 5-digit GEOID/FIPS)

## Run order
1. 00_setup.R   - paths + packages (sf, lwgeom, tigris, dplyr, purrr, readr, units)
2. 01_atack_railroads_county.R
3. 02_tiger_hydrography_county.R   (scripts 01 and 02 are independent)

## Outputs
- derived/atack_rail_county_instruments_1911.csv
    rail_any, rail_km, rail_km_per_km2, rdi (Ananat 2011 Railroad Division
    Index = 1 - Herfindahl of rail-delineated subpolygon area shares),
    n_rail_pieces
- derived/tiger_hydrography_county_instruments_2023.csv
    n_stream_features, n_named_rivers (+ its square, since Cutler & Glaeser
    1997 use quadratics in rivers), stream_km, stream_km_per_km2

## Runtime
Script 01: the st_split/RDI loop is ~10-30 min for all CONUS counties.
Script 02: ~3,100 county downloads; 1-3 hours on first run. Both scripts
checkpoint every 50-100 counties and resume where they left off.

## Specification notes
- YEAR_CUTOFF in script 01 defaults to 1911 (the full Atack network). For a
  strictly pre-Great Migration network use 1910 or 1900 and rerun.
- Chyn, Haggag & Stuart (NBER WP 30563) discuss whether to control for track
  length per km^2 alongside the RDI (Ananat does; they do not, citing
  Blandhol et al. 2022 and an influential outlier). Both variables are in
  the output so you can test it either way.
- Counties are modern (2020 cartographic boundaries). For historical county
  boundaries, swap NHGIS shapefiles in at the marked block in script 01.
- Hydrography excludes canals/ditches (H3020) and artificial paths as
  man-made/endogenous; named wide rivers still enter via named stream
  segments. If you want a measure closer to *navigable* historical rivers,
  Atack's steamboat-navigated rivers shapefile (same download page) can be
  processed with the same county-intersection code as script 01.

## Data citations
- Atack, Jeremy. "Historical Geographic Information Systems (GIS) database
  of U.S. Railroads for [years per .dbf field 'InOpBy']" (May 2016; revised
  Oct 31, 2023). https://my.vanderbilt.edu/jeremyatack/data-downloads/
  (Also archived as ICPSR 36353.)
- U.S. Census Bureau. TIGER/Line Shapefiles, Linear Hydrography
  (county-based), accessed via the R tigris package.

## Method citations
- Ananat, E.O. (2011). "The Wrong Side(s) of the Tracks." AEJ: Applied 3(2).
- Chyn, Haggag & Stuart (2022). NBER WP 30563.
- Cutler, D. & Glaeser, E. (1997). "Are Ghettos Good or Bad?" QJE 112(3),
  esp. pp. 853-54 on rivers as instruments.
- Hoxby, C.M. (1994). NBER WP 4979 (origin of topographical instruments).
