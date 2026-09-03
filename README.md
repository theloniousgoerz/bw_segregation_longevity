# Segregation, Political Economy, and The Longevity of Black and White Americans

## Replication Package

This repository includes code to replicate all figures and tables in the paper. Reproducing it end to end requires downloading the raw data listed under [Data](#data) below and placing each file at the path given there. Note to replicators: this code was run on a MacBook Pro M2 laptop with 64 GB of RAM; `Code/06_tiger_hydrography_county.R` in particular can take 1-3 hours on a first run (see the note above that step in [00_run_all.R](00_run_all.R)).

    Clone this repository
    Download the data listed below and place each file at the path given
    Run 00_run_all.R to reproduce data processing, cleaning, and all tables and figures

`00_run_all.R` runs every script in `Code/` (which builds the analytic and instrument files under `Data/_Cleaned` and `Data/derived` from the raw downloads), then every script in `Analysis/` (which cleans, models, and produces every table and figure, written to `FigTab/`) -- both in the order each script's inputs require, which is not always numeric filename order (see the comments in `00_run_all.R`).

## Data

All paths below are relative to the repository root. Items marked **(auto-downloaded)** are fetched by the script itself and need no manual download.

| Dataset | Expected path | Used by |
|---|---|---|
| CenSoc-Numident v3 | `Data/Censoc/dataverse_files (2)/censoc_numident_v3.csv` | `Code/01_Numident.R` |
| CenSoc-Numident Geography Supplement v1 | `Data/Censoc/dataverse_files (2)/censoc_numident_geography_supplement_v1.csv` | `Code/01_Numident.R` |
| CenSoc-Numident Siblings v2, exact match | `Data/censoc_numident_siblings_v2/censoc_numident_sibs_exact_match_v2.csv` | `Analysis/01_Data_Cleaning.R` |
| CenSoc-Numident Siblings v2, flexible match | `Data/censoc_numident_siblings_v2/censoc_numident_sibs_flexible_match_v2.csv` | `Analysis/01_Data_Cleaning.R` |
| CenSoc WWII Army Enlistment linked to the 1940 Census, v1.1 | `Data/dataverse_files(4)/censoc_enlistment_census_1940_v1.1.csv` | `Analysis/06_Robustness_Checks.R` |
| IPUMS USA 1940 census/ACS extract (linked sample + DDI) | `Data/Census/usa_00006.csv`, `Data/Census/usa_00007.xml` (+ matching `usa_00007.dat.gz`) | `Code/01_Numident.R` |
| IPUMS USA 1940 100% full-count extract | `Data/Census/usa_00021.ddi.xml` (+ matching `usa_00021.dat.gz`) | `Code/07_1940_segregation.R` |
| IPUMS NHGIS decennial tract data (extract 0017) | `Data/Census/nhgis0017_csv/nhgis0017_ts_nominal_tract copy.csv` | `Code/03_county_data.R` |
| ICPSR county identification crosswalk | `Data/Census/icpsrcnt.xls` | `Code/07_1940_segregation.R` |
| Government Finance Database -- Municipal Data | `Data/GFD/Government Finance Database Municipal Data/MunicipalData.csv` | `Code/02_cofgov_data.R` |
| Government Finance Database -- Township Data | `Data/GFD/Government Finance Database Township Data/TownshipData.csv` | `Code/02_cofgov_data.R` |
| Government Finance Database -- All Data | `Data/GFD/Government Finance Database All Data/The Government Finance Database_All Data.csv` | `Code/04_policy_data.R` |
| State Policy & Politics Database v2.0 (SPPD) | `Data/SPPD/SPPD_V2.0.dta` | `Code/04_policy_data.R` |
| USDA Rural-Urban Continuum Codes, 1983 & 1993 | `Data/Rural_Urban_codes/cd8393.xls` | `Analysis/01_Data_Cleaning.R` |
| USDA Rural-Urban Continuum Codes, 2003 | `Data/Rural_Urban_codes/ruralurbancodes2003.xls` | `Analysis/01_Data_Cleaning.R` |
| EPA county-level annual PM2.5 & cardiovascular mortality rate | `Data/raw/pm25_epa/County_annual_PM25_CMR.csv` | `Analysis/06_Robustness_Checks.R` |
| PCE price index, annual (BEA/FRED series PCEPI) | `Data/derived/pce_price_index_annual.csv` (bundled in repo) | `Code/04_policy_data.R` |
| Jeremy Atack historical railroad GIS, 1826-1911 | `Data/raw/atack_railroads_1826_1911.zip` **(auto-downloaded)** | `Code/05_atack_railroads_county.R` |
| Census TIGER/Line linear hydrography, county boundaries | cached under `Data/cache/` **(auto-downloaded via the `tigris` package)** | `Code/05_atack_railroads_county.R`, `Code/06_tiger_hydrography_county.R` |

Decennial Census tract data and USDA Rural-Urban Continuum Codes are detailed further below.

### Decennial Census Tract Data 

All data unless otherwise noted are sourced from IPUMS and NHGIS (extract 0017; the code reads a file literally named `nhgis0017_ts_nominal_tract copy.csv`, not `nhgis0017_ts_nominal_tract.csv` -- an artifact of the original download, kept here so this matches what `Code/03_county_data.R` actually reads).

Citation: Steven Manson, Jonathan Schroeder, David Van Riper, Katherine Knowles, Tracy Kugler, Finn Roberts, and Steven Ruggles.
        IPUMS National Historical Geographic Information System: Version 18.0 
        [dataset]. Minneapolis, MN: IPUMS. 2023.
        http://doi.org/10.18128/D050.V18.0
--------------------------------------------------------------------------------
NHGIS Data Summary
--------------------------------------------------------------------------------
 
Time series layout:     Time varies by row
Geographic level:       Census Tract (by State--County)
Geographic integration: Nominal
Years:                  1970, 1980, 1990, 2000
 
Tables:
 
1. Total Population
   Selected year(s): 1970, 1980, 1990, 2000
   Code: AV0
 
2. Total Population
   Selected year(s): 1980, 1990, 2000
   Code: B78
 
3. Persons by Race [5*]
   Selected year(s): 1970, 1980, 1990, 2000
   Code: B18
 
4. Persons by Single Race/Ethnicity [5]
   Selected year(s): 1980, 1990, 2000
   Code: CY6

5. Persons Who Are White* by Age [5]
   Selected year(s): 1970, 1980, 1990, 2000
   Code: AC4

6. Persons Who Are Black or African American* by Age [5]
   Selected year(s): 1970, 1980, 1990, 2000
   Code: AD6

Tables 5 and 6 (age by race) are used to build the population characteristics
(`w_Under5` ... `b_65p`) in `Code/03_county_data.R`; they are not present in
older NHGIS extracts of this series.

 ### Rural-Urban Codes 
--------------------------------------------------------------------------------
RUC Data Summary
--------------------------------------------------------------------------------
Two vintages are used, covering the three RUC revisions needed across the 1980/1990/2000 death decades: `cd8393.xls` (1983 and 1993 codes) and `ruralurbancodes2003.xls` (2003 codes).

Link to download RUC code data: https://www.ers.usda.gov/data-products/rural-urban-continuum-codes/documentation

## Notes for Replicators

- `Code/05_atack_railroads_county.R` and `Code/06_tiger_hydrography_county.R` checkpoint their progress (`Data/cache/rdi_checkpoint_1911.rds` and `Data/cache/hydro_checkpoint_2023.rds`), so only a first run involves a full download/processing effort; re-running `00_run_all.R` afterward reuses the checkpoints.
