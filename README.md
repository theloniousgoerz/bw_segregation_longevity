# Racial Segregation and Black White Longevity Disparities 

## Replication Package

This repository includes code to replicate all figures and tables in the paper. Please note that to run the replication code, you will have to download the publicly-available (1) IPUMS Full Count 1940 Census Data; (2) CenSoc NUMIDENT v3 and Geographic Supplement V2; (3) The Government Finance Database maintained by The Wilamette University Atkinson Graduate School of Management; (4) Decennial Census Data (detailed below); and (5) United States Department of Agriculture (USDA) Rural-Urban Continuum Codes (detailed below). Note to replicators: This code was run on a MacBook Pro M2 Laptop with 64 GB of RAM.

    Clone this repository
    Download Data and update scripts to point towards your data files
    Run 00_Run_All.R to reproduce data processing, cleaning, and main tables and figures.

### Deccenial Census Data 

All data unless otherwise noted are sourced from IPUMS and NHGIS. 
Citation: Steven Manson, Jonathan Schroeder, David Van Riper, Katherine Knowles, Tracy Kugler, Finn Roberts, and Steven Ruggles.
        IPUMS National Historical Geographic Information System: Version 18.0 
        [dataset]. Minneapolis, MN: IPUMS. 2023.
        http://doi.org/10.18128/D050.V18.0
--------------------------------------------------------------------------------
NHIGIS Data Summary
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
   
 ### Rural-Urban Codes 
--------------------------------------------------------------------------------
RUC Data Summary
--------------------------------------------------------------------------------
 Link to download RUC code data: https://www.ers.usda.gov/data-products/rural-urban-continuum-codes/documentation

