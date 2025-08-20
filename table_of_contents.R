#' Table of Contents
#' 2025-08-04 update

#' This ToC links to all code used in SM data collection, wrangling, metadata 
#' creation, and export to sm-docs and sm-explorer repos. To navigate to a 
#' script, use `F2` or `CTRL + LEFT CLICK` on the path to use the definition
#' function.

#' Note that the `setup.R` script must be run before anything else in the 
#' project, as it loads the projecter package with convenience functions use
#' throughout the project, as well as the SMdata package, which provides 
#' utility functions.



# Housekeeping ------------------------------------------------------------


# Load SMdata project, essential packages
source('4_scripts/setup.R')

# Prep keys to NE state and county fips codes, also county spatial layers
source('4_scripts/spatial_wrangling.R')

# Make a key for NAICS codes used in NASS and BLS
source('4_scripts/naics_key.R')



# Workflow ----------------------------------------------------------------
## API Calls ---------------------------------------------------------------


# Scripts with API calls are kept here. Raw API responses are in
# 5_objects/api_outs/, then the wrangle scripts below will take those responses
# and clean them and save them properly.
'4_scripts/api_calls/nass_api.R'
'4_scripts/api_calls/census_api.R'
'4_scripts/api_calls/usdm_api.R'
'4_scripts/api_calls/fda_api.R'
'4_scripts/api_calls/cdc_api.R'

# Currently using bulk download for BLS due to unwieldy API
# '4_scripts/api_calls/bls_api.R'



## Wrangle Data ------------------------------------------------------------


# These scripts take data from 1_raw/ (or 5_objects in the case of API calls),
# pull and wrangle relevant metrics from them, and create associated metadata
# files. Metrics are saved in long format to 5_objects/metrics/ and metadata
# is saved in wide format to 5_objects/metadata/ for each script. Scripts are
# organized somewhat thematically, or sometimes functionally as seemed 
# appropriate at the time, even though it might no longer be appropriate.

# USDA National Agricultural Statistics Service
source('4_scripts/nass.R')

# American Community Survey (ACS5)
source('4_scripts/census.R')

# BLS Quarterly Census of Employment and Wages
# ERS Farm and Income Wealth Statistics
source('4_scripts/bls_ers.R')

# University of Wisconsin - County Health Rankings
source('4_scripts/county_health_rankings.R')

# Feeding America - Map the Meal Gap
source('4_scripts/map_meal_gap.R')

# Other - EPA, USDA bee surveys, FSA disaster declarations, BEA GDP by industry,
# FDA recall enforcement, USDM drought
source('4_scripts/other_datasets.R')

# iNaturalist 
source('4_scripts/inaturalist.R')

# Forest Inventory Analysis
source('4_scripts/fia.R')

# Checking out ICPSR uniform crime data. In progress
'4_scripts/icpsr.R' 

# Spatial data - MRLC LULC, other spatial data. Note long run time
source('4_scripts/spatial.R')



## Export ------------------------------------------------------------------


# This script take the metrics data, metadata, and all associated files and
# datasets, like framework trees for graphs, helper datasets like fips keys,
# spatial data like counties and states, does some last minute checks, and puts
# them all together into two rds list objects. Then the export script sends 
# those to sm-docs and sm-explorer
source('4_scripts/export_data.R')



# Miscellany --------------------------------------------------------------


# Explore exported data
'4_scripts/explore.R'

# Get parameter values for NASS API - saved into object for reference.
'4_scripts/get_param_options.R'

# Explore bulk download from USDA ARMS. ARMS isn't helpful for us though.
'4_scripts/arms.R'

# USDA ERS Food Environment Atlas
'4_scripts/usda_food_environment_atlas.R'

# Sandbox for temporary work
'4_scripts/sandbox.R'


## Secondary paper scripts
'dev/update_excel.R'


## DB 
'webdb/webdb_test.R'
'webdb/get_tables.SQL'
'webdb/upload_csv.SQL'
