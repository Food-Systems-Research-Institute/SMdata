# USDA Food Environment Atlas
# 2025-01-08


# Description -------------------------------------------------------------

# NOTE: we might not need this after all. Relevant vars already pulled from
# USDA data warehouse. Could revisit this if need be though.


# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  purrr,
  stringr
)

# # Function to pull from Census API, and filter by fips
# source('3_functions/api/get_census_data.R')
# source('3_functions/pipeline_utilities.R')
# source('3_functions/metadata_utilities.R')
# 
# # county fips for New England (differences for CT restructuring)
# fips_key <- readRDS('5_objects/fips_key.rds')
# state_codes <- readRDS('5_objects/state_key.rds')

# lists of results
results <- list()
metas <- list()

# Load food environment atlas data
atlas <- read.csv('1_raw/usda/ers_food_environment_atlas/StateAndCountyData.csv') %>% 
  setNames(c(names(.) %>% str_to_lower()))
get_str(atlas)

# Dictionary - variable definitions
dict <- read.csv('1_raw/usda/ers_food_environment_atlas/VariableList.csv') %>% 
  setNames(c(names(.) %>% str_to_lower()))
get_str(dict)



# Wrangle -----------------------------------------------------------------


