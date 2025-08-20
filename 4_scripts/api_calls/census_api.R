# Census API
# 2025-08-05


# Description -------------------------------------------------------------

# Using SMdata::call_census_api() to pull data from American Community Survey
# 5 year. 

# Note: Should lump in gini into same calls next time we run this



# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  httr,
  jsonlite,
  glue,
  purrr,
  stringr,
  janitor,
  censusapi,
  glue
)

census_key <- Sys.getenv("CENSUS_API_KEY")



# Testing -----------------------------------------------------------------


# Try out censusapi
apis <- listCensusApis()
get_str(apis)

# Specifics about acs5
acs_apis <- listCensusApis(name = 'acs/acs5', vintage = 2022)

# Metadata for acs5
meta <- listCensusMetadata('acs/acs5', vintage = 2022)
get_str(meta)

# Check for education
meta %>% 
  filter(
    group == 'B15003',
  )
    # str_detect(label, regex('doctorate', ignore_case = TRUE)))
         
# Check acs1 also - might want population from here
out <- listCensusMetadata('acs/acs1', vintage = 2022)



# Prep --------------------------------------------------------------------


# Get vector of northeast states
state_codes <- fips_key %>% 
  filter(str_length(fips) == 2, fips != '00') %>% 
  pull(fips)

# Years
years <- seq(2008, 2023, 5)

# Variables
vars <- list(
  # Population
  'population5Year' = 'B01003_001E',
  
  # Education
  # To get prop with HS/GED or higher, have to take total and substract:
  #   doctorate, master, bachelor, associate, some college, some college < 1yr
  'edTotal' = 'B15003_001E',
  'edTotalHS' = 'B15003_017E',
  'edTotalGED' = 'B15003_018E',
  
  'edTotalBS' = 'B15003_022E',
  'edTotalPhD' = "B15003_025E",
  'edTotalProf' = "B15003_024E",
  'edTotalMaster' = "B15003_023E",
  'edTotalAssoc' = "B15003_021E",
  'edTotalSomeCollege' = "B15003_020E",
  'edTotalSomeCollegeLessThanYear' = "B15003_019E",
  
  # Housing
  'nHousingUnits' = 'B25001_001E',
  'nHousingOccupied' = 'B25002_002E',
  'nHousingVacant' = 'B25002_003E',
  
  # Rent by bedrooms
  'rentMedian1BR' = 'B25031_003E',
  'rentMedian4BR' = 'B25031_006E',
  
  # Housing age
  'medianHousingYear' = 'B25035_001E',
   
  # More rent
  'rentMedian' = 'B25064_001E',
  'rentMedianPercHH' = 'B25071_001E',
  
  # Wages in FFF
  'medianFemaleEarningsFFF' = 'B24022_067E',
  'medianMaleEarningsFFF' = 'B24022_031E',
  'medianFemaleEarningsFPS' = 'B24022_060E',
  'medianMaleEarningsFPS' = 'B24022_024E',
  
  # Gini coefficient
  'gini' = 'B19083_001E',
  
  # disconnected youth (method from UW county health)
  # Add all of these up, divide by n16to19
  'n16to19' = 'B14005_001E',
  'nMaleNotEnrolledHSGradNotInLaborForce' = 'B14005_011E',
  'nMaleNotEnrollNoGradNotInLaborForce' = 'B14005_015E',
  'nFemaleNotEnrollHSGradNotInLaborForce' = 'B14005_025E',
  'nFemaleNotEnrollNoGradNotInLaborForce' = 'B14005_029E'
)


# Save this as crosswalk to name them later
saveRDS(vars, '5_objects/census_acs5_crosswalk.rds')



# API Calls ---------------------------------------------------------------


# ACS5 state data
states_out <- call_census_api(
  state_codes = state_codes,
  years = years,
  vars = vars,
  census_key = census_key,
  region = 'state'
)
get_str(states_out)  
saveRDS(states_out, 'temp/states_out.rds')

# ACS5 county data
county_out <- call_census_api(
  state_codes = state_codes,
  years = years,
  vars = vars,
  census_key = census_key,
  region = 'county'
)
get_str(county_out)  
saveRDS(county_out, 'temp/county_out.rds')

# ACS1 county data
acs1_county_out <- call_census_api(
  state_codes = state_codes,
  years = 2000:2024,
  vars = 'B01003_001E',
  census_key = census_key,
  region = 'county',
  survey_name = 'acs/acs1'
)
get_str(acs1_county_out)
saveRDS(acs1_county_out, 'temp/acs1_county_out.rds')

# ACS1 state data
acs1_states_out<- call_census_api(
  state_codes = state_codes,
  years = 2000:2024,
  vars = 'B01003_001E',
  census_key = census_key,
  region = 'state',
  survey_name = 'acs/acs1'
)
get_str(acs1_states_out)
saveRDS(acs1_states_out, 'temp/acs1_states_out.rds')



# Combine -----------------------------------------------------------------


# Combine and rename ACS5 variables
acs5 <- bind_rows(states_out, county_out) %>% 
  rename(!!!setNames(vars, names(vars)))
get_str(acs5)

# Give population from ACS1 a different name, then bind to rest
acs1 <- bind_rows(acs1_county_out, acs1_states_out) %>% 
  rename(populationAnnual = B01003_001E)
get_str(acs1)

# Save all together
saveRDS(acs1, '5_objects/api_outs/acs1_neast_counties_states_2000_2023.rds')
saveRDS(acs5, '5_objects/api_outs/acs5_neast_counties_states_2008_2023.rds')


