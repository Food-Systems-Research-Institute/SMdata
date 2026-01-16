#' Pull NASS Data
#' 2025-07-02 update

# Pulling data from USDA NASS data through NASS API. Doing two separate calls,
# one for the census and one for the survey (yields). 

# Info on NASS QuickStat API:
# https://quickstats.nass.usda.gov/api/


# Note: clean up the testing parts of script, turn census calls into a function
# to use on both survey and census



# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  httr,
  jsonlite,
  glue,
  purrr,
  readr,
  stringr,
  rnassqs
)

# Authorize nass api key
nass_key <- Sys.getenv('NASS_API_KEY')
nassqs_auth(nass_key)

# Define fips - have to split back into 2 digit state and 3 digit county to 
# query with NASS
states_only <- fips_key$fips[str_length(fips_key$fips) == 2]
all_counties <- fips_key %>% 
  filter(str_length(fips) == 5) %>% 
  pull(fips)

# Use these two vectors to query. Goes through counties then states
county_fips <- c(str_sub(all_counties, start = 3), rep('', length(states_only)))
state_fips <- c(str_sub(all_counties, end = 2), states_only)
stopifnot(length(county_fips) == length(state_fips))



# API Calls ---------------------------------------------------------------


# Authorize key
nassqs_auth(nass_key)

# Load API parameters doc
nass_params <- read_csv('5_objects/api_parameters/nass_api_parameters.csv')

# Remove three broken variables
broken <- c(
  'INCOME, FARM-RELATED, FOREST PRODUCTS, (EXCL CHRISTMAS TREES & SHORT TERM WOODY CROPS & MAPLE SYRUP) - RECEIPTS, MEASURED IN $',
  'FARM OPERATIONS, ORGANIC - NUMBER OF OPERATIONS',
  'PRACTICES, ALLEY CROPPING & SILVAPASTURE - NUMBER OF OPERATIONS'
)
nass_params <- filter(nass_params, !short_desc %in% broken)
get_str(nass_params)



## Census Calls ------------------------------------------------------------


# Try going one variable at a time, GE 2000 instead of specific years
census_params <- nass_params %>%
  filter(
    source_desc == 'CENSUS',
    short_desc != 'derived',
    !is.na(short_desc)
  )
get_str(census_params)

# Rework census params for this new purpose
params <- list(
  source_desc = 'CENSUS',
  domain_desc = 'TOTAL',
  agg_level_desc = c('COUNTY', 'STATE'),
  state_fips_code = states_only[1:9],
  year__GE = 2000
)
vars <- census_params$short_desc
out <- imap(vars, ~ {
  cat('\n\nStarting:', .x, '\n', .y, 'of', length(vars), '\n')
  params[['short_desc']] <- .x
  Sys.sleep(0.5)
  tryCatch({
    nassqs(params)
  },
    error = function(e) {
      message('Error. Returning NULL')
      return(NULL)
    }
  )
}, .progress = TRUE) %>% 
  purrr::list_rbind()
get_str(out)

# Check if we got everything
setdiff(census_params$short_desc, out$short_desc)

# Save this to API outs
saveRDS(out, '5_objects/api_outs/neast_nass_census_2002_2022.rds')



## Farm Size Calls ---------------------------------------------------------


# Calling Census farm size variables separately to include domaincat_desc
farm_params <- nass_params %>%
  filter(
    short_desc == 'PRODUCERS, (ALL) - NUMBER OF PRODUCERS',
    str_detect(domaincat_desc, '^AREA OPERATED')
  )
get_str(farm_params)

# Set parameters
params <- list(
  source_desc = 'CENSUS',
  short_desc = farm_params$short_desc,
  domain_desc = 'AREA OPERATED',
  agg_level_desc = c('COUNTY', 'STATE'),
  state_fips_code = states_only[1:9]
)
years <- seq(2002, 2022, 5)
farm_out <- map(years, \(yr) {
  cat(
    '\nDownloading year ', yr, ' (', which(years == yr), ' of ', length(years), ')\n\n',
    sep = ''
  )
  param_set <- params
  param_set[['year']] <- yr
  Sys.sleep(1)
  tryCatch({
    nassqs(param_set)
  },
    error = function(e) {
      message('Error')
      return(NULL)
  })
}) %>% 
  purrr::list_rbind()
str(farm_out)

# Save this to API outs
saveRDS(farm_out, '5_objects/api_outs/neast_nass_farm_2002_2022.rds')



## Organic Call ------------------------------------------------------------


# Pulling this out separately to see if it will work
og_params <- nass_params %>%
  filter(short_desc == 'FARM OPERATIONS, ORGANIC - NUMBER OF OPERATIONS')
get_str(og_params)

# Set parameters
params <- list(
  source_desc = 'CENSUS',
  short_desc = og_params$short_desc,
  domain_desc = 'ORGANIC STATUS',
  domaincat_desc = 'ORGANIC STATUS: (NOP USDA CERTIFIED)',
  agg_level_desc = c('COUNTY', 'STATE'),
  state_fips_code = states_only[1:9]
)
years <- seq(2002, 2022, 5)
og_out <- map(years, \(yr) {
  cat(
    '\nDownloading year ', yr, ' (', which(years == yr), ' of ', length(years), ')\n\n',
    sep = ''
  )
  param_set <- params
  param_set[['year']] <- yr
  Sys.sleep(1)
  tryCatch({
    nassqs(param_set)
  },
    error = function(e) {
      message('Error')
      return(NULL)
  })
}) %>% 
  purrr::list_rbind()
get_str(og_out)

saveRDS(og_out, '5_objects/api_outs/neast_nass_og_2002_2022.rds')



## Survey Calls ------------------------------------------------------------


# Don't need preset parameters, just taking all yield variables
params <- list(
  source_desc = 'SURVEY',
  domain_desc = 'TOTAL',
  statisticcat_desc = c('YIELD', 'PRODUCTION'),
  agg_level_desc = c('COUNTY', 'STATE'),
  state_fips_code = states_only[1:9],
  freq_desc = 'ANNUAL'
)
years <- seq(2002, 2024, 1)
survey_out <- map(years, \(yr) {
  cat(
    '\nDownloading year ', yr, ' (', which(years == yr), ' of ', length(years), ')\n\n',
    sep = ''
  )
  param_set <- params
  param_set[['year']] <- yr
  Sys.sleep(1)
  nassqs(param_set)
}) %>% 
  purrr::list_rbind()
get_str(survey_out)

# Save this to API outs
saveRDS(survey_out, '5_objects/api_outs/neast_nass_survey_2002_2024.rds')



# Clear -------------------------------------------------------------------


clear_data(gc = TRUE)

