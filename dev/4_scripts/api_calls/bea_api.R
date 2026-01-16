# BEA API
# 2025-07-04


# Description -------------------------------------------------------------

# Get county GDP data, both totals and for Farming (NAICS 11), from BEA API


# Housekeeping ------------------------------------------------------------


pacman::p_load(
  httr,
  GET,
  jsonlite,
  glue,
  dplyr,
  purrr
)

# Use BEA API key
bea_key <- Sys.getenv('BEA_API_KEY')



# Parameters --------------------------------------------------------------


# Check parameters for CAGDP2 Regional data
base <- 'https://apps.bea.gov/api/data/?&UserID={bea_key}'

# Set up query
param <- 'LineCode'
url <- glue(
  base,
  '&method=GetParameterValuesFiltered',
  '&datasetname=Regional',
  '&TargetParameter={param}',
  '&TableName=CAGDP2',
  '&ResultFormat=json'
)

# Query
res <- GET(url)
res$status_code
res$all_headers

# Format
out <- content(res, as = 'text') %>%
  fromJSON() %>% 
  .[[1]] %>% 
  .[['Results']]
get_str(out)
# Two things we want:
# 1: all industries
# 3: ag, forestry, farming, fishing (NAICS 11)
# Note that we can only query one line code (industry) at at time



# API Call ----------------------------------------------------------------


## Mapping over our 2 calls for line codes 1 and 3
# Set parameters
dataset_name <- 'Regional'
line_codes <- c('1', '3')
year <- 'ALL'
table <- 'CAGDP2'
fips <- fips_key %>%
  filter(str_length(fips) == 5) %>%
  pull(fips) %>% 
  paste0(collapse = ',')

# Map over line codes to make requests
out <- map(line_codes, \(line_code) {
  
  # Glue query together
  url <- glue(
    base,
    '&method=GetData',
    '&DataSetName={dataset_name}',
    '&Year={year}',
    '&LineCode={line_code}',
    '&TableName={table}',
    '&Frequency=A',
    '&GeoFips={fips}',
    '&ResultFormat=json'
  )
  
  # Send query
  res <- GET(url)
  
  # Just take Results (includes status, notes, dimensions)
  out <- content(res, as = 'text') %>%
    fromJSON() %>% 
    .[[1]] %>% 
    .[['Results']]
})

get_str(out, 3)

# Save this as api output to be wrangled
saveRDS(out, '5_objects/api_outs/bea_county_gdp.rds')

clear_data(gc = TRUE)
