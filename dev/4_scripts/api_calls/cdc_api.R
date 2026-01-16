# CDC API
# 2025-07-07


# Description -------------------------------------------------------------

# Trying out CDC API for environmental health tracking. As it turns out, some
# of the data they purportedly host does not seem as complete as it looks in the
# UW county health statistics. The only thing we are taking from this API right
# now is air quality.


# Housekeeping ------------------------------------------------------------

pacman::p_load(
  dplyr,
  httr,
  jsonlite,
  glue,
  memoise,
  purrr,
  readr,
  stringr
)


# Explore -----------------------------------------------------------------


## Content areas
url <- 'https://ephtracking.cdc.gov/apigateway/api/v1/contentareas/json'
content_areas <- GET(url) %>%
  content(as = 'text') %>%
  fromJSON()
content_areas

# Get the one for air quality, demographics, drinking water
(content_code <- content_areas %>% 
  filter(str_detect(name, 'Air Quality|Demographics|Drinking Water|Heart Disease|Health Status')) %>% 
  pull(id) %>% 
  paste0(collapse = ','))


## Indicators 
url <- glue('https://ephtracking.cdc.gov/apigateway/api/v1/indicators/11')
indicators <- GET(url) %>%
  content(as = 'text') %>%
  fromJSON()
get_str(indicators)
indicators[, 1:3]

# Get the one for air quality
(indicator_code <- indicators %>% 
  filter(name == 'Current and Historical Air Quality') %>% 
  pull(id))


## Measures within an indicator
url <- glue('https://ephtracking.cdc.gov/apigateway/api/v1/measures/204')
measures <- GET(url) %>%
  content(as = 'text') %>%
  fromJSON()
measures[, 1:3]

# Want monitor data, not modeled
(measure_code <- measures %>% 
  filter(str_detect(name, 'Monitor Data')) %>% 
  pull(id))


## Geographic types for our measure
url <- glue('https://ephtracking.cdc.gov/apigateway/api/v1/geographicTypes/{measure_code}')
geo <- GET(url) %>%
  content(as = 'text') %>%
  fromJSON()
geo

# Pull geographic type id
(geo_code <- filter(geo, geographicType == 'County') %>% pull(geographicTypeId))


## Temporal level
url <- glue(
  'https://ephtracking.cdc.gov/apigateway/api/v1/temporalItems/',
  '{measure_code}/',
  '{geo_code}/ALL/ALL' # typeid, typeid filter, items filter (state)
)
temp <- GET(url) %>%
  content(as = 'text') %>%
  fromJSON()
temp
# Just want all


## Stratification
url <- glue(
  'https://ephtracking.cdc.gov/apigateway/api/v1/stratificationlevel/',
  '7/',
  '2/',
  '0'
)
strat <- GET(url) %>%
  content(as = 'text') %>%
  fromJSON()
strat 

# only one option, id2
strat_code <- strat$id



# Prep --------------------------------------------------------------------

# Format

# ephtracking.cdc.gov/apigateway/api/{version}/getCoreHolder/{measureId}/{stratificationLevelId}/
# {geographicTypeIdFilter}/{geographicItemsFilter}/{temporal}/{isSmoothed}/{getFullCoreHolder}
# [?stratificationLevelLocalIds][?apiToken]

# NE county fips
fips <- fips_key %>% 
  filter(str_length(fips) == 5) %>% 
  pull(fips) %>% 
  paste0(collapse = ',')

# Codes for variables
params <- data.frame(
  'lifeExpectancy' = c(43, 137, 874),
  'waterDomainIndex' = c(43, 200, 1201),
  'prevHeartDisease' = c(4, 193, 1119),
  'prevDiabetes' = c(13, 109, 7)
)



# API ---------------------------------------------------------------------


# Works for heart disease at least
url <- glue(
 'https://ephtracking.cdc.gov/apigateway/api/v1/getCoreHolder/',
 '1119/',
 '2/', # strat
 '2/', # geo type id filter for county
 '{fips}/', # geo items filter (fips)
 '2019/', # temporal filter
 '0/', # smoothed
 '0' # getFullCoreHolder
)

# Try just air quality
'https://ephtracking.cdc.gov/apigateway/api/v1/getCoreHolder/87/2/all/all/1/2022,2021,2020,2019,2018,2017,2016,2015,2014,2013,2012,2011,2010,2009,2008,2007,2006,2005,2004,2003,2002,2001,2000,1999/0/0'
url <- glue(
 'https://ephtracking.cdc.gov/apigateway/api/v1/getCoreHolder/',
 '85/',
 '2/', # strat
 'all/',
 '{fips}/',
 '1/',
 '2022,2021,2020,2019,2018,2017,2016,2015,2014,2013,2012,2011,2010,2009,2008,2007,2006,2005,2004,2003,2002,2001,2000/',
 '0/', # smoothed
 '0' # getFullCoreHolder
)

out <- GET(url) %>%
  content(as = 'text') %>%
  fromJSON()
get_str(out)

# Save output
saveRDS(out, '5_objects/api_outs/cdc_airquality_ne_counties.rds')
