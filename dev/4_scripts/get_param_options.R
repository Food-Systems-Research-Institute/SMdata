# Get ALL options 
# 2024-09-09

pacman::p_load(
  dplyr,
  purrr,
  httr,
  jsonlite,
  glue
)

# Get all options for every relevant parameter. Save for reference
parameters <- c(
  'source_desc',
  'sector_desc',
  'group_desc',
  'commodity_desc',
  'class_desc',
  'prodn_practice_desc',
  'util_practice_desc',
  'statisticcat_desc',
  'unit_desc',
  'short_desc',
  'domaincat_desc',
  'agg_level_desc',
  'state_ansi',
  'state_fips_code',
  'state_alpha',
  'state_name',
  'asd_code',
  'asd_desc',
  'county_ansi',
  'county_code',
  'county_name',
  'region_desc',
  'watershed_code',
  'country_code',
  'country_name',
  'year',
  'freq_desc',
  'begin_code',
  'end_code',
  'reference_period_desc'
)

api_key <- Sys.getenv('NASS_KEY')
base_url <- 'https://quickstats.nass.usda.gov/api/get_param_values/'

par_options <- map(parameters, \(x) {
  url <- paste0(base_url, '?key=', api_key, '&param=', x)
  out <- GET(url) %>%
    content(as = 'text') %>%
    fromJSON() %>% 
    .[[1]]
  return(out)
}) %>% 
  setNames(c(parameters))

get_str(par_options)

saveRDS(par_options, 'projects/secondary_data/par_options.rds')
