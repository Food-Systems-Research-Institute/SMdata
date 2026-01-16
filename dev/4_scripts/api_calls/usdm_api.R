# USDM API
# 2025-06-30


# Description -------------------------------------------------------------

# Pull data from US Drought Monitor API


# Housekeeping ------------------------------------------------------------

pacman::p_load(
  dplyr,
  httr,
  jsonlite,
  glue,
  memoise,
  purrr,
  readr,
  stringr,
  tidyr
)



# Rework - Non Consecutive Drought ----------------------------------------


# Weeks of non-consecutive drought by county for northeast
# Drought categories 2 (severe) or higher, since 2000
base <- 'https://usdmdataservices.unl.edu/api/ConsecutiveNonConsecutiveStatistics/'
drought_categories <- c(2, 3, 4)
states <- fips_key %>% 
  filter(!is.na(state_code), state_name != 'US') %>% 
  pull(state_code)
start_dates <- c(paste0('1/1/', 2000:2024))
end_dates <- c(paste0('12/31/', 2000:2024))

## API Call
out <- map2(start_dates, end_dates, \(start_date, end_date){
  Sys.sleep(3)
  call_usdm_api(
    base = base,
    drought_categories = drought_categories,
    states = states,
    start_date = start_date,
    end_date = end_date
  )
}) %>% 
  setNames(c(paste0('y', 2000:2024)))
get_str(out)
get_str(out, 4)

# Save API output
saveRDS(out, '5_objects/api_outs/usdm_weeks_drought_counties_2000_2023.rds')



# Avg Perc Drought by Area ------------------------------------------------


# This is state level - don't really need it, not reworking it for now.

# # Average weekly percent of each state that is in drought (d1 or greater)
# states <- paste0(state_key$state_code, collapse = ',')
# counties <- fips_key %>% 
#   filter(str_length(fips) == 5) %>% 
#   pull(fips) %>% 
#   paste0(collapse = ',')
# start_dates <- c(paste0('1/1/', 2020:2024))
# end_dates <- c(paste0('12/31/', 2020:2024))

# Map over dates for last five years, get data by state
# out <- map2(start_dates, end_dates, \(start, end) {
#   url <- glue(
#     'https://usdmdataservices.unl.edu/api/StateStatistics/GetDroughtSeverityStatisticsByAreaPercent/',
#     '?aoi={states}',
#     '&startdate={start}',
#     '&enddate={end}',
#     '&statisticsType=1'
#   )
#   print(url)
#   out <- GET(url, add_headers(Accept = "text/json")) %>%
#     content(as = 'text') %>%
#     fromJSON()
#   return(out)
# }) %>% 
#   setNames(c(paste0('y', 2020:2024)))
# 
# get_str(out)

# Save output for posterity
# saveRDS(out, '5_objects/api_outs/usdm_perc_area_drought_2020_2024.rds')
