# USDA ARMS
# 2024-09-30

# Exploring bulk download (national level)
# https://www.ers.usda.gov/data-products/arms-farm-financial-and-crop-production-practices/
# Use the api to get state level



# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  purrr,
  skimr,
  readr
)

dat <- read_csv('1_raw/2022-Bulk_Data_file_2022.csv')



# Explore -----------------------------------------------------------------


get_str(dat)

# States
dat$state %>% get_table()
# everything is all survey states. Not that helpful. Need API for state data

# Check out variable names
dat$variable_name %>% 
  unique %>% 
  sort

# Along with descriptions...
dat %>% 
  select(variable_name, variable_description) %>% 
  unique() %>% 
  arrange(variable_name) %>% 
  print(n = 200)

# Make a CSV
dat %>% 
  select(variable_name, variable_description) %>% 
  unique() %>% 
  arrange(variable_name) %>% 
  write_csv('6_outputs/usda_arms_variables.csv')



# Test API ----------------------------------------------------------------


ARMS_API_KEY <- Sys.getenv('ARMS_API_KEY')

pacman::p_load(
  dplyr,
  httr,
  jsonlite,
  glue,
  memoise,
  purrr,
  readr
)

api_endpoint <- 'https://api.ers.usda.gov/data/arms/'

url <- glue(
  api_endpoint,
  'state',
  '?api_key={ARMS_API_KEY}'
)

url

# Get state info
out <- GET(url) %>%
  content(as = 'text') %>%
  fromJSON()
get_str(out)

# Check states
out$data$name
# Only 16 states represented. Nothing in New England.
# This will not work for us.


