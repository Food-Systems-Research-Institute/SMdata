# Map the Meal Gap
# 2025-06-02


# Description -------------------------------------------------------------


# Pulling data from Feeding America - Map the Meal Gap. Available by request
# only. Also note that there are some serious compatibility issues with the
# time series here. See more details in the technical document.

# https://www.feedingamerica.org/research/map-the-meal-gap/by-county
# https://www.feedingamerica.org/research/map-the-meal-gap/how-we-got-the-map-data



# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  purrr,
  stringr,
  readr,
  readxl,
  janitor,
  tidyr
)



# Load and Wrangle --------------------------------------------------------


# File paths and names
paths <- list.files(
  '1_raw/feeding_america_mmg/',
  full.names = TRUE
)
names <- str_split_i(paths, '/', 3)

# Relevant sheet has 'County' in it
# Also replacing symbols with letters
# And removing one row from banner in 2020 dataset
mmg <- map(paths, ~ {
  sheet <- str_subset(excel_sheets(.x), regex('County$'))
  print(sheet)
  out <- read_excel(.x, sheet = sheet)
  if (str_detect(names(out)[1], '^Data')) {
    out <- read_excel(.x, sheet = sheet, skip = 1)
  }
  out <- out %>% 
    setNames(c(
      names(out) %>% 
        str_replace('≤', 'lte') %>% 
        str_replace('>', 'gt')
    )) %>% 
    janitor::clean_names()
  return(out)
}) %>% 
  setNames(c(paste0('mmg', str_sub(names, 4, 7))))
map(mmg, get_str)

# Now pull relevant columns: food insecurity rate, child food insecurity, 
# cost per meal, budget shortfall. Also fips
pattern <- paste0(
  c(
    'fips',
    'food_insecurity_rate',
    'cost_per_meal',
    'shortfall',
    'fi_rate',
    'weighted_annual_dollars'
  ),
  collapse = '|'
)

# pull those columns from each. also add a column with year
map(mmg, get_str)
mmg <- imap(mmg, \(wave, name) {
  data_year <- as.numeric(str_sub(name, start = 4)) - 2
  if (!'year' %in% names(wave)) {
    out <- mutate(wave, year = data_year)
  } else {
    out <- wave
  }
  out <- out %>% 
    select(matches(pattern) & !matches('*among*'), year) %>% 
    mutate(
      across(everything(), as.character),
      fips = case_when(
        str_length(fips) == 4 ~ paste0(0, fips),
        .default = fips
    )) %>% 
    setNames(c(
      names(.) %>% 
        str_replace('fi_rate', 'food_insecurity_rate') %>% 
        str_replace('weighted_annual_dollars', 'weighted_annual_food_budget_shortfall') %>% 
        str_replace('weighted_annual_food_budget_shortfall', 'shortfall') %>% 
        str_remove('x[0-9]{4}_') %>% 
        str_remove('overall_')
    ))
  return(out)
})
map(mmg, get_str)

# Check for missing
map(mmg, ~ sum(is.na(.x$fips)))

# Now put them together and fix n/a s
# Also fix names
mmg <- mmg %>% 
  bind_rows() %>% 
  mutate(across(everything(), ~ case_when(
    .x == 'n/a' ~ NA_character_,
    .default = .x
  ))) %>% 
  setNames(c(snakecase::to_lower_camel_case(names(.))))
get_str(mmg)

# Pivot longer to put in metrics format
mmg <- mmg %>% 
  pivot_longer(
    cols = !c(fips, year),
    names_to = 'variable_name',
    values_to = 'value'
  ) 
get_str(mmg)

# Check for missing data
sum(is.na(mmg$fips))
# 2009 dataset is missing some fips somehow. Remove these
mmg <- mmg %>% 
  drop_na()
get_str(mmg)
# Good to go



# Metadata ----------------------------------------------------------------


(vars <- meta_vars(mmg))


meta <- tibble(
  variable_name = vars
) %>% 
  mutate(
    dimension = 'health',
    index = 'food security',
    indicator = c(
      'food security tbd',
      'food affordability',
      'food security tbd',
      'food affordability'
    ),
    metric = c(
      'Child food insecurity rate',
      'Cost per meal',
      'Overall food insecurity rate',
      'Annual food budget shortfall'
    ),
    definition = c(
      'Proportion of children who are food insecure, as defined by a household with children answering affirmatively to at least 3 of 18 questions in the "children in food-insecure households" module of the Current Population Survey.',
      'Average cost of a meal, based on NielsenIQ data of UPC-coded food items in each county mapped to 24 food categories from the USDA Thrift Food Plan.',
      'Proportion of inhabitants who are food insecure, as defined by answering 3 or more questions affirmatively in the Core Food Security Module of the Current Population Survey. Note that data from 2018 or later cannot be compared to data from 2017 or later due to changes in methodology.',
      'USD needed to pay for food insecure households to meet their dietary needs, for seven months of the year (average time in which FI people experience the condition). National average shortfall weighted by the cost-of-food index to estiamte local shortfalls.'
    ),
    axis_name = c(
      'Child FI',
      'Cost per Meal',
      'Overall FI',
      'Shortfall'
    ),
    units = c(
      'proportion',
      'usd',
      'proportion',
      'usd'
    ),
    scope = 'national',
    resolution = meta_resolution(mmg),
    year = meta_years(mmg),
    latest_year = meta_latest_year(mmg),
    updates = "annual",
    source = 'Feeding America: Map the Meal Gap (2025). An analysis of county and congressional district food insecurity and couty food cost in the United States',
    url = 'https://map.feedingamerica.org/',
    warehouse = FALSE
  ) %>% 
    meta_citation(date = '2025-06-06')
get_str(meta)



# Save --------------------------------------------------------------------


## Check to make sure we have the same number of metrics and metas
check_n_records(mmg, meta, 'Map the Meal Gap')

# Save them
saveRDS(mmg, '5_objects/metrics/map_meal_gap.RDS')
saveRDS(meta, '5_objects/metadata/mmg_meta.RDS')

clear_data(gc = TRUE)
