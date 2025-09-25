# NASS
# 2025-09-25 update


# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  tidyr,
  vegan,
  readr,
  e1071,
  zoo
)



# Description -------------------------------------------------------------


# Taking our API calls from nass_api.R and combining them. Using the nass 
# api parameters to start metadata
# Note that we can consolidate some of these
census <- readRDS('5_objects/api_outs/neast_nass_census_2002_2022.rds')
census <- readRDS('5_objects/api_outs/neast_nass_census_2002_2022_test.rds') %>% 
  unique()
survey <- readRDS('5_objects/api_outs/neast_nass_survey_2002_2022.rds')
farm <- readRDS('5_objects/api_outs/neast_nass_farm_2002_2022.rds')
og <- readRDS('5_objects/api_outs/neast_nass_og_2002_2022.rds')

# Load nass api parameters to build metadata
nass_params <- read_csv('5_objects/api_parameters/nass_api_parameters.csv')



# Wrangle -----------------------------------------------------------------


# Combine 
bound <- bind_rows(census, survey, farm, og)
get_str(bound)

# Pull relevant variables, create a single FIPS variable where counties have 5
# digits and states have 2
dat <- bound %>% 
  filter(
    freq_desc %in% c('ANNUAL', 'POINT IN TIME'),
    county_code != '998'
  ) %>% 
  select(
    agg_level_desc,
    domaincat_desc,
    short_desc,
    value = Value,
    year,
    county_ansi,
    state_ansi,
    cv = `CV (%)`,
    unit_desc
  ) %>%
  mutate(
    fips = case_when(
      agg_level_desc == 'COUNTY' ~ paste0(state_ansi, county_ansi),
      agg_level_desc == 'STATE' ~ state_ansi,
      .default = NA
    ),
    .keep = 'unused'
  )
get_str(dat)

# Pull out farm size vars, give them variable names from nass_params separately
# This is because the short_desc is all the same for them
farm_size <- dat %>% 
  filter(str_detect(domaincat_desc, '^AREA')) %>% 
  left_join(
    select(nass_params, domaincat_desc, variable_name), 
    by = 'domaincat_desc'
  )
get_str(farm_size)

# For other vars, just join with nass params 
others <- dat %>% 
  filter(str_detect(domaincat_desc, '^AREA OPERATED', negate = TRUE))
# check <- nass_params %>% 
#   filter(is.na(domaincat_desc))
# get_str(check)
  
dat <- nass_params %>% 
  # filter(str_detect(domaincat_desc, '^AREA OPERATED', negate = TRUE)) %>% 
  filter(is.na(domaincat_desc)) %>% 
  select(short_desc, variable_name) %>% 
  right_join(others, by = 'short_desc')
get_str(dat)

# Check
dat %>% 
  select(variable_name, short_desc) %>% 
  unique()

# Now put the two back together
dat <- bind_rows(farm_size, dat)
get_str(dat)

# For those without variable names, create them from short desc
# This is slow, should rework
dat <- dat %>% 
  mutate(
    variable_name = case_when(
      is.na(variable_name) ~ snakecase::to_lower_camel_case(short_desc),
      .default = variable_name
    )
  )
get_str(dat)

# Reduce unnecessary variables
dat <- dat %>% 
  select(
    variable_name,
    fips,
    year,
    value
  )
get_str(dat)



# Calculate New Variables -------------------------------------------------


# Doing this one at a time to avoid hangups pivoting wider with uneven sized
# data
(vars <- meta_vars(dat))

# Function to add a derived variable
calculate_var <- function(df,
                          num,
                          denom,
                          name) {
  out <- df %>% 
    filter(variable_name %in% c(num, denom)) %>% 
    pivot_wider(
      id_cols = c(fips, year),
      names_from = variable_name,
      values_from = value
    ) %>% 
    rowwise() %>% 
    mutate(
      !!sym(name) := !!sym(num) / !!sym(denom),
      .keep = 'unused'
    )
  cat('\nRange:', range(out[[name]], na.rm = TRUE))
  
  out <- out %>% 
    pivot_longer(
      cols = !c(fips, year),
      values_to = 'value',
      names_to = 'variable_name'
    )
  bind_rows(df, out)
}

get_str(dat)

# Add value added sales as a proportion of total sales
dat <- calculate_var(
  dat,
  'totalSalesValueAddedDirect',
  'totalSalesCommodities',
  'd2cSalesPropTotal'
)

# Value added retail as a proportion of all sales
dat <- calculate_var(
  dat,
  'totalSalesValueAddedDirect',
  'totalSalesCommodities',
  'salesValueAddedDirectPropTotal'
)

# Value added wholesale as a proportion of all sales
dat <- calculate_var(
  dat,
  'totalSalesValueAddedWholesale',
  'totalSalesCommodities',
  'salesValueAddedWholesalePropTotal'
)

# Income from agritourism as proportion of all income
dat <- calculate_var(
  dat,
  'totalIncomeAgTourismRecreation',
  'totalFarmIncome',
  'incomeAgTourismRecPropTotal'
)

# Fertilizer expenses as proportion of total expenses
dat <- calculate_var(
  dat,
  'expensesFertilizerLimeSoilCond',
  'totalOperatingExpenses',
  'fertExpensePropTotalExpense'
)

# Fuels as proportion of total expenses
dat <- calculate_var(
  dat,
  'totalFuelExpenses',
  'totalOperatingExpenses',
  'fuelExpensePropTotalExpense'
)

# Female to male producer ratio
dat <- calculate_var(
  dat,
  'nFemProducers',
  'nMaleProducers',
  'ftmProdRatio'
)

# Income from crop and animal insurance as a proportion of total
dat <- calculate_var(
  dat,
  'totalIncomeCropAnimalInsurance',
  'totalFarmIncome',
  'incomeCropAnimalInsurancePropTotal'
)

# Proportion of organic operations
dat <- calculate_var(
  dat,
  'nOpsOrganic',
  'nOperations',
  'propOpsOrganic'
)

# Proportion retail to wholesale
dat <- calculate_var(
  dat,
  'totalSalesValueAddedDirect',
  'totalSalesValueAddedWholesale',
  'retailSalesPropWholesale'
)
# Note that this should be a target I guess, 50/50?

# Proportion of value added sales to total commodity sales
dat <- calculate_var(
  dat,
  'salesValueAdded',
  'totalSalesCommodities',
  'salesValueAddedPropTotal'
)



# Ad Hoc Vars -------------------------------------------------------------


## Producer racial diversity with Shannon index of producer races
get_str(dat)

# Prepare 
out <- dat %>% 
  filter(str_detect(variable_name, '^n.+Producers$')) %>% 
  pivot_wider(
    id_cols = c(fips, year),
    values_from = value,
    names_from = variable_name
  ) %>% 
  mutate(across(!c(fips, year), ~ ifelse(is.na(.x), 0, .x)))
get_str(out)

out$producerRacialDiversity <- out %>% 
  select(where(is.numeric)) %>% 
  diversity()
get_str(out) 

# Make it long again to add back into dat
dat <- out %>% 
  select(fips, year, producerRacialDiversity) %>% 
  pivot_longer(
    cols = producerRacialDiversity,
    names_to = 'variable_name',
    values_to = 'value'
  ) %>% 
  bind_rows(dat)
get_str(dat)  


## Total sales (animal sales + crop sales)
dat <- dat %>% 
  filter(variable_name %in% c('salesAnimal', 'salesCrop')) %>% 
  pivot_wider(
    id_cols = c(fips, year),
    values_from = value,
    names_from = variable_name
  ) %>% 
  mutate(
    salesAnimalAndCrop = salesAnimal + salesCrop,
    .keep = 'unused'
  ) %>% 
  pivot_longer(
    cols = !c(fips, year),
    values_to = 'value',
    names_to = 'variable_name'
  ) %>% 
  bind_rows(dat)
get_str(dat)


## Producer age diversity
# Pull out nProducers variables to get diversity and skew (not area operated)
out <- dat %>% 
  filter(
    str_detect(variable_name, '^nProducers.*'),
    str_detect(variable_name, 'AreaOperated', negate = TRUE)
  )
get_str(out)
out$variable_name %>% unique

# Pivot wider for calculations
out <- out %>% 
  pivot_wider(
    id_cols = c(fips, year),
    values_from = value,
    names_from = variable_name
  )
get_str(out)

# Calculate skew
trans <- out %>% 
  select(nProducersLT25:last_col())
get_str(trans)

# Test out skew calculation
(first_row <- as.numeric(trans[1, ]))
prod_midpoints <- c(22.5, 29.5, 39.5, 49.5, 59.5, 69.5, 79.5)
test <- map(1:7, ~ {
  rep(prod_midpoints[.x], first_row[.x])
}) %>% 
  unlist()
skewness(test)
hist(test)

# Function for weighted skew
get_skew <- function(weights, counts) {
  stopifnot(length(weights) == length(counts))
  
  # If all NA, return NA
  if (all(is.na(counts)) || sum(counts, na.rm = TRUE) == 0) {
    return(NA_real_)
  }
  
  # Turn individual NAs into zeroes
  counts[is.na(counts)] <- 0
  
  # Repeat each bin midpoint x number of times based on count
  map(1:length(weights), ~ {
    rep(weights[.x], counts[.x])
  }) %>% 
    unlist() %>% 
    skewness()
}
get_skew(prod_midpoints, first_row)

# Calculate it in real data
skew <- trans %>% 
  rowwise() %>% 
  mutate(producerAgeSkew = get_skew(prod_midpoints, c_across(everything()))) %>% 
  ungroup() %>% 
  pull(producerAgeSkew)
skew

# Join back to out as new variable, put back in long format
combine <- out %>% 
  mutate(
    producerAgeSkew = skew
  ) %>% 
  select(fips, year, producerAgeSkew) %>% 
  pivot_longer(
    cols = !c(fips, year),
    names_to = 'variable_name',
    values_to = 'value'
  )
get_str(combine)

# Bind it back to dat
get_str(dat)
dat <- bind_rows(dat, combine)
get_str(dat)


## Farm size skew
get_str(dat)
out <- dat %>% 
  filter(str_detect(variable_name, 'nProducersAreaOperated'))
get_str(out)
out$variable_name %>% unique

# Pivot wider for calculations
out <- out %>% 
  pivot_wider(
    id_cols = c(fips, year),
    values_from = value,
    names_from = variable_name
  )
get_str(out)

# Put in order of distribution
trans <- out %>%
  select(matches('nProducers.')) %>% 
  select(
    all_of(
      names(.) %>%
        .[order(as.integer(str_extract(., "\\d+$")))]
    )
  )
get_str(trans)

# Calculate skew
size_midpoints <- c(5, 30, 60, 80, 120, 160, 200, 240, 380, 750, 1500, 2500)
skew <- trans %>%
  rowwise() %>%
  mutate(farmSizeSkew = get_skew(size_midpoints, c_across(everything()))) %>% 
  ungroup() %>%
  pull(farmSizeSkew)
skew

# Join back to out as new variable, put back in long format
combine <- out %>%
  mutate(farmSizeSkew = skew) %>%
  select(fips, year, farmSizeSkew) %>%
  pivot_longer(
    cols = !c(fips, year),
    names_to = 'variable_name',
    values_to = 'value'
  )
get_str(combine)

# Bind it back to dat
get_str(dat)
dat <- bind_rows(dat, combine)
get_str(dat)



# Smooth Acres Operated ---------------------------------------------------


# Smoothing out acres opreated to be even steps between those years
ac <- filter(dat, variable_name == 'acresOperated')
get_str(ac)

# Complete range of years
range(ac$year)
(full_range <- 2002:2022)
ac <- ac %>% 
  mutate(year = as.numeric(year)) %>% 
  tidyr::complete(fips, year = full_range, variable_name)
get_str(ac)
ac

# Group by year and fips, fill in missing values
ac <- ac %>% 
  group_by(fips) %>% 
  mutate(
    value = na.approx(value, na.rm = FALSE),
    variable_name = 'acresOperatedSmooth'
  ) %>% 
  ungroup()
ac
get_str(ac)

# Add it back to rest of dataset
get_str(dat)
dat <- ac %>% 
  mutate(year = as.character(year)) %>% 
  bind_rows(dat)
get_str(dat)



# Transformations ---------------------------------------------------------


# Dividing some metrics by 1e3 or 1e6 to make them more tractable.
trans <- dat %>% 
  meta_vars() %>% 
  str_subset(regex('landValPerAcre|expPF|animalandcrop', ignore_case = TRUE))
trans

# Check
dat %>% filter(variable_name == trans[1])
dat %>% filter(variable_name == trans[2])
dat %>% filter(variable_name == trans[3])

dat <- dat %>% 
  mutate(value = case_when(
    variable_name %in% trans ~ value / 1000,
    .default = value
  ))

# Check
dat %>% filter(variable_name == trans[1])
dat %>% filter(variable_name == trans[2])
dat %>% filter(variable_name == trans[3])

# Adjust nass params to account for transformations
get_str(nass_params)
nass_params %>% 
  filter(variable_name %in% trans) %>% 
  select(variable_name, units)
nass_params <- nass_params %>% 
  mutate(units = case_when(
    variable_name %in% trans ~ '1000 usd',
    .default = units
  ))
nass_params %>% 
  filter(variable_name %in% trans) %>% 
  select(variable_name, units)



# Metadata ----------------------------------------------------------------


get_str(dat)
get_str(nass_params)

(vars <- meta_vars(dat))
dat_df <- data.frame(variable_name = vars)

# Join NASS params with variable names from dat to start metadata
meta <- nass_params %>% 
  filter(source_desc == 'CENSUS') %>%
  right_join(dat_df) %>% 
  select(-ends_with('desc'), -note)
get_str(meta)

# Work off of this to fill in rest of metadata
meta <- meta %>% 
  mutate(
    dimension = case_when(
      is.na(dimension) & str_detect(variable_name, 'Yield') ~ 'production',
      .default = dimension
    ),
    index = case_when(
      is.na(index) & str_detect(variable_name, 'Yield') ~ 'production margins',
      .default = dimension
    ),
    indicator = case_when(
      is.na(indicator) & str_detect(variable_name, 'Yield') ~ 'yield',
      .default = indicator
    ),
    axis_name = case_when(
      is.na(axis_name) ~ variable_name,
      .default = axis_name
    )
  ) %>% 
  
  # Fresh calculated variables
  mutate(
    latest_year = meta_latest_year(dat),
    year = meta_years(dat),
    resolution = meta_resolution(dat),
    scope = 'national',
    updates = '5 years',
    source = paste0(
      "U.S. Department of Agriculture, National Agricultural Statistics Service. ",
      "(2024). 2022 Census of Agriculture."
    ),
    url = 'https://www.nass.usda.gov/Publications/AgCensus/2022/',
  ) %>% 
  meta_citation(date = '2025-07-28')
get_str(meta)



# Aggregate ---------------------------------------------------------------


# Check record counts
check_n_records(dat, meta, 'nass')

saveRDS(dat, '5_objects/metrics/nass.RDS')
saveRDS(meta, '5_objects/metadata/nass_meta.RDS')

clear_data(gc = TRUE)

