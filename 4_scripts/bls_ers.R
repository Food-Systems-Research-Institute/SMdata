# BLS and ERS
# 2025-06-30 update


# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  jsonlite,
  httr,
  glue,
  snakecase,
  readxl,
  tidyr,
  stringr,
  readr,
  purrr
)

# Results list
results <- list()
metas <- list()



# QCEW --------------------------------------------------------------------
## Clean -------------------------------------------------------------------


# API called in '4_scripts/api_calls/bls_api.R'
out <- readRDS('5_objects/api_outs/bls_qcew.rds')
get_str(out)

# Clean up each county dataframe, then bind together
dat <- out %>%
  keep(is.data.frame) %>% 
  map(\(x) {
    x %>% 
      filter(
        industry_code %in% naics_key$naics
        ) %>% 
      mutate(
        across(everything(), as.character),
        
        # If fips has a missing leading 0 (length 4), add the leading 0 back
        area_fips = ifelse(
          str_length(area_fips) == 4,
          paste0('0', area_fips),
          area_fips
        ),
        
        # If area fips is a state (ends in 000), remove the 000
        area_fips = str_remove(area_fips, '000')
          
      ) %>%
      pivot_longer(
        cols = c(
          annual_avg_estabs:last_col(),
          -matches('disclosure')
        ),
        names_to = "variable_name",
        values_to = "value"
      ) %>%
      filter(if_all(contains("disclosure"), ~ . != "N")) %>% 
      mutate(variable_name = paste(variable_name, industry_code, sep = "_")) %>%
      select(-c(
        industry_code,
        matches('disclosure')
      ))
  }) %>% 
  bind_rows()
get_str(dat)

# Remove irrelevant columns
results$qcew <- dat %>% 
  select(-c(
    own_code, agglvl_code, size_code, qtr
  )) %>% 
  mutate(
    # disclosure = ifelse(disclosure == '', NA, 'N'),
    variable_name = paste0(
      snakecase::to_lower_camel_case(variable_name),
      'NAICS'
    ),
    .keep = 'unused'
  ) %>% 
  rename(fips = area_fips)
get_str(results$qcew)

results$qcew$variable_name %>% unique %>% sort

# Let's just get rid of disclosure codes actually
# Could deal with these later...
results$qcew <- results$qcew %>% 
  filter(str_detect(variable_name, 'DisclosureCode', negate = TRUE))
get_str(results$qcew)



## Metadata ----------------------------------------------------------------


(vars <- results$qcew$variable_name %>% unique %>% sort)

# Join qcew data to the NAICS key
# Keeping both old var name and new var name
qcew_fields <- read_csv('1_raw/bls/naics-based-annual-layout.csv')
metas$qcew <- results$qcew %>% 
  mutate(
    og_variable_name = str_remove_all(variable_name, '[0-9]*NAICS') %>% 
      snakecase::to_snake_case()
  ) %>% 
  select(variable_name, og_variable_name) %>% 
  unique() %>% 
  left_join(qcew_fields, by = join_by(og_variable_name == field_name)) %>% 
  select(
    variable_name,
    og_variable_name,
    definition = field_description
  ) %>% 
  mutate(
    variable_name = snakecase::to_lower_camel_case(variable_name) %>% 
      str_replace_all('Naics', 'NAICS')
  )
metas$qcew

# Check og vars
(og_vars <- metas$qcew$og_variable_name %>% unique %>% sort)

# Made regular metadata file
metas$qcew <- metas$qcew %>% 
  mutate(
    dimension = 'economics',
    index = 'community economy',
    indicator = case_when(
      str_detect(variable_name, 'Emplvl') ~ 'job availability',
      str_detect(variable_name, 'Estabs') ~ 'business failure rate',
      str_detect(variable_name, 'Wage|Pay') ~ 'wage rate',
      .default = NA
    ),
    axis_name = variable_name, # fix this eventually...
    metric = variable_name,
    units = case_when(
      str_detect(variable_name, 'Estabs') ~ 'ratio',
      str_detect(variable_name, 'Emplvl') ~ 'count',
      str_detect(variable_name, 'Wage|AnnualPay|Contribut') ~ 'usd',
       str_detect(variable_name, '^lq') ~ 'ratio',
      str_detect(variable_name, '^oty.*PctChg') ~ 'percentage',
    ),
    annotation = 'disclosure',
    scope = 'national',
    resolution = 'county',
    year = '2023',
    updates = 'annual',
    warehouse = FALSE,
    latest_year = meta_latest_year(results$qcew),
    source = 'U.S. Bureau of Labor Statistics, Quarterly Census of Employment and Wages (2023)',
    url = 'https://www.bls.gov/cew/'
  ) %>% 
  meta_citation(date = '2024-11-05') %>%
  select(-og_variable_name)
  
get_str(metas$qcew)
try(check_n_records(results$qcew, metas$qcew, 'QCEW'))



# BLS Bulk ----------------------------------------------------------------


# Is this actually better than using API?
# https://www.bls.gov/cew/downloadable-data-files.htm
# Download CSVs by industry. Then just take the ones for 11 - agriculture
# File layout: https://www.bls.gov/cew/about-data/downloadable-file-layouts/annual/naics-based-annual-layout.htm

# Load csv files in bulk
file_paths <- list.files(
  path = '1_raw/bls/bls_naics_11/',
  pattern = '*.csv',
  full.names = TRUE
)
dat <- map(file_paths, ~ read_csv(.x)) %>% 
  bind_rows()
get_str(dat)

# Filter to private sector only, make sure it is annual, filter to neast states
dat <- dat %>% 
  filter(
    area_fips %in% fips_key$fips,
    qtr == 'A',
    own_code == '5'
  )
get_str(dat)

# Select relevant variables
dat <- dat %>% 
  select(
    fips = area_fips,
    year,
    matches('^annual|^total|^taxable|^oty')
  )
get_str(dat)

# Fix names
names(dat)[3:length(names(dat))] <- names(dat)[3:length(names(dat))] %>% 
  snakecase::to_lower_camel_case() %>% 
  paste0('NAICS11')
get_str(dat)

# Pivot longer
dat <- dat %>%
  mutate(across(everything(), as.character)) %>%
  pivot_longer(
    cols = !c(fips, year),
    names_to = 'variable_name',
    values_to = 'value'
  )
get_str(dat)

# Check for 0s and NAs
meta_vars(dat)
dat %>% filter(variable_name == 'annualAvgWklyWageNAICS11')
dat %>% filter(variable_name == 'annualAvgEstabsCountNAICS11')
dat %>% filter(variable_name == 'annualAvgEmplvlNAICS11')

# Check weekly wage 0s to NAs. Doesn't make sense for them to be 0.
# But we could have 0 establishments or 0 employees
dat <- dat %>% 
  mutate(value = case_when(
    variable_name == 'annualAvgWklyWageNAICS11' & value == 0 ~ NA,
    .default = value
  ))
dat %>% filter(variable_name == 'annualAvgWklyWageNAICS11')

get_str(dat)
results$bulk <- dat



## Metadata ----------------------------------------------------------------


(vars <- meta_vars(dat))
metas$bulk <- data.frame(variable_name = meta_vars(results$bulk)) %>% 
  mutate(
    dimension = 'economics',
    index = 'community economy',
    indicator = case_when(
      str_detect(variable_name, 'Estabs') ~ 'business failure rate of food business',
      .default = 'wealth/income distribution'
    ),
    metric = variable_name,
    axis_name = variable_name,
    definition = NA,
    resolution = meta_resolution(dat),
    scope = 'national',
    updates = 'annual',
    latest_year = meta_latest_year(dat),
    year = meta_years(dat),
    source = 'U.S. Bureau of Labor Statistics, Census of Employment and Wages (2023)',
    url = 'https://www.bls.gov/cew/'
  ) %>% 
  meta_citation(date = '2025-07-03')
get_str(metas$qcew)



# BLS CPI -----------------------------------------------------------------


# Pulling this from a download for now to check it out. 
# TODO: Get this from BLS API

# Consumer Price Index for all urban consumers (CPI-U), Northeast
# Same for Class A and for Class B/C
# cpi <- readxl::read_xlsx(
#   '1_raw/bls/cpi/SeriesReport-20250902175923_338893_CPIU_Northeast.xlsx',
#   skip = 10
# )

# All items
cpi_all <- readxl::read_xlsx(
  '1_raw/bls/cpi/SeriesReport-20251008130852_CPIU_northeast_all_items.xlsx',
  skip = 10
)
get_str(cpi_all)

# Food at home
cpi_fah <- readxl::read_xlsx(
  '1_raw/bls/cpi/SeriesReport-20251008130728_CPIU_northeast_food_at_home.xlsx',
  skip = 10
)
get_str(cpi_fah)

# Make list of both
cpi <- list(cpi_all, cpi_fah) %>% 
  setNames(c('cpiAllItems', 'cpiFoodAtHome'))
get_str(cpi, 3)

# Not much to do with this since it is at level of northeast.
# Don't even have a fips code to give it. Just put it in a usable format

cpi <- imap(cpi, ~ {
  .x %>% 
    select(year = Year, value = Annual) %>% 
    filter(year < 2025) %>% 
    mutate(
      fips = '99999',
      variable_name = .y
    )
}) %>% 
  bind_rows()
get_str(cpi, 3)
meta_vars(cpi)

results$cpi <- cpi



## Metadata ----------------------------------------------------------------


meta_vars(cpi)
metas$cpi <- data.frame(
  dimension = 'economics',
  index = 'community economy',
  indicator = 'marketplace',
  metric = c(
    'consumer price index - all items', 
    'consumer price index - food at home'
  ),
  variable_name = meta_vars(cpi),
  axis_name = c(
    'CPI - All Items',
    'CPI - Food at Home'
  ),
  definition = c(
    'Average annual change in prices paid by urban consumers in the Northeast for all items',
    'Average annual change in prices paid by urban consumers in the Northeast for food at home, which excludes restaurants, alcohol, and other beverages'
  ),
  resolution = 'Northeast',
  scope = 'national',
  updates = 'monthly',
  latest_year = meta_latest_year(cpi),
  year = meta_years(cpi),
  source = 'Bureau of Labor Statistics, Office of Prices and Living Conditions',
  url = 'https://www.bls.gov/cpi/data.htm'
) %>% 
  meta_citation(date = '2025-10-08')
get_str(metas$cpi)



# ERS Bulk Unemployment ---------------------------------------------------


# Using unemployment and household income data from BLS, bulk download
# https://www.ers.usda.gov/data-products/county-level-data-sets/county-level-data-sets-download-data/

bls_bulk <- read_xlsx(
  '1_raw/bls/Unemployment.xlsx',
  sheet = 1,
  skip = 4
)

get_str(bls_bulk)
names(bls_bulk) %>% 
  sort()

# Cleaning
results$unemp <- bls_bulk %>% 
  select(-c(Area_Name, State)) %>% 
  rename(fips = FIPS_Code) %>% 
  
  # Change fips to match our system (2-digit codes for state and US)
  # Then we can filter by fips key to get NE states and counties
  mutate(fips = case_when(
    str_detect(fips, '^.{2}000') ~ str_sub(fips, start = 1, end = 2),
    .default = fips 
  )) %>% 
  
  # Get names to match our system, then pivot longer, split name and year
  setNames(c(to_lower_camel_case(names(.)))) %>% 
  pivot_longer(
    cols = -fips,
    names_to = c("variable_name", "year"),
    names_pattern = "^(.*)(\\d{4})$"
  ) %>% 
  mutate(
    variable_name = case_when(
      variable_name == 'medHhIncomePercentOfStateTotal' ~ 'medHhIncomePercState',
      variable_name == 'medianHouseholdIncome' ~ 'medHhIncome',
      variable_name == 'civilianLaborForce' ~ 'civLaborForce',
      .default = variable_name
    ),
    disclosure = NA
  )

get_str(results$unemp)



## Metadata ----------------------------------------------------------------


# Unique variable names
(vars <- meta_vars(results$unemp))

metas$unemp <- data.frame(
  dimension = c(
    rep('economics', 4),
    rep('utilities', 2),
    rep('economics', 2),
    'utilities'
  ),
  index = c(
    rep('community economy', 4),
    rep('utilities_index', 2),
    rep('community economy', 2),
    'utilities_index'
  ),
  indicator = c(
    'employee numbers',
    rep('wealth/income distribution', 3),
    rep('utilities_indicator', 2),  # revisit this at some point
    rep('wealth/income distribution', 2),
    'utilities_indicator'
  ),
  axis_name = c(
    'Civ Labor Force',
    'Number Employed',
    'Median HH Income',
    'Median HH Income (% of state avg)',
    'Metro Area',
    'Rural Urban Continuum',
    'Number Unemployed',
    'Unemployment Rate (%)',
    'Urban Influence'
  ),
  metric = c(
    'Civilian Labor Force',
    'Number Employed',
    'Median Household Income',
    'Median Household Income (% of State Avg)',
    'Metropolitan Area',
    'Rural Urban Continuum Code',
    'Number unemployed',
    'Unemployment Rate',
    'Urban Influence Code'
  ), 
  variable_name = vars,
  definition = c(
    paste(
      'Number of civilians age 16 or older who are classified either as employed or unemployed.',
      'Conceptually, the labor force is the number of people who are either working or actively looking for work.'
    ),
    paste(
      'People are classified as employed if during the survey reference week, they meet ANY of the following criteria:',
      '1. Worked at least 1 hour as paid employee,',
      '2. Worked at least 1 hour in own business (self-employed),',
      '3. Temporarily absent from job, whether or not they were paid for time off,',
      '4. Worked without pay for a minimum of 15 hours in family owned business.'
    ),
    'Median household income.',
    'Median household income as a percentage of average median household income for the state.',
    'Coded between 1 and 3 on Rural-Urban Continuum (county in metro area and population of 250,000 or more). See URL for details.',
    'Scale from 1 (county in metro area and population of 1 million or more) to 9 (urban population of 5,000 or fewer, not adjacent to metro area). See URL for details.',
    paste0(
      'Meet all of the following criteria:',
      '1. Not employed during survey reference week.',
      '2. Available for work during survey reference week, except for temporary illness.',
      '3. Made at least one specific, active effort to find a job during 4-week period ending with survey reference week.'
    ),
    'Number of unemployed people as percentage of labor force: (Unemployed / Labor Force) * 100',
    'Scale of 1 (metropolitan county, 1 million ort more residents) to 12 (noncore not adjacent to metro or micro area, does nto contain town of 2,500 residents or more). See URL for details)'
  ),
  units = c(
    'count',
    'count',
    'usd',
    'percentage',
    'binary',
    'categorical',
    'count',
    'percentage',
    'categorical'
  ),
  annotation = rep(NA, 9),
  scope = rep('national', 9),
  resolution = meta_resolution(results$unemp),
  year = meta_years(results$unemp),
  latest_year = meta_latest_year(results$unemp),
  updates = c(
    rep('annual', 4),
    rep(NA, 2),
    rep('annual', 2),
    NA
  ),
  warehouse = rep(FALSE, 9),
  source = c(
    rep('U.S. Department of Labor, Bureau of Labor Statistics, Local Area Unemployment Statistics (LAUS)', 2),
    rep('U.S. Department of Commerce, Bureau of the Census, Small Area Income and Poverty Estimates (SAIPE) Program', 2),
    rep('U.S. Department of of Agriculture, Economic Research Service', 2),
    rep('U.S. Department of Labor, Bureau of Labor Statistics, Local Area Unemployment Statistics (LAUS)', 2),
    'U.S. Department of of Agriculture, Economic Research Service'
  ),
  url = c(
    rep('https://www.bls.gov/lau/tables.htm', 2),
    rep('https://www.census.gov/programs-surveys/saipe.html', 2),
    rep('https://www.ers.usda.gov/topics/rural-economy-population/rural-classifications/', 2),
    rep('https://www.bls.gov/lau/tables.htm', 2),
    'https://www.ers.usda.gov/topics/rural-economy-population/rural-classifications/'
  )
) %>% 
  meta_citation(date = '2024-11-05')

get_str(metas$unemp)
metas$unemp



# ERS State Ag Data -------------------------------------------------------


# Imports and exports by state
# https://www.ers.usda.gov/data-products/state-agricultural-trade-data/



## Imports -----------------------------------------------------------------


imports_raw <- read_csv('1_raw/usda/ers_state_trade/top_5_ag_imports_by_state.csv') %>% 
  setNames(c(snakecase::to_snake_case(names(.))))
get_str(imports_raw)
get_str(state_key)

# Filter down to New England
# Only keep world totals of top 5 imports, not each individual one
# Also only keeping fiscal year total, not quarterly
imports <- state_key %>%
  select(state, state_code) %>%
  inner_join(imports_raw) %>% 
  filter(country == 'World', fiscal_quarter == '0') %>%
  select(
    fips = state_code, 
    year = fiscal_year, 
    variable_name = commodity_name, 
    value = dollar_value
  )
get_str(imports)
# Note that this is pretty interesting, but not sure what we can really do with
# the product-specific data. It would be hard to organize into 5 metrics because
# they are not the same across each state. For now I guess we're just taking the
# world total?

# Group by state and year to get total world imports for top 5 products
# Also pivot to fit format 
imports <- imports %>% 
  group_by(fips, year) %>% 
  summarize(importsTopFive = sum(value)) %>% 
  pivot_longer(
    cols = importsTopFive,
    values_to = 'value',
    names_to = 'variable_name'
  ) %>% 
  mutate(value = value / 1e6)
get_str(imports)

# Save it
results$imports <- imports



## Exports -----------------------------------------------------------------


exports_raw <- read_csv('1_raw/usda/ers_state_trade/state_exports.csv') %>% 
  setNames(c(snakecase::to_snake_case(names(.))))
get_str(exports_raw)
get_str(state_key)

# Filter to New England and clean
exports <- state_key %>% 
  select(state, state_code, state_name) %>% 
  inner_join(exports_raw, by = join_by(state_name == state)) %>% 
  select(-state_name, -state, -units) %>% 
  mutate(
    variable_name = paste0('exports', snakecase::to_upper_camel_case(commodity)) %>% 
      str_remove('Exports$'),
    .keep = 'unused'
  ) %>% 
  rename(fips = state_code)
get_str(exports)

# Save 
results$exports <- exports



## Metadata ----------------------------------------------------------------


# Check vars for exports. do imports separately
(vars <- meta_vars(results$exports))

# Plain text versions of vars to use in definition
plain_vars <- vars %>% 
  str_remove('exports') %>% 
  snakecase::to_sentence_case() %>% 
  str_to_lower() %>% 
  case_when(
    . == 'total' ~ 'total exports',
    .default = .
  )
plain_vars

metas$import_export <- data.frame(
  variable_name = c(vars, 'importsTopFive'),
  definition = c(
    paste(
      'Value (in millions) of',
      plain_vars,
      'exports'
    ),
    'Value (in millions) of imports for top five agricultural commodity groups from outside of the United States. Note that this is not total imports, nor does it include imports from other states.'
  ),
  axis_name = c(
    paste(
      c(
        'Beef',
        'Broiler Meat', 
        'Corn',
        'Cotton',
        'Dairy',
        'Feed',
        'Fresh Fruit',
        'Processed Fruit',
        'Grain',
        'Hide and Skin',
        'Other Livestock',
        'Other Oilseed',
        'Other Plant',
        'Other Poultry',
        'Pork',
        'Rice',
        'Soybean Mean',
        'Soybean',
        'Tobacco',
        'Total Agricultural',
        'Total Animal',
        'Total Plant',
        'Tree Nut',
        'Vegetable Oil',
        'Fresh Vegetable',
        'Processed Vegetable',
        'Wheat'
      ),
      'Exports (million usd)'
    ),
    'Top Five Imports (million usd)'
  ),
  dimension = "production",
  index = 'imports vs exports',
  indicator = c(
    rep('total quantity exported', length(plain_vars)),
    'total quantity imported'
  ),
  units = 'million usd',
  scope = 'national',
  resolution = 'state',
  year = c(
    meta_years(results$exports),
    meta_years(results$imports)
  ),
  latest_year = c(
    meta_latest_year(results$exports),
    meta_latest_year(results$imports)
  ),
  updates = "annual",
  source = c(
    rep('US Department of Agriculture, Economic Research Service, State Agricultural Trade Data, State Exports', length(plain_vars)),
    'US Department of Agriculture, Economic Research Service, State Agricultural Trade Data, State Trade by Country of Origin and Destination'
  ),
  url = 'https://www.ers.usda.gov/data-products/state-agricultural-trade-data/',
  citation = paste0(
    'USDA ERS (2023). State Agricultural Trade Data. Accessed from ',
    'https://www.ers.usda.gov/data-products/state-agricultural-trade-data/, ',
    'December 16th, 2024.'
  )
) %>% 
  
  # Build metrics out of variable_names 
  # Second step because we need to finish data frame first
  mutate(
    metric = variable_name %>% 
      str_remove('^exports|^imports') %>% 
      snakecase::to_sentence_case() %>% 
      str_to_lower(),
    metric = case_when(
      metric == 'top five' ~ 'value of top five imports',
      .default = paste(
        'Value of', metric, 'exports'
      )
    )
  )

metas$import_export



# ERS Income and Wealth ---------------------------------------------------


# https://www.ers.usda.gov/data-products/farm-income-and-wealth-statistics/data-files-us-and-state-level-farm-income-and-wealth-statistics
# 'Download all data in CSV file'

# Load csv file from ERS
raw <- read_csv('1_raw/usda/ers_wealth_and_income/FarmIncome_WealthStatisticsData_February2025.csv') %>% 
  janitor::clean_names()
get_str(raw)

# Explore variables available at state level (some only at US aggregate)
vars <- raw %>% 
  filter(state != 'US') %>%
  pull(variable_description_total) %>% 
  unique()
vars

# Subset by a few keywords we are interested in
relevant_vars <- vars %>% 
  str_subset(
    regex(
      '^Net|Interest|Capital|emergency|indemnit|risk|dairy margin|milk|loss|forest',
      ignore_case = TRUE
    )
  ) %>% 
  str_subset(regex('Government transactions' , ignore_case = TRUE), negate = TRUE)
relevant_vars

# Check years
get_table(raw$year)
# Goes back to 1910, but scant data
# Looks like 2024 and 2025 are still incomplete

# Check 2024
raw %>% 
  filter(year == 2024) %>% 
  pull(state) %>% 
  unique
# 2024 and 2025 are ONLY for US. No good. So just take up to 2023

# Pull relevant vars for years going back to 2000, and up to 2023
# Then ditch irrelevant columns
get_str(raw)
ers_dat <- raw %>% 
  filter(
    year > 2000, 
    year <= 2023, 
    variable_description_total %in% relevant_vars
  ) %>% 
  select(
    year,
    fips = state,
    metric = variable_description_total,
    value = amount
  )
get_str(ers_dat)

# Recode states to fips codes. First add US to state codes
get_str(state_key)
states_and_us <- state_key %>% 
  select(state, state_code) %>% 
  add_row(state = 'US', state_code = '00')

# Recode two letter state names with 2 digit fips codes
ers_dat <- ers_dat %>% 
  mutate(fips = states_and_us$state_code[match(ers_dat$fips, states_and_us$state)])
get_str(ers_dat)

# DF of metric names and variable names
(metric_names <- meta_vars(ers_dat, 'metric'))
ers_crosswalk <- data.frame(
  metric = metric_names,
  variable_name = c(
    'capConsNoDwellings',
    'capConsWithDwellings',
    'capExpNoDwellings',
    'capExpWithDwellings',
    'capExpBldgs',
    'capExpBldgsLandNoDwellings',
    'capExpBldgsLandWithDwellings',
    'capExpBldgsLandDwellingsOnly',
    'capExpCars',
    'capExpTractors',
    'capExpTrucks',
    'capExpLandImprovements',
    'capExpMisc',
    'capExpVehiclesTractors',
    'capExpOtherMachinery',
    'capExpVehiclesMachinery',
    'receiptsAllForestProducts',
    'intExpNoDwellings',
    'intExpWithDwellings',
    'intExpNomRealEstateAll',
    'intExpRealEstateNoDwellings',
    'intExpRealEstateWithDwellings',
    
    'netCashIncome',
    'netFarmIncome',
    # 'netGovTransactions',
    'netRentAllLandlords',
    'netRentAllLandlordsExclCapCons',
    'netRentNonOpLandlordsInclCapCons',
    'netRentNonOpLandlordsExclCapCons',
    
    'netRentOpLandlords',
    'netRentOpLandlordsExclCapCons',
    'netReturnsToOpExclOpDwellings',
    'netValueAdded',
   
    'incomeInsuranceIndemnities',
    'incomeInsuranceIndemnitiesFederal',
    'valueEmergPayments',
    'valueAgRiskCoveragePayments',
    'valueOtherAdHocEmergPayments',
    'valueDairyMarginProtPayments',
    'valueMilkLossPayments',
    'valueAllLossCoveragePayments',
    'valueServicesAndForestry'
  )
)
ers_crosswalk

# Recode ers_dat to swap out the metric names for variable names
ers_dat <- ers_dat %>% 
  mutate(variable_name = ers_crosswalk$variable_name[match(metric, ers_crosswalk$metric)]) %>% 
  select(-metric)
get_str(ers_dat)
ers_dat$variable_name %>% unique

# Save to results list
results$ers_income_wealth <- ers_dat



## Metadata ----------------------------------------------------------------


(vars <- meta_vars(ers_dat))
ers_crosswalk

# Check units for our variables
unique(raw$unit_desc[raw$variable_description_total %in% ers_crosswalk$metric])
# All 1000 usd

metas$ers_income_wealth <- ers_crosswalk %>% 
  mutate(
    definition = metric,
    axis_name = variable_name,
    dimension = case_when(
      str_detect(metric, 'Capital|Interest|Net rent|income$|Net returns|Net value') ~ 'economics',
      .default = 'production'
    ),
    index = case_when(
      str_detect(metric, regex('forest', ignore_case = TRUE)) ~ 'production margins',
      str_detect(metric, 'Capital|Interest|Net rent') ~ 'access to capital/credit',
      str_detect(metric, 'income$|Net returns|Net value') ~ 'food business resilience',
      .default = 'waste and losses'
    ),
    indicator = case_when(
      str_detect(metric, 'forest') ~ 'total quantity non-food agricultural products',
      str_detect(metric, 'Capital|Interest|Net rent') ~ 'access to land',
      str_detect(metric, 'income$|Net returns|Net value') ~ 'income stability',
      .default = 'crop failure'
    ),
    units = '$1,000 usd',
    scope = 'national',
    resolution = 'state',
    year = meta_years(ers_dat),
    latest_year = meta_latest_year(ers_dat),
    updates = "annual",
    source = 'U.S. Department of Agriculture, Economic Research Service. (2025, February 6). Farm Income and Wealth Statistics.',
    url = 'https://www.ers.usda.gov/data-products/farm-income-and-wealth-statistics/data-files-us-and-state-level-farm-income-and-wealth-statistics'
) %>%  
  meta_citation(date = '2025-02-12')

metas$ers_income_wealth



# Aggregate and Save ------------------------------------------------------


# Put metrics and metadata together into two single DFs
out <- aggregate_metrics(results, metas)

# Check record counts
check_n_records(out$result, out$meta, 'other')

saveRDS(out$result, '5_objects/metrics/bls_ers.RDS')
saveRDS(out$meta, '5_objects/metadata/bls_ers_meta.RDS')

clear_data(gc = TRUE)
