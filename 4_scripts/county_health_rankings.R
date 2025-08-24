# County Health Ranking dataset
# 2025-07-08 update

# https://www.countyhealthrankings.org/


# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  readxl,
  purrr,
  snakecase,
  tidyr,
  readr,
  stringr
)

# Initiate results and metas lists
res <- list()
metas <- list()



# Outcomes and Factors ----------------------------------------------------


# Load national data with health factors and health outcomes
# Note that national Z-scores are only available when downloading by state
# Because they really want to make it difficult for some reason
path <- '1_raw/county_health_rankings/national/2024_county_health_release_data_-_v1.xlsx'
sheets <- excel_sheets(path)
nat_raw <- read_xlsx(path, sheet = sheets[6], skip = 1)
get_str(nat_raw)

## Rework
paths <- list.files(
  '1_raw/county_health_rankings/national/',
  pattern = '*.xlsx',
  full.names = TRUE
)
(sheets <- map(paths, excel_sheets))

# Get only the sheet from each workbook that has outcomes and factors
(sheets <- map(sheets, ~ str_subset(.x, '(Outcomes & Factors|Health Groups)(?!.*SubRankings)')))

# Read them in
raw <- map2(paths, sheets, \(path, sheet) {
  read_xlsx(path, sheet = sheet, skip = 1)
}) %>% 
  setNames(c(paste0('y', 2020:2025)))
get_str(raw)
get_str(raw, 3)
# NOTE: 2023 and earlier, used ranks and quartiles. 2024 and later, used 
# z-scores. Cannot compare them. So let's just use 2024 and 2025 z-scores here.

# Reduce to last 2 years
raw <- raw[names(raw) %in% c('y2024', 'y2025')]
get_str(raw, 3)

# Reduce to FIPS and z scores for health outcomes and factors
# Also reduce to northeast counties, add a column for year
nat <- imap(raw, ~ {
  year = str_sub(.y, start = 2)
  .x %>% 
    select(FIPS, contains('Z-Score')) %>% 
    setNames(c('fips', 'healthOutcomeZ', 'healthFactorZ')) %>% 
    filter_fips('all') %>% 
    mutate(year = year)
})
get_str(nat, 3)

# Bind together, then convert to long format
nat <- nat %>% 
  bind_rows() %>% 
  pivot_longer(
    cols = matches('Z$'),
    names_to = 'variable_name',
    values_to = 'value'
  )
get_str(nat)

# Save to results
res$nat <- nat



## Metadata ----------------------------------------------------------------


(vars <- meta_vars(res$nat))

metas$nat <- data.frame(
  variable_name = vars,
  definition = c(
    'Nationally standardized z-score index of county level health factors (30% health behaviors, 20% clinical care, 40% social and economic factors, 10% physical environment). Scores are recoded so that larger values represent healthier counties.',
    'Nationally standardized z-score index of county level health outcomes (50% premature death rate, 10% reports of poor or fair health, 10% reports of poor physical health days, 10% reports of poor mental health days, 10% low birthweight rates). Scores are recoded so that larger values represent healthier counties.'
  ),
  metric = c(
    'Health Factor Z-Score',
    'Health Outcome Z-Score'
  ),
  dimension = "health",
  index = 'physical health',
  indicator = 'county health rankings',
  units = rep('index', 2),
  scope = 'national',
  resolution = 'county',
  year = meta_years(res$nat),
  latest_year = meta_latest_year(res$nat),
  updates = "annual",
  source = 'University of Wisconsin Population Health Institute, County Health Rankings and Roadmaps. (2024)',
  url = 'https://www.countyhealthrankings.org/health-data/methodology-and-sources/data-documentation'
) %>%
  mutate(axis_name = str_remove(metric, '-Score')) %>% 
  meta_citation(date = '2025-07-08')

metas$nat



# Select Measures ---------------------------------------------------------


# Also pulling other interesting measures from national data. Note that we can
# pull county, state, and US data here. Using data dictionary to find the
# variables we want from the analytic sheets
dict <- read_xlsx('1_raw/county_health_rankings/DataDictionary_2024.xlsx') %>% 
  mutate(Measure = str_replace_all(Measure, '\n', ' '))

# We are removing disconnected youth because we took it from Census directly
dict <- dict %>%
  filter(str_detect(Measure, 'Disconnected Youth', negate = TRUE))

# Take everything else though
dict_summary <- dict %>% 
  filter(
    str_detect(Measure, 'raw value$'),
    str_detect(Measure, '%', negate = TRUE)
  )
var_codes <- dict_summary %>% 
  pull(`Variable Name`) %>% 
  str_split_i('_', 1)
suffixes <- c('_rawvalue', '_cilow', '_cihigh')
vars <- paste0(rep(var_codes, each = length(suffixes)), suffixes)

# Pull last 5 years of analytic data
paths <- dir('1_raw/county_health_rankings/analytic/', full.names = TRUE)
analytic <- map(paths, ~ read_csv(.x, skip = 1))

# Check it
get_str(analytic, 3)
head(analytic[[1]][, 1:4])

# Clean them as individual DFs, take vars we want, then see if we can bind them
select_measures <- map(analytic, ~ {
  .x %>% 
    select(statecode, fipscode, year, any_of(vars)) %>% 
    filter(
      statecode %in% fips_key$fips 
       | fipscode %in% fips_key$fips
       | fipscode %in% state_key$full_state_code
    ) %>% 
    setNames(c(
      ifelse(
        names(.) %in% dict$`Variable Name`,
        dict$Measure[match(names(.), dict$`Variable Name`)],
        names(.)
      )
    )) %>% 
    select(-'State FIPS Code') %>%
    rename(
      fips = '5-digit FIPS Code',
      year = 'Release Year'
    ) %>% 
    mutate(fips = case_when(
      fips == '00000' ~ '00',
      str_detect(fips, '000$') ~ str_remove(fips, '000'),
      .default = fips
    )) %>% 
    pivot_longer(
      cols = !c(fips, year),
      names_to = 'variable_name',
      values_to = 'value'
    ) 
}) %>% 
  bind_rows()
get_str(select_measures)

# Remove population - using census ACS5 here instead
select_measures <- select_measures %>% 
  filter(str_detect(variable_name, 'Population', negate = TRUE))
get_str(select_measures)

# Save to results
res$select_measures <- select_measures



## Metadata ----------------------------------------------------------------


vars <- res$select_measures$variable_name %>% 
  unique %>% 
  sort
vars
dict

# Get set of names and definitions we actually used
select_meta <- dict %>% 
  select(-`Variable Name`) %>% 
  filter(Measure %in% vars)

for (row in 1:nrow(select_meta)) {
  if (str_detect(select_meta$Measure[row], ' raw value$')) {
    select_meta$Measure[row] <- str_remove(select_meta$Measure[row], ' raw value')
  } else if (str_detect(select_meta$Measure[row], ' CI low$')) {
    select_meta$Description[row] <- paste0(
      'Confidence Interval, Low: ',
      select_meta$Description[row - 1]
    )
  } else if (str_detect(select_meta$Measure[row], ' CI high$')) {
    select_meta$Description[row] <- paste0(
      'Confidence Interval, High: ',
      select_meta$Description[row - 1]
    )
  }
}
get_str(select_meta)
select_meta

# Reference measures
ref_measures <- res$select_measures %>% 
  filter(str_detect(variable_name, ' CI ', negate = TRUE)) %>% 
  mutate(variable_name = str_remove(variable_name, ' raw value')) %>% 
  select(-fips, -value) %>% 
  unique()

# Start putting it into metadata format, but stop at dimensions
select_meta_meta <- select_meta %>% 
  rename(definition = Description) %>% 
  filter(str_detect(Measure, ' CI low$| CI high$', negate = TRUE)) %>% 
  mutate(
    metric = str_to_sentence(Measure),
    variable_name = snakecase::to_lower_camel_case(Measure),
    axis_name = variable_name,
    dimension = case_when(
      str_detect(metric, regex('social|disconnected|census|turnout|traffic|arrest|segregation|commute|driving|single-parent', ignore_case = TRUE)) ~ 'social',
      str_detect(metric, regex('broadband|poverty|employm|income|pay gap|free|wage', ignore_case = TRUE)) ~ 'economics',
      str_detect(metric, regex('population', ignore_case = TRUE)) ~ 'utilities',
      .default = 'health'
    ),
    scope = 'national',
    resolution = 'county, state',
    year = paste0(2020:2025, collapse = ', '),
    latest_year = 2025,
    updates = "annual",
    source = 'University of Wisconsin Population Health Institute, County Health Rankings and Roadmaps. (2024)',
    url = 'https://www.countyhealthrankings.org/health-data/methodology-and-sources/data-documentation'
  ) %>% 
  meta_citation(date = '2025-07-07') %>% 
  select(-Measure)

# Now just deal with social
metas$select_measures_social <- select_meta_meta %>%
  filter(dimension == 'social') %>% 
  arrange(variable_name) %>% 
  mutate(
    index = c(
      'food system governance',
      'food system governance',
      'community embeddedness',
      'community livability',
      'community livability',
      'community livability',
      'community livability',
      'community livability',
      'community embeddedness',
      'community livability',
      'food system governance'
    ),
    indicator = c(
      'community safety',
      'participatory governance',
      'social connectedness',
      'tbd',
      'community safety',
      'tbd',
      'diverse representation',
      'diverse representation',
      'social connectedness',
      'tbd',
      'participatory governance'
    ),
    units = c(
      rep('percentage', 4),
      'delinquencies per 1,000 juvelines',
      'percentage',
      'index',
      'index',
      'associations per 10,000 residents',
      'traffic volume per peter of roadway',
      'percentage'
    )
  )

# Economics
metas$select_measures_economics <- select_meta_meta %>%
  filter(dimension == 'economics') %>% 
  arrange(variable_name) %>% 
  mutate(
    index = 'community economy',
    indicator = c(
      'tbd',
      rep('wealth/income distribution', 6),
      'unemployment'
    ),
    units = c(
      rep('percentage', 3),
      'ratio',
      'ratio',
      'usd/hour',
      'usd',
      'percentage'
    )
  )

# Health
metas$select_measures_health <- select_meta_meta %>%
  filter(dimension == 'health') %>% 
  arrange(variable_name) %>% 
  mutate(
    index = c(
      'health infrastructure',
      rep('physical health', 3),
      rep('health infrastructure', 2),
      'physical health',
      'health infrastructure',
      'physical health',
      'health infrastructure',
      rep('physical health', 4),
      'health infrastructure',
      'food security',
      'mental health',
      'physical health',
      rep('education', 2),
      'physical health',
      'housing',
      'physical health',
      rep('physical health', 4),
      'health infrastructure',
      'physical health',
      'health infrastructure',
      'education',
      'health infrastructure',
      'physical health',
      'health infrastructure',
      'physical health',
      'mental health',
      rep('physical health', 5),
      'health infrastructure',
      'education',
      'education',
      'housing',
      'housing',
      'physical health',
      'education',
      'mental health',
      'physical health',
      rep('health infrastructure', 3)
    ),
    indicator = 'tbd',
    units = case_when(
      str_detect(definition, 'Percentage|percent') ~ 'percentage',
      str_detect(definition, ' per ') ~ 'ratio',
      str_detect(definition, 'Index') ~ 'index',
      str_detect(definition, '1=Yes') ~ 'binary',
      str_detect(definition, 'years|Average|average') ~ 'numeric'
    )
  )

# Remove the initial set of metadata from results list
metas$select_measures <- NULL

# Now we can also make our metric variable names formatted properly
res$select_measures$variable_name <- res$select_measures$variable_name %>% 
  snakecase::to_lower_camel_case() %>% 
  str_remove('RawValue')

res$select_measures %>%
  filter(
    variable_name == 'voterTurnout',
    str_length(fips) == 5
  ) %>%
  group_by(fips) %>%
  summarize(years = paste0(year, collapse = ', '))
  


# Aggregate and Save ------------------------------------------------------


# Combine lists together, one for results, one for meta
out <- aggregate_metrics(res, metas)

# Check record counts
try(check_n_records(out$result, out$meta, 'County Health'))

saveRDS(out$result, '5_objects/metrics/county_health.RDS')
saveRDS(out$meta, '5_objects/metadata/county_health.RDS')

clear_data(gc = TRUE)

