# EPA GHG data
# 2025-07-04 update


# Description -------------------------------------------------------------


# Note we are temporarily dropping some pieces here:
# FSA - replace with indemnities
# FDA - not in framework? might come back though.
# Happiness - not reliable



# Housekeeping -------------------------------------------------------------


pacman::p_load(
  dplyr,
  purrr,
  readr,
  tidyr,
  snakecase,
  readxl,
  sf,
  stringr
)

# Get areas of each county - used in FSA calculations
areas <- readRDS('5_objects/areas.rds')

results <- list()
metas <- list()



# EPA GHGs ----------------------------------------------------------------


# https://www.epa.gov/ghgemissions/state-ghg-emissions-and-removals
# Go to 'download consolidated data for all states (zip)'

# First UN sectors
un_raw <- read_xlsx(
  '1_raw/epa/state_ghgs_1990_2022/AllStateGHGData90-22_v082924.xlsx',
  sheet = 2
) %>% 
  unique()
get_str(un_raw)

# Narrow down to New England, Agriculture, relevant columns
un <- un_raw %>% 
  select(
    sector:sub_category_3, 
    geo_ref,
    ghg_category,
    starts_with('Y')
  ) %>% 
  filter(sector == 'Agriculture') %>% 
  left_join(
    select(state_key, state, fips = state_code), 
    by = join_by(geo_ref == state),
    keep = TRUE
  ) %>% 
  select(-sector, -geo_ref)
get_str(un)

# Combine categories so we can use them as definition later. It will also become
# variable name I guess. Leaving it as definition for now.
un <- un %>% 
  mutate(
    # Turn NA into '' so we can paste them together
    across(starts_with('sub_'), ~ ifelse(is.na(.x), '', .x)),
    # Paste unique identifiers as deep as we need to go
    definition = paste(
      subsector, 
      category, 
      sub_category_1, 
      sub_category_2, 
      sub_category_3, 
      sep = ' > '
    ),
    definition = case_when(
      str_detect(definition, '^CO2') ~ paste0(
        str_sub(definition, end = 4),
        str_to_lower(str_sub(definition, start = 5))
      ),
      str_detect(definition, '^CO2', negate = TRUE) ~ paste(
        ghg_category, 
        'emissions from',
        str_to_lower(definition)
      ),
      .default = NA
    ),
    definition = str_remove_all(definition, ' > $| >  > ') # remove blanks
  )
get_str(un)
dim(un)

# Calculate aggregates by category in separate df
subsector_totals <- un %>%
  pivot_longer(
    cols = starts_with("Y"),
    names_to = "year",
    names_prefix = "Y",
    values_to = "value"
  ) %>%
  # aggregating to subsector
  group_by(fips, year, subsector, ghg_category) %>%
  summarize(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(definition = paste0(
    'Subsector total of ', 
    ghg_category, 
    ' emissions from agriculture: ', 
    str_to_lower(subsector)
  )) %>%
  select(-subsector)
get_str(subsector_totals)
dim(subsector_totals)

# Another set where we get CO2, CH4, and N2O totals per fips and year
ghg_totals <- subsector_totals %>% 
  group_by(fips, year, ghg_category) %>% 
  summarize(value = sum(value, na.rm = TRUE), .groups = 'drop') %>% 
  mutate(definition = paste0('Total ', ghg_category, ' emissions from agriculture')) %>% 
  select(-ghg_category)
get_str(ghg_totals)

# Put main data in long format
un <- un %>%
  select(-c(subsector:ghg_category)) %>% 
  pivot_longer(
    cols = starts_with('Y'),
    names_to = 'year',
    values_to = 'value'
  ) %>%
  mutate(year = str_remove(year, 'Y'))
get_str(un)

# Now we can combine them all into one long format df
# Also add variable names here. Save it to use in metadata
# Also adding teragrams to end of each definition
un_definitions <- un %>% 
  bind_rows(select(subsector_totals, -ghg_category)) %>% 
  bind_rows(ghg_totals) %>% 
  mutate(
    definition = paste(definition, '(Tg)'),
    
    metric = str_remove(
      definition, 
      'manure management > |field burning of agricultural residues > |enteric fermentation > '
    ) %>%
      str_remove('total of ') %>% 
      str_remove('agricultural ') %>% 
      str_remove('carbon-containing ') %>% 
      str_replace_all(' > ', ' '),
    
    variable_name = metric %>% 
      str_remove('Total ') %>% 
      str_remove('emissions ') %>% 
      str_replace('from agriculture ', 'from ag ') %>% 
      str_remove('from manure management ') %>% 
      str_remove('from agriculture: ') %>% 
      str_replace('from field burning of residues ', 'burning residues ') %>% 
      str_remove('from liming, urea application and other ') %>% 
      str_remove('from enteric ') %>% 
      str_remove('residues ') %>% 
      str_remove('from direct and indirect n2o emissions from soils agricultural ') %>% 
      str_replace('other other ', 'other ') %>% 
      str_remove('and roots other ') %>% 
      str_replace('management ', 'manage ') %>% 
      str_replace('cropland ', 'crop ') %>% 
      str_replace('grassland ', 'grass ') %>% 
      str_remove(' \\(Tg\\)'),
    
    variable_name = paste0(
      str_sub(variable_name, end = 3),
      snakecase::to_upper_camel_case(str_sub(variable_name, start = 4))
    )
  )
get_str(un_definitions)

# Pull out just the metric values for results list
un_metrics <- un_definitions %>% 
  select(fips, variable_name, value, year)
get_str(un_metrics)

# Save it
results$ghgs <- un_metrics


## Metadata ----------------------------------------------------------------


# Check vars
(vars <- meta_vars(un_metrics))

# Reformat the definitions and metrics into wide for metadata
get_str(un_definitions)
metas$ghgs <- un_definitions %>% 
  select(variable_name, metric, definition, year) %>% 
  group_by(variable_name, metric, definition) %>% 
  summarize(
    latest_year = max(unique(year)),
    year = paste0(year, collapse = ', ')
  ) %>% 
  mutate(
    axis_name = variable_name,
    dimension = 'environment',
    index = 'carbon, ghg, nutrients',
    indicator = 'fluxes',
    units = 'Tg',
    scope = 'national',
    resolution = 'state',
    updates = "annual",
    source = 'U.S. Environmental Protection Agency State GHG Data (2024).',
    url = 'https://www.epa.gov/ghgemissions/state-ghg-emissions-and-removals',
    warehouse = FALSE
  ) %>% 
  meta_citation(date = '2024-12-18')

metas$ghgs



# EPA NARS ----------------------------------------------------------------
## Lakes -------------------------------------------------------------------


## Load
lakes <- read_csv('1_raw/epa/nars/lakes_data_for_population_estimates_2022.csv')
get_str(lakes)

lakes_meta <- read_tsv(
  '1_raw/epa/nars/lakes_metadata.txt',
  col_names = TRUE
)


## Explore
# Lake data
get_str(lakes)
lakes %>% 
  filter(PSTL_CODE %in% fips_key$state_code) %>% 
  get_str()
lakes %>% 
  filter(PSTL_CODE == 'VT') %>% 
  get_str()

# Meta
get_str(lakes_meta)
lakes_meta %>% 
  filter(str_detect(COLUMN_NAME, 'COND$')) %>% 
  select(COLUMN_NAME, LEGAL_VALUES)
# Condition variables are ordinal

## Clean
# Using condition variables, recoding them as integers
lake_dat <- lakes %>% 
  select(
    PSTL_CODE,
    year = DSGN_CYCLE,
    ends_with('COND'),
    -DRAWDOWN_COND
  ) %>% 
  mutate(
    across(matches('^ACID|^CHLA|^LITCVR|^LITRIPCVR|^NTL|^PTL|^RDIS|^RVEG'), ~ case_when(
      .x == 'Good' ~ 3,
      .x == 'Fair' ~ 2,
      .x == 'Poor' ~ 1,
      .default = NA
    )),
    across(matches('^CYLSPER|^ENT_|MICX_'), ~ case_when(
      .x == 'Above Benchmark' ~ 1,
      .x == 'At or Below Benchmark' ~ 0,
      .default = NA
    ))
  ) %>% 
  
  # Group by geography and take mean of condition scores
  group_by(PSTL_CODE, year) %>% 
  summarize(across(matches('COND$'), ~ mean(.x, na.rm = TRUE))) %>% 
  ungroup() %>% 
  pivot_longer(
    cols = ends_with('COND'),
    names_to = 'variable_name',
    values_to = 'value'
  ) %>% 
  inner_join(state_key, by = join_by(PSTL_CODE == state)) %>% 
  select(fips = state_code, year, variable_name, value) %>% 
  filter(variable_name != 'RDIS_COND')
get_str(lake_dat)

# Check Vermont
lake_dat %>% 
  filter(fips == '50')

results$lakes <- lake_dat



## All Lakes ---------------------------------------------------------------


# ## Load
# # lakes <- read_csv('1_raw/epa/nars/lakes_data_for_population_estimates_2022.csv')
# # lakes2 <- read_csv('1_raw/epa/nars/lakes_data_for_population_estimates_2017.csv')
# # get_str(lakes)
# # get_str(lakes2)
# paths <- list.files(
#   path = '1_raw/epa/nars/',
#   pattern = '.*lakes.*\\.csv',
#   full.names = TRUE
# )
# 
# lakes_meta <- read_tsv(
#   '1_raw/epa/nars/lakes_metadata.txt',
#   col_names = TRUE
# )
# 
# # Load all
# all_lakes <- map(paths, ~ read_csv(.x))
# get_str(all_lakes)
# get_str(all_lakes, 3)
# 
# 
# ## Explore
# # Lake data
# get_str(lakes)
# lakes %>% 
#   filter(PSTL_CODE %in% fips_key$state_code) %>% 
#   get_str()
# lakes %>% 
#   filter(PSTL_CODE == 'VT') %>% 
#   get_str()
# # Plot points in VT
# # vt <- lakes %>% 
# #   filter(PSTL_CODE == 'VT') %>% 
# #   sf::st_as_sf(coords = c('LON_DD83', 'LAT_DD83'))
# # sf::st_crs(vt) <- 4269
# # get_str(vt)
# # mapview(vt)
# 
# 
# # Meta
# get_str(lakes_meta)
# lakes_meta %>% 
#   filter(str_detect(COLUMN_NAME, 'COND$')) %>% 
#   select(COLUMN_NAME, LEGAL_VALUES)
# # Condition variables are ordinal
# 
# ## Clean
# # Using condition variables, recoding them as integers
# lake_dat <- lakes %>% 
#   select(
#     PSTL_CODE,
#     ends_with('COND'),
#     -DRAWDOWN_COND
#   ) %>% 
#   mutate(
#     across(matches('^ACID|^CHLA|^LITCVR|^LITRIPCVR|^NTL|^PTL|^RDIS|^RVEG'), ~ case_when(
#       .x == 'Good' ~ 3,
#       .x == 'Fair' ~ 2,
#       .x == 'Poor' ~ 1,
#       .default = NA
#     )),
#     across(matches('^CYLSPER|^ENT_|MICX_'), ~ case_when(
#       .x == 'Above Benchmark' ~ 1,
#       .x == 'At or Below Benchmark' ~ 0,
#       .default = NA
#     ))
#   ) %>% 
#   
#   # Group by geography and take mean of condition scores
#   group_by(PSTL_CODE) %>% 
#   summarize(across(matches('COND$'), ~ mean(.x, na.rm = TRUE))) %>% 
#   pivot_longer(
#     cols = ends_with('COND'),
#     names_to = 'variable_name',
#     values_to = 'value'
#   ) %>% 
#   inner_join(state_key, by = join_by(PSTL_CODE == state)) %>% 
#   select(fips = state_code, variable_name, value) %>% 
#   mutate(year = '2022') %>% 
#   filter(variable_name != 'RDIS_COND')
# get_str(lake_dat)
# 
# results$lakes <- lake_dat


### Metadata ----------------------------------------------------------------


get_str(results$lakes)

metas$lakes <- lakes_meta %>%
  filter(COLUMN_NAME %in% unique(results$lakes$variable_name)) %>% 
  rename(variable_name = COLUMN_NAME, metric = LABEL) %>% 
  mutate(
    variable_name = paste0('lakes', snakecase::to_upper_camel_case(variable_name)),
    metric = case_when(
      str_detect(metric, '^Enterococci') ~ str_split_i(metric, ',', 1),
      .default = metric
    ), 
    definition = case_when(
      str_detect(LEGAL_VALUES, 'Benchmark') ~ 'Qualitative ratings (Above Benchmark, At or Below Benchmark) recoded to (1, 0) and averaged by state',
      .default = 'Qualitative ratings recoded to 3 (Good), 2 (Fair), and 1 (Poor) and averaged by state'
    ),
    dimension = "environment",
    axis_name = variable_name,
    index = 'water',
    indicator = 'quality',
    units = 'proportion',
    scope = 'national',
    resolution = 'state',
    year = '2022',
    latest_year = '2022',
    updates = "5 years",
    source = 'U.S. Environmental Protection Agency, National Aquatic Resource Surveys, 2022',
    url = 'https://www.epa.gov/national-aquatic-resource-surveys/data-national-aquatic-resource-surveys'
  ) %>% 
  meta_citation(date = '2024-11-14') %>% 
  select(-LEGAL_VALUES)
metas$lakes  

# Go back and fix variable names from dataset
results$lakes <- results$lakes %>% 
  mutate(variable_name = paste0('lakes', snakecase::to_upper_camel_case(variable_name)))

# Check
results$lakes %>% 
  group_by(variable_name, fips, year) %>% 
  summarize(count = n())



## Rivers ------------------------------------------------------------------


rivers <- read_csv('1_raw/epa/nars/rivers_data_for_population_estimates_2019.csv')
get_str(rivers)
rivers
rivers %>% 
  filter(PSTL_CODE == 'VT') %>% 
  get_str()
# Plot points in VT
# vt <- rivers %>% 
#   filter(PSTL_CODE == 'VT') %>% 
#   sf::st_as_sf(coords = c('LON_DD83', 'LAT_DD83'))
# sf::st_crs(vt) <- 4269
# get_str(vt)
# mapview(vt)

rivers_meta <- read.table(
  '1_raw/epa/nars/rivers_metadata.txt',
  header = TRUE, 
  sep = '\t',
  fill = TRUE
)
get_str(rivers_meta)
river_vars <- rivers_meta %>%
  filter(str_detect(COLUMN_NAME, 'COND$')) %>% 
  select(COLUMN_NAME, LEGAL_VALUES) %>% 
  filter(str_detect(LEGAL_VALUES, 'Good')) %>% 
  pull(COLUMN_NAME)
river_vars

river_dat <- rivers %>% 
  inner_join(state_key, by = join_by(PSTL_CODE == state)) %>% 
  select(fips = state_code, all_of(river_vars))
get_str(river_dat)

river_dat <- river_dat %>% 
  group_by(fips) %>% 
  # summarize(across(everything(), ~ mean(.x %in% c('Good', 'Fair')))) %>% 
  summarize(
    across(
      everything(), 
      ~ case_when(
        .x == 'Good' ~ 3,
        .x == 'Fair' ~ 2,
        .x == 'Poor' ~ 1,
        .default = NA
      ) %>% mean(na.rm = TRUE)
    )
  ) %>% 
  pivot_longer(
    cols = !fips,
    values_to = 'value',
    names_to = 'variable_name'
  ) %>% 
  mutate(year = '2019')
get_str(river_dat)

results$rivers <- river_dat



### Metadata ----------------------------------------------------------------


get_str(results$rivers)

metas$rivers <- rivers_meta %>%
  filter(COLUMN_NAME %in% unique(results$rivers$variable_name)) %>% 
  rename(variable_name = COLUMN_NAME, metric = LABEL) %>% 
  mutate(
    variable_name = paste0('rivers', snakecase::to_upper_camel_case(variable_name)),
    definition = case_when(
      str_detect(LEGAL_VALUES, 'Benchmark') ~ 'Qualitative ratings (Above Benchmark, At or Below Benchmark) recoded to (1, 0) and averaged by state',
      .default = 'Qualitative ratings recoded to 3 (Good), 2 (Fair), and 1 (Poor) and averaged by state'
    ),
    axis_name = variable_name,
    dimension = "environment",
    index = 'water',
    indicator = 'quality',
    units = 'index',
    scope = 'national',
    resolution = 'state',
    year = '2019',
    latest_year = '2019',
    updates = "5 years",
    source = 'U.S. Environmental Protection Agency, National Aquatic Resource Surveys, 2022',
    url = 'https://www.epa.gov/national-aquatic-resource-surveys/data-national-aquatic-resource-surveys'
  ) %>% 
  meta_citation(date = '2024-11-14') %>% 
  select(-LEGAL_VALUES)

metas$rivers
get_str(metas$rivers)

# Go back and fix data names
results$rivers <- results$rivers %>% 
  mutate(variable_name = paste0('rivers', snakecase::to_upper_camel_case(variable_name)))



# FSA Disaster Declarations -----------------------------------------------
## Secretary ---------------------------------------------------------------


# NOTE: taking this out for now - replacing with indemnities data hopefully

# # Secretary of Ag disaster declarations - load all
# paths <- list.files(
#   '1_raw/usda/fsa/disaster_declarations/sec/',
#   full.names = TRUE
# )
# ag_raw <- map(paths, read_excel)
#   
# 
# # Select cols 1, 5, and last, filter by fips to NE, combine all years
# # Note that we are not keeping info on what kind of events they are. May regret
# ag_all <- map(ag_raw, ~ {
#   .x %>% 
#     select(1, 5, contains('YEAR')) %>% 
#     setNames(c('fips', 'des', 'year')) %>% 
#     mutate(
#       across(everything(), as.character),
#       year = ifelse(year == '2011, 2012', '2012', year)
#     ) 
# }) %>% 
#   bind_rows()
# get_str(ag_all)
# 
# 
# # Want variables by county, state, whole system
# # county first - number of unique events in each year
# ag_county <- ag_all %>% 
#   filter(fips %in% fips_key$fips) %>% 
#   group_by(fips, year) %>% 
#   summarize(n_des = length(unique(des))) %>% 
#   arrange(fips)
# get_str(ag_county)
# 
# # Get grid of all possibilities of counties and years - make sure no missing
# old_fips <- fips_key %>% 
#   filter_fips('old') %>% 
#   pull(fips)
# all_years <- c(2012:2020, 2022:2023)
# grid <- expand.grid(fips = old_fips, year = all_years) %>% 
#   mutate(across(everything(), as.character))
# 
# # Join back to grid, turn NA into 0
# ag_county <- left_join(grid, ag_county) %>% 
#   mutate(across(everything(), ~ ifelse(is.na(.x), '0', .x)))
# get_str(ag_county)
# 
# # Arrange like a regular variable
# ag_county <- ag_county %>% 
#   rename(nFsaSecDisasters = n_des) %>% 
#   pivot_longer(
#     cols = nFsaSecDisasters,
#     values_to = 'value',
#     names_to = 'variable_name'
#   )
# get_str(ag_county)
# 
# # Save it
# results$fsa_sec <- ag_county
# 
# 
# 
### State -------------------------------------------------------------------
# 
# 
# # Different metric for state, because data only exists at county level
# # For state, do proportion of area of state that had a declaration per year
# # So we need to combine all the counties per state per year, get area, then
# # divide by total area in that state
# get_str(ag_all)
# get_str(areas)
# 
# # Get unique counties with a declaration in each year
# by_state_year <- ag_all %>% 
#   mutate(state_fips = str_sub(fips, end = 2)) %>% 
#   group_by(state_fips, year) %>% 
#   distinct(fips) %>% 
#   ungroup()
# get_str(by_state_year)
# 
# # Now add areas, get total affected per state per year
# area_per_state <- by_state_year %>% 
#   left_join(select(areas, fips, area_sqkm)) %>% 
#   group_by(state_fips, year) %>% 
#   summarize(area_affected = sum(area_sqkm))
# get_str(area_per_state)
# 
# # Add area of each state, and get proportion affected
# prop_by_state <- area_per_state %>% 
#   left_join(
#     select(areas, fips, total_area = area_sqkm), 
#     by = join_by(state_fips == fips)
#   ) %>% 
#   mutate(
#     propAreaFsaSecDisasters = (area_affected / total_area) %>% 
#       ifelse(is.na(.), 0, .) %>% 
#       round(3) %>% 
#       format(nsmall = 3),
#     .keep = 'unused'
#   )
# get_str(prop_by_state)
# 
# # Clean it up into long format
# prop_area_fsa_sec <- prop_by_state %>% 
#   rename(fips = state_fips) %>% 
#   pivot_longer(
#     cols = propAreaFsaSecDisasters,
#     values_to = 'value',
#     names_to = 'variable_name'
#   )
# get_str(prop_area_fsa_sec)
# 
# # Save this to results
# results$prop_area_fsa_sec <- prop_area_fsa_sec
# 
# 
# 
## Presidential ------------------------------------------------------------
# 
# 
# # Presidential disaster declarations
# paths <- list.files(
#   '1_raw/usda/fsa/disaster_declarations/pres/',
#   full.names = TRUE
# )
# pres_raw <- map(paths, read_excel)
# map(pres_raw, get_str)  
# # Select cols 1, 5, and last, filter by fips to NE, combine all years
# # Note that we are not keeping info on what kind of events they are. May regret
# pres <- map(pres_raw, ~ {
#   .x %>% 
#     select(1, 5, contains('YEAR')) %>% 
#     setNames(c('fips', 'des', 'year')) %>% 
#     mutate(
#       across(everything(), as.character)
#     ) %>% 
#     filter(fips %in% fips_key$fips)
# }) %>% 
#   bind_rows()
# get_str(pres)
# 
# 
# # Want variables by county, state, whole system
# # county first - number of unique events in each year
# pres_county <- pres %>% 
#   group_by(fips, year) %>% 
#   summarize(n_des = length(unique(des))) %>% 
#   arrange(fips)
# get_str(pres_county)
# 
# # Get grid of all possibilities of counties and years - make sure no missing
# old_fips <- fips_key %>% 
#   filter_fips('old') %>% 
#   pull(fips)
# all_years <- c(2017:2020, 2023)
# grid <- expand.grid(fips = old_fips, year = all_years) %>% 
#   mutate(across(everything(), as.character))
# 
# # Join back to grid, turn NA into 0
# pres_county <- left_join(grid, pres_county) %>% 
#   mutate(across(everything(), ~ ifelse(is.na(.x), '0', .x)))
# get_str(pres_county)
# 
# # Arrange like a regular variable
# pres_county <- pres_county %>% 
#   rename(nFsaPresDisasters = n_des) %>% 
#   pivot_longer(
#     cols = nFsaPresDisasters,
#     values_to = 'value',
#     names_to = 'variable_name'
#   ) %>% 
#   arrange(year)
# get_str(pres_county)
# 
# # Save
# results$fsa_pres <- pres_county
# 
# 
# 
## Metadata ----------------------------------------------------------------
# 
# 
# names(results)
# results$fsa_pres$variable_name %>% unique
# results$fsa_sec$variable_name %>% unique
# results$fsa_sec$variable_name %>% unique
# 
# check <- bind_rows(
#   results$fsa_pres, 
#   results$fsa_sec, 
#   results$prop_area_fsa_sec
# )
# meta_vars(check)
# 
# metas$fsa <- data.frame(
#   variable_name = meta_vars(check),
#   metric = c(
#     'Number of FSA presidential disaster declarations',
#     'Number of FSA Agriculture Secretary disaster declarations',
#     'Proportion of area affected by FSA Agriculture Secretary disaster declaration'
#   ),
#   definition = c(
#     'Number of unique Presidential major disaster declarations. Used to trigger eligibility for emergency loans and FSA disaster assistance programs.',
#     'Number of unique disaster declarations made by the Secretary of Agriculture. Used to trigger eligibility for emergency loans and FSA disaster assistance programs.',
#     paste(
#       'Proportion of area within a state affected by FSA Agriculture Secretary disaster declarations.',
#       'Declarations are made at the county level, so the area of each county affected is summed and divided by the area of the state.'
#     )
#   ),
#   axis_name = c(
#     'FSA Pres. Disasters',
#     'FSA Sec. Disasters',
#     'Prop Area FSA Sec. Disasters'
#   ),
#   dimension = "environment",
#   index = 'water stability',
#   indicator = 'days / events of extremes',
#   units = c(
#     rep('count', 2),
#     'proportion'
#   ),
#   scope = 'national',
#   resolution = c(
#     rep('county', 2),
#     'state'
#   ),
#   year = get_all_years(check),
#   latest_year = get_max_year(check),
#   updates = "annual",
#   source = 'U.S. Department of Agriculture, Farm Service Agency Disaster Assistance',
#   url = 'https://www.fsa.usda.gov/resources/disaster-assistance-program/disaster-designation-information'
# ) %>% 
#   add_citation('December 11, 2024')
# 
# metas$fsa




# USDA F2S Census ---------------------------------------------------------


# USDA Farm to School Census
# https://farmtoschoolcensus.fns.usda.gov/census-results/census-data-explorer

# Load national summary data. Need specific pages for the data we want
# Have to do these manually to get right cells. Unfortunate

# NOTE: Can get zip code level data if we go to individual response files.
# Trouble is format and variables are different each year - need to wrangle them
# separately. Includes 2023, 2019, 2015, 2013
path <- '1_raw/usda/f2s_census/ops-f2s-2023NationalStateDataWorkbook-120924.xlsx'
census <- list()
census$sfa_participation <- read_xlsx(
  path, 
  sheet = '2. Number of SFAs',
  range = 'A4:F61'
) %>% 
  select(state = 1, sfaFarmToSchool = 4)
census$culture <- read_xlsx(
  path, 
  sheet = '6. F2S Activities',
  range = 'A5:T62'
) %>% 
  select(state = 1, sfaCulturallyRelevant = 20)
census$activities <- read_xlsx(
  path, 
  sheet = '7. F2S Activity Categories',
  range = 'A4:D61'
) %>% 
  select(state = 1, sfaServeLocal = 4)
census$local_spending <- read_xlsx(
  path, 
  sheet = '20. Local Foods Spending',
  range = 'A4:D61'
) %>% 
  select(state = 1, sfaLocalFoodCosts = 4)
get_str(census)
get_str(census, 3)

# Now we can put them together
# all_states <- fips_key$state_code[!is.na(fips_key$state_code)]
census <- map(census, ~ {
  .x %>% 
    mutate(state = ifelse(state == 'National', 'US', state)) %>% 
    filter(state %in% c(state_key$state, 'US')) %>% 
    left_join(select(state_key, fips = state_code, state), by = join_by(state == state)) %>% 
    mutate(fips = ifelse(state == 'US', '00', fips)) %>% 
    select(-state)
}) %>% 
  reduce(inner_join) %>% 
  mutate(sfaLocalFoodCosts = as.numeric(sfaLocalFoodCosts)) %>% 
  mutate(across(is.numeric, ~ format(round(.x, 2), nsmall = 2)))
get_str(census, 3)

# Pivot longer, add year, format like all other variables
census <- census %>%
  pivot_longer(
    cols = !fips,
    values_to = 'value',
    names_to = 'variable_name'
  ) %>% 
  mutate(year = '2023')
get_str(census)  

# Save to results
results$census <- census



## Metadata ----------------------------------------------------------------


(vars <- meta_vars(census))

metas$census <- data.frame(
  variable_name = meta_vars(census),
  metric = c(
    'SFAs serving culturally relevant food',
    'SFAs with Farm to School program',
    'SFAs percentage of spending on local food',
    'SFAs serving local food'
  ),
  definition = c(
    'Percentage of all School Food Authorities growing or serving culturally relevant foods.',
    'Percentage of all School Food Authorities participating in a Farm to School program.',
    'Percentage of total food costs spent on local foods among School Food Authorities which participate in a Farm to School program.',
    'Percentage of all School Food Authorities serving local foods. Note that the definition of "local" can very by SFA.'
  ),
  axis_name = c(
    'SFA Culturally Relevant Food (%)',
    'SFA with Farm to School (%)',
    'SFA Local Food Costs (%)',
    'SFA Serving Local Food (%)'
  ),
  dimension = 'health',
  index = 'food security',
  indicator = c(
    'access to culturally appropriate food',
    'dietary quality',
    'dietary quality',
    'dietary quality'
  ),
  units = 'percentage',
  scope = 'national',
  resolution = 'state',
  year = meta_years(census),
  latest_year = meta_latest_year(census),
  updates = "~ 4 years",
  source = 'U.S. Department of Agriculture Food and Nutrition Service Farm to School Program Census (2023).',
  url = 'https://farmtoschoolcensus.fns.usda.gov/census-results/census-data-explorer'
) %>% 
  meta_citation(date = '2024-12-18')
  
metas$census



# Happiness ---------------------------------------------------------------


# NOTE: taking this out for now, don't much trust it

# # World Population Review / WalletHub
# # https://worldpopulationreview.com/state-rankings/happiest-states
# 
# # Load state data from 2024
# happy <- read_csv('1_raw/world_pop_review/world_pop_review_happiness_scores.csv')
# get_str(happy)
# 
# # Add fips and fix names
# happy_df <- state_key %>% 
#   select(state_name, state_code) %>% 
#   right_join(happy, by = join_by(state_name == state)) %>% 
#   select(-state_name) %>% 
#   setNames(c('fips', 'happinessScore', 'wellbeingRank', 'communityEnvRank', 'workEnvRank')) %>% 
#   mutate(year = 2024)
# get_str(happy_df)
# 
# # Pivot longer to format for metrics data
# happy_df <- happy_df %>% 
#   pivot_longer(
#     cols = happinessScore:workEnvRank,
#     values_to = 'value',
#     names_to = 'variable_name'
#   )
# get_str(happy_df)
# 
# # Save it
# results$happiness <- happy_df



## Metadata ----------------------------------------------------------------


# (vars <- meta_vars(happy_df))
# 
# metas$happiness <- data.frame(
#   variable_name = meta_vars(happy_df),
#   metric = c(
#     'Community and Environment Rank',
#     'Total Happiness Score',
#     'Physical and Emotional Wellbeing Rank',
#     'Work Environment Rank'
#   ),
#   definition = c(
#     'Composite index that includes volunteer rate, ideal weather, average leisure time per day, separation and divorce rate, and safety.',
#     'Weighted average of Physical and Emotional Wellbeing rank, Community and Environment rank, and Work Environment rank.',
#     'Composite index that includes career wellbeing, physical health index, adverse childhood experiences, share of adult depression, social wellbeing, share of adults with alcohol use disorder, adequate sleep rate, sports participation rate, share of adults feeling active and productive, share of adults with poor mental health, life expectancy, suicide rate, and food insecurity rate',
#     'Composite index that includes number of work hours, commute time, share of households earning $75,000, share of adults with anxiety, unemployment rate, share of labor force unemployed 15 weeks or longer, underemployment rate, job security, share of work related stressed tweets, income growth rate, economic security, and median credit score.'
#   ),
#   axis_name = c(
#     'Community and Env Rank',
#     'Happiness Score',
#     'Wellbeing Rank',
#     'Work Environment Rank'
#   ),
#   dimension = 'health',
#   index = 'happiness',
#   indicator = 'happiness tbd',
#   units = c(
#     'rank',
#     'index',
#     rep('rank', 2)
#   ),
#   scope = 'national',
#   resolution = 'state',
#   year = meta_years(happy_df),
#   latest_year = meta_latest_year(happy_df),
#   updates = "annual",
#   source = 'WalletHub Happiest States in America (2025)',
#   url = 'https://worldpopulationreview.com/state-rankings/happiest-states'
# ) %>% 
#   add_citation(date = '2025-02-12')
# metas$happiness



# BEA GDP -----------------------------------------------------------------


# Load API outs
out <- readRDS('5_objects/api_outs/bea_county_gdp.rds')
get_str(out, 3)
map(out, ~ .x$Statistic)
map(out, ~ get_str(.x$Data))

# Add the Statistic (variable name) to each DF, pull selected vars, and combine
dat <- map(out, ~ {
  
  # Set variable names based on given Statistic
  if (str_detect(.x$Statistic, 'Agriculture')) {
    var <- 'gdpFromAg'
  } else {
    var <- 'gdp'
  }
  
  # Select relevant columns, add variable name from Statistic
  .x$Data %>% 
    select(
      fips = GeoFips,
      year = TimePeriod,
      value = DataValue,
      matches('NoteRef')
    ) %>% 
    mutate(variable_name = var)
}) %>% 
  bind_rows()
get_str(dat)

# If NoteRef is D, value should be missing instead of 0
# We will just remove them rather than making them NA
dat <- dat %>% 
  filter(is.na(NoteRef)) %>% 
  select(-NoteRef)
get_str(dat)

results$gdp <- dat



## Metadata ----------------------------------------------------------------


get_str(results$gdp)
meta_vars(results$gdp)

metas$gdp <- data.frame(
  variable_name = meta_vars(results$gdp),
  metric = c(
    'Gross Domestic Product',
    'Gross Domestic Product from Agriculture'
  ),
  definition = c(
    'Gross Domestic Product: All industry total',
    'Gross Domestic Product: Agriculture, forestry, fishing and hunting'
  ),
  dimension = 'util_dimension',
  index = 'util_index',
  indicator = 'util_indicator',
  axis_name = c('GDP', 'GDP from Ag'),
  scope = 'national',
  resolution = 'county',
  year = meta_years(results$gdp),
  latest_year = meta_latest_year(results$gdp),
  updates = "annual",
  source = 'U.S. Bureau of Economic Analysis (2025). CAGDP2 Gross domestic product (GDP) by county and metropolitan area',
  url = 'https://www.bea.gov/'
) %>% 
  meta_citation(date = '2025-07-04')
metas$gdp



# FDA ---------------------------------------------------------------------


# # Load API output
# out <- readRDS('5_objects/api_outs/fda_recalls_ne_2019_2024.rds')
# get_str(out)
# get_str(out$results)
# 
# # Make sure we are in US and it is food. Then select relevant columns
# # Note that we can pull it by zip code and by state, but we will need unique id
# dat <- out$results %>% 
#   filter(country == 'United States', product_type == 'Food') %>% 
#   select(
#     state, 
#     postal_code, 
#     recall_number, 
#     date = recall_initiation_date
#   ) %>% 
#   mutate(
#     zip = str_sub(postal_code, end = 5), 
#     year = str_sub(date, end = 4),
#     .keep = 'unused'
#   )
# get_str(dat)
# 
# # Swap out state for fips key
# dat <- fips_key %>% 
#   select(fips, state_code) %>% 
#   right_join(dat, by = join_by(state_code == state)) %>% 
#   select(-state_code)
# get_str(dat)
# 
# # Here is where we split if we want to keep zip code
# zip_level <- dat %>% 
#   select(-fips) %>% 
#   group_by(zip, year) %>% 
#   summarize(nFoodRecallsZip = n()) %>% 
#   pivot_longer(
#     cols = nFoodRecallsZip,
#     values_to = 'value',
#     names_to = 'variable_name'
#   )
# get_str(zip_level)
# 
# # Save it
# results$food_recall_zip <- zip_level
# 
# # Now aggregate at state level
# state_level <- dat %>% 
#   select(-zip) %>% 
#   group_by(fips, year) %>% 
#   summarize(nFoodRecallsState = n()) %>% 
#   pivot_longer(
#     cols = nFoodRecallsState,
#     values_to = 'value',
#     names_to = 'variable_name'
#   )
# get_str(state_level)
# 
# # Save it
# results$food_recall_state <- state_level
# 
# # Let's just do all of new england while we're at it
# ne_level <- dat %>% 
#   select(-zip, -fips) %>% 
#   group_by(year) %>% 
#   summarize(nFoodRecallsNE = n()) %>% 
#   pivot_longer(
#     cols = nFoodRecallsNE,
#     values_to = 'value',
#     names_to = 'variable_name'
#   )
# get_str(ne_level)
# 
# # Save it
# results$food_recall_ne <- ne_level



## Metadata ----------------------------------------------------------------


# # Check vars
# vars <- map_chr(results, ~ {
#   .x$variable_name %>% 
#     unique
# }) %>% 
#   sort
# vars
# 
# all_years <- map_chr(results, ~ {
#   .x$year %>% 
#     unique %>% 
#     paste0(collapse = ', ')
# }) %>% 
#   sort
# 
# latest_years <- map_chr(results, ~ {
#   .x$year %>%
#     unique() %>% 
#     str_split(', ') %>% 
#     as.character() %>% 
#     max()
# }) %>% 
#   unname()
# 
# metas$unemp <- data.frame(
#   variable_name = vars,
#   dimension = 'production',
#   index = 'product quality',
#   indicator = 'product safety (not livestock)',
#   axis_name = c(
#     'Food Recalls, New England',
#     'Food Recalls by State',
#     'Food Recalls by ZIP'
#   ),
#   metric = 'FDA food recalls',
#   definition = rep('Number of food recall enforcement reports documented in the FDA Recall Enterprise System', 3),
#   units = 'count',
#   annotation = NA,
#   scope = 'national',
#   resolution = c(
#     'New England',
#     'state',
#     'ZIP code'
#   ),
#   year = all_years,
#   latest_year = latest_years,
#   updates = 'weekly',
#   warehouse = FALSE,
#   source = 'U.S. Food and Drug Administration, Recall Enterprise System (2024)',
#   url = 'https://open.fda.gov/apis/food/enforcement/'
# ) %>% 
#   meta_citation(date = '2024-12-16')
# 
# get_str(metas$unemp)



# USDM --------------------------------------------------------------------
## Non-Consecutive Drought -------------------------------------------------


# Pull drought monitor data from API
dm <- readRDS('5_objects/api_outs/usdm_weeks_drought_counties_2000_2023.rds')
get_str(dm)
get_str(dm, 3)
get_str(dm[[1]])

# Drop years with no counties having any droughts at this severity
dm <- dm %>% 
  keep(~ any(map_lgl(.x, ~ any(map_lgl(.x, is.data.frame)))))
get_str(dm, 3)
get_str(dm, 4)

# Combine DFs. Make separate variables for cat 2, 3, and 4 droughts
usdm <- imap(dm, \(year, year_name) {
  out <- map(year, \(state) {
    not_null_dfs <- state %>% 
      keep(\(x) is.data.frame(x)) %>% 
      imap(\(df, cat) {
        cat_num <- str_sub(cat, start = 5)
        df %>% 
          setNames(c('fips', paste0('droughtNonConWeeksCat', cat_num)))
      })
    if (length(not_null_dfs) > 0) {
      reduce(not_null_dfs, full_join, by = "fips")
    } else {
      NULL
    }
  }) %>% 
    keep(\(x) !is.null(x))
  
  if (length(out) > 0) {
    out %>% 
      reduce(full_join) %>%
      mutate(year = str_sub(year_name, start = 2))
  } else {
    NULL
  }
  
}) %>% 
  keep(~ !is.null(.x)) %>% 
  bind_rows()
get_str(usdm, 3)

# Make zeroes explicit
usdm <- usdm %>% 
  complete(fips, year) %>% 
  mutate(across(starts_with('drought'), ~ ifelse(is.na(.x), 0, .x)))
get_str(usdm)

# Pivot longer to format like rest of data
usdm <- usdm %>% 
  pivot_longer(
    cols = starts_with('drought'),
    names_to = 'variable_name',
    values_to = 'value'
  )
get_str(usdm)

# Save
results$usdm <- usdm



## Avg Perc Drought --------------------------------------------------------


# NOTE: Don't plan on using this. It is state level. Leaving it here for now
# though

# We can just take 100 - none, which will give us percent in drought for each week
# Then take average of every week, giving one value per year per state
out <- readRDS('5_objects/api_outs/usdm_perc_area_drought_2020_2024.rds')
get_str(out)
perc_area_drought <- imap(out, ~ {
  .x %>%
    left_join(
      select(state_key, state, state_code),
      by = join_by(stateAbbreviation == state)
    ) %>%
    mutate(perc_drought = 100 - none) %>%
    select(fips = state_code, perc_drought) %>%
    group_by(fips) %>%
    summarize(droughtMeanPercArea = mean(perc_drought, na.rm = TRUE)) %>%
    mutate(year = str_sub(.y, start = 2)) %>%
    pivot_longer(
      cols = droughtMeanPercArea,
      names_to = 'variable_name',
      values_to = 'value'
    )
}) %>%
  bind_rows()
get_str(perc_area_drought)

# Add it onto USDM DF
results$usdm <- bind_rows(results$usdm, perc_area_drought)
get_str(results$usdm)



## Metadata ----------------------------------------------------------------


(vars <- meta_vars(results$usdm))

metas$usdm <- data.frame(
  variable_name = vars,
  dimension = 'environment',
  index = 'water',
  indicator = 'quantity',
  axis_name = c(
    'Mean % Area in Drought',
    'Weeks of Severe Drought',
    'Weeks of Extreme Drought',
    'Weeks of Exceptional Drought'
  ),
  metric = c(
    'Mean percent area in drought',
    'Weeks of severe drought',
    'Weeks of extreme drought',
    'Weeks of exceptional drought'
  ),
  definition = c(
    'Mean of weekly percentages of area that under a drought, as defined by the US Drought Monitor (below 20th percentile for most indicators)',
    'Weeks in the year in which there were severe droughts or worse, as defined by the US Drought Monitor (below 10th percentile for most indicators)',
    'Weeks in the year in which there were extreme droughts or worse, as defined by the US Drought Monitor (below 5th percentile for most indicators)',
    'Weeks in the year in which there were exceptional droughts, as defined by the US Drought Monitor (below 2nd percentile for most indicators)'
  ),
  units = c(
    'percentage',
    rep('count', 3)
  ),
  annotation = NA,
  scope = 'national',
  resolution = c(
    'state',
    rep('county', 3)
  ),
  year = meta_years(results$usdm),
  latest_year = meta_latest_year(results$usdm),
  updates = 'weekly',
  warehouse = FALSE,
  source = 'U.S. Department of Agriculture, U.S. Drought Monitor. (2024).',
  url = 'https://droughtmonitor.unl.edu/DmData/DataDownload.aspx'
) %>% 
  meta_citation(date = '2025-07-04')

get_str(metas$usdm)



# CDC ---------------------------------------------------------------------
## Air Quality -------------------------------------------------------------


# Air quality from api calls
out <- readRDS('5_objects/api_outs/cdc_airquality_ne_counties.rds')
get_str(out)
get_str(out$tableResult)

# Filter down to fips and select relevant columns. Also add variable name
dat <- out$tableResult %>% 
  select(
    fips = geoId,
    year = temporal,
    value = dataValue
  ) %>% 
  filter_fips(scope = 'all') %>% 
  mutate(variable_name = 'airQuality')
get_str(dat)

results$cdc <- dat
  


### Metadata ----------------------------------------------------------------


(vars <- meta_vars(results$cdc))

metas$cdc <- data.frame(
  variable_name = vars,
  dimension = 'environment',
  index = 'carbon, ghg, nutrients',
  indicator = 'carbon fluxes',
  axis_name = 'Air Quality',
  definition = 'Percent of days with PM2.5 levels over the NAAQS',
  metric = 'Air quality',
  units = 'percentage',
  annotation = NA,
  scope = 'national',
  resolution = 'county',
  year = meta_years(results$cdc),
  latest_year = meta_latest_year(results$cdc),
  updates = 'annual',
  warehouse = FALSE,
  source = 'CDC National Environmental Public Health Tracking Network (2025)',
  url = 'https://ephtracking.cdc.gov/'
) %>% 
  meta_citation(date = '2025-07-07')

get_str(metas$cdc)



## Nutrition ---------------------------------------------------------------


# BRFSS 
# TODO: Convert this to API call
# TODO: Take all these data, not just the 3 we wanted here
nut <- read_csv('1_raw/cdc/brfss/Nutrition__Physical_Activity__and_Obesity_-_Behavioral_Risk_Factor_Surveillance_System_20250728.csv') %>% 
  janitor::clean_names()
get_str(nut)

# Check vars
all_vars <- nut$question %>% 
  unique %>% 
  sort()
all_vars

# Pull fruits, vegetables, and the more modest exercise var
good_vars <- all_vars %>% 
  str_subset('fruit|vegetable|at least 150.*equivalent')
good_vars

# Filter to 3 vars, only keep totals (sums of each sample)
dat <- nut %>% 
  filter(
    question %in% good_vars,
    total == 'Total'
    ) %>% 
  select(
    year = year_start,
    state = location_abbr,
    definition = question,
    value = data_value
  )
get_str(dat)

# Replace state codes with FIPS
dat <- state_key %>% 
  select(state, state_code) %>% 
  right_join(dat) %>% 
  select(-state) %>% 
  rename(fips = state_code)
get_str(dat)
  
# Replace definitions with variable names
dat <- dat %>% 
  mutate(
    variable_name = case_when(
      str_detect(definition, 'aerobic') ~ 'percExercise150Min',
      str_detect(definition, 'fruit') ~ 'percDailyFruitLessThanOne',
      str_detect(definition, 'vegetables') ~ 'percDailyVegLessThanOne',
      .default = NA
    )
  )
get_str(dat)

# Pull out a crosswalk for metadata
crosswalk <- dat %>% 
  select(variable_name, definition) %>% 
  unique()
crosswalk

# Remove definition to finish off metrics data
dat <- dat %>% 
  select(-definition)
get_str(dat)

results$brfss <- dat



### Metadata ----------------------------------------------------------------


(vars <- meta_vars(results$brfss))

metas$brfss <- crosswalk %>% 
  arrange(variable_name) %>% 
  mutate(
    dimension = 'health',
    index = 'physical health',
    indicator = c(rep('nutritious diets', 2), 'mobility, pain'),
    metric = c(
      'Percent low daily fruit consumption',
      'Percent low daily vegetable consumption',
      'Percent 150 minute weekly exercise'
    ),
    axis_name = c(
      'Daily Fruit < 1 (%)',
      'Daily Veg < 1 (%)',
      'Exercise 150 Min (%)'
    ),
    units = 'percentage',
    scope = 'national',
    resolution = 'state',
    year = meta_years(results$brfss),
    latest_year = meta_latest_year(results$brfss),
    updates = 'annual',
    source = 'CDC Behavioral Risk Factor Surveillance System (2025).',
    url = 'https://data.cdc.gov/Nutrition-Physical-Activity-and-Obesity/Nutrition-Physical-Activity-and-Obesity-Behavioral/hn4x-zwk7/about_data'
  ) %>% 
  meta_citation(date = '2025-07-28')
get_str(metas$brfss)



## Places ------------------------------------------------------------------


# From CDC Places dataset
# https://data.cdc.gov/500-Cities-Places/PLACES-County-Data-GIS-Friendly-Format-2024-releas/i46a-9kgh/about_data
# Note that not all of our relevant variables exist in all surveys.

# TODO: Use API for this if possible. Would have to chunk it though
# TODO: Take all variables, not just relevant ones

## Load all files 
# Paths for each file
paths <- list.files(
  '1_raw/cdc/places/',
  full.names = TRUE,
  pattern = 'release'
)

# Make a clean name for each file
file_names <- paths %>% 
  str_extract('[0-9]{4}_release')

# Load all files into list and name them
out <- map(paths, ~ { 
  read_csv(.x) %>% 
    janitor::clean_names()
}) %>% 
  setNames(c(file_names))
get_str(out)
get_str(out, 3)

# Get all variable names
all_vars <- map(out, ~ names(.x)) %>% 
  unlist() %>% 
  unique()

# Get relevant vars
good_vars <- all_vars %>% 
  str_subset(
    paste0(
      c(
        'emotionspt',
        'isolation',
        'mobility',
        'chd',
        'highchol'
      ),
    '_crude_prev',
    collapse = '|'
    )
  )
good_vars

# Crosswalk for cdc var, variable_name, definition
crosswalk <- data.frame(cdc_var = good_vars) %>% 
  arrange(cdc_var) %>% 
  mutate(
    variable_name = c(
      'heartDisease',
      'lackSocialSupport',
      'highCholesterol',
      'socialIsolation',
      'mobilityDisability'
    ),
    definition = c(
      'Model-based estimate for crude prevalence of coronary heart disease among adults',
      'Model-based estimate for crude prevalence of lack of social and emotional support among adults',
      'Model-based estimate for crude prevalence of high cholesterol among adults who have ever been screened',
      'Model-based estimate for crude prevalence of feeling socially isolated among adults',
      'Model-based estimate for crude prevalence of mobility disability among adults'
    )
  )


# Pull good vars from each survey, add year, fix names, pivot longer
get_str(out, 3)
dat <- imap(out, ~ {
  yr <- str_sub(.y, end = 4)
  df <- .x %>% 
    select(
      fips = county_fips,
      any_of(good_vars)
    ) %>% 
    mutate(year = yr)
  
  matches <- match(names(df), crosswalk$cdc_var)
  names(df)[!is.na(matches)] <- crosswalk$variable_name[matches[!is.na(matches)]]
 
  df %>% 
    pivot_longer(
      cols = !c(year, fips),
      names_to = 'variable_name',
      values_to = 'value'
    )
})
get_str(dat)
get_str(dat, 3)
# map(dat, ~ unique(.x$variable_name))
# Some variables started later, 1 in 2023, 2 more in 2024

# Bind into one DF
dat <- bind_rows(dat)
get_str(dat)

# Check for var coverage by year
vars <- meta_vars(dat)
map(vars, ~ {
  dat %>% 
    filter(variable_name == .x) %>% 
    group_by(year) %>% 
    summarize(count = n())
}) %>% 
  setNames(c(vars))

results$places <- dat



### Metadata ----------------------------------------------------------------


(vars <- meta_vars(results$places))

metas$places <- crosswalk %>% 
  select(-cdc_var) %>% 
  arrange(variable_name) %>% 
  mutate(
    dimension = case_when(
      str_detect(variable_name, 'Isolation') ~ 'social',
      .default = 'health'
    ) ,
    index = case_when(
      str_detect(variable_name, 'lackSocial') ~ 'mental health',
      str_detect(variable_name, 'socialIsolation') ~ 'social connectedness',
      .default = 'physical health tbd'
    ),
    indicator = case_when(
      str_detect(variable_name, 'mobility') ~ 'mobility, pain',
      str_detect(variable_name, 'lackSocialSupport') ~ 'access to social support',
      str_detect(variable_name, 'Isolation') ~ 'social connectedness',
      .default = 'physical health tbd'
    ),
    metric = c(
      'Coronary heart disease',
      'High cholesterol',
      'Lack of social support',
      'Mobility disability',
      'Social isolation'
    ),
    axis_name = c(
      'Heart Disease (%)',
      'High Chol (%)',
      'Lack Support (%)',
      'Mobility Disability (%)',
      'Social Isolation (%)'
    ),
    units = 'percentage',
    scope = 'national',
    resolution = 'county',
    year = meta_years(results$places),
    latest_year = meta_latest_year(results$places),
    updates = 'annual',
    source = 'CDC Division of Population Health, Epidemiology and Surveillance Branch (2024).',
    url = 'https://data.cdc.gov/500-Cities-Places/PLACES-County-Data-GIS-Friendly-Format-2024-releas/i46a-9kgh/about_data'
  ) %>% 
  meta_citation(date = '2025-08-07')
get_str(metas$places)



# Areas -------------------------------------------------------------------


# Pulling state and county area from our polygons from tidycensus
# Map over both, just taking fips, awater, and aland
dat <- map(list(neast_counties_2024, neast_states), ~ {
  .x %>% 
    sf::st_drop_geometry() %>% 
    select(fips, aland, awater)
}) %>% 
  bind_rows()
get_str(dat)

# Convert to acres (4046.8564224 sq meters per acre)
dat <- dat %>% 
  mutate(across(
    c(aland, awater), 
    ~ .x / 4046.8564224, 
    .names = '{str_sub(.col, start = 2)}Acres'
  ), .keep = 'unused',
  year = NA
)
get_str(dat)

# Pivot to fit with metrics
dat <- dat %>% 
  pivot_longer(
    cols = c(landAcres, waterAcres),
    names_to = 'variable_name',
    values_to = 'value'
  )
get_str(dat)

results$area <- dat



## Metadata ----------------------------------------------------------------


(vars <- meta_vars(dat))

# Abbreviated metadata
metas$area <- data.frame(
  variable_name = vars,
  dimension = 'util_dimension',
  index = 'util_index',
  indicator = 'util_indicator',
  definition = c(
    'Acres of land based on US Census shapefiles from 2024',
    'Acres of water based on US Census shapefiles from 2024'
  ),
  metric = c(
    'Acres of land',
    'Acres of water'
  ),
  units = 'acres',
  axis_name = NA,
  annotation = NA,
  scope = NA,
  resolution = NA,
  year = NA,
  latest_year = NA,
  updates = NA,
  source = 'US Census Bureau',
  url = NA
)

get_str(metas$area)



# RMA ---------------------------------------------------------------------


# Risk management agency for crop losses and indemnities
# https://pubfs-rma.fpac.usda.gov/pub/Web_Data_Files/Summary_of_Business/cause_of_loss/index.html
col_names <- c(
  'year',
  'state_fips',
  'state_abbr',
  'county_fips',
  'county_name',
  'commodity_code',
  'commodity_name',
  'insurance_plan_code',
  'insurance_plan_name',
  'coverage_category',
  'stage_code',
  'cause_of_loss_code',
  'cause_of_loss_desc',
  'month_of_loss',
  'month_of_loss_name',
  'year_of_loss',
  'policies_earning_premium',
  'policies_indemnified',
  'net_planted_quantity',
  'net_endorsed_acres',
  'liability',
  'total_premium',
  'producer_paid_premium',
  'subsidy',
  'state_private_subsidy',
  'additional_subsidy',
  'efa_premium_discount',
  'net_determined_quantity',
  'indemnity_amount',
  'loss_ratio'
)

# Get file paths and names to pull them all
# Not taking 2002 because it crashes R for some reason
paths <- list.files(
  '1_raw/usda/rma/',
  pattern = '*.txt',
  full.names = TRUE
) %>% 
  discard(~ str_detect(.x, 'colsom02'))
names <- list.files(
  '1_raw/usda/rma/',
  pattern = '*.txt'
) %>% 
  discard(~ str_detect(.x, 'colsom02')) %>% 
  str_remove('\\.txt')

# Load all years except 2002
out <- map(paths, ~ {
  read_delim(.x, col_names = col_names, trim_ws = TRUE)
}) %>% 
  setNames(c(names))
get_str(out)
get_str(out, 3)

# Make 5 digit fips, reduce to northeast, choose columns, and bind into one df
dat <- map(out, ~ {
  .x %>% 
    mutate(fips = paste0(state_fips, county_fips)) %>% 
    filter_fips('new') %>% 
    mutate(indemnity_amount = as.numeric(indemnity_amount)) %>% 
    select(fips, year, stage_code, indemnity_amount)
}) %>% 
  bind_rows()
get_str(dat)

# Group by fips, get totals per year
# Filter stage code for final loss payment (FL), get total indemnity
dat <- dat %>% 
  filter(stage_code == 'FL') %>% 
  group_by(fips, year) %>% 
  summarize(totalIndemnities = sum(indemnity_amount))
get_str(dat)

# Check county coverage
dat %>% 
  group_by(year) %>% 
  summarize(n())
dat %>% 
  # group_by(fips) %>% 
  summarize(n())
# Missing a bunch of counties - are they zero?
# Some are nearly zero so it sounds reasonable. Check this

# Checked indemnity tables: https://www.rma.usda.gov/tools-reports/crop-indemnity-maps/crop-indemnity-maps-crop-year-2024
# Looks like yes, many zeroes, and no NAs. So we will expand to complete cases
# and turn implicit NAs into zeroes

# Get master key of all combos of year and fips. Using new CT codes only
all_fips <- fips_key %>% 
  filter(
    str_length(fips) == 5, 
    !(str_detect(fips, '^09.*[^0]$'))
  ) %>% 
  pull(fips)
combos <- expand.grid(all_fips, unique(dat$year)) %>% 
  setNames(c('fips', 'year'))

# Join combos with dataset to make missing data explicit
# Then turn NA into 0
dat <- full_join(dat, combos) %>% 
  mutate(totalIndemnities = ifelse(is.na(totalIndemnities), 0, totalIndemnities))
get_str(dat)
tail(dat)  


# Pivot longer to fit with other metrics
dat <- dat %>% 
  pivot_longer(
    cols = totalIndemnities,
    values_to = 'value',
    names_to = 'variable_name'
  )
get_str(dat)

results$indemnities <- dat



## Metadata ----------------------------------------------------------------


(vars <- meta_vars(dat))

# Abbreviated metadata
metas$indemnities <- data.frame(
  variable_name = vars,
  dimension = 'economics',
  index = 'food business resilience',
  indicator = 'use of ag/farm/crop insurance',
  definition = 'Sum of indemnity amounts for all final loss amounts (stage code = FL)',
  metric = 'Total indemnities',
  units = 'usd',
  axis_name = 'Indemnities ($)',
  annotation = NA,
  scope = NA,
  resolution = 'county',
  year = meta_years(dat),
  latest_year = meta_latest_year(dat),
  updates = 'annual',
  source = 'U.S. Department of Agriculture, Risk Management Agency (2025). Summary of Business - Cause of Loss',
  url = 'https://www.rma.usda.gov/tools-reports'
)

get_str(metas$indemnities)



# Aggregate and Save ------------------------------------------------------


# Put metrics and metadata together into two single DFs
out <- aggregate_metrics(results, metas)

# Check record counts
check_n_records(out$result, out$meta, 'other')

saveRDS(out$result, '5_objects/metrics/other.RDS')
saveRDS(out$meta, '5_objects/metadata/other_meta.RDS')

clear_data(gc = TRUE)
