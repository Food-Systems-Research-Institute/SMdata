# FIA
# 2025-12-21


# Description -------------------------------------------------------------


# Downloaded a SQLite database for each Northeast state from FIA
# https://research.fs.usda.gov/products/dataandtools/fia-datamart
# Otherwise the whole database is 65+ GB



# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  dbplyr,
  RSQLite,
  vegan,
  readxl,
  tibble,
  tidyr,
  openxlsx2,
  arrow
)

conflicts_prefer(openxlsx2::read_xlsx)

# Invasive species list
invasive <- read_xlsx('1_raw/usfs/v9-5_2024-11_Natl_MasterInvasiveSpeciesList.xlsx')
get_str(invasive)

results <- list()



# Loop through each state -------------------------------------------------

paths <- list.files(
  "1_raw/usfs/FIADB_northeast_states/",
  pattern = "*.db",
  full.names = TRUE,
)
names <- paths %>% 
  str_split_i('_', 6) %>% 
  str_sub(end = 2)

walk2(paths, names, ~ {
  save_path <- paste0('1_raw/usfs/FIADB_northeast_states/partials/', .y, '.parquet')
  con <- dbConnect(RSQLite::SQLite(), .x)
  df <- tbl(con, 'tree')
  df %>% 
    select(
      year = INVYR,
      state_fips = STATECD,
      county_fips = COUNTYCD,
      species = SPCD,
      dia = DIA
    ) %>% 
    filter(year >= 2000) %>% 
    collect() %>% 
    write_parquet(save_path)
  dbDisconnect(con)
})


# Put them all together
paths <- list.files(
  '1_raw/usfs/FIADB_northeast_states/partials/',
  pattern = '*.parquet',
  full.names = TRUE
)
dat <- map(paths, ~ {
  read_parquet(.x)
}) %>% 
  bind_rows()
get_str(dat)



# Wrangle -----------------------------------------------------------------


# Fix fips
dat <- dat %>% 
  mutate(
    state_fips = sprintf("%02d", state_fips),
    county_fips = sprintf("%03d", county_fips),
    fips = paste0(state_fips, county_fips)
  )
get_str(dat)

# Check coverage of fips
dat %>% 
  group_by(fips) %>% 
  summarize(count = n())

# Check coverage of years
dat %>% 
  group_by(year) %>% 
  summarize(count = n())

# Check coverage of fips in 2024
dat %>% 
  filter(year == 2024) %>% 
  group_by(fips) %>% 
  summarize(count = n())



# Complexity --------------------------------------------------------------
## Diversity ---------------------------------------------------------------


# Check counts by county
dat %>% 
  group_by(year, fips) %>% 
  summarize(count = n()) %>% 
  pull(count) %>% 
  range()

# Get matrix of observations
matrix <- dat %>% 
  count(year, fips, species) %>% 
  pivot_wider(
    names_from = species,
    values_from = n,
    values_fill = 0
  )
matrix

# Get diversity
div <- matrix %>% 
  select(3:last_col()) %>% 
  diversity()
length(div)

# Put it back together
div <- matrix %>% 
  select(year, fips) %>% 
  mutate(treeDiversity = div)
get_str(div)

# Clean it up and save to results
results$div <- div %>% 
  pivot_longer(
    cols = treeDiversity,
    values_to = 'value',
    names_to = 'variable_name'
  )
get_str(results$div)



## Size --------------------------------------------------------------------


get_str(dat)
sd_dia <- dat %>% 
  group_by(year, fips) %>% 
  summarize(sizeDiversity = sd(dia, na.rm = TRUE))
get_str(sd_dia)

# Pivot
results$sd_dia <- sd_dia %>% 
  pivot_longer(
    cols = sizeDiversity,
    values_to = 'value',
    names_to = 'variable_name'
  )
get_str(results$sd_dia)



# Health ------------------------------------------------------------------


# Pull in invasive species codes to get proportion invasive
get_str(invasive)

# Get vector of invasive FIA codes
invasive_codes <- invasive %>% 
  select('FIA code') %>% 
  na.omit() %>% 
  pull()
invasive_codes  

# Get proportion invasive out of counts for each county
get_str(dat)
out <- dat %>% 
  group_by(year, fips) %>% 
  summarize(
    obs_count = n(),
    invasive_count = sum(species %in% invasive_codes),
    propInvasive = invasive_count / obs_count
  )
out
get_str(out)

# Arrange like others %>% 
results$invasive <- out %>% 
  select(year, fips, propInvasive) %>% 
  pivot_longer(
    cols = propInvasive,
    values_to = 'value',
    names_to = 'variable_name'
  )
get_str(results$invasive)



# Meta --------------------------------------------------------------------


results <- bind_rows(results)
meta_vars(results)

metas <- data.frame(
  dimension = 'environment',
  index = 'forests',
  indicator = c('forest_health', rep('forest complexity', 2)),
  metric = c(
    'Proportion of invasive species',
    'Tree size diversity',
    'Tree species diversity'
  ),
  axis_name = c(
    'Prop Invasive',
    'SD DBH',
    'Tree Diversity'
  ),
  definition = c(
    'Shannon index of diversity on tree species. Larger values are more diverse.',
    'Standard deviation of distribution of tree sizes as measured by DBH (diameter at breast height)',
    'Proportion of invasive species out of all observed species.'
  ),
  variable_name = meta_vars(results),
  units = c(
    'proportion',
    'standard deviations',
    'index'
  ),
  annotation = 'none',
  latest_year = meta_latest_year(results),
  year = meta_years(results),
  resolution = meta_resolution(results),
  scope = 'national',
  updates = 'annual',
  source = 'U.S. Department of Agriculture, Forest Service (2025). Forest Inventory Analysis.',
  url = 'https://research.fs.usda.gov/products/dataandtools/fia-datamart'
) %>% 
  meta_citation(date = '2025-12-21')
get_str(metas)



# Aggregate ---------------------------------------------------------------


# Check record counts
check_n_records(results, metas, 'Forest Inventory Analysis')

saveRDS(results, '5_objects/metrics/fia.RDS')
saveRDS(metas, '5_objects/metadata/fia_meta.RDS')

clear_data(gc = TRUE)
