# Icpsr
# 2025-08-18


# Description -------------------------------------------------------------


# NOTE: ICPSR only goes up to 2016. Yet to find a good county level dataset 
# that will work here. Can use arrest data (Wrangle section), but not county 
# level data. More work, but I suppose it works

# NOTE: we are at []. New format for the arrest data. Need to make a state 
# crosswalk because the state codes are NOT fips codes and state names are NOT
# actual state names, they are weird abbreviations. Currently working with 
# the tsv files, but potential with dta - just issues with labels. Maybe 
# that would be better in the end though.
# https://www.icpsr.umich.edu/web/ICPSR/series/57

# Series 2 is what we want



# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  haven
)

load(file = '1_raw/icpsr/ICPSR_39063-V1/ICPSR_39063/DS0002/39063-0002-Data.rda')
dat <- da39063.0002
get_str(dat)



# Explore -----------------------------------------------------------------


# Offense code to see whether violent
dat$OFFENSE

# Check geography
dat$COUNTY
dat$COUNTY %>% get_table()
dat$STATE
dat$STATE %>% get_table()
dat %>% 
  filter(str_detect(STATE, 'Pennsylvania')) %>% 
  pull(COUNTY) %>% 
  get_table()
dat %>% 
  filter(str_detect(STATE, 'Vermont')) %>% 
  pull(COUNTY) %>% 
  get_table()
# NOTE: these are not county fips. They are just counties numbered 1:n in 
# alphabetical order.



# Wrangle -----------------------------------------------------------------


# Make a county crosswalk
crosswalk <- fips_key %>% 
  filter(str_length(fips) == 5) %>% 
  group_by(state_name) %>% 
  mutate(ucr_code = 1:n()) %>% 
  ungroup() %>% 
  select(fips, state_name, ucr_code)
crosswalk
get_str(crosswalk)

# Make new state column with just state fips
get_str(dat)
dat <- dat %>% 
  mutate(
    # state_fips = str_sub(STATE, start = 2, end = 3),
    state_name = str_sub(STATE, start = 6)
  ) %>% 
  left_join(crosswalk, by = join_by(state_name == state_name, COUNTY == ucr_code)) %>% 
  filter(!is.na(fips))
get_str(dat)
dat$fips %>% get_table()
dat$fips %>% unique %>% length

# Narrow down to violent crimes in offense code
# rape, sexual assault, robbery, assault, murder 
# https://nij.ojp.gov/topics/crimes/violent-crime
# Linking to offense codes in documentation
levels(dat$OFFENSE)
violent <- c(
  '011',
  '012',
  '020',
  '030',
  '040',
  '050',
  '080'
)

# Filter down to violent crimes
dat <- dat %>% 
  filter(OFFENSE %in% violent)
get_str(dat)

# Get county pops by summing unique agency values
county_pops <- dat %>%
  group_by(fips, AGENCY) %>%
  slice(1) %>%
  ungroup() %>%
  group_by(fips) %>%
  summarize(county_pop = sum(POP, na.rm = TRUE))
county_pops

# With county pops, we can get crime rate per 100k
get_str(dat)
out <- dat %>% 
  group_by(fips) %>% 
  summarize(crime_count = n()) %>% 
  left_join(county_pops) %>% 
  filter(county_pop != 0) %>% 
  mutate(violentCrimeArrestsPer100k = crime_count / (county_pop / 1e5))
out
  


# One Full Year -----------------------------------------------------------


# dat <- haven::read_dta(file = '1_raw/icpsr/arrests/30762-0001-Data-2009.dta') %>%
dat <- readr::read_tsv('1_raw/icpsr/03729-0001-Data.tsv')
get_str(dat)


# Make a county crosswalk
crosswalk <- fips_key %>% 
  filter(str_length(fips) == 5) %>% 
  group_by(state_name) %>% 
  mutate(ucr_code = 1:n()) %>% 
  ungroup() %>% 
  select(fips, state_name, ucr_code) %>% 
  left_join(
    fips_key %>% 
      select(state_name, state_code) %>% 
      filter(!is.na(state_code))
  )
crosswalk
get_str(crosswalk)

# Make a state crosswalk
# [] we are here

# Use first 2 letters of ORI to get state
# Link this to fips key state_code to get 2-digit code
# Then paste with county already in crosswalk
head(dat$ORI)
dat <- dat %>% 
  mutate(state_code = str_sub(ORI, end = 2)) %>% 
  filter(state_code %in% fips_key$state_code) %>% 
  left_join(select(fips_key, state_fips = fips, state_code), by = 'state_code')
get_str(dat)

# Join to crosswalk
get_str(crosswalk)
get_str(dat)
dat <- dat %>% 
  left_join(
    crosswalk, 
    by = join_by(
      state_code == state_code, 
      COUNTY == ucr_code
    )
  )
get_str(dat)

# Narrow down to violent crimes in offense code
# rape, sexual assault, robbery, assault, murder 
# https://nij.ojp.gov/topics/crimes/violent-crime
# Linking to offense codes in documentation
dat$OFFENSE %>% unique
violent <- c(
  '011',
  '012',
  '020',
  '030',
  '040',
  '050',
  '080'
)

# Filter down to violent crimes
dat <- dat %>% 
  filter(OFFENSE %in% violent)
get_str(dat)

# Get county pop estimate by summing unique agency values
# Note that this is not county pop, but pop reported by included agencies
county_pops <- dat %>%
  group_by(fips, AGENCY) %>%
  slice(1) %>%
  ungroup() %>%
  group_by(fips) %>%
  summarize(county_pop = sum(POP, na.rm = TRUE))
county_pops

# With county pops, we can get crime rate per 100k
get_str(dat)
out <- dat %>% 
  group_by(fips) %>% 
  summarize(crime_count = n()) %>% 
  left_join(county_pops) %>% 
  filter(county_pop != 0) %>% 
  mutate(violentCrimesPer10k = crime_count / (county_pop / 1e5))
out
  


# All Years ---------------------------------------------------------------
## Load --------------------------------------------------------------------


# Get folder paths
icpsr_paths <- list.files(
  path = '1_raw/icpsr/ucr_arrests/',
  pattern = 'ICPSR*',
  full.names = TRUE
)

# Get real file paths
paths <- map_chr(icpsr_paths, ~ {
  new_path <- paste0(.x, '/DS0001')
  list.files(
    path = new_path,
    pattern = '*.tsv',
    full.names = TRUE
  )
})

# Load all files. Combine, and keep only relevant columns
all <- map(paths, ~ {
  .x %>% 
    readr::read_tsv() %>% 
    select(ORI, YEAR, POP, OFFENSE, AGENCY, COUNTY)
})
get_str(all)



## Prep --------------------------------------------------------------------


# Make a county crosswalk. Use it to fix UCR county codes and get real FIPS
crosswalk <- fips_key %>% 
  filter(str_length(fips) == 5) %>% 
  group_by(state_name) %>% 
  mutate(ucr_code = 1:n()) %>% 
  ungroup() %>% 
  select(fips, state_name, ucr_code) %>% 
  left_join(
    fips_key %>% 
      select(state_name, state_code) %>% 
      filter(!is.na(state_code))
  )
crosswalk
get_str(crosswalk)

# Make a vector of violent crime codes we will use to filter below
# rape, sexual assault, robbery, assault, murder 
# https://nij.ojp.gov/topics/crimes/violent-crime
# Linking to offense codes in documentation
all[[1]]$OFFENSE %>% unique
violent <- c(
  '011',
  '012',
  '020',
  '030',
  '040',
  '050',
  '080'
)



## Map  --------------------------------------------------------------------


# Map through all years using assets above
get_str(all)
get_str(all[[1]])

out <- map(all, ~ {
  
  year <- .x$YEAR %>% 
    na.omit() %>% 
    unique()
  
  df <- .x %>% 
    mutate(state_code = str_sub(ORI, end = 2)) %>% 
    filter(state_code %in% fips_key$state_code) %>% 
    left_join(select(fips_key, state_fips = fips, state_code), by = 'state_code')
  
  # Join to crosswalk to get proper fips
  df <- df %>% 
  left_join(
    crosswalk, 
    by = join_by(
      state_code == state_code, 
      COUNTY == ucr_code
    )
  )
  
  # Filter to violent offenses
  df <- df %>% 
    filter(OFFENSE %in% violent)
  
  # Get couty pops for year
  county_pops <- df %>%
    group_by(fips, AGENCY) %>%
    slice(1) %>%
    ungroup() %>% 
    group_by(fips) %>%
    summarize(county_pop = sum(POP, na.rm = TRUE))
  county_pops
  
  # Get crime rate per 100k
  df <- df %>% 
    group_by(fips) %>% 
    summarize(crime_count = n()) %>% 
    left_join(county_pops) %>% 
    filter(!is.na(fips)) %>%
    mutate(
      county_pop = ifelse(county_pop == 0, NA, county_pop),
      value = crime_count / (county_pop / 1e5),
      variable_name = 'violentCrimeArrestsPer100k',
      year = year
    ) %>% 
    select(
      fips,
      year,
      variable_name,
      value
    )
  
  return(df)
}) %>% 
  bind_rows()

get_str(out)
out %>% 
  filter(str_detect(fips, '^50'))



# Metadata ----------------------------------------------------------------


(vars <- meta_vars(out))

meta <- data.frame(
  dimension = 'social',
  index = 'food system governance',
  indicator = 'community safety',
  variable_name = vars,
  metric = 'Violent Crime Rate',
  definition = 'Number of arrests for violent crimes (assault, robbery, murder) per 100k residents',
  axis_name = 'Crime Rate (per 100k)',
  units = 'Rate per 100k',
  scope = 'national',
  resolution = 'county',
  year = meta_years(out),
  latest_year = meta_latest_year(out),
  updates = "annual",
  source = 'U.S. Federal Bureau of Investigation, Uniform Crime Reporting Program',
  url = 'https://www.icpsr.umich.edu/web/ICPSR/search/studies?AUTHOR_FACET=United%20States.%20Federal%20Bureau%20of%20Investigation'
) %>%  
  meta_citation(date = '2025-09-14')



# Save and Clear ----------------------------------------------------------


# Check record counts
check_n_records(out, meta, 'icpsr')

saveRDS(out, '5_objects/metrics/icpsr_crime.RDS')
saveRDS(meta, '5_objects/metadata/icpsr_crime.RDS')

clear_data(gc = TRUE)
