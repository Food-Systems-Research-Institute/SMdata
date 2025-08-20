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



# Wrangle -----------------------------------------------------------------


# Make a county crosswalk
crosswalk <- fips_key %>% 
  filter(str_length(fips) == 5) %>% 
  group_by(state_name) %>% 
  mutate(ucr_code = 1:n()) %>% 
  ungroup() %>% 
  select(fips, state_name, ucr_code)
  # mutate(state_fips = str_sub(fips, end = 2))
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

# With county pops, we can get crime rate per 10k
get_str(dat)
out <- dat %>% 
  group_by(fips) %>% 
  summarize(crime_count = n()) %>% 
  left_join(county_pops) %>% 
  filter(county_pop != 0) %>% 
  mutate(violentCrimesPer10k = crime_count / (county_pop / 10000))
out
  


## Check -------------------------------------------------------------------





## All Years ---------------------------------------------------------------


# dat <- haven::read_dta(file = '1_raw/icpsr/arrests/30762-0001-Data-2009.dta') %>% 
dat <- readr::read_tsv('1_raw/icpsr/arrests/30762-0001-Data-2009.tsv')
get_str(dat)


# Make a county crosswalk
crosswalk <- fips_key %>% 
  filter(str_length(fips) == 5) %>% 
  group_by(state_name) %>% 
  mutate(ucr_code = 1:n()) %>% 
  ungroup() %>% 
  select(fips, state_name, ucr_code)
crosswalk
get_str(crosswalk)

# Make a state crosswalk
# [] we are here

# Make new state column with just state fips
get_str(dat)
dat$STNAME %>% unique %>% sort
dat <- dat %>% 
  mutate(STNAME = stringr::str_to_title(STNAME)) %>% 
  left_join(crosswalk, by = join_by(STNAME == state_name, COUNTY == ucr_code)) %>% 
  filter(!is.na(fips))
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

# Get county pops by summing unique agency values
county_pops <- dat %>%
  group_by(fips, AGENCY) %>%
  slice(1) %>%
  ungroup() %>%
  group_by(fips) %>%
  summarize(county_pop = sum(POP, na.rm = TRUE))
county_pops

# With county pops, we can get crime rate per 10k
get_str(dat)
out <- dat %>% 
  group_by(fips) %>% 
  summarize(crime_count = n()) %>% 
  left_join(county_pops) %>% 
  filter(county_pop != 0) %>% 
  mutate(violentCrimesPer10k = crime_count / (county_pop / 10000))
out
  





# County Level -----------------------------------------------------------


# Using reported crimes - 0004
load(file = '1_raw/icpsr/ICPSR_33523-V2/ICPSR_33523/DS0004/33523-0004-Data.rda')
dat <- da33523.0004
get_str(dat)

# Pick relevant columns and get proper fips codes
dat <- dat %>% 
  select(FIPS_ST, FIPS_CTY, pop = CPOPCRIM, VIOL) %>% 
  mutate(
    state_fips = sprintf("%02d", FIPS_ST),
    county_fips = sprintf("%03d", FIPS_CTY),
    fips = paste0(state_fips, county_fips)
  ) %>% 
  select(fips, pop, VIOL)
get_str(dat)

# Filter to NEast, group by fips to get violent crimes per 10,000 people
dat <- dat %>% 
  filter(fips %in% fips_key$fips) %>% 
  group_by(fips) %>% 
  summarize(
    total_pop = sum(pop, na.rm = TRUE),
    violent_crimes = sum(VIOL, na.rm = TRUE)
  ) %>% 
  mutate(crime_rate = violent_crimes / (total_pop/10000))
get_str(dat)



## All Years ---------------------------------------------------------------


# Using reported crimes - 0004
# Use a dta file to get col names
paths <- list.files(
  '1_raw/icpsr/county/',
  pattern = '*.dta',
  full.names = TRUE
)
years <- paths %>% 
  str_extract("\\d{4}(?=\\.dta)")
dat <- map2(paths, years, ~ haven::read_dta(.x) %>% mutate(year = .y)) %>% 
  setNames(c(years))
get_str(dat)
get_str(dat, 3)

# Bind together, then pick relevant columns and get proper fips codes
dat <- dat %>% 
  bind_rows() %>% 
  select(year, FIPS_ST, FIPS_CTY, pop = CPOPCRIM, VIOL) %>% 
  mutate(
    state_fips = sprintf("%02d", FIPS_ST),
    county_fips = sprintf("%03d", FIPS_CTY),
    fips = paste0(state_fips, county_fips)
  ) %>% 
  select(fips, year, pop, VIOL)
get_str(dat)

# Filter to NEast, group by fips and year to get violent crimes per 10,000 people
dat <- dat %>% 
  filter(fips %in% fips_key$fips) %>% 
  group_by(fips, year) %>% 
  summarize(
    total_pop = sum(pop, na.rm = TRUE),
    violent_crimes = sum(VIOL, na.rm = TRUE)
  ) %>% 
  mutate(
    violentCrimePer10k = violent_crimes / (total_pop/10000),
    .keep = 'unused'
  )
get_str(dat)





