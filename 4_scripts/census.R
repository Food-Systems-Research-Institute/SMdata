# Census API
# 2024-10-02

# Variables list: https://api.census.gov/data/2023/acs/acs1/variables.html


# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  purrr,
  readr,
  tibble,
  janitor,
  tidyr,
  stringr
)

# lists of results
results <- list()
metas <- list()



# ACS5 --------------------------------------------------------------------


# Wrangle ACS5, calculating some derived variables
out <- readRDS('5_objects/api_outs/acs5_neast_counties_states_2008_2023.rds')
get_str(out)

# Make normal fips column
dat <- out %>% 
  mutate(fips = paste0(state, ifelse(is.na(county), '', county))) %>% 
  select(-c(state, county))
get_str(dat)

# Check ranges for weird values
map(dat, range, na.rm = TRUE)
# median income is hinky, also housing year

# Recode -666666666 to missing for income and 0 to missing for housing year
dat <- dat %>% 
  mutate(
    across(everything(), ~ case_when(
      .x == -666666666 | .x == 0 ~ NA,
      .default = .x
    ))
  )
get_str(dat)           
map(dat, range, na.rm = TRUE)
  
  
# Calculations to get proportions for education, housing, earnings
# edPercHsOrMore is:
#   edTotal - (BS, Phd, Prof, Master, Assoc, some college, some college < 1y)
# 'edTotalBS' = 'B15003_022E',
# 'edTotalPhD' = "B15003_025E",
# 'edTotalProf' = "B15003_024E",
# 'edTotalMaster' = "B15003_023E",
# 'edTotalAssoc' = "B15003_021E",
# 'edTotalSomeCollege' = "B15003_020E",
# 'edTotalSomeCollegeLessThanYear' = "B15003_019E",
dat <- dat %>% 
  mutate(
    edPercHSGED = ((edTotalHS + edTotalGED) / edTotal) * 100,
    edPercBS = (edTotalBS / edTotal) * 100,
    edPercHSOrMore = ((edTotal - (edTotalBS + edTotalPhD + edTotalProf + edTotalMaster + edTotalSomeCollege + edTotalSomeCollegeLessThanYear)) / edTotal) * 100,
    vacancyRate = (nHousingVacant / nHousingUnits) * 100,
    womenEarnPercMenFFF = (medianFemaleEarningsFFF / medianMaleEarningsFPS) * 100,
    womenEarnPercMenFPS = (medianFemaleEarningsFPS / medianMaleEarningsFPS) * 100,
    disconnectedYouth = (nMaleNotEnrolledHSGradNotInLaborForce + nMaleNotEnrollNoGradNotInLaborForce +
      nFemaleNotEnrollHSGradNotInLaborForce + nFemaleNotEnrollNoGradNotInLaborForce) / n16to19 * 100
  ) %>% 
  select(-matches('edTotal|^nMale|^nFemale|^n16'))
get_str(dat)
map(dat, range, na.rm = TRUE)
  
# Pivot longer
dat <- dat %>% 
  pivot_longer(
    cols = !c(year, fips),
    names_to = 'variable_name',
    values_to = 'value'
  )
get_str(dat)



# ACS1 --------------------------------------------------------------------


# Now for ACS1, just one population variable
acs1 <- readRDS('5_objects/api_outs/acs1_neast_counties_states_2000_2023.rds')
get_str(acs1)
range(acs1$populationAnnual)

acs1 <- acs1 %>% 
  mutate(
    fips = case_when(
      is.na(county) ~ state,
      .default = paste0(state, county)
    ),
    .keep = 'unused'
  ) %>% 
  pivot_longer(
    cols = populationAnnual,
    names_to = 'variable_name',
    values_to = 'value'
  )
get_str(acs1)

# Combine with ACS5 above
dat <- bind_rows(dat, acs1)
get_str(dat)
  


# Smooth Population -------------------------------------------------------


# Smoothing out 5-year population to be even steps between those years

# First make sure population5Year is properly capitalized
# TODO: Remove this once we run census API calls again - will be moot
get_str(dat)
dat <- dat %>% 
  mutate(variable_name = case_when(
    variable_name == 'population5year' ~ 'population5Year',
    .default = variable_name
  ))

pop <- filter(dat, variable_name == 'population5Year')
get_str(pop)

# Complete range of years
range(pop$year)
(full_range <- 2013:2023)
pop <- pop %>% 
  tidyr::complete(fips, year = full_range, variable_name)
get_str(pop)
pop

# Group by year and fips, fill in missing values
pop <- pop %>% 
  group_by(fips) %>% 
  mutate(
    value = na.approx(value, na.rm = FALSE),
    variable_name = 'population5YearSmooth'
  ) %>% 
  ungroup()
pop
get_str(pop)

# Add it back to rest of dataset
get_str(dat)
dat <- bind_rows(dat, pop)



# Metadata ----------------------------------------------------------------


meta_vars(dat)

# Load from CSV
meta <- read_csv('5_objects/metadata_csv/census_meta.csv')
get_str(meta)

meta <- meta %>% 
  arrange(variable_name) %>% 
  mutate(
    resolution = meta_resolution(dat),
    updates = case_when(
      str_detect(variable_name, '5year') ~ 'annual',
      .default = '5 years'
    ),
    latest_year = meta_latest_year(dat),
    year = meta_years(dat)
  ) %>% 
  meta_citation(date = '2025-07-13')
get_str(meta)



# Save and Clear ----------------------------------------------------------


# Check to make sure we have the same number of metrics and metas
check_n_records(dat, meta, 'Census')

# Save them
saveRDS(dat, '5_objects/metrics/census.RDS')
saveRDS(meta, '5_objects/metadata/census_meta.RDS')

clear_data(gc = TRUE)
