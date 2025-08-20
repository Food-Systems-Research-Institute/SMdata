# Weights
# 2025-08-18


# Description -------------------------------------------------------------

# Working with our weighting variables. Need to smooth out years between
# ACS5 population estimates for all counties, for example.


# Housekeeping ------------------------------------------------------------

pacman::p_load(
  dplyr,
  stringr,
  purrr
)

# devtools::load_all()
get_str(weighting)
weighting$metric %>% sort



# Smooth 5-year Population ------------------------------------------------


# Plan is to pull metric data, from objects, smooth it out, then save it back

# First explore
dat <- readRDS('5_objects/metrics/census.RDS')
get_str(dat)

pop <- filter(dat, variable_name == 'population5year')
pop


# Now group by fips, make sure all years are represented,
