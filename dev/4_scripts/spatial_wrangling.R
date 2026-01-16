#' Spatial Wrangling
#' 2025-07-02 update

#' Getting spatial features, counties, fips and states codes sorted out and 
#' saved to be used throughout project. Original script from USDA FAME warehouse

#' Note that this script has been a slow drip of additions leading to some 
#' pretty gross code, and calling from tigris more than necessary. Update this
#' at some point.



# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  stringr,
  readr,
  purrr,
  janitor,
  skimr,
  tidyr,
  tidycensus,
  sf,
  rmapshaper
)



# FIPS Keys ---------------------------------------------------------------


# Vector of New England states
neng_st_codes <- c('CT', 'ME', 'MA', 'NH', 'RI', 'VT')
neast_st_codes <- c('CT', 'ME', 'MA', 'NH', 'NJ', 'NY', 'PA', 'RI', 'VT')

# Get county and state fips, state name, county name for New England
neng_counties <- tidycensus::fips_codes %>% 
  filter(state %in% neng_st_codes) %>% 
  unite(
    "fips", 
    c(state_code, county_code), 
    sep = "", 
    remove = FALSE
  ) %>% 
  rename(county_name = county) %>% 
  select(fips, county_name, state_name) %>% 
  `rownames<-`(NULL)

# Same for Northeast
neast_counties <- tidycensus::fips_codes %>% 
  filter(state %in% neast_st_codes) %>% 
  unite(
    "fips", 
    c(state_code, county_code), 
    sep = "", 
    remove = FALSE
  ) %>% 
  rename(county_name = county) %>% 
  select(fips, county_name, state_name) %>% 
  `rownames<-`(NULL)
  
# Same for New England states
neng_states <- tidycensus::fips_codes %>% 
  filter(state %in% neng_st_codes) %>% 
  select(state_code, state_name) %>% 
  rename(fips = state_code) %>% 
  distinct() %>% 
  mutate(state_code = neng_st_codes) %>% 
  `rownames<-`(NULL)

# and Northeast states
neast_states <- tidycensus::fips_codes %>% 
  filter(state %in% neast_st_codes) %>% 
  select(state_code, state_name) %>% 
  rename(fips = state_code) %>% 
  distinct() %>% 
  mutate(state_code = neast_st_codes) %>% 
  `rownames<-`(NULL)

# Merge so we have county and state data in one data frame
# This will be our fips key
# formerly county_state
neast_county_state <- bind_rows(neast_counties, neast_states) %>%
  add_row(
    fips = "00",
    county_name = NA,
    state_name = "US",
    state_code = 'US'
  )
neng_county_state <- bind_rows(neng_counties, neng_states) %>%
  add_row(
    fips = "00",
    county_name = NA,
    state_name = "US",
    state_code = 'US'
  )



# Spatial Data ------------------------------------------------------------


# Import northeast county spatial data frame (before CT changes)
neast_counties_2021 <- tigris::counties(
  state = neast_st_codes, 
  progress_bar = TRUE,
  year = 2021
) %>% 
  clean_names() %>% 
  mutate(fips = paste0(statefp, countyfp)) %>% 
  select(fips, aland, awater, geometry) %>% 
  ms_simplify(keep = 0.05)

# Import county spatial data frame for 2024 (after CT changes)
neast_counties_2024 <- tigris::counties(
  state = neast_st_codes, 
  progress_bar = TRUE,
  year = 2024
) %>% 
  clean_names() %>% 
  mutate(fips = paste0(statefp, countyfp)) %>% 
  select(fips, aland, awater, geometry) %>% 
  ms_simplify(keep = 0.05)

# Get spatial data for all counties (using this for area later)
all_counties_2021 <- tigris::counties(
  state = NULL, 
  progress_bar = TRUE,
  year = 2021
) %>% 
  clean_names() %>% 
  mutate(fips = paste0(statefp, countyfp)) %>% 
  select(fips, aland, awater, geometry) %>% 
  ms_simplify(keep = 0.05)
  

# Get spatial data for all counties (using this for area later)
all_counties_2024 <- tigris::counties(
  state = NULL, 
  progress_bar = TRUE,
  year = 2024
) %>% 
  clean_names() %>% 
  mutate(fips = paste0(statefp, countyfp)) %>% 
  select(fips, aland, awater, geometry) %>% 
  ms_simplify(keep = 0.05)

# Also get spatial files for northeast states
states_2024 <- tigris::states(
  progress_bar = TRUE,
  year = 2024
) %>% 
  filter(STUSPS %in% neast_st_codes) %>% 
  setNames(snakecase::to_snake_case(names(.))) %>% 
  select(
    fips = statefp,
    name,
    aland,
    awater,
    intptlat,
    intptlon,
    geometry
  ) %>% 
  ms_simplify(keep = 0.05)

# Make a combined single polygon of neast states - use as bbox later
neast_mask <- st_union(states_2024)
  
# And do it again for all states - not just northeast
all_states_2024 <- tigris::states(
  progress_bar = TRUE,
  year = 2024
) %>% 
  setNames(snakecase::to_snake_case(names(.))) %>% 
  select(
    fips = statefp,
    name,
    aland,
    awater,
    intptlat,
    intptlon,
    geometry
  ) %>% 
  ms_simplify(keep = 0.05)

# Also just get fips codes for all states - keep these separate
all_state_codes <- fips_codes %>% 
  select(state, state_code, state_name) %>% 
  unique() %>% 
  filter(str_detect(
    state_name, 
    'Mariana|Guam|Rico|Islands|Samoa', 
    negate = TRUE
  )) %>% 
  mutate(full_state_code = paste0(state_code, '000'))

# One more vector of ALL relevant fips codes 
# This includes all northeast counties, but also 51 states (not their counties)
# also US
all_fips <- c(
  all_state_codes$full_state_code,
  all_state_codes$state_code,
  neast_county_state$fips
) %>% unique()



# Area DF -----------------------------------------------------------------


# creating DF with all county and state fips, and their area

## neast FIPS key
neast_county_state
neast_counties_2021
neast_counties_2024

# Take 2024 counties first. Then just add remaining 2021 counties not included
most_areas <- reduce(list(all_counties_2024, all_states_2024), bind_rows)

# Pull just old 2021 counties from CT
old_ct <- all_counties_2021 %>% 
  filter(str_detect(fips, '^09.{2}[1-9]'))

# Combine old CT counties with rest of areas
areas <- bind_rows(most_areas, old_ct)

# Just get clean DF with fips and area, put areas in sq km units
area_df <- areas %>% 
  st_drop_geometry() %>% 
  select(fips, area_sqkm = aland, water_sqkm = awater) %>% 
  mutate(across(c(area_sqkm, water_sqkm), ~ round(.x / 1e6, 2)))
get_str(area_df)



# Save and Clear ----------------------------------------------------------


# Save everything as RDS and as gpkg where appropriate

# Our standard fips key will be for northeast states and counties
saveRDS(neast_county_state, '5_objects/fips_key.rds')

# Also keeping this legacy new england fips key just in case
saveRDS(neng_county_state, '5_objects/neng_fips_key.rds')

saveRDS(all_state_codes, '5_objects/state_key.rds')
saveRDS(all_fips, '5_objects/all_fips.rds')

# Northeast counties polygons 2021 (not saving New England shapefiles)
saveRDS(neast_counties_2021, '2_clean/spatial/neast_counties_2021.RDS')
st_write(neast_counties_2021, '2_clean/spatial/neast_counties_2021.gpkg', append = FALSE)

# Northeast counties polygons 2024
saveRDS(neast_counties_2024, '2_clean/spatial/neast_counties_2024.RDS')
st_write(neast_counties_2024, '2_clean/spatial/neast_counties_2024.gpkg', append = FALSE)

# Same for northeast states
saveRDS(states_2024, '2_clean/spatial/neast_states.RDS')
st_write(states_2024, '2_clean/spatial/neast_states.gpkg', append = FALSE)

# Mask for all neast states
saveRDS(neast_mask, '2_clean/spatial/neast_mask.RDS')
st_write(neast_mask, '2_clean/spatial/neast_mask.gpkg', append = FALSE)


# All states and counties
saveRDS(all_states_2024, '2_clean/spatial/all_states.RDS')
st_write(all_states_2024, '2_clean/spatial/all_states.gpkg', append = FALSE)
saveRDS(all_counties_2021, '2_clean/spatial/all_counties_2021.RDS')
st_write(all_counties_2021, '2_clean/spatial/all_counties_2021.gpkg', append = FALSE)
saveRDS(all_counties_2024, '2_clean/spatial/all_counties_2024.RDS')
st_write(all_counties_2024, '2_clean/spatial/all_counties_2024.gpkg', append = FALSE)

# DF of areas for neast counties and states
saveRDS(area_df, '5_objects/areas.RDS')

# Clear
clear_data(gc = TRUE)
