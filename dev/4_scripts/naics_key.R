#' NAICS key
#' 2025-06-30

#' Using selection of NAICS codes from Allison Bauman in FAME data warehouse
#' to make a key to each code


# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  stringr,
  readr,
  janitor,
  tidyr
)



# Key ---------------------------------------------------------------------


food_codes <- c(
  '111',
  '112',
  '114',
  '115',
  '311',
  '484',
  '493',
  '445',
  '722',
  '3111',
  '3112',
  '3113',
  '3114',
  '3115',
  '3116',
  '3117',
  '3118',
  '3119',
  '3121',
  '3122',
  '4244',
  '4245',
  '4248',
  '4451',
  '4452',
  '4453',
  '7223',
  '7224',
  '7225',
  '32532',
  '33311',
  '42491',
  '44511',
  '44512',
  '311811',
  '72232',
  '72233',
  '722511'
)

# Load codes and titles from BLS
naics_codes <- read_csv('1_raw/bls/2022_titles_descriptions.csv')
get_str(naics_codes)

# Filter the full set of NAICS codes down to the food codes
naics_key <- naics_codes %>% 
  setNames(c('naics', 'short_title', 'full_title', 'description')) %>% 
  filter(naics %in% food_codes)
get_str(naics_key)



# Save and Clear ----------------------------------------------------------


saveRDS(naics_key, '5_objects/naics_key.rds')

clear_data()
