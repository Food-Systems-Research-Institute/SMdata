# eBird
# 2025-07-10


# Description -------------------------------------------------------------


# Exploring eBird datasets through API and bulk downloads



# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  readr,
  auk
)


  
# Bulk Download -----------------------------------------------------------


ebd <- read_ebd('1_raw/ebird/ebd_US-VT-007_202101_202312_smp_relMay-2025/ebd_US-VT-007_202101_202312_smp_relMay-2025.txt')
get_str(ebd)

ebd <- auk_rollup(ebd)
get_str(ebd)

range(ebd$observation_date)

# Create year column, group by county and year, get unique species counts
out <- ebd %>% 
  mutate(year = str_sub(observation_date, end = 4)) %>% 
  group_by(county_code, year) %>% 
  summarize(
    n_species = length(unique(scientific_name))
  )
out
# This works fine. Have to download a lot of data even for just a single county
# though.



# API ---------------------------------------------------------------------


pacman::p_load(
  httr,
  GET,
  jsonlite,
  RCurl
)

# "https://api.ebird.org/v2/product/stats/{{regionCode}}/{{y}}/{{m}}/{{d}}"
regionCode <- 'US-VT-007'
y <- 2023
m <- 1
d <- 1
url <- glue("https://api.ebird.org/v2/product/stats/{regionCode}/{y}/{m}/{d}")
res <- GET(url)
out <- content(res, as = 'text')
out



## RCurl
regionCode <- 'US-VT-007'
y <- 2023
m <- 1
d <- 1
res <- getURL("https://api.ebird.org/v2/product/stats/{{regionCode}}/{{y}}/{{m}}/{{d}}", .opts=list(followlocation = TRUE))
cat(res)



# Bulker Download ---------------------------------------------------------


# Large download GBIF. 9 states, only 2 years to see how things go. Still
# 45 million rows

pacman::p_load(
  arrow,
  dplyr,
  readr
)

# Tester for now
dat <- read_delim_arrow(
  '1_raw/ebird/ebd_US-VT-007_202101_202312_smp_relMay-2025/ebd_US-VT-007_202101_202312_smp_relMay-2025.txt',
  delim = '\t'
)
get_str(dat)


