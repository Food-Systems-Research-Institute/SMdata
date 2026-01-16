# BLS API
# 2025-07-03 update


# Note: BLS API is not at all fun to use. The bls wrangling script is currently
# using bulk downloads. That might be better, because I can only download one
# year per day from the API before getting rate limited.



# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  jsonlite,
  httr,
  glue,
  snakecase,
  readxl,
  tidyr,
  stringr,
  readr,
  purrr
)

pacman::p_load_gh('mikeasilva/blsAPI')



# QCEW --------------------------------------------------------------------


# Pull counties and states

# Get a list of NE counties and all US states
fips_list <- c(
  fips_key$fips[str_length(fips_key$fips) == 5],
  paste0(state_key$state_code, '000')
) %>% 
  unique()

# Map over whole list. Only goes up to 2023
out <- map(fips_list, \(fips) {
  tryCatch({
    blsQCEW(
      method = 'Area',
      year = '2023',
      quarter = 'a',
      area = fips
    )
  }, error = function(e) {
    cat("Error: ", e$message, "\n")
    return(
      list(
        error_message = e$message,
        fips = fips
      )
    )
  })
})

get_str(out)
# Works!

# Save this as intermediate object so we don't have to call API
# saveRDS(out, '5_objects/api_outs/bls_qcew.rds')


# More Years --------------------------------------------------------------


years <- 2010:2022

out <- map(years, \(yr) {
  Sys.sleep(5)
  map(fips_list, \(fips) {
    tryCatch({
      blsQCEW(
        method = 'Area',
        year = yr,
        quarter = 'a',
        area = fips
      )
    }, error = function(e) {
      cat("Error: ", e$message, "\n")
      return(
        list(
          error_message = e$message,
          fips = fips
        )
      )
    })
  })
})
  
  
 
get_str(out)
# Works!


# BLS API -----------------------------------------------------------------


# Without using blsAPI app, just regular GET requests
# Guess we didn't end up using this though

# series id info: https://www.bls.gov/help/hlpforma.htm#EN
# field definitions: https://www.bls.gov/help/def/en.htm#type
seriesid <- 'ENU04013105111150'

# Try one for 11, agriculture forestry fishing hunting, Chittenden
id <- paste0(
  'ENU', # english, unadjusted
  '50007', # chittenden county
  '4', # average weekly wage
  '0', # size code
  '5', # ownership - they do not explain what the fuck this means
  '000011' # industry code (farming)
)

url <- glue(
  'https://api.bls.gov/publicAPI/v2/timeseries/data/',
  '{id}'
)

response <- GET(
  url, 
  add_headers('Content-Type=application/x-www-form-urlencoded')
)
get_str(response)

out <- content(response, as = 'text') %>%
  fromJSON()
get_str(out)
get_str(out$message)
get_str(out$Results)
get_str(out$Results$series)
get_str(out$Results$series$data)


# test --------------------------------------------------------------------

library(readr)
library(dplyr)

northeast_fips <- c("09", "23", "25", "33", "34", "36", "42", "44", "50")

urls <- paste0("https://data.bls.gov/cew/data/api/2023/county/", northeast_fips, ".csv")

all_data <- lapply(urls, read_csv)

combined <- bind_rows(all_data)

naics_11 <- combined %>%
  filter(industry_code == 11, own_code == 0, agglvl_code == 70)



# test --------------------------------------------------------------------

library(httr)
library(jsonlite)

payload <- list(
  seriesid = c("LAUST010000000000003",  # Alabama
               "LAUST370000000000003",  # North Carolina
               "LAUST360000000000003"), # New York
  startyear = "2022",
  endyear = "2023"
  # registrationKey = "your_api_key"
)

res <- POST(
  url = "https://api.bls.gov/publicAPI/v2/timeseries/data/",
  body = toJSON(payload, auto_unbox = TRUE),
  encode = "json",
  content_type_json()
)

json <- content(res, as = "text", encoding = "UTF-8")
data <- fromJSON(json, flatten = TRUE)
get_str(data)



# old ---------------------------------------------------------------------



# seriesid='BDS0000000000300115110001LQ5'
# 
# url <- glue(
#   'https://api.bls.gov/publicAPI/v2/timeseries/data/',
#   '{seriesid}'
# )
# 
# response <- GET(url, add_headers('Content-Type=application/x-www-form-urlencoded'))
# get_str(response)
# 
# out <- content(response, as = 'text') %>%
#   fromJSON()
# get_str(out$Results$series$data[[1]])

# # This works too if we know series ID up front
