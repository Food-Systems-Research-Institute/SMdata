api_nass_census_setup <- function(key) {
  nassqs_auth(get_env_var(key))
  
  # define fips - have to split back into 2 digit state and 3 digit county
  states_only <- fips_key$fips[str_length(fips_key$fips) == 2]
  all_counties <- fips_key %>%
    filter(str_length(fips) == 5) %>%
    pull(fips)
  
  # use these two vectors to query. goes through counties then states
  county_fips <- c(str_sub(all_counties, start = 3), rep('', length(states_only)))
  state_fips <- c(str_sub(all_counties, end = 2), states_only)
  stopifnot(length(county_fips) == length(state_fips))
  
  return(list(
    states_only = states_only,
    county_fips = county_fips,
    state_fips = state_fips
  ))
}
  

call_api_nass_census <- function(nass_params, nass_api_fips, config) {
  census_params <- nass_params %>%
    dplyr::filter(
      source_desc == 'CENSUS',
      short_desc != 'derived',
      !is.na(short_desc)
    )

  params <- list(
    source_desc = 'CENSUS',
    domain_desc = 'TOTAL',
    agg_level_desc = c('COUNTY', 'STATE'),
    state_fips_code = nass_api_fips$states_only[1:9],
    year__GE = config$year_start,
    year__LE = config$year_end
  )
  
  vars <- limit_calls(census_params$short_desc, config$api_limit)
  
  out <- imap(vars, ~ {
    cat('\n\nStarting:', .x, '\n', .y, 'of', length(vars), '\n')
    params[['short_desc']] <- .x
    Sys.sleep(config$api_sleep)
    tryCatch({
      nassqs(params)
    },
      error = function(e) {
        message('Error. Returning NULL')
        return(NULL)
      }
    )
  }, .progress = TRUE) %>%
    purrr::list_rbind()
  
  return(out)
}


call_api_nass_farm <- function(nass_params, nass_api_fips, config) {
  farm_params <- nass_params %>%
    dplyr::filter(
      short_desc == 'PRODUCERS, (ALL) - NUMBER OF PRODUCERS',
      stringr::str_detect(domaincat_desc, '^AREA OPERATED')
    )

  params <- list(
    source_desc = 'CENSUS',
    short_desc = farm_params$short_desc,
    domain_desc = 'AREA OPERATED',
    agg_level_desc = c('COUNTY', 'STATE'),
    state_fips_code = nass_api_fips$states_only[1:9]
  )

  years <- limit_calls(seq(config$year_start, config$year_end %||% 2022, 5), config$api_limit)

  out <- purrr::map(years, \(yr) {
    cat('\nDownloading year ', yr, ' (', which(years == yr), ' of ', length(years), ')\n\n', sep = '')
    param_set <- params
    param_set[['year']] <- yr
    Sys.sleep(config$api_sleep)
    tryCatch({
      nassqs(param_set)
    }, error = function(e) {
      message('Error')
      return(NULL)
    })
  }) %>%
    purrr::list_rbind()

  return(out)
}


call_api_nass_organic <- function(nass_params, nass_api_fips, config) {
  og_params <- nass_params %>%
    dplyr::filter(short_desc == 'FARM OPERATIONS, ORGANIC - NUMBER OF OPERATIONS')

  params <- list(
    source_desc = 'CENSUS',
    short_desc = og_params$short_desc,
    domain_desc = 'ORGANIC STATUS',
    domaincat_desc = 'ORGANIC STATUS: (NOP USDA CERTIFIED)',
    agg_level_desc = c('COUNTY', 'STATE'),
    state_fips_code = nass_api_fips$states_only[1:9]
  )

  years <- limit_calls(seq(config$year_start, config$year_end %||% 2022, 5), config$api_limit)

  out <- purrr::map(years, \(yr) {
    cat('\nDownloading year ', yr, ' (', which(years == yr), ' of ', length(years), ')\n\n', sep = '')
    param_set <- params
    param_set[['year']] <- yr
    Sys.sleep(config$api_sleep)
    tryCatch({
      nassqs(param_set)
    }, error = function(e) {
      message('Error')
      return(NULL)
    })
  }) %>%
    purrr::list_rbind()

  return(out)
}


call_api_nass_survey <- function(nass_api_fips, config) {
  params <- list(
    source_desc = 'SURVEY',
    domain_desc = 'TOTAL',
    statisticcat_desc = c('YIELD', 'PRODUCTION'),
    agg_level_desc = c('COUNTY', 'STATE'),
    state_fips_code = nass_api_fips$states_only[1:9],
    freq_desc = 'ANNUAL'
  )

  years <- limit_calls(seq(config$year_start, config$year_end %||% 2024, 1), config$api_limit)

  out <- purrr::map(years, \(yr) {
    cat('\nDownloading year ', yr, ' (', which(years == yr), ' of ', length(years), ')\n\n', sep = '')
    param_set <- params
    param_set[['year']] <- yr
    Sys.sleep(config$api_sleep)
    tryCatch({
      nassqs(param_set)
    }, error = function(e) {
      message('Error')
      return(NULL)
    })
  }) %>%
    purrr::list_rbind()

  return(out)
}
