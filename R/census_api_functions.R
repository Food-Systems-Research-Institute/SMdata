#' Call Census API
#'
#' @description Download US Census data through API using censusapi package.
#'   Note that instead of conveniently batching variables in calls, we are
#'   calling each iteration of region, year, and variables separately.
#'   Otherwise, if we hit a single error because a variable does not exist in a
#'   certain year, we lose the whole call's worth of data.
#' @param state_codes Character vector of 2-digit state codes (e.g. Vermont = 50)
#' @param years Numeric vector of 4-digit years
#' @param vars Character vector of census variables
#' @param census_key Census API key
#' @param region Character string, one of c('county', 'state')
#' @param sleep_time Rest between API calls at state/year level (not variable)
#' @param survey_name Name of census survey to pull from
#'
#' @returns Data frame of results
#' @keywords internal
call_census_api <- function(state_codes,
                            years,
                            vars,
                            census_key,
                            region = c('state', 'county'),
                            sleep_time = 1,
                            survey_name = 'acs/acs5') {
  
  purrr::map(state_codes, \(state_code) {

    logger::log_info("Starting state: {state_code} ({which(state_codes == state_code)} of {length(state_codes)})")

    # Set region based on state or county data
    if (region == 'state') {
      region_var <- paste0('state:', state_code)
      regionin_var <- NULL
    }
    else if (region == 'county') {
      region_var <- "county:*"
      regionin_var <- paste0("state:", state_code)
    }

    # For given state, map over all years
    purrr::map(years, \(yr) {
      logger::log_info("State: {state_code} | Year: {yr}")
      Sys.sleep(sleep_time)

      # For given state and year, map over all variables and pull data
      vars_out <- purrr::map(vars, \(var) {
        tryCatch(
          {
            censusapi::getCensus(
              name = survey_name,
              vintage = yr,
              key = census_key,
              vars = var,
              region = region_var,
              regionin = regionin_var
            ) %>%
              dplyr::mutate(year = yr)
          },
          error = function(e) {
            message(glue::glue("Error for state {state_code}, year {yr}: {e$message}"))
            data.frame()
          }
        )
      }) %>%
        purrr::keep(~ is.data.frame(.x) && nrow(.x) > 0)

      if (length(vars_out) > 1) {
        purrr::reduce(vars_out, dplyr::full_join)
      } else if (length(vars_out) == 1) {
        vars_out[[1]]
      } else {
        NULL
      }

    }) %>%
      purrr::keep(~ !is.null(.x)) %>%
      dplyr::bind_rows()
  }) %>%
    purrr::keep(~ !is.null(.x)) %>%
    dplyr::bind_rows()
}


#' Get ACS5 Variable Codes
#'
#' Returns named list of ACS 5-year variable codes for Census API
#' @returns Named list of variable codes
#' @keywords internal
get_census_acs5_vars <- function() {
  list(
    # Population
    'population5Year' = 'B01003_001E',

    # Education
    'edTotal' = 'B15003_001E',
    'edTotalHS' = 'B15003_017E',
    'edTotalGED' = 'B15003_018E',
    'edTotalBS' = 'B15003_022E',
    'edTotalPhD' = "B15003_025E",
    'edTotalProf' = "B15003_024E",
    'edTotalMaster' = "B15003_023E",
    'edTotalAssoc' = "B15003_021E",
    'edTotalSomeCollege' = "B15003_020E",
    'edTotalSomeCollegeLessThanYear' = "B15003_019E",

    # Housing
    'nHousingUnits' = 'B25001_001E',
    'nHousingOccupied' = 'B25002_002E',
    'nHousingVacant' = 'B25002_003E',

    # Rent by bedrooms
    'rentMedian1BR' = 'B25031_003E',
    'rentMedian4BR' = 'B25031_006E',

    # Housing age
    'medianHousingYear' = 'B25035_001E',

    # More rent
    'rentMedian' = 'B25064_001E',
    'rentMedianPercHH' = 'B25071_001E',

    # Wages in FFF
    'medianFemaleEarningsFFF' = 'B24022_067E',
    'medianMaleEarningsFFF' = 'B24022_031E',
    'medianFemaleEarningsFPS' = 'B24022_060E',
    'medianMaleEarningsFPS' = 'B24022_024E',

    # Gini coefficient
    'gini' = 'B19083_001E',

    # Disconnected youth
    'n16to19' = 'B14005_001E',
    'nMaleNotEnrolledHSGradNotInLaborForce' = 'B14005_011E',
    'nMaleNotEnrollNoGradNotInLaborForce' = 'B14005_015E',
    'nFemaleNotEnrollHSGradNotInLaborForce' = 'B14005_025E',
    'nFemaleNotEnrollNoGradNotInLaborForce' = 'B14005_029E'
  )
}


#' Call Census ACS5 API
#'
#' Download ACS 5-year estimates for state and county levels
#' @param config Pipeline configuration list
#' @returns Data frame with ACS5 variables
#' @keywords internal
call_api_census_acs5 <- function(config) {
  census_key <- get_env_var('CENSUS_API_KEY')

  # Limit state codes during development
  state_codes <- fips_key$fips[stringr::str_length(fips_key$fips) == 2 & fips_key$fips != '00']
  state_codes <- limit_calls(state_codes, config$api_limit)

  # Limit years during development
  years <- seq(2008, config$year_end %||% 2023, 5)
  years <- limit_calls(years, config$api_limit)

  # Limit variables during development
  vars <- get_census_acs5_vars()
  vars <- limit_calls(vars, config$api_limit)

  log_step("call_api_census_acs5", "Starting ACS5 API calls")

  # State data
  states_out <- call_census_api(
    state_codes = state_codes,
    years = years,
    vars = vars,
    census_key = census_key,
    region = 'state',
    sleep_time = config$api_sleep
  )

  # County data
  county_out <- call_census_api(
    state_codes = state_codes,
    years = years,
    vars = vars,
    census_key = census_key,
    region = 'county',
    sleep_time = config$api_sleep
  )

  # Combine and rename variables
  out <- dplyr::bind_rows(states_out, county_out)

  if (nrow(out) == 0) {
    logger::log_warn("ACS5 API returned no data")
    return(out)
  }

  # Rename API codes to readable names
  vars_in_data <- names(vars)[names(vars) %in% names(out) | unlist(vars) %in% names(out)]
  vars_to_rename <- vars[vars_in_data]
  out <- dplyr::rename(out, !!!setNames(vars_to_rename, names(vars_to_rename)))

  log_step("call_api_census_acs5", "Complete", out)
  out
}


#' Call Census ACS1 API
#'
#' Download ACS 1-year population estimates for state and county levels
#' @param config Pipeline configuration list
#' @returns Data frame with annual population
#' @keywords internal
call_api_census_acs1 <- function(config) {
  census_key <- get_env_var('CENSUS_API_KEY')

  # Limit state codes during development
  state_codes <- fips_key$fips[stringr::str_length(fips_key$fips) == 2 & fips_key$fips != '00']
  state_codes <- limit_calls(state_codes, config$api_limit)

  # ACS1 started in 2005, not 2000
  years <- seq(2005, config$year_end %||% 2024, 1)
  years <- limit_calls(years, config$api_limit)

  vars <- 'B01003_001E'

  log_step("call_api_census_acs1", "Starting ACS1 API calls ({length(state_codes)} states, {length(years)} years)")

  # State data
  states_out <- call_census_api(
    state_codes = state_codes,
    years = years,
    vars = vars,
    census_key = census_key,
    region = 'state',
    survey_name = 'acs/acs1',
    sleep_time = config$api_sleep
  )

  # County data
  county_out <- call_census_api(
    state_codes = state_codes,
    years = years,
    vars = vars,
    census_key = census_key,
    region = 'county',
    survey_name = 'acs/acs1',
    sleep_time = config$api_sleep
  )

  # Combine and rename
  out <- dplyr::bind_rows(states_out, county_out)

  if (nrow(out) == 0) {
    logger::log_warn("ACS1 API returned no data")
    return(out)
  }

  if ('B01003_001E' %in% names(out)) {
    out <- dplyr::rename(out, populationAnnual = B01003_001E)
  }

  log_step("call_api_census_acs1", "Complete", out)
  out
}


#' Call Census Voting API
#'
#' Download CPS voting data for states
#' @param config Pipeline configuration list
#' @returns Data frame with voter turnout by state and year
#' @keywords internal
call_api_census_voting <- function(config) {
  census_key <- get_env_var('CENSUS_API_KEY')

  # Limit state codes during development
  state_codes <- fips_key$fips[stringr::str_length(fips_key$fips) == 2 & fips_key$fips != '00']
  state_codes <- limit_calls(state_codes, config$api_limit)

  # Limit years during development
  years <- seq(2000, config$year_end %||% 2024, 2)
  years <- limit_calls(years, config$api_limit)

  vars <- 'PES1'

  log_step("call_api_census_voting", "Starting voting API calls")

  # PES1 codes: 1 yes, 2 no, -1 not in universe, -2 dont know, -3 refused
  # Filter to 1s and 2s only, make 'no' into 0, then get mean for voting rate
  out <- purrr::map(years, \(yr) {
    call_census_api(
      state_codes = state_codes,
      years = yr,
      vars = vars,
      census_key = census_key,
      region = 'state',
      survey_name = 'cps/voting/nov',
      sleep_time = config$api_sleep
    ) %>%
      dplyr::filter(PES1 %in% c(1, 2)) %>%
      dplyr::mutate(PES1 = ifelse(PES1 == 2, 0, PES1)) %>%
      dplyr::group_by(state) %>%
      dplyr::summarize(voterTurnout = mean(PES1, na.rm = TRUE)) %>%
      dplyr::mutate(year = yr)
  }) %>%
    dplyr::bind_rows()

  log_step("call_api_census_voting", "Complete", out)
  out
}
