#' Call Census API
#'
#' @description Download US Census data through API using censusapi package.
#'   Note that instead of conveniently batching variables in calls, we are
#'   calling each iteration of region, year, and variables separately.
#'   Otherwise, if we hit a single error because a variable does not exist in a
#'   certain year, we lose the whole call's worth of data.
#' @param state_codes Character vector of 2-digit state codes (e.g. Vermont =
#'   50)
#' @param years Numeric vector of 4-digit years
#' @param vars Character vector of census variables
#' @param census_key Census API key
#' @param region Character string, one of c('county', 'state')
#' @param sleep_time Rest between API calls at state/year level (not variable)
#' @param survey_name Name of census survey to pull from
#'
#' @returns Data frame of results
#' @importFrom purrr map keep reduce
#' @importFrom dplyr mutate full_join bind_rows
#' @importFrom glue glue
#' @keywords internal
call_census_api <- function(state_codes,
                            years,
                            vars,
                            census_key,
                            region = c('state', 'county'),
                            sleep_time = 1,
                            survey_name = 'acs/acs5') {
  if (!requireNamespace("censusapi", quietly = TRUE)) {
    stop("The 'censusapi' package is required to use this function. Please install it with install.packages('censusapi')", call. = FALSE)
  }
  
  map(state_codes, \(state_code) {
    
    cat(glue("\n\nStarting state: {state_code} ({which(state_codes == state_code)} of {length(state_codes)})\n"))
    
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
    map(years, \(yr) {
      cat('\nState:', state_code, '\nStarting year:', yr, '\n')
      Sys.sleep(sleep_time)
      
      # For given state and year, map over all variables and pull data
      vars_out <- map(vars, \(var) {
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
              mutate(year = yr)  
          },
          error = function(e) {
            message(glue("Error for state {state_code}, year {yr}: {e$message}"))
            data.frame()
          }
        )
      }) %>% 
        purrr::keep(~ is.data.frame(.x) && nrow(.x) > 0)
      
      if (length(vars_out) > 1) {
        purrr::reduce(vars_out, full_join)
      } else if (length(vars_out) == 1) {
        vars_out[[1]]
      } else {
        NULL
      }
      
    }) %>% 
      purrr::keep(~ !is.null(.x)) %>% 
      bind_rows()
  }) %>% 
    purrr::keep(~ !is.null(.x)) %>% 
    bind_rows()
}
