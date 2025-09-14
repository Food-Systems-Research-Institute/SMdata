#' Filter metrics df by FIPS code
#'
#' @description
#' Conveniently filter a long-format metrics df by FIPS code.
#' 
#' @param df A long-format metrics df.
#' @param scope Method by which FIPS codes are filtered. `all` = all states
#'   nationally and any county in Northeast (9 + 226 = 235). `counties` = any county in
#'   Northeast. This includes Connecticut's old county system and new governance
#'   region system (226). `new` = all counties in Northeast, but only
#'   Connecticut's new governance region system (218). `old` = all counties in
#'   Northeast, but only Connecticut's old county system (217). `neast` =
#'   Northeast states and Counties. `states` = Northeast states only.
#' @param fips_col column specifying fips code.
#'
#' @returns A data.frame with filters applied.
#' @importFrom dplyr select filter
#' @importFrom stringr str_length str_detect
#' @importFrom assertthat assert_that
#' @export
#'
#' @examples
#' data(metrics_example)
#' filter_fips(metrics_example, scope = 'all')
filter_fips <- function(df, 
                        scope = c('all', 'counties', 'new', 'old', 'states', 'us', 'neast'),
                        fips_col = 'fips') {
  assertthat::assert_that(
    'data.frame' %in% class(df),
    msg = paste('df must be a data.frame object, not a', class(df))
  )
  assertthat::assert_that(
    !is.null(scope) & length(scope) == 1,
    msg = paste('scope must be a character string of length 1 and must not be NULL')
  )
  assertthat::assert_that(
    scope %in% c('all', 'counties', 'new', 'old', 'states', 'us', 'neast'),
    msg = paste('scope argument must be one of: all, counties, new, old, states, us, neast')
  )
  assertthat::assert_that(
    fips_col %in% names(df),
    msg = paste0('fips_col ("', fips_col, '") must be a column in df')
  )
  
  # Match to one of arguments if it is a short version
  scope <- match.arg(scope)
  
  # Filter to set of fips numbers based on scope
  if (scope == 'all') {
    out <- df %>% 
      dplyr::filter(.data[[fips_col]] %in% fips_key$fips)
    
  } else if (scope == 'neast') {
    subset <- fips_key %>% 
      dplyr::filter(stringr::str_length(fips) == 5 | (!is.na(state_code) & state_code != 'US')) %>% 
      dplyr::pull(fips)
    out <- df %>% 
      dplyr::filter(.data[[fips_col]] %in% subset) 
    
  } else if (scope == 'counties') {
    subset <- fips_key %>% 
      dplyr::filter(str_length(fips) == 5) %>% 
      dplyr::pull(fips)
    out <- df %>% 
      dplyr::filter(.data[[fips_col]] %in% subset)
    
  } else if (scope == 'new') {
    subset <- fips_key %>% 
      dplyr::filter(
        stringr::str_length(fips) == 5,
        !stringr::str_detect(fips, '^09.*[1-9]$')
      ) %>% 
      pull(fips)
    out <- df %>% 
      dplyr::filter(.data[[fips_col]] %in% subset)
    
  } else if (scope == 'old') {
    subset <- fips_key %>% 
      dplyr::filter(
        stringr::str_length(fips) == 5,
        !stringr::str_detect(fips, '^09.*0$')
      ) %>% 
      pull(fips)
    out <- df %>% 
      dplyr::filter(.data[[fips_col]] %in% subset)
    
  } else if (scope == 'states') {
    subset <- fips_key %>% 
      dplyr::filter(
        stringr::str_length(fips) == 2,
        is.na(county_name),
        state_name != 'US'
      ) %>% 
      pull(fips)
    out <- df %>% 
      dplyr::filter(.data[[fips_col]] %in% subset)
    
  } else if (scope == 'us') {
    out <- df %>% 
      dplyr::filter(.data[[fips_col]] == '00')
    
  } else {
    stop('Could not filter fips.')
  }
  
  return(out)  
}

