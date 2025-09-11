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
#' @import dplyr
#' @import stringr
#' @export
#'
#' @examples
#' data(metrics_example)
#' filter_fips(metrics_example, scope = 'all')
filter_fips <- function(df, 
                        scope = c('all', 'counties', 'new', 'old', 'states', 'us', 'neast'),
                        fips_col = 'fips') {
  # Match to one of arguments if it is a short version
  scope <- match.arg(scope)
  
  # Filter to set of fips numbers based on scope
  if (scope == 'all') {
    out <- df %>% 
      dplyr::filter(.data[[fips_col]] %in% fips_key$fips)
    
  } else if (scope == 'neast') {
    subset <- fips_key %>% 
      dplyr::filter(str_length(fips) == 5 | (!is.na(state_code) & state_code != 'US')) %>% 
      pull(fips)
    out <- df %>% 
      dplyr::filter(.data[[fips_col]] %in% subset) 
    
  } else if (scope == 'counties') {
    subset <- fips_key %>% 
      dplyr::filter(str_length(fips) == 5) %>% 
      pull(fips)
    out <- df %>% 
      dplyr::filter(.data[[fips_col]] %in% subset)
    
  } else if (scope == 'new') {
    subset <- fips_key %>% 
      dplyr::filter(
        str_length(fips) == 5,
        !str_detect(fips, '^09.*[1-9]$')
      ) %>% 
      pull(fips)
    out <- df %>% 
      dplyr::filter(.data[[fips_col]] %in% subset)
    
  } else if (scope == 'old') {
    subset <- fips_key %>% 
      dplyr::filter(
        str_length(fips) == 5,
        !str_detect(fips, '^09.*0$')
      ) %>% 
      pull(fips)
    out <- df %>% 
      dplyr::filter(.data[[fips_col]] %in% subset)
    
  } else if (scope == 'states') {
    subset <- fips_key %>% 
      dplyr::filter(
        str_length(fips) == 2,
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

