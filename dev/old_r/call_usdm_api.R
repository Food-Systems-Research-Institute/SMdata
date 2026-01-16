#' Call US Drought Monitor API
#' 
#' @description Call the US Drought Monitor API. Have to add years manually, map
#'   over them, and set names. Info at
#'   <https://droughtmonitor.unl.edu/DmData/DataDownload/WebServiceInfo.aspx#comp>.
#' @param base Base URL as a string, up to but not including the county or state
#'   specification.
#' @param drought_categories Vector of integers specifying which drought
#'   severity categories to collect (0 to 4).
#' @param states Vector of 2-letter character strings specifying states (e.g.,
#'   VT)
#' @param start_date Start date as string in MM/DD/YYYY format, as is accepted
#'   by the API.
#' @param end_date End date as string in MM/DD/YYYY format, as is accepted by
#'   the API.
#'
#' @returns Loops over states and drought categories to collect list of data,
#'   nested by state and category
#' @import glue
#' @import httr2
#' @import purrr
#' @import dplyr
#' @keywords internal
call_usdm_api <- function(base,
                          drought_categories,
                          states,
                          start_date,
                          end_date) {
  
  out <- map(states, \(state) {
    state_out <- map(drought_categories, \(cat) {
      
      # Build query based on parameters
      url <- glue(
        base,
        'GetNonConsecutiveStatisticsCounty?',
        'aoi={state}',
        '&dx={cat}',
        '&startdate={start_date}',
        '&enddate={end_date}'
      )
      print(url)
      
      # Get all categories for given state
      cat_out <- GET(url, add_headers(Accept = "text/json")) %>%
        content(as = 'text') %>%
        fromJSON()
      
      if (length(cat_out) > 0) {
        cat_out <- cat_out %>% 
          as.data.frame() %>% 
          select(fips, nonConsecutiveWeeks)
      }
      
      return(cat_out)
    }) %>% 
      setNames(c(paste0('cat_', drought_categories)))
  }) %>% 
    setNames(c(states))
  return(out)
}