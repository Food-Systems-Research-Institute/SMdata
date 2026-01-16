#' Aggregate metrics and metadata lists
#'
#' @description Takes a list of metrics dfs and a list of metadata dfs,
#' aggregates them, and returns a list of 1 metric df and 1 metadata df. Also 
#' ensures that value and year columns are coded as character so they are easier
#' to combine at this stage. Later, they should be turned back to numeric.
#'
#' @param metrics List of metrics dfs
#' @param metadata List of metadata dfs
#'
#' @returns A list containing 1 aggregated metrics df and 1 aggregasted metadata
#'   df
#' @import dplyr
#' @importFrom purrr map
#' @keywords internal
aggregate_metrics <- function(metrics = results,
                              metadata = metas) {
  out <- list()

  out$result <- map(metrics, ~ {
    .x %>%
      mutate(
        value = as.character(value), 
        year = as.character(year)
      )
  }) %>% 
    bind_rows()
  
  out$meta <- map(metas, ~ {
    .x %>% 
      mutate(
        year = as.character(year),
        latest_year = as.character(latest_year)
      )
  }) %>%  
    bind_rows()
  
  return(out)
}
