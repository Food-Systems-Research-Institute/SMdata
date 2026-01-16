#' Check metric and metadata variable count
#'
#' @description Compares a long-format metrics df and wide-format metadata df to
#'   make sure they contain the same number of variables and that the variables
#'   have the same names. Throws an error if they do not match. Use at the end
#'   of each wrangling script, right after using `aggregate_metrics()`, and also
#'   in the aggregation script before exporting.
#'
#' @param metric_vars A metrics df
#' @param meta_vars A metadata df
#' @param section Identifier for the script to call out in prints and errors
#' @param var_col Name of column that contains variable names
#'
#' @returns If pass, prints results to screen. If fail, throw error and identify
#'   the section.
#' @keywords internal 
check_n_records <- function(metric_vars, 
                            meta_vars, 
                            section = 'Section',
                            var_col = 'variable_name') {
  
  # Get clean vectors of each, sorted to compare
  metrics <- sort(unique(metric_vars[[var_col]]))
  
  # If NAICS variables are included, remove the NAICS and numeric code
  # Otherwise there will be heaps of "different variables" and won't match meta
  if (any(stringr::str_detect(metrics, 'NAICS'))) {
    metrics <- stringr::str_remove_all(metrics, 'NAICS[0-9]*.*') %>% 
      unique()
  }
  
  meta <- sort(unique(meta_vars[[var_col]]))
  
  # Check if they are all the same
  if (all(metrics == meta) & length(metrics) == length(meta)) {
    cat('\n', section, ' variable check: PASS\n',
        'Number of metrics: ', length(metrics), '\n',
        'Number of metas: ', length(meta), '\n\n', 
        sep = '')
  } else {
    stop('\n', section, ' variable check: FAIL\n',
         'Number of metrics: ', length(metrics), '\n',
         'Number of metas: ', length(meta), '\n\n')
  }
}
