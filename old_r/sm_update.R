#' Update exported datasets
#'
#' Internal function that updates the .rda files exported by the package. This
#' should come after using the `sm_export()` function which updates the .rds
#' files in the outputs folder. Updates only the essentials from the spatial 
#' data, including counties, states, and the mask for the Northeast region.
#' @param export If TRUE, run `sm_export()` first.
#' @importFrom purrr walk2
#' @keywords internal
sm_update <- function(export = FALSE) {
  if (export) sm_export()
  sm_data <- readRDS('6_outputs/sm_data.rds')
  walk2(sm_data, names(sm_data), function(obj, name) {
    assign(name, obj)
    do.call("use_data", list(as.name(name), overwrite = TRUE))
  })
  sm_spatial <- readRDS('6_outputs/sm_spatial.rds')
  vars <- c(
    'all_states',
    'neast_states',
    'neast_counties_2021',
    'neast_counties_2024',
    'neast_mask'
  )
  walk(vars, ~ {
    assign(.x, sm_spatial[[.x]])
    do.call("use_data", list(as.name(.x), overwrite = TRUE))
  })
cat('\n*Update complete*\n')
}
