#' SM Wrangle
#'
#' @description
#' @param spatial If `TRUE`, run the spatial wrangling script, which will take
#'   at least 15 minutes or so to run.
#'
#' @returns Runs all data wrangling scripts. Each one saves metric outputs to
#'   "5_objects/results" and metadata to "5_objects/metadata".
#' @keywords internal
sm_wrangle <- function(spatial = FALSE) {
  source('4_scripts/nass.R')
  source('4_scripts/census.R')
  source('4_scripts/bls_ers.R')
  source('4_scripts/county_health_rankings.R')
  source('4_scripts/map_meal_gap.R')
  source('4_scripts/other_datasets.R')
  if (spatial) source('4_scripts/lulc.R')
}