#' SM Call APIs
#'
#' @description Call all APIs used in secondary data collection. Note that this
#' function will take some time to run. API keys are invoked from .Renviron file
#' which is called in the appropriate scripts.
#' @param nass 
#' @param bls 
#' @param census 
#' @param fda 
#' @param usdm 
#' @param cdc 
#' @param all If `TRUE`, run all API scripts.
#'
#' @returns Runs all APIs and outputs are saved to "5_outputs/api_outs/"
#' @keywords internal
sm_call_apis <- function(nass = FALSE,
                         bls = FALSE,
                         census = FALSE,
                         fda = FALSE,
                         usdm = FALSE,
                         cdc = FALSE,
                         all = FALSE) {
  if (all) {
    nass <- TRUE
    bls <- TRUE
    census <- TRUE
    fda <- TRUE
    usdm <- TRUE
  }
  if (nass) source('4_scripts/api_calls/nass_api.R')
  # if (bls) source('4_scripts/api_calls/bls_api.R')
  if (census) source('4_scripts/api_calls/census_api.R')
  # if (fda) source('4_scripts/api_calls/fda_api.R') # Not using this currently
  if (usdm) source('4_scripts/api_calls/usdm_api.R')
  if (cdc) source('4_scripts/api_calls/cdc_api.R')
}
