#' FIPS key
#'
#' A `df` with FIPS keys for all states and all counties in the Northeast
#' 
#' @format ## `fips_key`
#' A data frame with county and state fips information
#' \describe{
#'   \item{fips}{FIPS key. Generally 5 digits: first 2 digits represent state, last 3 digits represent county.
#'   Here, states only have 2 digits (e.g. Vermont is 50), while counties have 5 digits 
#'   (e.g. Chittenden County is 50007).}
#'   \item{county_name}{County name}
#'   \item{state_name}{State name in long format (e.g. Vermont)}
#'   \item{state_code}{If `fips` refers to a state, the 2-character state code is shown here (e.g. VT).}
#' } 
"fips_key"
