#' Sustainability Metrics Data
#'
#' All data for 50 states and 68 New England counties.
#' 
#' @format ## `metrics`
#' A data frame in long format with one value per row. 
#' \describe{
#'   \item{fips}{
#'   Federal Information Processing Standards code, a 5-digit code identifying
#'   the location. The first two digits are state, last three are county. Here, 
#'   state data is given only 2 digits (e.g. Vermont is `50`), while counties
#'   are given all 5 (e.g. Chittenden county is `50007`).
#'   }
#'   \item{year}{Year in `YYYY` format.}
#'   \item{variable_name}{
#'   String used to identify metric and link it to metadata. Meant for internal
#'   use. Human-friendly metric names are found in the metdata.
#'   }
#'   \item{value}{Value of metric. See metadata for units and interpretation.}
#' }
"metrics"
