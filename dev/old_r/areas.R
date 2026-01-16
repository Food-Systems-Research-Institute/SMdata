#' Land and Water Area by FIPS
#'
#' Land and water area in sq km for all US counties and states. Total area is 
#' the sum of `area_sqkm` and `water_sqkm`.
#' 
#' @format ## `areas`
#' A data frame in long format
#' \describe{
#'   \item{fips}{
#'   Federal Information Processing Standards code, a 5-digit code identifying
#'   the location. The first two digits are state, last three are county. Here, 
#'   state data is given only 2 digits (e.g. Vermont is `50`), while counties
#'   are given all 5 (e.g. Chittenden county is `50007`).
#'   }
#'   \item{area_sqkm}{Area of land in county or state in sq km}
#'   \item{water_sqkm}{Area of water in county or state in sq km}
#' }
"areas"
