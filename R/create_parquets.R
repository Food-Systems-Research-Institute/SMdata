#' Create Metrics Parquet
#' 
#' Combined metrics data into parquet file
#' @param census_data 
#' @param nass_data 
#'
#' @returns
#' @export
#'
#' @examples
create_metrics_parquet <- function(census_data, nass_data) {
  census_data <- mutate(census_data, year = as.numeric(year))
  nass_data <- mutate(nass_data, year = as.numeric(year))
  
  metrics_data <- dplyr::bind_rows(census_data, nass_data)
  
  path <- 'outputs/metrics.parquet'
  arrow::write_parquet(metrics_data, path)
  
  return(path)
}

#' Create Metadata Parquet
#' 
#' Combined metadata into parquet file
#' @param census_data 
#' @param nass_data 
#'
#' @returns
#' @export
#'
#' @examples
create_metadata_parquet <- function(census_metadata, nass_metadata) {
  census_metadata <- mutate(census_metadata, year = as.numeric(year))
  nass_metadata <- mutate(nass_metadata, year = as.numeric(year))
  
  metrics_metadata <- dplyr::bind_rows(census_metadata, nass_metadata)
  
  path <- 'outputs/metadata.parquet'
  arrow::write_parquet(metrics_metadata, path)
  
  return(path)
}
