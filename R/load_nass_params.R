#' Title
#'
#' @param path 
#'
#' @returns
#' @export
#'
#' @examples
load_nass_params <- function(path) {
  broken <- c(
    'INCOME, FARM-RELATED, FOREST PRODUCTS, (EXCL CHRISTMAS TREES & SHORT TERM WOODY CROPS & MAPLE SYRUP) - RECEIPTS, MEASURED IN $',
    'FARM OPERATIONS, ORGANIC - NUMBER OF OPERATIONS',
    'PRACTICES, ALLEY CROPPING & SILVAPASTURE - NUMBER OF OPERATIONS'
  )
  readr::read_csv(path) %>% 
    filter(!short_desc %in% broken)
}
