#' Update SM Database
#' 
#' @description Uses FSRC admin login to update MySQL database hosted on WebDB
#' @keywords internal
sm_db <- function() {
  readRenviron('~/.Renviron')
  # con <- dbConnect(
  #   MySQL(),
  #   user = Sys.getenv('fsrc_admin'),
  #   password = Sys.getenv('fsrc_admin_pass'),
  #   host = Sys.getenv('webdb_host'),
  #   dbname = 'FSRC_METRICS'
  # )
  
  user = Sys.getenv('fsrc_admin')
  password = Sys.getenv('fsrc_admin_pass')
  host = Sys.getenv('webdb_host')
  dbname = 'FSRC_METRICS'
  
  cmd <- glue(
    'mysql -u {user} -h {host} -p {password}'
  )
  
}
