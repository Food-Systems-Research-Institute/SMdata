#' Explore
#' 2024-09-20


# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  purrr,
  readr,
  stringr,
  sf,
  tidyr
)

check_var <- function(str, col = 'variable_name') {
  out <- map(list(metadata, metrics), ~ {
    .x[[col]] %>% 
      str_subset(regex(paste0('^', str, '$'), ignore_case = TRUE)) %>% 
      unique() %>% 
      sort()
  })
  out[[3]] <- metrics %>% 
    filter(variable_name == str) %>% 
    pull(year) %>% 
    unique() %>% 
    sort() %>% 
    paste0(collapse = ', ')
  cat('\nMeta:', paste0(out[[1]], collapse = ', '), 
      '\n\nMetrics:', paste0(out[[2]], collapse = ', '),
      '\n\nMetric Years:', paste0(out[[3]], collapse = ', '))
}

check_meta <- function(str, col = 'variable_name') {
  metadata %>% 
    filter(str_detect(variable_name, regex(str, ignore_case = TRUE))) %>% 
    select(variable_name, metric, definition, source, url, year)
}



# Checking ----------------------------------------------------------------


check_meta('expHiredLaborPercOpExp')
check_var('expHiredLaborPercOpExp')

metrics %>% 
  filter(
    variable_name == 'expHiredLaborPercOpExp'
  ) %>% 
  group_by(year) %>% 
  summarize(count = sum(!is.na(value)))
  
metrics %>% 
  # filter_fips('counties') %>% 
  filter(variable_name == 'expHiredLaborPercOpExp')



# -------------------------------------------------------------------------


updated_vars <- c(
  # 'hayYieldMeasuredInTonsAcre',
  # 'yieldMilk',
  # 'mapleSyrupYieldMeasuredInGallonsTap',
  'receiptsAllForestProducts'
)
walk(updated_vars, ~ {
  ran <- metrics %>% 
    filter(variable_name == .x) %>% 
    pull(year) %>% 
    range()
  print(cat(
    '\n', .x, ':', ran[1], ran[2]
  ))
})


df <- readRDS('5_objects/metrics/bls_ers.RDS')
get_str(df)
df <- df %>% 
  filter(variable_name == 'receiptsAllForestProducts')
get_str(df)
df$year %>% 
  range()
df$year
