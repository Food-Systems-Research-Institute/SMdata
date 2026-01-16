# Sandbox


# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  purrr
)

# Clean, aggregated data
dat <- readRDS('2_clean/metrics.rds')
meta <- readRDS('2_clean/metadata.rds')



# Check employee numbers --------------------------------------------------


get_str(meta)
meta$variable_name



# -------------------------------------------------------------------------


get_str(metrics)

metrics %>% 
  filter(
    str_detect(
      variable_name, 
      regex('naics|^lq|^avgEmpLevel', ignore_case = TRUE), 
      negate = TRUE
    )
  ) %>% 
  pull(variable_name) %>% 
  unique() %>% 
  sort()

dat <- metrics %>% 
  filter(str_detect(variable_name, '^oty'))
get_str(dat)


