# Manual ------------------------------------------------------------------


# Packages to define pipeline. Run these first
library(targets)
devtools::load_all() # for development

# Set target options (applies to each target environment, add SMdata here)
tar_option_set(
  packages = c("tibble", "dplyr")
)

# Development Workflow
set_test_mode(TRUE)
setup_logger()
# devtools::load_all() in _targets.R - preserves environment (no callr)
# tar_debug() wrapper around tar_make()
# tar_visnetwork()
# tar_invalidate()

# Proper Workflow
# Manually call devtools::install()
# Add SMdata to tar_option_set packages below


# Debug -------------------------------------------------------------------

# tar_option_set(
#   packages = c("tibble", "dplyr"),
#   # debug = "nass_farm_out",
#   cue = tar_cue(mode = "always") # always run every target
# )


# Pipeline ----------------------------------------------------------------

list(
  ## Config ------------------------------------------------------------------
  tar_target(
    config,
    list(
      year_start = 2002,
      year_end = NULL,
      api_sleep = 0.5,
      api_limit = 5
    )
  ),

  ## NASS API ----------------------------------------------------------------
  tar_target(
    nass_params_path,
    '5_objects/api_parameters/nass_api_parameters.csv',
    format = 'file'
  ),
  tar_target(
    nass_params,
    load_nass_params(path = nass_params_path),
    packages = 'readr'
  ),
  tar_target(
    nass_api_fips,
    api_nass_census_setup('nass_api_key'),
    packages = c('rnassqs', 'purrr', 'dplyr')
  ),
  tar_target(
    nass_census_out,
    call_api_nass_census(nass_params, nass_api_fips, config),
    packages = c('rnassqs', 'purrr', 'dplyr')
  ),
  tar_target(
    nass_farm_out,
    call_api_nass_farm(nass_params, nass_api_fips, config),
    packages = c('rnassqs', 'purrr', 'dplyr', 'stringr')
  ),
  tar_target(
    nass_organic_out,
    call_api_nass_organic(nass_params, nass_api_fips, config),
    packages = c('rnassqs', 'purrr', 'dplyr')
  ),
  tar_target(
    nass_survey_out,
    call_api_nass_survey(nass_api_fips, config),
    packages = c('rnassqs', 'purrr')
  ),

  ## NASS Wrangling ----------------------------------------------------------
  tar_target(
    nass_combined,
    wrangle_nass_combine(nass_census_out, nass_survey_out, nass_farm_out, nass_organic_out)
  ),
  tar_target(
    nass_data,
    process_nass(nass_combined, nass_params),
    packages = c(
      'dplyr',
      'tidyr',
      'stringr',
      'snakecase',
      'rlang',
      'vegan',
      'purrr',
      'e1071',
      'zoo'
    )
  ),
  tar_target(
    nass_metadata,
    create_nass_metadata(nass_data, nass_params),
    packages = c('dplyr', 'stringr')
  )
)
