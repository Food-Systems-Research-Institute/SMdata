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
# Call load_all() again manually - tar_debug() does not run _targets.R
# tar_debug() wrapper around tar_make().
# tar_visnetwork()
# tar_invalidate()

# Proper Workflow
# Manually call devtools::install()
# Add SMdata to tar_option_set packages below
# Then call tar_make() - automatically runs _targets.R


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

  ## NASS --------------------------------------------------------------------
  ### API ---------------------------------------------------------------------
  tar_target(
    nass_params_path,
    'inputs/nass_api_parameters.csv',
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

  ### Wrangling ---------------------------------------------------------------
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
  ),


  ## Census ------------------------------------------------------------------
  ### API ---------------------------------------------------------------------
  tar_target(
    census_meta_path,
    'inputs/census_meta.csv',
    format = 'file'
  ),
  tar_target(
    census_acs5_out,
    call_api_census_acs5(config),
    packages = c('censusapi', 'purrr', 'dplyr', 'glue', 'stringr')
  ),
  tar_target(
    census_acs1_out,
    call_api_census_acs1(config),
    packages = c('censusapi', 'purrr', 'dplyr', 'glue', 'stringr')
  ),
  tar_target(
    census_voting_out,
    call_api_census_voting(config),
    packages = c('censusapi', 'purrr', 'dplyr', 'glue', 'stringr')
  ),

  ### Wrangling ---------------------------------------------------------------
  tar_target(
    census_combined,
    wrangle_census_combine(census_acs5_out, census_acs1_out, census_voting_out)
  ),
  tar_target(
    census_data,
    process_census(census_combined),
    packages = c('dplyr', 'tidyr', 'stringr', 'zoo')
  ),
  tar_target(
    census_metadata,
    create_census_metadata(census_data, census_meta_path),
    packages = c('dplyr', 'stringr', 'readr')
  ),
  
  ## Combine -----------------------------------------------------------------
  tar_target(
    metrics_parquet,
    create_metrics_parquet(census_data, nass_data),
    packages = c('dplyr', 'arrow'),
    format = 'file'
  ),
  tar_target(
    metadata_parquet,
    create_metadata_parquet(census_data, nass_data),
    packages = c('dplyr', 'arrow'),
    format = 'file'
  )
)
