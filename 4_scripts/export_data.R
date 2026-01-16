# Export Data
# 2025-09-11


# Description -------------------------------------------------------------


# Combine data from all sources into a single metrics file, along with a 
# metadata file. Save as zipped csvs for easy transport to other projects and
# languages. 

# Also saving a sm_data.rds and sm_spatial.rds file which make for easy transfer
# to R specific projects.



# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  purrr,
  readr,
  sf,
  stars,
  stringr,
  arrow
)

conflicted::conflicts_prefer(
  testthat::matches(),
  .quiet = TRUE
)


# Aggregate ---------------------------------------------------------------
## Metrics -----------------------------------------------------------------


# Load datasets
metrics <- read_all_rds('5_objects/metrics/')  
# get_str(metrics)

# Keep all the weird variables, cv percent, disclosure, value codes, margins
# Just make sure year is numeric
metrics_agg <- map(metrics, ~ {
  suppressWarnings(
    .x %>% 
      mutate(across(c(year, value), as.numeric)) %>% 
      select(-any_of(c('metric', 'county_name')))
  )
}) %>% 
  bind_rows() %>% 
  select(fips, year, variable_name, value)
get_str(metrics_agg)



## Metadata ---------------------------------------------------------------


# Load metadata
meta <- read_all_rds('5_objects/metadata/')

# Combine them, warehouse is FALSE if not included
meta_agg <- bind_rows(meta) %>% 
  select(-any_of('warehouse'))

# If there is no axis name, make it the variable_name
meta_agg <- meta_agg %>% 
  mutate(axis_name = ifelse(is.na(axis_name), variable_name, axis_name))



## Check -------------------------------------------------------------------


# Check to make sure we have the same number of metrics and metas
# try(check_n_records(metrics_agg, meta_agg, 'Aggregation'))



# Save CSV and Parquet ----------------------------------------------------


# Save metrics and metadata as a zipped csv file for easy transport
paths <- c('6_outputs/metrics.csv', '6_outputs/metadata.csv', '6_outputs/fips_key.csv')
write_csv(metrics_agg, paths[1])
write_csv(meta_agg, paths[2])
write_csv(fips_key, paths[3])

# Zip csv files together, save into SMdata and SMdocs, remove original csvs
zip_paths <- c('6_outputs/metrics_and_metadata.zip', '../SMdocs/data/metrics_and_metadata.zip')
walk(zip_paths, ~ {
  zip(
    zipfile = .x, 
    files = c(paths)
  )
})
file.remove(paths)

# Also save a parquet file
pq_paths <- c(
  '6_outputs/metrics.parquet'
  # '../SMdocs/data/metrics.parquet'
)
walk(pq_paths, ~ write_parquet(metrics_agg, .x))



# Export sm_data ----------------------------------------------------------


# Throwing together a heap of utility objects into a single object, sm_data.
# Includes metrics, metadata, fips keys, state keys, etc.

# Initiate list
sm_data <- list()

# Metrics and metadata
sm_data$metrics <- metrics_agg
sm_data$metadata <- meta_agg

# Crosswalk for weighting variable names and metrics
sm_data$weighting <- readRDS('5_objects/weighting_vars.rds')

# Add county land and water area as utility dataset
sm_data$areas <- readRDS('5_objects/areas.rds')

# Spatial objects and references
sm_data$fips <- read_all_rds(path = '5_objects/', pattern = '_key.rds$|^all_fips')

# Add trees for various iterations of framework. Would do well do remove 
# unecessary versions at some point...
# Also giving placeholders a unique value
tree <- read.csv('2_clean/trees/refined_secondary_tree.csv')
count <- sum(tree$metric == 'NONE')
tree$metric[tree$metric == 'NONE'] <- paste0('NONE_', 1:count)
sm_data$refined_tree <- tree

# Add fixed refined tree with all the indicators from workshops
fixed_tree <- read.csv('2_clean/trees/fixed_tree.csv')
count <- sum(fixed_tree$metric == 'NONE')
fixed_tree$metric[fixed_tree$metric == 'NONE'] <- paste0('NONE_', 1:count)
sm_data$fixed_tree <- fixed_tree

# Also add new tree (reducing metrics for RFPP and data inventory paper)
new_tree <- read.csv('2_clean/trees/new_tree.csv')
count <- sum(new_tree$metric == 'NONE')
new_tree$metric[new_tree$metric == 'NONE'] <- paste0('NONE_', 1:count)
sm_data$new_tree <- new_tree

# And conference tree...
conference_tree <- read.csv('2_clean/trees/conference_tree.csv')
count <- sum(conference_tree$metric == 'NONE')
conference_tree$metric[conference_tree$metric == 'NONE'] <- paste0('NONE_', 1:count)
sm_data$conf_tree <- conference_tree


# Flatten into single layer list
sm_data <- list_flatten(sm_data, name_spec = "{inner}")

# Paths to SMdocs and SMexplorer. Also keeping a copy in outputs
sm_data_paths <- c(
  '../SMexplorer/dev/data/sm_data.rds',
  '6_outputs/sm_data.rds'
)
walk(sm_data_paths, ~ try(saveRDS(sm_data, .x)))



# Export sm_spatial -------------------------------------------------------


# Another RDS object for spatial files to be used in other projects. Note that
# to hand off to python, we will need to add rasters and geopackages separately.

# Start empty list
spatial <- list()

# Map layers - this is where rasters are transported if they are to be mapped
# in Quarto
spatial$spatial <- read_all_rds('2_clean/spatial/map_layers/', pattern = '.rds$')

# County and state layers
# (?i) is case insensitive
spatial$counties_and_states <- read_all_rds(
  '2_clean/spatial/',
  pattern = '(?i)^neast_.*.rds|^all_states.*.rds'
)

# Flatten into single level list
spatial <- list_flatten(spatial, name_spec = "{inner}")

# Save spatial data in those places also
spatial_paths <- c(
  # '../SMdocs/data/sm_spatial.rds',
  '../SMexplorer/dev/data/sm_spatial.rds',
  '6_outputs/sm_spatial.rds'
)
walk(spatial_paths, ~ try(saveRDS(spatial, .x)))



# End ---------------------------------------------------------------------


clear_data(gc = TRUE)
cat('\n*Export complete*')
