# Update Excel
# 2025-07-11 update


# Description -------------------------------------------------------------


# Pulling working copy of metrics from excel on OneDrive, adding information
# about the metrics we actually have in the project, and saving a metrics 
# summary back to the same folder that shows what variables we have, what we
# are missing, and how many states, counties, and years each one represents. 
# Use this to track how metric collection is coming along and get a sense of 
# missing data.



# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  purrr,
  stringr,
  readxl,
  openxlsx2,
  tidyr,
  readr
)

conflicted::conflicts_prefer(
  testthat::matches(),
  .quiet = TRUE
)



# Pull Excel --------------------------------------------------------------


# Copy it locally. Using revised metrics sheet (liberties taken with framework)
# path <- 'C:/Users/cdonov12/OneDrive - University of Vermont/Food Systems Research Center/Sustainability Metrics/Sustainability Metrics Manuscript/Metrics/secondary_metrics.xlsx'
path <- 'C:/Users/cdonov12/OneDrive - University of Vermont/Food Systems Research Center/Sustainability Metrics/Sustainability Metrics Manuscript/Metrics/secondary_metrics_revised.xlsx'
new_xl <- '2_clean/secondary_metrics.xlsx'
file.copy(path, new_xl, overwrite = TRUE)

# Pull the working excel from OneDrive to yoink variable names and figure out
# what we need to do
sheets <- excel_sheets(new_xl)[1:5]
tab <- map(sheets, ~ {
  read_excel('2_clean/secondary_metrics.xlsx', sheet = .x) %>% 
    mutate(
      dimension = str_to_lower(.x), .before = 'index',
      quality = as.character(quality)
    ) %>% 
    fill(c(index, indicator), .direction = 'down')
}) %>% 
  bind_rows()
get_str(tab)

# Also pull weighting variables from utilities sheet
# add this to 5_objects so it gets lumped with sm_data and exported as rda
util_sheet <- read_excel('2_clean/secondary_metrics.xlsx', sheet = 'utilities')
utils <- util_sheet %>% 
  filter(status != 'stall') %>% 
  select(metric, variable_name)
saveRDS(utils, '5_objects/weighting_vars.rds')



# Get Summary -------------------------------------------------------------


# Summary table showing how states, counties, and years are represented by 
# each metric

# Pull them from our metrics and see which ones are fucked
existing_metrics <- metrics %>% 
  filter(variable_name %in% tab$variable_name) %>% 
  filter_fips('neast')
get_str(existing_metrics)

# Do existing first
existing_metrics_sum <- existing_metrics %>% 
  group_by(variable_name) %>% 
  summarize(
    n_states = length(unique(fips[nchar(fips) == 2])),
    n_counties = length(unique(fips[nchar(fips) == 5])),
    n_years = length(unique(sort(year))),
    first_year = min(unique(year)),
    latest_year = max(unique(year)),
    year_range = max(unique(as.numeric(year))) - min(unique(as.numeric(year)))
  )
get_str(existing_metrics_sum)
  
# Join with tab to get full metadata in addition to coverage
# Remove anything with quality NONE so it only shows metrics we are supposed
# to have.
sum <- full_join(tab, existing_metrics_sum) %>% 
  filter(
    quality != 'NONE',
    metric != 'NONE'
  )

# If there is no definition from excel sheet, use definition from metadata
sum <- sum %>% 
  mutate(definition = ifelse(
    is.na(definition),
    metadata$definition[match(variable_name, metadata$variable_name)],
    definition
  ))
get_str(sum)
sum



# Save to excel -----------------------------------------------------------

 
# Save this summary of what we have back to excel in OneDrive. Save with date
new_path <- paste0(
  'C:/Users/cdonov12/OneDrive - University of Vermont/Food Systems Research Center/Sustainability Metrics/Sustainability Metrics Manuscript/Metrics/',
  Sys.Date(),
  '_current_metric_collection.xlsx'
)

# Second sheet for column info
info <- data.frame(
  cols = c(
    'variable_name', 
    'n_states',
    'n_counties',
    'n_years', 
    '(other)',
    '(other)'
  ),
  definitions = c(
    'gross names used in code to uniquely identify metric',
    'number of states represented by metric (total 9 in Northeast)',
    'number of counties represented by metric. Total is technically 218, but 226 or 217 may also be complete because of issues with Connecticut',
    'number of years represented by metric',
    paste(
      'Note that this workbook will be overwritten periodically, so please don\'t edit anything here.',
      'Or at least be okay with it getting erased.'
    ),
    paste(
      'Note that this workbook only shows metrics that are listed in the metadata sheet and do not contain "NONE" for quality or metric columns.',
      'In other words, it shows only the metrics that are "supposed" to be there, to get a sense of how things are progressing.'
    )
  )
)
sheet_names <- list('metric_summary' = sum, 'new_col_info' = info)
openxlsx2::write_xlsx(
  sheet_names, 
  new_path, 
  widths = c(15, 'auto'),
  na.strings = 'NA'
)



# Save RDS ----------------------------------------------------------------


# NOTE: moving data paper sets to SMdocs. No longer contained in SMdata

# # Save the same data paper meta to objects, to be lumped into sm_data export
# saveRDS(sum, '5_objects/data_paper_meta.rds')
# 
# # Also save a version as a tree with numbered NONEs to show gaps
# tree <- tab %>% 
#   select(dimension, index, indicator, metric)
# count <- sum(tree$metric == 'NONE')
# tree$metric[tree$metric == 'NONE'] <- paste0('NONE_', 1:count)
# saveRDS(tree, '5_objects/data_paper_tree.rds')
# 
# # Also saving all existing metrics for data paper to be lumped into sm_data
# saveRDS(existing_metrics, '5_objects/data_paper_metrics.rds')



# Save CSV ----------------------------------------------------------------


# Saving a somewhat cleaner version of our metrics data specific to this 
# project, i.e. only these metrics, not the whole collection.
long <- metrics %>% 
  filter(variable_name %in% tab$variable_name) %>% 
  filter_fips('neast')
get_str(long)
# This is a clean long-format version

# Also make a clean wide-format version with last years only
# TODO: Can't pivot properly here - just hay yield for one year?
# Removing fips 42 hayYieldMeasuredInTonsAcre 2022. has 2 values. Look into this
# though
source('3_functions/data_pipeline_functions.R')
indices <- which(
  long$variable_name == 'hayYieldMeasuredInTonsAcre' 
  & long$year == '2022'
  & long$fips == '42'
)
long <- long[-indices, ]

# Continue with wide
wide <- long %>% 
  get_latest_year() %>% 
  pivot_wider(
    id_cols = fips,
    names_from = variable_name,
    values_from = value
  )
get_str(wide)

# long %>%
#   get_latest_year() %>%
#   dplyr::summarise(n = dplyr::n(), .by = c(fips, variable_name)) |>
#   dplyr::filter(n > 1L)

# Save these both to OneDrive for Isabella
root <- 'C:/Users/cdonov12/OneDrive - University of Vermont/Food Systems Research Center/Sustainability Metrics/Sustainability Metrics Manuscript/Data/'
paths <- c(
  paste0(root, 'metrics_', c('long', 'wide'), '.csv'),
  paste0(root, c('metadata.csv', 'fips_key.csv'))
)
clean_sum <- sum %>% 
  select(-c(n_states:last_col()))
walk2(paths, list(long, wide, clean_sum, fips_key), ~ write_csv(.y, .x))

zip_path <- paste0(root, 'prelim_data.zip')
zip(
  zipfile = zip_path, 
  files = paths,
  flags = '-j'
)
file.remove(paths)

