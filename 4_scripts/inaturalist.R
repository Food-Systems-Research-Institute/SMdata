# iNaturalist
# 2025-08-24 update


# Description -------------------------------------------------------------


# Using iNaturalist observations to get species counts by taxa at county level
# across Northeast counties. Bulk download through GBIF for just northeast 
# states. This zip is already ~20GB, so doing this for CONUS would be 
# challenging.

# Note that a better way to process might be reading chunked csv from the
# download link itself. However, this will probably take about an hour. To
# download and then unzip.

# TODO: issue where read_chunked_csv fails with about 5% of dataset left due to
# max string length. Not sure why this is happening or why string length keeps
# growing. Could try to split csv in half from CLI 



# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  readr,
  sf,
  mapview,
  vegan,
  tidyr,
  arrow,
  furrr,
  tictoc
)



# Load 2018-2024 ----------------------------------------------------------


# This is rework, just taking 18 to 24, processing all at once. Hope to avoid
# the string length problem we had before

# Processing in chunks, saving as parquet files
# Filtering to animals and plants - these will be metrics for biodiversity
output_dir <- "1_raw/inaturalist/parquet_chunks"
if (!dir.exists(output_dir)) dir.create(output_dir)

chunk_filter <- function(df) {
  df[
    df$kingdom %in% c("Animalia", "Plantae") & df$year >= 2018,
    c("kingdom", "species", "year", "decimalLatitude", "decimalLongitude")
  ]
}

# Counter to name each chunk file uniquely
chunk_id <- 0

# Callback function to process each chunk
callback <- SideEffectChunkCallback$new(function(df, pos) {
  filtered <- chunk_filter(df)
  # Update chunk ID
  chunk_id <<- chunk_id + 1
  chunk_path <- file.path(output_dir, paste0("chunk_", chunk_id, ".parquet"))
  # Write filtered chunk as Parquet file, remove files and clear memory
  arrow::write_parquet(filtered, sink = chunk_path)
  rm(filtered, df)
  gc()
})

# Read CSV in chunks, using callback on each chunk, saving to parquet files
read_tsv_chunked(
  file = "1_raw/inaturalist/0033206-250811113504898/0033206-250811113504898.csv",
  callback = callback,
  chunk_size = 100000 
)

# # Read all parquet files together to check
# dataset <- open_dataset(output_dir, format = "parquet")
# dat <- collect(dataset)
# get_str(dat)
# sort(unique(dat$year))



## Process Parquet ---------------------------------------------------------


# Combined files are too big to perform spatial join. We need to process each
# parquet separately, run the join, then save without spatial data. Then we can
# hopefully combine and get summary stats.

# Find files
raw_chunks <- "1_raw/inaturalist/parquet_chunks"
files <- list.files(raw_chunks, full.names = TRUE)

# Prep counties file. Project into same format as iNaturalist data (4326)
neast_counties_2024
st_crs(neast_counties_2024)
county_prj <- st_transform(neast_counties_2024, 4326)
st_crs(county_prj)

# New output directory
output <- '1_raw/inaturalist/spatial_chunks/'
# For each parquet, drop all NAs (if we are missing any we can't use it), make
# it sf object, define projection, join with counties, remove extra cols and
# spatial data, and save as parquet

# test <- read_parquet(files[1]) %>%
#   drop_na(decimalLongitude, decimalLatitude) %>% 
#   st_as_sf(coords = c('decimalLongitude', 'decimalLatitude'))

get_time()
tic()
plan(multisession, workers = availableCores(omit = 2))
future_iwalk(files, ~ {
  df <- .x %>% 
    read_parquet() %>% 
    na.omit() %>% 
    st_as_sf(coords = c('decimalLongitude', 'decimalLatitude'))
  st_crs(df) <- 4326
  df <- df %>% 
    st_join(county_prj) %>% 
    select(kingdom, species, year, fips) %>% 
    st_drop_geometry()
  path <- paste0(output, 'chunk_', .y, '.parquet')
  write_parquet(df, path)
}, .options = furrr_options(seed = TRUE))
plan(sequential)
toc()

get_str(read_parquet(paste0(output, 'chunk_99.parquet')))



# Group and Rarefy --------------------------------------------------------


# Read all spatial parquet files together from 2018 to 2024
all <- open_dataset(output, format = "parquet")
all <- collect(all)
get_str(all)

## Group
# then get abundance table, filter for > 100 observations
dat_n100 <- all %>%
  group_by(fips, year, kingdom) %>%
  filter(n() >= 100) %>%
  ungroup()
get_str(dat_n100)
nrow(all) - nrow(dat_n100)
# Lost very little here - that's good

abund_table <- dat_n100 %>%
  group_by(fips, year, kingdom, species) %>%
  summarize(n = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = species,
    values_from = n,
    values_fill = 0
  )
abund_table

abund_only <- abund_table %>%
  select(-fips, -year, -kingdom)
abund_only

min_n <- min(rowSums(abund_only))



## Rarefy ------------------------------------------------------------------


rare_rich <- rarefy(abund_only, sample = min_n)

# Combine with grouping info
abund_table$rare_rich <- rare_rich
rare_rich <- abund_table %>% 
  select(fips, year, kingdom, rare_rich)
get_str(rare_rich)

# Make name 3 characters to match others
rare_rich <- rename(rare_rich, rar = rare_rich)
get_str(rare_rich)



## Counts ------------------------------------------------------------------


# Get counts of observations, species, and diversity by county
dat <- all %>%
  group_by(fips, year, kingdom) %>%
  summarize(
    n_spp = n_distinct(species),
    n_obs = n(),
    div = diversity(table(species)),
    .groups = "drop"
  )
get_str(dat)

# Add rarefied richness to check bias
dat <- left_join(dat, rare_rich)
get_str(dat)

# # Map diversity by county to check
# check <- out %>% 
#   filter(kingdom == 'Animalia') %>% 
#   right_join(neast_counties_2024) %>% 
#   st_as_sf()
# get_str(check)
# mapview(check, zcol = 'div')

# Get into format for metrics
get_str(dat)
dat <- dat %>%
  pivot_wider(
    id_cols = c(fips, year),
    names_from = kingdom,
    values_from = c(n_spp, n_obs, div, rar),
    names_glue = "{kingdom}_{.value}"
  )
get_str(dat)

# Rename cols
dat <- dat %>% 
  setNames(c(names(dat) %>% 
               str_replace('Animalia', 'animal') %>% 
               str_replace('Fungi', 'fungus') %>% 
               str_replace('Plantae', 'plant') %>% 
               str_remove('_n') %>% 
               snakecase::to_lower_camel_case()
             ))
get_str(dat)  
  
# Now pivot longer to put in format for metrics
# Also remove any missing fips
dat <- dat %>% 
  pivot_longer(
    cols = !c(fips, year),
    names_to = 'variable_name',
    values_to = 'value'
  ) %>% 
  drop_na(fips)
get_str(dat)    



# Metadata ----------------------------------------------------------------


(vars <- meta_vars(dat))

meta <- data.frame(
  variable_name = vars
) %>% 
  mutate(
    dimension = 'environment',
    index = 'species and habitat',
    indicator = 'biodiversity', 
    metric = paste(
      str_sub(variable_name, end = -4),
      case_when(
        str_detect(variable_name, 'Spp$') ~ 'species',
        str_detect(variable_name, 'Obs$') ~ 'observations',
        str_detect(variable_name, 'Div$') ~ 'diversity',
        str_detect(variable_name, 'Rar$') ~ 'rarefied richness',
        .default = NA
      )
    ) %>% 
      snakecase::to_sentence_case(),
    definition = case_when(
      str_detect(metric, 'diversity') ~ 'Shannon diversity of species within kingdom',
      str_detect(metric, 'species') ~ 'Number of species within kingdom',
      str_detect(metric, 'observations') ~ 'Number of observations within kingdom',
      str_detect(metric, 'rarefied richnes') ~ 'Rarefied richness of observations within kingdom, truncated at counties wtih > 100 total observations',
      .default = NA
    ),
    units = case_when(
      str_detect(metric, 'diversity|rarefied') ~ 'index',
      .default = 'count'
    ),
    scope = 'national',
    resolution = meta_resolution(dat),
    year = meta_years(dat),
    latest_year = meta_latest_year(dat),
    updates = "continuous",
    source = 'iNaturalist',
    url = 'https://map.feedingamerica.org/'
  ) %>% 
    meta_citation(date = '2025-07-11')
get_str(meta)



# Save --------------------------------------------------------------------


## Check to make sure we have the same number of metrics and metas
check_n_records(dat, meta, 'iNaturalist')

# Save them
saveRDS(dat, '5_objects/metrics/inaturalist.RDS')
saveRDS(meta, '5_objects/metadata/inaturalist.RDS')

clear_data(gc = TRUE)
