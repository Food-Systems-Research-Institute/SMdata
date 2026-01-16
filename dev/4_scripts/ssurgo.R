# SSURGO
# 2025-07-14


# Description -------------------------------------------------------------

# Pulling SSURGO soil data through soilDB package


# Housekeeping ------------------------------------------------------------

pacman::p_load(
  dplyr,
  sf,
  reticulate,
  tidyr,
  stars
)



# SSURGO ------------------------------------------------------------------


valu <- st_read('1_raw/spatial/ssurgo/gSSURGO_CONUS/gSSURGO_CONUS.gdb/', layer = 'Valu1')
get_str(valu)

# Has soil organic carbon at different depths
  # IPCC sets 30cm fixed depth sample as minimum Smith, P. et al. How to measure, report and verify soil carbon change to realize the potential of soil carbon sequestration for atmospheric greenhouse gas removal. Glob. Chang Biol. 26, 219–241 (2020)
  # use sum of first three layers (soc0_5, soc5_20, soc20_50)
  # we would need to take weighted average of these because it is in C per square meter
# available water storage at depths - similar to filtration?
  # aws0_5, aws5_20, aws20_50
  # these are in mm, so we can sum them

component <- st_read('1_raw/spatial/ssurgo/gSSURGO_CONUS/gSSURGO_CONUS.gdb/', layer = 'component')
get_str(component)
# Has cation exchange (taxceactcl)



# Process Raster ----------------------------------------------------------


# Reducing raster from CONUS to northeast only
# Note: Real raster processed with ArcMap (only way to do so, can't in python)
# Just saved it as a tif

# Note: This conus raster is 56GB. Worth getting stats for US then deleting?

# Run mask function
# reticulate::source_python('3_functions/spatial/mask_rasters.py')
# mask_rasters(
#   input_dir = '1_raw/spatial/ssurgo/',
#   output_dir = '1_raw/spatial/ssurgo/masked/',
#   aoi_path = '2_clean/spatial/neast_mask.gpkg'
# )

# Check
# raster <- stars::read_stars('1_raw/spatial/ssurgo/masked/gSSURGO_CONUS_masked.tif')
# plot(raster)

# Get counts of cell types in each county
# TODO: run this for all counties, not just neast
reticulate::source_python('3_functions/spatial/cat_zonal_stats.py')
raster_path <- '1_raw/spatial/ssurgo/masked/gSSURGO_CONUS_masked.tif' 
county_path <- '2_clean/spatial/neast_counties_2024.gpkg'
out <- cat_zonal_stats(raster_path, county_path)
get_str(out)
dim(out)

# Check sums of cells across each fips
test <- out
test$cell_count <- rowSums(test[ , !(names(test) %in% "fips")], na.rm = TRUE)
test %>% 
  select(fips, cell_count) %>% 
  arrange(fips)
# Chittenden is 1786221
# 1786221 * 900 because 30m cells, this is sq meters
# 1786221 * 900 / 1,000,000 to get sq km
1786221 * 900 / 1e6 # 1600 is right, checks out pretty close

# Prep our ssurgo dataset for join
# Using valu and component sets
get_str(valu)
get_str(component)

# From valu: 
  # soc0_5, soc5_20, soc20_50
  # we would need to take weighted average of these because it is in C per square meter
  # aws0_5, aws5_20, aws20_50
  # these are in mm, so we can sum them
valu_tab <- valu %>% 
  select(mukey, soc0_5, soc5_20, soc20_50, aws0_5, aws5_20, aws20_50)
get_str(valu_tab)  

# # Let's not use this - doesn't have much info
# # From component:
#   # taxceactcl
# comp_tab <- component %>% 
#   select(mukey, taxceactcl)
# get_str(comp_tab)  

# Pivot counties longer to join with value tab
out_long <- out %>% 
  pivot_longer(
    cols = !fips,
    names_to = 'mukey',
    values_to = 'count'
  )
get_str(out_long)

# Join together
dat <- left_join(out_long, valu_tab)
get_str(dat)

# Check cells per county
dat %>% 
  group_by(fips) %>% 
  summarize(n_cells = sum(count, na.rm = TRUE))
# Still good

# Get weighted average of soil organic carbon, and sum of aws (in mms)
dat <- dat %>% 
  mutate(
    soc = ((soc0_5 + (soc5_20 * 3) + (soc20_50 * 6))/10) * count,
    aws = (aws0_5 + aws5_20 + aws20_50) * count
  )
get_str(dat)
head(dat)

# Now group by fips to get summaries
dat <- dat %>% 
  group_by(fips) %>% 
  summarize(
    cell_count = sum(count, na.rm = TRUE),
    soilOrganicCarbon = sum(soc, na.rm = TRUE) / cell_count,
    availableWaterStorage = sum(aws, na.rm = TRUE) / cell_count
  )
head(dat)
get_str(dat)
dat[dat$fips == '50007', ]

# Pivot longer to metrics format
dat <- dat %>% 
  select(-cell_count) %>%
  pivot_longer(
    cols = !fips,
    values_to = 'value',
    names_to = 'variable_name'
  ) %>% 
  mutate(year = '2024')
get_str(dat)


# Metadata ----------------------------------------------------------------


meta_vars(dat)

meta <- data.frame(
  variable_name = meta_vars(dat),
  metric = c(
    'Available water storage',
    'Soil organic carbon'
  ),
  definition = c(
    'Available water storage estimate (AWS) as a sum of layers 1, 2, and 3 (0 to 50 cm depth total) expressed in mm. The volume of plant available water that the soil can store in this layer based on all map unit components (weighted average).',
    'Soil organic carbon stock estimate (SOC) as weighted average of standard layers 1, 2, and 3 (0 to 50cm depth total). The concentration of organic carbon present in the soil expressed in grams C per square meter.'
  ),
  units = c(
    'mm',
    'grams per sq meter'
  ),
  axis_name = c(
    'AWS (mm)',
    'SOC (G/m^2)'
  ),
  dimension = 'environment',
  index = 'soils',
  indicator = c(
    'structure',
    'biology'
  ),
  scope = 'national',
  resolution = '30m',
  year = meta_years(dat),
  latest_year = meta_latest_year(dat),
  updates = "annual",
  source = 'U.S. Department of Agriculture, Natural Resources Conservation Service (2025). Soil Survey Geographic Database (SSURGO).',
  url = 'https://www.nrcs.usda.gov/resources/data-and-reports/gridded-soil-survey-geographic-gssurgo-database'
) %>% 
  meta_citation(date = '2025-08-06')

get_str(meta)



# Save and Clear ----------------------------------------------------------


# Check record counts
check_n_records(dat, meta, 'ssurgo')

saveRDS(dat, '5_objects/metrics/ssurgo.RDS')
saveRDS(dat, '5_objects/metadata/ssurgo_meta.RDS')

clear_data(gc = TRUE)
