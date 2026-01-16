# Check BRFSS data

pacman::p_load(
  readr,
  dplyr,
  haven
)
# dat <- read.delim('1_raw/cdc/brfss/LLCP2023.ASC')
# dat <- haven::read_xpt('1_raw/cdc/brfss/MMSA2023.xpt')
get_str(dat)

dat[['_MMSA']] %>% unique %>% length
# only 128 MMSAs or counties represented?

dat[['MMSANAME']] %>% 
  unique %>% 
  sort


# -------------------------------------------------------------------------


dat <- haven::read_xpt('1_raw/cdc/brfss/LLCP2023.XPT')
get_str(dat)
# This might work, but we need to figure out weights and PSUs
# Does it have life satisfaction?
