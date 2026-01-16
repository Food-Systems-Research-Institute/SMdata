# Setup
# 2025-07-03 update


# Description -------------------------------------------------------------

# Loads essential packages, including a package manager, conflict manager,
# and package of convenience functions that are used throughout the project.
# Finally, loads the SMdata project, which makes all datasets and objects
# like the fips_key available, as well as internal functions used in data
# wrangling and metadata creation.


# Housekeeping ------------------------------------------------------------

# Install package manager if needed
if (!require('pacman')) install.packages('pacman')

# Load critical packages
pacman::p_load(
  conflicted,
  devtools
)

# Load package of convenience functions used throughout project
pacman::p_load_gh('ChrisDonovan307/projecter')

# Load SMdata package, making functions and datasets available. This includes
# internal functions for use in data wrangling
devtools::load_all()

