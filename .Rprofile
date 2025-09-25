# Load utils so we can use install.packages()
library(utils)

# Set CRAN mirror
local({
  r <- getOption("repos")
  r["CRAN"] <- "https://cloud.r-project.org"
  options(repos = r)
})

# Install pacman if not installed already
# Load conflicted for namespace conflicts
# Load devtools
if (interactive()) {
  suppressPackageStartupMessages(
    if (!requireNamespace('pacman')) {
      install.packages('pacman', dependencies = TRUE, quiet = TRUE)
    }
  )
  pacman::p_load(conflicted, usethis)
  pacman::p_load_gh('ChrisDonovan307/projecter')
  
  suppressMessages(require(devtools))
  
  # Set up vim function for kj escape
  try(vim <- function() rstudiovim::rsvim_exec_file())
  
  # Load SMdata package
  devtools::load_all()
  
  # Set conflict winners
  conflicts_prefer(
    dplyr::select(),
    dplyr::filter(),
    dplyr::rename(),
    dplyr::summarize(),
    purrr::flatten(),
    base::intersect(),
    base::union(),
    testthat::matches(),
    .quiet = TRUE
  )
}

# Set options
options(
  max.print = 950,
  pillar.print_max = 950,
  pillar.print_min = 950
)

# Load table of contents script
if (interactive() && Sys.getenv("RSTUDIO") == "1") {
	try(shell.exec('table_of_contents.R'))
}

source("renv/activate.R")
