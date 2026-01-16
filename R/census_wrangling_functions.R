# Census Wrangling Functions
# Refactored from 4_scripts/census.R


#' Combine Census API Outputs
#'
#' Combine ACS5, ACS1, and voting data into single data frame
#' @param acs5 ACS 5-year data
#' @param acs1 ACS 1-year data
#' @param voting Voting data
#' @returns Combined data frame
#' @keywords internal
wrangle_census_combine <- function(acs5, acs1, voting) {
  list(
    acs5 = acs5,
    acs1 = acs1,
    voting = voting
  )
}


#' Wrangle Census Data
#'
#' Core wrangling: create FIPS, recode NA values, pivot to long format
#' @param combined List with acs5, acs1, voting data frames
#' @returns Long format data frame with variable_name, fips, year, value
#' @keywords internal
wrangle_census <- function(combined) {
  acs5 <- combined$acs5
  acs1 <- combined$acs1
  voting <- combined$voting

  browser()
  dat <- acs5 %>%
    dplyr::mutate(fips = paste0(state, ifelse(is.na(county), '', county))) %>%
    dplyr::select(-c(state, county))

  # Recode -666666666 to missing for income and 0 to missing for housing year
  dat <- dat %>%
    dplyr::mutate(
      dplyr::across(dplyr::everything(), ~ dplyr::case_when(
        .x == -666666666 | .x == 0 ~ NA,
        .default = .x
      ))
    ) %>% 
    tidyr::pivot_longer(
      cols = !c(year, fips),
      names_to = 'variable_name',
      values_to = 'value'
    )

  if (nrow(acs1) > 0) {
    acs1_long <- acs1 %>%
      dplyr::mutate(
        fips = dplyr::case_when(
          is.na(county) ~ state,
          .default = paste0(state, county)
        ),
        .keep = 'unused'
      ) %>%
      tidyr::pivot_longer(
        cols = populationAnnual,
        names_to = 'variable_name',
        values_to = 'value'
      )
    dat <- dplyr::bind_rows(dat, acs1_long)
  }


  # Voter turnout
  vote_long <- voting %>%
    dplyr::rename(fips = state) %>%
    dplyr::mutate(fips = stringr::str_pad(fips, width = 2, pad = '0')) %>%
    tidyr::pivot_longer(
      cols = voterTurnout,
      names_to = 'variable_name',
      values_to = 'value'
    )

  # Combine all
  dplyr::bind_rows(dat, vote_long)
}


#' Calculate Census Derived Variables
#'
#' Calculate education percentages, vacancy rates, earnings ratios, disconnected youth
#' @param dat Long format data frame
#' @returns Data frame with derived variables added
#' @keywords internal
calculate_census_derived <- function(dat) {
  # Pivot wide for calculations
  wide <- dat %>%
    tidyr::pivot_wider(
      id_cols = c(fips, year),
      names_from = variable_name,
      values_from = value
    )

  # Calculate derived variables
  wide <- wide %>%
    dplyr::mutate(
      edPercHSGED = ((edTotalHS + edTotalGED) / edTotal) * 100,
      edPercBS = (edTotalBS / edTotal) * 100,
      edPercHSOrMore = ((edTotal - (edTotalBS + edTotalPhD + edTotalProf + edTotalMaster + edTotalSomeCollege + edTotalSomeCollegeLessThanYear)) / edTotal) * 100,
      vacancyRate = (nHousingVacant / nHousingUnits) * 100,
      womenEarnPercMenFFF = (medianFemaleEarningsFFF / medianMaleEarningsFPS) * 100,
      womenEarnPercMenFPS = (medianFemaleEarningsFPS / medianMaleEarningsFPS) * 100,
      disconnectedYouth = (nMaleNotEnrolledHSGradNotInLaborForce + nMaleNotEnrollNoGradNotInLaborForce +
        nFemaleNotEnrollHSGradNotInLaborForce + nFemaleNotEnrollNoGradNotInLaborForce) / n16to19 * 100
    ) %>%
    dplyr::select(-dplyr::matches('edTotal|^nMale|^nFemale|^n16'))

  # Pivot back to long
  wide %>%
    tidyr::pivot_longer(
      cols = !c(year, fips),
      names_to = 'variable_name',
      values_to = 'value'
    )
}


#' Smooth Census Population
#'
#' Fill missing years in population5Year using linear interpolation
#' @param dat Long format data frame
#' @returns Data frame with smoothed population added
#' @keywords internal
smooth_census_population <- function(dat) {
  # Fix capitalization if needed
  dat <- dat %>%
    dplyr::mutate(variable_name = dplyr::case_when(
      variable_name == 'population5year' ~ 'population5Year',
      .default = variable_name
    ))

  pop <- dplyr::filter(dat, variable_name == 'population5Year')

  # Complete range of years
  full_range <- 2013:2023
  pop <- pop %>%
    tidyr::complete(fips, year = full_range, variable_name)

  # Fill in missing values with linear interpolation
  pop <- pop %>%
    dplyr::group_by(fips) %>%
    dplyr::mutate(
      value = round(zoo::na.approx(value, na.rm = FALSE), 0),
      variable_name = 'population5YearSmooth'
    ) %>%
    dplyr::ungroup()

  # Add back to rest of dataset
  dplyr::bind_rows(dat, pop)
}


#' Calculate Census Population Change
#'
#' Calculate year-over-year percent change in population
#' @param dat Long format data frame with population5YearSmooth
#' @returns Data frame with populationChangePerc added
#' @keywords internal
calculate_census_pop_change <- function(dat) {
  out <- dat %>%
    dplyr::filter(variable_name == 'population5YearSmooth')

  out <- out %>%
    tidyr::pivot_wider(
      id_cols = c(year, fips),
      values_from = value,
      names_from = variable_name
    ) %>%
    dplyr::group_by(fips) %>%
    dplyr::arrange(year, .by_group = TRUE) %>%
    dplyr::mutate(
      populationChangePerc = (population5YearSmooth - dplyr::lag(population5YearSmooth)) / dplyr::lag(population5YearSmooth) * 100
    ) %>%
    dplyr::ungroup()

  # Back to long format
  out <- out %>%
    tidyr::pivot_longer(
      cols = c(population5YearSmooth, populationChangePerc),
      values_to = 'value',
      names_to = 'variable_name'
    )

  dplyr::bind_rows(dat, out)
}


#' Process Census Data
#'
#' Main orchestration function with logging and validation
#' @param census_combined Combined Census API outputs
#' @returns Processed long format data frame
#' @export
process_census <- function(census_combined) {
  log_step("process_census", "Starting Census processing")

  log_step("wrangle_census", "Wrangling raw data")
  dat <- wrangle_census(census_combined)
  validate_data(
    dat,
    create_wrangled_validator(dat),
    step = "census_wrangled"
  )
  log_step("wrangle_census", "Complete", dat)

  # # Derived variables
  # log_step("calculate_census_derived", "Calculating derived variables")
  # dat <- calculate_census_derived(dat)
  # log_step("calculate_census_derived", "Complete", dat)
  # 
  # # Population smoothing
  # log_step("smooth_census_population", "Smoothing population data")
  # dat <- smooth_census_population(dat)
  # log_step("smooth_census_population", "Complete", dat)
  # 
  # # Population change
  # log_step("calculate_census_pop_change", "Calculating population change")
  # dat <- calculate_census_pop_change(dat)
  # log_step("calculate_census_pop_change", "Complete", dat)

  # Final validation
  validate_data(
    dat,
    create_final_validator(dat),
    step = "census_final"
  )

  log_step("process_census", "Processing complete", dat)
  dat
}


#' Create Census Metadata
#'
#' Create metadata data frame from CSV template
#' @param dat Processed Census data
#' @param census_meta_path Path to metadata CSV file
#' @returns Metadata data frame
#' @export
create_census_metadata <- function(dat, census_meta_path) {
  # Load metadata from CSV
  meta <- readr::read_csv(census_meta_path, show_col_types = FALSE)
  
  # TODO: Remove this with full dataset
  dat <- dplyr::filter(dat, variable_name %in% meta$variable_name)
  vars <- meta_vars(dat)
  
  meta <- meta %>% 
    dplyr::filter(variable_name %in% vars) %>% 
    dplyr::arrange(variable_name) %>%
    dplyr::mutate(
      resolution = meta_resolution(dat),
      updates = dplyr::case_when(
        stringr::str_detect(variable_name, '5year') ~ 'annual',
        stringr::str_detect(variable_name, 'voterTurnout') ~ '2 years',
        .default = '5 years'
      ),
      latest_year = meta_latest_year(dat),
      year = meta_years(dat)
    ) %>%
    meta_citation(date = Sys.Date())

  meta
}
