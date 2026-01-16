# NASS Wrangling Functions
# Refactored from 4_scripts/nass.R


wrangle_nass_combine <- function(census, survey, farm, organic) {
  dplyr::bind_rows(census, survey, farm, organic)
}


wrangle_nass <- function(bound, nass_params) {
  # Filter and select relevant variables, create FIPS
 dat <- bound %>%
    dplyr::filter(
      freq_desc %in% c('ANNUAL', 'POINT IN TIME'),
      county_code != '998'
    ) %>%
    dplyr::select(
      agg_level_desc,
      domaincat_desc,
      short_desc,
      value = Value,
      year,
      county_ansi,
      state_ansi,
      cv = `CV (%)`,
      unit_desc
    ) %>%
    dplyr::mutate(
      fips = dplyr::case_when(
        agg_level_desc == 'COUNTY' ~ paste0(state_ansi, county_ansi),
        agg_level_desc == 'STATE' ~ state_ansi,
        .default = NA
      ),
      .keep = 'unused'
    )

  # Pull out farm size vars (short_desc is same for all, need domaincat_desc)
  farm_size <- dat %>%
    dplyr::filter(stringr::str_detect(domaincat_desc, '^AREA')) %>%
    dplyr::left_join(
      dplyr::select(nass_params, domaincat_desc, variable_name),
      by = 'domaincat_desc'
    )

  # For other vars, join with nass_params
  others <- dat %>%
    dplyr::filter(stringr::str_detect(domaincat_desc, '^AREA OPERATED', negate = TRUE))

  dat <- nass_params %>%
    dplyr::filter(is.na(domaincat_desc)) %>%
    dplyr::select(short_desc, variable_name) %>%
    dplyr::right_join(others, by = 'short_desc')

  # Combine back together
 dat <- dplyr::bind_rows(farm_size, dat)

  # Create variable names from short_desc where missing
  dat <- dat %>%
    dplyr::mutate(
      variable_name = dplyr::case_when(
        is.na(variable_name) ~ snakecase::to_lower_camel_case(short_desc),
        .default = variable_name
      )
    )

  # Reduce to essential columns
  dat %>%
    dplyr::select(variable_name, fips, year, value)
}


# Helper function for calculating derived variables
calculate_var <- function(df, num, denom, name) {
  # Check if required variables exist
  available_vars <- unique(df$variable_name)
  missing <- setdiff(c(num, denom), available_vars)

  if (length(missing) > 0) {
    msg <- paste0("Missing variables for '", name, "': ", paste(missing, collapse = ", "))
    if (is_test_mode()) {
      logger::log_warn("[calculate_var] {msg}")
      return(df)  # Return unchanged
    } else {
      logger::log_error("[calculate_var] {msg}")
      stop(msg, call. = FALSE)
    }
  }

  out <- df %>%
    dplyr::filter(variable_name %in% c(num, denom)) %>%
    tidyr::pivot_wider(
      id_cols = c(fips, year),
      names_from = variable_name,
      values_from = value
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      !!rlang::sym(name) := !!rlang::sym(num) / !!rlang::sym(denom),
      .keep = 'unused'
    )

  out <- out %>%
    tidyr::pivot_longer(
      cols = !c(fips, year),
      values_to = 'value',
      names_to = 'variable_name'
    )

  dplyr::bind_rows(df, out)
}


calculate_nass_derived <- function(dat) {
  dat <- calculate_var(dat, 'totalSalesValueAddedDirect', 'totalSalesCommodities', 'd2cSalesPropTotal')
  dat <- calculate_var(dat, 'totalSalesValueAddedDirect', 'totalSalesCommodities', 'salesValueAddedDirectPropTotal')
  dat <- calculate_var(dat, 'totalSalesValueAddedWholesale', 'totalSalesCommodities', 'salesValueAddedWholesalePropTotal')
  dat <- calculate_var(dat, 'totalIncomeAgTourismRecreation', 'totalFarmIncome', 'incomeAgTourismRecPropTotal')
  dat <- calculate_var(dat, 'expensesFertilizerLimeSoilCond', 'totalOperatingExpenses', 'fertExpensePropTotalExpense')
  dat <- calculate_var(dat, 'totalFuelExpenses', 'totalOperatingExpenses', 'fuelExpensePropTotalExpense')
  dat <- calculate_var(dat, 'nFemProducers', 'nMaleProducers', 'ftmProdRatio')
  dat <- calculate_var(dat, 'totalIncomeCropAnimalInsurance', 'totalFarmIncome', 'incomeCropAnimalInsurancePropTotal')
  dat <- calculate_var(dat, 'nOpsOrganic', 'nOperations', 'propOpsOrganic')
  dat <- calculate_var(dat, 'totalSalesValueAddedDirect', 'totalSalesValueAddedWholesale', 'retailSalesPropWholesale')
  dat <- dat %>%
    dplyr::filter(variable_name %in% c('totalSalesValueAddedDirect', 'totalSalesValueAddedWholesale')) %>%
    tidyr::pivot_wider(
      id_cols = c(fips, year),
      values_from = value,
      names_from = variable_name
    ) %>%
    dplyr::mutate(
      salesValueAdded = totalSalesValueAddedDirect + totalSalesValueAddedWholesale,
      .keep = 'unused'
    ) %>%
    tidyr::pivot_longer(
      cols = !c(fips, year),
      values_to = 'value',
      names_to = 'variable_name'
    ) %>%
    dplyr::bind_rows(dat)

  dat <- calculate_var(dat, 'salesValueAdded', 'totalSalesCommodities', 'salesValueAddedPropTotal')

  out <- dat %>%
    dplyr::filter(stringr::str_detect(variable_name, '^n.+Producers$')) %>%
    tidyr::pivot_wider(
      id_cols = c(fips, year),
      values_from = value,
      names_from = variable_name
    ) %>%
    dplyr::mutate(dplyr::across(!c(fips, year), ~ ifelse(is.na(.x), 0, .x)))

  out$producerRacialDiversity <- out %>%
    dplyr::select(where(is.numeric)) %>%
    vegan::diversity()

  dat <- out %>%
    dplyr::select(fips, year, producerRacialDiversity) %>%
    tidyr::pivot_longer(
      cols = producerRacialDiversity,
      names_to = 'variable_name',
      values_to = 'value'
    ) %>%
    dplyr::bind_rows(dat)

  dat <- dat %>%
    dplyr::filter(variable_name %in% c('salesAnimal', 'salesCrop')) %>%
    tidyr::pivot_wider(
      id_cols = c(fips, year),
      values_from = value,
      names_from = variable_name
    ) %>%
    dplyr::mutate(
      salesAnimalAndCrop = salesAnimal + salesCrop,
      .keep = 'unused'
    ) %>%
    tidyr::pivot_longer(
      cols = !c(fips, year),
      values_to = 'value',
      names_to = 'variable_name'
    ) %>%
    dplyr::bind_rows(dat)

  dat
}


# Helper for weighted skew calculation
get_skew <- function(weights, counts) {
  stopifnot(length(weights) == length(counts))

  if (all(is.na(counts)) || sum(counts, na.rm = TRUE) == 0) {
    return(NA_real_)
  }

  counts[is.na(counts)] <- 0

  purrr::map(seq_along(weights), ~ {
    rep(weights[.x], counts[.x])
  }) %>%
    unlist() %>%
    e1071::skewness()
}


calculate_nass_skew <- function(dat) {
  # Producer age skew
  out <- dat %>%
    dplyr::filter(
      stringr::str_detect(variable_name, '^nProducers.*'),
      stringr::str_detect(variable_name, 'AreaOperated', negate = TRUE)
    ) %>%
    tidyr::pivot_wider(
      id_cols = c(fips, year),
      values_from = value,
      names_from = variable_name
    )

  trans <- out %>%
    dplyr::select(nProducersLT25:last_col())

  prod_midpoints <- c(22.5, 29.5, 39.5, 49.5, 59.5, 69.5, 79.5)

  skew <- trans %>%
    dplyr::rowwise() %>%
    dplyr::mutate(producerAgeSkew = get_skew(prod_midpoints, dplyr::c_across(everything()))) %>%
    dplyr::ungroup() %>%
    dplyr::pull(producerAgeSkew)

  dat <- out %>%
    dplyr::mutate(producerAgeSkew = skew) %>%
    dplyr::select(fips, year, producerAgeSkew) %>%
    tidyr::pivot_longer(
      cols = !c(fips, year),
      names_to = 'variable_name',
      values_to = 'value'
    ) %>%
    dplyr::bind_rows(dat)

  # Farm size skew
  out <- dat %>%
    dplyr::filter(stringr::str_detect(variable_name, 'nProducersAreaOperated')) %>%
    tidyr::pivot_wider(
      id_cols = c(fips, year),
      values_from = value,
      names_from = variable_name
    )

  trans <- out %>%
    dplyr::select(dplyr::matches('nProducers.')) %>%
    dplyr::select(
      dplyr::all_of(
        names(.) %>%
          .[order(as.integer(stringr::str_extract(., "\\d+$")))]
      )
    )

  size_midpoints <- c(5, 30, 60, 80, 120, 160, 200, 240, 380, 750, 1500, 2500)

  skew <- trans %>%
    dplyr::rowwise() %>%
    dplyr::mutate(farmSizeSkew = get_skew(size_midpoints, dplyr::c_across(everything()))) %>%
    dplyr::ungroup() %>%
    dplyr::pull(farmSizeSkew)

  out %>%
    dplyr::mutate(farmSizeSkew = skew) %>%
    dplyr::select(fips, year, farmSizeSkew) %>%
    tidyr::pivot_longer(
      cols = !c(fips, year),
      names_to = 'variable_name',
      values_to = 'value'
    ) %>%
    dplyr::bind_rows(dat)
}


smooth_acres_operated <- function(dat) {
  ac <- dplyr::filter(dat, variable_name == 'acresOperated')
  full_range <- 2002:2022

  ac <- ac %>%
    dplyr::mutate(year = as.numeric(year)) %>%
    tidyr::complete(fips, year = full_range, variable_name)

  ac <- ac %>%
    dplyr::group_by(fips) %>%
    dplyr::mutate(
      value = zoo::na.approx(value, na.rm = FALSE),
      variable_name = 'acresOperatedSmooth'
    ) %>%
    dplyr::ungroup()

  ac %>%
    dplyr::mutate(year = as.character(year)) %>%
    dplyr::bind_rows(dat)
}


#' USD to thousands
#'
#' Transform variables in USD into units of 1,000
#' @param dat 
#'
#' @returns
#' @export
#'
#' @examples
usd_to_thousands <- function(dat) {
  trans <- dat %>%
    meta_vars() %>%
    stringr::str_subset(
      stringr::regex(
        'landValPerAcre|expPF|animalandcrop',
        ignore_case = TRUE
      )
    )

  dat %>%
    dplyr::mutate(value = dplyr::case_when(
      variable_name %in% trans ~ value / 1000,
      .default = value
    ))
}


process_nass <- function(nass_combined, nass_params) {
  log_step("process_nass", "Starting NASS processing", nass_combined)

  # Validation - create agent
  validate_data(
    nass_combined,
    create_api_validator(nass_combined),
    step = "nass_combined_input"
  )

  # Wrangle
  log_step("wrangle_nass", "Wrangling raw data")
  dat <- wrangle_nass(nass_combined, nass_params)
  validate_data(
    dat,
    create_wrangled_validator(dat),
    step = "wrangle_nass_output"
  )
  log_step("wrangle_nass", "Complete", dat)

  # # Derived variables
  # log_step("calculate_nass_derived", "Calculating derived variables")
  # dat <- calculate_nass_derived(dat)
  # log_step("calculate_nass_derived", "Complete", dat)
  # 
  # # Skew calculations
  # log_step("calculate_nass_skew", "Calculating skew metrics")
  # dat <- calculate_nass_skew(dat)
  # log_step("calculate_nass_skew", "Complete", dat)
  # 
  # # Smooth acres
  # log_step("smooth_acres_operated", "Smoothing acres operated")
  # dat <- smooth_acres_operated(dat)
  # log_step("smooth_acres_operated", "Complete", dat)
  # 
  # # Transform values
  # log_step("usd_to_thousands", "Transforming values")
  # dat <- usd_to_thousands(dat)
  # log_step("usd_to_thousands", "Complete", dat)

  # Validation
  validate_data(
    dat,
    create_final_validator(dat),
    step = "process_nass_final"
  )

  log_step("process_nass", "Processing complete", dat)
  dat
}


create_nass_metadata <- function(dat, nass_params) {
  vars <- meta_vars(dat)
  dat_df <- data.frame(variable_name = vars)

  meta <- nass_params %>%
    dplyr::filter(source_desc == 'CENSUS') %>%
    dplyr::right_join(dat_df, by = 'variable_name') %>%
    dplyr::select(-dplyr::ends_with('desc'), -note)

  meta <- meta %>%
    dplyr::mutate(
      dimension = dplyr::case_when(
        is.na(dimension) & stringr::str_detect(variable_name, 'Yield') ~ 'production',
        .default = dimension
      ),
      index = dplyr::case_when(
        is.na(index) & stringr::str_detect(variable_name, 'Yield') ~ 'production margins',
        .default = dimension
      ),
      indicator = dplyr::case_when(
        is.na(indicator) & stringr::str_detect(variable_name, 'Yield') ~ 'yield',
        .default = indicator
      ),
      axis_name = dplyr::case_when(
        is.na(axis_name) ~ variable_name,
        .default = axis_name
      )
    ) %>%
    dplyr::mutate(
      latest_year = meta_latest_year(dat),
      year = meta_years(dat),
      resolution = meta_resolution(dat),
      scope = 'national',
      updates = '5 years',
      source = paste0(
        "U.S. Department of Agriculture, National Agricultural Statistics Service. ",
        "(2024). 2022 Census of Agriculture."
      ),
      url = 'https://www.nass.usda.gov/Publications/AgCensus/2022/'
    ) %>%
    meta_citation(date = Sys.Date())

  meta
}
