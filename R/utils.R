# Test Mode and Debugging -------------------------------------------------

tar_debug <- function() {
  targets::tar_make(
    callr_function = NULL,
    use_crew = FALSE,
    as_job = FALSE
  )
}

# Test mode - warn instead of error for missing data
is_test_mode <- function() {
  getOption("SMdata.test_mode", default = FALSE)
}

set_test_mode <- function(value = TRUE) {
  options(SMdata.test_mode = value)
}

# Limit API calls - NULL is no limit (all items)
limit_calls <- function(x, n = NULL) {
  if (is.null(n)) {
    x
  } else {
    x[seq_len(min(n, length(x)))]
  }
}


# Logging -----------------------------------------------------------------

setup_logger <- function(log_dir = "logs", log_file = "pipeline.log") {
  dir.create(log_dir, showWarnings = FALSE, recursive = TRUE)
  log_path <- file.path(log_dir, log_file)

  # Log to both console and file
  logger::log_appender(logger::appender_tee(log_path))
  logger::log_threshold(logger::INFO)
  logger::log_info("Logger initialized: {log_path}")

  invisible(log_path)
}

log_step <- function(step, msg = NULL, data = NULL) {
  if (!is.null(msg)) {
    logger::log_info("[{step}] {msg}")
  } else {
    logger::log_info("[{step}]")
  }

  if (!is.null(data) && is.data.frame(data)) {
    n_vars <- if ("variable_name" %in% names(data)) length(unique(data$variable_name)) else ncol(data)
    logger::log_info("
 Rows: {nrow(data)} | Vars: {n_vars}")
  }

  invisible(NULL)
}


# Validation --------------------------------------------------------------


validate_data <- function(data,
                          agent,
                          step = "validation",
                          report_dir = "logs/validation") {
  
  agent <- agent %>% pointblank::interrogate()

  x_list <- pointblank::get_agent_x_list(agent)
  n_steps <- length(x_list$validation_set)
  n_pass <- sum(x_list$validation_set$all_passed, na.rm = TRUE)
  n_fail <- n_steps - n_pass

  # Log it
  logger::log_info("[{step}] Validation: {n_pass}/{n_steps} checks passed")

  if (!is.null(report_dir)) {
    dir.create(report_dir, showWarnings = FALSE, recursive = TRUE)
    report_file <- file.path(report_dir, paste0(step, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html"))
    tryCatch({
      pointblank::export_report(agent, filename = report_file)
      logger::log_info("[{step}] Report saved: {report_file}")
    }, error = function(e) {
      logger::log_warn("[{step}] Could not save report: {e$message}")
    })
  }

  # Handle failures
  if (n_fail > 0) {
    failed_idx <- which(!x_list$validation_set$all_passed)
    msg <- paste0(
      "[", step, "] Validation failed for checks: ", 
      paste(failed_idx, collapse = ", ")
    )

    if (is_test_mode()) {
      logger::log_warn(msg)
      return(list(passed = FALSE, agent = agent))
    } else {
      logger::log_error(msg)
      stop(msg, call. = FALSE)
    }
  }

  logger::log_success("[{step}] All validations passed")
  list(passed = TRUE, agent = agent)
}


# Create standard validation agents for different pipeline stages
create_api_validator <- function(data) {
  pointblank::create_agent(data) %>%
    pointblank::rows_distinct() %>%
    pointblank::col_exists(columns = c("Value", "year", "short_desc")) %>%
    pointblank::col_vals_not_null(columns = "Value")
}

create_wrangled_validator <- function(data) {
  pointblank::create_agent(data) %>%
    pointblank::col_exists(columns = c("variable_name", "fips", "year", "value")) %>%
    pointblank::col_vals_not_null(columns = c("variable_name", "fips", "year")) %>%
    pointblank::col_vals_regex(columns = "fips", regex = "^\\d{2,5}$")
}

create_final_validator <- function(data, min_rows = 100) {
  if (nrow(data) < min_rows) {
    logger::log_warn("Data has {nrow(data)} rows, expected at least {min_rows}")
  }

  pointblank::create_agent(data) %>%
    pointblank::col_exists(
      columns = c("variable_name", "fips", "year", "value")
    ) %>%
    pointblank::col_vals_not_null(
      columns = c("variable_name", "fips", "year")
    ) %>%
    pointblank::rows_distinct(
      columns = c("variable_name", "fips", "year")
    )
}

check_data <- function(data, checks = list(), step = "check") {
  agent <- pointblank::create_agent(data)

  if (isTRUE(checks$not_empty)) {
    agent <- agent %>%
      pointblank::col_vals_not_null(columns = names(data)[1])
  }

  if (!is.null(checks$has_vars)) {
    agent <- agent %>%
      pointblank::col_exists(columns = checks$has_vars)
  }

  if (!is.null(checks$no_na)) {
    agent <- agent %>%
      pointblank::col_vals_not_null(columns = checks$no_na)
  }

  if (!is.null(checks$min_rows)) {
    if (nrow(data) < checks$min_rows) {
      msg <- paste0("[", step, "] Expected at least ", checks$min_rows, " rows, got ", nrow(data))
      if (is_test_mode()) {
        logger::log_warn(msg)
        return(FALSE)
      } else {
        logger::log_error(msg)
        stop(msg, call. = FALSE)
      }
    }
  }

  result <- validate_data(data, agent, step = step, report_dir = NULL)
  result$passed
}
