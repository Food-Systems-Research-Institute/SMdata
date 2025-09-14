test_that("check_n_records works correctly", {
  # Create matching test data
  metric_data <- data.frame(
    variable_name = c("var1", "var2", "var1", "var2"),
    value = 1:4
  )

  meta_data <- data.frame(
    variable_name = c("var1", "var2"),
    description = c("Variable 1", "Variable 2")
  )

  # Should pass without error
  expect_output(
    check_n_records(metric_data, meta_data, section = "test"),
    "test variable check: PASS"
  )

  # Test with mismatched data
  mismatched_meta <- data.frame(
    variable_name = c("var1", "var3"),
    description = c("Variable 1", "Variable 3")
  )

  expect_error(
    check_n_records(metric_data, mismatched_meta, section = "test"),
    "test variable check: FAIL"
  )

  # Test custom column name
  custom_metric <- metric_data
  names(custom_metric)[1] <- "custom_var"
  custom_meta <- meta_data
  names(custom_meta)[1] <- "custom_var"

  expect_output(
    check_n_records(custom_metric, custom_meta,
                   section = "custom", var_col = "custom_var"),
    "custom variable check: PASS"
  )
})
