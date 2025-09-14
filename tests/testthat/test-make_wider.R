test_that("make_wider works correctly", {
  # Create test data in long format
  test_data <- data.frame(
    fips = rep(c("50001", "50002"), each = 2),
    year = rep(c(2022, 2023), 2),
    variable_name = rep(c("var1", "var2"), each = 2),
    value = c(1, 2, 3, 4)
  )

  result <- make_wider(test_data)

  # Check that result has correct structure
  expect_true("data.frame" %in% class(result))
  expect_true("fips" %in% names(result))
  expect_true("var1_2022" %in% names(result))
  expect_true("var2_2023" %in% names(result))
  expect_equal(nrow(result), 2)

  # Check that year column is removed
  expect_false("year" %in% names(result))

  # Test custom column names
  custom_data <- test_data
  names(custom_data) <- c("fips", "time", "metric", "val")

  result_custom <- make_wider(custom_data,
                             var_col = "metric",
                             year_col = "time",
                             val_col = "val")

  expect_true("var1_2022" %in% names(result_custom))
  expect_true("var2_2023" %in% names(result_custom))
})
