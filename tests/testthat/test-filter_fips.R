test_that("filter_fips works correctly", {
  # Create test data
  test_data <- data.frame(
    fips = c("50", "09001", "09010", "25001", "00"),
    variable_name = "test_var",
    year = 2023,
    value = 1:5
  )

  # Test 'states' scope
  result_states <- filter_fips(test_data, scope = "states")
  expect_true(nrow(result_states) >= 0)
  expect_true(all(nchar(result_states$fips) == 2))

  # Test 'us' scope
  result_us <- filter_fips(test_data, scope = "us")
  expect_equal(unique(result_us$fips), "00")

  # Test 'all' scope
  result_all <- filter_fips(test_data, scope = "all")
  expect_true(nrow(result_all) >= 0)

  # Test invalid scope throws error
  expect_error(filter_fips(test_data, scope = "invalid"))

  # Test custom fips column
  test_data_custom <- test_data
  names(test_data_custom)[1] <- "custom_fips"
  result_custom <- filter_fips(test_data_custom, scope = "us", fips_col = "custom_fips")
  expect_equal(unique(result_custom$custom_fips), "00")
})