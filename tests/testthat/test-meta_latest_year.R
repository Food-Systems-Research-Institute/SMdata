test_that("meta_latest_year works correctly", {
  # Create test data with multiple years per variable
  test_data <- data.frame(
    fips = rep("50001", 6),
    variable_name = rep(c("var1", "var2", "var3"), each = 2),
    year = c(2020, 2023, 2021, 2022, 2019, 2024),
    value = 1:6
  )

  result <- meta_latest_year(test_data)

  # Check that result is character vector
  expect_type(result, "character")
  expect_length(result, 3)

  # Check that results are in correct order (alphabetical by variable name)
  expect_equal(names(result), NULL)  # Should be unnamed vector

  # Check that latest years are correctly identified
  expect_equal(result[1], "2023")  # var1: max(2020, 2023) = 2023
  expect_equal(result[2], "2022")  # var2: max(2021, 2022) = 2022
  expect_equal(result[3], "2024")  # var3: max(2019, 2024) = 2024

  # Test with single year data
  single_year_data <- data.frame(
    variable_name = c("varA", "varB"),
    year = c(2023, 2023),
    value = 1:2
  )

  result_single <- meta_latest_year(single_year_data)
  expect_equal(result_single, c("2023", "2023"))

  # Test with character years
  char_year_data <- data.frame(
    variable_name = "var1",
    year = "2023",
    value = 1
  )

  result_char <- meta_latest_year(char_year_data)
  expect_equal(result_char, "2023")
})