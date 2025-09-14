test_that("fips_key data object is properly structured", {
  data(fips_key, package = "SMdata")

  # Check that fips_key exists and is a data frame
  expect_true(exists("fips_key"))
  expect_s3_class(fips_key, "data.frame")

  # Check required columns exist
  required_cols <- c("fips", "county_name", "state_name", "state_code")
  expect_true(all(required_cols %in% names(fips_key)))

  # Check that fips column contains character data
  expect_type(fips_key$fips, "character")

  # Check that we have both state and county level data
  fips_lengths <- nchar(fips_key$fips)
  expect_true(any(fips_lengths == 2))  # States
  expect_true(any(fips_lengths == 5))  # Counties
})

test_that("state_key data object is properly structured", {
  data(state_key, package = "SMdata")

  # Check that state_key exists and is a data frame
  expect_true(exists("state_key"))
  expect_s3_class(state_key, "data.frame")

  # Check required columns exist
  required_cols <- c("state", "state_code", "state_name", "full_state_code")
  expect_true(all(required_cols %in% names(state_key)))

  # Check data types
  expect_type(state_key$state, "character")
  expect_type(state_key$state_name, "character")

  # Check that state codes are 2 characters
  expect_true(all(nchar(state_key$state) == 2))
})