# -------------------------------------------------------------------------------------------------
# File: test-fit_ar_models.R
# Purpose: To provide unit tests for the fit_ar_models() function.
# Description: This script uses the 'testthat' framework to validate the behavior of the
#              fit_ar_models() function. It creates several simulated datasets to cover
#              various input scenarios and asserts that the function's output is correct.
#
# Scenarios tested:
#   - Correctness of fitted values for a simple AR(1) model against a manual lm() fit.
#   - Correct handling of data with multiple groups.
#   - Graceful handling of groups with insufficient data to fit a model.
#   - Correct behavior when input data contains NA values.
#
# Notes: This test script is designed to be run within the FishSET package structure using
#        devtools::test() or a similar tool. It assumes that the 'fit_ar_models' function
#        is loaded as part of the package. It requires the 'testthat' package.
#
# -------------------------------------------------------------------------------------------------

# Test Data Setup ---------------------------------------------------------------------------------
# Simple, predictable data for a single group to test correctness
ar_data_simple <- data.frame(
  date = as.Date("2023-01-01") + 0:4,
  group = "A",
  value = c(10, 20, 30, 40, 50)
)
unique_dates_simple <- ar_data_simple$date
unique_groups_simple <- "A"

# Data with multiple groups to test independent model fitting
ar_data_multi <- data.frame(
  date = rep(as.Date("2023-01-01") + 0:3, 2),
  group = rep(c("A", "B"), each = 4),
  value = c(10, 20, 30, 40, 5, 10, 15, 20) # Group B has a different series
)
unique_dates_multi <- unique(ar_data_multi$date)
unique_groups_multi <- c("A", "B")

# Data for edge case: one group has insufficient observations
ar_data_edge <- data.frame(
  date = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03", "2023-01-01")),
  group = c("A", "A", "A", "B"),
  value = c(10, 20, 30, 100)
)
unique_dates_edge <- unique(ar_data_edge$date)
unique_groups_edge <- c("A", "B")

# Unit Tests --------------------------------------------------------------------------------------
test_that("fit_ar_models gives correct fitted values for AR(1)", {
  # Fit the model using the function
  result_matrix <- fit_ar_models(
    unique_dates = unique_dates_simple,
    unique_groups = unique_groups_simple,
    obs_dates = ar_data_simple$date,
    obs_groups = ar_data_simple$group,
    obs_values = ar_data_simple$value,
    lag_p = 1
  )
  
  # Manually perform the same linear regression
  manual_df <- data.frame(
    value = ar_data_simple$value,
    lag1 = c(NA, 10, 20, 30, 40) # Create the lagged variable
  )
  manual_model <- lm(value ~ lag1, data = manual_df, na.action = na.omit)
  expected_fitted_values <- fitted(manual_model)
  
  # Extract the non-NA fitted values from the function's output matrix
  actual_values <- result_matrix[2:5, "A"]
  
  # Assert that the function's output matches the manual calculation
  expect_equal(unname(actual_values), unname(expected_fitted_values))
  # Assert that the first value is NA, as it cannot be fitted
  expect_true(is.na(result_matrix[1, "A"]))
})


test_that("fit_ar_models handles multiple groups correctly", {
  result_matrix <- fit_ar_models(
    unique_dates = unique_dates_multi,
    unique_groups = unique_groups_multi,
    obs_dates = ar_data_multi$date,
    obs_groups = ar_data_multi$group,
    obs_values = ar_data_multi$value,
    lag_p = 1
  )
  
  # Check dimensions and names
  expect_equal(nrow(result_matrix), length(unique_dates_multi))
  expect_equal(sort(colnames(result_matrix)), sort(unique_groups_multi))
  
  # Manually check group "B"
  manual_df_b <- data.frame(
    value = c(5, 10, 15, 20),
    lag1 = c(NA, 5, 10, 15)
  )
  manual_model_b <- lm(value ~ lag1, data = manual_df_b)
  expected_b <- unname(fitted(manual_model_b))
  
  # Extract and compare
  actual_b <- unname(result_matrix[2:4, "B"])
  expect_equal(actual_b, expected_b)
})


test_that("fit_ar_models handles groups with insufficient data", {
  # lag_p = 2 requires more than 2 observations to fit. Group "B" only has 1.
  result_matrix <- fit_ar_models(
    unique_dates = unique_dates_edge,
    unique_groups = unique_groups_edge,
    obs_dates = ar_data_edge$date,
    obs_groups = ar_data_edge$group,
    obs_values = ar_data_edge$value,
    lag_p = 2
  )
  
  # The column for Group "B" should contain only NAs
  expect_true(all(is.na(result_matrix[, "B"])))
  
  # The column for Group "A" should have a fitted value where possible
  expect_false(all(is.na(result_matrix[, "A"])))
  expect_equal(sum(!is.na(result_matrix[, "A"])), 1) # Only one fitted value possible
})


test_that("fit_ar_models correctly handles NAs in value series", {
  # Introduce an NA into the simple dataset
  ar_data_na <- ar_data_simple
  ar_data_na$value[3] <- NA # value series is now c(10, 20, NA, 40, 50)
  
  result_matrix <- fit_ar_models(
    unique_dates = unique_dates_simple,
    unique_groups = unique_groups_simple,
    obs_dates = ar_data_na$date,
    obs_groups = ar_data_na$group,
    obs_values = ar_data_na$value,
    lag_p = 1,
    empty_catch = NA # Make sure default NA handling is used
  )
  
  # Manually fit the model, lm() with na.omit will skip the NA and the row after it
  manual_values <- na.omit(ar_data_na)
  manual_df_na <- data.frame(
    value = manual_values,
    lag1 = c(NA, 10, 20, 40) # lag for row 4 is NA because value for row 3 is NA
  )
  manual_model <- lm(value.value ~ lag1, data = manual_df_na)
  
  # Only two rows are used: (value=20, lag1=10) and (value=50, lag1=40)
  expect_equal(length(as.vector(fitted(manual_model))), 3)
  expect_equal(as.character(ar_data_na$date[-na.action(manual_values)]), 
               c("2023-01-01", "2023-01-02", "2023-01-04", "2023-01-05"))
  
  # Check that the function's output matches
  expect_equal(unname(result_matrix[c("2023-01-02", "2023-01-04", "2023-01-05"), "A"]), 
               unname(fitted(manual_model)))
})
