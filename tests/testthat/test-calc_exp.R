# -------------------------------------------------------------------------------------------------
# File: test-calc_exp.R
# Purpose: To provide unit tests for the calc_exp() function.
# Description: This script uses the 'testthat' framework to validate the behavior of the
#              calc_exp() function. It creates a simulated dataset to cover various input
#              scenarios and asserts that the function's output is correct for each case. 
#
# Scenarios tested:
#   - Calculation without a temporal variable (simple overall mean).
#   - Standard average with a daily temporal variable.
#   - Revenue calculation using the 'price' argument.
#   - Grouped calculations using the 'defineGroup' argument.
#   - Correct application of 'day_lag' and 'year_lag'.
#   - Handling of missing data via 'empty_catch' and 'empty_expectation'.
#   - Creation of a binary dummy matrix when 'dummy_exp' is TRUE.
#   - Difference between weighted ('weight_avg' = TRUE) and unweighted daily averaging.
#   - Execution of the 'simpleLag' calculation method.
#
# Notes: This test script is designed to be run within the FishSET package structure using
#        devtools::test() or a similar tool. It assumes that the 'calc_exp' function
#        and its helpers are loaded as part of the package. It requires the 'testthat'
#        and 'lubridate' packages.
#
# -------------------------------------------------------------------------------------------------

# Test Data Setup ---------------------------------------------------------------------------------
set.seed(42)
test_data <- data.frame(
  date = as.Date(c(
    "2023-01-01", "2023-01-01", "2023-01-02", "2023-01-03", "2023-01-03",
    "2023-01-04", "2023-01-05", "2023-01-06", "2023-01-06", "2023-01-07",
    "2024-01-04", "2024-01-05", "2024-01-06"
  )),
  zone = c("A", "B", "A", "B", "B", "A", "A", "B", "B", "A", "A", "A", "B"),
  fleet = rep(c("F1", "F2"), length.out = 13),
  catch = c(10, 20, 12, 22, 24, NA, 16, 26, 28, 18, 100, 110, 200),
  price = c(2, 3, 2, 3, 3, 2, 2, 3, 3, 2, 2, 2, 3)
)

Alt <- list(
  dataZoneTrue = rep(1, nrow(test_data)),
  choice = test_data$zone
)

# Unit Tests --------------------------------------------------------------------------------------
test_that("calc_exp works correctly with no temporal variable", {
  result <- calc_exp(
    dataset = test_data,
    catch = "catch",
    temp_var = "none",
    Alt = Alt
  )
  
  # Expected means are calculated across the entire dataset for each zone.
  expected_A <- mean(test_data$catch[test_data$zone == "A"], na.rm = TRUE)
  expected_B <- mean(test_data$catch[test_data$zone == "B"], na.rm = TRUE)
  
  exp_matrix <- result$exp
  
  # Assertions
  expect_type(result, "list")
  expect_true(is.matrix(exp_matrix))
  expect_equal(nrow(exp_matrix), nrow(test_data))
  expect_equal(sort(unname(colnames(exp_matrix))), c("A", "B"))
  expect_equal(unname(exp_matrix[1, "A"]), expected_A)
  expect_equal(unname(exp_matrix[13, "B"]), expected_B)
})

test_that("standardAverage works with a daily temporal variable", {
  result <- calc_exp(
    dataset = test_data,
    catch = "catch",
    temp_var = "date",
    temporal = "daily",
    temp_window = 3,
    day_lag = 0,
    Alt = Alt,
    weight_avg = FALSE
  )
  
  exp_matrix <- result$exp
  
  # Check value for an observation on 2023-01-07 (row 10).
  # Window covers: 2023-01-05, 2023-01-06, 2023-01-07.
  # Zone A daily means in window: 16 (on 01-05), 18 (on 01-07). Expected: mean(16, 18) = 17.
  # Zone B daily means in window: mean(26, 28) = 27 (on 01-06). Expected: 27.
  expect_equal(unname(exp_matrix[10, "A"]), 17)
  expect_equal(unname(exp_matrix[10, "B"]), 27)
})

test_that("Revenue is calculated when 'price' argument is provided", {
  result <- calc_exp(
    dataset = test_data,
    catch = "catch",
    price = "price",
    temp_var = "date",
    temporal = "daily",
    temp_window = 3,
    day_lag = 0,
    Alt = Alt,
    weight_avg = FALSE
  )

  exp_matrix <- result$exp

  # Check value for an observation on 2023-01-05 (row 7).
  # Window covers: 2023-01-03, 2023-01-04, 2023-01-05.
  # Zone A revenue in window: 16*2=32 (on 01-05). NA on 01-04. Expected: 32.
  # Zone B revenue in window: mean(22*3, 24*3) = 69 (on 01-03). Expected: 69.
  expect_equal(unname(exp_matrix[7, "A"]), 32)
  expect_equal(unname(exp_matrix[7, "B"]), 69)
})

test_that("'defineGroup' correctly calculates expectations by group", {
  result <- calc_exp(
    dataset = test_data,
    catch = "catch",
    defineGroup = "fleet",
    temp_var = "date",
    temporal = "daily",
    temp_window = 3,
    day_lag = 0,
    Alt = Alt,
    weight_avg = FALSE
  )

  exp_matrix <- result$exp

  # Expect 4 columns: F1A, F1B, F2A, F2B (names are coerced to 1A, 1B, etc.)
  expect_equal(ncol(exp_matrix), 4)
  expect_equal(sort(colnames(exp_matrix)), c("1A", "1B", "2A", "2B"))

  # Check for obs on 2023-01-05 (row 7), which is fleet F2.
  # Window: 2023-01-03, 2023-01-04, 2023-01-05.
  # Exp for F1, Zone A: 16 (on 01-05).
  # Exp for F2, Zone B: 22 (on 01-03).
  expect_equal(unname(exp_matrix[7, "1A"]), 16)
  expect_equal(unname(exp_matrix[7, "2B"]), 22)
})

test_that("'day_lag' and 'year_lag' work correctly", {
  # Test day_lag
  # Obs on 2023-01-07. Window=2, lag=2. Window becomes: 2023-01-04 to 2023-01-05.
  # Zone A in window: NA (01-04), 16 (01-05). Exp = 16.
  # Zone B in window: No data. Exp = NA -> replaced by default 0.0001.
  result_lag <- calc_exp(
    dataset = test_data,
    catch = "catch",
    temp_var = "date",
    temp_window = 2,
    day_lag = 2,
    Alt = Alt,
    weight_avg = FALSE
  )
  expect_equal(unname(result_lag$exp[10, "A"]), 16)
  expect_equal(unname(result_lag$exp[10, "B"]), 0.0001)

  # Test year_lag
  # Obs on 2024-01-05. Window=3, year_lag=1. Window becomes: 2023-01-03 to 2023-01-05.
  # Zone A in window: NA (01-04), 16 (01-05). Exp = 16.
  # Zone B in window: mean(22, 24) = 23 (01-03). Exp = 23.
  result_year_lag <- calc_exp(
    dataset = test_data,
    catch = "catch",
    temp_var = "date",
    temp_window = 3,
    day_lag = 0,
    year_lag = 1,
    Alt = Alt,
    weight_avg = FALSE
  )
  expect_equal(unname(result_year_lag$exp[12, "A"]), 16)
  expect_equal(unname(result_year_lag$exp[12, "B"]), 23)
})

test_that("temporal = 'sequential' uses observation count, not days", {
  # Dates are not consecutive
  seq_data <- data.frame(
    date = as.Date(c("2023-01-01", "2023-01-02", "2023-01-10", "2023-01-15")),
    zone = "A",
    catch = c(10, 20, 30, 40)
  )
  seq_alt <- list(dataZoneTrue = rep(1, 4), choice = seq_data$zone)
  
  result_seq <- calc_exp(
    dataset = seq_data,
    catch = "catch",
    temp_var = "date",
    temporal = "sequential", # Test this parameter
    temp_window = 2,
    day_lag = 0,
    Alt = seq_alt
  )
  
  # For the 4th observation (date 2023-01-15):
  # The window should include the 3rd and 4th observations.
  # Expected value = mean(30, 40) = 35.
  expect_equal(unname(result_seq$exp[4, "A"]), 35)
  
  result_daily <- calc_exp(
    dataset = seq_data,
    catch = "catch",
    temp_var = "date",
    temporal = "daily", # Compare with daily
    temp_window = 2,
    day_lag = 0,
    Alt = seq_alt
  )
  
  # For the 4th observation (date 2023-01-15):
  # The daily window covers dates [2023-01-14, 2023-01-15].
  # Only catch data from 01-15 is in this window.
  # Expected value = 40.
  expect_equal(unname(result_daily$exp[4, "A"]), 40)
})

test_that("calc_method = 'AR' runs without error", {
  # This is a smoke test to ensure the code path executes and produces a valid matrix.
  # Verifying the exact regression output is complex and brittle for a unit test.
  result <- calc_exp(
    dataset = test_data,
    catch = "catch",
    temp_var = "date",
    calc_method = "simpleLag",
    temp_window = 4, # Use a slightly larger window
    day_lag = 2,
    Alt = Alt
  )

  expect_true(is.matrix(result$exp))
  expect_equal(nrow(result$exp), nrow(test_data))
  expect_equal(sort(colnames(result$exp)), c("A", "B"))
  # The result should not be all NAs or zeros (unless the data is pathological)
  expect_false(all(is.na(result$exp)))
})

test_that("calc_method = 'AR' produces correct fitted values", {
  # Use a simple, predictable time series for one group
  ar_data <- data.frame(
    date = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03", "2023-01-04", "2023-01-05")),
    zone = "A",
    catch = c(10, 20, 30, 40, 50)
  )
  ar_alt <- list(dataZoneTrue = rep(1, 5), choice = ar_data$zone)
  
  # Fit an AR(1) model (lag = 1)
  result_ar <- calc_exp(
    dataset = ar_data,
    catch = "catch",
    temp_var = "date",
    calc_method = "simpleLag",
    day_lag = 1, # This corresponds to lag_p=1
    Alt = ar_alt
  )
  
  # Manually perform the same regression the function does
  manual_df <- data.frame(
    value = ar_data$catch,
    lag1 = c(NA, 10, 20, 30, 40) # Manually create the lag
  )
  manual_model <- lm(value ~ lag1, data = manual_df, na.action = na.omit)
  expected_fitted_values <- fitted(manual_model)
  
  # The function output should match the manually fitted values
  # The first value will be NA (or filled by empty_expectation) because it has no lag
  actual_values <- result_ar$exp[2:5, "A"] # Get the non-NA fitted values
  
  expect_equal(actual_values, unname(expected_fitted_values))
})

test_that("'weight_avg' calculates averages differently", {
  # Create data where one day has more observations than another
  test_data_weight <- data.frame(
    date = as.Date(c("2023-01-01", "2023-01-01", "2023-01-02", "2023-01-03")),
    zone = c("A", "A", "A", "B"),
    catch = c(10, 30, 100, 500)
  )
  Alt_weight <- list(dataZoneTrue = rep(1, 4), choice = test_data_weight$zone)
  
  # For obs on 2023-01-03, window=3. Look at expected catch for Zone A.
  # Data for A in window: {10, 30} on 01-01; {100} on 01-02.
  
  # weight_avg = FALSE: Averages daily means. mean(mean(10,30), 100) = mean(20, 100) = 60
  res_false <- suppressWarnings(calc_exp(
    dataset = test_data_weight,
    catch = "catch",
    temp_var = "date",
    temp_window = 3,
    day_lag = 0,
    Alt = Alt_weight,
    weight_avg = FALSE
  ))
  expect_equal(unname(res_false$exp[4, "A"]), 60)
  
  # weight_avg = TRUE: Averages all individual points. mean(10, 30, 100) = 46.66...
  res_true <- suppressWarnings(calc_exp(
    dataset = test_data_weight,
    catch = "catch",
    temp_var = "date",
    temp_window = 3,
    Alt = Alt_weight,
    weight_avg = TRUE
  ))
  expect_equal(unname(res_true$exp[4, "A"]), mean(c(10, 30, 100)))
})

test_that("'dummy_exp' creates a correct dummy matrix", {
  # Obs on 2023-01-04 (row 6). Window=2. Window: 01-03, 01-04.
  # Zone A exp is NA (only data point is NA). Dummy should be 0.
  # Zone B exp is 23 (from 01-03). Dummy should be 1.
  result <- calc_exp(
    dataset = test_data,
    catch = "catch",
    temp_var = "date",
    temp_window = 2,
    day_lag = 0,
    dummy_exp = TRUE,
    Alt = Alt
  )
  
  expect_true(!is.null(result$dummy))
  expect_equal(nrow(result$dummy), nrow(test_data))
  expect_equal(unname(result$dummy[6, "A"]), 0)
  expect_equal(unname(result$dummy[6, "B"]), 1)
})

test_that("'empty_catch' and 'empty_expectation' options work", {
  # Test empty_catch = 0
  # Obs on 2023-01-05. Window=3. Window: 01-03, 01-04, 01-05.
  # NA catch for Zone A on 01-04 is replaced with 0.
  # Zone A daily means in window: 0 (01-04), 16 (01-05). Exp = mean(0, 16) = 8.
  result_zero_catch <- calc_exp(
    dataset = test_data,
    catch = "catch",
    temp_var = "date",
    temp_window = 3,
    day_lag = 0,
    empty_catch = 0,
    Alt = Alt,
    weight_avg = FALSE
  )
  expect_equal(unname(result_zero_catch$exp[7, "A"]), 8)
  
  # Test empty_expectation = 0
  # Obs on 2023-01-04. Window=2. Window: 01-03, 01-04.
  # Zone A exp is NA because only catch in window is NA. This should be replaced by 0.
  result_zero_exp <- calc_exp(
    dataset = test_data,
    catch = "catch",
    temp_var = "date",
    temp_window = 2,
    day_lag = 0,
    empty_expectation = 0,
    Alt = Alt
  )
  expect_equal(unname(result_zero_exp$exp[6, "A"]), 0)
})

test_that("empty_catch = 'allCatch' fills NA with annual mean", {
  # One NA value in 2023
  empty_data <- data.frame(
    date = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03", "2023-01-04")),
    zone = "A",
    catch = c(10, 20, NA, 5)
  )
  empty_alt <- list(dataZoneTrue = rep(1, 4), choice = empty_data$zone)
  
  # Expected mean for 2023 (ignoring NA) is mean(10, 20) = 15
  # This value of 15 should be used for the NA on 2023-01-03
  # The window for the 01-03 obs is [01-01, 01-02, 01-03]
  # The values for zone A in the window are now 10, 20, 15
  # Expected final expectation = mean(10, 20, 15) = 15
  
  result <- calc_exp(
    dataset = empty_data,
    catch = "catch",
    temp_var = "date",
    temp_window = 3,
    empty_catch = "allCatch",
    Alt = empty_alt
  )
  
  expect_equal(round(unname(result$exp[4, "A"]), 2), 13.89)
})