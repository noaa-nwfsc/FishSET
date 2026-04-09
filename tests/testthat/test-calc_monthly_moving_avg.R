# -------------------------------------------------------------------------------------------------
# File: test-calc_monthly_moving_avg.R
# Purpose: To provide unit tests for the calc_monthly_moving_avg() function.
# Description: This script uses the 'testthat' framework to validate the behavior of the
#              calc_monthly_moving_avg() wrapper function. It validates that the R wrapper
#              correctly prepares data for the C++ backend and handles post-processing tasks
#              like merging results, naming columns, and filling NA values.
#
# Scenarios tested:
#   - Basic moving average calculation crossing year boundaries.
#   - Correct application of 'year_lag' logic.
#   - Independent calculations across multiple grouping variables.
#   - Ability to customize the output column name via the 'name' argument.
#   - Handling of missing data gaps (NA results).
#   - Replacement of NA values using 'fill_empty_expectation'.
#
# Notes: This test script is designed to be run within the FishSET package structure using
#        devtools::test() or a similar tool. It assumes that 'calc_monthly_moving_avg'
#        is loaded as part of the package.
#
# -------------------------------------------------------------------------------------------------

# Test Data Setup ---------------------------------------------------------------------------------
# Data for basic boundary crossing and lag tests
basic_data <- data.frame(
  year = c(2023, 2023, 2023, 2024),
  month = c(10, 11, 12, 1),
  val = c(10, 20, 30, 40), # Increasing values
  group = "A"
)

# Data for grouping tests (Vessels and Zones)
group_data <- data.frame(
  year = c(2023, 2023, 2023, 2023),
  month = c(1, 2, 1, 2),
  vessel = c("V1", "V1", "V2", "V2"),
  zone = c("Z1", "Z1", "Z1", "Z1"),
  catch = c(10, 20, 100, 200) 
)

# Data for year lag tests
year_lag_data <- data.frame(
  year = c(2022, 2023),
  month = c(5, 5),
  val = c(50, 500),
  group = "A"
)

# Test calc across years --------------------------------------------------------------------------
test_that("calculates moving average correctly across year boundaries", {
  # Mock the database write so it doesn't fail on missing project
  local_mocked_bindings(load_grid = function(...) invisible(TRUE))
  
  # Scenario: Calculate average for Jan 2024.
  # Parameters: Window=3, Lag=1.
  # Logic: Target Jan 2024. Lag 1 starts at Dec 2023. Window covers [Oct, Nov, Dec].
  # Values: 10, 20, 30. Expected Mean: 20.
  result <- calc_monthly_moving_avg(
    project = "test_project",
    grid_name = "test_grid",
    append_to_existing = FALSE,
    df = basic_data,
    var_name = "avg_val",
    year_col = "year",
    month_col = "month",
    group_cols = "group",
    value_col = "val",
    window_size = 3,
    month_lag = 1
  )
  
  # Check result for Jan 2024 (4th row)
  expect_equal(result$avg_val[4], 20)
  
  # Check result for Nov 2023 (2nd row). 
  # Window [Oct]. (Nov is Lag 0, Oct is Lag 1). Mean: 10.
  expect_equal(result$avg_val[2], 10)
})


# Test year lag -----------------------------------------------------------------------------------
test_that("applies 'year_lag' correctly", {
  local_mocked_bindings(load_grid = function(...) invisible(TRUE))
  
  # Scenario: Predict May 2023 based on May 2022.
  # Parameters: Window=1, Lag=0, Year Lag=1.
  # Logic: Target May 2023 -> Shift 1 year back -> May 2022.
  # Window covers [May 2022]. Value: 50.
  result <- calc_monthly_moving_avg(
    project = "test_project",
    grid_name = "test_grid",
    append_to_existing = FALSE,
    df = year_lag_data,
    var_name = "hist_avg",
    year_col = "year",
    month_col = "month",
    group_cols = "group",
    value_col = "val",
    window_size = 1,
    month_lag = 0,
    year_lag = 1
  )

  # Check result for May 2023 (2nd row)
  expect_equal(result$hist_avg[2], 50)

  # Check result for May 2022 (1st row).
  # Shift back -> May 2021 (does not exist). Expected: NA.
  expect_true(is.na(result$hist_avg[1]))
})


# Test multiple groups ----------------------------------------------------------------------------
test_that("handles multiple grouping variables independently", {
  local_mocked_bindings(load_grid = function(...) invisible(TRUE))
  
  # Scenario: V1 and V2 operate in the same Zone Z1.
  # We group by BOTH vessel and zone to ensure V1's average doesn't include V2's data.
  # Test Target: Feb 2023. Lag=1, Window=1 (Look at Jan 2023).
  result <- calc_monthly_moving_avg(
    project = "test_project",
    grid_name = "test_grid",
    append_to_existing = FALSE,
    df = group_data,
    var_name = "mov_avg",
    year_col = "year",
    month_col = "month",
    group_cols = c("vessel", "zone"), # Multiple columns
    value_col = "catch",
    window_size = 1,
    month_lag = 1
  )

  # V1 Feb 2023 (Row 2). Previous month (Jan) catch: 10.
  expect_equal(result$mov_avg[2], 10)

  # V2 Feb 2023 (Row 4). Previous month (Jan) catch: 100.
  expect_equal(result$mov_avg[4], 100)
})
 

# Test column name --------------------------------------------------------------------------------
test_that("assigns the correct column name specified by 'name'", {
  local_mocked_bindings(load_grid = function(...) invisible(TRUE))
  
  custom_name <- "my_custom_metric"

  result <- calc_monthly_moving_avg(
    project = "test_project",
    grid_name = "test_grid",
    append_to_existing = FALSE,
    df = basic_data,
    var_name = custom_name,
    year_col = "year",
    month_col = "month",
    group_cols = "group",
    value_col = "val",
    window_size = 3
  )

  # Assert the column exists
  expect_true(custom_name %in% names(result))

  # Assert it is numeric
  expect_true(is.numeric(result[[custom_name]]))
})


# Test fill values --------------------------------------------------------------------------------
test_that("fill_empty_expectation replaces NA values correctly", {
  local_mocked_bindings(load_grid = function(...) invisible(TRUE))
  
  # Scenario: Oct 2023 (1st row of basic_data).
  # With Lag=1, it looks for Sept 2023, which does not exist.
  # Default result would be NA. We want it to be 0.

  result <- calc_monthly_moving_avg(
    project = "test_project",
    grid_name = "test_grid",
    append_to_existing = FALSE,
    df = basic_data,
    var_name = "filled_avg",
    year_col = "year",
    month_col = "month",
    group_cols = "group",
    value_col = "val",
    window_size = 3,
    month_lag = 1,
    fill_empty_expectation = 0
  )

  # Check Oct 2023 (Row 1). Should be 0 instead of NA.
  expect_equal(result$filled_avg[1], 0)

  # Ensure valid data (Jan 2024) is NOT overwritten. Should still be 20.
  expect_equal(result$filled_avg[4], 20)
})


# Test NA fill ------------------------------------------------------------------------------------
test_that("returns NA when fill_empty_expectation is not provided", {
  local_mocked_bindings(load_grid = function(...) invisible(TRUE))
  
  # Same scenario as above, but with default behavior.

  result <- calc_monthly_moving_avg(
    project = "test_project",
    grid_name = "test_grid",
    append_to_existing = FALSE,
    df = basic_data,
    var_name = "na_avg",
    year_col = "year",
    month_col = "month",
    group_cols = "group",
    value_col = "val",
    window_size = 3,
    month_lag = 1
    # fill_empty_expectation defaults to NA
  )

  expect_true(is.na(result$na_avg[1]))
})