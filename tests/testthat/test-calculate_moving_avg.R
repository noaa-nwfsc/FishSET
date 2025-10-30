# -------------------------------------------------------------------------------------------------
# File: test-calculate_moving_avg.R
# Purpose: To provide unit tests for the calculate_moving_avg() C++ function.
# Description: This script uses the 'testthat' framework to validate the behavior of the
#              calculate_moving_avg() function. It creates several simulated datasets to cover
#              various input scenarios and asserts that the function's output is correct.
#
# Scenarios tested:
#   - Correctness of a simple daily moving average.
#   - Proper application of 'lag' and 'year_lag'.
#   - Difference between weighted ('weighted' = TRUE) and unweighted daily averaging.
#   - Correctness of sequential averaging ('temporal' = FALSE) with irregular dates.
#   - Correct handling of multiple groups.
#   - Graceful handling of windows with no data.
#
# Notes: This test script is designed to be run within the FishSET package structure using
#        devtools::test() or a similar tool. It assumes that the 'calculate_moving_avg'
#        function (compiled from C++) is loaded as part of the package. It requires the
#        'testthat' package.
#
# -------------------------------------------------------------------------------------------------

# Test Data Setup ---------------------------------------------------------------------------------
# Base data for daily, lag, and year_lag tests
base_data <- data.frame(
  date = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03", "2023-01-04", "2024-01-03")),
  group = "A",
  value = c(10, 20, 30, 40, 500)
)
all_dates <- seq(from = min(base_data$date), to = max(base_data$date), by = "day")

# Data for testing weighted average
weighted_data <- data.frame(
  date = as.Date(c("2023-01-01", "2023-01-02", "2023-01-02", "2023-01-03")),
  group = "A",
  value = c(10, 20, 30, 40) # Two observations on Jan 2nd
)
all_dates_weighted <- seq(from = min(weighted_data$date), to = max(weighted_data$date), by = "day")

# Data for testing sequential average
sequential_data <- data.frame(
  date = as.Date(c("2023-01-01", "2023-01-05", "2023-01-10", "2023-01-15")),
  group = "A",
  value = c(10, 20, 30, 40) # Dates are not consecutive
)
all_dates_seq <- seq(from = min(sequential_data$date), to = max(sequential_data$date), by = "day")

# Data for testing multiple groups
multi_group_data <- data.frame(
  date = as.Date(c("2023-01-02", "2023-01-03", "2023-01-02", "2023-01-03")),
  group = c("A", "A", "B", "B"),
  value = c(20, 30, 200, 300)
)
all_dates_multi <- unique(multi_group_data$date)

# Unit Tests --------------------------------------------------------------------------------------

test_that("calculates a simple daily moving average correctly", {
  result <- calculate_moving_avg(
    unique_dates = all_dates,
    unique_groups = "A",
    obs_dates = base_data$date,
    obs_groups = base_data$group,
    obs_values = base_data$value,
    window_size = 3,
    lag = 1
  )
  
  # For date 2023-01-04, window is [01-01, 01-02, 01-03]. Lag=1 shifts it to [01-02, 01-03, 01-04] 
  # if lag were 0. 
  # With lag=1, the window for 2023-01-04 ends on 2023-01-03.
  # Window should cover: 2023-01-01, 2023-01-02, 2023-01-03. Values: 10, 20, 30. Mean = 20.
  # Let's check for 2023-01-05 (not in data). Window is [01-02, 01-03, 01-04]. Values: 20, 30, 40. 
  # Mean = 30.
  expect_equal(result[as.character(as.Date("2023-01-05")), "A"], 30)
})

test_that("applies year_lag correctly", {
  result <- calculate_moving_avg(
    unique_dates = all_dates,
    unique_groups = "A",
    obs_dates = base_data$date,
    obs_groups = base_data$group,
    obs_values = base_data$value,
    window_size = 2,
    lag = 0,
    year_lag = 1
  )
  
  # For date 2024-01-03, year_lag=1 makes the base date 2023-01-03.
  # Window size=2, lag=0 covers [2023-01-02, 2023-01-03].
  # Values are 20, 30. Expected mean = 25.
  expect_equal(result[as.character(as.Date("2024-01-03")), "A"], 25)
})

test_that("calculates weighted and unweighted averages differently", {
  # Unweighted: averages the daily means.
  # Window for 2023-01-03: [2023-01-02, 2023-01-03].
  # Mean for 01-02 is mean(20, 30) = 25. Mean for 01-03 is 40.
  # Expected: mean(25, 40) = 32.5
  res_unweighted <- calculate_moving_avg(
    unique_dates = all_dates_weighted, 
    unique_groups = "A", 
    obs_dates = weighted_data$date, 
    obs_groups = weighted_data$group, 
    obs_values = weighted_data$value, 
    window_size = 2, 
    lag = 0, 
    year_lag = 0, 
    temporal = TRUE, 
    weighted = FALSE)
  
  expect_equal(res_unweighted[as.character(as.Date("2023-01-03")), "A"], 32.5)
  
  # Weighted: averages all points in the window.
  # Points in window: 20, 30, 40.
  # Expected: mean(20, 30, 40) = 30.
  # C++ logic: (25*2 + 40*1) / (2+1) = 90/3 = 30
  res_weighted <- calculate_moving_avg(
    unique_dates = all_dates_weighted, 
    unique_groups = "A", 
    obs_dates = weighted_data$date, 
    obs_groups = weighted_data$group, 
    obs_values = weighted_data$value, 
    window_size = 2, 
    lag = 0, 
    year_lag = 0, 
    temporal = TRUE, 
    weighted = TRUE)
  
  expect_equal(res_weighted[as.character(as.Date("2023-01-03")), "A"], 30)
})

test_that("calculates sequential average correctly", {
  result <- calculate_moving_avg(
    unique_dates = all_dates_seq,
    unique_groups = "A",
    obs_dates = sequential_data$date,
    obs_groups = sequential_data$group,
    obs_values = sequential_data$value,
    window_size = 2,
    lag = 1,
    year_lag = 0,
    temporal = FALSE # Sequential
  )
  
  # For the 4th observation (2023-01-15), the window should be the 2nd and 3rd obs.
  # Values: 20, 30. Expected mean = 25.
  expect_equal(result[as.character(as.Date("2023-01-15")), "A"], 25)
})

test_that("handles multiple groups independently", {
  result <- calculate_moving_avg(
    all_dates_multi, c("A", "B"), 
    multi_group_data$date, 
    multi_group_data$group, 
    multi_group_data$value, 
    2, 
    0)
  
  # Check value for 2023-01-03
  # Group A: window [01-02, 01-03]. Values: 20, 30. Mean = 25
  # Group B: window [01-02, 01-03]. Values: 200, 300. Mean = 250
  expect_equal(result[as.character(as.Date("2023-01-03")), "A"], 25)
  expect_equal(result[as.character(as.Date("2023-01-03")), "B"], 250)
})

test_that("returns NA for windows with no data", {
  result <- calculate_moving_avg(
    unique_dates = all_dates,
    unique_groups = "A",
    obs_dates = base_data$date,
    obs_groups = base_data$group,
    obs_values = base_data$value,
    window_size = 2,
    lag = 5 # This lag shifts the window well before any data exists
  )
  
  # For any date in our series, this lag should result in an empty window
  expect_true(is.na(result[as.character(as.Date("2023-01-04")), "A"]))
})
