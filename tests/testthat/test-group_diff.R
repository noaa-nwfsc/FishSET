# -------------------------------------------------------------------------------------------------
# File: test-group_diff.R
# Purpose: Unit tests for the `group_diff()` function 
# Description:
#   This script tests the that `group_diff()` function creates a within-group lagged difference 
#   variable. A total value variable is also created and can be saved as well.
#   
# Scenarios tested:
#   - Default parameters
#   - Custom name parameter works correctly
#   - Respects lag parameter
#   - Drop total column correctly 
#   - Testing multiple grouping variables 
#   - Ensures date variable is a Date
#   
# Notes:
#   - Created sample data for simplicity 
#   - The script uses mock functions (`mock_log_call`, `mock_data_pull`, `mock_parse_data_name`)
#     to isolate the test environment and avoid external dependencies like database connections.
# -------------------------------------------------------------------------------------------------

# A data frame for testing
test_data <- data.frame(
  # Grouping variable
  group_var = c("A", "A", "A", "A", "A", "B", "B", "B"),
  # Sorting variable (already a Date)
  sort_var = as.Date(
    c("2023-01-01", "2023-01-01", "2023-01-02", "2023-01-03", "2023-01-04", "2023-01-01",
      "2023-01-02", "2023-01-03")
  ),
  # Value to be summed and diffed
  value_var = c(10, 5, 20, 30, 15, 100, 200, 50),
  # An extra column to ensure it's preserved
  other_col = 1:8
)

# Data for testing multiple group columns
test_data_multi <- data.frame(
  group_1 = c("A", "A", "A", "A", "B", "B"),
  group_2 = c("X", "X", "Y", "Y", "X", "X"),
  sort_var = as.Date(
    c("2023-01-01", "2023-01-02", "2023-01-01", "2023-01-02", "2023-01-01", "2023-01-02")
  ),
  value_var = c(10, 20, 30, 40, 50, 60)
)

# Define the mock functions ----------------------------------------------------------------------
mock_log_call <- function(...) invisible(NULL)
mock_data_pull <- function(dat, project) list(dataset = dat)
mock_parse_data_name <- function(dat, type, project) dat

# Save the original functions from the package namespace
original_log_call <- get("log_call", envir = as.environment("package:FishSET"))
original_data_pull <- get("data_pull", envir = as.environment("package:FishSET"))
original_parse_data_name <- get("parse_data_name", envir = as.environment("package:FishSET"))

# Schedule the restoration of all original functions.
on.exit({
  assignInNamespace("log_call", original_log_call, ns = "FishSET")
  assignInNamespace("data_pull", original_data_pull, ns = "FishSET")
  assignInNamespace("parse_data_name", original_parse_data_name, ns = "FishSET")
})

# Overwrite the real functions with our mocks
assignInNamespace("log_call", mock_log_call, ns = "FishSET")
assignInNamespace("data_pull", mock_data_pull, ns = "FishSET")
assignInNamespace("parse_data_name", mock_parse_data_name, ns = "FishSET")

# Default parameters ------------------------------------------------------------------------------
test_that("group_diff works with default parameters (lag=1)", {
  
  # We use data with correct dates, so date_parser won't be called
  result <- group_diff(
    dat = test_data,
    project = "test_proj",
    group = "group_var",
    sort_by = "sort_var",
    value = "value_var"
  )
  
  # Manually calculated expected values
  # Group A totals: 15, 20, 30, 15
  # Group B totals: 100, 200, 50
  # Diffs A: 0, (20-15)=5, (30-20)=10, (15-30)=-15
  # Diffs B: 0, (200-100)=100, (50-200)=-150
  expected_totals <- c(15, 15, 20, 30, 15, 100, 200, 50)
  expected_diffs <- c(0, 0, 5, 10, -15, 0, 100, -150)
  
  # Check: New columns exist
  expect_true("group_total" %in% names(result))
  expect_true("group_diff" %in% names(result))
  
  # Check: Values are correct
  expect_equal(result$group_total, expected_totals)
  expect_equal(result$group_diff, expected_diffs)
  
  # Check: Original columns are preserved
  expect_equal(result$other_col, test_data$other_col)
})

# Custom name parameter works correctly -----------------------------------------------------------
test_that("group_diff respects custom 'name' parameter", {
  
  result <- group_diff(
    dat = test_data,
    project = "test_proj",
    group = "group_var",
    sort_by = "sort_var",
    value = "value_var",
    name = "my_custom_diff"
  )
  
  # Check: Custom name is used and default is not
  expect_true("my_custom_diff" %in% names(result))
  expect_false("group_diff" %in% names(result))
  
  # Check: Values are still correct
  expected_diffs <- c(0, 0, 5, 10, -15, 0, 100, -150)
  expect_equal(result$my_custom_diff, expected_diffs)
})

# Respects lag parameter --------------------------------------------------------------------------
test_that("group_diff respects 'lag' parameter (lag=2)", {
  
  result <- group_diff(
    dat = test_data,
    project = "test_proj",
    group = "group_var",
    sort_by = "sort_var",
    value = "value_var",
    lag = 2
  )
  
  # Manually calculated expected values for lag = 2
  # Group A totals: 15, 20, 30, 15
  # Group B totals: 100, 200, 50
  # Diffs A: 0, 0, (30-15)=15, (15-20)=-5
  # Diffs B: 0, 0, (50-100)=-50
  expected_diffs <- c(0, 0, 0, 15, -5, 0, 0, -50)
  
  expect_equal(result$group_diff, expected_diffs)
})

# Drop total column correctly ---------------------------------------------------------------------
test_that("group_diff respects 'drop_total_col = TRUE'", {
  
  result <- group_diff(
    dat = test_data,
    project = "test_proj",
    group = "group_var",
    sort_by = "sort_var",
    value = "value_var",
    drop_total_col = TRUE
  )
  
  # Check: 'group_total' is gone, 'group_diff' remains
  expect_false("group_total" %in% names(result))
  expect_true("group_diff" %in% names(result))
})

# Testing multiple grouping variables --------------------------------------------------------------
test_that("group_diff works with multiple grouping variables", {
  
  result <- group_diff(
    dat = test_data_multi,
    project = "test_proj",
    group = c("group_1", "group_2"), # <-- Vector of group names
    sort_by = "sort_var",
    value = "value_var"
  )
  
  # Manually calculated expected values
  # Group A-X totals: 10, 20. Diffs: 0, 10
  # Group A-Y totals: 30, 40. Diffs: 0, 10
  # Group B-X totals: 50, 60. Diffs: 0, 10
  expected_totals <- c(10, 20, 30, 40, 50, 60)
  expected_diffs <- c(0, 10, 0, 10, 0, 10)
  
  expect_equal(result$group_total, expected_totals)
  expect_equal(result$group_diff, expected_diffs)
})

# Ensures date variable is a Date ----------------------------------------------------------------
test_that("group_diff calls date_parser if sort_by is not a Date", {
  # Create test data with a character date
  char_date_data <- test_data
  char_date_data$sort_var <- as.character(char_date_data$sort_var)
  
  # Check class before running
  expect_equal(class(char_date_data$sort_var), "character")
  
  result <- group_diff(
    dat = char_date_data,
    project = "test_proj",
    group = "group_var",
    sort_by = "sort_var",
    value = "value_var"
  )
  
  # Test that the logic is still correct (implying parsing worked)
  expected_diffs <- c(0, 0, 5, 10, -15, 0, 100, -150)
  expect_equal(result$group_diff, expected_diffs)
})
