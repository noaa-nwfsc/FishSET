# -------------------------------------------------------------------------------------------------
# File: test-qaqc_helper.R
# Purpose: Unit tests for identifying NAs, NaN, Inf values
# Description:
#   This script tests the that `qaqc_helper()` function returns a logical list indicating
#   whether or not NAs, NaNs, or Inf values are found. It can also return just a list of the 
#   variable names that contain these values. For special cases, the user can define a custom
#   function to identify specific values.
#   
# Scenarios tested:
#   - Identifies NA values 
#   - Identifies NaN values
#   - Identifies Inf values
#   - Custom function
#   - Invalid function input
#   - Empty data frame 
#
# Notes:
#   - Created sample data for simplicity 
# -------------------------------------------------------------------------------------------------

 # Create a sample data frame for testing
test_data <- data.frame(
  a = c(1, 2, NA, 4),
  b = c(5, NaN, 7, 8),
  c = c(9, 10, 11, Inf),
  d = c("x", "y", "z", "w"),
  e = c(TRUE, FALSE, TRUE, FALSE),
  f = c(NA, NA, NA, NA)
)

# Identifies NA values ----------------------------------------------------------------------------
  test_that("qaqc_helper correctly identifies NA values", {
  # Test with output = "logical"
  expect_equal(qaqc_helper(test_data, "NA"), 
               c(a = TRUE, b = FALSE, c = FALSE, d = FALSE, e = FALSE, f = TRUE))
               
  # Test with output = "names"
  expect_equal(qaqc_helper(test_data, "NA", "names"), c("a", "f"))
  
  # Test with a data frame that has no NAs
  no_na_data <- data.frame(x = 1:4, y = 5:8)
  expect_equal(qaqc_helper(no_na_data, "NA", "names"), character(0))
})

# Identifies NaN values ---------------------------------------------------------------------------
test_that("qaqc_helper correctly identifies NaN values", {
  # Test with output = "logical"
  expect_equal(qaqc_helper(test_data, "NaN"),
               c(a = FALSE, b = TRUE, c = FALSE, d = FALSE, e = FALSE, f = FALSE))

  # Test with output = "names"
  expect_equal(qaqc_helper(test_data, "NaN", "names"), "b")

  # Test with a data frame that has no NaNs
  no_nan_data <- data.frame(x = 1:4, y = 5:8)
  expect_equal(qaqc_helper(no_nan_data, "NaN", "names"), character(0))
})

# Identifies Inf values ---------------------------------------------------------------------------
test_that("qaqc_helper correctly identifies Inf values", {
  # Test with output = "logical"
  expect_equal(qaqc_helper(test_data, "Inf"),
               c(a = FALSE, b = FALSE, c = TRUE, d = FALSE, e = FALSE, f = FALSE))

  # Test with output = "names"
  expect_equal(qaqc_helper(test_data, "Inf", "names"), "c")

  # Test with a data frame that has no Infs
  no_inf_data <- data.frame(x = 1:4, y = 5:8)
  expect_equal(qaqc_helper(no_inf_data, "Inf", "names"), character(0))
})

# Custom function ---------------------------------------------------------------------------------
test_that("qaqc_helper works with a custom function", {
  # Custom function to check if a column is of character type
  is_char_fun <- function(x) is.character(x)

  # Test with output = "logical"
  expect_equal(qaqc_helper(test_data, is_char_fun),
               c(a = FALSE, b = FALSE, c = FALSE, d = TRUE, e = FALSE, f = FALSE))

  # Test with output = "names"
  expect_equal(qaqc_helper(test_data, is_char_fun, "names"), "d")

  # Custom function to check if all values are NA
  all_na_fun <- function(x) all(is.na(x))
  expect_equal(qaqc_helper(test_data, all_na_fun, "names"), "f")
})

# Invalid function input --------------------------------------------------------------------------

test_that("qaqc_helper handles invalid function input", {
  invalid_fun <- NULL

  # Test with an invalid string for the function
  expect_warning(qaqc_helper(test_data, invalid_fun),
                 "Invalid function entered into qaqc_helper()")

  # Test with a non-function, non-string input
  expect_warning(qaqc_helper(test_data, 123),
                 "Invalid function entered into qaqc_helper()")
})

# Empty data frame --------------------------------------------------------------------------------

test_that("qaqc_helper works with an empty data frame", {
  empty_df <- data.frame()

  # Test with "NA" and output = "logical"
  # expect_equal(qaqc_helper(empty_df, "NA"), logical(0))

  # Test with "NA" and output = "names"
  expect_equal(qaqc_helper(empty_df, "NA", "names"), character(0))
})

