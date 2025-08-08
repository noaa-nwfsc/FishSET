# -------------------------------------------------------------------------------------------------
# File: test-ID_var.R
# Purpose: Unit tests for creating summary statistics data table
# Description:
#   This script tests the that `summary_stats()` function returns a data frame containing summary
#   statistics for all (or selected) variables in the primary data. The summary statistics that are
#   expected for numeric variables are: 'Min', 'Median', 'Mean', 'Max', 'Missing', 'Unique Obs.',
#   "No. 0's", while character variables are: "First" ,"NA's", "Unique Obs", "No. Empty" 
#
#
# Scenarios tested:
#   - Return summary stats for single numeric variable
#   - Return summary stats for single character variable
#   - Return summary stats for all variables 
#
# Notes:
#   - Uses test project: "s1"
#   - Assumes access to example model and spatial objects for reproducibility.
# -------------------------------------------------------------------------------------------------

# Test single numeric variable --------------------------------------------------------------------
test_that("summary_stats works correctly for a single numeric column", {
  # Define the base folder path to the test data directory
  # This folder should contain the subfolder named "s1" to pass the test
  test_folder <- testthat::test_path("testdata/FishSETFolder")
  
  # Override the folder path used by locproject() which is nested within 
  # This isolates the test env from the default paths
  old_option <- getOption("test_folder_path")
  options(test_folder_path = test_folder)
  
  result_main <- table_view("s1MainDataTable", "s1")
  
  result <- summary_stats(dat = result_main, project = "s1", x = "TRIPID", log_fun = FALSE)

  # Define the expected output
  expected <- c(
    "Min." = 6.00,
    "Median" = 3884.00,
    "Mean" = 3844.31,
    "Max." = 9458.00,
    "NA's" = 0.00,
    "Unique Obs" = 1992.00,
    "No. 0's" = 0.00
  )
  
  # Check that the result matches the expected output
  expect_equal(result, expected)
  # Check that the names of the vector are correct
  expect_named(result, names(expected))
  
})

# Test single character variable ------------------------------------------------------------------
test_that("summary_stats works correctly for a single character column", {
  # Define the base folder path to the test data directory
  # This folder should contain the subfolder named "s1" to pass the test
  test_folder <- testthat::test_path("testdata/FishSETFolder")
  
  # Override the folder path used by locproject() which is nested within 
  # This isolates the test env from the default paths
  old_option <- getOption("test_folder_path")
  options(test_folder_path = test_folder)
  
  result_main <- table_view("s1MainDataTable", "s1")
  
  # Run the function on the character column
  result <- summary_stats(result_main, project = "s1",
    x = "GEARCODE", log_fun = FALSE)
  
  # Define the expected output
  expected <- c(
    "First" = "DREDGE-SCALLOP",
    "NA's" = 0,
    "Unique Obs" = 3, 
    "No. Empty" = 0  
  )
  
  # Check that the result matches the expected output
  expect_equal(result, expected)
  # Check that the names of the vector are correct
  expect_named(result, names(expected))
})

# Test for all variables --------------------------------------------------------------------------
test_that("summary_stats works correctly for the entire data frame (no 'x' specified)", {
   # Define the base folder path to the test data directory
  # This folder should contain the subfolder named "s1" to pass the test
  test_folder <- testthat::test_path("testdata/FishSETFolder")
  
  # Override the folder path used by locproject() which is nested within 
  # This isolates the test env from the default paths
  old_option <- getOption("test_folder_path")
  options(test_folder_path = test_folder)
  
  result_main <- table_view("s1MainDataTable", "s1")
  
  # Run the function on the whole data frame
  result <- summary_stats(result_main, project = "s1", log_fun = FALSE)
  
  # Check the structure of the output
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 7) # 7 summary rows
  expect_equal(ncol(result), 20) # 3 columns in the original data
  
  # Check a few specific values to ensure correctness
  # Value for Min. of num_col
  expect_equal(result[[1]][[1]], "Min.   :   6  ") 
  expect_equal(result[[17]][[1]], "First: 2007-05-01") 
})