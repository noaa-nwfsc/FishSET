# -------------------------------------------------------------------------------------------------
# File: test-outlier_check.R
# Purpose: 
# Description:
#
# Scenarios tested:
#
# Notes:
#
# -------------------------------------------------------------------------------------------------

# Create mock log_call() --------------------------------------------------------------------------
# Description: Override the FishSET::log_call() function to avoid errors with connecting when
#              running spat_qaqc() through unit tests
# Get a reference to the original log_call function from its namespace.
# We'll use this to restore it later.
original_log_call <- get("log_call", envir = as.environment("package:FishSET"))

# This is the fake function that will do nothing.
mock_log_call <- function(...) {
  # This function does nothing and returns invisibly.
  invisible(NULL)
}

# On exit, restore the original function to the namespace. This is crucial
# for clean testing and prevents side effects.
on.exit({
  assignInNamespace("log_call", original_log_call, ns = "FishSET")
})

# Now, replace the original log_call with our mock function.
assignInNamespace("log_call", mock_log_call, ns = "FishSET")

# Test for the outlier_table() function -----------------------------------------------------------
test_that("test outlier_table() works", {
  # Define the base folder path to the test data directory
  # This folder should contain the subfolder named "s1" to pass the test
  test_folder <- testthat::test_path("testdata/FishSETFolder")
  
  # Override the folder path used by locproject() which is nested within projects()
  # This isolates the test env from the default paths
  # withr::local_options(list(test_folder_path = test_folder))
  old_option <- getOption("test_folder_path")
  options(test_folder_path = test_folder)
  
  # Run the function
  result <- outlier_table(dat = "s1MainDataTable",
                          project = "s1",
                          x = "LANDED_OBSCURED")
  
  # Check that the result is a data frame
  expect_s3_class(result, "data.frame")
  
  # Check that the data frame has the correct columns
  expect_named(result, c("Vector", "outlier_check", "N", "mean", "median",
                         "SD", "min", "max", "NAs", "skew"))
  
  # Check that the statistics are calculated correctly
  expect_equal(result$mean[2], 13824.95, tolerance = 0.01)
  expect_equal(result$median[2], 16127.70, tolerance = 0.01)
  expect_equal(result$SD[2], 5949.98, tolerance = 0.01)
  expect_equal(result$min[2], 1435.44, tolerance = 0.01)
  expect_equal(result$max[2], 24994.48, tolerance = 0.01)
  expect_equal(result$NAs[2], 0, tolerance = 0.01)
  expect_equal(result$skew[2], -0.45, tolerance = 0.01)
})

# Test that outlier_table() function handles errors -----------------------------------------------
test_that("test outlier_table() handles errors", {
  # Define the base folder path to the test data directory
  # This folder should contain the subfolder named "s1" to pass the test
  test_folder <- testthat::test_path("testdata/FishSETFolder")
  
  # Override the folder path used by locproject() which is nested within projects()
  # This isolates the test env from the default paths
  # withr::local_options(list(test_folder_path = test_folder))
  old_option <- getOption("test_folder_path")
  options(test_folder_path = test_folder)
  
  # Run the function
  expect_error(outlier_table(dat = "s1MainDataTable",
                             project = "s1",
                             x = "GEARCODE"))
})
