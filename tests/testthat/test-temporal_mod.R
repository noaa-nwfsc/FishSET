# -------------------------------------------------------------------------------------------------
# File: test-temporal_mod.R
# Purpose: Unit tests for transforming units of date variables
# Description:
#   This script tests the that `temporal_mod()` function creates a new temporal variable by
#    extracting temporal unit, such as year, month, or day from a date variable.
#   
# Scenarios tested:
#   - Tests no changes
#   - Predefined 'year' format
#   - Extracts 'hour'
#   - Extracts 'minute'
#   - Timezone change correctly
#   - Timezone and define_format are NULL 
#
# Notes:
#   - Uses test project: "s1"
#   - Assumes access to example model and spatial objects for reproducibility.
# -------------------------------------------------------------------------------------------------

# Create mock log_call() --------------------------------------------------------------------------
# Description: Override the FishSET::log_call() function to avoid errors with connecting when
#              running unit tests
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


# Tests no changes --------------------------------------------------------------------------------
test_that("change_class returns data frame and prints class table without changes", {
  # Define the base folder path to the test data directory
  # This folder should contain the subfolder named "s1" to pass the test
  test_folder <- testthat::test_path("testdata/FishSETFolder")
  
  # Override the folder path used by locproject() which is nested within 
  # This isolates the test env from the default paths
  old_option <- getOption("test_folder_path")
  options(test_folder_path = test_folder)
  
  result_main <- table_view("s1MainDataTable", "s1")
  
  new_name <- "CustomDate"
  
  result <- temporal_mod(result_main, "s1", "DATE_TRIP",
                         define_format = "%d/%m/%Y", name = new_name, log_fun = FALSE)
  
  expected_output <- c("01/05/2007", "01/05/2007", "01/05/2007")
  
  expect_true(new_name %in% names(result))
  expect_equal(head(result[[new_name]], 3), expected_output)
})

# predefined 'year' format ------------------------------------------------------------------------
test_that("temporal_mod handles predefined 'year' format correctly and default name", {
  
  # Define the base folder path to the test data directory
  # This folder should contain the subfolder named "s1" to pass the test
  test_folder <- testthat::test_path("testdata/FishSETFolder")
  
  # Override the folder path used by locproject() which is nested within 
  # This isolates the test env from the default paths
  old_option <- getOption("test_folder_path")
  options(test_folder_path = test_folder)
  
  result_main <- table_view("s1MainDataTable", "s1")
  
  result <- temporal_mod(result_main, "s1", "DATE_TRIP",
                         define_format = "year", name = NULL, log_fun = FALSE)
  
  new_name <- "temp_mod_year"
  expected_output <- c("2007", "2007", "2007")
  
  expect_true(new_name %in% names(result))
  expect_equal(head(result[[new_name]],3), expected_output)
})

# extracts 'hour' ---------------------------------------------------------------------------------
test_that("temporal_mod correctly extracts 'hour' using the corrected logic", {
  
  # Define the base folder path to the test data directory
  # This folder should contain the subfolder named "s1" to pass the test
  test_folder <- testthat::test_path("testdata/FishSETFolder")
  
  # Override the folder path used by locproject() which is nested within 
  # This isolates the test env from the default paths
  old_option <- getOption("test_folder_path")
  options(test_folder_path = test_folder)
  
  new_name <- "YMD_H"
  
  result_main <- table_view("s1MainDataTable", "s1")
  
  result <- temporal_mod(result_main, "s1", "DATE_TRIP",
                         define_format = "hour", name = new_name, log_fun = FALSE)
  
  expected_output <- c("2007/05/01 00", "2007/05/01 00", "2007/05/01 00")
  
  expect_true(new_name %in% names(result))
  expect_equal(head(result[[new_name]],3), expected_output)
})

# extracts 'minute' -------------------------------------------------------------------------------
test_that("temporal_mod correctly extracts 'minute' using the corrected logic", {
  
  # Define the base folder path to the test data directory
  # This folder should contain the subfolder named "s1" to pass the test
  test_folder <- testthat::test_path("testdata/FishSETFolder")
  
  # Override the folder path used by locproject() which is nested within 
  # This isolates the test env from the default paths
  old_option <- getOption("test_folder_path")
  options(test_folder_path = test_folder)
  
  format_choice <- "minute"
  new_name <- "YMD_HM"
  
  result_main <- table_view("s1MainDataTable", "s1")
  
  result <- temporal_mod(result_main, "s1", "DATE_TRIP",
                         define_format = format_choice, name = new_name, log_fun = FALSE)
  
  expected_output <- c("2007/05/01 00:00", "2007/05/01 00:00", "2007/05/01 00:00")
  
  expect_true(new_name %in% names(result))
  expect_equal(head(result[[new_name]],3), expected_output)
})

# timezone change correctly -----------------------------------------------------------------------
test_that("temporal_mod performs timezone change correctly without creating a new column", {
  
  # Define the base folder path to the test data directory
  # This folder should contain the subfolder named "s1" to pass the test
  test_folder <- testthat::test_path("testdata/FishSETFolder")
  
  # Override the folder path used by locproject() which is nested within 
  # This isolates the test env from the default paths
  old_option <- getOption("test_folder_path")
  options(test_folder_path = test_folder)
  
  # Input is "2023-01-15 10:30:00 UTC"
  new_timezone <- "America/New_York" # EST = UTC-5
  
  result_main <- table_view("s1MainDataTable", "s1")
  
  # When define_format is NULL, the original column is modified in place and returned.
  result <- temporal_mod(result_main, "s1", "DATE_TRIP",
                         define_format = NULL, timezone = new_timezone, log_fun = FALSE)
  
  # 1. Check that the column still exists and no new column was created.
  expect_true("DATE_TRIP" %in% names(result))
  expect_equal(names(result), names(result_main))
  
  # 2. Check that the POSIXct object's timezone attribute has changed.
  expect_equal(attr(result[["DATE_TRIP"]], "tzone"), new_timezone)
  
  # 3. Check the clock time is preserved (force_tz behavior).
  first_element_time <- format(result[["DATE_TRIP"]][1], format = "%Y-%m-%d %H:%M:%S")
  expect_equal(first_element_time, "2007-05-01 00:00:00")
})


# timezone and define_format are NULL ------------------------------------------------------------
test_that("temporal_mod returns original dataset if both timezone and define_format are NULL", {
  
  # Define the base folder path to the test data directory
  # This folder should contain the subfolder named "s1" to pass the test
  test_folder <- testthat::test_path("testdata/FishSETFolder")
  
  # Override the folder path used by locproject() which is nested within 
  # This isolates the test env from the default paths
  old_option <- getOption("test_folder_path")
  options(test_folder_path = test_folder)
  
  result_main <- table_view("s1MainDataTable", "s1")
  
  result <- temporal_mod(result_main, "s1", "DATE_TRIP",
                         define_format = NULL, timezone = NULL, log_fun = FALSE)
  
  # The result should be identical to the original mock data
  expect_equal(result, result_main)
})