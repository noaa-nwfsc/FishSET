# -------------------------------------------------------------------------------------------------
# File: test-change_class.R
# Purpose: Unit tests for changing variable class
# Description:
#   This script tests the that `change_class()` function returns a data frame containing all
#   variables in the primary data, the variable class, and the first value. The user can use this 
#   function update selected variables to a different class type (character, numeric, factor, date)
#   and have it saved to the FishSET database to be used throughout their project.
#   
# Scenarios tested:
#   - Returns data frame and prints class table without changes
#   - Changes a character to numeric
#   - Changes a numeric to a factor
#   - Changes a numeric to a character
#   - Changes a character string to a date
#   - Handles multiple class changes correctly
#
# Notes:
#   - Uses test project: "s1"
#   - Assumes access to example model and spatial objects for reproducibility.
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
  
  # Expect a warning because no class was changed
  expect_warning(
    result <- change_class(result_main, "s1"),
    "Variable class was not changed."
  )
  # The output should be identical to the input data frame when no changes are made
  expect_identical(result, NULL) # Function returns NULL when no class change
})

# Character to numeric ----------------------------------------------------------------------------
test_that("change_class correctly changes a character to numeric", {
   # Define the base folder path to the test data directory
  # This folder should contain the subfolder named "s1" to pass the test
  test_folder <- testthat::test_path("testdata/FishSETFolder")

  # Override the folder path used by locproject() which is nested within
  # This isolates the test env from the default paths
  old_option <- getOption("test_folder_path")
  options(test_folder_path = test_folder)

  result_main <- table_view("s1MainDataTable", "s1")

  result_main$DDLAT <-  as.character(as.numeric(result_main$DDLAT))

  # Expect a message indicating a successful class change
  expect_message(
    result <- change_class(result_main, "s1", x = "DDLAT", new_class = "numeric"),
    "Variable class changed."
  )
  # Check if the class of the 'TRIPID' column is now numeric
  expect_true(is.numeric(result$DDLAT))
})

# Numeric to factor -------------------------------------------------------------------------------
test_that("change_class correctly changes a numeric to a factor", {
  # Define the base folder path to the test data directory
  # This folder should contain the subfolder named "s1" to pass the test
  test_folder <- testthat::test_path("testdata/FishSETFolder")

  # Override the folder path used by locproject() which is nested within
  # This isolates the test env from the default paths
  old_option <- getOption("test_folder_path")
  options(test_folder_path = test_folder)

  result_main <- table_view("s1MainDataTable", "s1")

  expect_message(
    result <- change_class(result_main, "s1", x = "TRIPID", new_class = "factor"),
    "Variable class changed."
  )
  # Check if the class of the 'TRIPID' column is now a factor
  expect_s3_class(result$TRIPID, "factor")
})

# Numeric to character ----------------------------------------------------------------------------
test_that("change_class correctly changes a numeric to a character", {
   # Define the base folder path to the test data directory
  # This folder should contain the subfolder named "s1" to pass the test
  test_folder <- testthat::test_path("testdata/FishSETFolder")

  # Override the folder path used by locproject() which is nested within
  # This isolates the test env from the default paths
  old_option <- getOption("test_folder_path")
  options(test_folder_path = test_folder)

  result_main <- table_view("s1MainDataTable", "s1")

  expect_message(
    result <- change_class(result_main, "s1",  x = "TRIPID", new_class = "character"),
    "Variable class changed."
  )
  # Check if the class of the 'NUMERIC_COL' column is now character
  expect_type(result$TRIPID, "character")
})

# Character to date -------------------------------------------------------------------------------
test_that("change_class correctly changes a character string to a date", {
  # Mock the date_parser helper function if it's not available globally
  # Define the base folder path to the test data directory
  # This folder should contain the subfolder named "s1" to pass the test
  test_folder <- testthat::test_path("testdata/FishSETFolder")

  # Override the folder path used by locproject() which is nested within
  # This isolates the test env from the default paths
  old_option <- getOption("test_folder_path")
  options(test_folder_path = test_folder)

  result_main <- table_view("s1MainDataTable", "s1")

  result_main$test_date <- seq.Date(from = as.Date("2025-08-25"), by = "day",
    length.out = nrow(result_main))

  result_main$test_date <- format(result_main$test_date, "%Y-%m-%d")

  expect_message(
    result <- change_class(result_main, "s1", x = "test_date", new_class = "date"),
    "Variable class changed."
  )
  # Check if the class of the 'test_date' column is now Date
  expect_s3_class(result$test_date, "Date")
})

# Tests mulitple class changes --------------------------------------------------------------------
test_that("change_class handles multiple class changes correctly", {
   # Define the base folder path to the test data directory
  # This folder should contain the subfolder named "s1" to pass the test
  test_folder <- testthat::test_path("testdata/FishSETFolder")

  # Override the folder path used by locproject() which is nested within
  # This isolates the test env from the default paths
  old_option <- getOption("test_folder_path")
  options(test_folder_path = test_folder)

  result_main <- table_view("s1MainDataTable", "s1")

  expect_message(
    result <- change_class(
     result_main, "s1",
      x = c("ZoneID", "Plan Code"),
      new_class = c("character", "factor")),
    "Variable class changed."
  )
  # Verify the new classes of the changed columns
  expect_type(result$ZoneID, "character")
  expect_s3_class(result$`Plan Code`, "factor")
})




