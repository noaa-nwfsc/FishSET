# -------------------------------------------------------------------------------------------------
# File: test-set_quants.R
# Purpose: To conduct unit tests for the set_quants() function.
# Description: This script uses the 'testthat' package to verify that the
# Scenarios tested:
#   - 
#
# Notes:
#   - Several internal functions are mocked to isolate the lagging logic of lag_zone()
#     from external dependencies like file I/O or database connections.
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


# --- Test Cases ---

test_that("set_quants creates quartiles (0.25) correctly", {
   # Define the base folder path to the test data directory
  # This folder should contain the subfolder named "s1" to pass the test
  test_folder <- testthat::test_path("testdata/FishSETFolder")
  
  # Override the folder path used by locproject() which is nested within projects()
  # This isolates the test env from the default paths
  # withr::local_options(list(test_folder_path = test_folder))
  old_option <- getOption("test_folder_path")
  options(test_folder_path = test_folder)
  
  test_df <- table_view("s1MainDataTable", "s1")
  
  result <- set_quants(test_df, "test", "TRIP_LENGTH", quant.cat = 0.25, name = "quartiles")
  
  # Check column properties
  expect_true("quartiles" %in% names(result))
  expect_equal(ncol(result), 21)
  expect_true(is.integer(result$quartiles))
  expect_equal(length(unique(result$quartiles)), 4)
  expect_equal(head(result$quartiles, 3), c(4,3,4))
})

test_that("set_quants creates tertiles (0.33) correctly", {
  # Define the base folder path to the test data directory
  # This folder should contain the subfolder named "s1" to pass the test
  test_folder <- testthat::test_path("testdata/FishSETFolder")
  
  # Override the folder path used by locproject() which is nested within projects()
  # This isolates the test env from the default paths
  # withr::local_options(list(test_folder_path = test_folder))
  old_option <- getOption("test_folder_path")
  options(test_folder_path = test_folder)
  
  test_df <- table_view("s1MainDataTable", "s1")
  
  result <- set_quants(test_df, "test", "TRIP_LENGTH", quant.cat = 0.33, name = "tertiles")
  
  # Check column properties
  expect_true("tertiles" %in% names(result))
  expect_equal(length(unique(result$tertiles)), 3)
  expect_equal(head(result$tertiles, 3), c(3,3,3))

})

test_that("custom.quant overrides quant.cat", {
  # Define the base folder path to the test data directory
  # This folder should contain the subfolder named "s1" to pass the test
  test_folder <- testthat::test_path("testdata/FishSETFolder")
  
  # Override the folder path used by locproject() which is nested within projects()
  # This isolates the test env from the default paths
  # withr::local_options(list(test_folder_path = test_folder))
  old_option <- getOption("test_folder_path")
  options(test_folder_path = test_folder)
  

  test_df <- table_view("s1MainDataTable", "s1")
  result <- set_quants(test_df, "test", "TRIP_LENGTH", 
                       quant.cat = 0.25, # This should be ignored
                       custom.quant = c(0, 0.5, 1), 
                       name = "median_split")
  
  # Check column properties
  expect_true("median_split" %in% names(result))
  expect_equal(length(unique(result$median_split)), 2)
  expect_equal(head(result$median_split, 3), c(2,2,2))

})

test_that("function handles non-numeric variable", {
  # Define the base folder path to the test data directory
  # This folder should contain the subfolder named "s1" to pass the test
  test_folder <- testthat::test_path("testdata/FishSETFolder")
  
  # Override the folder path used by locproject() which is nested within projects()
  # This isolates the test env from the default paths
  # withr::local_options(list(test_folder_path = test_folder))
  old_option <- getOption("test_folder_path")
  options(test_folder_path = test_folder)
  
  test_df <- table_view("s1MainDataTable", "s1")  
  # Expect a warning
  expect_warning(
    result <- set_quants(test_df, "test", "GEARCODE", quant.cat = 0.25),
    "Variable must be numeric. Function not run."
  )
  
  # Expect NULL return (based on current code's control flow)
  expect_null(result)
})

test_that("default column name is used", {
  test_df <- data.frame(my_var = 1:10)
  # Call without 'name' argument
  result <- set_quants(test_df, "test_proj", "my_var", quant.cat = 0.2)
  
  # Check for default name "set_quants"
  expect_true("set_quants" %in% names(result))
})


test_that("Test quant.cat = 0.2 (5 groups)", {
  test_df <- data.frame(my_var = 1:100)
  result <- set_quants(test_df, "test_proj", "my_var", quant.cat = 0.2, name = "quintiles")
  
  # quantiles for 1:100 are c(1, 20.8, 40.6, 60.4, 80.2, 100)
  expect_equal(length(unique(result$quintiles)), 5)
  expect_equal(result$quintiles[20], 1L)
  expect_equal(result$quintiles[21], 2L)
  expect_equal(result$quintiles[80], 4L)
  expect_equal(result$quintiles[81], 5L)
})