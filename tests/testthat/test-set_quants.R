# -------------------------------------------------------------------------------------------------
# File: test-set_quants.R
# Purpose: To conduct unit tests for the set_quants() function.
# Description: This script uses the 'testthat' package to verify that the `set_quants()` function 
#   creates quantile categories based on the numeric variable given. The user can use the default 
#   quantiles provided or create custom categories. 
# 
# Scenarios tested:
#   - Creates quartiles
#   - Creates tertiles
#   - A custom quantile overrides quant_cat
#   - Tests non-numeric variable error 
#   - Checks default column name is used correctly
#   - Tests quintiles
#
# Notes:
#   - The script uses mock functions (`mock_log_call`) and dummy data sets to isolate the test
#     environment and avoid external dependencies like database connections
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

# creates quartiles -------------------------------------------------------------------------------
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
  
  result <- set_quants(test_df, "test", "TRIP_LENGTH", quant_cat = 0.25, name = "quartiles")
  
  # Check column properties
  expect_true("quartiles" %in% names(result))
  expect_equal(ncol(result), 21)
  expect_true(is.numeric(result$quartiles))
  expect_equal(length(unique(result$quartiles)), 4)
  expect_equal(head(result$quartiles, 3), c(1,.75,1))
})

# creates tertiles --------------------------------------------------------------------------------
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
  
  result <- set_quants(test_df, "test", "TRIP_LENGTH", quant_cat = 0.33, name = "tertiles")
  
  # Check column properties
  expect_true("tertiles" %in% names(result))
  expect_equal(length(unique(result$tertiles)), 3)
  expect_equal(head(result$tertiles, 3), c(1,1,1))

})

# custom quantiles overrides quant_cat ------------------------------------------------------------
test_that("custom_quant overrides quant_cat", {
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
                       quant_cat = 0.25, # This should be ignored
                       custom_quant = c(0, 0.5, 1), 
                       name = "median_split")
  
  # Check column properties
  expect_true("median_split" %in% names(result))
  expect_equal(length(unique(result$median_split)), 2)
  expect_equal(head(result$median_split, 4), c(1,1,1, .5))

})

# Tests non-numeric variable error ----------------------------------------------------------------
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
  # Expect error
  expect_error(
    result <- set_quants(test_df, "test", "GEARCODE", quant_cat = 0.25),
    "Variable must be numeric. Function not run."
  )
})

# Checks default column name is used correctly ----------------------------------------------------
test_that("default column name is used", {
  test_df <- data.frame(my_var = 1:10)
  # Call without 'name' argument
  result <- set_quants(test_df, "test_proj", "my_var", quant_cat = 0.2)
  
  # Check for default name "set_quants"
  expect_true("set_quants" %in% names(result))
})

# Tests quintiles ---------------------------------------------------------------------------------
test_that("Test quant_cat = 0.2 (5 groups)", {
  test_df <- data.frame(my_var = 1:100)
  result <- set_quants(test_df, "test_proj", "my_var", quant_cat = 0.2, name = "quintiles")
  
  # quantiles for 1:100 are c(1, 20.8, 40.6, 60.4, 80.2, 100)
  expect_equal(length(unique(result$quintiles)), 5)
  expect_equal(result$quintiles[20], 0.2)
  expect_equal(result$quintiles[21], 0.4)
  expect_equal(result$quintiles[41], 0.6)
  expect_equal(result$quintiles[61], 0.8)
  expect_equal(result$quintiles[81], 1)
})
