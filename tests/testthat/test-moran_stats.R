# -------------------------------------------------------------------------------------------------
# File: test-moran_stats.R
# Purpose: Unit tests for the `moran_stats()` function in the FishSET package
# Description:
# This script tests the `moran_stats()` function to ensure it correctly calculates and
# visualizes global and local Moran's I statistics. It checks for successful execution,
# validates the output object types, and confirms the numerical result of the Moran's I test.
#
# Scenarios tested:
# - Successful execution with valid inputs.
# - Validation of the returned object types (list, htest, ggplot).
# - Confirmation of the calculated global Moran's I value against an expected result.
#
# Notes:
# - The script uses mock functions (`mock_log_call`)
#   to isolate the test environment and avoid external dependencies like database connections.
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

# Test moran_stats() ------------------------------------------------------------------------------
test_that("Test moran_stats() works", {
  # Define the base folder path to the test data directory
  # This folder should contain the subfolder named "s1" to pass the test
  test_folder <- testthat::test_path("testdata/FishSETFolder")
  
  # Override the folder path used by locproject() which is nested within predict_map()
  # This isolates the test env from the default paths
  old_option <- getOption("test_folder_path")
  options(test_folder_path = test_folder)
  
  # Call the moran_stats function
  result <- moran_stats(dat = "s1MainDataTable", 
                        var = "landed_thousands", 
                        dat_zone = "ZoneID", 
                        spat = "s1spatSpatTable", 
                        spat_zone = "TEN_ID", 
                        project = "s1")
  
  # Check that the function returns a list of length 3
  expect_true(is.list(result))
  expect_equal(length(result), 3)
  
  # Check the class of the returned objects
  # The first element should be the global Moran's I test result (class htest)
  expect_s3_class(result[[1]], "htest")
  
  # The second element should be the Moran plot (class ggplot)
  expect_s3_class(result[[2]], "ggplot")
  
  # The third element should be the LISA cluster map (class ggplot)
  expect_s3_class(result[[3]], "ggplot")
  
  # Results should match 
  expect_equal(round(unname(result[[1]]$estimate[1]),2), 0.18)
})


# Test moran_stats() errors -----------------------------------------------------------------------
test_that("Test moran_stats() throws errors appropriately", {
  # Define the base folder path to the test data directory
  # This folder should contain the subfolder named "s1" to pass the test
  test_folder <- testthat::test_path("testdata/FishSETFolder")
  
  # Override the folder path used by locproject() which is nested within predict_map()
  # This isolates the test env from the default paths
  old_option <- getOption("test_folder_path")
  options(test_folder_path = test_folder)
  
  # Call the moran_stats function
  expect_error(moran_stats(dat = "s1MainDataTable", 
                           var = "test_var", 
                           dat_zone = "ZoneID", 
                           spat = "s1spatSpatTable", 
                           spat_zone = "TEN_ID", 
                           project = "s1"))
  
  expect_error(moran_stats(dat = "s1MainDataTable", 
                           var = "GEARCODE", 
                           dat_zone = "ZoneID", 
                           spat = "s1spatSpatTable", 
                           spat_zone = "TEN_ID", 
                           project = "s1"))
  
  expect_error(moran_stats(dat = "s1MainDataTable", 
                           var = "landed_thousands", 
                           dat_zone = "fake_zoneID", 
                           spat = "s1spatSpatTable", 
                           spat_zone = "TEN_ID", 
                           project = "s1"))
  
  expect_error(moran_stats(dat = "s1MainDataTable", 
                           var = "landed_thousands", 
                           dat_zone = "ZoneID", 
                           spat = "s1spatSpatTable", 
                           spat_zone = "fake_zoneID", 
                           project = "s1"))
  
})