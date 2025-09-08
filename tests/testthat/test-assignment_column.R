# -------------------------------------------------------------------------------------------------
# File: test-assignment_column.R
# Purpose: To conduct unit tests for the assignment_column() function.
# Description: This script uses the 'testthat' package to verify the core functionality and
#   error-handling capabilities of the assignment_column() function. It ensures that 
#   the function correctly assigns spatial zones and fails gracefully when provided 
#   with invalid inputs. To create a controlled and isolated testing environment, 
#   the script mocks the log_call() function and uses a dedicated test data directory.
#   
# Scenarios tested:
#   - A successful run where points from a test dataset are assigned to the correct zones.
#   - Error handling for invalid coordinate inputs, including out-of-bounds longitude,
#     out-of-bounds latitude, and NA coordinate values.
#
# Notes:
#   - The log_call() function is mocked to prevent the test from attempting to write 
#     log files, which could fail or cause side effects.
#   - A temporary folder path is set to ensure the function reads from a specific 
#     'testdata' directory, making the tests reproducible and independent of the
#     user's local environment.
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

# Test assignment_column() ------------------------------------------------------------------------
test_that("test assignment_column() works", {
  # Define the base folder path to the test data directory
  # This folder should contain the subfolder named "s1" to pass the test
  test_folder <- testthat::test_path("testdata/FishSETFolder")
  
  # Override the folder path used by locproject() which is nested within projects()
  # This isolates the test env from the default paths
  # withr::local_options(list(test_folder_path = test_folder))
  old_option <- getOption("test_folder_path")
  options(test_folder_path = test_folder)
  
  # Run assignment column function
  result <- suppressWarnings(
    assignment_column(
      dat = "s1MainDataTable",
      project = "s1",
      spat = "s1spatSpatTable",
      lon.dat = "DDLON",
      lat.dat = "DDLAT",
      cat = "TEN_ID",
      name = "test_zoneID"
    )
  )
  
  # Expectations
  expect_true("test_zoneID" %in% names(result))
  expect_true(all(head(result$ZoneID) == head(result$test_zoneID)))
})

# Test assignment_column() handles errors ---------------------------------------------------------
test_that("function stops with invalid coordinate values", {
  # Define the base folder path to the test data directory
  # This folder should contain the subfolder named "s1" to pass the test
  test_folder <- testthat::test_path("testdata/FishSETFolder")
  
  # Override the folder path used by locproject() which is nested within projects()
  # This isolates the test env from the default paths
  # withr::local_options(list(test_folder_path = test_folder))
  old_option <- getOption("test_folder_path")
  options(test_folder_path = test_folder)
  
  # Mock spatial data
  poly_a <- st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0))))
  spat_data <- st_sf(zone_id = "A", geometry = st_sfc(poly_a), crs = 4326)
  
  # Mock point data with bad values
  point_data_bad_lon <- data.frame(id = 1, lon = 200, lat = 50)
  point_data_bad_lat <- data.frame(id = 1, lon = 50, lat = 100)
  point_data_na <- data.frame(id = 1, lon = 50, lat = NA)
  
  # Assert error expectations
  expect_error(
    assignment_column(
      dat = point_data_bad_lon, 
      project = "s1", 
      spat = spat_data, 
      lon.dat = "lon", 
      lat.dat = "lat", 
      cat = "zone_id", 
      log.fun = FALSE
    )
  )
  
  expect_error(
    assignment_column(
      dat = point_data_bad_lat, 
      project = "s1", 
      spat = spat_data,
      lon.dat = "lon", 
      lat.dat = "lat", 
      cat = "zone_id", 
      log.fun = FALSE
    )
  )
  
  expect_error(
    assignment_column(
      dat = point_data_na, 
      project = "s1", 
      spat = spat_data,
      lon.dat = "lon", 
      lat.dat = "lat", 
      cat = "zone_id", 
      log.fun = FALSE
    )
  )
})