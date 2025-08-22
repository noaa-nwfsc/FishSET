# -------------------------------------------------------------------------------------------------
# File: test-spatial_qaqc.R
# Purpose: Unit tests for spatial QAQC checks in the FishSET package
# Description: 
#   This script tests the spatial quality assurance and quality control (QAQC) checks
#   performed by the spatial_qaqc() function. It validates that the function correctly
#   identifies spatial errors such as observations on land, outside regulatory zones,
#   or on zone boundaries.
#   
# Scenarios tested:
#   - Correctly identifying observations on land.
#   - Correctly identifying observations outside defined spatial zones.
#   - Correctly identifying observations on zone boundaries.
#   - Validating the structure and content of the returned output list.
#   - Ensuring the function throws appropriate errors for invalid coordinates.
#
# Notes:
#   - Uses test project: "s1" and its associated tables.
#   - Mocks the log_call() function to prevent side effects during testing.
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

# Test spat_qaqc() --------------------------------------------------------------------------------
test_that("test spatial_qaqc() works", {
  # Define the base folder path to the test data directory
  # This folder should contain the subfolder named "s1" to pass the test
  test_folder <- testthat::test_path("testdata/FishSETFolder")
  
  # Override the folder path used by locproject() which is nested within projects()
  # This isolates the test env from the default paths
  # withr::local_options(list(test_folder_path = test_folder))
  old_option <- getOption("test_folder_path")
  options(test_folder_path = test_folder)
  
  # Save a sample of the data
  test_df <- head(table_view("s1MainDataTable", "s1"), 20)
  
  # Save tail of data to modify coordinates for spatial checks
  tmp_df <- tail(table_view("s1MainDataTable", "s1"), 4)
  tmp_df[1:2,c("DDLON", "DDLAT")] <- 
    matrix(c(-73,-72,41.5,42), nrow=2) # On land
  tmp_df[3:4,c("DDLON", "DDLAT")] <- 
    matrix(c(-61,-60,40.5,40), nrow=2) # Out of spatial bounds
  
  # Bind sample datasets
  test_df <- rbind(test_df, tmp_df)
  
  # Call the function to test a few cases
  result <- suppressMessages(
    suppressWarnings(
      spatial_qaqc(dat = test_df,
                   project = "s1",
                   spat = "s1spatSpatTable",
                   lon.dat = "DDLON",
                   lat.dat = "DDLAT",
                   date = "DATE_TRIP",
                   epsg = 4269))) 
  
  # Check that the output is a list and contains key elements
  expect_type(result, "list")
  expect_true("dataset" %in% names(result))
  expect_true("spatial_summary" %in% names(result))
  expect_true("land_plot" %in% names(result))
  expect_true("outside_plot" %in% names(result))
  expect_true("expected_plot" %in% names(result))
  
  # Check flagged columns
  expect_true("ON_LAND" %in% names(result$dataset))
  expect_true("OUTSIDE_ZONE" %in% names(result$dataset))
  
  # Check rows for on boundary, land, and outside zone
  expect_true(all(result$dataset$ON_ZONE_BOUNDARY[4]))
  expect_true(all(result$dataset$ON_LAND[21:22]))
  expect_true(all(result$dataset$OUTSIDE_ZONE[23:24]))
})

# Test spat_qaqc() errors -------------------------------------------------------------------------
test_that("test spatial_qaqc() throws errors", {
  # Define the base folder path to the test data directory
  # This folder should contain the subfolder named "s1" to pass the test
  test_folder <- testthat::test_path("testdata/FishSETFolder")
  
  # Override the folder path used by locproject() which is nested within projects()
  # This isolates the test env from the default paths
  # withr::local_options(list(test_folder_path = test_folder))
  old_option <- getOption("test_folder_path")
  options(test_folder_path = test_folder)
  
  # Save a sample of the data and create invalid coordinates
  test_df <- head(table_view("s1MainDataTable", "s1"), 4)
  test_df[,"DDLON"] <- c(-190, -125, -192, 0)

  # Check that the function correctly throws an error for invalid longitude values
  expect_error(
    suppressMessages(
      suppressWarnings(
        spatial_qaqc(dat = test_df,
                     project = "s1",
                     spat = "s1spatSpatTable",
                     lon.dat = "DDLON",
                     lat.dat = "DDLAT",
                     date = "DATE_TRIP",
                     epsg = 4269))))
})