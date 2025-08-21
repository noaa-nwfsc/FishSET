# -------------------------------------------------------------------------------------------------
# File: test-spatial_qaqc.R
# Purpose: Unit tests for spatial QAQC checks in the FishSET package
# Description:
#   
#
# Scenarios tested:
#   - 
#
# Notes:
#   - 
# -------------------------------------------------------------------------------------------------

test_that("test spatial_qaqc() works", {
  # Define the base folder path to the test data directory
  # This folder should contain the subfolder named "s1" to pass the test
  test_folder <- testthat::test_path("testdata/FishSETFolder")
  
  # Override the folder path used by locproject() which is nested within projects()
  # This isolates the test env from the default paths
  # withr::local_options(list(test_folder_path = test_folder))
  old_option <- getOption("test_folder_path")
  options(test_folder_path = test_folder)
  
  test_df <- head(table_view("s1MainDataTable", "s1"), 20)
  
  result <- suppressWarnings(spatial_qaqc(dat = test_df,
                                          project = "s1",
                                          spat = "s1spatSpatTable",
                                          lon.dat = "DDLON",
                                          lat.dat = "DDLAT",
                                          date = "DATE_TRIP",
                                          epsg = 4269)) 
})
