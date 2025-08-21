# -------------------------------------------------------------------------------------------------
# File: test-utils.R
# Purpose: Unit tests for the utility functions in the FishSET package
# Description:
#   This test script verifies the behavior of utility functions related to locating and validating
#   project directories and data within the FishSET package structure. It ensures that functions 
#   correctly resolve test paths and detect the existence of project subfolders and data.
#
# Notes:
#   - Uses a test data folder located at: testthat/testdata/FishSETFolder
#   - These tests use testthat::test_path() and withr::local_options() for safe and 
#     isolated testing
# -------------------------------------------------------------------------------------------------

# Test for 'locproject()' function ----------------------------------------------------------------
test_that("test locproject() works", {
  # Get the full path to the test folder inside the testdata directory
  test_folder <- testthat::test_path("testdata/FishSETFolder")

  # Override the folder path option used by locproject()
  # This isolates the test env from the default paths
  old_option <- getOption("test_folder_path")
  options(test_folder_path = test_folder)
  
  # Call the function
  result <- locproject()
  
  # Check that the function correctly returns the overridden test path
  expect_equal(result, test_folder)
})


# Tests for the 'project_exists()' function -------------------------------------------------------
test_that("test project_exists() works", {
  # Define the base folder path to the test data directory
  # This folder should contain the subfolder named "s1" to pass the test
  test_folder <- testthat::test_path("testdata/FishSETFolder")
  
  # Override the folder path used by locproject() which is called in project_exists()
  # This isolates the test env from the default paths
  old_option <- getOption("test_folder_path")
  options(test_folder_path = test_folder)
  
  # Call the function for a pass (s1) and a fail test (s2)
  result1 <- project_exists("s1")
  result2 <- project_exists("s2")
  
  # Check that the "s1" subfolder exists and the "s2" folder does not
  expect_equal(result1, TRUE)
  expect_equal(result2, FALSE)
})


# Test for the 'locdatabase()' function -----------------------------------------------------------
test_that("test locdatabase() works", {
  # Define the base folder path to the test data directory
  # This folder should contain the subfolder named "s1" to pass the test
  test_folder <- testthat::test_path("testdata/FishSETFolder")
  
  # Override the folder path used by locproject() which is called in locdatabase()
  # This isolates the test env from the default paths
  old_option <- getOption("test_folder_path")
  options(test_folder_path = test_folder)
  
  # Call the function to construct the path to the database
  result <- locdatabase("s1")
  
  # Check that the path to the database is correct
  expect_equal(result, paste0(test_folder, "/s1/fishset_db.sqlite"))
})


# Test for the 'loc_data()' function --------------------------------------------------------------
test_that("test loc_data() works", {
  # Define the base folder path to the test data directory
  # This folder should contain the subfolder named "s1" to pass the test
  test_folder <- testthat::test_path("testdata/FishSETFolder")
  
  # Override the folder path used by locproject() which is called in loc_data()
  # This isolates the test env from the default paths
  old_option <- getOption("test_folder_path")
  options(test_folder_path = test_folder)
  
  # Call the function to construct the path to the database
  result <- loc_data("s1")
  
  # Check that the path to the database is correct
  expect_equal(result, paste0(test_folder, "/s1/data"))
})


# Test for the 'data_pull()' function -------------------------------------------------------------
test_that("test data_pull() works with dat as character", {
  # Define the base folder path to the test data directory
  # This folder should contain the subfolder named "s1" to pass the test
  test_folder <- testthat::test_path("testdata/FishSETFolder")
  
  # Override the folder path used by locproject() which is called in loc_data()
  # This isolates the test env from the default paths
  old_option <- getOption("test_folder_path")
  options(test_folder_path = test_folder)
  
  # Call the function to pull data 
  result <- data_pull(dat = "s1MainDataTable", project = "s1")
  
  # Check that list output is correct
  expect_equal(result[[1]], "s1MainDataTable")
  expect_equal(dim(result[[2]]), c(1992, 20))
  expect_equal(names(result[[2]]), c("TRIPID", "DATE_TRIP", "PERMIT.y", "TRIP_LENGTH", "GEARCODE",
                                     "port_lat", "port_lon", "previous_port_lat", 
                                     "previous_port_lon", "Plan Code", "Program Code",
                                     "TRIP_COST_WINSOR_2020_DOL", "DDLAT", "DDLON", "ZoneID",
                                     "LANDED_OBSCURED", "DOLLAR_OBSCURED", "DOLLAR_2020_OBSCURED",
                                     "DOLLAR_ALL_SP_2020_OBSCURED", "landed_thousands"))
})

test_that("test data_pull() works with dat as tibble", {
  # Define the base folder path to the test data directory
  # This folder should contain the subfolder named "s1" to pass the test
  test_folder <- testthat::test_path("testdata/FishSETFolder")
  
  # Override the folder path used by locproject() which is called in loc_data()
  # This isolates the test env from the default paths
  old_option <- getOption("test_folder_path")
  options(test_folder_path = test_folder)

  # Create the example tibble with the specified structure
  test_dataset <- tibble(
    TRIPID = as.integer(c(22, 65, 466)),
    DATE_TRIP = as.Date(c("2007-05-01", "2007-05-01", "2007-05-01")),
    `PERMIT.y` = as.integer(c(55, 305, 126)),
    TRIP_LENGTH = as.numeric(c(12.23, 10.52, 11.94)),
    GEARCODE = c("DREDGE-SCALLOP", "DREDGE-SCALLOP", "DREDGE-SCALLOP"),
    port_lat = as.numeric(c(37, 38.9, 41.6)),
    port_lon = as.numeric(c(-76.4, -74.9, -70.9)),
    previous_port_lat = as.numeric(c(37, 37, 38.9)),
    previous_port_lon = as.numeric(c(-76.4, -76.3, -74.9)),
    `Plan Code` = c("SES", "SES", "SES"),
    `Program Code` = c("SCA", "SAA", "SCA"),
    TRIP_COST_WINSOR_2020_DOL = as.numeric(c(24366, 19979, 23302)),
    DDLAT = as.numeric(c(39, 38.5, 39.8)),
    DDLON = as.numeric(c(-73.7, -74.1, -72.5)),
    ZoneID = as.numeric(c(387312, 387446, 397224)),
    LANDED_OBSCURED = as.numeric(c(18273, 14899, 15277)),
    DOLLAR_OBSCURED = as.numeric(c(124276, 100568, 106939)),
    DOLLAR_2020_OBSCURED = as.numeric(c(151706, 122765, 130542)),
    DOLLAR_ALL_SP_2020_OBSCURED = as.numeric(c(151706, 122765, 131620)),
    landed_thousands = as.numeric(c(18.3, 14.9, 15.3))
  )
  
  # Call the function to pull data
  result <- data_pull(dat = test_dataset, project = "s1")
  
  # Check that list output is correct
  expect_equal(result[[1]], "test_dataset")
  expect_equal(dim(result[[2]]), c(3, 20))
  expect_equal(names(result[[2]]), c("TRIPID", "DATE_TRIP", "PERMIT.y", "TRIP_LENGTH", "GEARCODE",
                                     "port_lat", "port_lon", "previous_port_lat", 
                                     "previous_port_lon", "Plan Code", "Program Code",
                                     "TRIP_COST_WINSOR_2020_DOL", "DDLAT", "DDLON", "ZoneID",
                                     "LANDED_OBSCURED", "DOLLAR_OBSCURED", "DOLLAR_2020_OBSCURED",
                                     "DOLLAR_ALL_SP_2020_OBSCURED", "landed_thousands"))
})

test_that("test data_pull() throws errors", {
  # Define the base folder path to the test data directory
  # This folder should contain the subfolder named "s1" to pass the test
  test_folder <- testthat::test_path("testdata/FishSETFolder")
  
  # Override the folder path used by locproject() which is called in loc_data()
  # This isolates the test env from the default paths
  old_option <- getOption("test_folder_path")
  options(test_folder_path = test_folder)
  
  # Expect error when dat is not a character or dataframe, or table doesn't exist
  expect_error(data_pull(dat = 2, project = "s1"))
  expect_error(data_pull(dat = NULL, project = "s1"))
  expect_warning(data_pull(dat = "testFail", project = "s1"))
})