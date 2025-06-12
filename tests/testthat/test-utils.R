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

test_that("test locproject() works", {
  # Get the full path to the test folder inside the testdata directory
  test_folder <- testthat::test_path("testdata/FishSETFolder")

  # Override the folder path option used by locproject()
  # This isolates the test env from the default paths
  withr::local_options(list(test_folder_path = test_folder))
  
  # Call the function
  result <- locproject()
  
  # Check that the function correctly returns the overridden test path
  expect_equal(result, test_folder)
})


test_that("test project_exists() works", {
  # Define the base folder path to the test data directory
  # This folder should contain the subfolder named "s1" to pass the test
  test_folder <- testthat::test_path("testdata/FishSETFolder")
  
  # Override the folder path used by locproject() which is called in project_exists()
  # This isolates the test env from the default paths
  withr::local_options(list(test_folder_path = test_folder))
  
  # Call the function for a pass (s1) and a fail test (s2)
  result1 <- project_exists("s1")
  result2 <- project_exists("s2")
  
  # Check that the "s1" subfolder exists amd the "s2" folder does not
  expect_equal(result1, TRUE)
  expect_equal(result2, FALSE)
})


test_that("test locdatabase() works", {
  # Define the base folder path to the test data directory
  # This folder should contain the subfolder named "s1" to pass the test
  test_folder <- testthat::test_path("testdata/FishSETFolder")
  
  # Override the folder path used by locproject() which is called in locdatabase()
  # This isolates the test env from the default paths
  withr::local_options(list(test_folder_path = test_folder))
  
  # Call the function to construct the path to the database
  result <- locdatabase("s1")
  
  # Check that the path to the database is correct
  expect_equal(result, paste0(test_folder, "/s1/fishset_db.sqlite"))
})


test_that("test loc_data() works", {
  # Define the base folder path to the test data directory
  # This folder should contain the subfolder named "s1" to pass the test
  test_folder <- testthat::test_path("testdata/FishSETFolder")
  
  # Override the folder path used by locproject() which is called in loc_data()
  # This isolates the test env from the default paths
  withr::local_options(list(test_folder_path = test_folder))
  
  # Call the function to construct the path to the database
  result <- loc_data("s1")
  
  # Check that the path to the database is correct
  expect_equal(result, paste0(test_folder, "/s1/data"))
})

test_that("test data_pull() works", {
  # Define the base folder path to the test data directory
  # This folder should contain the subfolder named "s1" to pass the test
  test_folder <- testthat::test_path("testdata/FishSETFolder")
  
  # Override the folder path used by locproject() which is called in loc_data()
  # This isolates the test env from the default paths
  withr::local_options(list(test_folder_path = test_folder))
  
  # Call the function to construct the path to the database
  result <- loc_data("s1")
  
  # Check that the path to the database is correct
  expect_equal(result, paste0(test_folder, "/s1/data"))
})