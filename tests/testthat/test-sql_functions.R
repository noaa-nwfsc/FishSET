# -------------------------------------------------------------------------------------------------
# File: test-sql_functions.R
# Purpose: Unit tests for the SQL functions in the FishSET package
# Description:
#
# Scenarios tested:
#
# Notes:
#   - Uses test project: "s1"
#   - Assumes access to example model and spatial objects for reproducibility.
# -------------------------------------------------------------------------------------------------

test_that("test projects() works", {
  # Define the base folder path to the test data directory
  # This folder should contain the subfolder named "s1" to pass the test
  test_folder <- testthat::test_path("testdata/FishSETFolder")

  # Override the folder path used by locproject() which is nested within projects()
  # This isolates the test env from the default paths
  withr::local_options(list(test_folder_path = test_folder))

  # Call the function and "s1" should be returned
  result <- projects()

  # Check the result
  expect_equal(result, "s1")
})


test_that("test list_tables() works", {
  # Define the base folder path to the test data directory
  # This folder should contain the subfolder named "s1" to pass the test
  test_folder <- testthat::test_path("testdata/FishSETFolder")
  
  # Override the folder path used by locproject() which is nested within list_tables()
  # This isolates the test env from the default paths
  withr::local_options(list(test_folder_path = test_folder))
  
  # Call the function and assess table names
  result_main <- list_tables("s1")
  result_spat <- list_tables("s1", "spat")
  
  # Check the results
  expect_equal(length(result_main), 3)
  expect_equal(result_main[1], "s1MainDataTable")
  expect_equal(length(result_spat), 2)
  expect_equal(result_spat[1], "s1spatSpatTable")
})


test_that("test tables_database() works", {
  # Define the base folder path to the test data directory
  # This folder should contain the subfolder named "s1" to pass the test
  test_folder <- testthat::test_path("testdata/FishSETFolder")

  # Override the folder path used by locproject() which is nested within tables_database()
  # This isolates the test env from the default paths
  withr::local_options(list(test_folder_path = test_folder))

  # Call the function and assess table names
  result <- tables_database("s1")

  # Check the result
  expect_equal(length(result), 17)
  expect_equal(result[1], "s1AltMatrix")
  expect_equal(result[17], "s1spatSpatTable20250612")
})


test_that("test table_exists() works", {
  # Define the base folder path to the test data directory
  # This folder should contain the subfolder named "s1" to pass the test
  test_folder <- testthat::test_path("testdata/FishSETFolder")

  # Override the folder path used by locproject() which is nested within table_exists()
  # This isolates the test env from the default paths
  withr::local_options(list(test_folder_path = test_folder))

  # Call the function to test a few cases
  result1 <- table_exists(paste0("s1", "MainDataTable"), "s1")
  result2 <- table_exists(paste0("s1", "predictOutput"), "s1")
  result3 <- table_exists(paste0("s2", "MainDataTable"), "s1") # fail becase "s2"
  
  # Check that the results are correct
  expect_equal(result1, TRUE)
  expect_equal(result2, TRUE)
  expect_equal(result3, FALSE)
})

test_that("test unserialize_table() works", {
  # Define the base folder path to the test data directory
  # This folder should contain the subfolder named "s1" to pass the test
  test_folder <- testthat::test_path("testdata/FishSETFolder")
  
  # Override the folder path used by locproject() which is nested 
  # within unserialize_table().
  # This isolates the test env from the default paths
  withr::local_options(list(test_folder_path = test_folder))
  
  # Call the function to test a few cases
  result1 <- unserialize_table(paste0("s1", "predictOutput"), "s1")
  
  # Check that the results are correct
  expect_equal(length(result1[[1]]), 10)
  expect_equal(result1[[1]][[1]], "logit_c_mod1 closure_1")
  expect_equal(dim(result1[[1]][[2]]), c(2, 2))
  expect_equal(dim(result1[[1]][[3]]), c(6, 2))
})