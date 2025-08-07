# -------------------------------------------------------------------------------------------------
# File: test-ID_var.R
# Purpose: Unit tests for creating trip/haul level id
# Description:
#   This script tests the that `ID_var()` function returns a new variable in the primary data table
#   and how it is displayed (type, separator, etc.). It also checks if some arguments are not 
#   provided, then it will create it based on row number or expect an error.
#
#
# Scenarios tested:
#   - Two variables and name provided and return as string
#   - Two variables and name provided and return as integer
#   - Using different separator ('-' instead of default '_')
#   - Dropping variables used to create new id variable
#   - No variables provided, but a name is
#   - No variables or name, so error is expected
#
# Notes:
#   - Uses test project: "s1"
#   - Assumes access to example model and spatial objects for reproducibility.
# -------------------------------------------------------------------------------------------------

# Test default settings ---------------------------------------------------------------------------
test_that("ID_var creates correct string ID with default setting", {
  # Define the base folder path to the test data directory
  # This folder should contain the subfolder named "s1" to pass the test
  test_folder <- testthat::test_path("testdata/FishSETFolder")
  
  # Override the folder path used by locproject() which is nested within 
  # This isolates the test env from the default paths
  old_option <- getOption("test_folder_path")
  options(test_folder_path = test_folder)
  
  result_main <- table_view("s1MainDataTable", "s1")
  
  result <- ID_var(dat = result_main,
                   project = "s1",
                   vars = c("ZoneID", "TRIPID"),
                   name = "PermitID",
                   type = "string",
                   log_fun = FALSE)
  
  expect_true("PermitID" %in% names(result))
  expect_equal(result$PermitID[1], "387312_22")
  expect_type(result$PermitID, "character")
  expect_true(all(c("GEARCODE", "TRIPID", "ZoneID")
                  %in% names(result))) # Ensure original vars are not dropped by default
  
})

# Test with integer ID ---------------------------------------------------------------------------
test_that("ID_var creates correct integer ID", {
  # Define the base folder path to the test data directory
  # This folder should contain the subfolder named "s1" to pass the test
  test_folder <- testthat::test_path("testdata/FishSETFolder")
  
  # Override the folder path used by locproject() which is nested within 
  # This isolates the test env from the default paths
  old_option <- getOption("test_folder_path")
  options(test_folder_path = test_folder)
  
  result_main <- table_view("s1MainDataTable", "s1")
  
  result <- ID_var(dat = result_main,
                   project = "s1",
                   vars = c("ZoneID", "TRIPID"),
                   name = "PermitID",
                   type = "integer",
                   log_fun = FALSE)
  
  expect_true("PermitID" %in% names(result))
  expect_type(result$PermitID, "integer")
})

# Test with custom separator -----------------------------------------------------------------------
test_that("ID_var uses custom separator for string ID", {
  # Define the base folder path to the test data directory
  # This folder should contain the subfolder named "s1" to pass the test
  test_folder <- testthat::test_path("testdata/FishSETFolder")
  
  # Override the folder path used by locproject() which is nested within 
  # This isolates the test env from the default paths
  old_option <- getOption("test_folder_path")
  options(test_folder_path = test_folder)
  
  result_main <- table_view("s1MainDataTable", "s1")
  
  result <- ID_var(dat = result_main,
                   project = "s1",
                   vars = c("ZoneID", "TRIPID"),
                   name = "PermitID",
                   log_fun = FALSE,
                   sep = "-")
  
  expect_true("PermitID" %in% names(result))
  expect_type(result$PermitID, "character")
  expect_equal(result$PermitID[1], "387312-22")
  
  
})

# Test with drop = TRUE ---------------------------------------------------------------------------
test_that("ID_var drops original variables when drop = TRUE", {
  # Define the base folder path to the test data directory
  # This folder should contain the subfolder named "s1" to pass the test
  test_folder <- testthat::test_path("testdata/FishSETFolder")
  
  # Override the folder path used by locproject() which is nested within 
  # This isolates the test env from the default paths
  old_option <- getOption("test_folder_path")
  options(test_folder_path = test_folder)
  
  result_main <- table_view("s1MainDataTable", "s1")
  
  result <- ID_var(dat = result_main,
                   project = "s1",
                   vars = c("ZoneID", "TRIPID"),
                   name = "PermitID",
                   log_fun = FALSE,
                   drop = TRUE)
  
  expect_true("PermitID" %in% names(result))
  expect_false(all(c( "TRIPID", "ZoneID") %in% names(result)))  
})

# Test with vars empty and name provided -----------------------------------------------------------
test_that("ID_var creates row_id when vars is empty and name is provided", {
  # Define the base folder path to the test data directory
  # This folder should contain the subfolder named "s1" to pass the test
  test_folder <- testthat::test_path("testdata/FishSETFolder")
  
  # Override the folder path used by locproject() which is nested within 
  # This isolates the test env from the default paths
  old_option <- getOption("test_folder_path")
  options(test_folder_path = test_folder)
  
  result_main <- table_view("s1MainDataTable", "s1")
  
  result <- ID_var(dat = result_main,
                   project = "s1",
                   name = "PermitID",
                   vars = NULL,
                   log_fun = FALSE)
  
  expect_true("PermitID" %in% names(result))
  expect_equal(result$PermitID[1], "1")
  expect_equal(result$PermitID[5], "5")
})

# Test with vars and name empty -------------------------------------------------------------------
test_that("ID_var stops if vars is empty and name is NULL", {
  # Define the base folder path to the test data directory
  # This folder should contain the subfolder named "s1" to pass the test
  test_folder <- testthat::test_path("testdata/FishSETFolder")
  
  # Override the folder path used by locproject() which is nested within 
  # This isolates the test env from the default paths
  old_option <- getOption("test_folder_path")
  options(test_folder_path = test_folder)
  
  result_main <- table_view("s1MainDataTable", "s1")
  
  expect_error(ID_var(dat = result_main,
                   project = "s1",
                   vars = NULL,
                   log_fun = FALSE))
})
