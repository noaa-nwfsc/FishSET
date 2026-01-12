# -------------------------------------------------------------------------------------------------
# File: test-create_alternative_choice.R
# Purpose: To provide unit tests for the create_alternative_choice() function.
# Description: This script uses the 'testthat' framework to validate the behavior of the
#              create_alternative_choice() function.
#
# Scenarios tested:
#   - Zonal centroid work correctly
#   - Duplicate alt_name throws error
#   - Missing zone_cent_name throws error
#   - Port occasion requires occasion_var
#   - Throws error if spatial data is missing for 'nearest point' alternative
#   - Ensuring alt_name auto-generates if missing
#
# Notes: This test script is designed to be run within the FishSET package structure using
#        devtools::test() or a similar tool.
# -------------------------------------------------------------------------------------------------

# Define the mock functions
mock_log_call <- function(...) invisible(NULL)
mock_data_pull <- function(dat, project) list(dataset = dat)
mock_parse_data_name <- function(dat, type, project) dat

# Save the original functions from the package namespace
original_log_call <- get("log_call", envir = as.environment("package:FishSET"))
original_data_pull <- get("data_pull", envir = as.environment("package:FishSET"))
original_parse_data_name <- get("parse_data_name", envir = as.environment("package:FishSET"))

# Schedule the restoration of all original functions.
on.exit({
  assignInNamespace("log_call", original_log_call, ns = "FishSET")
  assignInNamespace("data_pull", original_data_pull, ns = "FishSET")
  assignInNamespace("parse_data_name", original_parse_data_name, ns = "FishSET")
})

# Overwrite the real functions with our mocks
assignInNamespace("log_call", mock_log_call, ns = "FishSET")
assignInNamespace("data_pull", mock_data_pull, ns = "FishSET")
assignInNamespace("parse_data_name", mock_parse_data_name, ns = "FishSET")


# zonal centroid work correctly -------------------------------------------------------------------
test_that("Standard run: Zonal Centroid succeeds with valid inputs", {
  # This folder should contain the subfolder named "s1" to pass the test
  test_folder <- testthat::test_path("testdata/FishSETFolder")
  
  # Override the folder path used by locproject() which is nested within projects()
  # This isolates the test env from the default paths
  # withr::local_options(list(test_folder_path = test_folder))
  old_option <- getOption("test_folder_path")
  options(test_folder_path = test_folder)
  
  table_remove("s1AltMatrix","s1")
  
  test_df <- table_view("s1MainDataTable", "s1")
  
  expect_message(
    create_alternative_choice(
      dat = test_df,
      project = "s1",
      occasion = "zonal centroid",
      alt_var = "zonal centroid",
      zoneID = "ZoneID",
      spatname = "s1spatSpatTable",
      alt_name = "MyAltChoice"
    ),
    "saved to FishSET database"
  )
})

# Duplicate alt_name throws error -----------------------------------------------------------------
test_that("Error: Duplicate alt_name throws error", {
  
  # This folder should contain the subfolder named "s1" to pass the test
  test_folder <- testthat::test_path("testdata/FishSETFolder")
  
  # Override the folder path used by locproject() which is nested within projects()
  # This isolates the test env from the default paths
  # withr::local_options(list(test_folder_path = test_folder))
  old_option <- getOption("test_folder_path")
  options(test_folder_path = test_folder)
  
  test_df <- table_view("s1MainDataTable", "s1")
  
  expect_error(
    create_alternative_choice(
      dat = test_df,
      project = "s1",
      occasion = "zonal centroid",
      alt_var = "zonal centroid",
      zoneID = "ZoneID",
      spatname = "s1spatSpatTable",
      
      alt_name = "MyAltChoice"
    ),
    "already exists in the database"
  )
})


# Port occasion requires occasion_var -------------------------------------------------------------
test_that("Logic: Occasion = 'port' requires occasion_var", {
  
  # This folder should contain the subfolder named "s1" to pass the test
  test_folder <- testthat::test_path("testdata/FishSETFolder")
  
  # Override the folder path used by locproject() which is nested within projects()
  # This isolates the test env from the default paths
  # withr::local_options(list(test_folder_path = test_folder))
  old_option <- getOption("test_folder_path")
  options(test_folder_path = test_folder)
  
  test_df <- table_view("s1MainDataTable", "s1")
  
  expect_error(
    create_alternative_choice(
      dat = test_df,
      project = "s1",
      occasion = "port",
      occasion_var = NULL, # Missing
      zoneID = "ZoneID",
      spatname = "s1spatSpatTable",
      alt_name = "NewName2"
    ),
    "Port column name required"
  )
})

# Throws error if spatial data is missing for 'nearest point' alternative -------------------------
test_that("Logic: alt_var = 'nearest point' requires spatial data", {
  
  # 1. Setup Mocks specific to this test
  # This folder should contain the subfolder named "s1" to pass the test
  test_folder <- testthat::test_path("testdata/FishSETFolder")
  
  # Override the folder path used by locproject() which is nested within projects()
  # This isolates the test env from the default paths
  # withr::local_options(list(test_folder_path = test_folder))
  old_option <- getOption("test_folder_path")
  options(test_folder_path = test_folder)
  
  test_df <- table_view("s1MainDataTable", "s1")
  
  
  expect_error(
    create_alternative_choice(
      dat = test_df,
      project = "s1",
      alt_var = "nearest point",
      spatname = NULL, # Missing
      zoneID = "ZoneID",
      
      alt_name = "NewName"
    ),
    "'spat' and 'spatID' are required"
  )
})

# Ensuring alt_name auto-generates if missing
test_that("Warning: Auto-generating alt_name if missing", {
  
  # This folder should contain the subfolder named "s1" to pass the test
  test_folder <- testthat::test_path("testdata/FishSETFolder")
  
  # Override the folder path used by locproject() which is nested within projects()
  # This isolates the test env from the default paths
  # withr::local_options(list(test_folder_path = test_folder))
  old_option <- getOption("test_folder_path")
  options(test_folder_path = test_folder)
  
  test_df <- table_view("s1MainDataTable", "s1")
  
  expect_warning(
    try(
      create_alternative_choice(
        dat = test_df,
        project = "s1",
        alt_name = NULL, # Missing
        zoneID = "ZoneID",
      ), silent = TRUE),
    "No 'alt_name' provided"
  )
})
