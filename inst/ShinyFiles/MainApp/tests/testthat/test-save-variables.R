# -------------------------------------------------------------------------------------------------
# File: test-save-variables.R
# Purpose: Test variable saving functionality of the shiny app.
# Description: 
#   Tests the "Save Variables" feature. It ensures that when the user clicks the save button, the 
#   app correctly generates two key outputs: a .rds file with selected variables and a zone 
#   centroid table in the project's SQLite database.
#
# Notes:
#  - The app directory is specified relative to the FishSET package directory.
#  - The test is skipped on CI environments and during R CMD check to avoid unnecessary failures.
# -------------------------------------------------------------------------------------------------

test_that("test-saved-variables", {
  ## Set up testing environment -------------------------------------------------------------------
  skip_on_ci() # Skip this test on CI environments
  
  app_dir <- system.file("ShinyFiles/MainApp", package = "FishSET") # Path to the app directory
  
  # Skip if app directory does not exist
  if (!dir.exists(app_dir) || nchar(app_dir) == 0) {
    skip("App directory does not exist")
  }
  
  # Skip during R CMD check
  if (identical(Sys.getenv("_R_CHECK_PACKAGE_NAME_"), "FishSET")) {
    skip("Skipping test during R CMD check")
  }
  
  ## Clean up previous files and tables -----------------------------------------------------------
  ### Define paths
  saved_var_path <- system.file(
    "tests/testthat/testdata/FishSETFolder/s1/data/s1SavedVariables.rds",
    package = "FishSET"
  )
  
  db_path <- system.file(
    "tests/testthat/testdata/FishSETFolder/s1/fishset_db.sqlite",
    package = "FishSET"
  )
  
  ### Clean up previous files and tables
  # Clean up previously created files to ensure a clean test run
  if (file.exists(saved_var_path)) {
    unlink(saved_var_path)
  }
  
  # Connect to DB to delete old table if it exists
  if (file.exists(db_path)) {
    con <- dbConnect(RSQLite::SQLite(), db_path)
    if (dbExistsTable(con, "s1_ZoneCentroid")) {
      dbRemoveTable(con, "s1_ZoneCentroid")
    }
    dbDisconnect(con)
  }
  
  ## Interact with app ----------------------------------------------------------------------------
  app <- AppDriver$new(
    app_dir = app_dir,
    name = "test-saved-variables",
    options = list(test.mode = TRUE),
    shiny_args = list(test.mode = TRUE),
    load_timeout = 120000, # Increased timeout for loading the app
    timeout = 120000) # Increased timeout for app operations
  
  app$click("folderpath-change_fs_folder_btn") # Click the button to change the folder path
  Sys.sleep(2) # Brief pause to allow the dialog to open
  
  app$click("load_data-load_data_btn") # Click the button to load data
  Sys.sleep(2)
  
  app$wait_for_idle(timeout = 30000) # Wait for the app to finish loading data
  app$click("load_data-load_data_next_btn")
  
  # Click button that triggers saving .rds file and creating centroid table
  app$click("saving_all_variables-save_vars_btn")
  app$wait_for_idle() # Wait for server-side operations to complete
  
  ## Check files and tables -----------------------------------------------------------------------
  sav_var <- file.exists(saved_var_path)
  
  con <- dbConnect(RSQLite::SQLite(), db_path)
  table_exists <- dbExistsTable(con, "s1_ZoneCentroid")
  dbDisconnect(con)
  
  ## Run tests ------------------------------------------------------------------------------------
  expect_true(sav_var)
  expect_true(table_exists)
})