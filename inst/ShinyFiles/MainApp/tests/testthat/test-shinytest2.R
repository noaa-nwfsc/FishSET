# -------------------------------------------------------------------------------------------------
# File: test-shinytest2.R
# Purpose: Test the FishSET app using shinytest2 
# Description: 
#   This script uses the shinytest2 package to record and test the FishSET Shiny app.
#
# Notes:
#  - The app directory is specified relative to the FishSET package directory.
#  - The test checks if the app loads correctly and if the data is loaded as expected.
#  - The test is skipped on CI environments and during R CMD check to avoid unnecessary failures.
# -------------------------------------------------------------------------------------------------

# Test for loading data in shiny - ----------------------------------------------------------------
test_that("test-load-data", {
  skip_on_ci() # Skip this test on CI environments
  
  app_dir <- system.file("ShinyFiles/MainApp", package = "FishSET") # Path to the app directory
  
  message("Working directory: ", getwd()) # Print the current working directory")
  message("App directory: ", app_dir)
  message("App directory exists: ", dir.exists(app_dir))
  
  # Skip if app directory does not exist
  if (!dir.exists(app_dir) || nchar(app_dir) == 0) {
    skip("App directory does not exist")
  }
  
  # Skip during R CMD check
  if (identical(Sys.getenv("_R_CHECK_PACKAGE_NAME_"), "FishSET")) {
    skip("Skipping test during R CMD check")
  }
  
  app <- AppDriver$new(
    app_dir = app_dir,
    name = "test-load-data",
    options = list(test.mode = TRUE),
    shiny_args = list(test.mode = TRUE),
    load_timeout = 120000, # Increased timeout for loading the app
    timeout = 120000) # Increased timeout for app operations
  
  app$click("folderpath-change_fs_folder_btn") # Click the button to change the folder path
  Sys.sleep(2) # Brief pause to allow the dialog to open
  
  app$click("load_data-load_data_btn") # Click the button to load data
  Sys.sleep(2)
  
  app$wait_for_idle(timeout = 30000) # Wait for the app to finish loading data
  
  main_data <- app$get_values(export = "main") # Get the main data values
  port_data <- app$get_values(export = "port") # Get the port data values
  aux_data <- app$get_values(export = "aux") # Get the auxiliary data values
  spat_data <- app$get_values(export = "spat") # Get the spatial data values
  grid_data <- app$get_values(export = "grid") # Get the gridded data values
  
  expect_equal(dim(main_data$export$main), c(1992, 20)) # Check dimensions of main data
  expect_equal(dim(port_data$export$port), c(40, 3)) # Check dimensions of port data
  expect_equal(dim(aux_data$export$aux), c(106, 3)) # Check dimensions of auxiliary data
  expect_equal(dim(spat_data$export$spat), c(5267, 2)) # Check dimensions of spatial data
  expect_equal(dim(grid_data$export$grid), c(658, 232)) # Check dimensions of gridded data
})

# Test save variables button ----------------------------------------------------------------------
test_that("test-saved-variables", {
  skip_on_ci() # Skip this test on CI environments
  
  app_dir <- system.file("ShinyFiles/MainApp", package = "FishSET") # Path to the app directory
  
  message("Working directory: ", getwd()) # Print the current working directory")
  message("App directory: ", app_dir)
  message("App directory exists: ", dir.exists(app_dir))
  
  # Skip if app directory does not exist
  if (!dir.exists(app_dir) || nchar(app_dir) == 0) {
    skip("App directory does not exist")
  }
  
  # Skip during R CMD check
  if (identical(Sys.getenv("_R_CHECK_PACKAGE_NAME_"), "FishSET")) {
    skip("Skipping test during R CMD check")
  }
  
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
  
  saved_var_path <- 
    system.file(
      "tests/testthat/testdata/FishSETFolder/s1/data/s1SavedVariables.rds", 
      package = "FishSET"
    )
  
  message("file path to saved variables: ", saved_var_path)
  
  app$click("saving_all_variables-save_vars_btn")
  
  sav_var <- file.exists(saved_var_path)
  
  expect_equal(sav_var, TRUE)
})
