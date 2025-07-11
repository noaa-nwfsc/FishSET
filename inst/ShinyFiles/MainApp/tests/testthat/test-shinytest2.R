# -------------------------------------------------------------------------------------------------
# File: test-shinytest2.R
# Purpose: Test the FishSET app using shinytest2
# Description: 
#   This script uses the shinytest2 package to record and test the FishSET Shiny app. 
#
# Notes: 
#
# -------------------------------------------------------------------------------------------------

# # Test for loading data in shiny - ----------------------------------------------------------------
test_that("test-load-data", {
  skip_on_cran() # Skip this test on CRAN
  
  app_dir <- system.file("ShinyFiles/MainApp", package = "FishSET") # Path to the app directory
  
  message("Working directory: ", getwd()) # Print the current working directory")
  message("App directory: ", app_dir)
  message("App directory exists: ", dir.exists(app_dir))
  
  app <- AppDriver$new(
    app_dir = app_dir,
    name = "test-load-data",
    options = list(test.mode = TRUE),
    shiny_args = list(test.mode = TRUE),
    load_timeout = 100000, # Increased timeout for loading the app
    timeout = 100000) # Increased timeout for app operations

  app$click("folderpath-change_fs_folder_btn") # Click the button to change the folder path
  app$click("load_data-load_data_btn") # Click the button to load data

  app$wait_for_idle(timeout = 10000) # Wait for the app to finish loading data
  
  main_data <- app$get_values(export = "main") # Get the main data values
  port_data <- app$get_values(export = "port") # Get the port data values
  aux_data <- app$get_values(export = "aux") # Get the auxiliary data values
  spat_data <- app$get_values(export = "spat") # Get the spatial data values
  grid_data <- app$get_values(export = "grid") # Get the gridded data values
  # 
  expect_equal(dim(main_data$export$main), c(1992, 20)) # Check dimensions of main data
  expect_equal(dim(port_data$export$port), c(40, 3)) # Check dimensions of port data
  expect_equal(dim(aux_data$export$aux), c(106, 3)) # Check dimensions of auxiliary data
  expect_equal(dim(spat_data$export$spat), c(5267, 2)) # Check dimensions of spatial data
  expect_equal(dim(grid_data$export$grid), c(658, 232)) # Check dimensions of gridded data
})

