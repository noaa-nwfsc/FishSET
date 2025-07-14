# -------------------------------------------------------------------------------------------------
# File: test-shiny-app.R
# Purpose: Test the FishSET app using shinytest2
# Description:
#   This script uses the shinytest2 package to record and test the FishSET Shiny app. The tests
#   are located in ShinyFiles/MainApp/tests/testthat/test-shinytest2.R.
#   
# Notes:
#   - The app directory is specified relative to the FishSET package directory.
#   - The test checks if the app loads correctly and if the data is loaded as expected.
# -------------------------------------------------------------------------------------------------

library(shinytest2)

test_that("MainApp works", {
  skip_on_ci() # Skip this test on CI environments

  app_dir <- system.file("ShinyFiles/MainApp", package = "FishSET")

  # Ensure the app directory exists
  expect_true(dir.exists(app_dir), info = "App directory does not exist")
  expect_true(nchar(app_dir) > 0, info = "App directory path is empty")

  # Run shinytest2 tests
  shinytest2::test_app(app_dir, test_file = NULL)
})


