# -------------------------------------------------------------------------------------------------
# File: test-predict_map.R
# Purpose: Unit tests for the `predict_map()` function in the FishSET package
# Description:
#   These tests verify that the `predict_map()` function returns the correct
#   type of plot object (`ggplot` for static, `leaflet` for dynamic) and that 
#   the key graphical elements (labels, tiles, layers) are correctly constructed.
#
# Scenarios tested:
#   1. Static map output (ggplot):
#      - Verifies that the function returns a ggplot object.
#      - Checks that the fill label is correctly set to "Probability".
#
#   2. Dynamic map output (leaflet):
#      - Verifies that the function returns a leaflet object.
#      - Checks that the base map tiles and color mapping arguments are as expected.
#
# Notes:
#   - Uses test project: "s1"
#   - Assumes access to example model and spatial objects for reproducibility.
# -------------------------------------------------------------------------------------------------

# Test static map output (ggplot) -----------------------------------------------------------------
test_that("predict_map ggplot output works", {
  # Define the base folder path to the test data directory
  # This folder should contain the subfolder named "s1" to pass the test
  test_folder <- testthat::test_path("testdata/FishSETFolder")

  # Override the folder path used by locproject() which is nested within predict_map()
  # This isolates the test env from the default paths
  old_option <- getOption("test_folder_path")
  options(test_folder_path = test_folder)

  test_fig <- predict_map(
    project <- "s1",
    mod.name <- "logit_c_mod1",
    policy.name <- "closure_1",
    spat <- "s1spatSpatTable",
    zone.spat <- "TEN_ID",
    plot_type <- "static",
    outsample <- FALSE,
    outsample_pred <- NULL
  )

  # Confirm the output is a ggplot
  expect_s3_class(test_fig, "ggplot")

  # Check figure characteristics
  expect_equal(test_fig$labels$fill, "Probability")
})

# Test dynamic map output (leaflet-----------------------------------------------------------------
test_that("predict_map leaflet output works", {
  # Define the base folder path to the test data directory
  # This folder should contain the subfolder named "s1" to pass the test
  test_folder <- testthat::test_path("testdata/FishSETFolder")
  
  # Override the folder path used by locproject() which is nested within predict_map()
  # This isolates the test env from the default paths
  old_option <- getOption("test_folder_path")
  options(test_folder_path = test_folder)
  
  test_fig <- predict_map(
    project <- "s1",
    mod.name <- "logit_c_mod1",
    policy.name <- "closure_1",
    spat <- "s1spatSpatTable",
    zone.spat <- "TEN_ID",
    plot_type <- "dynamic",
    outsample <- FALSE,
    outsample_pred <- NULL
  )

  # Confirm the output is a ggplot
  expect_s3_class(test_fig, "leaflet")

  # Check figure characteristics
  expect_equal(test_fig$x$calls[[1]]$args[[1]], "OpenStreetMap")
  expect_equal(as.character(test_fig$x$calls[[3]]$args[[2]]), "Probability")
})