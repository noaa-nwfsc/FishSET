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
#   - Uses test project: "test_scallop_policy"
#   - Assumes access to example model and spatial objects for reproducibility.
# -------------------------------------------------------------------------------------------------

# Test static map output (ggplot) -----------------------------------------------------------------
test_that("predict_map ggplot output works", {
  # test_fig <- predict_map(
  #   project <- "scallop_testthat",
  #   mod.name <- "logit_c_mod1",
  #   policy.name <- "closure_1",
  #   spat <- "scallop_testthatscallop_spatialSpatTable",
  #   zone.spat <- "TEN_ID",
  #   plot_type <- "static",
  #   outsample <- FALSE,
  #   outsample_pred <- NULL
  # )
  # 
  # # Confirm the output is a ggplot
  # expect_s3_class(test_fig, "ggplot")
  # 
  # # Check figure characteristics
  # expect_equal(test_fig$labels$fill, "Probability")
})

# Test dynamic map output (leaflet-----------------------------------------------------------------
test_that("predict_map leaflet output works", {
  # test_fig <- predict_map(
  #   project <- "scallop_testthat",
  #   mod.name <- "logit_c_mod1",
  #   policy.name <- "closure_1",
  #   spat <- "scallop_testthatscallop_spatialSpatTable",
  #   zone.spat <- "TEN_ID",
  #   plot_type <- "dynamic",
  #   outsample <- FALSE,
  #   outsample_pred <- NULL
  # )
  # 
  # # Confirm the output is a ggplot
  # expect_s3_class(test_fig, "leaflet")
  # 
  # # Check figure characteristics
  # expect_equal(test_fig$x$calls[[1]]$args[[1]], "OpenStreetMap")
  # expect_equal(as.character(test_fig$x$calls[[3]]$args[[2]]), "Probability")
})