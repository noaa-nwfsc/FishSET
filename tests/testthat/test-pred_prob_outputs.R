# -------------------------------------------------------------------------------------------------
# File: test-pred_prob_outputs.R
# Purpose: Unit tests for the `pred_prob_outputs()` function in the FishSET package
# Description:
#   These tests verify that the `pred_prob_outputs()` function returns the correct
#
# Scenarios tested:
#   1. Static map output (ggplot):
#      - Verifies that the function returns a ggplot object.
#      - Checks that the fill label is correctly set to "Probability".
#
# Notes:
#   - Uses test project: "s1"
#   - Assumes access to example model and spatial objects for reproducibility.
# -------------------------------------------------------------------------------------------------

# # Test table output -------------------------------------------------------------------------------
# test_that("pred_prob_outputs generates correct table output", {
#   test_table <- pred_prob_outputs(project = "s1",
#                                   mod.name = "logit_c_mod1",
#                                   policy.name = "closure_1",
#                                   output_option = "table")
#   
#   
#   
#   
#   # Confirm the output is a ggplot
#   expect_s3_class(test_fig, "ggplot")
#   
# })