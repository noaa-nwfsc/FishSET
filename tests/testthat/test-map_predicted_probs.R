# -------------------------------------------------------------------------------------------------
# File: test-map_predicted_probs.R
# Purpose: To provide unit tests for the map_predicted_probs() function.
# Description: This script uses the 'testthat' framework to validate the behavior of the
#              map_predicted_probs() function, which generates spatial predictions.
#
# Scenarios tested:
#   - Input Validation: Checks for missing parameters, wrong lengths, and bad column names.
#   - Error Handling: Verifies appropriate stops when prob_matrix or spatial files are missing.
#   - Output Formatting: Ensures returned objects are correctly structured (data.frame, ggplot, 
#                        leaflet).
#   - Data Integrity: Validates that mean probabilities and observation-specific probabilities
#                     are calculated and joined to the spatial data accurately.
#
# Notes: This test mocks internal FishSET functions and database connections (unserialize_table,
#        data_pull, etc.) using `local_mocked_bindings()` to tightly isolate the mapping logic 
#        from the local file system and database constraints.
# -------------------------------------------------------------------------------------------------

library(testthat)
library(sf)
library(ggplot2)
library(leaflet)

# Test Data Setup ---------------------------------------------------------------------------------
set.seed(42)

# 1. Create a minimal sf polygon dataset representing 3 spatial zones ("Zone_A", "Zone_B", "Zone_C")
# We use tiny, simple coordinate matrices to keep the test extremely fast.
poly1 <- st_polygon(list(matrix(c(0,0, 1,0, 1,1, 0,1, 0,0), ncol=2, byrow=TRUE)))
poly2 <- st_polygon(list(matrix(c(1,0, 2,0, 2,1, 1,1, 1,0), ncol=2, byrow=TRUE)))
poly3 <- st_polygon(list(matrix(c(2,0, 3,0, 3,1, 2,1, 2,0), ncol=2, byrow=TRUE)))

dummy_spat <- st_sf(
  TEN_ID = c("Zone_A", "Zone_B", "Zone_C"), 
  geometry = st_sfc(poly1, poly2, poly3), 
  crs = 4326
)

# 2. Create a synthetic probability matrix (5 observations across the 3 zones)
# Note: Row 3, column 1 ("Zone_A") is explicitly set to 0.5 for targeted index testing.
dummy_prob_mat <- matrix(
  c(0.1, 0.8, 0.1,  
    0.2, 0.7, 0.1,  
    0.5, 0.3, 0.2,  
    0.3, 0.4, 0.3,  
    0.9, 0.05, 0.05), 
  nrow = 5, ncol = 3, byrow = TRUE
)
colnames(dummy_prob_mat) <- c("Zone_A", "Zone_B", "Zone_C")

# 3. Create synthetic model fit objects
# One perfectly formed, one missing the critical probability matrix
dummy_fit <- list(
  prob_matrix = dummy_prob_mat
)

dummy_fit_missing <- list(
  coefficients = c(0.1, 0.5)
)

# 4. Mock project SQLite database list containing both models
mock_db_list <- list(
  "clogit1_fit" = dummy_fit,
  "bad_fit" = dummy_fit_missing
)

# Test Input Validation ---------------------------------------------------------------------------
test_that("Input validation catches missing and invalid arguments", {
  
  # Ensure it gracefully catches entirely missing arguments
  expect_error(map_predicted_probs(), "Argument 'fit_name' is missing")
  
  expect_error(
    map_predicted_probs(fit_name = "clogit1_fit"), 
    "Argument 'spat' is missing"
  )
  
  # Ensure it catches vectors passed into a string-only parameter
  expect_error(
    map_predicted_probs(
      fit_name = c("fit1", "fit2"), 
      spat = "spat", 
      project = "proj", 
      zone_spat = "zone"
    ),
    "Please provide only a single model name"
  )
})

# Test Execution Logic ----------------------------------------------------------------------------
test_that("Function logic executes correctly with mocked dependencies", {
  
  # Mock the external FishSET database and internal styling functions.
  # Using local_mocked_bindings isolates these overrides strictly to this test block.
  local_mocked_bindings(
    unserialize_table = function(table, proj) mock_db_list,
    data_pull = function(spat, proj) list(dataset = dummy_spat),
    parse_data_name = function(...) "mock_spat_name",
    save_plot = function(...) TRUE,
    save_table = function(...) TRUE,
    log_call = function(...) TRUE,
    shift_long = function(...) FALSE,                           # Mock coordinate shift math
    fishset_theme = function(...) ggplot2::theme_minimal()      # Mock FishSET UI styling
  )
  
  # --- Test 1: Catches missing prob_matrix ---
  expect_error(
    map_predicted_probs(
      fit_name = "bad_fit", 
      spat = "dummy_spat", 
      project = "proj", 
      zone_spat = "TEN_ID"
    ),
    "The fit object does not contain 'prob_matrix'"
  )
  
  # --- Test 2: Catches mismatched Zone ID Column ---
  # Verifies the spatial left_join won't break dynamically
  expect_error(
    map_predicted_probs(
      fit_name = "clogit1_fit", 
      spat = "dummy_spat", 
      project = "proj", 
      zone_spat = "WRONG_COL"
    ),
    "The Zone ID column 'WRONG_COL' was NOT found"
  )
  
  # --- Test 3: Data Table output (Average Probabilities) ---
  res_table <- map_predicted_probs(
    fit_name = "clogit1_fit", 
    spat = "dummy_spat", 
    project = "proj", 
    zone_spat = "TEN_ID",
    output = "table"
  )
  
  expect_s3_class(res_table, "data.frame")
  expect_equal(colnames(res_table), c("TEN_ID", "mean_prob"))
  
  # Check average math matches (Column 1 is Zone_A)
  expected_mean <- unname(mean(dummy_prob_mat[, 1]))
  expect_equal(unname(res_table$mean_prob[1]), expected_mean)
  
  
  # --- Test 4: Data Table output (Specific Observation) ---
  res_obs <- map_predicted_probs(
    fit_name = "clogit1_fit", 
    spat = "dummy_spat", 
    project = "proj", 
    zone_spat = "TEN_ID",
    obs_index = 3,
    output = "table"
  )
  
  expect_s3_class(res_obs, "data.frame")
  expect_equal(colnames(res_obs), c("TEN_ID", "prob_obs_3"))
  
  # Check the 3rd row prediction for Zone A is explicitly 0.5 (set in our dummy matrix)
  expect_equal(unname(res_obs$prob_obs_3[1]), 0.5)
  
  
  # --- Test 5: Out of bounds obs_index ---
  expect_error(
    map_predicted_probs(
      fit_name = "clogit1_fit", 
      spat = "dummy_spat", 
      project = "proj", 
      zone_spat = "TEN_ID", 
      obs_index = 999,
      output = "table"
    ),
    "obs_index is out of bounds"
  )
  
  
  # --- Test 6: Static Plot Generation (ggplot) ---
  # Skip this test if the CI/CD server does not have the 'maps' package installed
  skip_if_not_installed("maps") 
  
  res_static <- map_predicted_probs(
    fit_name = "clogit1_fit", 
    spat = "dummy_spat", 
    project = "proj", 
    zone_spat = "TEN_ID",
    plot_type = "static",
    output = "plot"
  )
  
  expect_s3_class(res_static, "ggplot")
  
  
  # --- Test 7: Dynamic Plot Generation (Leaflet) ---
  res_dynamic <- map_predicted_probs(
    fit_name = "clogit1_fit", 
    spat = "dummy_spat", 
    project = "proj", 
    zone_spat = "TEN_ID",
    plot_type = "dynamic",
    output = "plot"
  )
  
  expect_s3_class(res_dynamic, "leaflet")
  expect_s3_class(res_dynamic, "htmlwidget")
  
  
  # --- Test 8: Combined Output (Both Plot and Table) ---
  res_both <- map_predicted_probs(
    fit_name = "clogit1_fit", 
    spat = "dummy_spat", 
    project = "proj", 
    zone_spat = "TEN_ID",
    output = "tab_plot"
  )
  
  expect_type(res_both, "list")
  expect_named(res_both, c("table", "plot"))
  expect_s3_class(res_both$table, "data.frame")
  expect_s3_class(res_both$plot, "leaflet")
})