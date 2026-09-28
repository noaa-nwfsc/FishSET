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
#   - Spatial Bounds: Verifies that 'dat_center' correctly sets bounding box limits.
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

# Create a subset matrix containing ONLY Zone_A to test bounding box zoom limits (dat_center)
dummy_prob_mat_subset <- dummy_prob_mat[, "Zone_A", drop = FALSE]

# 3. Create synthetic model fit objects
# One perfectly formed, one missing the critical probability matrix, and one partial match
dummy_fit <- list(
  prob_matrix = dummy_prob_mat
)

dummy_fit_missing <- list(
  coefficients = c(0.1, 0.5)
)

dummy_fit_subset <- list(
  prob_matrix = dummy_prob_mat_subset
)

# 4. Mock project SQLite database list containing all models
mock_db_list <- list(
  "clogit1_fit" = dummy_fit,
  "bad_fit" = dummy_fit_missing,
  "subset_fit" = dummy_fit_subset
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

test_that("Static plot normalizes projected spatial data to lon/lat", {
  skip_if_not_installed("maps")
  
  projected_spat <- sf::st_transform(dummy_spat, 3857)
  
  local_mocked_bindings(
    unserialize_table = function(table, proj) mock_db_list,
    data_pull = function(spat, proj) list(dataset = projected_spat),
    parse_data_name = function(...) "mock_spat_name",
    save_plot = function(...) TRUE,
    save_table = function(...) TRUE,
    log_call = function(...) TRUE,
    shift_long = function(...) FALSE,
    fishset_theme = function(...) ggplot2::theme_minimal()
  )
  
  res_static <- map_predicted_probs(
    fit_name = "clogit1_fit",
    spat = "dummy_spat",
    project = "proj",
    zone_spat = "TEN_ID",
    plot_type = "static",
    output = "plot"
  )
  
  zone_layer <- res_static$layers[[1]]$data
  zone_bbox <- sf::st_bbox(zone_layer)
  
  expect_equal(sf::st_crs(zone_layer)$epsg, 4326)
  expect_equal(unname(zone_bbox[c("xmin", "xmax", "ymin", "ymax")]), c(0, 3, 0, 1))
  expect_equal(unname(res_static$coordinates$limits$x), c(0, 3))
  expect_equal(unname(res_static$coordinates$limits$y), c(0, 1))
})

test_that("dat_center parameter correctly toggles plot bounding box limits", {
  skip_if_not_installed("maps")
  
  local_mocked_bindings(
    unserialize_table = function(table, proj) mock_db_list,
    data_pull = function(spat, proj) list(dataset = dummy_spat),
    parse_data_name = function(...) "mock_spat_name",
    save_plot = function(...) TRUE,
    save_table = function(...) TRUE,
    log_call = function(...) TRUE,
    shift_long = function(...) FALSE,
    fishset_theme = function(...) ggplot2::theme_minimal()
  )
  
  # Test with dat_center = TRUE (should bound closely to only Zone_A)
  p_centered <- map_predicted_probs(
    fit_name = "subset_fit",        # We use the subset fit (only Zone_A)
    spat = "dummy_spat",
    project = "proj",
    zone_spat = "TEN_ID",
    dat_center = TRUE,              # TRUE flag
    plot_type = "static",
    output = "plot"
  )
  
  # Test with dat_center = FALSE (should bound to the entire dummy_spat: Zones A, B, and C)
  p_full <- map_predicted_probs(
    fit_name = "subset_fit", 
    spat = "dummy_spat",
    project = "proj",
    zone_spat = "TEN_ID",
    dat_center = FALSE,             # FALSE flag
    plot_type = "static",
    output = "plot"
  )
  
  # Zone A has an x-limit of c(0, 1) based on poly1
  expect_equal(unname(p_centered$coordinates$limits$x), c(0, 1))
  
  # The full spatial dataset has an x-limit of c(0, 3) spanning all 3 polys
  expect_equal(unname(p_full$coordinates$limits$x), c(0, 3))
})