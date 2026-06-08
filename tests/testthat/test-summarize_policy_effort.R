# -------------------------------------------------------------------------------------------------
# File: test-summarize_policy_effort.R
# Purpose: To provide unit tests for the summarize_policy_effort() function.
# Description: This script uses the 'testthat' framework to validate the reporting wrapper.
#              It mocks internal database functions to supply synthetic simulation data and 
#              spatial 'sf' objects without requiring a real FishSET project structure.
#
# Scenarios tested:
#   - Full Execution: Validates that the function returns the expected list structure containing
#     the data frame and three sets of nested ggplot objects.
#   - Mathematical Accuracy: Validates that percentage changes are calculated correctly 
#     (e.g., a drop to 0 is -100%) and that baseline zeroes safely return NA to avoid Inf.
#   - Error Handling: Validates that an empty database triggers the correct error.
#
# Notes: Uses the 'sf' package to create a minimal viable spatial polygon matrix for testing.
# -------------------------------------------------------------------------------------------------

# Test Data Setup ---------------------------------------------------------------------------------
# Create simple mock spatial polygons using sf
p1 <- sf::st_polygon(list(matrix(c(0,0, 1,0, 1,1, 0,1, 0,0), ncol=2, byrow=TRUE)))
p2 <- sf::st_polygon(list(matrix(c(1,0, 2,0, 2,1, 1,1, 1,0), ncol=2, byrow=TRUE)))
mock_sf <- sf::st_sf(zone_id = c("zone_1", "zone_2"), geometry = sf::st_sfc(p1, p2))

# Create synthetic policy simulation output
mock_sims <- list(
  mod_1_baseline = list(
    model_name = "mod_1",
    scenario = "baseline",
    results = list(
      effort_base = c(zone_1 = 100, zone_2 = 200),
      effort_new  = c(zone_1 = 100, zone_2 = 200)
    )
  ),
  mod_1_closure_1 = list(
    model_name = "mod_1",
    scenario = "closure_1",
    results = list(
      effort_base = c(zone_1 = 100, zone_2 = 200),
      effort_new  = c(zone_1 = 0,   zone_2 = 300) # Zone 1 closes, spills 100 trips to Zone 2
    )
  ),
  mod_1_zero_base = list(
    model_name = "mod_1",
    scenario = "zero_base",
    results = list(
      effort_base = c(zone_1 = 0,  zone_2 = 200), # Zone 1 starts at 0 to test division by zero
      effort_new  = c(zone_1 = 50, zone_2 = 150)
    )
  )
)

# Environment Setup -------------------------------------------------------------------------------
orig_functions <- list(
  unserialize_table = getFromNamespace("unserialize_table", "FishSET"),
  data_pull         = getFromNamespace("data_pull", "FishSET"),
  check_spatdat     = getFromNamespace("check_spatdat", "FishSET")
)

setup_mocks <- function(return_sims = mock_sims) {
  assignInNamespace("unserialize_table", function(...) return_sims, ns = "FishSET")
  assignInNamespace("data_pull", function(...) list(dataset = mock_sf), ns = "FishSET")
  assignInNamespace("check_spatdat", function(...) mock_sf, ns = "FishSET")
}

restore_mocks <- function() {
  assignInNamespace("unserialize_table", orig_functions$unserialize_table, ns = "FishSET")
  assignInNamespace("data_pull", orig_functions$data_pull, ns = "FishSET")
  assignInNamespace("check_spatdat", orig_functions$check_spatdat, ns = "FishSET")
}

# Test expected structure and plots ---------------------------------------------------------------
test_that("summarize_policy_effort returns expected structure and valid plots", {
  setup_mocks()
  on.exit(restore_mocks(), add = TRUE)
  
  res <- summarize_policy_effort(
    project = "test_proj", 
    spat = "mock_spat", 
    zone_spat = "zone_id"
  )
  
  # Structure checks
  expect_type(res, "list")
  expect_named(res, c("summary_data", "plots_absolute_map", "plots_percent_map", "plots_scatter"))
  
  # Data frame checks
  expect_s3_class(res$summary_data, "data.frame")
  expect_equal(nrow(res$summary_data), 6) # 3 scenarios * 2 zones = 6 rows
  
  # Plot generation checks
  expect_s3_class(res$plots_absolute_map$mod_1_closure_1, "ggplot")
  expect_s3_class(res$plots_percent_map$mod_1_closure_1, "ggplot")
  expect_s3_class(res$plots_scatter$mod_1_closure_1, "ggplot")
})

# Test output values ------------------------------------------------------------------------------
test_that("summarize_policy_effort calculates percentages correctly and handles zero division", {
  setup_mocks()
  on.exit(restore_mocks(), add = TRUE)
  
  res <- summarize_policy_effort(
    project = "test_proj", 
    spat = "mock_spat", 
    zone_spat = "zone_id"
  )
  df <- res$summary_data
  
  # closure_1, zone_1 (100 -> 0) should equal exactly -100%
  pct_closure_z1 <- df$Pct_Effort_Change[df$Simulation == "mod_1_closure_1" & df$Zone == "zone_1"]
  expect_equal(pct_closure_z1, -100)
  
  # closure_1, zone_2 (200 -> 300) should equal exactly +50%
  pct_closure_z2 <- df$Pct_Effort_Change[df$Simulation == "mod_1_closure_1" & df$Zone == "zone_2"]
  expect_equal(pct_closure_z2, 50)
  
  # zero_base, zone_1 (0 -> 50) should gracefully return NA to avoid Inf
  pct_zero_z1 <- df$Pct_Effort_Change[df$Simulation == "mod_1_zero_base" & df$Zone == "zone_1"]
  expect_true(is.na(pct_zero_z1))
})

# Test missing policy simulations -----------------------------------------------------------------
test_that("Error Handling: Missing policy simulations triggers error", {
  # Pass an empty list to simulate an empty or un-run database
  setup_mocks(return_sims = list()) 
  on.exit(restore_mocks(), add = TRUE)
  
  expect_error(
    summarize_policy_effort(
      project = "test_proj", 
      spat = "mock_spat", 
      zone_spat = "zone_id"
    ),
    "No policy simulations found"
  )
})