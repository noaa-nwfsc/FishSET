# -------------------------------------------------------------------------------------------------
# File: test-summarize_policy_welfare.R
# Purpose: To provide unit tests for the summarize_policy_welfare() function.
# Description: This script uses the 'testthat' framework to validate the reporting wrapper.
#              It mocks internal database functions to supply synthetic simulation data 
#              without requiring a real FishSET project structure.
#
# Scenarios tested:
#   - Full Execution: Validates that the function returns the expected list structure containing
#     the summary data frame, the bar plot, and the density (violin) plot.
#   - Mathematical Extraction: Validates that quantiles and means are correctly mapped to the
#     data frame and that 'baseline' scenarios are properly excluded from the draw distributions.
#   - Error Handling: Validates that an empty database triggers the correct error.
#
# Notes: Uses synthetic normal distributions to mimic parameter uncertainty draws.
# -------------------------------------------------------------------------------------------------

# Test Data Setup ---------------------------------------------------------------------------------
set.seed(42)

# Create synthetic policy simulation output mimicking the structure of fishset_policy objects
mock_sims <- list(
  mod_1_baseline = list(
    model_name = "mod_1",
    scenario = "baseline",
    results = list(
      welfare_draws = rep(0, 100),
      mean_welfare_loss = 0,
      quantiles = c("2.5%" = 0, "5%" = 0, "50%" = 0, "95%" = 0, "97.5%" = 0)
    ),
    metadata = list(N_obs = 1000)
  ),
  mod_1_closure_1 = list(
    model_name = "mod_1",
    scenario = "closure_1",
    results = list(
      welfare_draws = rnorm(100, mean = -50, sd = 10),
      mean_welfare_loss = -50,
      quantiles = c("2.5%" = -70, "5%" = -65, "50%" = -50, "95%" = -35, "97.5%" = -30)
    ),
    metadata = list(N_obs = 1000)
  ),
  mod_1_data_mod = list(
    model_name = "mod_1",
    scenario = "data_mod",
    results = list(
      welfare_draws = rnorm(100, mean = 25, sd = 5),
      mean_welfare_loss = 25,
      quantiles = c("2.5%" = 15, "5%" = 17, "50%" = 25, "95%" = 33, "97.5%" = 35)
    ),
    metadata = list(N_obs = 1000)
  )
)

# Environment Setup -------------------------------------------------------------------------------
orig_functions <- list(
  unserialize_table = getFromNamespace("unserialize_table", "FishSET")
)

setup_mocks <- function(return_sims = mock_sims) {
  assignInNamespace("unserialize_table", function(...) return_sims, ns = "FishSET")
}

restore_mocks <- function() {
  assignInNamespace("unserialize_table", orig_functions$unserialize_table, ns = "FishSET")
}

# Test structure and plots ------------------------------------------------------------------------
test_that("summarize_policy_welfare returns expected structure and valid plots", {
  setup_mocks()
  on.exit(restore_mocks(), add = TRUE)
  
  res <- summarize_policy_welfare(project = "test_proj")
  
  # Structure checks
  expect_type(res, "list")
  expect_named(res, c("summary_data", "plot_bar", "plot_density"))
  
  # Data frame checks
  expect_s3_class(res$summary_data, "data.frame")
  expect_equal(nrow(res$summary_data), 3) # 3 scenarios (baseline, closure, data mod)
  
  # Plot generation checks
  expect_s3_class(res$plot_bar, "ggplot")
  expect_s3_class(res$plot_density, "ggplot")
})

# Test maps and filter of baseline ----------------------------------------------------------------
test_that("summarize_policy_welfare correctly maps data and filters baseline draws", {
  setup_mocks()
  on.exit(restore_mocks(), add = TRUE)
  
  res <- summarize_policy_welfare(project = "test_proj")
  df <- res$summary_data
  
  # Check if metrics correctly mapped to the dataframe
  closure_row <- df[df$Simulation == "mod_1_closure_1", ]
  expect_equal(closure_row$Mean_Welfare_Per_Trip, -50)
  expect_equal(closure_row$Lower_95, -70)
  expect_equal(closure_row$Upper_95, -30)
  
  # Check that baseline was correctly included in the summary table
  expect_true("mod_1_baseline" %in% df$Simulation)
  
  # Verify that the plotting data (inside the ggplot object) successfully stripped the baseline
  # The density plot data should only contain 200 rows (100 draws * 2 non-baseline scenarios)
  density_data <- res$plot_density$data
  expect_equal(nrow(density_data), 200)
  expect_false("mod_1_baseline" %in% density_data$Simulation)
})

# Test missing policy simulations -----------------------------------------------------------------
test_that("Error Handling: Missing policy simulations triggers error", {
  # Pass an empty list to simulate an empty or un-run database
  setup_mocks(return_sims = list()) 
  on.exit(restore_mocks(), add = TRUE)
  
  expect_error(
    summarize_policy_welfare(project = "test_proj"),
    "No policy simulations found"
  )
})