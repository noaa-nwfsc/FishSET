# -------------------------------------------------------------------------------------------------
# File: test-fishset_design.R
# Purpose: To provide unit tests for the fishset_design() function.
# Description: This script uses the 'testthat' framework to validate the behavior of the
#              fishset_design() function.
#
# Scenarios tested:
#   - Standard Conditional Logit (Part 1 formula only).
#   - Error handling: Model design already exists.
#   - Error handling: Missing formatted data or columns.
#   - Zonal Logit with trip-specific variables (Part 2 formula / Interactions).
#   - Expected Profit Model (EPM) configuration (catch_formula validation).
#   - Scaling option functionality.
#
# Notes: This test script is designed to be run within the FishSET package structure.
#        It mocks internal database and IO functions (e.g., unserialize_table, log_call)
#        to run in isolation without requiring actual project databases.
# -------------------------------------------------------------------------------------------------

# Test Data Setup ---------------------------------------------------------------------------------
set.seed(42)
N_obs <- 20
J_alts <- 3

# Create a synthetic "long format" dataframe
test_data <- data.frame(
  haul_id = rep(1:N_obs, each = J_alts),
  zone_id = factor(rep(1:J_alts, times = N_obs)),
  chosen = 0,
  distance = runif(N_obs * J_alts, 10, 100), # Varying by alt
  expected_catch = runif(N_obs * J_alts, 50, 500), # Varying by alt
  vessel_len = rep(runif(N_obs, 20, 50), each = J_alts), # Fixed per haul
  price = rep(runif(N_obs, 2, 5), each = J_alts), # Fixed per haul (for EPM)
  actual_catch = runif(N_obs * J_alts, 0, 1000) # Continuous outcome (for EPM)
)

# Ensure one choice per haul
for(i in 1:N_obs) {
  idx <- which(test_data$haul_id == i)
  test_data$chosen[idx[sample(1:J_alts, 1)]] <- 1
}

project_name <- "TestProj"

# Environment Setup -------------------------------------------------------------------------------
# Preserve original functions to prevent bleeding
orig_functions <- list(
  log_call = getFromNamespace("log_call", "FishSET")
)

# Helpers to safely apply and remove mocks
setup_mocks <- function() {
  assignInNamespace("log_call", function(...) invisible(NULL), ns = "FishSET")
}

restore_mocks <- function() {
  assignInNamespace("log_call", orig_functions$log_call, ns = "FishSET")
}

# Helper: Set up isolated directory per test
setup_test_env <- function(project_name) {
  test_base_dir <- normalizePath(file.path(tempdir(), "FishSET_Design_Tests"), 
                                 winslash = "/", 
                                 mustWork = FALSE)
  formatted_dir <- file.path(test_base_dir, project_name, "Models", "FormattedData")
  dir.create(formatted_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(test_base_dir, project_name, "src"), recursive = TRUE, showWarnings = FALSE)
  
  # Write the dummy formatted data
  saveRDS(list(my_formatted_data = test_data), 
          file.path(formatted_dir, paste0(project_name, "LongFormatData.rds")))
  return(test_base_dir)
}

# Helper: Read the design output handling both qs2 and rds
read_design_output <- function(project_name, model_name, base_dir) {
  md_dir <- file.path(base_dir, project_name, "Models", "ModelDesigns")
  qs2_file <- file.path(md_dir, paste0(model_name, ".qs2"))
  rds_file <- file.path(md_dir, paste0(model_name, ".rds"))
  
  if (file.exists(qs2_file) && requireNamespace("qs2", quietly = TRUE)) {
    return(qs2::qs_read(qs2_file))
  } else if (file.exists(rds_file)) {
    return(readRDS(rds_file))
  }
  stop(paste("Design file not found for model:", model_name))
}

# Standard conditional logit ----------------------------------------------------------------------
test_that("Standard Conditional Logit (Part 1 only) runs successfully", {
  setup_mocks()
  on.exit(restore_mocks(), add = TRUE) # Bulletproof restoration scoped to this block
  
  test_base_dir <- setup_test_env(project_name)
  withr::local_options(list(test_folder_path = test_base_dir))
  
  suppressMessages(
    fishset_design(formula = chosen ~ distance + expected_catch, 
                   project = project_name, 
                   model_name = "clogit_test",
                   formatted_data_name = "my_formatted_data", 
                   unique_obs_id = "haul_id", 
                   zone_id = "zone_id")
  )
  
  files_saved <- list.files(file.path(test_base_dir, project_name, "Models", "ModelDesigns"))
  expect_true(any(grepl("clogit_test", files_saved)))
})


# Test duplicate model names error ----------------------------------------------------------------
test_that("Error: Duplicate model name throws error", {
  setup_mocks()
  on.exit(restore_mocks(), add = TRUE)
  
  test_base_dir <- setup_test_env(project_name)
  withr::local_options(list(test_folder_path = test_base_dir))
  
  # Natively create a dummy file to trigger the duplicate error gracefully
  md_dir <- file.path(test_base_dir, project_name, "Models", "ModelDesigns")
  dir.create(md_dir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(list(), file.path(md_dir, "existing_model.rds"))
  
  expect_error(
    fishset_design(formula = chosen ~ distance, 
                   project = project_name, 
                   model_name = "existing_model",
                   formatted_data_name = "my_formatted_data", 
                   unique_obs_id = "haul_id", 
                   zone_id = "zone_id"),
    "already exists"
  )
})


# Test missing data error -------------------------------------------------------------------------
test_that("Error: Missing data name throws error", {
  setup_mocks()
  on.exit(restore_mocks(), add = TRUE)
  
  test_base_dir <- setup_test_env(project_name)
  withr::local_options(list(test_folder_path = test_base_dir))
  
  expect_error(
    fishset_design(formula = chosen ~ distance, 
                   project = project_name, 
                   model_name = "new_model",
                   formatted_data_name = "NON_EXISTENT_DATA", 
                   unique_obs_id = "haul_id", 
                   zone_id = "zone_id"),
    "Formatted data name not found"
  )
})


# Test logit with two parts -----------------------------------------------------------------------
test_that("Interaction terms (Part 2 formula) are generated correctly", {
  setup_mocks()
  on.exit(restore_mocks(), add = TRUE)
  
  test_base_dir <- setup_test_env(project_name)
  withr::local_options(list(test_folder_path = test_base_dir))
  
  suppressMessages(
    fishset_design(formula = chosen ~ distance | vessel_len, 
                   project = project_name, 
                   model_name = "interact_test",
                   formatted_data_name = "my_formatted_data", 
                   unique_obs_id = "haul_id", 
                   zone_id = "zone_id")  
  )
  
  obj <- read_design_output(project_name, "interact_test", test_base_dir)
  expect_true(any(grepl("vessel_len", colnames(obj$X))))
})


# Test EPM normal ---------------------------------------------------------------------------------
test_that("Expected Profit Model (EPM) configuration works", {
  setup_mocks()
  on.exit(restore_mocks(), add = TRUE)
  
  test_base_dir <- setup_test_env(project_name)
  withr::local_options(list(test_folder_path = test_base_dir))
  
  suppressMessages(
    fishset_design(formula = chosen ~ distance | vessel_len, 
                   project = project_name, 
                   model_name = "epm_test",
                   formatted_data_name = "my_formatted_data", 
                   unique_obs_id = "haul_id", 
                   zone_id = "zone_id",
                   catch_formula = actual_catch ~ vessel_len:zone_id, 
                   price_var = "price")  
  )
  
  obj <- read_design_output(project_name, "epm_test", test_base_dir)
  expect_true(obj$epm$is_epm)
})


# Test EPM missing price error --------------------------------------------------------------------
test_that("EPM Error: Missing price variable", {
  setup_mocks()
  on.exit(restore_mocks(), add = TRUE)
  
  test_base_dir <- setup_test_env(project_name)
  withr::local_options(list(test_folder_path = test_base_dir))
  
  expect_error(
    fishset_design(formula = chosen ~ distance, 
                   project = project_name, 
                   model_name = "epm_fail",
                   formatted_data_name = "my_formatted_data", 
                   unique_obs_id = "haul_id", 
                   zone_id = "zone_id",
                   catch_formula = actual_catch ~ distance),
    "must also be specified"
  )
})


# Test scaler functionality -----------------------------------------------------------------------
test_that("Scaling functionality stores scalers", {
  setup_mocks()
  on.exit(restore_mocks(), add = TRUE)
  
  test_base_dir <- setup_test_env(project_name)
  withr::local_options(list(test_folder_path = test_base_dir))
  
  suppressMessages(
    fishset_design(formula = chosen ~ distance + expected_catch, 
                   project = project_name, 
                   model_name = "scale_test",
                   formatted_data_name = "my_formatted_data", 
                   unique_obs_id = "haul_id", 
                   zone_id = "zone_id",
                   scale = TRUE)
  )
  
  obj <- read_design_output(project_name, "scale_test", test_base_dir)
  expect_true(length(obj$scalers) > 0)
  expect_true("X1" %in% names(obj$scalers))
})