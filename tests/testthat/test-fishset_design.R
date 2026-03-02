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

# Define Mocks ------------------------------------------------------------------------------------
# Mock database path retrieval to use tempdir
mock_locdatabase <- function(project) {
  # Mimic: project_folder/database_name
  # This ensures dirname(path) returns the project folder
  file.path(tempdir(), project, paste0(project, ".sqlite"))
}

# Mock model_design_list to control "existing" models
# By default, returns empty (no models exist)
mock_model_design_list <- function(project) {
  return(character(0))
}

# Mock data loading to return our test_data
mock_unserialize_table <- function(name, project) {
  # The function expects a list containing the dataframe
  out <- list()
  out[["my_formatted_data"]] <- test_data
  return(out)
}

# Mock logging to silence output
mock_log_call <- function(...) {
  invisible(NULL)
}

# Save original functions
original_locdatabase <- get("locdatabase", envir = as.environment("package:FishSET"))
original_model_design_list <- get("model_design_list", envir = as.environment("package:FishSET"))
original_unserialize_table <- get("unserialize_table", envir = as.environment("package:FishSET"))
original_log_call <- get("log_call", envir = as.environment("package:FishSET"))

# Schedule restoration
on.exit({
  assignInNamespace("locdatabase", original_locdatabase, ns = "FishSET")
  assignInNamespace("model_design_list", original_model_design_list, ns = "FishSET")
  assignInNamespace("unserialize_table", original_unserialize_table, ns = "FishSET")
  assignInNamespace("log_call", original_log_call, ns = "FishSET")
})

# Apply mocks
assignInNamespace("locdatabase", mock_locdatabase, ns = "FishSET")
assignInNamespace("model_design_list", mock_model_design_list, ns = "FishSET")
assignInNamespace("unserialize_table", mock_unserialize_table, ns = "FishSET")
assignInNamespace("log_call", mock_log_call, ns = "FishSET")


# Standard conditional logit ----------------------------------------------------------------------
test_that("Standard Conditional Logit (Part 1 only) runs successfully", {
  project_name <- "TestProj"
  model_name <- "clogit_test"
  
  # Ensure the directory structure exists for saving
  db_path <- mock_locdatabase(project_name)
  dir.create(dirname(db_path), recursive = TRUE, showWarnings = FALSE)
  
  # Run function
  suppressMessages(
    fishset_design(
      formula = chosen ~ distance + expected_catch,
      project = project_name,
      model_name = model_name,
      formatted_data_name = "my_formatted_data",
      unique_obs_id = "haul_id",
      zone_id = "zone_id"
    )  
  )

  # Check if file was saved
  save_path <- file.path(dirname(db_path), "ModelDesigns", paste0(model_name, ".rds"))
  # Note: logic handles .qs2 if available, but checking generic existence is safer
  files_saved <- list.files(file.path(dirname(db_path), "ModelDesigns"))
  expect_true(any(grepl(model_name, files_saved)))
  
  # Load and validate content
  # Determine which file was saved (qs2 or rds)
  saved_file <- files_saved[grep(model_name, files_saved)][1]
  full_path <- file.path(dirname(db_path), "ModelDesigns", saved_file)
  
  if (grepl("\\.qs2$", saved_file)) {
    design_obj <- qs2::qs_read(full_path)
  } else {
    design_obj <- readRDS(full_path)
  }
  
  expect_type(design_obj, "list")
  expect_equal(class(design_obj), "fishset_design")
  expect_equal(design_obj$settings$N_obs, N_obs)
  expect_equal(design_obj$settings$J_alts, J_alts)
  
  # Check X dimensions (intercept dropped automatically?)
  # formula: chosen ~ distance + expected_catch. 2 vars.
  expect_equal(ncol(design_obj$X), 2) 
})


# Test duplicate model names error ----------------------------------------------------------------
test_that("Error: Duplicate model name throws error", {
  # Mock model_design_list to return "existing_model"
  mock_existing <- function(project) { return("existing_model") }
  assignInNamespace("model_design_list", mock_existing, ns = "FishSET")

  expect_error(
    fishset_design(
      formula = chosen ~ distance,
      project = "TestProj",
      model_name = "existing_model",
      formatted_data_name = "my_formatted_data",
      unique_obs_id = "haul_id",
      zone_id = "zone_id"
    ),
    "already exists"
  )

  # Restore mock for subsequent tests
  assignInNamespace("model_design_list", mock_model_design_list, ns = "FishSET")
})


# Test missing data error -------------------------------------------------------------------------
test_that("Error: Missing data name throws error", {
  expect_error(
    fishset_design(
      formula = chosen ~ distance,
      project = "TestProj",
      model_name = "new_model",
      formatted_data_name = "NON_EXISTENT_DATA",
      unique_obs_id = "haul_id",
      zone_id = "zone_id"
    ),
    "Formatted data name not found"
  )
})


# Test logit with two parts -----------------------------------------------------------------------
test_that("Interaction terms (Part 2 formula) are generated correctly", {
  # Part 2 variables (vessel_len) should interact with zone constants
  # Formula: chosen ~ distance | vessel_len
  # J=3, so vessel_len should create J-1 = 2 columns (vessel_len:zone2, vessel_len:zone3)
  # Total cols = 1 (distance) + 2 (interactions) = 3
  
  project_name <- "TestProj"
  model_name <- "interact_test"

  suppressMessages(
    fishset_design(
      formula = chosen ~ distance | vessel_len,
      project = project_name,
      model_name = model_name,
      formatted_data_name = "my_formatted_data",
      unique_obs_id = "haul_id",
      zone_id = "zone_id"
    )  
  )
  
  # Retrieve object
  db_path <- mock_locdatabase(project_name)
  # Find file (rds or qs2)
  files <- list.files(file.path(dirname(db_path), "ModelDesigns"), full.names = TRUE)
  # Basic grep to ensure we get the file we just made
  target_file <- files[grep(model_name, files)][1]

  if (grepl("\\.qs2$", target_file)) {
    obj <- qs2::qs_read(target_file)
  } else {
    obj <- readRDS(target_file)
  }

  # Check columns
  col_names <- colnames(obj$X)
  expect_true("distance" %in% col_names)
  # Check for interaction naming convention usually "var:zone_idVal"
  expect_true(any(grepl("vessel_len", col_names)))
  expect_equal(ncol(obj$X), 3)
})


# Test EPM normal ---------------------------------------------------------------------------------
test_that("Expected Profit Model (EPM) configuration works", {
  project_name <- "TestProj"
  model_name <- "epm_test"

  # Main: chosen ~ distance | vessel_len
  # Catch: actual_catch ~ vessel_len:ZoneID
  # 'vessel_len' is in Part 2 of main formula, so this should pass.

  suppressMessages(
    fishset_design(
      formula = chosen ~ distance | vessel_len,
      project = project_name,
      model_name = model_name,
      formatted_data_name = "my_formatted_data",
      unique_obs_id = "haul_id",
      zone_id = "zone_id",
      catch_formula = actual_catch ~ vessel_len:zone_id,
      price_var = "price"
    )  
  )
  
  # Retrieve object
  db_path <- mock_locdatabase(project_name)
  files <- list.files(file.path(dirname(db_path), "ModelDesigns"), full.names = TRUE)
  target_file <- files[grep(model_name, files)][1]

  if (grepl("\\.qs2$", target_file)) {
    obj <- qs2::qs_read(target_file)
  } else {
    obj <- readRDS(target_file)
  }

  expect_true(obj$epm$is_epm)
  expect_equal(length(obj$epm$Y_catch), nrow(test_data))
  expect_equal(length(obj$epm$price_vec), nrow(test_data))
})


# Test EPM missing price error --------------------------------------------------------------------
test_that("EPM Error: Missing price variable", {
  expect_error(
    fishset_design(
      formula = chosen ~ distance,
      project = "TestProj",
      model_name = "epm_fail",
      formatted_data_name = "my_formatted_data",
      unique_obs_id = "haul_id",
      zone_id = "zone_id",
      catch_formula = actual_catch ~ distance
      # Missing price_var
    ),
    "must also be specified"
  )
})


# Test scaler functionality -----------------------------------------------------------------------
test_that("Scaling functionality stores scalers", {
  project_name <- "TestProj"
  model_name <- "scale_test"

  suppressMessages(
    fishset_design(
      formula = chosen ~ distance + expected_catch,
      project = project_name,
      model_name = model_name,
      formatted_data_name = "my_formatted_data",
      unique_obs_id = "haul_id",
      zone_id = "zone_id",
      scale = TRUE
    )
  )
  
  # Retrieve object
  db_path <- mock_locdatabase(project_name)
  files <- list.files(file.path(dirname(db_path), "ModelDesigns"), full.names = TRUE)
  target_file <- files[grep(model_name, files)][1]

  if (grepl("\\.qs2$", target_file)) {
    obj <- qs2::qs_read(target_file)
  } else {
    obj <- readRDS(target_file)
  }

  # Check that scalers list is populated
  expect_true(length(obj$scalers) > 0)
  expect_true("X1" %in% names(obj$scalers))
  expect_true("mu" %in% names(obj$scalers$X1))
})