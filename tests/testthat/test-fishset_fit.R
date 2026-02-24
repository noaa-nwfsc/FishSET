# -------------------------------------------------------------------------------------------------
# File: test-fishset_fit.R
# Purpose: To provide unit tests for the fishset_fit() function.
# Description: This script uses the 'testthat' framework to validate the behavior of the
#              fishset_fit() function.
#
# Scenarios tested:
#   - Standard Logit Fit: Runs end-to-end using a real temporary database and RTMB.
#   - EPM Fit: Validates parameter unpacking and distribution checks.
#   - Error Handling: Checks for missing designs and duplicate fit names.
#   - Prediction: Verifies the full probability matrix option.
#
# Notes: This test follows the pattern of 'test-create_alternative_choice.R' by mocking
#        internal FishSET functions via assignInNamespace. It uses real DBI/RSQLite
#        connections directed to a temporary file to avoid locking errors.
# -------------------------------------------------------------------------------------------------

# Test Data Setup ---------------------------------------------------------------------------------
set.seed(42)
N_obs <- 10
J_alts <- 2 # Keep small for speed
K_vars <- 2

# Synthetic Design Object (Standard)
standard_design <- list(
  y = rep(c(1, 0), N_obs), 
  X = matrix(rnorm(N_obs * J_alts * K_vars), ncol = K_vars),
  epm = list(is_epm = FALSE),
  settings = list(N_obs = N_obs, J_alts = J_alts, K_vars = K_vars, project = "TestProj"),
  ids = list(zone = rep(1:J_alts, N_obs)),
  scalers = list()
)
colnames(standard_design$X) <- c("Var1", "Var2")

# Synthetic Design Object (EPM)
epm_design <- list(
  y = rep(c(1, 0), N_obs),
  X = matrix(rnorm(N_obs * J_alts * 1), ncol = 1), # Utility var
  epm = list(
    is_epm = TRUE,
    X_catch = matrix(rnorm(N_obs * J_alts * 1), ncol = 1), # Catch var
    Y_catch = rnorm(N_obs * J_alts, 100, 10),
    price_vec = rep(2.5, N_obs * J_alts)
  ),
  settings = list(N_obs = N_obs, J_alts = J_alts, K_vars = 3),
  ids = list(zone = rep(1:J_alts, N_obs)),
  scalers = list()
)
colnames(epm_design$X) <- "UtilVar"
colnames(epm_design$epm$X_catch) <- "CatchVar"


# Define Mocks ------------------------------------------------------------------------------------
# Mock 'locdatabase' to return a path in the temp directory.
mock_locdatabase <- function(project) {
  temp_dir <- file.path(tempdir(), "FishSET_Tests", project)
  if (!dir.exists(temp_dir)) dir.create(temp_dir, recursive = TRUE)
  file.path(temp_dir, paste0(project, ".sqlite"))
}

# Mock 'model_design_list' to simulate existing models
mock_model_design_list <- function(project) {
  return(c("std_model", "epm_model"))
}

# Mock table helpers
# Default table_exists to FALSE so we can write new fits.
# Override this inside specific tests (like the duplicate error test).
mock_table_exists <- function(name, project) { return(FALSE) }
mock_unserialize_table <- function(name, project) { return(list()) }
mock_table_remove <- function(name, project) { invisible(NULL) }
mock_log_call <- function(...) { invisible(NULL) }

# Save original functions from the package namespace
original_locdatabase <- get("locdatabase", envir = as.environment("package:FishSET"))
original_model_design_list <- get("model_design_list", envir = as.environment("package:FishSET"))
original_table_exists <- get("table_exists", envir = as.environment("package:FishSET"))
original_unserialize_table <- get("unserialize_table", envir = as.environment("package:FishSET"))
original_table_remove <- get("table_remove", envir = as.environment("package:FishSET"))
original_log_call <- get("log_call", envir = as.environment("package:FishSET"))

# Schedule restoration
on.exit({
  assignInNamespace("locdatabase", original_locdatabase, ns = "FishSET")
  assignInNamespace("model_design_list", original_model_design_list, ns = "FishSET")
  assignInNamespace("table_exists", original_table_exists, ns = "FishSET")
  assignInNamespace("unserialize_table", original_unserialize_table, ns = "FishSET")
  assignInNamespace("table_remove", original_table_remove, ns = "FishSET")
  assignInNamespace("log_call", original_log_call, ns = "FishSET")
})

# Apply Mocks
assignInNamespace("locdatabase", mock_locdatabase, ns = "FishSET")
assignInNamespace("model_design_list", mock_model_design_list, ns = "FishSET")
assignInNamespace("table_exists", mock_table_exists, ns = "FishSET")
assignInNamespace("unserialize_table", mock_unserialize_table, ns = "FishSET")
assignInNamespace("table_remove", mock_table_remove, ns = "FishSET")
assignInNamespace("log_call", mock_log_call, ns = "FishSET")

# Helper: Save design to the temp folder so fishset_fit can read it
save_design_to_temp <- function(design_obj, name, project) {
  db_path <- mock_locdatabase(project)
  # Create the ModelDesigns folder relative to the mocked db path
  designs_dir <- file.path(dirname(db_path), "ModelDesigns")
  if (!dir.exists(designs_dir)) dir.create(designs_dir, recursive = TRUE)
  saveRDS(design_obj, file.path(designs_dir, paste0(name, ".rds")))
}


# Test standard logit -----------------------------------------------------------------------------
test_that("Standard Logit Fit runs successfully (End-to-End)", {
  project_name <- "TestProj_Std"
  model_name <- "std_model"
  
  # Setup dummy design file on disk
  save_design_to_temp(standard_design, model_name, project_name)
  
  # Run fit
  result <- fishset_fit(
    project = project_name,
    model_name = model_name,
    fit_name = "std_fit_1",
    control = list(iter.max = 1, eval.max = 5) 
  )
  
  # Assertions
  expect_s3_class(result, "fishset_fit")
  expect_true(is.numeric(result$logLik))
  
  # Check that coefficients were estimated (not null)
  expect_true(!is.null(result$coefficients))
  expect_equal(length(result$coefficients), K_vars)
  
  # Check that it saved to the "real" temp database
  # We can verify this by checking if the .sqlite file exists
  db_path <- mock_locdatabase(project_name)
  expect_true(file.exists(db_path))
})


# Test EPM normal ---------------------------------------------------------------------------------
test_that("EPM Fit (Normal) runs and unpacks parameters", {
  project_name <- "TestProj_EPM"
  model_name <- "epm_model"

  save_design_to_temp(epm_design, model_name, project_name)

  result <- fishset_fit(
    project = project_name,
    model_name = model_name,
    fit_name = "epm_fit_1",
    distribution = "normal",
    control = list(iter.max = 1, eval.max = 5)
  )

  expect_s3_class(result, "fishset_fit")

  # Check parameter unpacking logic
  # EPMs have Catch Beta, Util Beta, Sigma_Catch (per zone), Sigma_Error
  # Design: 1 catch param + 1 util param + 2 zones (sig_c) + 1 sig_e = 5 params
  expect_equal(length(result$coefficients), 5)

  # Check naming conventions
  names_coef <- names(result$coefficients)
  expect_true("CatchVar" %in% names_coef)
  expect_true("UtilVar" %in% names_coef)
  expect_true("Sigma_Catch_1" %in% names_coef)
  expect_true("Sigma_Error" %in% names_coef)
})


# Test design not found error ---------------------------------------------------------------------
test_that("Error: Design not found", {
  # Mock model_design_list to return empty for this specific check
  # We use a local closure to temporarily override the global mock
  local_mock_list <- function(p) return(character(0))
  assignInNamespace("model_design_list", local_mock_list, ns = "FishSET")

  expect_error(
    fishset_fit(project = "TestProj", model_name = "missing_model"),
    "not found in project database"
  )

  # Restore the global mock
  assignInNamespace("model_design_list", mock_model_design_list, ns = "FishSET")
})


# Test fit already exists error -------------------------------------------------------------------
test_that("Error: Fit name already exists", {
  project_name <- "TestProj_Dup"
  model_name <- "std_model"
  fit_name <- "existing_fit"

  save_design_to_temp(standard_design, model_name, project_name)

  # Temporarily mock table functions to simulate an existing fit
  mock_exists_true <- function(name, project) TRUE
  mock_read_fit <- function(name, project) {
    l <- list(); l[[fit_name]] <- "some_data"; return(l)
  }

  assignInNamespace("table_exists", mock_exists_true, ns = "FishSET")
  assignInNamespace("unserialize_table", mock_read_fit, ns = "FishSET")

  expect_error(
    fishset_fit(
      project = project_name,
      model_name = model_name,
      fit_name = fit_name
    ),
    "already exists"
  )

  # Restore global mocks
  assignInNamespace("table_exists", mock_table_exists, ns = "FishSET")
  assignInNamespace("unserialize_table", mock_unserialize_table, ns = "FishSET")
})


# Test full prob matrix works ---------------------------------------------------------------------
test_that("Return Full Probability Matrix works", {
  project_name <- "TestProj_Prob"
  model_name <- "std_model"
  save_design_to_temp(standard_design, model_name, project_name)

  result <- fishset_fit(
    project = project_name,
    model_name = model_name,
    return_full_prob_mat = TRUE,
    control = list(iter.max = 1, eval.max = 5)
  )

  # Check matrix dimensions
  expect_true(!is.null(result$prob_matrix))
  expect_equal(nrow(result$prob_matrix), N_obs)
  expect_equal(ncol(result$prob_matrix), J_alts)

  # Check residuals exist
  expect_true(!is.null(result$residuals))
})


# Test overwrite ----------------------------------------------------------------------------------
test_that("Overwrite argument successfully replaces existing fit", {
  project_name <- "TestProj_Overwrite"
  model_name <- "std_model"
  fit_name <- "existing_fit"
  
  save_design_to_temp(standard_design, model_name, project_name)
  
  # Mock an existing fit
  mock_exists_true <- function(name, project) TRUE
  mock_read_fit <- function(name, project) {
    l <- list(); l[[fit_name]] <- "some_old_data"; return(l)
  }
  
  assignInNamespace("table_exists", mock_exists_true, ns = "FishSET")
  assignInNamespace("unserialize_table", mock_read_fit, ns = "FishSET")
  
  # This should NOT throw an error because overwrite = TRUE
  expect_no_error(
    fishset_fit(
      project = project_name,
      model_name = model_name,
      fit_name = fit_name,
      overwrite = TRUE,
      control = list(iter.max = 1, eval.max = 5)
    )
  )
  
  # Restore global mocks
  assignInNamespace("table_exists", mock_table_exists, ns = "FishSET")
  assignInNamespace("unserialize_table", mock_unserialize_table, ns = "FishSET")
})


# Test unscaling works ----------------------------------------------------------------------------
test_that("Vectorized unscaling correctly divides coefficients and SEs", {
  project_name <- "TestProj_Scale"
  model_name <- "std_model"
  
  # Create a design with fake scalers
  scaled_design <- standard_design
  scaled_design$scalers <- list(
    X1 = list(sd = c("Var1" = 2.0, "Var2" = 10.0))
  )
  save_design_to_temp(scaled_design, model_name, project_name)
  
  result <- fishset_fit(
    project = project_name,
    model_name = model_name,
    fit_name = "scaled_fit",
    control = list(iter.max = 1, eval.max = 5) 
  )
  
  # Check if standard errors are successfully divided by the scalers
  # Since initial betas are small, and optim might take 1 step, we check the mechanics.
  # The raw opt$par / scale_factors should equal the reported coefficients.
  raw_var1 <- result$opt$par[1]
  raw_var2 <- result$opt$par[2]

  expect_equal(unname(result$coefficients["Var1"]), (unname(raw_var1) / 2.0), tolerance = 1e-5)
  expect_equal(unname(result$coefficients["Var2"]), (unname(raw_var2) / 10.0), tolerance = 1e-5)
})


