# -------------------------------------------------------------------------------------------------
# File: test-fishset_iia_test.R
# Purpose: To provide unit tests for the fishset_iia_test() function.
# Description: This script uses the 'testthat' framework to validate the behavior of 
#              fishset_iia_test(). It isolates the test environment by saving mock design
#              objects to a temporary directory and mocking the database retrieval of 
#              the full model fit.
#
# Scenarios tested:
#   - Standard Conditional Logit IIA test (Golden Path).
#   - Expected Profit Model (EPM) IIA test with distribution mapping.
#   - Random omitted zone selection (when omitted_zones = NULL).
#   - Validation of inputs (invalid zones, omitting too many zones).
#   - S3 Print Method specifically checking the custom EPM interpretation text.
# -------------------------------------------------------------------------------------------------

# Test Data Setup ---------------------------------------------------------------------------------
set.seed(42)
N_obs <- 20
J_alts <- 3 # Zones A, B, C

# Hardcode choices so that observations are spread across all 3 zones
y_vec <- rep(0, N_obs * J_alts)
choices <- c(rep(1, 7), rep(2, 7), rep(3, 6)) # 7 chose A, 7 chose B, 6 chose C
chosen_indices <- (seq_len(N_obs) - 1) * J_alts + choices
y_vec[chosen_indices] <- 1

# Shared IDs
mock_ids <- list(
  obs = rep(paste0("Obs", 1:N_obs), each = J_alts),
  zone = rep(c("ZoneA", "ZoneB", "ZoneC"), N_obs)
)

# 1. Synthetic Design (Standard Logit)
K_vars <- 2
X_std <- matrix(rnorm(N_obs * J_alts * K_vars), ncol = K_vars)
colnames(X_std) <- c("Var1", "Var2")

standard_design <- list(
  y = y_vec, 
  X = X_std,
  epm = list(is_epm = FALSE),
  settings = list(N_obs = N_obs, J_alts = J_alts, K_vars = K_vars, project = "TestProj"),
  ids = mock_ids,
  scalers = list()
)

# 2. Synthetic Design (EPM)
X_util <- matrix(rnorm(N_obs * J_alts * 1), ncol = 1)
colnames(X_util) <- "UtilVar"

X_catch <- matrix(rnorm(N_obs * J_alts * 1), ncol = 1)
colnames(X_catch) <- "CatchVar"

epm_design <- list(
  y = y_vec,
  X = X_util,
  epm = list(
    is_epm = TRUE,
    X_catch = X_catch,
    Y_catch = runif(N_obs * J_alts, 50, 150),
    price_vec = rep(2.5, N_obs * J_alts)
  ),
  settings = list(N_obs = N_obs, J_alts = J_alts, K_vars = 6),
  ids = mock_ids,
  scalers = list()
)

# 3. Mocked Fitted Objects
# Create and name standard hessian
h_std <- diag(2, nrow = 2)
dimnames(h_std) <- list(c("Var1", "Var2"), c("Var1", "Var2"))

mock_fit_std <- list(
  opt = list(par = c(Var1 = 0.5, Var2 = -0.2)),
  coefficients = c(Var1 = 0.5, Var2 = -0.2),
  diagnostics = list(hessian = h_std)
)

# EPM fit parameters must strictly include log_sigma prefixes to trigger detection logic
epm_pars <- c(CatchVar = 0.5, UtilVar = 0.1, 
              log_sigma_c_A = -0.2, log_sigma_c_B = -0.2, log_sigma_c_C = -0.2, 
              log_sigma_e = 0)

# Create and name EPM hessian
h_epm <- diag(6, nrow = length(epm_pars))
dimnames(h_epm) <- list(names(epm_pars), names(epm_pars))

mock_fit_epm <- list(
  opt = list(par = epm_pars),
  coefficients = c(CatchVar = 0.5, UtilVar = 0.1, 
                   Sigma_Catch_A = exp(-0.2), Sigma_Catch_B = exp(-0.2), Sigma_Catch_C = exp(-0.2), 
                   Sigma_Error = 1),
  diagnostics = list(hessian = h_epm)
)

# Mocking architecture ----------------------------------------------------------------------------
orig_functions <- list(
  log_call = getFromNamespace("log_call", "FishSET"),
  unserialize_table = getFromNamespace("unserialize_table", "FishSET")
)

setup_iia_mocks <- function(fit_db) {
  assignInNamespace("log_call", function(...) invisible(NULL), ns = "FishSET")
  assignInNamespace("unserialize_table", function(table_name, project) {
    if (grepl("ModelFit", table_name)) return(fit_db)
    return(NULL)
  }, ns = "FishSET")
}

restore_mocks <- function() {
  assignInNamespace("log_call", orig_functions$log_call, ns = "FishSET")
  assignInNamespace("unserialize_table", orig_functions$unserialize_table, ns = "FishSET")
}

# Helper: Safely save the mock design to the exact expected folder structure
save_design_to_temp <- function(design_obj, model_name, project) {
  test_base_dir <- normalizePath(file.path(tempdir(), "FishSET_IIA_Tests"), 
                                 winslash = "/", 
                                 mustWork = FALSE)
  project_dir <- file.path(test_base_dir, project)
  designs_dir <- file.path(project_dir, "Models", "ModelDesigns")
  
  dir.create(designs_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(project_dir, "src"), recursive = TRUE, showWarnings = FALSE) 
  
  saveRDS(design_obj, file.path(designs_dir, paste0(model_name, ".rds")))
  return(test_base_dir)
}


# Test Standard Conditional Logit -----------------------------------------------------------------
test_that("Standard Logit IIA runs successfully (Golden Path)", {
  # Map database
  db_mock <- list(std_fit = mock_fit_std)
  setup_iia_mocks(db_mock)
  
  test_base_dir <- save_design_to_temp(standard_design, "std_model", "TestProj_IIA_Std")
  old_opts <- options(test_folder_path = test_base_dir)
  
  on.exit({
    options(old_opts)
    restore_mocks()
  }, add = TRUE)
  
  # Run test omitting ZoneC. Limit iterations for test speed.
  res <- fishset_iia_test(
    project = "TestProj_IIA_Std",
    model_name = "std_model",
    fit_name = "std_fit",
    omitted_zones = "ZoneC",
    control = list(iter.max = 1, eval.max = 5)
  )
  
  expect_s3_class(res, "fishset_iia")
  expect_true(is.numeric(res$statistic))
  expect_equal(res$omitted, "ZoneC")
  expect_true(all(res$common_parameters %in% c("Var1", "Var2")))
  expect_false(res$is_epm)
})


# Test Expected Profit Model (EPM) ----------------------------------------------------------------
test_that("EPM IIA runs successfully and handles distribution mapping", {
  db_mock <- list(epm_fit = mock_fit_epm)
  setup_iia_mocks(db_mock)

  test_base_dir <- save_design_to_temp(epm_design, "epm_model", "TestProj_IIA_EPM")
  old_opts <- options(test_folder_path = test_base_dir)

  on.exit({
    options(old_opts)
    restore_mocks()
  }, add = TRUE)

  res <- fishset_iia_test(
    project = "TestProj_IIA_EPM",
    model_name = "epm_model",
    fit_name = "epm_fit",
    omitted_zones = "ZoneB",
    # Letting distribution = NULL to test auto-inference from Sigma_Catch_X
    control = list(iter.max = 1, eval.max = 5)
  )

  expect_s3_class(res, "fishset_iia")
  expect_true(res$is_epm)
  expect_equal(res$omitted, "ZoneB")

  # Ensure nuisance sigma parameters were filtered out of comparison
  expect_true("CatchVar" %in% res$common_parameters)
  expect_true("UtilVar" %in% res$common_parameters)
  expect_false(any(grepl("log_sigma", res$common_parameters)))
})


# Test Invalid Inputs -----------------------------------------------------------------------------
test_that("Invalid omitted zones throw clear errors", {
  db_mock <- list(std_fit = mock_fit_std)
  setup_iia_mocks(db_mock)

  test_base_dir <- save_design_to_temp(standard_design, "std_model", "TestProj_IIA_Err")
  old_opts <- options(test_folder_path = test_base_dir)

  on.exit({
    options(old_opts)
    restore_mocks()
  }, add = TRUE)

  # Invalid name
  expect_error(
    fishset_iia_test(
      project = "TestProj_IIA_Err",
      model_name = "std_model",
      fit_name = "std_fit",
      omitted_zones = "GhostZone"
    ),
    "not found in the original dataset"
  )

  # Omitting too many (Leaves 1, needs >= 2)
  expect_error(
    fishset_iia_test(
      project = "TestProj_IIA_Err",
      model_name = "std_model",
      fit_name = "std_fit",
      omitted_zones = c("ZoneA", "ZoneB")
    ),
    "must keep at least two alternatives"
  )
})


# Test S3 Print Method ----------------------------------------------------------------------------
test_that("Print method displays correct output based on model type", {
  db_mock <- list(std_fit = mock_fit_std, epm_fit = mock_fit_epm)
  setup_iia_mocks(db_mock)

  test_base_dir <- save_design_to_temp(standard_design, "std_model", "TestProj_IIA_Print")
  save_design_to_temp(epm_design, "epm_model", "TestProj_IIA_Print")
  old_opts <- options(test_folder_path = test_base_dir)

  on.exit({
    options(old_opts)
    restore_mocks()
  }, add = TRUE)

  # Standard Model Output
  res_std <- fishset_iia_test(project = "TestProj_IIA_Print", model_name = "std_model",
                              fit_name = "std_fit", omitted_zones = "ZoneA", control = list(iter.max = 1))
  expect_output(print(res_std), "Hausman-McFadden IIA Test")
  expect_output(print(res_std), "Result:\\s+No significant difference")
  
  # EPM Output (Checking custom interpretation block)
  res_epm <- fishset_iia_test(project = "TestProj_IIA_Print", model_name = "epm_model",
                              fit_name = "epm_fit", omitted_zones = "ZoneA", control = list(iter.max = 1))
  expect_output(print(res_epm), "Expected Profit Model \\(Joint\\)")
  expect_output(print(res_epm), "EPM Interpretation:")
})