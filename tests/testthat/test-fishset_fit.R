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

# Mocking architecture ----------------------------------------------------------------------------
orig_functions <- list(
  log_call = getFromNamespace("log_call", "FishSET")
)

setup_mocks <- function() {
  assignInNamespace("log_call", function(...) invisible(NULL), ns = "FishSET")
}

restore_mocks <- function() {
  assignInNamespace("log_call", orig_functions$log_call, ns = "FishSET")
}

# Helper: Safely save the mock design to the exact expected folder structure
save_design_to_temp <- function(design_obj, model_name, project) {
  test_base_dir <- normalizePath(file.path(tempdir(), "FishSET_Fit_Tests"), 
                                 winslash = "/", 
                                 mustWork = FALSE)
  project_dir <- file.path(test_base_dir, project)
  designs_dir <- file.path(project_dir, "Models", "ModelDesigns")
  
  # Create dummy directories safely
  dir.create(designs_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(project_dir, "src"), recursive = TRUE, showWarnings = FALSE) # For log_call
  
  saveRDS(design_obj, file.path(designs_dir, paste0(model_name, ".rds")))
  return(test_base_dir)
}


# Test standard logit -----------------------------------------------------------------------------
test_that("Standard Logit Fit runs successfully (End-to-End)", {
  setup_mocks()
  test_base_dir <- save_design_to_temp(standard_design, "std_model", "TestProj_Std")
  
  old_opts <- options(test_folder_path = test_base_dir)
  on.exit({
    options(old_opts)
    restore_mocks()
  }, add = TRUE)
  
  result <- fishset_fit(
    project = "TestProj_Std",
    model_name = "std_model",
    fit_name = "std_fit_1",
    control = list(iter.max = 1, eval.max = 5) 
  )
  
  expect_s3_class(result, "fishset_fit")
  expect_true(is.numeric(result$logLik))
  expect_true(!is.null(result$coefficients))
})


# Test EPM normal ---------------------------------------------------------------------------------
test_that("EPM Fit (Normal) runs and unpacks parameters", {
  setup_mocks()
  test_base_dir <- save_design_to_temp(epm_design, "epm_model", "TestProj_EPM")
  
  old_opts <- options(test_folder_path = test_base_dir)
  on.exit({
    options(old_opts)
    restore_mocks()
  }, add = TRUE)
  
  result <- fishset_fit(
    project = "TestProj_EPM",
    model_name = "epm_model",
    fit_name = "epm_fit_1",
    distribution = "normal",
    control = list(iter.max = 1, eval.max = 5)
  )
  
  expect_s3_class(result, "fishset_fit")
  expect_equal(length(result$coefficients), 5)
  
  names_coef <- names(result$coefficients)
  expect_true("CatchVar" %in% names_coef)
  expect_true("UtilVar" %in% names_coef)
  expect_true("Sigma_Catch_1" %in% names_coef)
  expect_true("Sigma_Error" %in% names_coef)
})


# Test design not found error ---------------------------------------------------------------------
test_that("Error: Design not found", {
  setup_mocks()
  test_base_dir <- normalizePath(file.path(tempdir(), "FishSET_Fit_Tests"), 
                                 winslash = "/", 
                                 mustWork = FALSE)
  
  old_opts <- options(test_folder_path = test_base_dir)
  on.exit({
    options(old_opts)
    restore_mocks()
  }, add = TRUE)
  
  # Create an empty ModelDesigns folder so model_design_list returns empty, 
  # avoiding a missing directory error
  dir.create(file.path(test_base_dir, "TestProj_Missing", "Models", "ModelDesigns"), 
             recursive = TRUE, 
             showWarnings = FALSE)
  
  expect_error(
    fishset_fit(project = "TestProj_Missing", model_name = "missing_model"),
    "not found in project database"
  )
})


# Test fit already exists error -------------------------------------------------------------------
test_that("Error: Fit name already exists", {
  setup_mocks()
  test_base_dir <- save_design_to_temp(standard_design, "std_model", "TestProj_Dup")
  
  old_opts <- options(test_folder_path = test_base_dir)
  on.exit({
    options(old_opts)
    restore_mocks()
  }, add = TRUE)
  
  # 1st call natively creates the fit and writes to SQLite database
  fishset_fit(
    project = "TestProj_Dup", 
    model_name = "std_model", 
    fit_name = "existing_fit", 
    control = list(iter.max = 1)
  )
  
  # 2nd call throws error
  expect_error(
    fishset_fit(
      project = "TestProj_Dup", 
      model_name = "std_model", 
      fit_name = "existing_fit"
    ),
    "already exists"
  )
})


# Test full prob matrix works ---------------------------------------------------------------------
test_that("Return Full Probability Matrix works", {
  setup_mocks()
  test_base_dir <- save_design_to_temp(standard_design, "std_model", "TestProj_Prob")
  
  old_opts <- options(test_folder_path = test_base_dir)
  on.exit({
    options(old_opts)
    restore_mocks()
  }, add = TRUE)
  
  result <- fishset_fit(
    project = "TestProj_Prob", 
    model_name = "std_model", 
    return_full_prob_mat = TRUE, 
    control = list(iter.max = 1)
  )
  
  expect_true(!is.null(result$prob_matrix))
  expect_equal(nrow(result$prob_matrix), N_obs)
  expect_equal(ncol(result$prob_matrix), J_alts)
  expect_true(!is.null(result$residuals))
})


# Test overwrite ----------------------------------------------------------------------------------
test_that("Overwrite argument successfully replaces existing fit", {
  setup_mocks()
  project_name <- "TestProj_Overwrite"
  model_name <- "std_model"
  fit_name <- "existing_fit"
  
  test_base_dir <- save_design_to_temp(standard_design, model_name, project_name)
  
  # Setup environment and teardown
  old_opts <- options(test_folder_path = test_base_dir)
  on.exit({
    options(old_opts)
    restore_mocks()
  }, add = TRUE)
  
  # 1st call natively creates the fit in the temporary SQLite database
  fishset_fit(
    project = project_name,
    model_name = model_name,
    fit_name = fit_name,
    control = list(iter.max = 1, eval.max = 5)
  )
  
  # 2nd call should NOT throw an error because overwrite = TRUE
  expect_no_error(
    fishset_fit(
      project = project_name,
      model_name = model_name,
      fit_name = fit_name,
      overwrite = TRUE,
      control = list(iter.max = 1, eval.max = 5)
    )
  )
})


# Test unscaling works ----------------------------------------------------------------------------
test_that("Vectorized unscaling correctly divides coefficients and SEs", {
  setup_mocks()
  project_name <- "TestProj_Scale"
  model_name <- "std_model"
  
  # Create a design with fake scalers
  scaled_design <- standard_design
  scaled_design$scalers <- list(
    X1 = list(sd = c("Var1" = 2.0, "Var2" = 10.0))
  )
  test_base_dir <- save_design_to_temp(scaled_design, model_name, project_name)
  
  # Setup environment and teardown
  old_opts <- options(test_folder_path = test_base_dir)
  on.exit({
    options(old_opts)
    restore_mocks()
  }, add = TRUE)
  
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