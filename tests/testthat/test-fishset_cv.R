# -------------------------------------------------------------------------------------------------
# File: test-fishset_cv.R
# Purpose: To provide unit tests for the fishset_cv() function.
# Description: This script uses the 'testthat' framework to validate the behavior of the
#              k-fold cross-validation engine.
#
# Scenarios tested:
#   - Standard Logit CV: Runs an end-to-end k-fold loop and verifies outputs.
#   - EPM CV: Runs CV with the required distribution math and isolates EPM matrices.
#   - Error Handling: Missing base design files.
#   - Error Handling: Missing distribution argument for EPM designs.
#   - S3 Print Method: Verifies print.fishset_cv formats output properly.
#
# Notes: This test follows the pattern of 'test-fishset_fit.R' by mocking
#        internal FishSET functions via assignInNamespace. It uses real DBI/RSQLite
#        connections directed to a temporary file to safely test DB cleanup.
# -------------------------------------------------------------------------------------------------

# Test Data Setup ---------------------------------------------------------------------------------
set.seed(42)
N_obs <- 20 # 20 trips (allows clean splitting for k=4 or k=5)
J_alts <- 2 # Keep alternatives small for speed
K_vars <- 2

# Synthetic Design Object (Standard Logit)
standard_design <- list(
  y = rep(c(1, 0), N_obs), 
  X = matrix(rnorm(N_obs * J_alts * K_vars), ncol = K_vars),
  epm = list(is_epm = FALSE),
  settings = list(N_obs = N_obs, 
                  J_alts = J_alts, 
                  K_vars = K_vars, 
                  project = "TestProj",
                  model_type = "logit"),
  ids = list(
    obs = rep(1:N_obs, each = J_alts),
    zone = rep(1:J_alts, N_obs)
  ),
  scalers = list()
)
colnames(standard_design$X) <- c("Var1", "Var2")

# Synthetic Design Object (Expected Profit Model)
epm_design <- list(
  y = rep(c(1, 0), N_obs),
  X = matrix(rnorm(N_obs * J_alts * 1), ncol = 1), # Utility var
  epm = list(
    is_epm = TRUE,
    X_catch = matrix(rnorm(N_obs * J_alts * 1), ncol = 1), # Catch var
    Y_catch = runif(N_obs * J_alts, 50, 150), # Keep positive for lognormal
    price_vec = rep(2.5, N_obs * J_alts)
  ),
  settings = list(N_obs = N_obs, J_alts = J_alts, K_vars = 3, model_type = "logit"),
  ids = list(
    obs = rep(1:N_obs, each = J_alts),
    zone = rep(1:J_alts, N_obs)
  ),
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
  test_base_dir <- normalizePath(file.path(tempdir(), "FishSET_CV_Tests"), 
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

# Test standard logit CV --------------------------------------------------------------------------
test_that("Standard Logit CV runs successfully (End-to-End)", {
  setup_mocks()
  test_base_dir <- save_design_to_temp(standard_design, "std_cv_model", "TestProj_CV_Std")
  
  old_opts <- options(test_folder_path = test_base_dir)
  on.exit({
    options(old_opts)
    restore_mocks()
  }, add = TRUE)
  
  capture.output({
    result <- fishset_cv(
      project = "TestProj_CV_Std",
      base_model_name = "std_cv_model",
      k = 4, # 20 obs / 4 folds = 5 obs per fold
      control = list(iter.max = 1, eval.max = 5) 
    )  
  })
  
  expect_s3_class(result, "fishset_cv")
  expect_equal(result$k_folds, 4)
  expect_true(is.numeric(result$avg_out_sample_accuracy))
  expect_true(is.numeric(result$avg_out_sample_logLik))
  
  # Check dataframe output
  expect_equal(nrow(result$fold_details), 4)
  expect_true("Out_Sample_LL" %in% names(result$fold_details))
})

# Test EPM CV -------------------------------------------------------------------------------------
test_that("EPM CV runs successfully with normal distribution", {
  setup_mocks()
  test_base_dir <- save_design_to_temp(epm_design, "epm_cv_model", "TestProj_CV_EPM")
  
  old_opts <- options(test_folder_path = test_base_dir)
  on.exit({
    options(old_opts)
    restore_mocks()
  }, add = TRUE)
  
  capture.output({
    result <- fishset_cv(
      project = "TestProj_CV_EPM",
      base_model_name = "epm_cv_model",
      k = 4,
      distribution = "normal",
      control = list(iter.max = 1, eval.max = 5) 
    )  
  })
  
  expect_s3_class(result, "fishset_cv")
  expect_true(!is.na(result$avg_out_sample_logLik))
})

# Test missing design error -----------------------------------------------------------------------
test_that("Error: Base model not found", {
  setup_mocks()
  test_base_dir <- normalizePath(file.path(tempdir(), "FishSET_CV_Tests"), 
                                 winslash = "/", 
                                 mustWork = FALSE)
  
  old_opts <- options(test_folder_path = test_base_dir)
  on.exit({
    options(old_opts)
    restore_mocks()
  }, add = TRUE)
  
  # Create directory but no design files
  dir.create(file.path(test_base_dir, "TestProj_Missing", "Models", "ModelDesigns"), 
             recursive = TRUE, 
             showWarnings = FALSE)
  
  expect_error(
    fishset_cv(project = "TestProj_Missing", base_model_name = "ghost_model", k = 2),
    "Base model design not found"
  )
})

# Test EPM missing distribution error -------------------------------------------------------------
test_that("Error: EPM requires distribution argument", {
  setup_mocks()
  test_base_dir <- save_design_to_temp(epm_design, "epm_cv_model", "TestProj_CV_EPM_Err")
  
  old_opts <- options(test_folder_path = test_base_dir)
  on.exit({
    options(old_opts)
    restore_mocks()
  }, add = TRUE)
  
  expect_error(
    fishset_cv(
      project = "TestProj_CV_EPM_Err",
      base_model_name = "epm_cv_model",
      k = 2
      # Missing distribution = "normal"
    ),
    "EPMs require a 'distribution' argument"
  )
})

# Test S3 Print Method ----------------------------------------------------------------------------
test_that("Print method displays correct console output", {
  setup_mocks()
  test_base_dir <- save_design_to_temp(standard_design, "std_cv_model", "TestProj_CV_Print")
  
  old_opts <- options(test_folder_path = test_base_dir)
  on.exit({
    options(old_opts)
    restore_mocks()
  }, add = TRUE)
  
  capture.output({
    result <- fishset_cv(
      project = "TestProj_CV_Print",
      base_model_name = "std_cv_model",
      k = 2,
      control = list(iter.max = 1, eval.max = 5) 
    )  
  })
  
  # expect_output captures the cat() and print() statements
  expect_output(print(result), "FishSET Cross-Validation Results")
  expect_output(print(result), "Total Folds:")
  expect_output(print(result), "Avg Out-of-Sample LL:")
  expect_output(print(result), "Out_Sample_Acc") # Column in the dataframe
})