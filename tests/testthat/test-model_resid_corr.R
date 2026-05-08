# -------------------------------------------------------------------------------------------------
# File: test-model_resid_corr.R
# Purpose: To provide unit tests for the model_resid_corr() function.
# Description: This script uses the 'testthat' framework to validate the calculation 
#              of spatial residuals and Moran's I statistic. It isolates the test 
#              environment by mocking design objects, model fits, and building a 
#              synthetic spatial grid using the `sf` package.
#
# Scenarios tested:
#   - Standard Conditional Logit spatial residual extraction and plotting (Golden Path).
#   - Expected Profit Model (EPM) spatial residual extraction and parameter routing.
#   - Error Handling: Missing distribution argument for EPMs.
#   - Error Handling: Mismatched/missing spatial ID columns.
#   - S3 Print and Plot Methods.
# -------------------------------------------------------------------------------------------------

# Test Data Setup ---------------------------------------------------------------------------------
set.seed(42)
N_obs <- 20
J_alts <- 4 # 4 zones to match a 2x2 spatial grid

# Hardcode choices so that observations are spread across all 4 zones
y_vec <- rep(0, N_obs * J_alts)
choices <- rep(1:4, length.out = N_obs) # Even distribution across 4 zones
chosen_indices <- (seq_len(N_obs) - 1) * J_alts + choices
y_vec[chosen_indices] <- 1

zone_names <- c("Z1", "Z2", "Z3", "Z4")

# Shared IDs
mock_ids <- list(
  obs = rep(paste0("Obs", 1:N_obs), each = J_alts),
  zone = rep(zone_names, N_obs)
)

# Synthetic spatial data (sf 2x2 Grid)
# Create a basic 2x2 spatial grid representing 4 adjacent fishing zones
bbox <- sf::st_bbox(c(xmin = 0, xmax = 2, ymin = 0, ymax = 2))
grid <- sf::st_make_grid(bbox, n = c(2, 2))
spat_mock <- sf::st_sf(ZoneID = zone_names, geometry = grid)


# Synthetic design (Standard Logit)
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

# 3. Synthetic design (EPM)
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
  settings = list(N_obs = N_obs, J_alts = J_alts, K_vars = 7),
  ids = mock_ids,
  scalers = list()
)

# Mocked fitted objects
mock_fit_std <- list(
  opt = list(par = c(Var1 = 0.5, Var2 = -0.2))
)

# EPM fit parameters must strictly map to design matrices and capture sigmas
epm_pars <- c(CatchVar = 0.5, UtilVar = 0.1, 
              log_sigma_c_Z1 = 0, log_sigma_c_Z2 = 0, log_sigma_c_Z3 = 0, log_sigma_c_Z4 = 0, 
              log_sigma_e = 0)

mock_fit_epm <- list(
  opt = list(par = epm_pars)
)


# Mocking architecture ----------------------------------------------------------------------------
orig_functions <- list(
  unserialize_table = getFromNamespace("unserialize_table", "FishSET")
)

setup_spatial_mocks <- function(fit_db) {
  assignInNamespace("unserialize_table", function(table_name, project) {
    if (grepl("ModelFit", table_name)) return(fit_db)
    return(NULL)
  }, ns = "FishSET")
}

restore_mocks <- function() {
  assignInNamespace("unserialize_table", orig_functions$unserialize_table, ns = "FishSET")
}

# Helper: Safely save the mock design to the exact expected folder structure
save_design_to_temp <- function(design_obj, model_name, project) {
  test_base_dir <- normalizePath(file.path(tempdir(), "FishSET_Spatial_Tests"), 
                                 winslash = "/", 
                                 mustWork = FALSE)
  project_dir <- file.path(test_base_dir, project)
  designs_dir <- file.path(project_dir, "Models", "ModelDesigns")
  
  dir.create(designs_dir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(design_obj, file.path(designs_dir, paste0(model_name, ".rds")))
  return(test_base_dir)
}


# Test Standard Conditional Logit Spatial Residuals -----------------------------------------------
test_that("Standard Logit spatial residuals and Moran's I run successfully", {
  db_mock <- list(std_fit = mock_fit_std)
  setup_spatial_mocks(db_mock)
  
  test_base_dir <- save_design_to_temp(standard_design, "std_model", "TestProj_Spat_Std")
  old_opts <- options(test_folder_path = test_base_dir)
  
  on.exit({
    options(old_opts)
    restore_mocks()
  }, add = TRUE)
  
  res <- model_resid_corr(
    project = "TestProj_Spat_Std",
    model_name = "std_model",
    spat = spat_mock,
    spat_id = "ZoneID",
    fit_name = "std_fit"
  )
  
  expect_s3_class(res, "fishset_spatial_resid")
  expect_s3_class(res$residual_map, "ggplot")
  
  # Check dataframe formatting
  expect_equal(nrow(res$zonal_residuals), J_alts)
  expect_true("mean_residual" %in% colnames(res$zonal_residuals))
  
  # Check Moran's I test outputs
  expect_true(inherits(res$moran_test, "htest"))
  expect_true(is.numeric(res$moran_test$estimate[1]))
})


# Test Expected Profit Model (EPM) Spatial Residuals ----------------------------------------------
test_that("EPM spatial residuals run successfully with distribution specified", {
  db_mock <- list(epm_fit = mock_fit_epm)
  setup_spatial_mocks(db_mock)

  test_base_dir <- save_design_to_temp(epm_design, "epm_model", "TestProj_Spat_EPM")
  old_opts <- options(test_folder_path = test_base_dir)

  on.exit({
    options(old_opts)
    restore_mocks()
  }, add = TRUE)

  res <- model_resid_corr(
    project = "TestProj_Spat_EPM",
    model_name = "epm_model",
    spat = spat_mock,
    spat_id = "ZoneID",
    fit_name = "epm_fit",
    distribution = "normal"
  )

  expect_s3_class(res, "fishset_spatial_resid")
  expect_s3_class(res$spatial_data, "sf")
  expect_true(!is.null(res$moran_test$p.value))
})


# Test Invalid Inputs -----------------------------------------------------------------------------
test_that("Invalid inputs throw clear spatial and routing errors", {
  db_mock <- list(std_fit = mock_fit_std, epm_fit = mock_fit_epm)
  setup_spatial_mocks(db_mock)

  test_base_dir <- save_design_to_temp(epm_design, "epm_model", "TestProj_Spat_Err")
  save_design_to_temp(standard_design, "std_model", "TestProj_Spat_Err")
  old_opts <- options(test_folder_path = test_base_dir)

  on.exit({
    options(old_opts)
    restore_mocks()
  }, add = TRUE)

  # Missing distribution for EPM
  expect_error(
    model_resid_corr(
      project = "TestProj_Spat_Err",
      model_name = "epm_model",
      spat = spat_mock,
      spat_id = "ZoneID",
      fit_name = "epm_fit"
      # Missing distribution argument
    ),
    "EPMs require the 'distribution' argument"
  )

  # Missing spatial ID column in the sf object
  expect_error(
    model_resid_corr(
      project = "TestProj_Spat_Err",
      model_name = "std_model",
      spat = spat_mock,
      spat_id = "GhostColumn",
      fit_name = "std_fit"
    ),
    "Column GhostColumn not found in the provided spat object"
  )
})


# Test S3 Methods (Print & Plot) ------------------------------------------------------------------
test_that("Print and plot methods display expected output", {
  db_mock <- list(std_fit = mock_fit_std)
  setup_spatial_mocks(db_mock)

  test_base_dir <- save_design_to_temp(standard_design, "std_model", "TestProj_Spat_Print")
  old_opts <- options(test_folder_path = test_base_dir)

  on.exit({
    options(old_opts)
    restore_mocks()
  }, add = TRUE)

  res <- model_resid_corr(
    project = "TestProj_Spat_Print",
    model_name = "std_model",
    spat = spat_mock,
    spat_id = "ZoneID",
    fit_name = "std_fit"
  )

  # Print Output
  expect_output(print(res), "FishSET Spatial Residual Analysis")
  expect_output(print(res), "Moran's I Statistic:")
  expect_output(print(res), "P-value:")

  # Plot Output (Checking it doesn't crash)
  expect_no_error(plot(res))
})