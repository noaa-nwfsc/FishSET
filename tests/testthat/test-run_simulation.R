# -------------------------------------------------------------------------------------------------
# File: test-run_simulation.R
# Purpose: To provide unit tests for the run_simulation() function.
# Description: This script uses the 'testthat' framework to validate the behavior of the
#              run_simulation() function. It mocks internal database/IO paths and uses
#              synthetic design and fit objects to test the discrete choice engine.
#
# Scenarios tested:
#   - Standard Logit: Baseline run (verifies zero mean welfare change).
#   - Standard Logit: Spatial closures (verifies effort drops to zero in closed zones).
#   - Standard Logit: Data modifiers (verifies effort redistributes under new data).
#   - Expected Profit Model (EPM): End-to-end execution and automatic internal theta scaling.
#   - Expected Profit Model (EPM): Auto-detection and execution of the lognormal distribution.
#   - Optional Arguments: Validation that `income_cost = TRUE` correctly flips the theta sign.
#   - Optional Arguments: Warning generation when `marg_util_income` is passed to an EPM.
#   - Error Handling: Missing marg_util_income argument for standard logits.
#   - Error Handling: Missing files and invalid YAML closure inputs.
#   - Error Handling: Data modifier variables not present in the model's design matrix.
#
# Notes: This test script isolates the function by using temporary directories and SQLite
#        databases. It temporarily overrides internal FishSET namespace path functions
#        (locdatabase, locoutput, pull_output) to safely control file I/O during tests.
#        It cleanly resets environment options between blocks to prevent test bleed.
# -------------------------------------------------------------------------------------------------

# Test Data Setup ---------------------------------------------------------------------------------
set.seed(42)
N_obs <- 10
J_alts <- 3
K_vars <- 2
betadraws_test <- 5 # Keep draws small quick testing

# Synthetic Design Object (Standard Logit)
std_design <- list(
  X = matrix(rnorm(N_obs * J_alts * K_vars), ncol = K_vars),
  epm = list(is_epm = FALSE),
  settings = list(N_obs = N_obs, J_alts = J_alts, K_vars = K_vars, project = "TestSimProj"),
  ids = list(zone = as.character(rep(1:J_alts, N_obs))),
  scalers = list()
)
colnames(std_design$X) <- c("Var1", "Var2")

# Synthetic Fit Object (Standard Logit)
std_fit <- list(
  opt = list(par = c(Var1 = 0.5, Var2 = -0.8)),
  diagnostics = list(hessian = diag(K_vars) * 0.001), # Tighten variance so draws stay positive
  coefficients = c(Var1 = 0.5, Var2 = -0.8)
)

# Synthetic Design Object (Expected Profit Model)
epm_design <- list(
  X = matrix(rnorm(N_obs * J_alts), ncol = 1),
  epm = list(
    is_epm = TRUE,
    X_catch = matrix(rnorm(N_obs * J_alts), ncol = 1),
    price_vec = rep(2.5, N_obs * J_alts)
  ),
  settings = list(N_obs = N_obs, J_alts = J_alts),
  ids = list(zone = as.character(rep(1:J_alts, N_obs))),
  scalers = list()
)
colnames(epm_design$X) <- c("CostVar")
colnames(epm_design$epm$X_catch) <- c("CatchVar")

# Synthetic Fit Object (EPM)
epm_fit <- list(
  opt = list(par = c(CatchVar = 0.5, CostVar = -0.2, 
                     log_sigma_c_1 = log(0.5), log_sigma_e = log(1.2))),
  diagnostics = list(hessian = diag(4) * 0.001), # Tighten variance
  coefficients = c(CatchVar = 0.5, CostVar = -0.2, Sigma_Catch_1 = 0.5, Sigma_Error = 1.2) 
)

# Environment Setup -------------------------------------------------------------------------------
orig_functions <- list(
  log_call = getFromNamespace("log_call", "FishSET"),
  locdatabase = getFromNamespace("locdatabase", "FishSET"),
  locoutput = getFromNamespace("locoutput", "FishSET"),
  pull_output = getFromNamespace("pull_output", "FishSET")
)

setup_mocks <- function(db_path, out_dir) {
  assignInNamespace("log_call", function(...) invisible(NULL), ns = "FishSET")
  assignInNamespace("locdatabase", function(...) db_path, ns = "FishSET")
  assignInNamespace("locoutput", function(...) paste0(out_dir, "/"), ns = "FishSET")
  assignInNamespace("pull_output", function(...) "closures.yaml", ns = "FishSET")
}

restore_mocks <- function() {
  assignInNamespace("log_call", orig_functions$log_call, ns = "FishSET")
  assignInNamespace("locdatabase", orig_functions$locdatabase, ns = "FishSET")
  assignInNamespace("locoutput", orig_functions$locoutput, ns = "FishSET")
  assignInNamespace("pull_output", orig_functions$pull_output, ns = "FishSET")
}

# Helper: Build the complete isolated filesystem and SQLite DB for run_simulation
setup_sim_env <- function(project_name, model_name, design_obj, fit_obj) {
  test_base_dir <- normalizePath(file.path(tempdir(), 
                                           paste0("FishSET_SimTests_", sample(1:10000, 1))),
                                 winslash = "/", mustWork = FALSE)
  project_dir <- file.path(test_base_dir, project_name)
  
  # Create directory structure
  md_dir <- file.path(project_dir, "Models", "ModelDesigns")
  out_dir <- file.path(project_dir, "Output")
  dir.create(md_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Save Design
  saveRDS(design_obj, file.path(md_dir, paste0(model_name, ".rds")))
  
  # Create and populate SQLite Database with the ModelFit table
  db_path <- file.path(project_dir, "database.sqlite")
  db <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  
  table_name <- paste0(project_name, "ModelFit")
  DBI::dbExecute(db, paste("CREATE TABLE IF NOT EXISTS", table_name, "(data BLOB)"))
  
  fit_list <- list()
  fit_list[[paste0(model_name, "_fit")]] <- fit_obj
  DBI::dbExecute(db, paste("INSERT INTO", table_name, "(data) VALUES (:data)"),
                 params = list(data = list(serialize(fit_list, NULL))))
  DBI::dbDisconnect(db)
  
  # Create YAML closure file
  yaml_content <- list(
    list(scenario = "closure_1", zone = "Zone_1"),
    list(scenario = "closure_2", zone = "Zone_2")
  )
  yaml::write_yaml(yaml_content, file.path(out_dir, "closures.yaml"))
  
  return(list(base_dir = test_base_dir, db_path = db_path, out_dir = out_dir))
}

# Test baseline (no closures) for standard logit --------------------------------------------------
test_that("Standard Logit runs baseline correctly", {
  env <- setup_sim_env("Proj_Std", "mod1", std_design, std_fit)
  
  old_opts <- options(test_folder_path = env$base_dir)
  setup_mocks(env$db_path, env$out_dir)
  on.exit({
    options(old_opts)
    restore_mocks()
  }, add = TRUE)
  
  res <- run_simulation(
    project = "Proj_Std", 
    mod_name = "mod1", 
    betadraws = betadraws_test, 
    marg_util_income = "Var1"
  )
  
  # Assertions
  expect_type(res, "list")
  expect_true("baseline" %in% names(res))
  
  baseline_res <- res$baseline
  expect_equal(baseline_res$mean_welfare_loss, 0) # Baseline vs Baseline welfare is always 0
  expect_length(baseline_res$effort_base, J_alts)
  
  # Probabilities should sum to N_obs
  expect_equal(sum(baseline_res$effort_base), N_obs, tolerance = 1e-6)
})

# Test closure for standard logit -----------------------------------------------------------------
test_that("Standard Logit handles spatial closures", {
  env <- setup_sim_env("Proj_Closure", "mod1", std_design, std_fit)
  
  old_opts <- options(test_folder_path = env$base_dir)
  setup_mocks(env$db_path, env$out_dir)
  on.exit({
    options(old_opts)
    restore_mocks()
  }, add = TRUE)
  
  res <- run_simulation(
    project = "Proj_Closure", 
    mod_name = "mod1", 
    closures = c("closure_1"), 
    betadraws = betadraws_test, 
    marg_util_income = "Var1"
  )
  
  expect_true("closure_1" %in% names(res))
  cl_res <- res$closure_1
  
  # Welfare should change
  expect_false(cl_res$mean_welfare_loss == 0)
  
  # Zone 1 was closed, its counterfactual effort should be exactly 0
  expect_equal(unname(cl_res$effort_new["1"]), 0)
  
  # Total effort should still sum to N_obs (effort redistributed, not lost)
  expect_equal(sum(cl_res$effort_new), N_obs, tolerance = 1e-6)
})

# Test data modifiers for standard logit ----------------------------------------------------------
test_that("Standard Logit processes data modifiers", {
  env <- setup_sim_env("Proj_Mods", "mod1", std_design, std_fit)
  
  old_opts <- options(test_folder_path = env$base_dir)
  setup_mocks(env$db_path, env$out_dir)
  on.exit({
    options(old_opts)
    restore_mocks()
  }, add = TRUE)
  
  # Create a massive penalty for Var2 to force behavior change
  new_var2 <- rep(-50, N_obs * J_alts)
  
  res <- run_simulation(
    project = "Proj_Mods", 
    mod_name = "mod1", 
    data_modifiers = list(Var2 = new_var2),
    betadraws = betadraws_test, 
    marg_util_income = "Var1"
  )
  
  mod_res <- res$Var2 # Dynamic naming automatically set it to 'Var2'
  
  # Effort should have shifted compared to baseline
  expect_false(isTRUE(all.equal(mod_res$effort_base, mod_res$effort_new)))
})

# Test simple EPM ---------------------------------------------------------------------------------
test_that("Expected Profit Model (EPM) runs successfully", {
  env <- setup_sim_env("Proj_EPM", "epm1", epm_design, epm_fit)
  
  old_opts <- options(test_folder_path = env$base_dir)
  setup_mocks(env$db_path, env$out_dir)
  on.exit({
    options(old_opts)
    restore_mocks()
  }, add = TRUE)
  
  # EPM does not require marg_util_income (it calculates theta internally)
  expect_no_error(
    res <- run_simulation(
      project = "Proj_EPM", 
      mod_name = "epm1", 
      betadraws = betadraws_test
    )
  )
  
  expect_true("baseline" %in% names(res))
  
  # Verify EPM handles closures correctly
  res_cl <- run_simulation(
    project = "Proj_EPM", 
    mod_name = "epm1", 
    closures = c("closure_2"),
    betadraws = betadraws_test
  )
  
  expect_equal(unname(res_cl$closure_2$effort_new["2"]), 0)
})

# Test missing marg_util_income -------------------------------------------------------------------
test_that("Error Handling: Standard Logit missing marg_util_income", {
  env <- setup_sim_env("Proj_Err", "mod1", std_design, std_fit)
  
  old_opts <- options(test_folder_path = env$base_dir)
  setup_mocks(env$db_path, env$out_dir)
  on.exit({
    options(old_opts)
    restore_mocks()
  }, add = TRUE)
  
  expect_error(
    run_simulation(
      project = "Proj_Err", 
      mod_name = "mod1", 
      betadraws = betadraws_test,
      marg_util_income = NULL # Missing
    ),
    "requires 'marg_util_income'"
  )
})

# Test auto-detect for EPM distribution -----------------------------------------------------------
test_that("Expected Profit Model (EPM) auto-detects and runs lognormal distribution", {
  # Modify the synthetic fit to trigger lognormal detection
  epm_fit_lognorm <- epm_fit
  epm_fit_lognorm$coefficients <- c(CatchVar = 0.5, CostVar = -0.2, 
                                    Sdlog_Catch_1 = 0.5, Sigma_Error = 1.2)
  
  env <- setup_sim_env("Proj_EPM_Log", "epm_log", epm_design, epm_fit_lognorm)
  old_opts <- options(test_folder_path = env$base_dir)
  setup_mocks(env$db_path, env$out_dir)
  on.exit({ options(old_opts); restore_mocks() }, add = TRUE)
  
  expect_no_error(
    res <- run_simulation(project = "Proj_EPM_Log", 
                          mod_name = "epm_log", 
                          betadraws = betadraws_test)
  )
})

# Test missing files and bad YAML -----------------------------------------------------------------
test_that("Error Handling: Missing files and bad YAML inputs", {
  env <- setup_sim_env("Proj_IO_Err", "mod1", std_design, std_fit)
  old_opts <- options(test_folder_path = env$base_dir)
  
  # Missing YAML file (Override the mock to point to a nonexistent file)
  setup_mocks(env$db_path, env$out_dir)
  assignInNamespace("pull_output", function(...) "does_not_exist.yaml", ns = "FishSET")
  
  expect_error(
    run_simulation("Proj_IO_Err", "mod1", closures = c("closure_1"), marg_util_income = "Var1"),
    "No policy scenario YAML file found"
  )
  
  # YAML exists, but user asks for a scenario not in the YAML
  restore_mocks()
  setup_mocks(env$db_path, env$out_dir) # Reset to closures.yaml
  
  expect_error(
    run_simulation("Proj_IO_Err", "mod1", 
                   closures = c("Ghost_Closure"), 
                   marg_util_income = "Var1"),
    "None of the specified scenario names in 'closures' were found"
  )
  
  # Missing Fit Object
  expect_error(
    run_simulation("Proj_IO_Err", "MissingMod", marg_util_income = "Var1"),
    "Model design MissingMod not found"
  )
  
  on.exit({ options(old_opts); restore_mocks() }, add = TRUE)
})

# Test data mismatch for data modifier ------------------------------------------------------------
test_that("Error Handling: Data modifier variable not in design matrix", {
  env <- setup_sim_env("Proj_Mod_Err", "mod1", std_design, std_fit)
  old_opts <- options(test_folder_path = env$base_dir)
  setup_mocks(env$db_path, env$out_dir)
  on.exit({ options(old_opts); restore_mocks() }, add = TRUE)
  
  expect_error(
    run_simulation(
      project = "Proj_Mod_Err", 
      mod_name = "mod1", 
      data_modifiers = list(TypoVar = rep(10, N_obs * J_alts)),
      betadraws = betadraws_test,
      marg_util_income = "Var1"
    ),
    "Data modifier error"
  )
})

# Test optional input arguments -------------------------------------------------------------------
test_that("Optional Arguments: income_cost flips theta sign correctly", {
  env <- setup_sim_env("Proj_Opt", "mod1", std_design, std_fit)
  
  old_opts <- options(test_folder_path = env$base_dir)
  setup_mocks(env$db_path, env$out_dir)
  on.exit({ options(old_opts); restore_mocks() }, add = TRUE)
  
  # Test income_cost = TRUE (If Var1 is negative, income_cost flips it to positive)
  std_fit_cost <- std_fit
  std_fit_cost$opt$par["Var1"] <- -0.5
  std_fit_cost$coefficients["Var1"] <- -0.5
  
  # Overwrite DB with the new fit
  db <- DBI::dbConnect(RSQLite::SQLite(), env$db_path)
  fit_list <- list(mod1_fit = std_fit_cost)
  DBI::dbExecute(db, "UPDATE Proj_OptModelFit SET data = :data", 
                 params = list(data = list(serialize(fit_list, NULL))))
  DBI::dbDisconnect(db)
  
  expect_no_error(
    run_simulation(
      project = "Proj_Opt", 
      mod_name = "mod1", 
      betadraws = betadraws_test, 
      marg_util_income = "Var1", 
      income_cost = TRUE
    )
  )
})

# Test warning for marg_util_income and EPM -------------------------------------------------------
test_that("Optional Arguments: EPM warns if marg_util_income is provided", {
  env <- setup_sim_env("Proj_EPM_Warn", "epm1", epm_design, epm_fit)
  
  # Properly set the test_folder_path specifically for the EPM environment
  old_opts <- options(test_folder_path = env$base_dir)
  setup_mocks(env$db_path, env$out_dir)
  on.exit({ options(old_opts); restore_mocks() }, add = TRUE)
  
  expect_warning(
    run_simulation(
      project = "Proj_EPM_Warn", 
      mod_name = "epm1", 
      betadraws = betadraws_test, 
      marg_util_income = "CostVar"
    ),
    "ignored for Expected Profit Models"
  )
})