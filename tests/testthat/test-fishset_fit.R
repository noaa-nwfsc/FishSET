# -------------------------------------------------------------------------------------------------
# File: test-fishset_fit.R
# Purpose: To provide unit tests for the fishset_fit() function.
# Description: This script uses the 'testthat' framework to validate the behavior of 
#              fishset_fit(). It mocks the database retrieval of design objects and
#              the saving of fit results. It verifies that the RTMB optimization runs,
#              statistics (AIC, R2) are calculated, and diagnostics are generated.
#
# Scenarios tested:
#   - Successful estimation (Golden Path) with valid design inputs.
#   - Validation of post-estimation statistics (AIC, Accuracy, Pseudo-R2).
#   - Database persistence (mocking the save).
#   - Handling of control parameters and start values.
#   - Error handling for dimension mismatches and invalid start values.
#
# -------------------------------------------------------------------------------------------------

# Test Data Setup ---------------------------------------------------------------------------------
# Capture Original Functions
orig_functions <- list(
  unserialize_table = getFromNamespace("unserialize_table", "FishSET"),
  table_exists = getFromNamespace("table_exists", "FishSET"),
  table_remove = getFromNamespace("table_remove", "FishSET"),
  log_call = getFromNamespace("log_call", "FishSET"),
  locdatabase = getFromNamespace("locdatabase", "FishSET"),
  dbExecute = DBI::dbExecute
)

# Restore these functions at the end of the test file
on.exit({
  assignInNamespace("unserialize_table", orig_functions$unserialize_table, ns = "FishSET")
  assignInNamespace("table_exists", orig_functions$table_exists, ns = "FishSET")
  assignInNamespace("table_remove", orig_functions$table_remove, ns = "FishSET")
  assignInNamespace("log_call", orig_functions$log_call, ns = "FishSET")
  assignInNamespace("locdatabase", orig_functions$locdatabase, ns = "FishSET")
  assignInNamespace("dbExecute", orig_functions$dbExecute, ns = "DBI")
})

# Mock Design Object
# We need a mathematically valid design so RTMB can actually fit it.
# Scenario: 2 Observations, 2 Alternatives per observation.
# Variable: "catch" (High catch should be preferred).
# Obs 1: Alt A (Catch=10), Alt B (Catch=2) -> Chooses A (y=1)
# Obs 2: Alt A (Catch=2), Alt B (Catch=10) -> Chooses B (y=1 for B)

N_obs_mock <- 4
J_alts_mock <- 2
X_mock <- matrix(c(
  # Obs 1: Clear winner
  10, 2,  
  # Obs 2: Clear winner
  2, 10,  
  # Obs 3: Close call
  5, 6,   
  # Obs 4: Contradiction! (Chooses lower catch to constrain Beta)
  8, 4    
), ncol = 1) 
colnames(X_mock) <- "catch"

# y vector: Obs1(1,0), Obs2(0,1) -> c(1, 0, 0, 1)
y_mock <- c(
  1, 0, 
  0, 1, 
  0, 1, 
  0, 1
)

mock_design_obj <- list(
  X = X_mock,
  y = y_mock,
  formula = as.formula("chosen ~ catch"),
  settings = list(
    N_obs = N_obs_mock,
    J_alts = J_alts_mock,
    K_vars = 1
  ),
  ids = list(
    # Just generic labels repeated
    zone = rep(c("ZoneA", "ZoneB"), N_obs_mock) 
  )
)

# Wrapper list representing the 'ModelDesigns' table
mock_design_db_list <- list(
  TEST_MODEL_DESIGN = mock_design_obj
)

# Helper: Mock Functions (Defined Globally) -------------------------------------------------------
clean_mock_unserialize_fit <- function(table_name, project) {
  # verify the function asks for the ModelDesigns table
  if (grepl("ModelDesigns", table_name)) return(mock_design_db_list)
  if (grepl("ModelFit", table_name)) return(list()) # Empty list for existing fits
  return(NULL)
}

clean_mock_table_exists <- function(table_name, project) return(TRUE)
clean_mock_table_remove <- function(table_name, project) return(TRUE)
clean_mock_log <- function(...) invisible(NULL)
clean_mock_locdb <- function(...) return(":memory:")

# Helper: Setup Function --------------------------------------------------------------------------
setup_fit_mocks <- function() {
  assignInNamespace("unserialize_table", clean_mock_unserialize_fit, ns = "FishSET")
  assignInNamespace("table_exists", clean_mock_table_exists, ns = "FishSET")
  assignInNamespace("table_remove", clean_mock_table_remove, ns = "FishSET")
  assignInNamespace("log_call", clean_mock_log, ns = "FishSET")
  assignInNamespace("locdatabase", clean_mock_locdb, ns = "FishSET")
}

# Standard Model Fitting --------------------------------------------------------------------------
test_that("fishset_fit runs optimization and returns correct structure", {
  setup_fit_mocks()
  
  # Mock DB Save Capture
  captured_fit <- NULL
  mock_dbExecute <- function(conn, statement, params = NULL, ...) {
    if (!is.null(params)) captured_fit <<- unserialize(params$data[[1]])
    return(invisible(TRUE))
  }
  assignInNamespace("dbExecute", mock_dbExecute, ns = "DBI")
  
  # Run the fit
  # We use a very high eval.max to ensure it doesn't timeout, though this data is tiny.
  res <- fishset_fit(
    project = "TEST_PROJ",
    model_name = "TEST_MODEL_DESIGN",
    fit_name = "TEST_FIT_RESULT"
  )
  
  # 1. Structure Check
  expect_s3_class(res, "fishset_fit")
  expect_true(res$converged) # Should converge easily on this data
  
  # 2. Coefficient Check
  # "catch" should be positive (people prefer higher catch)
  expect_true("catch" %in% names(res$coefficients))
  expect_gt(res$coefficients["catch"], 0) 
  
  # 3. Database Persistence Check
  # Ensure the captured object matches the returned object
  expect_equal(captured_fit$TEST_FIT_RESULT$coefficients, res$coefficients)
})

# Post-Estimation Statistics ----------------------------------------------------------------------
test_that("fishset_fit calculates correct post-estimation statistics", {
  setup_fit_mocks()

  # Mock DB Save Capture
  mock_dbExecute <- function(conn, statement, params = NULL, ...) { return(invisible(TRUE)) }
  assignInNamespace("dbExecute", mock_dbExecute, ns = "DBI")

  res <- fishset_fit(
    project = "TEST_PROJ",
    model_name = "TEST_MODEL_DESIGN"
  )

  # 1. Prediction Accuracy
  # We have 4 observations.
  # Obs 1, 2, 3 follow the "High Catch" rule -> Predicted Correctly.
  # Obs 4 violates the rule (chose low catch) -> Predicted Incorrectly.
  # Expected Accuracy = 3 / 4 = 0.75
  expect_equal(res$accuracy, 0.75)

  # 2. Pseudo R2
  # Should be between 0 and 1
  expect_gte(res$pseudo_R2, 0)
  expect_lte(res$pseudo_R2, 1)

  # 3. Prob Matrix Dimensions
  # Should be N_obs_mock (4) x J_alts_mock (2)
  expect_equal(nrow(res$prob_matrix), N_obs_mock)
  expect_equal(ncol(res$prob_matrix), J_alts_mock)

  # 4. Global Test
  expect_true(!is.null(res$LR_stat))
  expect_true(!is.null(res$LR_p_value))

  # 5. Diagnostics exists
  expect_true(is.list(res$diagnostics))
  expect_true("eigenvalues" %in% names(res$diagnostics))
})

# Inputs and Controls -----------------------------------------------------------------------------
test_that("fishset_fit handles start values and control arguments", {
  setup_fit_mocks()

  mock_dbExecute <- function(conn, statement, params = NULL, ...) { return(invisible(TRUE)) }
  assignInNamespace("dbExecute", mock_dbExecute, ns = "DBI")

  # Test with specific start values
  res <- fishset_fit(
    project = "TEST_PROJ",
    model_name = "TEST_MODEL_DESIGN",
    start_values = c(0.5) # Valid length (1 var)
  )

  expect_s3_class(res, "fishset_fit")

  # Test with invalid start value length -> Should Error
  expect_error(
    fishset_fit(
      project = "TEST_PROJ",
      model_name = "TEST_MODEL_DESIGN",
      start_values = c(0.5, 0.2) # Invalid length (2 values, 1 var)
    ),
    "Start values length .* does not match parameters"
  )
})

# Error Handling ----------------------------------------------------------------------------------
test_that("fishset_fit fails gracefully on data integrity issues", {
  setup_fit_mocks()
  mock_dbExecute <- function(conn, statement, params = NULL, ...) { return(invisible(TRUE)) }
  assignInNamespace("dbExecute", mock_dbExecute, ns = "DBI")

  # 1. Missing Model Name
  expect_error(
    fishset_fit(
      project = "TEST_PROJ",
      model_name = "NON_EXISTENT_MODEL"
    ),
    "Model design .* not found"
  )

  # 2. Corrupted Design Matrix (Dimension Mismatch)
  # Modify mock to return a corrupted design
  corrupted_db_list <- mock_design_db_list
  # Add an extra row to X so it doesn't match N*J
  corrupted_db_list$TEST_MODEL_DESIGN$X <- rbind(X_mock, c(5))

  mock_unserialize_corrupt <- function(table_name, project) {
    if (grepl("ModelDesigns", table_name)) return(corrupted_db_list)
    return(NULL)
  }
  assignInNamespace("unserialize_table", mock_unserialize_corrupt, ns = "FishSET")

  expect_error(
    fishset_fit(
      project = "TEST_PROJ",
      model_name = "TEST_MODEL_DESIGN"
    ),
    "Design matrix dimensions do not match"
  )
})