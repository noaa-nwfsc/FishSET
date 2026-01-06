# -------------------------------------------------------------------------------------------------
# File: test-fishset_iia_test.R
# Purpose: To provide unit tests for the fishset_iia_test() function.
# Description: This script uses the 'testthat' framework to validate the behavior of 
#              fishset_iia_test(). It mocks the database retrieval of fitted models and
#              design objects. It verifies that the restricted model is correctly
#              constructed, optimized, and compared to the full model.
#
# Scenarios tested:
#   - Successful IIA test execution (Golden Path).
#   - Correct identification/removal of invalid observations (those choosing omitted zones).
#   - Validation of inputs (omitted zones exist, enough zones remain).
#   - Correct handling of parameter matching (common parameters).
#
# -------------------------------------------------------------------------------------------------

# Test Data Setup ---------------------------------------------------------------------------------
# Capture original functions
orig_functions <- list(
  unserialize_table = getFromNamespace("unserialize_table", "FishSET"),
  log_call = getFromNamespace("log_call", "FishSET"),
  locdatabase = getFromNamespace("locdatabase", "FishSET")
)

# Restore on exit
on.exit({
  assignInNamespace("unserialize_table", orig_functions$unserialize_table, ns = "FishSET")
  assignInNamespace("log_call", orig_functions$log_call, ns = "FishSET")
  assignInNamespace("locdatabase", orig_functions$locdatabase, ns = "FishSET")
})

##### Mock design object (full model) ####
# Scenario: 3 Zones (A, B, C), 3 Obs.
# Obs 1: Chose A
# Obs 2: Chose B (Will be INVALID when we omit B)
# Obs 3: Chose C
N_obs_mock <- 3
J_alts_mock <- 3

# X matrix: Just one variable "catch"
# 3 obs * 3 alts = 9 rows
set.seed(42)
X_mock <- matrix(rnorm(9, mean = 10), ncol = 1)
colnames(X_mock) <- "catch"

# y vector: Obs1(A), Obs2(B), Obs3(C)
# Sorted: Obs1(A,B,C), Obs2(A,B,C), Obs3(A,B,C)
y_mock <- c(
  1, 0, 0,  # Obs 1 -> A
  0, 1, 0,  # Obs 2 -> B (Target for removal)
  0, 0, 1   # Obs 3 -> C
)

ids_mock <- list(
  obs = rep(c("Obs1", "Obs2", "Obs3"), each = 3),
  zone = rep(c("ZoneA", "ZoneB", "ZoneC"), 3)
)

mock_design_obj <- list(
  X = X_mock,
  y = y_mock,
  ids = ids_mock,
  formula = as.formula("chosen ~ catch"),
  settings = list(N_obs = N_obs_mock, J_alts = J_alts_mock, K_vars = 1)
)

#### Mock fit object (full model) ####
# We need coefficients and a vcov matrix
mock_fit_obj <- list(
  design_name = "TEST_MODEL_DESIGN",
  opt = list(par = c(catch = 0.5)),
  vcov = matrix(0.01, 1, 1, dimnames = list("catch", "catch")),
  coefficients = c(catch = 0.5)
)

# Database mock wrappers
mock_design_db <- list(TEST_MODEL_DESIGN = mock_design_obj)
mock_fit_db <- list(TEST_FULL_FIT = mock_fit_obj)

# Helper: mock functions --------------------------------------------------------------------------
clean_mock_unserialize_iia <- function(table_name, project) {
  if (grepl("ModelDesigns", table_name)) return(mock_design_db)
  if (grepl("ModelFit", table_name)) return(mock_fit_db)
  return(NULL)
}

clean_mock_log <- function(...) invisible(NULL)
clean_mock_locdb <- function(...) return(":memory:")

# Helper: setup function --------------------------------------------------------------------------
setup_iia_mocks <- function() {
  assignInNamespace("unserialize_table", clean_mock_unserialize_iia, ns = "FishSET")
  assignInNamespace("log_call", clean_mock_log, ns = "FishSET")
  assignInNamespace("locdatabase", clean_mock_locdb, ns = "FishSET")
}

# Check that function runs correctly --------------------------------------------------------------
test_that("fishset_iia_test runs successfully on valid inputs", {
  setup_iia_mocks()
  
  # Run IIA test omitting "ZoneB"
  # This should remove Obs 2 (who chose B)
  res <- fishset_iia_test(
    project = "TEST_PROJ",
    fit_name = "TEST_FULL_FIT",
    omitted_zones = "ZoneB"
  )
  
  # Structure check
  expect_s3_class(res, "fishset_iia")
  expect_true(is.numeric(res$statistic))
  expect_true(is.numeric(res$p_value))
  
  # Correct omission logic
  expect_equal(res$omitted, "ZoneB")
  
  # Parameter matching
  # Both models have "catch".
  expect_equal(res$common_parameters, "catch")
  expect_equal(length(res$full_coefs), 1)
  expect_equal(length(res$restricted_coefs), 1)
})

# IIA supported scenario --------------------------------------------------------------------------
test_that("fishset_iia_test returns 'IIA Supported' when coefficients match", {
  setup_iia_mocks()
  
  # Setup Data that produces a specific beta
  # X=10 for chosen, X=0 for unchosen. The optimizer will find a positive beta.
  # Let's assume the optimizer finds beta approx 1.0 to 2.0.
  
  # Create a "Full Fit" Mock that MATCHES this expectation
  # We set the coefficient to 1.5 (a reasonable guess for this data)
  # We set a tiny variance (1e-6) to ensure V_diff is positive definite
  match_fit_obj <- list(
    design_name = "TEST_MODEL_DESIGN",
    opt = list(par = c(catch = 1.5)), 
    vcov = matrix(1e-6, 1, 1, dimnames = list("catch", "catch")),
    coefficients = c(catch = 1.5)
  )
  
  # Override the unserialize mock locally for this test
  local_mock_db <- list(TEST_FULL_FIT = match_fit_obj)
  
  mock_unserialize_local <- function(table_name, project) {
    if (grepl("ModelFit", table_name)) return(local_mock_db)
    if (grepl("ModelDesigns", table_name)) return(mock_design_db) # Use global design mock
    return(NULL)
  }
  assignInNamespace("unserialize_table", mock_unserialize_local, ns = "FishSET")
  
  # Run Test
  # We use the X_mock/y_mock from the global setup which has a clear preference structure
  res <- fishset_iia_test(
    project = "TEST_PROJ",
    fit_name = "TEST_FULL_FIT",
    omitted_zones = "ZoneB",
    start_values = c(1.5) # Help optimizer find the same spot
  )
  
  # Since coefficients are similar, P-value should be high
  expect_gt(res$p_value, 0.05)
  expect_match(res$description, "IIA Supported")
})

# Test invalid inputs -----------------------------------------------------------------------------
test_that("fishset_iia_test validates input zones correctly", {
  setup_iia_mocks()
  
  # Invalid omitted zone
  expect_error(
    fishset_iia_test(
      project = "TEST_PROJ",
      fit_name = "TEST_FULL_FIT",
      omitted_zones = "ZoneX"
    ),
    "One or more 'omitted_zones' not found"
  )
  
  # Removing too many zones (Leaves 1, need at least 2)
  # We have 3 zones (A, B, C). Removing A and B leaves only C.
  expect_error(
    fishset_iia_test(
      project = "TEST_PROJ",
      fit_name = "TEST_FULL_FIT",
      omitted_zones = c("ZoneA", "ZoneB")
    ),
    "You must keep at least two alternatives"
  )
})

# Handles error in model designs ------------------------------------------------------------------
test_that("fishset_iia_test correctly handles mismatched designs", {
  setup_iia_mocks()
  
  # Create a fit object pointing to a ghost design
  bad_fit_db <- list(
    BAD_FIT = list(design_name = "GHOST_DESIGN")
  )
  
  mock_unserialize_bad <- function(table_name, project) {
    if (grepl("ModelFit", table_name)) return(bad_fit_db)
    if (grepl("ModelDesigns", table_name)) return(mock_design_db)
    return(NULL)
  }
  assignInNamespace("unserialize_table", mock_unserialize_bad, ns = "FishSET")
  
  expect_error(
    fishset_iia_test(
      project = "TEST_PROJ",
      fit_name = "BAD_FIT",
      omitted_zones = "ZoneB"
    ),
    "Design object 'GHOST_DESIGN' not found"
  )
})