# -------------------------------------------------------------------------------------------------
# File: test-fishset_design.R
# Purpose: To provide unit tests for the fishset_design() function.
# Description: This script uses the 'testthat' framework to validate the behavior of 
#              fishset_design(). It mocks the database retrieval of formatted data and
#              the saving of design objects.
#
# Scenarios tested:
#   - Standard Conditional Logit (Part 1 variables only).
#   - Individual-Specific Interactions (Part 2 variables handling).
#   - Alternative Specific Constants (ASCs) via factor columns.
#   - Data sorting logic (ensuring X/Y alignment).
#   - Error handling for missing columns, invalid data types, or missing DB entries.
#   - Database persistence (mocking the save).
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
  dbExecute = DBI::dbExecute # DBI is exported, so direct access is fine
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

# Mock Long Format Data
# We intentionally scramble the order to test if fishset_design() sorts correctly
# Structure: 2 Trips (T1, T2), 2 Zones (A, B)
mock_lf_data <- data.frame(
  trip_id = c("T2", "T1", "T2", "T1"), 
  zone_id = c("A", "B", "B", "A"),
  chosen = c(0, 1, 1, 0), # T1 chose B, T2 chose B
  catch = c(10, 50, 20, 40), # Alt-specific var
  dist = c(5, 10, 5, 10), # Alt-specific var
  vessel = c(100, 80, 100, 80), # Indiv-specific var (constant per trip)
  price = c(2, 3, 4, 5),
  stringsAsFactors = FALSE
)

# Wrapper list as returned by unserialize_table in format_model_data
mock_db_list <- list(
  TEST_DATA = mock_lf_data
)

# Helper: Mock Functions (Defined Globally) -------------------------------------------------------
clean_mock_unserialize <- function(table_name, project) {
  # verify the function asks for the right table
  if (grepl("LongFormatData", table_name)) return(mock_db_list)
  if (grepl("ModelDesigns", table_name)) return(list()) # Empty list for existing designs
  return(NULL)
}

clean_mock_table_exists <- function(table_name, project) return(TRUE)

clean_mock_table_remove <- function(table_name, project) return(TRUE)

clean_mock_log <- function(...) invisible(NULL)

clean_mock_locdb <- function(...) return(":memory:")

# Helper: Setup Function --------------------------------------------------------------------------
setup_mocks <- function() {
  assignInNamespace("unserialize_table", clean_mock_unserialize, ns = "FishSET")
  assignInNamespace("table_exists", clean_mock_table_exists, ns = "FishSET")
  assignInNamespace("table_remove", clean_mock_table_remove, ns = "FishSET")
  assignInNamespace("log_call", clean_mock_log, ns = "FishSET")
  assignInNamespace("locdatabase", clean_mock_locdb, ns = "FishSET")
}

# Standard Conditional Logit ----------------------------------------------------------------------
test_that("fishset_design handles standard clogit formulas and sorting", {
  setup_mocks()

  # Mock DB Save Capture
  captured_design <- NULL
  mock_dbExecute <- function(conn, statement, params = NULL, ...) {
    if (!is.null(params)) captured_design <<- unserialize(params$data[[1]])
    return(invisible(TRUE))
  }
  assignInNamespace("dbExecute", mock_dbExecute, ns = "DBI")

  fishset_design(
    formula = chosen ~ catch + dist,
    project = "TEST_PROJ",
    model_name = "MODEL_CLOGIT",
    formatted_data_name = "TEST_DATA",
    unique_obs_id = "trip_id",
    zone_id = "zone_id"
  )

  # Extract result from the capture wrapper
  res <- captured_design$MODEL_CLOGIT

  # Class Check
  expect_s3_class(res, "fishset_design")

  # Dimensions Check
  # Data has 4 rows. X should have 4 rows.
  expect_equal(length(res$y), 4)
  expect_equal(nrow(res$X), 4)

  # Variable Check
  # Formula was `~ catch + dist`. X should have columns "catch" and "dist".
  expect_true(all(c("catch", "dist") %in% colnames(res$X)))
  expect_false("(Intercept)" %in% colnames(res$X)) # Should be removed

  # 4. Sorting Check
  # Original data was T2, T1, T2, T1.
  # Function should sort by ID then Zone -> T1(A), T1(B), T2(A), T2(B)
  # T1(A): chosen=0
  # T1(B): chosen=1
  # T2(A): chosen=0
  # T2(B): chosen=1
  # Expected Y vector: 0, 1, 0, 1
  expect_equal(res$y, c(0, 1, 0, 1))
  expect_equal(res$ids$obs, c("T1", "T1", "T2", "T2"))
})

# Interactions (Individual Specific Vars) ---------------------------------------------------------
test_that("fishset_design handles Part 2 individual-specific interactions", {
  setup_mocks()

  # Mock DB Save Capture
  captured_design <- NULL
  mock_dbExecute <- function(conn, statement, params = NULL, ...) {
    if (!is.null(params)) captured_design <<- unserialize(params$data[[1]])
    return(invisible(TRUE))
  }
  assignInNamespace("dbExecute", mock_dbExecute, ns = "DBI")

  fishset_design(
    formula = chosen ~ catch | vessel,
    project = "TEST_PROJ",
    model_name = "MODEL_INTERACT",
    formatted_data_name = "TEST_DATA",
    unique_obs_id = "trip_id",
    zone_id = "zone_id"
  )

  res <- captured_design$MODEL_INTERACT

  # Logic Check:
  # "catch" should be in X (Part 1)
  # "vessel" (Part 2) should NOT be a raw column.
  # "vessel" should be interacted with zones: "vessel:zone_factorA", "vessel:zone_factorB"
  # (or cleaned names depending on function logic)
  cols <- colnames(res$X)
  expect_true("catch" %in% cols)

  # Check for interaction columns.
  # Note: The function sorts data, so Zone A is first level, Zone B is second.
  # fishset_design uses `- 1`, so both levels should appear.
  # The function cleans names by removing "zone_factor".

  # Regex check for "vessel" combined with "A" or "B"
  expect_true(any(grepl("vessel", cols) & grepl("A", cols)))
  expect_true(any(grepl("vessel", cols) & grepl("B", cols)))

  # Ensure raw 'vessel' is NOT there (singularly)
  expect_false("vessel" %in% cols)
})

# Test 3: Alternative Specific Constants (Zonal Logit) --------------------------------------------
test_that("fishset_design handles ASCs correctly", {
  setup_mocks()

  captured_design <- NULL
  mock_dbExecute <- function(conn, statement, params = NULL, ...) {
    if (!is.null(params)) captured_design <<- unserialize(params$data[[1]])
    return(invisible(TRUE))
  }
  assignInNamespace("dbExecute", mock_dbExecute, ns = "DBI")

  fishset_design(
    formula = chosen ~ catch + zone_id, # zone_id in Part 1
    project = "TEST_PROJ",
    model_name = "MODEL_ASC",
    formatted_data_name = "TEST_DATA",
    unique_obs_id = "trip_id",
    zone_id = "zone_id"
  )

  res <- captured_design$MODEL_ASC
  cols <- colnames(res$X)

  # Logic Check:
  # zone_id is a factor. model.matrix will create dummies.
  # Intercept is removed.
  # Standard R behavior for `~ factor - 1` (implied by intercept removal logic)
  # OR `~ factor` without intercept usually keeps n-1 dummies if intercept is removed manually.
  # The function removes "(Intercept)".
  # If zone_id has levels A, B.
  # Columns should be: catch, zone_idB (if A is reference).

  expect_true("catch" %in% cols)

  # We expect at least one zone dummy (usually B if A is base)
  # We check that we see the variable name "zone_id" in the columns
  expect_true(any(grepl("zone_id", cols)))
})

# Test 4: Error Handling --------------------------------------------------------------------------
test_that("fishset_design fails fast on invalid inputs", {
  setup_mocks()

  # 1. Missing Formatted Data Name in DB
  expect_error(
    fishset_design(
      formula = chosen ~ catch,
      project = "TEST_PROJ",
      model_name = "FAIL_MODEL",
      formatted_data_name = "GHOST_DATA", # Does not exist in mock_db_list
      unique_obs_id = "trip_id",
      zone_id = "zone_id"
    ),
    "Formatted data name not found"
  )

  # 2. Missing ID Columns
  expect_error(
    fishset_design(
      formula = chosen ~ catch,
      project = "TEST_PROJ",
      model_name = "FAIL_MODEL",
      formatted_data_name = "TEST_DATA",
      unique_obs_id = "wrong_id", # Wrong column
      zone_id = "zone_id"
    ),
    "Specified 'unique_obs_id' or 'zone_id' columns not found"
  )

  # 3. Non-Binary LHS
  # We temporarily mock data with non-binary choice
  bad_data <- mock_lf_data
  bad_data$chosen[1] <- 5
  bad_db_list <- list(BAD_DATA = bad_data)

  mock_unserialize_bad <- function(...) return(bad_db_list)
  assignInNamespace("unserialize_table", mock_unserialize_bad, ns = "FishSET")

  expect_error(
    fishset_design(
      formula = chosen ~ catch,
      project = "TEST_PROJ",
      model_name = "FAIL_MODEL",
      formatted_data_name = "BAD_DATA",
      unique_obs_id = "trip_id",
      zone_id = "zone_id"
    ),
    "The choice variable .* must be binary"
  )
})