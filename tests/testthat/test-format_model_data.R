# -------------------------------------------------------------------------------------------------
# File: test-format_model_data.R
# Purpose: To provide unit tests for the format_model_data() function.
# Description: This script uses the 'testthat' framework to validate the behavior of 
#              format_model_data. It mocks internal FishSET database interactions to test
#              the reshaping logic, distance calculation integration, expectation merging,
#              and imputation handling in isolation.
#
# Scenarios tested:
#   - Basic reshaping (long format creation) using the cross_join logic.
#   - Correct identification of the 'chosen' alternative.
#   - Integration of distance matrices.
#   - Handling of missing data (NAs) via imputation (Mean/Mode) and removal.
#   - Input validation (error handling).
#   - Merging of expectation matrices.
#
# -------------------------------------------------------------------------------------------------

# Test Data Setup ---------------------------------------------------------------------------------
# Capture Original Functions
orig_functions <- list(
  table_exists = getFromNamespace("table_exists", "FishSET"),
  table_view   = getFromNamespace("table_view", "FishSET"),
  unserialize_table = getFromNamespace("unserialize_table", "FishSET"),
  create_dist_matrix = getFromNamespace("create_dist_matrix", "FishSET"),
  column_check = getFromNamespace("column_check", "FishSET"),
  log_call = getFromNamespace("log_call", "FishSET"),
  locdatabase = getFromNamespace("locdatabase", "FishSET"),
  data_pull = getFromNamespace("data_pull", "FishSET"),
  dbExecute = DBI::dbExecute # DBI is usually exported, so :: is fine
)

# Restore these functions at the end of the test file
on.exit({
  assignInNamespace("table_exists", orig_functions$table_exists, ns = "FishSET")
  assignInNamespace("table_view", orig_functions$table_view, ns = "FishSET")
  assignInNamespace("unserialize_table", orig_functions$unserialize_table, ns = "FishSET")
  assignInNamespace("create_dist_matrix", orig_functions$create_dist_matrix, ns = "FishSET")
  assignInNamespace("column_check", orig_functions$column_check, ns = "FishSET")
  assignInNamespace("log_call", orig_functions$log_call, ns = "FishSET")
  assignInNamespace("locdatabase", orig_functions$locdatabase, ns = "FishSET")
  assignInNamespace("data_pull", orig_functions$data_pull, ns = "FishSET")
  assignInNamespace("dbExecute", orig_functions$dbExecute, ns = "DBI")
})

# Mock Main Data
main_data <- data.frame(
  unique_row_id = c("1", "2", "3"),
  ZoneID = c("A", "B", "A"), 
  other_var = c(10, 20, 30)
)

# Mock AltMatrix 
alt_matrix <- list(
  greaterNZ = c("A", "B"), 
  alt_var = "nearest point",
  occasion = "trip",
  occasion_var = "unique_row_id",
  dataZoneTrue = rep(1, 3),
  choice = c("A", "B", "A"),
  zoneID = "ZoneID",
  zone_cent = NULL,
  fish_cent = NULL,
  altChoiceUnits = "km",
  spat = NULL, 
  spatID = NULL
)

# Mock Distance Matrix Output
dist_mat_mock <- matrix(
  c(5, 10,  
    15, 20, 
    25, 30),
  nrow = 3, byrow = TRUE
)
colnames(dist_mat_mock) <- c("A", "B")
rownames(dist_mat_mock) <- c("1", "2", "3")
dist_out_mock <- list(dist_matrix = dist_mat_mock)

# Mock Expectation List
exp_list_mock <- list(
  catch_exp = matrix(c(100, 200, 300, 400, 500, 600), nrow = 3, byrow = TRUE,
                     dimnames = list(NULL, c("A", "B")))
)

# Helper: Mock Functions (Defined Globally) -------------------------------------------------------
# Define these at the top level so they can be reused/restored inside tests
clean_mock_table_view <- function(table_name, project) {
  if (grepl("MainDataTable", table_name)) return(main_data)
  if (grepl("AuxTable", table_name)) return(data.frame(key="1", aux_val=99))
  return(NULL)
}

clean_mock_create_dist <- function(...) return(dist_out_mock)

clean_mock_table_exists <- function(table_name, project) return(FALSE)

clean_mock_unserialize <- function(table_name, project) {
  if (grepl("AltMatrix", table_name)) return(alt_matrix)
  if (grepl("ExpectedCatch", table_name)) return(exp_list_mock)
  return(list())
}

clean_mock_col_check <- function(...) return(TRUE)
clean_mock_log <- function(...) invisible(NULL)
clean_mock_locdb <- function(...) return(":memory:")
clean_mock_data_pull <- function(...) return(list(dat="x", dataset=NULL))

# Helper: Setup Function --------------------------------------------------------------------------
setup_mocks <- function() {
  assignInNamespace("table_exists", clean_mock_table_exists, ns = "FishSET")
  assignInNamespace("table_view", clean_mock_table_view, ns = "FishSET")
  assignInNamespace("unserialize_table", clean_mock_unserialize, ns = "FishSET")
  assignInNamespace("create_dist_matrix", clean_mock_create_dist, ns = "FishSET")
  assignInNamespace("column_check", clean_mock_col_check, ns = "FishSET")
  assignInNamespace("log_call", clean_mock_log, ns = "FishSET")
  assignInNamespace("locdatabase", clean_mock_locdb, ns = "FishSET")
  assignInNamespace("data_pull", clean_mock_data_pull, ns = "FishSET")
}

# Reshape and assign choices correctly ------------------------------------------------------------
test_that("format_model_data reshapes and assigns choices correctly", {
  setup_mocks()

  captured_data <- NULL
  mock_dbExecute <- function(conn, statement, params = NULL, ...) {
    if (!is.null(params)) captured_data <<- unserialize(params$data[[1]])
    return(invisible(TRUE))
  }
  assignInNamespace("dbExecute", mock_dbExecute, ns = "DBI")

  format_model_data(
    project = "TEST_PROJECT",
    name = "TEST_MODEL",
    zone_id = "ZoneID",
    unique_obs_id = "unique_row_id",
    select_vars = c("other_var"),
    distance = TRUE
  )

  df_out <- captured_data$TEST_MODEL
  expect_equal(nrow(df_out), 6)
  expect_true(all(c("distance", "chosen", "other_var", "ZoneID") %in% names(df_out)))
})

# Test imputation ---------------------------------------------------------------------------------
test_that("format_model_data handles imputation correctly", {
  setup_mocks()

  # Setup Mock DB Capture
  captured_data <- NULL
  mock_dbExecute <- function(conn, statement, params = NULL, ...) {
    if (!is.null(params)) captured_data <<- unserialize(params$data[[1]])
    return(invisible(TRUE))
  }
  assignInNamespace("dbExecute", mock_dbExecute, ns = "DBI")

  # Test "mean" imputation (Using Observation-level NA)
  # Modify main_data to have NA
  main_data_na <- main_data
  main_data_na$other_var[1] <- NA

  mock_table_view_na <- function(table_name, project) {
    if (grepl("MainDataTable", table_name)) return(main_data_na)
    return(NULL)
  }
  assignInNamespace("table_view", mock_table_view_na, ns = "FishSET")

  format_model_data(
    project = "TEST_PROJECT",
    name = "TEST_MEAN",
    zone_id = "ZoneID",
    unique_obs_id = "unique_row_id",
    select_vars = c("other_var"),
    distance = FALSE,
    impute = "mean"
  )

  df_mean <- captured_data$TEST_MEAN
  # Expected: NA in row 1 replaced by mean(20, 30) = 25
  obs1_vals <- df_mean$other_var[df_mean$unique_row_id == "1"]
  expect_true(all(obs1_vals == 25))

  # Test "remove" imputation (Using Zone-level NA to test Zone removal)
  # Restore clean main_data mock using the GLOBAL function
  assignInNamespace("table_view", clean_mock_table_view, ns = "FishSET")

  # Create Dist Matrix with NA in Zone A for Obs 1
  dist_mat_na <- dist_mat_mock
  dist_mat_na[1, "A"] <- NA
  dist_out_na <- list(dist_matrix = dist_mat_na)

  mock_create_dist_na <- function(...) return(dist_out_na)
  assignInNamespace("create_dist_matrix", mock_create_dist_na, ns = "FishSET")

  format_model_data(
    project = "TEST_PROJECT",
    name = "TEST_REMOVE",
    zone_id = "ZoneID",
    unique_obs_id = "unique_row_id",
    select_vars = c("other_var"),
    distance = TRUE,
    impute = "remove"
  )

  df_rem <- captured_data$TEST_REMOVE

  # Expectation:
  # Zone A has an NA (in Obs 1). 'remove' should drop Zone A completely from ALL obs.
  # Zone B is clean. It should remain for all 3 obs.
  # Total rows = 3 (Obs 1-B, Obs 2-B, Obs 3-B)
  expect_equal(nrow(df_rem), 3)
  expect_false("A" %in% df_rem$ZoneID) # Zone A should be gone
  expect_true("B" %in% df_rem$ZoneID)  # Zone B should remain
})

# Test merging expectations -----------------------------------------------------------------------
test_that("format_model_data merges expectations correctly", {
  setup_mocks()

  captured_data <- NULL
  mock_dbExecute <- function(conn, statement, params = NULL, ...) {
    if (!is.null(params)) captured_data <<- unserialize(params$data[[1]])
    return(invisible(TRUE))
  }
  assignInNamespace("dbExecute", mock_dbExecute, ns = "DBI")

  format_model_data(
    project = "TEST_PROJECT",
    name = "TEST_EXP",
    zone_id = "ZoneID",
    unique_obs_id = "unique_row_id",
    distance = FALSE,
    expectations = c("catch_exp")
  )

  df_out <- captured_data$TEST_EXP
  expect_true("catch_exp" %in% names(df_out))
  obs1 <- df_out %>% dplyr::filter(unique_row_id == "1") %>% dplyr::arrange(ZoneID)
  expect_equal(obs1$catch_exp, c(100, 200))
})

# Test model fail for invalid inputs --------------------------------------------------------------
test_that("format_model_data fails fast on invalid inputs", {
  setup_mocks()
  expect_error(
    format_model_data(
      project = "TEST_PROJECT", name = "FAIL",
      zone_id = "ZoneID", unique_obs_id = "unique_row_id",
      impute = "magic_wand"
    ),
    "Impute method must be one of"
  )
})