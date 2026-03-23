# -------------------------------------------------------------------------------------------------
# File: test-ID_var.R
# Purpose: Unit tests for creating trip/haul level id
# Description:
#   This script tests the that `ID_var()` function returns a new variable in the primary data table
#   and how it is displayed (type, separator, etc.). It also checks if some arguments are not 
#   provided, then it will create it based on row number or expect an error.
#
#
# Scenarios tested:
#   - Two variables and name provided and return as string
#   - Two variables and name provided and return as integer
#   - Using different separator ('-' instead of default '_')
#   - Dropping variables used to create new id variable
#   - No variables provided, but a name is
#   - No variables or name, so error is expected
#
# Notes:
#   - Uses test project: "s1"
#   - Assumes access to example model and spatial objects for reproducibility.
# -------------------------------------------------------------------------------------------------

# Environment Setup -------------------------------------------------------------------------------
# Mock log_call to prevent creating permanent log files in your dummy test directory
mock_log_call <- function(...) { invisible(NULL) }
original_log_call <- getFromNamespace("log_call", "FishSET")
assignInNamespace("log_call", mock_log_call, ns = "FishSET")

withr::defer({
  assignInNamespace("log_call", original_log_call, ns = "FishSET")
}, envir = testthat::teardown_env())

# Create dummy data so we don't rely on physical SQLite databases, paths, or table_view()
dummy_main_data <- data.frame(
  ZoneID = c("387312", "387312", "387313", "387314", "387315"),
  TRIPID = c("22", "23", "22", "24", "25"),
  GEARCODE = c("G1", "G1", "G2", "G3", "G1"),
  stringsAsFactors = FALSE
)

# Test default settings ---------------------------------------------------------------------------
test_that("ID_var creates correct string ID with default setting", {
  
  result <- ID_var(dat = dummy_main_data,
                   project = "dummy_proj",
                   vars = c("ZoneID", "TRIPID"),
                   name = "PermitID",
                   type = "string",
                   log_fun = FALSE)
  
  expect_true("PermitID" %in% names(result))
  expect_equal(result$PermitID[1], "387312_22")
  expect_type(result$PermitID, "character")
  # Ensure original vars are not dropped
  expect_true(all(c("GEARCODE", "TRIPID", "ZoneID") %in% names(result))) 
})


# Test with integer ID ----------------------------------------------------------------------------
test_that("ID_var creates correct integer ID", {
  
  result <- ID_var(dat = dummy_main_data,
                   project = "dummy_proj",
                   vars = c("ZoneID", "TRIPID"),
                   name = "PermitID",
                   type = "integer",
                   log_fun = FALSE)
  
  expect_true("PermitID" %in% names(result))
  expect_type(result$PermitID, "integer")
})


# Test with custom separator ----------------------------------------------------------------------
test_that("ID_var uses custom separator for string ID", {
  
  result <- ID_var(dat = dummy_main_data,
                   project = "dummy_proj",
                   vars = c("ZoneID", "TRIPID"),
                   name = "PermitID",
                   log_fun = FALSE,
                   sep = "-")
  
  expect_true("PermitID" %in% names(result))
  expect_type(result$PermitID, "character")
  expect_equal(result$PermitID[1], "387312-22")
})


# Test with drop = TRUE ---------------------------------------------------------------------------
test_that("ID_var drops original variables when drop = TRUE", {
  
  result <- ID_var(dat = dummy_main_data,
                   project = "dummy_proj",
                   vars = c("ZoneID", "TRIPID"),
                   name = "PermitID",
                   log_fun = FALSE,
                   drop = TRUE)
  
  expect_true("PermitID" %in% names(result))
  expect_false(all(c("TRIPID", "ZoneID") %in% names(result)))
})


# Test with vars empty and name provided ----------------------------------------------------------
test_that("ID_var creates row_id when vars is empty and name is provided", {
  
  result <- ID_var(dat = dummy_main_data,
                   project = "dummy_proj",
                   name = "PermitID",
                   vars = NULL,
                   log_fun = FALSE)
  
  expect_true("PermitID" %in% names(result))
  expect_equal(result$PermitID[1], "1")
  expect_equal(result$PermitID[5], "5")
})


# Test with vars and name empty -------------------------------------------------------------------
test_that("ID_var stops if vars is empty and name is NULL", {
  
  expect_error(ID_var(dat = dummy_main_data,
                      project = "dummy_proj",
                      vars = NULL,
                      log_fun = FALSE))
})