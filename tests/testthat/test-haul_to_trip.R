# -------------------------------------------------------------------------------------------------
# File: test-haul_to_trip.R
# Purpose: To conduct unit tests for the haul_to_trip() function.
# Description: This script uses the 'testthat' package to verify that haul_to_trip()
#   correctly aggregates haul-level data to the trip level. It tests various
#   aggregation methods for numeric, character, date, and zone data types, as well
#   as the optional haul counting feature. To create a controlled and isolated
#   testing environment, the script mocks several internal functions to prevent
#   side effects like file I/O or database connections.
#
# Scenarios tested:
#   - A successful run with default aggregation methods.
#   - Correct aggregation using specific methods like "sum" for numerics, "paste"
#     for characters, and "max" for dates.
#   - Proper handling of a single trip with multiple hauls.
#   - Proper handling of trips with only one haul each.
#   - Verification that the haul counting feature works as expected.
#   - Correct creation of a composite trip ID from multiple columns.
#
# Notes:
#   - Internal data-handling functions are mocked to isolate the aggregation
#     logic of haul_to_trip() from external dependencies.
# -------------------------------------------------------------------------------------------------

# Mock internal functions -------------------------------------------------------------------------
# Description: Override internal FishSET functions to prevent side effects like database
#              connections or file system access during testing. This allows the test to focus
#              solely on the logic of the haul_to_trip() function.

# Define the mock functions
mock_log_call <- function(...) invisible(NULL)
mock_data_pull <- function(dat, project) list(dataset = dat)
mock_parse_data_name <- function(dat, type, project) dat

# Save the original functions from the package namespace
original_log_call <- get("log_call", envir = as.environment("package:FishSET"))
original_data_pull <- get("data_pull", envir = as.environment("package:FishSET"))
original_parse_data_name <- get("parse_data_name", envir = as.environment("package:FishSET"))

# Schedule the restoration of all original functions.
on.exit({
  assignInNamespace("log_call", original_log_call, ns = "FishSET")
  assignInNamespace("data_pull", original_data_pull, ns = "FishSET")
  assignInNamespace("parse_data_name", original_parse_data_name, ns = "FishSET")
})

# Overwrite the real functions with our mocks
assignInNamespace("log_call", mock_log_call, ns = "FishSET")
assignInNamespace("data_pull", mock_data_pull, ns = "FishSET")
assignInNamespace("parse_data_name", mock_parse_data_name, ns = "FishSET")

# Create Mock Haul Data ---------------------------------------------------------------------------
# A data frame with multiple trips and hauls to test aggregation logic.
mock_haul_data <- data.frame(
  PERMIT = rep(c("P101", "P202"), times = c(3, 2)),
  PORT = rep(c("AST", "BRK"), times = c(3, 2)),
  HAUL_DATE = as.Date("2025-01-10") + c(0, 1, 2, 0, 1),
  UTC_TIME = as.POSIXct("2025-01-10 12:00:00", tz = "UTC") + c(0:4 * 3600),
  WEIGHT_KG = c(100, 150, 125, 200, 250),
  GEAR = c("TRAWL", "TRAWL", "TRAWL", "LONG-LINE", "LONG-LINE"),
  ZONE = c("ZN1", "ZN1", "ZN2", "ZN3", "ZN3")
)

test_that("haul_to_trip() applies custom aggregation functions correctly", {
  # --- Execution ---
  result <- haul_to_trip(
    dat = mock_haul_data,
    project = "test_proj",
    trip_id = "PERMIT", # Single ID
    zoneID_dat = "ZONE",
    zone_fun = "first",
    date_fun = "max",
    num_fun = "sum",
    char_fun = "paste",
    haul_count = FALSE, # Test turning this off
    log_fun = FALSE
  )
  
  # --- Expectations ---
  expect_equal(nrow(result), 2)
  # Haul count should be absent
  expect_false("haul_count" %in% names(result))
  # Numeric function is 'sum'
  expect_equal(result$WEIGHT_KG, c(375, 450))
  # Zone function is 'first'
  expect_equal(result$ZONE, c("ZN1", "ZN3"))
  # Character function is 'paste'
  expect_equal(result$GEAR, c("TRAWL", "LONG-LINE"))
  # Date function is 'max'
  expect_equal(result$HAUL_DATE, as.Date(c("2025-01-12", "2025-01-11")))
  # POSIXct dates should also use date_fun
  expect_equal(as.POSIXct(result$UTC_TIME, tz = "UTC"), 
               as.POSIXct(c("2025-01-10 14:00:00", "2025-01-10 16:00:00"), tz = "UTC"))
})

test_that("haul_to_trip() handles trips with a single haul", {
  # --- Setup ---
  mock_single_haul_data <- data.frame(
    PERMIT = c("P101", "P202", "P303"),
    HAUL_DATE = as.Date("2025-01-10"),
    WEIGHT_KG = c(100, 200, 150),
    ZONE = c("ZN1", "ZN2", "ZN1")
  )

  # --- Execution ---
  result <- haul_to_trip(
    dat = mock_single_haul_data,
    project = "test_proj",
    trip_id = "PERMIT",
    zoneID_dat = "ZONE",
    num_fun = "sum",
    log_fun = FALSE
  )

  # --- Expectations ---
  # Should return the same number of rows as input
  expect_equal(nrow(result), 3)
  # Haul counts should all be 1
  expect_equal(result$haul_count, c(1, 1, 1))
  # Sum of one number is just the number itself
  expect_equal(result$WEIGHT_KG, c(100, 200, 150))
  # Data should otherwise be identical to input
  expect_equal(result$ZONE, c("ZN1", "ZN2", "ZN1"))
})

test_that("haul_to_trip() handles data for a single trip", {
  # --- Setup ---
  mock_one_trip_data <- data.frame(
    PERMIT = rep("P101", 3),
    HAUL_DATE = as.Date("2025-01-10") + 0:2,
    WEIGHT_KG = c(100, 150, 125),
    ZONE = c("ZN1", "ZN1", "ZN2")
  )

  # --- Execution ---
  result <- haul_to_trip(
    dat = mock_one_trip_data,
    project = "test_proj",
    trip_id = "PERMIT",
    zoneID_dat = "ZONE",
    num_fun = "mean",
    log_fun = FALSE
  )

  # --- Expectations ---
  # Should collapse to a single row
  expect_equal(nrow(result), 1)
  # Haul count should be 3
  expect_equal(result$haul_count, 3)
  # Mean of weights should be 125
  expect_equal(result$WEIGHT_KG, 125)
  # Mode of Zone should be ZN1
  expect_equal(result$ZONE, "ZN1")
})