# -------------------------------------------------------------------------------------------------
# File: test-calc_trip_centroid.R
# Purpose: To conduct unit tests for the calc_trip_centroid() function.
# Description: This script uses the 'testthat' package to verify that
#   calc_trip_centroid() correctly calculates geographic centroids for trips.
#   It tests both unweighted (simple average) and weighted calculations,
#   as well as robustness to NA values and invalid coordinate data.
#
# Scenarios tested:
#   - Correct calculation of an unweighted centroid (default).
#   - Correct calculation of a weighted centroid.
#   - Proper handling of trips with only a single record.
#   - Correct propagation of NA values (matching R's default behavior).
#   - Validation errors are thrown for invalid coordinate values.
#
# Notes:
#   - Internal data-handling functions are mocked to isolate the aggregation
#     logic of calc_trip_centroid() from external dependencies.
# -------------------------------------------------------------------------------------------------

# Mock internal functions -------------------------------------------------------------------------
# Description: Override internal FishSET functions to prevent side effects like database
#              connections or file system access during testing. This allows the test to focus
#              solely on the logic of the calc_trip_centroid() function.

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


# Create Mock Centroid Data -----------------------------------------------------------------------
# A data frame with multiple trips and "hauls" (records) to test centroid logic.
mock_centroid_data <- data.frame(
  TRIP_ID = c(101, 101, 101, 202, 202),
  START_LON = c(10.0, 11.0, 12.0, -50.0, -52.0),
  START_LAT = c(45.0, 46.0, 47.0, 60.0, 61.0),
  CATCH_KG = c(100, 200, 300, 25, 75) # Uneven weights for testing
)

# --- Expected Calculations ---
# Trip 101 (Unweighted):
#   Lon: mean(10, 11, 12) = 11.0
#   Lat: mean(45, 46, 47) = 46.0
# Trip 101 (Weighted):
#   Lon: (10*100 + 11*200 + 12*300) / (100+200+300) = 
#     (1000 + 2200 + 3600) / 600 = 6800 / 600 = 11.333...
#   Lat: (45*100 + 46*200 + 47*300) / (100+200+300) = 
#     (4500 + 9200 + 14100) / 600 = 27800 / 600 = 46.333...
#
# Trip 202 (Unweighted):
#   Lon: mean(-50, -52) = -51.0
#   Lat: mean(60, 61) = 60.5
# Trip 202 (Weighted):
#   Lon: (-50*25 + -52*75) / (25+75) = (-1250 - 3900) / 100 = -5150 / 100 = -51.5
#   Lat: (60*25 + 61*75) / (25+75) = (1500 + 4575) / 100 = 6075 / 100 = 60.75
# -----------------------------

# Test calc_trip_centroid() -----------------------------------------------------------------------
test_that("calc_trip_centroid() computes unweighted centroid (default)", {
  # --- Execution ---
  result <- calc_trip_centroid(
    dat = mock_centroid_data,
    project = "test_proj",
    lon = "START_LON",
    lat = "START_LAT",
    trip_id = "TRIP_ID",
    weight_var = NULL
  )
  
  # --- Expectations ---
  # Function adds columns, does not aggregate rows
  expect_equal(nrow(result), 5)
  expect_true(all(c("cent_lon", "cent_lat") %in% names(result)))
  
  # Check calculated values for each group
  expected_lon <- c(11.0, 11.0, 11.0, -51.0, -51.0)
  expected_lat <- c(46.0, 46.0, 46.0, 60.5, 60.5)
  
  expect_equal(result$cent_lon, expected_lon)
  expect_equal(result$cent_lat, expected_lat)
})

test_that("calc_trip_centroid() computes weighted centroid correctly", {
  # --- Execution ---
  result <- calc_trip_centroid(
    dat = mock_centroid_data,
    project = "test_proj",
    lon = "START_LON",
    lat = "START_LAT",
    trip_id = "TRIP_ID",
    weight_var = "CATCH_KG" # Specify weight
  )

  # --- Expectations ---
  expect_equal(nrow(result), 5)
  expect_true(all(c("cent_lon", "cent_lat") %in% names(result)))

  # Check calculated values for each group
  expected_lon <- c(11.3333333, 11.3333333, 11.3333333, -51.5, -51.5)
  expected_lat <- c(46.3333333, 46.3333333, 46.3333333, 60.75, 60.75)

  # Use tolerance for floating point comparisons
  expect_equal(result$cent_lon, expected_lon, tolerance = 1e-6)
  expect_equal(result$cent_lat, expected_lat, tolerance = 1e-6)
})

test_that("calc_trip_centroid() propagates NA values (default R behavior)", {
  # --- Setup ---
  mock_na_data <- data.frame(
    TRIP_ID = c(101, 101, 101),
    START_LON = c(10, 20, NA), # NA in coordinate
    START_LAT = c(5, 10, 15),
    CATCH_KG = c(100, 100, NA) # NA in weight
  )

  # --- Execution (Unweighted) ---
  # mean(10, 20, NA) -> NA (since default is na.rm = FALSE)
  # mean(5, 10, 15) -> 10
  result_unweighted <- calc_trip_centroid(
    dat = mock_na_data, project = "test_proj", lon = "START_LON",
    lat = "START_LAT", trip_id = "TRIP_ID"
  )

  # --- Execution (Weighted) ---
  # The function uses sum(), which defaults to na.rm = FALSE.
  # sum(c(10*100, 20*100, NA*NA)) -> NA
  # sum(c(5*100, 10*100, 15*NA)) -> NA
  result_weighted <- calc_trip_centroid(
    dat = mock_na_data, project = "test_proj", lon = "START_LON",
    lat = "START_LAT", trip_id = "TRIP_ID", weight_var = "CATCH_KG"
  )

  # --- Expectations ---
  expect_equal(result_unweighted$cent_lon, as.numeric(c(NA, NA, NA)))
  expect_equal(result_unweighted$cent_lat, c(10, 10, 10))

  expect_equal(result_weighted$cent_lon, as.numeric(c(NA, NA, NA)))
  expect_equal(result_weighted$cent_lat, as.numeric(c(NA, NA, NA)))
})

test_that("calc_trip_centroid() throws errors for invalid coordinates", {
  # --- Setup ---
  bad_lon_data <- data.frame(TRIP_ID = 1, START_LON = 200, START_LAT = 45)
  bad_lat_data <- data.frame(TRIP_ID = 1, START_LON = 170, START_LAT = -95)

  # --- Expectations ---
  expect_error(
    calc_trip_centroid(
      dat = bad_lon_data, project = "test_proj", lon = "START_LON",
      lat = "START_LAT", trip_id = "TRIP_ID"
    ),
    "Longitude is not valid"
  )

  expect_error(
    calc_trip_centroid(
      dat = bad_lat_data, project = "test_proj", lon = "START_LON",
      lat = "START_LAT", trip_id = "TRIP_ID"
    ),
    "Latitude is not valid"
  )
})

test_that("calc_trip_centroid() handles trips with a single record", {
  # --- Setup ---
  mock_single_record_data <- data.frame(
    TRIP_ID = c(101, 202),
    START_LON = c(10, -50),
    START_LAT = c(45, 60),
    CATCH_KG = c(100, 200)
  )

  # --- Execution (Unweighted) ---
  result_unweighted <- calc_trip_centroid(
    dat = mock_single_record_data, project = "test_proj", lon = "START_LON",
    lat = "START_LAT", trip_id = "TRIP_ID"
  )

  # --- Execution (Weighted) ---
  result_weighted <- calc_trip_centroid(
    dat = mock_single_record_data, project = "test_proj", lon = "START_LON",
    lat = "START_LAT", trip_id = "TRIP_ID", weight_var = "CATCH_KG"
  )

  # --- Expectations ---
  # The centroid of a single point is the point itself, weighted or not
  expected_lon <- c(10, -50)
  expected_lat <- c(45, 60)

  expect_equal(result_unweighted$cent_lon, expected_lon)
  expect_equal(result_unweighted$cent_lat, expected_lat)

  expect_equal(result_weighted$cent_lon, expected_lon)
  expect_equal(result_weighted$cent_lat, expected_lat)
})