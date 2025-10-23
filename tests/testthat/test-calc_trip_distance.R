# -------------------------------------------------------------------------------------------------
# File: test-calc_trip_distance.R
# Purpose: To conduct unit tests for the calc_trip_distance() function.
# Description: This script uses the 'testthat' package to verify that calc_trip_distance()
#   correctly calculates the total distance of a fishing trip. It tests various
#   scenarios including multi-haul trips, single-haul trips, different distance units,
#   and custom column names. To create a controlled and isolated testing environment,
#   the script mocks several internal functions to prevent side effects like file
#   I/O or database connections.
#
# Scenarios tested:
#   - A successful run with default settings (output in miles).
#   - Correct calculation for a trip with multiple hauls.
#   - Correct calculation for a trip with only a single haul.
#   - Correct unit conversion for "kilometers" and "meters".
#   - Proper handling of custom input column names and a custom output column name.
#   - A warning is issued for invalid distance units.
#
# Notes:
#   - Internal data-handling functions are mocked to isolate the distance calculation
#     logic from any external dependencies.
# -------------------------------------------------------------------------------------------------

# Mock internal functions -------------------------------------------------------------------------
# Description: Override internal FishSET functions to prevent side effects like database
#              connections or file system access during testing. This allows the test to focus
#              solely on the logic of the calc_trip_distance() function.

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

# Create Mock Data --------------------------------------------------------------------------------
# Mock port data with names and coordinates
mock_port_data <- data.frame(
  Port_Name = c("ASTORIA", "NEWPORT"),
  Port_Lat = c(46.18, 44.62),
  Port_Long = c(-123.83, -124.05)
)

# Mock main haul data with multi-haul and single-haul trips
mock_main_data <- data.frame(
  TRIP_ID = rep(c(101, 202), times = c(2, 1)),
  HAUL_NUM = c(1, 2, 1),
  DEPART_PORT = c("ASTORIA", "ASTORIA", "NEWPORT"),
  LAND_PORT = c("ASTORIA", "ASTORIA", "NEWPORT"),
  START_LAT = c(46.2, 46.4, 44.7),
  START_LON = c(-123.9, -124.1, -124.1),
  END_LAT = c(46.3, 46.5, 44.8),
  END_LON = c(-124.0, -124.2, -124.2)
)

# Pre-calculated expected distances in meters for testing
# Trip 101 (multi-haul)
port_to_h1 <- geosphere::distGeo(c(-123.83, 46.18), c(-123.9, 46.2))
within_h1 <- geosphere::distGeo(c(-123.9, 46.2), c(-124.0, 46.3))
h1_to_h2 <- geosphere::distGeo(c(-124.0, 46.3), c(-124.1, 46.4))
within_h2 <- geosphere::distGeo(c(-124.1, 46.4), c(-124.2, 46.5))
h2_to_port <- geosphere::distGeo(c(-124.2, 46.5), c(-123.83, 46.18))
expected_dist_101_m <- port_to_h1 + within_h1 + h1_to_h2 + within_h2 + h2_to_port

# Trip 202 (single-haul)
port_to_h1_202 <- geosphere::distGeo(c(-124.05, 44.62), c(-124.1, 44.7))
within_h1_202 <- geosphere::distGeo(c(-124.1, 44.7), c(-124.2, 44.8))
h1_to_port_202 <- geosphere::distGeo(c(-124.2, 44.8), c(-124.05, 44.62))
expected_dist_202_m <- port_to_h1_202 + within_h1_202 + h1_to_port_202


# Test calc_trip_distance() -----------------------------------------------------------------------
test_that("calc_trip_distance calculates correctly with defaults (miles)", {
  # --- Execution ---
  result <- calc_trip_distance(
    dat = mock_main_data,
    port = mock_port_data,
    project = "test_proj",
    trip_id = "TRIP_ID",
    haul_order = "HAUL_NUM",
    starting_port = "DEPART_PORT",
    return_port = "LAND_PORT",
    start_haul_lat = "START_LAT",
    start_haul_lon = "START_LON",
    end_haul_lat = "END_LAT",
    end_haul_lon = "END_LON"
  )
  
  # --- Expectations ---
  # Returns the correct number of rows
  expect_equal(nrow(result), nrow(mock_main_data))
  # Adds the default column "trip_distance"
  expect_true("trip_distance" %in% names(result))
  
  # Check distance for the multi-haul trip (101)
  dist_101 <- result$trip_distance[result$TRIP_ID == 101][1]
  expect_equal(dist_101, expected_dist_101_m / 1609.34, tolerance = 1e-3)
  
  # Check distance for the single-haul trip (202)
  dist_202 <- result$trip_distance[result$TRIP_ID == 202]
  expect_equal(dist_202, expected_dist_202_m / 1609.34, tolerance = 1e-3)
})

test_that("calc_trip_distance handles unit conversions (km, meters)", {
  # --- Execution for Kilometers ---
  result_km <- calc_trip_distance(
    dat = mock_main_data, 
    port = mock_port_data, 
    project = "test_proj",
    trip_id = "TRIP_ID", 
    haul_order = "HAUL_NUM",
    starting_port = "DEPART_PORT", 
    return_port = "LAND_PORT",
    start_haul_lat = "START_LAT", 
    start_haul_lon = "START_LON",
    end_haul_lat = "END_LAT", 
    end_haul_lon = "END_LON",
    distance_unit = "kilometers"
  )
  
  # --- Expectations for Kilometers ---
  expect_equal(result_km$trip_distance[1], expected_dist_101_m / 1000, tolerance = 1e-3)
  expect_equal(result_km$trip_distance[3], expected_dist_202_m / 1000, tolerance = 1e-3)
  
  # --- Execution for Meters ---
  result_m <- calc_trip_distance(
    dat = mock_main_data, 
    port = mock_port_data, 
    project = "test_proj",
    trip_id = "TRIP_ID", 
    haul_order = "HAUL_NUM",
    starting_port = "DEPART_PORT", 
    return_port = "LAND_PORT",
    start_haul_lat = "START_LAT", 
    start_haul_lon = "START_LON",
    end_haul_lat = "END_LAT", 
    end_haul_lon = "END_LON",
    distance_unit = "meters"
  )
  
  # --- Expectations for Meters ---
  expect_equal(result_m$trip_distance[1], expected_dist_101_m, tolerance = 1e-3)
  expect_equal(result_m$trip_distance[3], expected_dist_202_m, tolerance = 1e-3)
})

test_that("calc_trip_distance works with custom column and output names", {
  # --- Setup ---
  # Create a copy with different column names
  custom_mock_data <- mock_main_data
  names(custom_mock_data) <- c("ID", "HAUL", "START_PORT", "END_PORT",
                               "S_LAT", "S_LON", "E_LAT", "E_LON")
  
  # --- Execution ---
  result <- calc_trip_distance(
    name = "Total_Dist_KM", # Custom output name
    dat = custom_mock_data,
    port = mock_port_data,
    project = "test_proj",
    trip_id = "ID",
    haul_order = "HAUL",
    starting_port = "START_PORT",
    return_port = "END_PORT",
    start_haul_lat = "S_LAT",
    start_haul_lon = "S_LON",
    end_haul_lat = "E_LAT",
    end_haul_lon = "E_LON",
    distance_unit = "km"
  )
  
  # --- Expectations ---
  expect_true("Total_Dist_KM" %in% names(result))
  expect_false("trip_distance" %in% names(result))
  expect_equal(result$Total_Dist_KM[1], expected_dist_101_m / 1000, tolerance = 1e-3)
})

test_that("calc_trip_distance warns on invalid unit and defaults to meters", {
  # --- Execution and Expectations ---
  expect_warning(
    result <- calc_trip_distance(
      dat = mock_main_data, port = mock_port_data, project = "test_proj",
      trip_id = "TRIP_ID", haul_order = "HAUL_NUM",
      starting_port = "DEPART_PORT", return_port = "LAND_PORT",
      start_haul_lat = "START_LAT", start_haul_lon = "START_LON",
      end_haul_lat = "END_LAT", end_haul_lon = "END_LON",
      distance_unit = "furlongs" # Invalid unit
    ),
    "Invalid distance_unit ' furlongs '. Defaulting to meters."
  )
  
  # Check that the output is in meters
  expect_equal(result$trip_distance[1], expected_dist_101_m, tolerance = 1e-3)
  expect_equal(result$trip_distance[3], expected_dist_202_m, tolerance = 1e-3)
})