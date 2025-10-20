# -------------------------------------------------------------------------------------------------
# File: test-lag_zone.R
# Purpose: To conduct unit tests for the lag_zone() function.
# Description: This script uses the 'testthat' package to verify that the lag_zone()
#   function correctly generates a lagged zone variable. It confirms that the first haul of each
#   trip is assigned the zone of the departure port and subsequent hauls are assigned the zone of
#   the preceding haul. To create a controlled and isolated testing environment, the script mocks
#   several functions (log_call, data_pull, parse_data_name, and
#   assignment_column) and uses self-contained, in-memory data frames.
#
# Scenarios tested:
#   - A successful run with multiple trips and hauls to ensure correct lagging.
#   - A run with a single trip to verify behavior with simpler data.
#   - A run where trips have only one haul to check edge-case handling.
#   - Proper handling of different column names for key variables.
#
# Notes:
#   - Several internal functions are mocked to isolate the lagging logic of lag_zone()
#     from external dependencies like file I/O or database connections.
# -------------------------------------------------------------------------------------------------

# Mock internal functions -------------------------------------------------------------------------
# Description: Override internal FishSET functions to prevent side effects like database
#              connections or file system access during testing. This allows the test to focus
#              solely on the logic of the lag_zone() function.

# Define the mock functions
mock_log_call <- function(...) invisible(NULL)
mock_data_pull <- function(dat, project) list(dataset = dat)
mock_parse_data_name <- function(dat, type, project) dat
mock_assignment_column <- function(dat, ...) {
  # This mock now robustly simulates the output of a spatial join.
  # It checks which port identifier is present and creates the port_zone
  # column, which is predictable for testing.
  if ("port_name" %in% names(dat)) {
    dat$port_zone <- paste0("zone_", dat$port_name)
  } else if ("PORT_CODE" %in% names(dat)) {
    dat$port_zone <- paste0("zone_", dat$PORT_CODE)
  }
  dat
}

# Save the original functions from the package namespace
original_log_call <- get("log_call", envir = as.environment("package:FishSET"))
original_data_pull <- get("data_pull", envir = as.environment("package:FishSET"))
original_parse_data_name <- get("parse_data_name", envir = as.environment("package:FishSET"))
original_assignment_column <- get("assignment_column", envir = as.environment("package:FishSET"))

# Schedule the restoration of all original functions.
# on.exit() ensures this code runs when the script finishes, even if tests fail.
on.exit({
  assignInNamespace("log_call", original_log_call, ns = "FishSET")
  assignInNamespace("data_pull", original_data_pull, ns = "FishSET")
  assignInNamespace("parse_data_name", original_parse_data_name, ns = "FishSET")
  assignInNamespace("assignment_column", original_assignment_column, ns = "FishSET")
})

# Overwrite the real functions with our mocks
assignInNamespace("log_call", mock_log_call, ns = "FishSET")
assignInNamespace("data_pull", mock_data_pull, ns = "FishSET")
assignInNamespace("parse_data_name", mock_parse_data_name, ns = "FishSET")
assignInNamespace("assignment_column", mock_assignment_column, ns = "FishSET")

# Create Mock Spatial Data ------------------------------------------------------------------------
# A simple sf data frame with three polygons to represent spatial zones.
# This object is passed to the function to ensure it handles spatial inputs correctly.
poly_a <- st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0))))
poly_b <- st_polygon(list(rbind(c(2, 2), c(3, 2), c(3, 3), c(2, 3), c(2, 2))))
poly_c <- st_polygon(list(rbind(c(4, 4), c(5, 4), c(5, 5), c(4, 5), c(4, 4))))
mock_spat_data <- st_sf(
  zone_id = c("zone_A", "zone_B", "zone_C"),
  geometry = st_sfc(poly_a, poly_b, poly_c),
  crs = 4326
)

# Test lag_zone() ---------------------------------------------------------------------------------
test_that("lag_zone() correctly lags zones for multiple trips", {
  # --- Setup ---
  # Mock primary haul data
  haul_data <- data.frame(
    trip = c(1, 1, 1, 2, 2),
    haul = c(1, 2, 3, 1, 2),
    port = c("A", "A", "A", "B", "B"),
    zone = c("Z1", "Z2", "Z1", "Z3", "Z2")
  )
  
  # Mock port data
  port_data <- data.frame(
    port_name = c("A", "B"),
    port_lon = c(-123, -124),
    port_lat = c(45, 46)
  )
  
  # --- Execution ---
  # Run the function with test data
  result <- lag_zone(
    dat = haul_data,
    project = "test_proj",
    spat = mock_spat_data, # Mocked, not used
    port = port_data,
    port_name = "port_name",
    port_lon = "port_lon",
    port_lat = "port_lat",
    trip_id = "trip",
    haul_order = "haul",
    starting_port = "port",
    zoneID_dat = "zone",
    zoneID_spat = "zone_id", # Mocked, not used
    name = "start_loc"
  )
  
  # --- Expectations ---
  # Expected lagged zone vector
  expected_lag <- c("zone_A", "Z1", "Z2", "zone_B", "Z3")
  
  # The new column should exist
  expect_true("start_loc" %in% names(result))
  # The values in the new column should match the expected logic
  expect_equal(result$start_loc, expected_lag)
  # The number of rows should be unchanged
  expect_equal(nrow(result), 5)
})

test_that("lag_zone() works with different column names and single trips", {
  # --- Setup ---
  # Mock primary haul data with different column names
  haul_data_alt <- data.frame(
    TRIP_ID = c(101, 101, 101),
    HAUL_NUM = c(1, 2, 3),
    DEPARTURE_PORT = c("C", "C", "C"),
    AREA = c("A10", "A20", "A30")
  )
  
  # Mock port data with different column names
  port_data_alt <- data.frame(
    PORT_CODE = c("C", "D"),
    LON = c(-123, -124),
    LAT = c(45, 46)
  )
  
  # --- Execution ---
  # Run the function with the alternative data
  result <- lag_zone(
    dat = haul_data_alt,
    project = "test_proj",
    spat = NULL, # Mocked
    port = port_data_alt,
    port_name = "PORT_CODE",
    port_lon = "LON",
    port_lat = "LAT",
    trip_id = "TRIP_ID",
    haul_order = "HAUL_NUM",
    starting_port = "DEPARTURE_PORT",
    zoneID_dat = "AREA",
    zoneID_spat = NULL, # Mocked
    name = "lagged_area"
  )
  
  # --- Expectations ---
  # Expected lagged zone vector for this single trip
  expected_lag <- c("zone_C", "A10", "A20")
  
  # The new column should have the specified name
  expect_true("lagged_area" %in% names(result))
  # The values should be correctly lagged
  expect_equal(result$lagged_area, expected_lag)
})

test_that("lag_zone() handles trips with only one haul", {
  # --- Setup ---
  # Mock data where each trip has only one haul
  haul_data_single <- data.frame(
    trip = c(1, 2, 3),
    haul = c(1, 1, 1),
    port = c("A", "B", "A"),
    zone = c("Z1", "Z3", "Z1")
  )
  
  port_data <- data.frame(
    port_name = c("A", "B"),
    port_lon = c(-123, -124),
    port_lat = c(45, 46)
  )
  
  # --- Execution ---
  # Run the function
  result <- lag_zone(
    dat = haul_data_single,
    project = "test_proj",
    spat = NULL, # Mocked
    port = port_data,
    port_name = "port_name",
    port_lon = "port_lon",
    port_lat = "port_lat",
    trip_id = "trip",
    haul_order = "haul",
    starting_port = "port",
    zoneID_dat = "zone",
    zoneID_spat = NULL, # Mocked
    name = "start_loc"
  )
  
  # --- Expectations ---
  # For single-haul trips, the lagged value should always be the port zone
  expected_lag <- c("zone_A", "zone_B", "zone_A")
  
  # The new column should exist
  expect_true("start_loc" %in% names(result))
  # All values should be the port zones
  expect_equal(result$start_loc, expected_lag)
  # The number of rows should be unchanged
  expect_equal(nrow(result), 3)
})