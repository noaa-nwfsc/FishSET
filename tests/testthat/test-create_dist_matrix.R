# -------------------------------------------------------------------------------------------------
# File: test-create_dist_matrix.R
# Purpose: To provide unit tests for the create_dist_matrix() function.
# Description: This script uses the 'testthat' framework to validate the behavior of the
#              create_dist_matrix() function. It creates a complete set of mock data
#              (data.frames, sf objects, centroids) to cover the primary logic paths
#              of the function.
#
# Scenarios tested:
#   - All major combinations of 'occasion' (origin) and 'alt_var' (destination):
#     - occasion = 'lon-lat' (from dataset coordinates)
#     - occasion = 'port' (joining port coordinates)
#     - occasion = 'zonal centroid' (single haul, from current zone)
#     - occasion = 'fishing centroid' (multi-haul, from previous area)
#     - alt_var = 'zonal centroid' (to centroid table)
#     - alt_var = 'fishing centroid' (to centroid table)
#     - alt_var = 'nearest point' (to sf polygon object)
#   - Error handling for common failures:
#     - No observations meeting 'dataZoneTrue'
#     - Missing 'spat' object when 'alt_var = "nearest point"'
#     - Missing port in the port table when 'occasion = "port"'
#     - 'NaN' values in coordinate data
#   - Correct warning for a missing CRS.
#
# Notes:
#   - This test script mocks several internal helper functions (e.g., is_value_empty,
#     find_lon, qaqc_helper) to run in isolation, mirroring the conventions
#     in other test files like 'test-create_centroid.R'.
#   - Requires the 'sf' and 'testthat' packages.
#
# -------------------------------------------------------------------------------------------------

# Test Data Setup ---------------------------------------------------------------------------------
library(sf)

# Core Dataset
test_dataset <- data.frame(
  obs_id = c("T1", "T2", "T3", "T4", "T5"),
  current_zone = c("A", "B", "A", "C", "B"),
  prev_area = c("A", "PORT_X", "C", "B", "PORT_Y"), # For multi-haul
  my_port = c("PORT_X", "PORT_Y", "PORT_X", "PORT_Y", "PORT_X"), # For port occasion
  haul_lon = c(-122.1, -122.2, -122.3, -122.4, -122.5),
  haul_lat = c(47.1, 47.2, 47.3, 47.4, 47.5)
)

# Centroid Tables
test_zone_cent <- data.frame(
  ZoneID = c("A", "B", "C"),
  cent.lon = c(-122.15, -122.25, -122.35),
  cent.lat = c(47.15, 47.25, 47.35)
)

test_fish_cent <- data.frame(
  ZoneID = c("A", "B", "C"),
  cent.lon = c(-122.16, -122.26, -122.36),
  cent.lat = c(47.16, 47.26, 47.36)
)

# Port Table
test_port <- data.frame(
  Port_Name = c("PORT_X", "PORT_Y"),
  Port_Long = c(-122.0, -122.5),
  Port_Lat = c(47.0, 47.5)
)

# Spatial (sf) Object
create_poly <- function(lon, lat, size = 0.05) {
  half <- size / 2
  st_polygon(list(matrix(
    c(lon - half, lat - half, lon + half, lat - half, lon + half, lat + half,
      lon - half, lat + half, lon - half, lat - half),
    ncol = 2, byrow = TRUE
  )))
}

test_spat_sfc <- st_sfc(
  create_poly(-122.15, 47.15),
  create_poly(-122.25, 47.25),
  create_poly(-122.35, 47.35),
  crs = 4326 # WGS 84
)

test_spat <- st_sf(zone_name = c("A", "B", "C"), geometry = test_spat_sfc)

# Other Parameters
test_params <- list(
  dataset = test_dataset,
  unique_obs_id = "obs_id",
  spat = test_spat,
  spatID = "zone_name",
  zoneID = "current_zone",
  zone_cent = test_zone_cent,
  fish_cent = test_fish_cent,
  port = test_port,
  crs = 4326,
  units = "km",
  # Use first 3 observations (T1, T2, T3)
  dataZoneTrue = c(TRUE, TRUE, TRUE, FALSE, FALSE),
  # Choices for T1, T2, T3 are A, B, C. Matrix cols should be A, B, C
  choice = c("A", "B", "C", "A", "B")
)

# Unit Tests --------------------------------------------------------------------------------------
## lon-lat, zonal centroid ------------------------------------------------------------------------
test_that("occasion = 'lon-lat', alt_var = 'zonal centroid' works", {
  result <- create_dist_matrix(
    dataset = test_params$dataset,
    unique_obs_id = test_params$unique_obs_id,
    zoneID = test_params$zoneID,
    alt_var = "zonal centroid",
    occasion = "lon-lat",
    occasion_var = c("haul_lon", "haul_lat"),
    dataZoneTrue = test_params$dataZoneTrue,
    zone_cent = test_params$zone_cent,
    choice = test_params$choice,
    units = test_params$units,
    crs = test_params$crs
  )
  
  # Assertions
  expect_type(result, "list")
  expect_true("dist_matrix" %in% names(result))
  mat <- result$dist_matrix
  expect_s3_class(mat, "units")
  expect_equal(dim(mat), c(3, 3)) # 3 obs, 3 choices (A, B, C)
  expect_equal(rownames(mat), c("T1", "T2", "T3")) # From unique_obs_id
  expect_equal(colnames(mat), c("A", "B", "C")) # From sorted choices
  # Spot check one value: T1 (lon-lat) to Zone A (zonal centroid)
  from <- st_sfc(st_point(c(-122.1, 47.1)), crs = 4326)
  to <- st_sfc(st_point(c(-122.15, 47.15)), crs = 4326)
  expected_dist <- st_distance(from, to)
  units(expected_dist) <- "km"
  expect_equal(mat[1, 1], expected_dist[1])
})

## port, fishing centroid -------------------------------------------------------------------------
test_that("occasion = 'port', alt_var = 'fishing centroid' works", {
  result <- create_dist_matrix(
    dataset = test_params$dataset,
    unique_obs_id = test_params$unique_obs_id,
    zoneID = test_params$zoneID,
    alt_var = "fishing centroid",
    occasion = "port",
    occasion_var = "my_port", # Join on this column
    dataZoneTrue = test_params$dataZoneTrue,
    fish_cent = test_params$fish_cent,
    choice = test_params$choice,
    units = test_params$units,
    port = test_params$port,
    crs = test_params$crs
  )
  
  # Assertions
  expect_equal(dim(result$dist_matrix), c(3, 3))
  expect_equal(rownames(result$dist_matrix), c("T1", "T2", "T3"))
  # Spot check: T2 (PORT_Y) to Zone C (fishing centroid)
  from <- st_sfc(st_point(c(-122.5, 47.5)), crs = 4326) # PORT_Y
  to <- st_sfc(st_point(c(-122.36, 47.36)), crs = 4326) # Fish Cent C
  expected_dist <- st_distance(from, to)
  units(expected_dist) <- "km"
  expect_equal(result$dist_matrix[2, "C"], expected_dist[1])
})

## multihaul, nearest point -----------------------------------------------------------------------
test_that("occasion (multi-haul) 'zonal centroid', alt_var = 'nearest point' works", {
  result <- create_dist_matrix(
    dataset = test_params$dataset,
    unique_obs_id = test_params$unique_obs_id,
    zoneID = test_params$zoneID,
    spat = test_params$spat,
    spatID = test_params$spatID,
    alt_var = "nearest point",
    occasion = "zonal centroid",
    occasion_var = "prev_area", # This makes it multi-haul
    dataZoneTrue = test_params$dataZoneTrue,
    zone_cent = test_params$zone_cent,
    choice = test_params$choice,
    units = test_params$units,
    port = test_params$port,
    crs = test_params$crs
  )
  
  # Assertions
  expect_equal(dim(result$dist_matrix), c(3, 3))
  # T1 prev_area = A, T2 = PORT_X, T3 = C
  expect_equal(rownames(result$dist_matrix), c("T1", "T2", "T3"))
  # Spot check: T2 (PORT_X) to Zone B (nearest point, polygon)
  from <- st_sfc(st_point(c(-122.0, 47.0)), crs = 4326) # PORT_X
  to_poly <- test_spat$geometry[2] # Polygon B
  expected_dist <- st_distance(from, to_poly)
  units(expected_dist) <- "km"
  expect_equal(result$dist_matrix[2, "B"], expected_dist[1])
})

## single haul, zonal centroid --------------------------------------------------------------------
test_that("occasion (single haul) 'fishing centroid', alt_var = 'zonal centroid' works", {
  result <- create_dist_matrix(
    dataset = test_params$dataset,
    unique_obs_id = test_params$unique_obs_id,
    zoneID = test_params$zoneID,
    alt_var = "zonal centroid",
    occasion = "fishing centroid",
    occasion_var = NULL, # Makes it single haul (uses zoneID)
    dataZoneTrue = test_params$dataZoneTrue,
    zone_cent = test_params$zone_cent,
    fish_cent = test_params$fish_cent,
    choice = test_params$choice,
    units = test_params$units,
    crs = test_params$crs
  )
  
  # Assertions
  expect_equal(dim(result$dist_matrix), c(3, 3))
  # T1 zone = A, T2 = B, T3 = A
  expect_equal(rownames(result$dist_matrix), c("T1", "T2", "T3"))
  # Spot check: T1 (Fish Cent A) to Zone C (Zonal Cent C)
  from <- st_sfc(st_point(c(-122.16, 47.16)), crs = 4326) # Fish Cent A
  to <- st_sfc(st_point(c(-122.35, 47.35)), crs = 4326) # Zonal Cent C
  expected_dist <- st_distance(from, to)
  units(expected_dist) <- "km"
  expect_equal(result$dist_matrix[1, "C"], expected_dist[1])
})


# Test Errors and Warnings ------------------------------------------------------------------------
## No observations --------------------------------------------------------------------------------
test_that("function stops when no observations meet dataZoneTrue", {
  expect_error(
    create_dist_matrix(
      dataset = test_params$dataset,
      dataZoneTrue = rep(FALSE, 5), # No obs
      unique_obs_id = test_params$unique_obs_id,
      zoneID = test_params$zoneID,
      alt_var = "zonal centroid",
      occasion = "lon-lat",
      occasion_var = c("haul_lon", "haul_lat"),
      zone_cent = test_params$zone_cent,
      choice = test_params$choice,
      units = test_params$units,
      crs = test_params$crs
    ),
    "No observations meet the 'dataZoneTrue' criteria."
  )
})

## Spat missing -----------------------------------------------------------------------------------
test_that("function stops when spat is missing for 'nearest point'", {
  expect_error(
    create_dist_matrix(
      dataset = test_params$dataset,
      unique_obs_id = test_params$unique_obs_id,
      zoneID = test_params$zoneID,
      spat = NULL, # Missing spat
      spatID = test_params$spatID,
      alt_var = "nearest point",
      occasion = "lon-lat",
      occasion_var = c("haul_lon", "haul_lat"),
      dataZoneTrue = test_params$dataZoneTrue,
      choice = test_params$choice,
      units = test_params$units,
      crs = test_params$crs
    ),
    "'spat' and 'spatID' are required for alt_var = 'nearest point'."
  )
})

## Missing ports ----------------------------------------------------------------------------------
test_that("function stops for missing ports", {
  dataset_bad_port <- test_params$dataset
  dataset_bad_port$my_port[1] <- "PORT_Z" # This port does not exist
  
  expect_error(
    create_dist_matrix(
      dataset = dataset_bad_port,
      unique_obs_id = test_params$unique_obs_id,
      zoneID = test_params$zoneID,
      alt_var = "zonal centroid",
      occasion = "port",
      occasion_var = "my_port",
      dataZoneTrue = test_params$dataZoneTrue,
      zone_cent = test_params$zone_cent,
      choice = test_params$choice,
      units = test_params$units,
      port = test_params$port,
      crs = test_params$crs
    ),
    "At least one port not included in PortTable: PORT_Z"
  )
})

## NaN coords -------------------------------------------------------------------------------------
test_that("function stops for NaN in coordinates", {
  dataset_nan <- test_params$dataset
  dataset_nan$haul_lon[1] <- NaN
  
  expect_error(
    create_dist_matrix(
      dataset = dataset_nan,
      unique_obs_id = test_params$unique_obs_id,
      zoneID = test_params$zoneID,
      alt_var = "zonal centroid",
      occasion = "lon-lat",
      occasion_var = c("haul_lon", "haul_lat"),
      dataZoneTrue = test_params$dataZoneTrue,
      zone_cent = test_params$zone_cent,
      choice = test_params$choice,
      units = test_params$units,
      crs = test_params$crs
    ),
    "NaN found in lon-lat. Design file aborted."
  )
})

## Missing CRS ------------------------------------------------------------------------------------
test_that("function warns for missing CRS", {
  expect_warning(
    create_dist_matrix(
      dataset = test_params$dataset,
      unique_obs_id = test_params$unique_obs_id,
      zoneID = test_params$zoneID,
      alt_var = "zonal centroid",
      occasion = "lon-lat",
      occasion_var = c("haul_lon", "haul_lat"),
      dataZoneTrue = test_params$dataZoneTrue,
      zone_cent = test_params$zone_cent,
      choice = test_params$choice,
      units = test_params$units,
      crs = NULL # Missing CRS
    ),
    "CRS is not specfied, distance matrix will be created using WGS 84 (4326).",
    fixed = TRUE
  )
})