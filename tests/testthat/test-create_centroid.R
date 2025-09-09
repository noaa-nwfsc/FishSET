# -------------------------------------------------------------------------------------------------
# File: test-create_centroid.R
# Purpose: 
# Description: 
#   
# Scenarios tested:
#
# Notes:
#
# -------------------------------------------------------------------------------------------------

# Create mock log_call() --------------------------------------------------------------------------
# Description: Override the FishSET::log_call() function to avoid errors with connecting when
#              running spat_qaqc() through unit tests
# Get a reference to the original log_call function from its namespace.
# We'll use this to restore it later.
original_log_call <- get("log_call", envir = as.environment("package:FishSET"))

# This is the fake function that will do nothing.
mock_log_call <- function(...) {
  # This function does nothing and returns invisibly.
  invisible(NULL)
}

# On exit, restore the original function to the namespace. This is crucial
# for clean testing and prevents side effects.
on.exit({
  assignInNamespace("log_call", original_log_call, ns = "FishSET")
})

# Now, replace the original log_call with our mock function.
assignInNamespace("log_call", mock_log_call, ns = "FishSET")

# Generate test data ------------------------------------------------------------------------------
# 1. Create spatial data (spat) with longitude from -180 to 180
poly1 <- st_polygon(list(rbind(c(-10, 50), c(-10, 55), c(0, 55), c(0, 50), c(-10, 50))))
poly2 <- st_polygon(list(rbind(c(0, 50), c(0, 55), c(10, 55), c(10, 50), c(0, 50))))
spat_180 <- st_as_sf(data.frame(zone_id = c("A", "B"), geom = st_sfc(poly1, poly2)), crs = 4326)

# 2. Create spatial data (spat) with longitude from 0 to 360 (crossing dateline)
# This polygon will be from 350 to 10 (crossing 360/0)
poly3 <- st_polygon(list(rbind(c(355, 50), c(355, 55), c(365, 55), c(365, 50), c(355, 50))))
spat_360 <- st_as_sf(data.frame(zone_id = c("C"), geom = st_sfc(poly3)), crs = 4326)
# Manually adjust bbox because sf can be tricky with 0-360 ranges.
class(spat_360$geom) <- c("sfc_POLYGON", "sfc")
attr(spat_360$geom, "bbox") <- c(xmin = 355, ymin = 50, xmax = 365, ymax = 55)

# 3. Create main dataset (dat)
dat_main <- data.frame(
  haul_id = 1:6,
  my_zone = rep(c("A", "B"), each = 3),
  h_lon = c(-8, -9, -7, 2, 3, 1),
  h_lat = c(51, 52, 53, 51, 52, 53),
  catch_kg = c(100, 200, 150, 300, 100, 250)
)

# Test zonal centroid -180,180 --------------------------------------------------------------------
test_that("test create_centroid() zonal centroid for -180 to 180 bbox", {
  # Define the base folder path to the test data directory
  # This folder should contain the subfolder named "s1" to pass the test
  test_folder <- testthat::test_path("testdata/FishSETFolder")
  
  # Override the folder path used by locproject() which is nested within projects()
  # This isolates the test env from the default paths
  # withr::local_options(list(test_folder_path = test_folder))
  old_option <- getOption("test_folder_path")
  options(test_folder_path = test_folder)
  
  result <- suppressMessages(suppressWarnings(
    create_centroid(
      spat = "s1spatSpatTable",
      dat = "s1MainDataTable",
      project = "s1",
      spatID = "TEN_ID",
      type = "zonal centroid",
      cent.name = "_",
      output = "centroid table"
    )
  ))
  
  # Check that centroid columns were added
  expect_true("cent.lon" %in% names(result))
  expect_true("cent.lat" %in% names(result))
  
  # Check output dimensions
  expect_equal(dim(result), c(4537, 3))
})

# Test zonal centroid 0,360 -----------------------------------------------------------------------
test_that("test create_centroid() zonal centroid for 0 to 360 bbox", {
  # Define the base folder path to the test data directory
  # This folder should contain the subfolder named "s1" to pass the test
  test_folder <- testthat::test_path("testdata/FishSETFolder")
  
  # Override the folder path used by locproject() which is nested within projects()
  # This isolates the test env from the default paths
  # withr::local_options(list(test_folder_path = test_folder))
  old_option <- getOption("test_folder_path")
  options(test_folder_path = test_folder)
  
  test_spat <- table_view("s1spatSpatTable", "s1")
  test_spat <- st_shift_longitude(test_spat)
  
  result <- suppressMessages(suppressWarnings(
    create_centroid(
      spat = test_spat,
      dat = "s1MainDataTable",
      project = "s1",
      spatID = "TEN_ID",
      type = "zonal centroid",
      cent.name = "_",
      output = "centroid table"
    )
  ))
  
  # Check that centroid columns were added
  expect_true("cent.lon" %in% names(result))
  expect_true("cent.lat" %in% names(result))
  
  # Check output dimensions
  expect_equal(dim(result), c(4537, 3))
})