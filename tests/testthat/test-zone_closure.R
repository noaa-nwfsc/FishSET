# -------------------------------------------------------------------------------------------------
# File: test-zone_closure.R
# Purpose: To provide unit tests for the zone_closure() wrapper and its Shiny server module.
# Description: This script uses the 'testthat' framework to validate the behavior of the
#              zone_closure() function and the reactive logic inside zone_closure_server().
#
# Scenarios tested:
#   - App Construction: Verifies the wrapper successfully builds a Shiny app object.
#   - Map Click Logic: Simulates user clicks to ensure only valid zones (based on the 
#                      selected alt_matrix) can be selected, while invalid zones trigger 
#                      an error and are ignored.
#   - Reactive Tables: Validates that selecting a valid zone properly updates the TAC table.
#
# Notes: This test uses 'testthat::local_mocked_bindings()' to safely mock internal data-pulling,
#        database checks, and directory paths (preventing real DB queries or file modifications). 
#        It also utilizes 'shiny::testServer()' to test the module's reactive environment in 
#        isolation without needing to launch a web browser.
# -------------------------------------------------------------------------------------------------

library(shiny)
library(sf)
library(dplyr)
library(testthat)

# Test zone closure set up ------------------------------------------------------------------------
module_path <- system.file("ShinyFiles", 
                           "MainApp", 
                           "modules", 
                           "zone_closure_module.R", 
                           package = "FishSET")

# Fallback path just in case you are running tests interactively instead of via devtools
if (module_path == "") {
  module_path <- file.path("..", 
                           "..", 
                           "inst", 
                           "ShinyFiles", 
                           "MainApp", 
                           "modules", 
                           "zone_closure_module.R")
}

if (file.exists(module_path)) {
  source(module_path, local = FALSE)
} else {
  stop("Testing failed: Could not locate zone_closure_module.R")
}

# Test shiny app object ---------------------------------------------------------------------------
test_that("zone_closure() successfully constructs a Shiny app object", {
  
  # Create a dummy spatial dataset
  dummy_spat <- data.frame(
    zone_id = c("ZoneA", "ZoneB"),
    lon = c(-122.0, -121.0),
    lat = c(37.0, 38.0)) %>%
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
  
  # Create a temporary empty file to represent the database so file.exists() returns TRUE
  tmp_db <- tempfile(fileext = ".sqlite")
  file.create(tmp_db)
  
  # Mock the FishSET helper functions to bypass real spatial/database queries
  local_mocked_bindings(
    data_pull = function(spatname, project) {
      list(dataset = dummy_spat)
    },
    check_spatdat = function(spatdat, id, lon, lat) {
      return(spatdat)
    },
    locdatabase = function(project) { 
      return(tmp_db) 
    },
    table_exists = function(table, project) { 
      return(TRUE) 
    },
    unserialize_table = function(table, project) { 
      list(TestMatrix = list(greaterNZ = c("ZoneA"))) 
    }
  )
  
  # Run the wrapper function with the new alt_matrix parameter
  app <- zone_closure(
    project = "TestProject",
    spatname = "dummy_spat_file",
    zone_spat = "zone_id",
    alt_matrix = "TestMatrix"
  )
  
  # Assertions: Ensure the function returns a valid shiny app object
  expect_s3_class(app, "shiny.appobj")
  
  # Clean up temp file
  unlink(tmp_db)
})


# Test map clicks ---------------------------------------------------------------------------------
test_that("zone_closure_server handles map clicks correctly based on alt_matrix", {
  
  # Create dummy inputs for the server module
  dummy_spat <- data.frame(
    zone_id = c("ZoneA", "ZoneB"),
    lon = c(-122.0, -121.0),
    lat = c(37.0, 38.0)) %>%
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
  
  rv_data <- reactiveValues(spat = dummy_spat)
  rv_project_name <- reactiveVal("TestProject")
  rv_folderpath <- reactiveVal(tempdir())
  
  # Create a temporary file to act as our mocked SQLite database
  tmp_db <- tempfile(fileext = ".sqlite")
  file.create(tmp_db)
  
  # Mock the database functions so the app thinks "TestMatrix" exists and contains "ZoneA"
  local_mocked_bindings(
    locdatabase = function(project) { return(tmp_db) },
    table_exists = function(table, project) { return(TRUE) },
    unserialize_table = function(table, project) { 
      list(TestMatrix = list(greaterNZ = c("ZoneA"))) 
    }
  )
  
  # Use shiny::testServer to test the module logic in isolation
  testServer(zone_closure_server, args = list(
    rv_folderpath = rv_folderpath,
    rv_project_name = rv_project_name,
    rv_data = rv_data,
    spat_zone_id = "zone_id",
    alt_matrix = "TestMatrix"), {
      
      # Simulate the dropdown updating (which bypasses the 'init' hold in the server)
      session$setInputs(alt_matrix_ui = "TestMatrix")
      
      # Check initial state
      expect_equal(length(rv_clicked_zones$ids), 0)
      
      # Simulate clicking on a modeled zone (ZoneA)
      session$setInputs(zone_map_output_shape_click = list(id = "Zone_ZoneA"))
      
      # Verify the zone was added to rv_clicked_zones
      expect_true("Zone_ZoneA" %in% rv_clicked_zones$ids)
      
      # Verify the TAC table updated automatically
      expect_equal(nrow(rv_tac_table$data), 1)
      expect_equal(rv_tac_table$data$Zones[1], "Zone_ZoneA")
      
      # Simulate clicking on a non-modeled zone (ZoneB)
      session$setInputs(zone_map_output_shape_click = list(id = "Zone_ZoneB"))
      
      # Verify ZoneB was NOT added because of our safety check (it is not in TestMatrix)
      expect_false("Zone_ZoneB" %in% rv_clicked_zones$ids)
    })
  
  # Clean up temp file
  unlink(tmp_db)
})