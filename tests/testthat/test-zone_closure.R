# -------------------------------------------------------------------------------------------------
# File: test-zone_closure.R
# Purpose: To provide unit tests for the zone_closure() wrapper and its Shiny server module.
# Description: This script uses the 'testthat' framework to validate the behavior of the
#              zone_closure() function and the reactive logic inside zone_closure_server().
#
# Scenarios tested:
#   - App Construction: Verifies the wrapper successfully builds a Shiny app object.
#   - Map Click Logic: Simulates user clicks to ensure only modeled zones can be selected, 
#                      while non-modeled zones correctly trigger an error and are ignored.
#   - Reactive Tables: Validates that selecting a valid zone properly updates the TAC table.
#
# Notes: This test uses 'testthat::local_mocked_bindings()' to safely mock internal data-pulling
#        and directory paths (preventing real database queries or file modifications). It also
#        utilizes 'shiny::testServer()' to test the module's reactive environment in isolation
#        without needing to launch a web browser.
# -------------------------------------------------------------------------------------------------

library(shiny)
library(sf)
library(dplyr)

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

# Tets shiny app object ---------------------------------------------------------------------------
test_that("zone_closure() successfully constructs a Shiny app object", {
  
  # Create a dummy spatial dataset
  dummy_spat <- data.frame(
    zone_id = c("ZoneA", "ZoneB"),
    lon = c(-122.0, -121.0),
    lat = c(37.0, 38.0)) %>%
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
  
  # Mock the FishSET helper functions to prevent real database queries during the test
  local_mocked_bindings(
    data_pull = function(spatname, project) {
      list(dataset = dummy_spat)
    },
    check_spatdat = function(spatdat, id, lon, lat) {
      return(spatdat)
    }
  )
  
  # Run the wrapper function
  app <- zone_closure(
    project = "TestProject",
    spatname = "dummy_spat_file",
    zone_spat = "zone_id"
  )
  
  # Assertions: Ensure the function returns a valid shiny app object
  expect_s3_class(app, "shiny.appobj")
})


# Test map clicks ---------------------------------------------------------------------------------
test_that("zone_closure_server handles map clicks correctly", {
  
  # Create dummy inputs for the server module
  dummy_spat <- data.frame(
    zone_id = c("ZoneA", "ZoneB"),
    lon = c(-122.0, -121.0),
    lat = c(37.0, 38.0)) %>%
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
  
  rv_data <- reactiveValues(spat = dummy_spat)
  rv_project_name <- reactiveVal("TestProject")
  
  # Setup a temporary directory to trick the modeled_zones() reactive
  tmp_dir <- tempdir()
  rv_folderpath <- reactiveVal(tmp_dir)
  
  # Build the folder structure the reactive expects: TestProject/Models/ModelDesigns
  design_dir <- file.path(tmp_dir, "TestProject", "Models", "ModelDesigns")
  dir.create(design_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Save a fake design file that specifies "ZoneA" is modeled
  fake_design <- list(ids = list(zone = "ZoneA"))
  saveRDS(fake_design, file.path(design_dir, "dummy_design.rds"))
  
  # Mock locproject() to point to our temporary directory
  local_mocked_bindings(
    locproject = function() { return(tmp_dir) }
  )
  
  # Use shiny::testServer to test the module logic in isolation
  testServer(zone_closure_server, args = list(
    rv_folderpath = rv_folderpath,
    rv_project_name = rv_project_name,
    rv_data = rv_data,
    spat_zone_id = "zone_id"), {
      
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
      
      # Verify ZoneB was NOT added because of our safety check
      expect_false("Zone_ZoneB" %in% rv_clicked_zones$ids)
    })
})
