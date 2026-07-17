#' Define zone closure scenarios
#' 
#' @param project Required, name of project.
#' @param spatname Required, data file or character. 
#'   \code{spat} is a spatial data file containing information on fishery 
#'   management or regulatory zone boundaries. Shape, json, geojson, and csv 
#'   formats are supported. geojson is the preferred format. json files must be 
#'   converted into geojson. This is done automatically when the file is loaded 
#'   with \code{\link{read_dat}} with \code{is.map} set to true. \code{spat} 
#'   cannot, at this time, be loaded from the FishSET database. \cr
#' @param zone_spat Variable in \code{spat} that identifies the individual areas or zones.
#' @param lon_spat Required for csv files. Variable or list from \code{spat} 
#'   containing longitude data. Leave as NULL if \code{spat} is a shape or json file.
#' @param lat_spat Required for csv files. Variable or list from \code{spat} 
#'   containing latitude data.  Leave as NULL if \code{spat} is a shape or json file.
#' @param epsg EPSG number. Set the epsg to ensure that \code{spat} has the correct projections. 
#'   If epsg is not specified but is defined for \code{spat}. 
#'   See \url{http://spatialreference.org/} to help identify the optimal epsg number.
#' @importFrom sf st_crs st_transform
#' @importFrom yaml write_yaml
#' @importFrom grDevices topo.colors
#' @importFrom shiny dataTableOutput renderDataTable
#' @import leaflet
#' @import bslib
#' @import bsicons
#' @details Define zone closure scenarios via an interactive app. 
#'   Users can define scenarios by clicking on one or more zones on the map, 
#'   adjusting the allowable TAC percentages in the table, and entering a unique scenario name. 
#'   Clicking 'Add closure' instantly saves the scenario to the project database. 
#'   These saved choices are later called in the policy scenario function.
#' @export
#' @return Returns a yaml file to the project output folder.

zone_closure <- function(project, 
                         spatname, 
                         zone_spat, 
                         lon_spat = NULL,
                         lat_spat = NULL, 
                         epsg = NULL) { 
  
  # Load alt matrix -------------------------------------------------------------------------------
  alt_table <- paste0(project, "AltMatrix")
  
  # Stop function if the alt matrix table doesn't exist
  if (!table_exists(alt_table, project)) {
    stop(paste0("Error: The table of alternative choice matrices does not exist for project ",
                project, 
                ". Run `create_alternative_choice()` to define your valid zones."))
  } else {
    alt_list <- unserialize_table(alt_table, project)
    
    # Ensure the table is not empty
    if (is.null(alt_list)) {
      stop(paste0("Error: The table of alternative choice matrices is empty. Create an ",
                  "alternative choice matrix to continue with zone_closure()."))
    }
  }
  
  # Default to first alt_matrix in the list
  alt_matrix <- alt_list[[1]]
  
  
  # Set zone closure shiny path -------------------------------------------------------------------
  zone_closure_dir <- system.file("ShinyFiles", "MainApp", "modules", "policy", package = "FishSET")
  if (zone_closure_dir == "") {
    stop("Could not find example directory. Try re-installing `FishSET`.", call. = FALSE)
  }
  source(file.path(zone_closure_dir, "zone_closure_module.R"), local = TRUE)
  
  
  # Pull spatial data if needed -------------------------------------------------------------------
  spat_out <- data_pull(spatname, project)
  spatdat <- spat_out$dataset
  
  # Check/build the sf object, then transform to Leaflet CRS
  spat <- check_spatdat(spatdat, id = zone_spat, lon = lon_spat, lat = lat_spat)
  
  # Zone closure ui -------------------------------------------------------------------------------
  ui <- bslib::page_fluid(
    theme = bslib::bs_theme(
      primary = "#41729F", 
      secondary = "#AACDE5", 
      info = "#274472",
      font_scale = 0.9,
      preset = "cerulean"),
    class = "p-4",   
    zone_closure_ui("policy")
  )
  
  # Zone closure server ---------------------------------------------------------------------------
  server <- function(input, output, session){
    session$onSessionEnded(function() {
      stopApp()
    })
    
    rv_project_name <- reactive({ project })
    rv_folderpath   <- reactive({ locproject() }) 
    rv_data         <- reactiveValues(spat = spat)
    
    # Call the module server, passing the parameter string directly
    zone_closure_server(
      id = "policy", 
      rv_folderpath = rv_folderpath, 
      rv_project_name = rv_project_name, 
      rv_data = rv_data,
      spat_zone_id = zone_spat 
    )
  }
  
  # Run the shiny app
  shinyApp(ui = ui, server = server)
}