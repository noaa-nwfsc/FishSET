#' Define zone closure scenarios
#' 
#' @param project Required, name of project.
#' @param spatdat Required, data file or character. 
#'   \code{spatdat} is a spatial data file containing information on fishery 
#'   management or regulatory zones boundaries. Shape, json, geojson, and csv 
#'   formats are supported. geojson is the preferred format. json files must be 
#'   converted into geoson. This is done automatically when the file is loaded 
#'   with \code{\link{read_dat}} with \code{is.map} set to true. \code{spatdat} 
#'   cannot, at this time, be loaded from the FishSET database. \cr
#' @param cat Variable in \code{spatdat} that identifies the individual areas or zones.
#' @param lon.spat Required for csv files. Variable or list from \code{spatdat} 
#'   containing longitude data. Leave as NULL if \code{spatdat} is a shape or json file.
#' @param lat.spat Required for csv files. Variable or list from \code{spatdat} 
#'   containing latitude data.  Leave as NULL if \code{spatdat} is a shape or json file.
#' @param epsg EPSG number. Set the epsg to ensure that \code{spatdat} have the correct projections. 
#' If epsg is not specified but is defined for \code{spatdat}. 
#'   See \url{http://spatialreference.org/} to help identify optimal epsg number.
#' @importFrom sf st_crs st_transform
#' @importFrom yaml write_yaml
#' @importFrom grDevices topo.colors
#' @importFrom shiny dataTableOutput renderDataTable
#' @import leaflet
#' @import bslib
#' @import bsicons
#' @details Define zone closure scenarios. Function opens an interactive map. 
#'   Define zone closures by clicking on one or more zones and clicking the 
#'   'Close zones' button. To define another closure scenario, unclick zones and 
#'   then click the desired zones. Press the 'Save closures' button to save choices.
#'   The saved choices are called in the policy scenario function.
#' @export
#' @return Returns a yaml file to the project output folder.

zone_closure <- function(project, spatdat, cat, lon.spat = NULL, lat.spat = NULL, epsg = NULL) {
  
  # Set these values to NULL to appease RCMD checks
  zone_closure_sidebarUI <- zone_closure_mapUI <- zone_closure_tableUI <- NULL
  zone_closure_mapServer <- zone_closure_tblServer <- NULL
  zone <- display <- NULL
  
  # Source module ui and server files for selecting closure areas
  zone_closure_dir <- system.file("ShinyFiles", "MainApp", package = "FishSET")
  if (zone_closure_dir == "") {
    stop("Could not find example directory. Try re-installing `FishSET`.", call. = FALSE)
  }
  
  source(file.path(zone_closure_dir, "zone_closure_UI.R"), local = TRUE)
  source(file.path(zone_closure_dir, "zone_closure_Server.R"), local = TRUE)
  
  # Initialize variables
  x <- 0
  secondLocationID <- NULL
  
  # leaflet requires WGS84
  spatdat <- sf::st_transform(spatdat, "+proj=longlat +datum=WGS84")
  
  # Run checks on spatial data
  spatdat <- check_spatdat(spatdat, id = cat, lon = lon.spat, lat = lat.spat)
  
  # Zone closure ui -------------------------------------------------------------------------------
  ui <- fluidPage(
    shinyjs::useShinyjs(),
    bslib::page_sidebar(
      sidebar = bslib::sidebar( 
        "Click on one or more zones to select closed zones.",
        "\nPress the 'Add closure' button to record choices.",
        "Repeat to add another closure.",
        "When done, press the 'Save closures' button.",
        zone_closure_sidebarUI("policy")
      ),
      
      bslib::page_fluid(
        zone_closure_mapUI("policy"),
        zone_closure_tableUI("policy")
      )
    )
  )
  
  server <- function(input, output, session){
    session$onSessionEnded(function() {
      stopApp()
    })
    
    # Initialize reactive values
    V <- reactiveValues(data = NULL)
    clicked_ids <- reactiveValues(ids = vector())
    closures <- reactiveValues()
    rv <- reactiveValues(edit = NULL)
    all_variables <- reactive({list(sz_id = cat)}) # identify zone ID in spatial data
    
    # zone_closure_sideServer("policy", project, spatdat)
    zone_closure_mapServer("policy", 
                           project, 
                           spatdat, 
                           clicked_ids, 
                           V, 
                           closures, 
                           rv, 
                           all_variables)
    
    zone_closure_tblServer("policy", 
                           project, 
                           spatdat, 
                           clicked_ids, 
                           V)
  }
  
  # Run the shiny app
  shinyApp(ui = ui, server = server)
}

