#' Define zone closure scenarios
#' 
#' @param project Required, name of project.
#' @param gridfile Required, data file or character. 
#'   \code{gridfile} is a spatial data file containing information on fishery management or regulatory zones boundaries.
#'   Shape, json, geojson, and csv formats are supported. geojson is the preferred format. json files must be converted
#'   into geoson. This is done automatically when the file is loaded with \code{\link{read_dat}} with \code{is.map} set to true.
#'   \code{gridfile} cannot, at this time, be loaded from the FishSET database. \cr
#' @param cat Variable in \code{gridfile} that identifies the individual areas or zones.
#' @param lon.grid Required for csv files. Variable or list from \code{gridfile} containing longitude data. Leave as NULL if \code{gridfile} is a shape or json file.
#' @param lat.grid Required for csv files. Variable or list from \code{gridfile} containing latitude data.  Leave as NULL if \code{gridfile} is a shape or json file.
#' @importFrom sf st_as_sf st_shift_longitude
#' @importFrom yaml write_yaml
#' @import leaflet
#' @import shiny
#' @import dplyr
#' @import sf
#' @details Define zone closure scenarios. Function opens an interactive map. Define zone closures by clicking on one or more zones and clicking the 'Close zones'
#'   button. To define another closure scenario, unclick zones and then click the desired zones. Press the 'Save closures' button to save choices.
#'   The saved choices are called in the policy scenario function.
#' @export
#' @return Returns a yaml file to the project output folder.



zone_closure <- function(project, gridfile, cat, lon.grid=NULL, lat.grid=NULL) {

  x <- 0
  
  if (any(grepl("Spatial", class(gridfile)))) {
    if(any(class(gridfile) %in% c("sp", "SpatialPolygonsDataFrame"))) {
      gridfile <- sf::st_as_sf(gridfile)
    } else {
      if (is_empty(lon.grid) | is_empty(lat.grid)) {
        warning("lat.grid and lon.grid must be supplied to convert sp object to a sf object.")
        x <- 1
      } else {
        gridfile <- sf::st_as_sf(
          x = gridfile,
          zone = cat,
          coords = c(lon.grid, lat.grid),
          crs = "+proj=longlat +datum=WGS84"
        )
      }
    }
  }
  
  gridfile <- sf::st_shift_longitude(gridfile)
  
  if(cat %in% names(gridfile)){
    
    gridfile$secondLocationID <- paste("Zone_", as.character(gridfile[[cat]]), sep="")
    names(gridfile)[which(names(gridfile) == cat)] <- 'zone'
    gridfile$zone <- as.character(gridfile$zone)
  } else if('zone' %in% names(gridfile)){
    gridfile$zone <- as.character(gridfile$zone)
    gridfile$secondLocationID <- paste("Zone_", as.character(gridfile$zone), sep="")
  } else {
    cat('Zone identifier not found.')
    x<-1
  }
  
  
if(x == 0){
  shinyApp(
    ui = fluidPage(
    
    leafletOutput("map"),
    "Click on one or more zones to select closed zones.",
    "\nPress the 'Add closure' button to record choices.",
    "Repeat to add another closure.",
    "When done, press the 'Save closures' button.",
    selectizeInput(inputId = "clicked_locations",
                   label = "Clicked",
                   choices = gridfile$secondLocationID,
                   selected = NULL,
                   multiple = TRUE),
    actionButton('addClose', 'Add closure'),
    verbatimTextOutput("closure"),
    actionButton('saveClose', 'Save Closure')
  ),
  
  server <- function(input, output, session){
    
    #create empty vector to hold all click ids
    clicked_ids <- reactiveValues(ids = vector())
    closures <-  reactiveValues()
    
   #initial map output
    output$map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        addPolygons(data = gridfile,
                    fillColor = "white",
                    fillOpacity = 0.5,
                    color = "black",
                    stroke = TRUE,
                    weight = 1,
                    layerId = ~secondLocationID,
                    group = "regions",
                    label = ~secondLocationID)
    }) #END RENDER LEAFLET
    
    observeEvent(input$map_shape_click, {
      
      #create object for clicked polygon
      click <- input$map_shape_click
      
      #define leaflet proxy for second regional level map
      proxy <- leafletProxy("map")
      
      #append all click ids in empty vector
      clicked_ids$ids <- c(clicked_ids$ids, click$id) # name when clicked, id when unclicked
      
      #shapefile with all clicked polygons - original shapefile subsetted by all admin names from the click list
      clicked_polys <- gridfile %>%
        filter(secondLocationID %in% clicked_ids$ids)
      
      #if the current click ID [from GID_1] exists in the clicked polygon (if it has been clicked twice)
      if(click$id %in% clicked_polys$zone){
        
        #define vector that subsets NAME that matches GID_1 click ID - needs to be different to above
        name_match <- clicked_polys$secondLocationID[clicked_polys$zone == click$id]
        
        #remove the current click$id AND its name match from the clicked_polys shapefile
        clicked_ids$ids <- clicked_ids$ids[!clicked_ids$ids %in% click$id]
        clicked_ids$ids <- clicked_ids$ids[!clicked_ids$ids %in% name_match]
        
        
        # update
        updateSelectizeInput(session,
                             inputId = "clicked_locations",
                             label = "",
                             choices = gridfile$secondLocationID,
                             selected = clicked_ids$ids)
        
        #remove that highlighted polygon from the map
        proxy %>% removeShape(layerId = click$id)
        
      } else {
        
        #map highlighted polygons
        proxy %>% addPolygons(data = clicked_polys,
                              fillColor = "red",
                              fillOpacity = 0.5,
                              weight = 1,
                              color = "black",
                              stroke = TRUE,
                              layerId = clicked_polys$zone)
        
        # update
        updateSelectizeInput(session,
                             inputId = "clicked_locations",
                             label = "",
                             choices = gridfile$secondLocationID,
                             selected = clicked_ids$ids)
        
      } #END CONDITIONAL
    }) #END OBSERVE EVENT
    
    
   observe({
      if(input$addClose > 0){
        closures$dList <- c(isolate(closures$dList), isolate(list(clicked_ids$ids)))
      }
    })
    
    
    
   output$closure <- renderPrint({
     print(closures$dList)                # rv$value
    })
    
    observeEvent(input$saveClose, {
      
      yaml::write_yaml(closures$dList, paste0(locoutput(project),"/", project, "_closures.yaml"))
      
    }) #END SAVE CLOSURE
    
  }) #END SHINYAPP

  }
}

