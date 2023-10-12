# Shiny app modules that display an interactive map for the user to select out-of-sample zones for predicting 
# probabilities of fishing

zone_outsample_mapUI <- function(id){
  ns <- NS(id)
  
  leafletOutput(ns("map"))
}

zone_outsample_mapServer <- function(id, clicked_ids){
  moduleServer(
    id,
    function(input, output, session){
      # Create reactive values to appease RMD check - seems clunky but this prevents an evergrowing list of notes in RMD check
      spat <- reactiveValues(data = spat)
      mod.spat <- reactiveValues(data = mod.spat)
      zone.dat <- reactiveValues(data = zone.dat)
      
      # Generate map
      output$map <- renderLeaflet({
            
        leaflet() %>%
          addTiles() %>%
          addPolygons(data = spat$data,
                      fillColor = "white",
                      fillOpacity = 0.5,
                      color = "black",
                      stroke = TRUE,
                      weight = 1,
                      layerId = ~secondLocationID,
                      group = "regions",
                      label = ~secondLocationID) %>%
          addPolygons(data = mod.spat$data,
                      fillColor = "#FFC107",
                      fillOpacity = 0.5,
                      color = "#FFC107",
                      stroke = TRUE,
                      weight = 1,
                      layerId = ~secondLocationID,
                      group = "regions",
                      label = ~secondLocationID)
      })
      
      # Allow user to select grids
      observeEvent(input$map_shape_click, {
        
        # create object for clicked polygon
        click <- input$map_shape_click
        
        req(click$id)
        
        temp_dat <- spat$data
        z_id <- zone.dat$data
        sec_id <- "secondLocationID"
        
        # define leaflet proxy for second regional level map
        proxy <- leafletProxy("map")
        
        # Check if the zone was already clicked. If not, then add polygon, otherwise remove polygon (ie zone clicked twice)
        if(!(gsub("Zone_", "", click$id) %in% clicked_ids$ids)){
          # Remove the preceding zone text to store only the zone ID to plot on the z_id layer of the leaflet map
          clicked_ids$ids <- c(clicked_ids$ids, gsub("Zone_", "", click$id))
          
          # shapefile with all clicked polygons - original shapefile subsetted by all admin names from the click list
          clicked_polys <- temp_dat %>% filter(.data[[z_id]] %in% clicked_ids$ids)
          
          # map highlighted polygons
          proxy %>% addPolygons(data = clicked_polys,
                                fillColor = "red",
                                fillOpacity = 0.5,
                                weight = 1,
                                color = "black",
                                stroke = TRUE,
                                layerId = clicked_polys[[z_id]],
                                label = ~secondLocationID)
          
        } else {
          # If the id was already clicked, then remove from the list and remove the red polygon
          clicked_ids$ids <- clicked_ids$ids[which(!(clicked_ids$ids %in% click$id))]
          
          proxy %>% removeShape(layerId = as.numeric(click$id))
        }
      })
    }
  )
}

zone_outsample_tableUI <- function(id){
  ns <- NS(id)
  
  dataTableOutput(ns("table"))
}

zone_outsample_tableServer <- function(id, clicked_ids, outsample_table){
  moduleServer(
    id,
    function(input, output, session){
      observeEvent(clicked_ids$ids, {
        outsample_table$data <- data.frame(Zones = as.numeric(clicked_ids$ids))
        
        output$table <- renderDataTable(outsample_table$data, options = list(pageLength = 10))
      })
    }
  )
}

zone_outsample_saveUI <- function(id){
  ns <- NS(id)
  
  actionButton(ns('saveZones'), 'Save out-of-sample zones',
               style = "color: white; background-color: blue;")
}

zone_outsample_saveServer <- function(id, outsample_table, filename){
  moduleServer(
    id,
    function(input, output, session){
      
      # create reactive values to appease RMD check
      zone.dat <- reactiveValues(data = zone.dat)
      project <- reactiveValues(data = project)
      
      observeEvent(input$saveZones, {
        # Filter data and save new table as an rds file
        tmp <- tempfile()
        on.exit(unlink(tmp), add = TRUE)
        
        dat <- dat[which(unlist(dat[zone.dat$data], use.names = FALSE) %in% unlist(outsample_table$data, use.names = FALSE)),]
        
        filename$name <- paste0(locoutput(project$data), project$data, "filtered_outsample.rds")
        
        saveRDS(dat, filename$name)
        
        message("Filtered out-of-sample data saved.")
      })
    }
  )
}

zone_outsample_closeUI <- function(id){
  ns <- NS(id)
  
  actionButton(ns('close'), 'Close app',
               style="color: #fff; background-color: #FF6347; border-color: #800000;")
}

zone_outsample_closeServer <- function(id, outsample_table, filename){
  moduleServer(
    id,
    function(input, output, session){
      observeEvent(input$close, {
        # Log function when called in process outsample
        stopApp()
      })
    }
  )
}
