source("zone_outsample_UI.R", local = TRUE)

zone_outsample_mapServer <- function(id, clicked_ids, spat, mod.spat, zone.dat){
  moduleServer(
    id,
    function(input, output, session){
      # Create reactive values to appease RMD check - seems clunky but this prevents an evergrowing list of notes in RMD check
      # spat <- reactiveValues(data = spat)
      # mod.spat <- reactiveValues(data = mod.spat)
      # zone.dat <- reactiveValues(data = zone.dat)
      
      # Generate map
      output$map <- leaflet::renderLeaflet({
        leaflet::leaflet() %>%
          leaflet::addTiles() %>%
          leaflet::addPolygons(data = spat,
                      fillColor = "white",
                      fillOpacity = 0.5,
                      color = "black",
                      stroke = TRUE,
                      weight = 1,
                      layerId = ~secondLocationID,
                      group = "regions",
                      label = ~secondLocationID) %>%
          leaflet::addPolygons(data = mod.spat,
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
        
        temp_dat <- spat#spat$data
        z_id <- zone.dat#zone.dat$data
        sec_id <- "secondLocationID"
        
        # define leaflet proxy for second regional level map
        proxy <- leaflet::leafletProxy("map")
        
        # Check if the zone was already clicked. If not, then add polygon, otherwise remove polygon (ie zone clicked twice)
        if(!(gsub("Zone_", "", click$id) %in% clicked_ids$ids)){
          # Remove the preceding zone text to store only the zone ID to plot on the z_id layer of the leaflet map
          clicked_ids$ids <- c(clicked_ids$ids, gsub("Zone_", "", click$id))
          
          # shapefile with all clicked polygons - original shapefile subsetted by all admin names from the click list
          clicked_polys <- temp_dat %>% filter(.data[[z_id]] %in% clicked_ids$ids)
          
          # map highlighted polygons
          proxy %>% 
            leaflet::addPolygons(data = clicked_polys,
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

zone_outsample_tableServer <- function(id, clicked_ids, outsample_table){
  moduleServer(
    id,
    function(input, output, session){
      observeEvent(clicked_ids$ids, {
        outsample_table$data <- data.frame(Zones = as.numeric(clicked_ids$ids))
        
        output$table <- shiny::renderDataTable(outsample_table$data, options = list(pageLength = 10))
      })
    }
  )
}

zone_outsample_saveServer <- function(id, outsample_table, filename, zone.dat, project, dat){
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
        
        showNotification("Filtered out-of-sample data saved. Zone selection window can be closed now.", type = "message", duration = 20)
      })
    }
  )
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