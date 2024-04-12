# zone closure module server code - sidebar, map, and table

### sidebar zone closure server
zone_closure_sideServer <- function(id, project, spatdat){
  moduleServer(
    id,
    function(input, output, session){
      
      ns <- session$ns
      
      
      output$zone_closure_cat <- renderUI({
        selectInput(ns("select_zone_cat"), "Select zone ID from spatial data",
                    choices = unique(names(spatdat)))
        
      })
      
      
      
    }
  )
}

### map and selected points zone closure server
zone_closure_mapServer <- function(id, project, spatdat, clicked_ids, V, closures, rv){
  moduleServer(
    id,
    function(input, output, session){
      
      ns <- session$ns
      
      mod_zones <- reactiveValues(data = NULL)
      
      
      
      observeEvent(input$select_zone_cat, {

        req(project)


        if(!is.null(model_out_view(project))){
          mod_output <- unserialize_table(paste0(project,"ModelOut"), project)
          mod_zones$data <- list()
          mod_zones$data <- lapply(1:length(mod_output), function(x){rbind(mod_zones$data,unique(mod_output[[x]]$choice.table$choice))})
          mod_zones$data <- unique(unlist(mod_zones$data))


        }else if(length(mod_zones$data) == 0){
          showNotification("WARNING: no zones found in model output", type = "warning")
          mod_zones$data <- NULL
        } else {
          # do nothing
          mod_zones$data <- NULL
        }
        return(mod_zones$data)

      })

      zone_df <- reactive({
        req(input$select_zone_cat)
        req(input$zoneplot)
        
          spatdat %>%
            sf::st_transform(., "+proj=longlat +datum=WGS84") %>%
            mutate(secondLocationID = paste0("Zone_", as.character(spatdat[[input$select_zone_cat]]))) %>%
            mutate(zone = as.character(spatdat[[input$select_zone_cat]])) 
        
      })
      
      
output$zmap <- leaflet::renderLeaflet({
  
  leaflet::leaflet() %>%
   leaflet::addProviderTiles("OpenStreetMap") 
  })
      
  observeEvent(input$zoneplot, {
        req(input$select_zone_cat)
        req(zone_df())

        
      # Generate map
        if(any(!is_empty(mod_zones$data))) {
          
          ## set map size
          coords <- sf::st_coordinates(zone_df()$geometry)
          lng <- mean(coords[,1])
          lat <- mean(coords[,2])
          
          tmp_spat_mod <- zone_df() %>%
            mutate(display = ifelse(zone %in% mod_zones$data, 1, 0))
          
          leaflet::leafletProxy(mapId = "zmap") %>%
            leaflet::addTiles() %>%
            leaflet::setView(lng, lat, zoom = 3) %>% 
            leaflet::addPolygons(data =  tmp_spat_mod,
                                 fillColor = "white",
                                 fillOpacity = 0.5,
                                 color = "black",
                                 stroke = TRUE,
                                 weight = 1,
                                 layerId = ~secondLocationID,
                                 group = "regions",
                                 label = ~secondLocationID) %>% 
            leaflet::addPolygons(data = (tmp_spat_mod %>% filter(display == 1)),
                                 fillColor = "#FFC107",
                                 fillOpacity = 0.5,
                                 color = "#FFC107",
                                 stroke = TRUE,
                                 weight = 1,
                                 layerId = ~secondLocationID,
                                 group = "regions",
                                 label = ~secondLocationID)
          
          #  else plot without model zones
        } else if(any(is_empty(mod_zones$data))){
          
          leaflet::leafletProxy(mapId = "zmap") %>%
            leaflet::addProviderTiles("OpenStreetMap") 
          # leaflet::addPolygons(data = zone_df(),
            #                      fillColor = "white",
            #                      fillOpacity = 0.5,
            #                      color = "black",
            #                      stroke = TRUE,
            #                      weight = 1,
            #                      layerId = ~secondLocationID,
            #                      group = "regions",
            #                      label = ~secondLocationID)
        }
        
        
      })
      
   # })
      
      observeEvent(input$zmap_shape_click, {
        
        # create object for clicked polygon
        click <- input$zmap_shape_click
        
        req(click$id)
        
        
        
        temp_dat <- zone_df()
        z_id <- "zone"
        sec_id <- "secondLocationID"
        #  }
        
        #define leaflet proxy for second regional level map
        proxy <- leaflet::leafletProxy("zmap")
        
        #append all click ids in empty vector
        clicked_ids$ids <- c(clicked_ids$ids, click$id) # name when clicked, id when unclicked
        
        #shapefile with all clicked polygons - original shapefile subsetted by all admin names from the click list
        clicked_polys <- temp_dat %>% filter(.data[[sec_id]] %in% clicked_ids$ids)
        
        #if the current click ID [from GID_1] exists in the clicked polygon (if it has been clicked twice)
        if (click$id %in% clicked_polys[[z_id]]){
          
          #define vector that subsets NAME that matches GID_1 click ID - needs to be different to above
          name_match <- clicked_polys[[sec_id]][clicked_polys[[z_id]] == click$id]
          
          #remove the current click$id AND its name match from the clicked_polys shapefile
          clicked_ids$ids <- clicked_ids$ids[!clicked_ids$ids %in% click$id]
          clicked_ids$ids <- clicked_ids$ids[!clicked_ids$ids %in% name_match]
          
          #remove that highlighted polygon from the map
          proxy %>% leaflet::removeShape(layerId = click$id)
          
        } else {
          
          #map highlighted polygons
          proxy %>% leaflet::addPolygons(data = clicked_polys,
                                         fillColor = "red",
                                         fillOpacity = 0.5,
                                         weight = 1,
                                         color = "black",
                                         stroke = TRUE,
                                         layerId = clicked_polys[[z_id]])
          
        }
      })
      
      # add closure ----
      observeEvent(input$addClose, {
        
        grid_nm <- deparse(substitute(zone_df())) # won't work in main app
        # close_nm <- deparse(substitute(dat$secondspat))
        pass <- TRUE
        
        click <- input$zmap_shape_click
        
        req(click$id)
        
        if (!isTruthy(input$scenarioname)) { # check for unique scenario name
          
          showNotification("Enter a scenario name", type = "message")
        }
        
        close_new <- vapply(closures$dList, function(x) x$scenario, character(1))
        
        if (input$scenarioname %in% c(close_new, close_names(project))) {
          
          showNotification("Enter a unique scenario name", type = "message")
          pass <- FALSE
        }
        
        req(input$scenarioname)
        req(pass)
        
        #  close_nm <- if (input$mode == "normal") NULL else close_nm
        
        closures$dList <- c(closures$dList,
                            list(c(list(scenario = input$scenarioname),
                                   list(date = as.character(Sys.Date())),
                                   list(zone = clicked_ids$ids),
                                   list(tac = V$data$`% allowable TAC`),
                                   list(grid_name = grid_nm)
                                   # list(closure_name = add_close(NULL, input$mode)),
                                   #  list(combined_areas = rv$combined_areas)
                            )))
      })
      
      
      output$closureVTO1 <- renderPrint({
        
        req(closures$dList)
        closures$dList
      })
      
      
      output$closureVTO2 <- renderPrint({
        rv$saved <- get_closure_scenario(project)
        rev(rv$saved)
        })
      
      # save ----
      observeEvent(input$saveClose, {
        
        
        
        
        if (!isTruthy(closures$dList)) {
          
          showNotification("Add a scenario", type = "message")
        }
        
        req(closures$dList)
        
        
        
        
        # update closure list w/ new grid names
        g_info <- get_grid_log(project)
        
        new_grid_nms <-
          vapply(closures$dList, function(cl) {
            
            
            cl$grid_name
            
          },
          character(1))
        
        close_list <-
          purrr::map2(closures$dList, new_grid_nms, function(x, y) {
            
            if (x$grid_name != y) x$grid_name <- y
            x
          })
        
        # save closure scenarios to project output folder
        save_closure_scenario(project, close_list)
        
        # reset closure list
        closures$dList <- NULL
        rv$saved <- get_closure_scenario(project)
      })
      
      # edit closure ----
      
      observeEvent(input$editClose, {

        showModal(
          modalDialog(title = "Edit or delete closure scenario",
                      
                      uiOutput(ns("editCloseUI")),
                      actionButton(ns("editCloseDelete"), "Delete"),
                      footer = tagList(modalButton("Close")),
                      easyClose = FALSE, size = "m"))
        
      })
      
      output$editCloseUI <- renderUI({
        
        rv$edit <- rv$saved
        
        close_vec <- vapply(rv$edit , function(x) x$scenario, character(1))
        
        checkboxGroupInput(ns("editCloseGrp"), "", choices =c(close_vec))
        
      })
      
      observeEvent(input$editCloseDelete, {
        
        req(input$editCloseGrp)
        
        del_ind <- vapply(rv$edit,
                          function(x) x$scenario %in% input$editCloseGrp,
                          logical(1))
        
        rv$edit[del_ind] <- NULL
        
        filename <- paste0(locoutput(project), project, "_closures.yaml")
        
        yaml::write_yaml(rv$edit, filename)
        
        rv$saved <- get_closure_scenario(project)
        
        
      })
    }
  )
}


### table zone closure server
zone_closure_tblServer <-  function(id, project, spatdat, clicked_ids, V){
  moduleServer(
    id,
    function(input, output, session){
      
      # editable table ----
      observeEvent(clicked_ids$ids, {
        V$data = data.frame(Zones = clicked_ids$ids,
                            `% allowable TAC` = rep(0, length(clicked_ids$ids)),
                            check.names = FALSE)
        
        proxy = DT::dataTableProxy("mod_table")
        
        observeEvent(input$mod_table_cell_edit, {
          info <- input$mod_table_cell_edit
          tab_i = info$row
          tab_j = info$col
          tab_k = info$value
          
          isolate(
            if(tab_j %in% match("% allowable TAC", names(V$data))) {
              V$data[tab_i, tab_j] <<- DT::coerceValue(tab_k, V$data[tab_i, tab_j])
              
              if(sum(V$data[,tab_j], na.rm = TRUE) > 100 || sum(V$data[,tab_j], na.rm = TRUE) < 0){
                showNotification("% allowable catch is out of bounds. Sum of values must be between 0 and 100.", type = "error", duration = 6)
                V$data[tab_i, tab_j] <- 0
              }
              
              if(any(V$data[, tab_j] < 0, na.rm = TRUE)){
                showNotification("% allowable catch cannot be negative", type = "error", duration = 6)
                V$data[tab_i, tab_j] <- 0
              }
              
              
            } else {
              stop("Change zone ID using the map.")
            }
          )
          
          replaceData(proxy, V$data, resetPaging = FALSE)
        })
        
        output$mod_table <- DT::renderDataTable({
          DT::datatable(V$data, editable = TRUE)})
        
      })
    }
  )
}
