edit_map_manual <- function(spat, id, project, coord = NULL) {
  #' Manually add polygon/area to spatial data
  #' 
  #' Allows users to add polygons (e.g. a closure area/zone) to spatial data
  #' by passing a vector or matrix containing coordinates. 
  #' 
  #' @param spat Spatial data to edit. 
  #' @param id Spatial ID column.The name must be unique. 
  #' @param coord Optional, a numeric vector or matrix with two columns containing the 
  #'   coordinates of a polygon. Users can build polygons point-by-point using the
  #'   longitude and latitude boxes in the app. 
  #' @export
  #' @import shiny
  #' @import leaflet
  #' @importFrom sf st_transform
  #' @seealso \code{\link{edit_map_gui}} for interactive editing of spatial tables,
  #'  \code{\link{list_tables}} and \code{\link{table_view}} for locating spatial
  #'  tables. 
  #' @details There are two ways to add polygons when using this function: by
  #' setting the \code{coord} argument or by building a polygon point-by-point. To
  #' build a polygon manually, enter each set of coordinates into the longitude and
  #' latitude boxes and click "Add new point". A table of coordinates will appear
  #' next to the lon/lat boxes. The polygon table will auto-populate if \code{coord} 
  #' contains a vector or matrix. The coordinate table can be edited by double-clicking 
  #' a cell. When finished, enter a unique ID into the "ID for
  #' new polygon" box and click "Add new polygon". Users can view their polygon
  #' in the map viewer at the top of the app. To add a new polygon, select "Clear table"
  #' and repeat. Click "Save" to save the edited version of the spatial table. 
  #' 
  #' The edited spatial table will be saved to the projects data/spat/ folder using 
  #' the naming convention "projectNameSpatTable_modn" ("modn" refers to modified version
  #' of the spatial table, e.g. "mod2"). Use 
  #' \code{list_tables("project", type = "spat")} to show spatial 
  #' tables by project and \code{table_view("table", "project")} to view or assign
  #' to global environment. To edit a map interactively, see \code{\link{edit_map_gui}}.
  #' @examples 
  #' \dontrun{
  #' new_area <- matrix(c(-174.6, 55.6, -172.7, 55.6, 
  #'                      -173.1, 54.4, -175.3, 54.8), 
  #'                      ncol = 2, byrow = TRUE)
  #' edit_map_manual(pollockNMFSSpatTable, id = "NMFS_AREA", project = "pollock",
  #'                 coord = new_area)
  #' }
  
  map_nm <- deparse(substitute(spat))
  
  map_list <- data_pull(spat, project)
  map_dat <- map_list$dataset
  
  # map_nm <- parse_data_name(map, "spat", project)
  
  map_dat <- sf::st_transform(map_dat, crs = 4326)
  
  leaf_map <-
    leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addPolygons(data = map_dat,
                         fillColor = "white",
                         fillOpacity = 0.5,
                         color = "black",
                         stroke = TRUE,
                         weight = 1,
                         group = "editable",
                         layerId = map_dat[[id]],
                         label = map_dat[[id]])
  
  shinyApp(
    ui = 
      fluidPage(
        
        h3("Create new spatial area manually"),
        
        leafletOutput("finalMap"),
       
        actionButton("save", "Save", style = "background: blue; color: white;"), 
        actionButton("reset", "Reset map", style = "background: blue; color: white;"),
 
        fluidRow(
          
          column(2,
                 numericInput("add_lon", "Longitude", value = 0),
                 numericInput("add_lat", "Latitude", value = 0)),
          
          column(3, offset = 1, 
                 uiOutput("clearCoordBttn"), 
                 DT::DTOutput("coord_m"))
        ),
        
        actionButton("new_point", "Add new point", 
                     style = "background: blue; color: white;"),
        
        h4("Name closure"),
        
        uiOutput("polyID"),
        
        # choose spatial ID column
        uiOutput("spatCols"),
        
        fluidRow(
          
          column(3,  
                 checkboxInput("poly_combine", "Combine if polygon overlaps?", value = TRUE)),
          
          column(6, offset = 0, 
                 p("Checking \"combine if polygon overlaps?\" will create separate polygons where
                 the new polygon and spatial table intersect. In this case, the names of the
                 the separate polygons will be derived from the spatial table ID; user inputed
                   ID will be ignored."))
        ),
       
        
        actionButton("run_add_poly", "Add new polygon",
                     style = "background: blue; color: white;"),
        
        textInput("exprUp", label = "Enter an R expression",
                  value = ""),
        actionButton("runUp", "Run", class = "btn-success"),
        div(style = "margin-top: 2em;",
            uiOutput('resultUp')
        ) 
      ),
    
    server = function(input, output, session) {
      
      # values ----
      
      rv <- reactiveValues(map_out = NULL, coord = coord)
      
      spat_dat <- reactive({
        
        if (is.null(rv$map_out)) map_dat
        else rv$map_out
      })
      
      leaf_map <- reactive({

        leaflet::leaflet() %>%
          leaflet::addTiles() %>%
          leaflet::addPolygons(data = spat_dat(),
                               fillColor = "white",
                               fillOpacity = 0.5,
                               color = "black",
                               stroke = TRUE,
                               weight = 1,
                               group = "editable",
                               layerId = spat_dat()[[id]],
                               label = spat_dat()[[id]])
      })
      
      output$finalMap <- leaflet::renderLeaflet(leaf_map())
      

      # add polygon ----
      
      observeEvent(input$new_point, {
        
        new_point <- matrix(c(input$add_lon, input$add_lat), ncol = 2, byrow = TRUE,
                            dimnames = list(NULL, c("Longitude", "Latitude")))
        rv$coord <- rbind(rv$coord, new_point)
      })
      
      coord_proxy <- DT::dataTableProxy("coord_m")
      
      output$coord_m <- DT::renderDT({
        
        rv$coord
        
      }, server = TRUE, editable = list(target ='cell'), 
      options = list(autoWidth = FALSE), rownames = FALSE)
      
      observeEvent(input$coord_m_cell_edit, {
        
        rv$coord <- DT::editData(rv$coord, 
                                 input$coord_m_cell_edit,
                                 "coord_m", rownames = FALSE)
      })
      
      
      output$polyID <- renderUI({
        
        if (is.character(spat[[id]])) {
          
          textInput("poly_id", "ID for new polygon")
          
        } else {
          
          numericInput("poly_id", "ID for new polygon", value = 0)
        }
      })
      
      
      output$spatCols <- renderUI({
        
        selectInput("spat_id", "Select spatial ID column", 
                    choices = colnames(spat))
      })

      # combine new closure with spat
      observeEvent(input$run_add_poly, {
        # check that new close was made
        if (!isTruthy(rv$coord)) {
          
          showNotification("Polygon coordinates missing.", type = "warning")
        }
        
        req(rv$coord)
        
        q_test <- quietly_test(make_spat_area)

        rv$map_out <- q_test(spat_dat(), coord = rv$coord, spat.id = input$spat_id,
                             new.id = input$poly_id, combine = input$poly_combine)
      })
      
      # clear coord table----
      
      output$clearCoordBttn <- renderUI({
        
        if (!is.null(rv$coord)) {
          
          tagList(
              actionButton("clear_coord", "Clear table", 
                           style = "background: orange; color = white;"),
              p(strong("Double-click table to edit.")))
        }
      })
      
      observeEvent(input$clear_coord, rv$coord <- NULL)
      
      # Save ----
      observeEvent(input$save, {
      
        # log new map
        grid_log <- get_grid_log(project)
        grid_info <- list()
        
        grid_info[[map_nm]] <- list(grid_name = map_nm,
                                    mod_type = "edit",
                                    combined_name = NULL,
                                    combined_areas = NULL)
        
        grid_list <- list()
        
        grid_list[[map_nm]] <- rv$map_out
        
        map_lab <- grid_lab_helper(project, 
                                   grid_info = grid_info, 
                                   grid_log = grid_log, 
                                   mod_type = "edit")
        
        save_grid_cache(project, grid_list, grid_info, mod_type = "edit")
        
        log_grid_info(project, grid_info, mod_type = "edit")
        
        showNotification("Modified map saved to project data folder", 
                         type = "message")
      })
      
      # map reset
      observeEvent(input$reset, rv$map_out <- map_dat)
      
      
      
      # Temp
      
      r <- reactiveValues(done = 0, ok = TRUE, output = "")
      
      observeEvent(input$runUp, {
        shinyjs::hide("error")
        r$ok <- FALSE
        tryCatch(
          {
            r$output <- isolate(
              paste(utils::capture.output(eval(parse(text = input$exprUp))), collapse = '\n')
            )
            r$ok <- TRUE
          },
          error = function(err) {r$output <- err$message}
        )
        r$done <- r$done + 1
      })
      output$resultUp <- renderUI({
        if(r$done > 0 ) { 
          content <- paste(paste(">", isolate(input$expr)), r$output, sep = '\n')
          if(r$ok) {
            pre(content)
          } else {
            pre( style = "color: red; font-weight: bold;", content)
          }
        }
      })
      
      
    }
  )
}
