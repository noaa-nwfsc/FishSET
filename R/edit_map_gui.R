edit_map_gui <- function(spat, id, project) {
  #' Edit spatial table interactively
  #' 
  #' Add, delete, or edit a spatial feature interactively
  #' 
  #' @param spat Spatial data table to edit.
  #' @param id String, name of ID column in \code{spat}.
  #' @param project Name of project. 
  #' @export
  #' @import leaflet
  #' @import shiny
  #' @importFrom shinyjs useShinyjs toggleState refresh
  #' @importFrom dplyr %>% 
  #' 
  
  if (!requireNamespace("mapedit", quietly = TRUE)) {
    stop(
      "Package \"mapedit\" must be installed to use this function.",
      call. = FALSE
    )
  }
  
  map_nm <- deparse(substitute(spat))
  
  map_list <- data_pull(spat, project)
  map_dat <- map_list$dataset
  
  # map_nm <- parse_data_name(map, "spat", project)
  
  # spatial checks?
  
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
        shinyjs::useShinyjs(),
        mapedit::editModUI("mapeditor"),
        actionButton("confirm", "Confirm changes",
                     style = "background: blue; color: white;"),
        actionButton("save", "Save", style = "background: blue; color: white;"), 
        actionButton("reset", "Reset map", style = "background: blue; color: white;"),
        
        uiOutput("showTable"),
        
        textInput("exprUp", label = "Enter an R expression",
                  value = ""),
        actionButton("runUp", "Run", class = "btn-success"),
        div(style = "margin-top: 2em;",
            uiOutput('resultUp')
        ) 
      ),
    
    server = function(input, output, session) {
      
      # values ----
      
      rv <- reactiveValues(map_out = NULL, base_map = map_dat, 
                           save = FALSE, showTable = FALSE,
                           leaf_map = NULL)
      edit_count <- reactiveVal(value = 0)
      drawn_count <- reactiveVal(value = 0)
      
      # editMod doesn't seem to work with a reactive
      # mapedit mod ----
      edits <- callModule(mapedit::editMod,
                          "mapeditor",
                          leafmap = leaf_map,
                          editor = "leaflet.extras",
                          record = TRUE,
                          targetLayerId = "editable")
      
      
      observe(shinyjs::toggleState("save", rv$save))
      
      
      # disable save if new edit is made
      observeEvent(edits(), {
        
        edit_count(edit_count() + 1) # remove
        rv$save <- FALSE
      })
      
      # remove
      observeEvent(edits()$drawn, {
        
        drawn_count(drawn_count() + 1)
      })
      
      # confirm map changes to save
      observeEvent(input$confirm, {
        
        # merge added polygons
        map_out <- mapedit:::merge_add(rv$base_map, drawn = edits()$drawn) 
        # merge edited polygons
        map_out <- mapedit:::merge_edit(map_out, edits = edits()$edited)
        # remove deleted polygons
        row_drop <- edits()$deleted$layerId
        
        if (!is.null(row_drop)) {
          
          map_out <- map_out[!(map_out[[id]] %in% row_drop), ]
        }
        # remove unnecessary cols
        cols_keep <- !names(map_out) %in% c("X_leaflet_id", "feature_type") 
        
        map_out <- map_out[, cols_keep]
        
        rv$map_out <- map_out
        
        if (!is.null(edits()$drawn)) {
          # if new polygon drawn, user enters new ID
          # order by empty IDs
          ind <- order(rv$map_out[[id]], na.last = FALSE)
          rv$map_out <- rv$map_out[ind, ]
          # Disable editing for geometry column
          rv$geo_col <- grep("geometry", names(rv$map_out))
          rv$showTable <- TRUE
          
        } else {
          
          rv$save <- TRUE
        }
        
        rv$base_map <- map_out
      })

      output$showTable <- renderUI({
        
        if (rv$showTable) {
          
          tagList(
            actionButton("update_tab", "Update table", 
                         style = "background: blue; color: white;"),
            fluidRow(
              
              column(6,
                     div(style = "background-color: yellow; border: 1px solid #999; margin: 5px; margin-bottom: 2em;",
                         p("New polygons created, new IDs must be added before map can be saved. Double-click", 
                           "the table to edit. Press Crtl + Enter to save changes, Esc to exit edit mode.")))
            ),
            DT::DTOutput("map_add")
          )
        }
      })

      # edit new feature attributes ----
      
      output$map_add <- DT::renderDT({
        
        req(edits()$drawn)
        
        rv$map_out
        
      }, editable = list(target = "column",
                         disable = list(columns = rv$geo_col))
      )
      
      
      observeEvent(input$map_add_cell_edit, {
        
        rv$map_out <<- DT::editData(rv$map_out, input$map_add_cell_edit, "map_add")
        
      })
      
      # update table ----
      observeEvent(input$update_tab, {
        # check for empty entries in ID column
        # browser()
        
        # NA if unedited
        invalid_id <- function(x) {
          
          is.na(x) || length(x) == 0 || (is.character(x) && nchar(trimws(x)) == 0)
        }
        
        bad_id <- qaqc_helper(rv$map_out[[id]], invalid_id)
        
        # check for duplicate IDs 
        dup_id <- duplicated(rv$map_out[[id]])
        
        if (any(bad_id)) {
          
          showNotification("Invaild ID entry detected.", type = "warning")
          
        } else if (any(dup_id)) {
          
          showNotification("Duplicate ID entry detected.", type = "warning")
          
        } else {
          
          showNotification("Changes confirmed. New spatial table can be saved.")
          rv$save <- TRUE
          rv$showTable <- FALSE
        }
        
      }) 
      
      # Save ----
      observeEvent(input$save, {
        
        browser()
        
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
        
        # save edits as .RDS for tracking changes (may cause issues for R < 3.6 users)
        saveRDS(edits(), paste0(loc_data(project), "/spat/", map_lab, ".rds"))
        
        showNotification("Modified map saved to project data folder", 
                         type = "message")
        
        rv$showTable <- FALSE
        rv$save <- FALSE
      })
      
      # map reset
      observeEvent(input$reset, shinyjs::refresh())
      
      
      
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