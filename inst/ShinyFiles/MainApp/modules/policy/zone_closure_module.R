# File: zone_closure_module.R
# Description: UI and server logic for managing zone closures. Provides a control 
#              bar for inputs, a leaflet map for selecting spatial zones, and 
#              interactive tables for managing closure scenarios and allowable catches.
#              
# Dependencies: shiny, DT, bslib, leaflet, sf, purrr, shinycssloaders, yaml
# =================================================================================================

# zone closure server -----------------------------------------------------------------------------
#' zone_closure_server
#'
#' @param id A character string that is unique to this module instance.
#' @param rv_folderpath Reactive value for the root folder path.
#' @param rv_project_name Reactive value for the current project.
#' @param rv_data Reactive list containing spatial data (rv_data$spat).
#' @param spat_zone_id Optional string passed directly from the console wrapper to bypass GUI 
#'                     loader.
#' @return This module does not return a value.
zone_closure_server <- function(id, rv_folderpath, rv_project_name, rv_data, 
                                spat_zone_id = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Initialize Reactives ------------------------------------------------------------------------
    rv_clicked_zones   <- reactiveValues(ids = character(0)) 
    rv_tac_table       <- reactiveValues(data = NULL)        
    rv_saved_closures  <- reactiveValues(saved = list())      
    rv_selected_vars   <- reactiveValues(vars = NULL)
    
    # Trackers for database polling and initialization state to prevent double-loading
    rv_db_state        <- reactiveValues(mtime = NULL, choices = NULL, initialized = FALSE)
    rv_last_matrix     <- reactiveValues(val = NULL)
    
    # Main App Logic: Only run GUI loader if NOT in standalone console mode
    if (is.null(spat_zone_id)) {
      observe({
        req(current_project(), rv_folderpath())
        
        # Tell Shiny to re-run this check every 2.5 seconds
        shiny::invalidateLater(5000, session)
        
        # Read the current state of the file from the disk
        new_vars <- load_gui_variables(current_project(), rv_folderpath())
        
        # If valid data is found AND it's different from what we currently have in memory
        if (!is.null(new_vars) && !identical(new_vars, rv_selected_vars$vars)) {
          # Overwrite the old memory with the new disk data
          rv_selected_vars$vars <- new_vars
        }
      })
    }
    
    # Safely extract the project name string
    current_project <- reactive({
      req(rv_project_name())
      if (is.list(rv_project_name())) return(rv_project_name()$value)
      return(rv_project_name())
    })
    
    # Unified Helper to get the Zone ID (Handles both Main App and Standalone mode)
    get_zone_id <- reactive({
      if (!is.null(spat_zone_id)) {
        return(spat_zone_id) 
        
      } else {
        req(rv_selected_vars$vars$spat$spat_zone_id)
        return(rv_selected_vars$vars$spat$spat_zone_id) 
      }
    })
    
    # Process spatial data
    zone_df <- reactive({
      req(rv_data$spat, get_zone_id() )
      
      spat_data <- rv_data$spat
      z_id <- get_zone_id()
      
      spat_data %>%
        sf::st_transform(., "+proj=longlat +datum=WGS84") %>%
        mutate(second_location_id = paste0("Zone_", as.character(.data[[z_id]]))) %>%
        mutate(zone = as.character(.data[[z_id]]))
    })
    
    # Render Missing Matrix Warning ---------------------------------------------------------------
    output$alt_matrix_warning <- renderUI({
      req(rv_db_state$initialized)
      req(!is.null(input$alt_matrix_ui), input$alt_matrix_ui != "init") # Wait until UI updates
      
      # Check if the database genuinely has no matrices
      if (length(rv_db_state$choices) == 1 && rv_db_state$choices == "") {
        div(
          class = "alert alert-warning mb-3",
          role = "alert",
          shiny::icon("triangle-exclamation"),
          tags$strong(" No Alternative Matrix selected. "),
          "Please select a matrix from the dropdown. If none exist, run ", 
          tags$code("create_alternative_choice()"), " in the console to define your valid zones. ",
          "Once created, the map will automatically detect it and highlight the available zones."
        )
      } else {
        NULL
      }
    })
    
    # Populate & Auto-Refresh the Alternative Matrix Dropdown -------------------------------------
    observe({
      req(current_project())
      shiny::invalidateLater(2500, session) # Lightly poll every 2.5 seconds
      
      proj <- current_project()
      db_path <- locdatabase(project = proj)
      
      current_mtime <- if (file.exists(db_path)) file.info(db_path)$mtime else "missing"
      
      if (!identical(current_mtime, rv_db_state$mtime)) {
        rv_db_state$mtime <- current_mtime
        
        alt_table <- paste0(proj, "AltMatrix")
        
        dropdown_choices <- c("No matrices available" = "")
        table_is_present <- tryCatch({
          table_exists(alt_table, proj)
        }, error = function(e) {
          FALSE # If an error is thrown, silently assume the table does not exist
        })
        
        if (table_is_present) {
          alt_list <- unserialize_table(alt_table, proj)
          if (length(names(alt_list)) > 0) {
            dropdown_choices <- stats::setNames(names(alt_list), names(alt_list))
          } 
        }
        
        if (!identical(dropdown_choices, rv_db_state$choices)) {
          rv_db_state$choices <- dropdown_choices
          current_selection <- shiny::isolate(input$alt_matrix_ui)
          
          if (!isTruthy(current_selection) || current_selection == "init" || 
              !(current_selection %in% dropdown_choices)) {
            current_selection <- dropdown_choices[1]
          }
          
          updateSelectInput(session, "alt_matrix_ui", choices = dropdown_choices, 
                            selected = current_selection)
        }
      }
      
      # Un-pause downstream map rendering once the first DB fetch finishes
      if (!rv_db_state$initialized) {
        rv_db_state$initialized <- TRUE
      }
    })
    
    # Clear user selections ONLY when the selected matrix actually changes ------------------------
    observeEvent(input$alt_matrix_ui, {
      current_val <- input$alt_matrix_ui
      req(!is.null(current_val), current_val != "init") # Ignore the initial loading state
      
      if (is.null(rv_last_matrix$val)) {
        rv_last_matrix$val <- current_val
      } else if (current_val != rv_last_matrix$val) {
        rv_clicked_zones$ids <- character(0)
        rv_last_matrix$val <- current_val
      }
    }, ignoreInit = TRUE)
    
    # Extract Modeled Zones (Evaluates lazily - NO FALLBACK) --------------------------------------
    modeled_zones <- reactive({
      req(rv_db_state$choices, rv_db_state$mtime)
      req(current_project(), rv_db_state$initialized)
      req(!is.null(input$alt_matrix_ui), input$alt_matrix_ui != "init") # Hold until updates
      
      proj <- current_project()
      selected_matrix <- input$alt_matrix_ui
      
      if (selected_matrix == "") {
        return(NULL)
      }
      
      # Standard logic: Grab character-based names from database
      alt_table <- paste0(proj, "AltMatrix")
      table_is_present <- tryCatch({
        table_exists(alt_table, proj)
      }, error = function(e) { FALSE })
      
      if (table_is_present) {
        alt_list <- tryCatch({
          unserialize_table(alt_table, proj)
        }, error = function(e) list())
        
        if (selected_matrix %in% names(alt_list)) {
          return(as.character(unique(alt_list[[selected_matrix]]$greaterNZ)))
        }
      }
      
      return(NULL)
    })
    
    # Output Leaflet Map (Combined Plotting Logic) ------------------------------------------------
    output$zone_map_output <- leaflet::renderLeaflet({
      req(!is.null(input$alt_matrix_ui), input$alt_matrix_ui != "init") # Prevent double-rendering
      
      z_df <- zone_df()
      m_zones <- modeled_zones()
      
      showNotification("Map rendering, this may take a few moments...", type = "default",
                       duration = 5)
      
      is_point_data <- any(sf::st_geometry_type(z_df) %in% c("POINT", "MULTIPOINT"))
      bounds <- sf::st_bbox(z_df)
      
      map <- leaflet::leaflet() %>%
        leaflet::addProviderTiles("OpenStreetMap") %>%
        leaflet::fitBounds(lng1 = bounds[["xmin"]], lat1 = bounds[["ymin"]], 
                           lng2 = bounds[["xmax"]], lat2 = bounds[["ymax"]])
      
      # Add Base Layer (White)
      if (is_point_data) {
        map <- map %>%
          leaflet::addCircleMarkers(data = z_df, radius = 6, fillColor = "white", 
                                    fillOpacity = 0.6,
                                    color = "black", stroke = TRUE, weight = 1,
                                    layerId = ~second_location_id, group = "regions", 
                                    label = ~second_location_id)
      } else {
        map <- map %>%
          leaflet::addPolygons(data = z_df, fillColor = "white", fillOpacity = 0.5,
                               color = "black", stroke = TRUE, weight = 1,
                               layerId = ~second_location_id, group = "regions", 
                               label = ~second_location_id)
      }
      
      # Add Highlights Layer (Yellow Fill) if matrix exists
      if (!is.null(m_zones) && length(m_zones) > 0) {
        highlight_data <- z_df %>% filter(zone %in% m_zones)
        
        if (nrow(highlight_data) > 0) {
          if (is_point_data) {
            map <- map %>% 
              leaflet::addCircleMarkers(data = highlight_data, radius = 6, fillColor = "#FFC107",
                                        fillOpacity = 0.7,
                                        color = "#FFC107", stroke = TRUE, weight = 1,
                                        layerId = ~second_location_id, group = "regions", 
                                        label = ~second_location_id)
          } else {
            map <- map %>% 
              leaflet::addPolygons(data = highlight_data, fillColor = "#FFC107", fillOpacity = 0.5,
                                   color = "#FFC107", stroke = TRUE, weight = 1,
                                   layerId = ~second_location_id, group = "regions", 
                                   label = ~second_location_id)
          }
        }
      }
      
      return(map)
    })
    
    # Map Shape Selection Logic -------------------------------------------------------------------
    observeEvent(input$zone_map_output_shape_click, {
      click <- input$zone_map_output_shape_click
      req(click$id)
      
      sec_id <- "second_location_id"
      proxy <- leaflet::leafletProxy("zone_map_output")
      is_point_data <- any(sf::st_geometry_type(zone_df()) %in% c("POINT", "MULTIPOINT"))
      
      if (click$id %in% rv_clicked_zones$ids) {
        # DESELECTING
        rv_clicked_zones$ids <- setdiff(rv_clicked_zones$ids, click$id)
        proxy %>% leaflet::removeShape(layerId = paste0(click$id, "_selected"))
        
      } else {
        # SELECTING
        clicked_poly <- zone_df() %>% filter(.data[[sec_id]] == click$id)
        m_zones <- modeled_zones()
        
        if (is.null(m_zones)) {
          showNotification("Error: Please select an Alternative Matrix first.", 
                           type = "error", duration = 4)
          return()
        }
        
        if (!(clicked_poly$zone[1] %in% m_zones)) {
          showNotification("Error: You can only select valid highlighted zones from the chosen
                           Alternative Matrix.", 
                           type = "error", duration = 4)
          return()
        }
        
        rv_clicked_zones$ids <- unique(c(rv_clicked_zones$ids, click$id))
        
        if (is_point_data) {
          proxy %>% leaflet::addCircleMarkers(data = clicked_poly, radius = 6, fillColor = "red", 
                                              fillOpacity = 0.8,
                                              weight = 2, color = "black", stroke = TRUE,
                                              layerId = paste0(click$id, "_selected"), 
                                              group = "selected_zones",
                                              options = leaflet::pathOptions(interactive = FALSE))
        } else {
          proxy %>% leaflet::addPolygons(data = clicked_poly, fillColor = "red", fillOpacity = 0.5,
                                         weight = 2, color = "black", stroke = TRUE,
                                         layerId = paste0(click$id, "_selected"), 
                                         group = "selected_zones",
                                         options = leaflet::pathOptions(interactive = FALSE))
        }
      }
    })
    
    # Add & Instantly Save Closure Logic ----------------------------------------------------------
    observeEvent(input$add_closure_btn, {
      req(current_project())
      proj <- current_project()
      
      if (!isTruthy(input$alt_matrix_ui) || input$alt_matrix_ui == "init") {
        showNotification("An Alternative Matrix must be selected before saving.", type = "error",
                         duration = 5)
        return()
      }
      
      if (!isTruthy(input$scenario_name_input)) { 
        showNotification("Please enter a scenario name.", type = "warning", duration = 5)
        return()
      }
      
      if (length(rv_clicked_zones$ids) == 0) {
        showNotification("Please select at least one zone on the map.", type = "warning", 
                         duration = 5)
        return()
      }
      
      current_saved <- get_closure_scenario(proj)
      saved_names <- vapply(current_saved, function(x) x$scenario, character(1))
      
      if (input$scenario_name_input %in% c(saved_names, close_names(proj))) {
        showNotification("Scenario name already exists. Please enter a unique name.",
                         type = "error", duration = 5)
        return()
      }
      
      grid_nm <- deparse(substitute(zone_df())) 
      
      new_scenario <- list(
        scenario = input$scenario_name_input,
        date = as.character(Sys.Date()),
        zone = rv_clicked_zones$ids,
        tac = rv_tac_table$data$`% allowable TAC`,
        grid_name = grid_nm,
        alt_matrix = input$alt_matrix_ui
      )
      
      current_saved <- append(current_saved, list(new_scenario))
      save_closure_scenario(proj, current_saved)
      
      rv_saved_closures$saved <- get_closure_scenario(proj)
      updateTextInput(session, 'scenario_name_input', value = "")
      
      showNotification(paste("Scenario '", input$scenario_name_input, "' saved successfully!"), 
                       type = "message")
    })
    
    # View Saved Closures Table -------------------------------------------------------------------
    output$saved_closures_table <- DT::renderDataTable({
      req(current_project())
      rv_saved_closures$saved <- get_closure_scenario(current_project())
      
      if (length(rv_saved_closures$saved) == 0) return(NULL)
      
      saved_list <- rev(rv_saved_closures$saved)
      
      df <- data.frame(
        Scenario = vapply(saved_list, function(x) x$scenario, character(1)),
        Date = vapply(saved_list, function(x) x$date, character(1)),
        Alt_Matrix = vapply(saved_list, function(x) {
          val <- x$alt_matrix
          if (is.null(val) || val == "") "Unknown (Legacy)" else val
        }, character(1)),
        Zones = vapply(saved_list, function(x) paste(x$zone, collapse = ", "), character(1)),
        TAC_Percents = vapply(saved_list, function(x) paste(x$tac, collapse = ", "), character(1)),
        stringsAsFactors = FALSE
      )
      
      DT::datatable(df, 
                    options = list(dom = 't', paging = FALSE, scrollX = TRUE), 
                    selection = 'multiple', 
                    rownames = FALSE,
                    class = 'cell-border stripe hover')
    })
    
    # Delete Closures Directly from Table ---------------------------------------------------------
    observeEvent(input$delete_closure_btn, {
      req(current_project())
      proj <- current_project()
      
      selected_rows <- input$saved_closures_table_rows_selected
      
      if (length(selected_rows) == 0) {
        showNotification("Click on one or more scenarios in the Saved Scenarios table to delete 
                          them.", type = "warning")
        return()
      }
      
      saved_list <- rev(rv_saved_closures$saved)
      scenarios_to_delete <- vapply(saved_list[selected_rows], 
                                    function(x) x$scenario, character(1))
      
      del_ind <- vapply(rv_saved_closures$saved, 
                        function(x) x$scenario %in% scenarios_to_delete, logical(1))
      rv_saved_closures$saved[del_ind] <- NULL
      
      filename <- paste0(locoutput(proj), proj, "_closures.yaml")
      yaml::write_yaml(rv_saved_closures$saved, filename)
      
      showNotification("Selected closure scenarios deleted.", type = "message")
    })
    
    # Visualize Saved Closures on Map -------------------------------------------------------------
    observeEvent(input$saved_closures_table_rows_selected, {
      proxy <- leaflet::leafletProxy("zone_map_output")
      proxy %>% leaflet::clearGroup("saved_scenario_layer")
      
      selected_rows <- input$saved_closures_table_rows_selected
      is_point_data <- any(sf::st_geometry_type(zone_df()) %in% c("POINT", "MULTIPOINT"))
      
      if (length(selected_rows) > 0) {
        saved_list <- rev(rv_saved_closures$saved)
        selected_scenarios <- saved_list[selected_rows]
        zones_to_show <- unique(unlist(lapply(selected_scenarios, function(x) x$zone)))
        
        if (length(zones_to_show) > 0) {
          sec_id <- "second_location_id"
          poly_to_show <- zone_df() %>% filter(.data[[sec_id]] %in% zones_to_show)
          
          if (is_point_data) {
            proxy %>% leaflet::addCircleMarkers(data = poly_to_show, radius = 6, 
                                                fillColor = "#007bc2", fillOpacity = 0.8,
                                                weight = 2, color = "white", 
                                                stroke = TRUE, group = "saved_scenario_layer",
                                                options = leaflet::pathOptions(interactive = FALSE)) 
          } else {
            proxy %>% leaflet::addPolygons(data = poly_to_show, fillColor = "#007bc2",
                                           fillOpacity = 0.6,
                                           weight = 2, color = "white", stroke = TRUE, 
                                           group = "saved_scenario_layer",
                                           options = leaflet::pathOptions(interactive = FALSE)) 
          }
        }
      }
    }, ignoreNULL = FALSE) 
    
    # Editable TAC Table Logic --------------------------------------------------------------------
    observeEvent(rv_clicked_zones$ids, {
      if (length(rv_clicked_zones$ids) == 0) {
        rv_tac_table$data <- data.frame(Zones = character(0), `% allowable TAC` = numeric(0), 
                                        check.names = FALSE)
      } else {
        rv_tac_table$data <- data.frame(
          Zones = rv_clicked_zones$ids, 
          `% allowable TAC` = rep(0, length(rv_clicked_zones$ids)), 
          check.names = FALSE
        )
      }
    }, ignoreNULL = FALSE)
    
    output$tac_table_output <- DT::renderDataTable({
      req(rv_tac_table$data)
      DT::datatable(rv_tac_table$data, editable = TRUE, options = list(dom = 't', scrollX = TRUE),
                    class = 'cell-border stripe hover')
    })
    
    observeEvent(input$tac_table_output_cell_edit, {
      info <- input$tac_table_output_cell_edit
      tab_i <- info$row
      tab_j <- info$col
      tab_k <- info$value
      
      isolate({
        if (names(rv_tac_table$data)[tab_j] == "% allowable TAC") {
          val <- suppressWarnings(as.numeric(tab_k))
          
          if (is.na(val) || val < 0 || val > 100) {
            showNotification("Catch % must be a numeric value between 0 and 100.", type = "error")
            val <- 0
          }
          
          rv_tac_table$data[tab_i, tab_j] <- val
          
          if (sum(rv_tac_table$data[, tab_j], na.rm = TRUE) > 100) {
            showNotification("Total % allowable catch across all zones cannot exceed 100.", 
                             type = "error")
            rv_tac_table$data[tab_i, tab_j] <- 0 
          }
          
        } else {
          showNotification("Change zone IDs using the map interface.", type = "warning")
        }
      })
      
      DT::replaceData(DT::dataTableProxy("tac_table_output"), rv_tac_table$data,
                      resetPaging = FALSE)
    })
  })
}

# zone closure UI ---------------------------------------------------------------------------------

#' zone_closure_ui
#'
#' @param id A character string that is unique to this module instance.
#' @return A tagList containing the complete zone closure UI (controls, map, and tables).
zone_closure_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    
    # Instructions
    div(
      class = "mb-3",
      h4("Design Spatial Closures"),
      p(class = "text-muted",
        "Click on the map to highlight zones for your scenario. Enter a scenario name below ",
        "the map, adjust the allowable TAC percentage for each selected zone in the table, and ",
        "click 'Add closure' to instantly save it to your project database."
      )
    ),
    
    # Missing Matrix Warning (only displays if no matrices are selected)
    uiOutput(ns("alt_matrix_warning")),
    
    # Alternative Matrix Selection Card (with overflow settings to prevent clipping)
    bslib::card(
      class = "mb-3",
      fill = FALSE,
      style = "overflow: visible;", 
      bslib::card_body(
        class = "p-3",
        style = "overflow: visible;", 
        selectInput(ns('alt_matrix_ui'), 
                    label = tags$span(
                      "Alternative Matrix ", 
                      bslib::tooltip(
                        shiny::icon("info-circle"), 
                        "Select a saved alternative coice matrix to display the valid zones on 
                        the map."
                      )
                    ), 
                    choices = c("Initializing..." = "init"), # Default prevents double-renders
                    width = "100%")
      )
    ),
    
    # Main Map Card
    bslib::card(
      class = "mb-3",
      height = "700px",  
      fill = FALSE,      
      full_screen = TRUE,
      bslib::card_header(
        class = "d-flex justify-content-between align-items-center",
        "Spatial Zone Selection"
      ),
      bslib::card_body(
        class = "p-0",
        style = "overflow: hidden;", 
        shinycssloaders::withSpinner(
          leaflet::leafletOutput(ns("zone_map_output"), height = 650), 
          type = 6, color = "#007bc2"
        )
      )
    ),
    
    # Control Panel (Scenario Naming)
    bslib::card(
      class = "mb-3",
      fill = FALSE, 
      bslib::card_body(
        class = "p-3",
        fluidRow(
          column(
            width = 8, 
            textInput(ns('scenario_name_input'), 
                      label = tags$span(
                        "Scenario Name ", 
                        bslib::tooltip(
                          shiny::icon("info-circle"), 
                          "Enter a unique name for the new closure scenario."
                        )
                      ), 
                      value = '', placeholder = "e.g., Spring_Closure",
                      width = "100%")
          ),
          column(
            width = 4, 
            div(
              style = "margin-top: 32px;", 
              actionButton(ns('add_closure_btn'), 'Add closure', 
                           icon = shiny::icon("plus"),
                           class = "btn-primary w-100")
            )
          )
        )
      )
    ),
    
    # Allowable TAC Table Card
    bslib::card(
      class = "mb-3",
      fill = FALSE,
      height = 300, 
      full_screen = TRUE,
      bslib::card_header("Allowable TAC by Zone"),
      bslib::card_body(
        style = "overflow-y: auto; overflow-x: auto;", 
        DT::dataTableOutput(ns("tac_table_output"))
      )
    ),
    
    # Scenario Management Card (Single Table)
    div(
      class = "mb-5", 
      bslib::card(
        class = "mb-3",
        height = "400px",
        fill = FALSE,
        bslib::card_header(
          class = "d-flex justify-content-between align-items-center",
          "Saved Closure Scenarios",
          actionButton(ns('delete_closure_btn'), 'Delete Selected', 
                       class = "btn-danger btn-sm", icon = shiny::icon("trash"))
        ),
        bslib::card_body(
          style = "overflow-y: auto;",
          DT::dataTableOutput(ns("saved_closures_table"))
        )
      )
    )
  )
}