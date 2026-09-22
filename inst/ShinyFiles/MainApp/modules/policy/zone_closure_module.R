# File: zone_closure_module.R
# Description: UI and server logic for managing zone closures. Provides a control 
#              bar for inputs, a leaflet map for selecting spatial zones, and 
#              interactive tables for managing closure scenarios and allowable catches.
#              
# Dependencies: shiny, DT, bslib, leaflet, sf, purrr, shinycssloaders
# =================================================================================================

# zone closure server -----------------------------------------------------------------------------
#' zone_closure_server
#'
#' @param id A character string that is unique to this module instance.
#' @param rv_folderpath Reactive value for the root folder path.
#' @param rv_project_name Reactive value for the current project.
#' @param rv_data Reactive list containing spatial data (rv_data$spat) and main data (rv_data$main).
#' @param spat_zone_id Optional string for spatial zone ID column name.
#' @param main_zone_id Optional string for main data zone ID column name.
#' @param spat_name Optional string for the name of the spatial dataset to display.
#' @param main_name Optional string for the name of the main dataset to display.
#' @return This module does not return a value.

zone_closure_server <- function(id, rv_folderpath, rv_project_name, rv_data, 
                                spat_zone_id = NULL, main_zone_id = NULL,
                                spat_name = "Spatial Data", main_name = "Main Data") {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Initialize Reactives ------------------------------------------------------------------------
    rv_clicked_zones   <- reactiveValues(
      ids = character(0),         # Strictly VALID zones (goes to TAC table & DB)
      manual = character(0),      # Valid zones clicked manually
      bulk_valid = character(0),  # Valid zones from upload/existing
      bulk_raw = character(0)     # ALL raw zones matched by upload/existing
    ) 
    rv_tac_table       <- reactiveValues(data = NULL)        
    rv_saved_closures  <- reactiveValues(saved = list())      
    rv_selected_vars   <- reactiveValues(vars = NULL)
    rv_uploaded_shape  <- reactiveValues(poly = NULL)
    rv_active_layers   <- reactiveValues(groups = character(0)) # Tracks dynamic legend items
    
    # Trackers for database polling and initialization state to prevent double-loading
    rv_db_state        <- reactiveValues(mtime = NULL, choices = NULL, initialized = FALSE)
    rv_last_matrix     <- reactiveValues(val = NULL)
    rv_last_mode       <- reactiveValues(val = NULL)
    
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
    
    # Unified Helper to get the Zone ID depending on the data source
    get_zone_id <- function(source = "spat") {
      if (source == "main") {
        if (!is.null(main_zone_id)) return(main_zone_id)
        req(rv_selected_vars$vars$main$main_zone_id)
        return(rv_selected_vars$vars$main$main_zone_id)
      } else {
        if (!is.null(spat_zone_id)) return(spat_zone_id)
        req(rv_selected_vars$vars$spat$spat_zone_id)
        return(rv_selected_vars$vars$spat$spat_zone_id)
      }
    }
    
    # Process spatial data for map rendering
    zone_df <- reactive({
      req(rv_data$spat)
      z_id <- get_zone_id("spat")
      req(z_id)
      
      spat_data <- rv_data$spat
      
      spat_data %>%
        sf::st_transform(., "+proj=longlat +datum=WGS84") %>%
        mutate(second_location_id = paste0("Zone_", as.character(.data[[z_id]]))) %>%
        mutate(zone = as.character(.data[[z_id]]))
    })
    
    # Render Missing Matrix Warning ---------------------------------------------------------------
    output$alt_matrix_warning <- renderUI({
      req(rv_db_state$initialized)
      req(!is.null(input$alt_matrix_ui), input$alt_matrix_ui != "init") 
      
      if (length(rv_db_state$choices) == 1 && rv_db_state$choices == "") {
        div(
          class = "alert alert-warning mb-2",
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
    
    # Render Land Zone (Zone 0) Warning -----------------------------------------------------------
    output$zone_0_warning <- renderUI({
      # Strictly warn ONLY if the bulk upload/existing variable selects a land zone
      if ("Zone_0" %in% rv_clicked_zones$bulk_raw) {
        div(
          class = "alert alert-danger mb-2",
          role = "alert",
          shiny::icon("triangle-exclamation"),
          tags$strong(" Land Zone Detected! "),
          "Zone_0 (a land zone) was detected in your uploaded shapefile or existing variable
          overlap. Please ensure this is intended or filter it out."
        )
      } else {
        NULL
      }
    })
    
    # Dynamically update the Data Source radio buttons to hide 'Main Data' if missing ---------------
    observe({
      choices <- c("Spatial Data" = "spat")
      if (!is.null(rv_data$main)) {
        choices <- c(choices, "Main Data" = "main")
      }
      
      current_selection <- isolate(input$existing_data_source)
      if (is.null(current_selection) || !(current_selection %in% choices)) {
        current_selection <- "spat"
      }
      
      updateRadioButtons(session, "existing_data_source", choices = choices, 
                         selected = current_selection, inline = TRUE)
    })
    
    # Populate selectable closure variables from the selected data source -------------------------
    observe({
      req(input$existing_data_source)
      selected_data <- switch(
        input$existing_data_source,
        "main" = rv_data$main,
        "spat" = rv_data$spat
      )
      req(selected_data)
      
      choices <- sort(names(selected_data))
      
      # Update Closure Variable dropdown
      selected_var <- isolate(input$existing_var_name)
      updateSelectInput(
        session,
        "existing_var_name",
        choices = choices,
        selected = if (selected_var %in% choices) selected_var else NULL
      )
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
          FALSE 
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
      
      if (!rv_db_state$initialized) {
        rv_db_state$initialized <- TRUE
      }
    })
    
    # Clear user selections ONLY when the selected matrix actually changes ------------------------
    observeEvent(input$alt_matrix_ui, {
      current_val <- input$alt_matrix_ui
      req(!is.null(current_val), current_val != "init") 
      
      if (is.null(rv_last_matrix$val)) {
        rv_last_matrix$val <- current_val
      } else if (current_val != rv_last_matrix$val) {
        rv_clicked_zones$ids <- character(0)
        rv_clicked_zones$manual <- character(0)
        rv_clicked_zones$bulk_valid <- character(0)
        rv_clicked_zones$bulk_raw <- character(0)
        rv_last_matrix$val <- current_val
      }
    }, ignoreInit = TRUE)
    
    # Clear selections and uploaded geometry when switching modes -----------------------------------
    observeEvent(input$closure_mode, {
      if (!is.null(rv_last_mode$val) && !identical(input$closure_mode, rv_last_mode$val)) {
        rv_clicked_zones$ids <- character(0)
        rv_clicked_zones$manual <- character(0)
        rv_clicked_zones$bulk_valid <- character(0)
        rv_clicked_zones$bulk_raw <- character(0)
      }
      if (!identical(input$closure_mode, "upload")) {
        rv_uploaded_shape$poly <- NULL
      }
      rv_last_mode$val <- input$closure_mode
    }, ignoreInit = TRUE)
    
    # Extract Modeled Zones (Evaluates lazily - NO FALLBACK) --------------------------------------
    modeled_zones <- reactive({
      req(rv_db_state$choices, rv_db_state$mtime)
      req(current_project(), rv_db_state$initialized)
      req(!is.null(input$alt_matrix_ui), input$alt_matrix_ui != "init") 
      
      proj <- current_project()
      selected_matrix <- input$alt_matrix_ui
      
      if (selected_matrix == "") {
        return(NULL)
      }
      
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
      req(!is.null(input$alt_matrix_ui), input$alt_matrix_ui != "init")
      
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
      
      if (is_point_data) {
        map <- map %>%
          leaflet::addCircleMarkers(data = z_df, radius = 6, fillColor = "white", 
                                    fillOpacity = 0.6,
                                    color = "black", stroke = TRUE, weight = 1,
                                    layerId = ~paste0(second_location_id, "_base"), 
                                    group = "Base Zones", 
                                    label = ~second_location_id)
      } else {
        map <- map %>%
          leaflet::addPolygons(data = z_df, fillColor = "white", fillOpacity = 0.5,
                               color = "black", stroke = TRUE, weight = 1,
                               layerId = ~paste0(second_location_id, "_base"),
                               group = "Base Zones", 
                               label = ~second_location_id)
      }
      
      if (!is.null(m_zones) && length(m_zones) > 0) {
        highlight_data <- z_df %>% filter(zone %in% m_zones)
        
        if (nrow(highlight_data) > 0) {
          if (is_point_data) {
            map <- map %>% 
              leaflet::addCircleMarkers(data = highlight_data, radius = 6, fillColor = "#FFC107",
                                        fillOpacity = 0.7,
                                        color = "#FFC107", stroke = TRUE, weight = 1,
                                        layerId = ~paste0(second_location_id, "_alt"), 
                                        group = "Alternative Choice Set", 
                                        label = ~second_location_id)
          } else {
            map <- map %>% 
              leaflet::addPolygons(data = highlight_data, fillColor = "#FFC107", fillOpacity = 0.5,
                                   color = "#FFC107", stroke = TRUE, weight = 1,
                                   layerId = ~paste0(second_location_id, "_alt"), 
                                   group = "Alternative Choice Set", 
                                   label = ~second_location_id)
          }
        }
      }
      
      return(map)
    })
    
    # Dynamic Map Layer Control -------------------------------------------------------------------
    observe({
      req(!is.null(input$alt_matrix_ui), input$alt_matrix_ui != "init")
      
      current_groups <- c("Base Zones", "Alternative Choice Set")
      
      if (!is.null(rv_uploaded_shape$poly)) {
        current_groups <- c(current_groups, "Uploaded Shapefile")
      }
      
      if (length(rv_clicked_zones$bulk_raw) > 0 && identical(input$closure_mode, "existing")) {
        current_groups <- c(current_groups, "Existing Variable Zones")
      }
      
      if (length(rv_clicked_zones$manual) > 0 || length(rv_clicked_zones$bulk_valid) > 0) {
        current_groups <- c(current_groups, "Selected Zones")
      }
      
      if (length(input$saved_closures_table_rows_selected) > 0) {
        current_groups <- c(current_groups, "Saved Scenarios")
      }
      
      if (!identical(current_groups, rv_active_layers$groups)) {
        rv_active_layers$groups <- current_groups
        
        leaflet::leafletProxy("zone_map_output") %>%
          leaflet::addLayersControl(
            overlayGroups = current_groups,
            options = leaflet::layersControlOptions(collapsed = FALSE)
          )
      }
    })
    
    # Map Shape Selection Logic -------------------------------------------------------------------
    observeEvent(input$zone_map_output_shape_click, {
      click <- input$zone_map_output_shape_click
      req(click$id)
      
      click_id <- sub("(_base|_alt|_selected|_existing|_saved)$", "", click$id)
      
      if (click_id %in% rv_clicked_zones$ids) {
        rv_clicked_zones$ids <- setdiff(rv_clicked_zones$ids, click_id)
        rv_clicked_zones$manual <- setdiff(rv_clicked_zones$manual, click_id)
        rv_clicked_zones$bulk_valid <- setdiff(rv_clicked_zones$bulk_valid, click_id)
      } else {
        sec_id <- "second_location_id"
        clicked_poly <- zone_df() %>% filter(.data[[sec_id]] == click_id)
        m_zones <- modeled_zones()
        
        if (is.null(m_zones)) {
          showNotification("Error: Please select an Alternative Matrix first.", 
                           type = "error", duration = 4)
          return()
        }
        
        if (!(clicked_poly$zone[1] %in% m_zones)) {
          showNotification("Error: You can only select valid highlighted zones from the 
                           chosen Alternative Matrix.", 
                           type = "error", duration = 4)
          return()
        }
        
        rv_clicked_zones$manual <- unique(c(rv_clicked_zones$manual, click_id))
        rv_clicked_zones$ids <- unique(c(rv_clicked_zones$manual, rv_clicked_zones$bulk_valid))
      }
    })
    
    # Independent Observer 1: Uploaded Shapefile layer --------------------------------------------
    observeEvent(rv_uploaded_shape$poly, {
      proxy <- leaflet::leafletProxy("zone_map_output")
      proxy %>% leaflet::clearGroup("Uploaded Shapefile")
      
      if (!is.null(rv_uploaded_shape$poly)) {
        proxy %>% leaflet::addPolygons(
          data = rv_uploaded_shape$poly, 
          color = "purple", weight = 2, fillOpacity = 0.1, 
          group = "Uploaded Shapefile"
        )
      }
    }, ignoreNULL = FALSE)
    
    # Independent Observer 2: Existing Variable layer ---------------------------------------------
    observeEvent(rv_clicked_zones$bulk_raw, {
      proxy <- leaflet::leafletProxy("zone_map_output")
      proxy %>% leaflet::clearGroup("Existing Variable Zones")
      
      if (length(rv_clicked_zones$bulk_raw) > 0 && identical(input$closure_mode, "existing")) {
        existing_zones <- zone_df() %>% filter(second_location_id %in% rv_clicked_zones$bulk_raw)
        is_point_data <- any(sf::st_geometry_type(existing_zones) %in% c("POINT", "MULTIPOINT"))
        
        if (nrow(existing_zones) > 0) {
          if (is_point_data) {
            proxy %>% leaflet::addCircleMarkers(
              data = existing_zones, radius = 6, fillColor = "purple", fillOpacity = 0.5,
              weight = 2, color = "purple", stroke = TRUE, group = "Existing Variable Zones",
              layerId = ~paste0(second_location_id, "_existing"),
              options = leaflet::pathOptions(interactive = FALSE)
            )
          } else {
            proxy %>% leaflet::addPolygons(
              data = existing_zones, fillColor = "purple", fillOpacity = 0.3,
              weight = 2, color = "purple", stroke = TRUE, group = "Existing Variable Zones",
              layerId = ~paste0(second_location_id, "_existing"),
              options = leaflet::pathOptions(interactive = FALSE)
            )
          }
        }
      }
    }, ignoreNULL = FALSE)
    
    # Independent Observer 3: Selected Zones layer (Red valid overlaps only) ----------------------
    observeEvent(rv_clicked_zones$ids, {
      proxy <- leaflet::leafletProxy("zone_map_output")
      proxy %>% leaflet::clearGroup("Selected Zones")
      
      visual_ids <- unique(c(rv_clicked_zones$manual, rv_clicked_zones$bulk_valid))
      
      if (length(visual_ids) > 0) {
        selected_zones <- zone_df() %>% filter(second_location_id %in% visual_ids)
        is_point_data <- any(sf::st_geometry_type(selected_zones) %in% c("POINT", "MULTIPOINT"))
        
        if (nrow(selected_zones) > 0) {
          if (is_point_data) {
            proxy %>% leaflet::addCircleMarkers(
              data = selected_zones, radius = 6, fillColor = "red", fillOpacity = 0.8,
              weight = 2, color = "red", stroke = TRUE, group = "Selected Zones",
              layerId = ~paste0(second_location_id, "_selected"),
              options = leaflet::pathOptions(interactive = FALSE)
            )
          } else {
            proxy %>% leaflet::addPolygons(
              data = selected_zones, fillColor = "red", fillOpacity = 0.5,
              weight = 2, color = "red", stroke = TRUE, group = "Selected Zones",
              layerId = ~paste0(second_location_id, "_selected"),
              options = leaflet::pathOptions(interactive = FALSE)
            )
          }
        }
      }
    }, ignoreNULL = FALSE)
    
    # Uploaded Spatial File Selection Logic --------------------------------------------------------
    observeEvent(input$process_upload_btn, {
      req(input$closure_shapefile)
      
      m_zones <- modeled_zones()
      if (is.null(m_zones)) {
        showNotification("Error: Please select an Alternative Matrix first.",
                         type = "error", duration = 4)
        return()
      }
      
      processing_notification <- showNotification(
        "Processing uploaded file and calculating overlaps. This may take a moment...",
        type = "message",
        duration = NULL
      )
      on.exit(removeNotification(processing_notification), add = TRUE)
      shinyjs::show("closure_processing_spinner_container")
      on.exit(shinyjs::hide("closure_processing_spinner_container"), add = TRUE)
      
      overlap_result <- tryCatch(
        compute_closure_overlaps(
          input$closure_shapefile,
          zone_df(),
          input$overlap_threshold,
          return_shape = TRUE
        ),
        error = function(e) {
          showNotification(conditionMessage(e), type = "error", duration = 6)
          NULL
        }
      )
      if (is.null(overlap_result)) {
        return()
      }
      
      valid_sec_ids <- paste0("Zone_", m_zones)
      raw_ids <- overlap_result$ids
      valid_ids <- intersect(raw_ids, valid_sec_ids)
      
      rv_uploaded_shape$poly <- overlap_result$shape
      rv_clicked_zones$bulk_raw <- raw_ids                                  
      rv_clicked_zones$bulk_valid <- valid_ids
      rv_clicked_zones$ids <- unique(c(rv_clicked_zones$manual, valid_ids))
      
      if (length(raw_ids) > 0 && length(valid_ids) == 0) {
        showNotification("None of the overlapping zones from the upload are in the selected
                           Alternative Matrix.", type = "warning", duration = 6)
      } else if (length(valid_ids) < length(raw_ids)) {
        showNotification(
          sprintf("%d zone(s) selected. Ignored %d zone(s) outside the Alternative Matrix.", 
                  length(valid_ids), length(raw_ids) - length(valid_ids)),
          type = "message", duration = 6
        )
      } else if (length(valid_ids) > 0) {
        showNotification(paste(length(valid_ids), "zone(s) selected from the uploaded 
                                 shapefile."), type = "message", duration = 5)
      } else {
        showNotification("No zones met the overlap threshold.", type = "warning", duration = 5)
      }
    },
    ignoreInit = TRUE)
    
    # Existing Variable Selection Logic -------------------------------------------------------------
    observeEvent(input$process_existing_btn, {
      req(input$existing_var_name, input$existing_var_val)
      
      m_zones <- modeled_zones()
      if (is.null(m_zones)) {
        showNotification("Error: Please select an Alternative Matrix first.",
                         type = "error", duration = 4)
        return()
      }
      
      shinyjs::show("closure_processing_spinner_container")
      on.exit(shinyjs::hide("closure_processing_spinner_container"), add = TRUE)
      
      selected_data <- switch(
        input$existing_data_source,
        "main" = rv_data$main,
        "spat" = rv_data$spat
      )
      req(selected_data)
      
      zone_id <- tryCatch(get_zone_id(input$existing_data_source), error = function(e) NULL)
      
      if (is.null(zone_id) || !zone_id %in% names(selected_data)) {
        showNotification(
          paste0("Error: Could not find the zone ID column ('",
                 if(is.null(zone_id)) "unknown" else zone_id, "') in the selected dataset."),
          type = "error", duration = 6
        )
        return()
      }
      
      zone_ids <- selected_data %>%
        filter(
          as.character(.data[[input$existing_var_name]]) ==
            input$existing_var_val
        ) %>%
        pull(.data[[zone_id]]) %>%
        unique()
      
      valid_sec_ids <- paste0("Zone_", m_zones)
      raw_ids <- paste0("Zone_", as.character(zone_ids))
      valid_ids <- intersect(raw_ids, valid_sec_ids)
      
      rv_clicked_zones$bulk_raw <- raw_ids                                  
      rv_clicked_zones$bulk_valid <- valid_ids
      rv_clicked_zones$ids <- unique(c(rv_clicked_zones$manual, valid_ids))
      
      if (length(raw_ids) > 0 && length(valid_ids) == 0) {
        showNotification("None of the zones for this variable are in the selected
                           Alternative Matrix.", type = "warning", duration = 6)
      } else if (length(valid_ids) < length(raw_ids)) {
        showNotification(
          sprintf("%d zone(s) selected. Ignored %d zone(s) outside the Alternative Matrix.", 
                  length(valid_ids), length(raw_ids) - length(valid_ids)),
          type = "message", duration = 6
        )
      } else if (length(valid_ids) > 0) {
        showNotification(paste(length(valid_ids), 
                               "zone(s) selected from the existing variable."),
                         type = "message", duration = 5)
      }
    },
    ignoreInit = TRUE)
    
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
        showNotification("Please select at least one valid zone on the map.", type = "warning", 
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
        alt_matrix = input$alt_matrix_ui,
        selection_method = input$closure_mode,
        overlap_threshold = if (identical(input$closure_mode, "upload")) {
          input$overlap_threshold
        } else {
          NULL
        }
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
      
      save_closure_scenario(proj, rv_saved_closures$saved)
      
      showNotification("Selected closure scenarios deleted.", type = "message")
    })
    
    # Visualize Saved Closures on Map -------------------------------------------------------------
    observeEvent(input$saved_closures_table_rows_selected, {
      proxy <- leaflet::leafletProxy("zone_map_output")
      proxy %>% leaflet::clearGroup("Saved Scenarios")
      
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
                                                stroke = TRUE, group = "Saved Scenarios",
                                                layerId = ~paste0(second_location_id, "_saved"),
                                                options = leaflet::pathOptions(interactive = FALSE)) 
          } else {
            proxy %>% leaflet::addPolygons(data = poly_to_show, fillColor = "#007bc2",
                                           fillOpacity = 0.6,
                                           weight = 2, color = "white", stroke = TRUE, 
                                           group = "Saved Scenarios",
                                           layerId = ~paste0(second_location_id, "_saved"),
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
      DT::datatable(
        rv_tac_table$data, 
        editable = TRUE, 
        class = 'cell-border stripe hover compact', 
        width = "100%", 
        options = list(
          dom = 'tp', 
          pageLength = 5, 
          lengthChange = FALSE, 
          scrollX = TRUE,
          scrollCollapse = TRUE
        )
      )
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
    
    # Enhanced Title and Instructions
    div(
      class = "mb-2",
      div(
        class = "d-flex align-items-center mb-2",
        shiny::icon("draw-polygon", class = "fa-2x text-primary me-2"),
        tags$h3("Design Spatial Closures", class = "mb-0 text-primary fw-bold")
      ),
      p(class = "text-muted mb-0",
        "Select zones by clicking the map or uploading a shapefile. Enter a scenario name below ",
        "the map, adjust the allowable TAC percentage for each selected zone in the table, and ",
        "click 'Add closure' to save it to your project database."
      )
    ),
    
    # Missing Matrix Warning & Zone 0 Warning
    uiOutput(ns("alt_matrix_warning")),
    uiOutput(ns("zone_0_warning")),
    
    # Alternative Matrix and Zone Selection grouped side-by-side
    fluidRow(
      class = "mb-3 d-flex align-items-stretch",
      
      # Column 1: Alt Matrix
      column(
        width = 4,
        bslib::card(
          class = "card-overflow h-100 mb-0",
          fill = FALSE,
          style = "overflow: visible;", 
          bslib::card_body(
            class = "p-3",
            style = "overflow: visible;", 
            selectInput(ns('alt_matrix_ui'), 
                        label = tags$span(
                          "Alternative Matrix ", 
                          bslib::tooltip(
                            shiny::icon("info-circle", class = "text-muted ms-1"), 
                            "Select a saved alternative choice matrix to display the valid 
                            zones on the map."
                          )
                        ), 
                        choices = c("Initializing..." = "init"), 
                        width = "100%")
          )
        )
      ),
      
      # Column 2: Zone Selection Method
      column(
        width = 8,
        bslib::card(
          class = "card-overflow h-100 mb-0",
          fill = FALSE,
          style = "overflow: visible;", 
          bslib::card_body(
            class = "p-3",
            style = "overflow: visible;", 
            radioButtons(
              ns("closure_mode"),
              label = tags$span(
                "Zone selection method ", 
                bslib::tooltip(
                  shiny::icon("info-circle", class = "text-muted ms-1"), 
                  "Choose how to identify zones for the closure scenario."
                )
              ),
              choices = c(
                "Click on Map" = "map",
                "Upload Shapefile" = "upload",
                "Select Existing Variable" = "existing"
              ),
              selected = "map",
              inline = TRUE
            ),
            
            # Upload panel (Inputs side-by-side)
            conditionalPanel(
              condition = sprintf("input['%s'] === 'upload'", ns("closure_mode")),
              fluidRow(
                column(6,
                       fileInput(
                         ns("closure_shapefile"),
                         "Upload spatial file components",
                         accept = c(".shp", ".shx", ".dbf", ".prj", ".cpg", ".geojson", 
                                    ".json", ".gpkg", ".rds", ".csv"),
                         multiple = TRUE,
                         width = "100%"
                       )
                ),
                column(3,
                       numericInput(
                         ns("overlap_threshold"),
                         label = tags$span(
                           "Min overlap (%) ", 
                           bslib::tooltip(
                             shiny::icon("info-circle", class = "text-muted ms-1"), 
                             "Only zones overlapping the shapefile by at least this percentage 
                        will be selected."
                           )
                         ),
                         value = 50,
                         min = 0,
                         max = 100,
                         step = 1,
                         width = "100%"
                       )
                ),
                column(3,
                       div(
                         style = "margin-top: 32px;",
                         actionButton(ns("process_upload_btn"), "Calculate",
                                      class = "btn-primary w-100", icon = icon("calculator"))
                       )
                )
              )
            ),
            
            # Existing panel (Inputs side-by-side)
            conditionalPanel(
              condition = sprintf("input['%s'] === 'existing'", ns("closure_mode")),
              fluidRow(
                column(3,
                       radioButtons(
                         ns("existing_data_source"),
                         "Data Source:",
                         choices = c("Spatial Data" = "spat", "Main Data" = "main"),
                         selected = "spat",
                         inline = TRUE
                       )
                ),
                column(4,
                       selectInput(
                         ns("existing_var_name"),
                         label = tags$span(
                           "Closure variable ", 
                           bslib::tooltip(
                             shiny::icon("info-circle", class = "text-muted ms-1"), 
                             "Select a column from the dataset that flags closure zones."
                           )
                         ),
                         choices = character(0),
                         width = "100%"
                       )
                ),
                column(3,
                       textInput(
                         ns("existing_var_val"),
                         label = tags$span(
                           "Closure value ", 
                           bslib::tooltip(
                             shiny::icon("info-circle", class = "text-muted ms-1"), 
                             "The value in the selected variable that indicates a zone is 
                        closed (e.g., 1 or TRUE)."
                           )
                         ),
                         value = "1",
                         width = "100%"
                       )
                ),
                column(2,
                       div(
                         style = "margin-top: 32px;",
                         actionButton(ns("process_existing_btn"), "Select", 
                                      class = "btn-primary w-100", icon = icon("check"))
                       )
                )
              )
            )
          )
        )
      )
    ),
    
    # Main Map Card
    bslib::card(
      class = "card-overflow mb-3",
      fill = FALSE,      
      full_screen = TRUE,
      bslib::card_header(
        class = "text-primary fw-bold d-flex justify-content-between align-items-center",
        tags$span(
          shiny::icon("map-location-dot", class = "me-2"), "Zone Selection Map",
          bslib::tooltip(
            shiny::icon("info-circle", class = "text-secondary ms-2"), 
            "Click individual valid zones to manually add or remove them from your selection."
          )
        )
      ),
      bslib::card_body(
        class = "p-0",
        style = "overflow: hidden;", 
        div(
          leaflet::leafletOutput(ns("zone_map_output"), height = 600),
          div(
            id = ns("closure_processing_spinner_container"),
            style = "display: none;",
            spinner_ui(
              id = ns("closure_processing_spinner"),
              spinner_type = "circle",
              size = "large",
              message = "Processing spatial closure...",
              overlay = TRUE
            )
          )
        )
      )
    ),
    
    # Control Panel (Scenario Naming)
    bslib::card(
      class = "card-overflow mb-3",
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
                          shiny::icon("info-circle", class = "text-muted ms-1"), 
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
      class = "card-overflow mb-3",
      fill = FALSE,
      full_screen = TRUE,
      bslib::card_header(
        class = "text-primary fw-bold",
        tags$span(
          shiny::icon("table-list", class = "me-2"), "Allowable TAC by Zone",
          bslib::tooltip(
            shiny::icon("info-circle", class = "text-secondary ms-2"), 
            "Double-click a cell in the '% allowable TAC' column to edit the value for
            that specific zone."
          )
        )
      ),
      bslib::card_body(
        class = "p-3",
        fill = FALSE,
        tags$style(HTML(paste0("#", ns("tac_table_output"),
                               " { height: auto !important; min-height: 0 !important; }"))),
        tags$div(
          style = "height: auto !important; width: 100%;",
          DT::dataTableOutput(ns("tac_table_output"), width = "100%", height = "auto")
        )
      )
    ),
    
    # Scenario Management Card 
    bslib::card(
      class = "card-overflow mb-3",
      height = "400px",
      fill = FALSE,
      bslib::card_header(
        class = "d-flex justify-content-between align-items-center text-primary fw-bold",
        tags$span(
          shiny::icon("folder-open", class = "me-2"), "Saved Closure Scenarios",
          bslib::tooltip(
            shiny::icon("info-circle", class = "text-secondary ms-2"), 
            "Click a row to visualize the saved scenario on the map above."
          )
        ),
        actionButton(ns('delete_closure_btn'), 'Delete Selected', 
                     class = "btn-danger btn-sm", icon = shiny::icon("trash"))
      ),
      bslib::card_body(
        class = "p-3",
        style = "overflow-y: auto;",
        DT::dataTableOutput(ns("saved_closures_table"))
      )
    )
  )
}

