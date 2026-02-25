# =================================================================================================
# File: format_model_data_module.R
# Description: This module defines the UI and server logic for formatting model data.
#              It manages a list of formatted datasets stored in [Project]LongFormatData.
#              
# Authors: Anna Abelman, Paul Carvalho
# Date created: 1/30/2026
# Dependencies: shiny, DT, shinyjs, bslib
# Notes: This module interacts with the main reactive data values (rv_data) and
#        project database.
# =================================================================================================

# format model data server ----------------------------------------------------------------------
#' format_model_data_server
#'
#' @param id A character string that is unique to this module instance.
#' @param rv_project_name A reactive value containing the current project name.
#' @param rv_data A reactiveValues object containing the loaded data frames.
#'
#' @return This module does not return a value.
format_model_data_server <- function(id, rv_folderpath, rv_project_name, 
                                     rv_data, rv_shared_alt_names = NULL,
                                     rv_shared_exp_names = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    rv_selected_vars <- reactiveValues(vars = NULL)
    
    # Reactive value to store strictly the NAMES of formatted datasets
    rv_existing_formatted_names <- reactiveVal(character(0))
    
    # Helper to convert empty strings to NULL for function args
    empty_to_null <- function(x) {
      if (is.null(x) || length(x) == 0) return(NULL)
      # If it's a vector (e.g. c("Year", "Month")), return it as is
      if (length(x) > 1) return(x) 
      # Only check for empty string if it's a single value
      if (x == "") return(NULL)
      return(x)
    }    
    
    # 1. Load Manage Table Data ----------------------------------------------------------------
    load_formatted_data <- function() {
      req(rv_project_name())
      project <- rv_project_name()$value
      table_name <- paste0(project, "LongFormatData")
      
      # Default to empty
      just_names <- character(0)
      
      if (table_exists(table_name, project)) {
        # Load the object from DB
        full_data <- tryCatch({
          unserialize_table(table_name, project)
        }, error = function(e) {
          return(list()) 
        })
        
        # Extract NAMES only
        # NOTE: format_model_data saves two entries per run: "Name" and "Name_settings"
        if (length(full_data) > 0) {
          all_keys <- names(full_data)
          # Filter out keys ending in "_settings"
          just_names <- all_keys[!grepl("_settings$", all_keys)]
        }
      }
      
      # Update reactive value
      rv_existing_formatted_names(just_names)
      
      # Update removal dropdown
      updateSelectizeInput(session, "data_to_remove", choices = just_names, selected = "")
    }
    
    # Load data on init
    observeEvent(rv_data$main, {
      load_formatted_data()
    }, once = TRUE)
    
    
    # 2. Dropdown Logic --------------------------------------------------------------------
    # Instead of querying the DB for alt names, we listen to the shared list
    observe({
      req(rv_shared_alt_names)
      req(rv_shared_exp_names)
      
      # Get the names directly from the shared reactive
      alt_current_names <- rv_shared_alt_names()
      exp_current_names <- rv_shared_exp_names()
      
      # Update the dropdown immediately
      updateSelectizeInput(session, "alt_name_input", 
                           choices = alt_current_names)
      updateSelectizeInput(session, "expectations_name_input", 
                           choices = exp_current_names)
    })
    
    # Update input choices based on the main data
    observe({
      req(rv_data$main)
      choices <- colnames(rv_data$main)
      updateSelectizeInput(session, "select_vars_input", choices = choices)
    })
    
    # Update Aux and Grid Dropdowns based on available data
    observe({
      req(rv_project_name())
      project <- rv_project_name()$value
      
      # Update Auxiliary Data Dropdown ---
      aux_choices <- c("None" = "")
      if (!is.null(rv_data$aux)) {
        label <- list_tables(project, "aux")[1]
        aux_choices <- c("None" = "", label)
      }
      updateSelectInput(session, "aux_data", choices = aux_choices)
      
      # Update Gridded Data Dropdown ---
      grid_choices <- c("None" = "")
      if (!is.null(rv_data$grid)) {
        label <- list_tables(project, "grid")[1]
        grid_choices <- c("None" = "", label)
      }
      updateSelectInput(session, "gridded_data", choices = grid_choices)
    })
    
    
    # 3. Execution Logic (Run Format) -------------------------------------------------------
    observeEvent(input$run_format_btn, {
      req(rv_project_name(), input$format_name_input)
      folderpath <- rv_folderpath()
      project_name <- rv_project_name()$value
      
      # UI State: Reset messages and show spinner
      shinyjs::hide("format_success_message")
      shinyjs::hide("format_error_message")
      shinyjs::show("run_format_spinner_container")
      shinyjs::disable("run_format_btn")
      
      # Load selected variables
      selected_vars <- load_gui_variables(project_name, folderpath)
      if (is.null(selected_vars)) {
        shinyjs::hide("run_format_spinner_container")
        shinyjs::enable("run_format_btn")
        showModal(modalDialog(
          title = "Error: Missing Data",
          "The selected variables file could not be found.",
          easyClose = TRUE
        ))
        return()
      }
      rv_selected_vars$vars <- selected_vars
      
      # Prepare Auxiliary Data
      if (input$aux_data != "" && !is.null(rv_data$aux)) {
        final_aux_data <- input$aux_data
        final_aux_key  <- empty_to_null(rv_selected_vars$vars$aux$aux_id)
        
        # Check if key is missing completely
        if (is.null(final_aux_key)) {
          shinyjs::hide("run_format_spinner_container")
          shinyjs::enable("run_format_btn")
          
          showModal(modalDialog(
            title = "Error: Missing Auxiliary Key",
            "You have selected an auxiliary dataset, but an auxiliary key could not be found. 
            Please ensure you have assigned the variable that links the main data to the 
            auxiliary data.",
            easyClose = TRUE,
            footer = modalButton("Close")
          ))
          return()
        }
        
        # Check if the key actually exists in the main data table
        if (!all(final_aux_key %in% colnames(rv_data$main))) {
          shinyjs::hide("run_format_spinner_container")
          shinyjs::enable("run_format_btn")
          
          showModal(modalDialog(
            title = "Error: Column Mismatch",
            paste0("The selected auxiliary key ('", paste(final_aux_key, collapse = ", "), 
                   "') does not exist in the main dataset. Please select a valid matching column."),
            easyClose = TRUE,
            footer = modalButton("Close")
          ))
          return()
        }
        
      } else {
        final_aux_data <- NULL
        final_aux_key  <- NULL
      }
      
      # Prepare Gridded Data
      if (input$gridded_data != "" && !is.null(rv_data$grid)) {
        final_grid_data <- input$gridded_data 
        
        final_grid_var  <- empty_to_null(input$grid_var_name)
        final_grid_time <- empty_to_null(rv_selected_vars$vars$grid$grid_time)
      } else {
        final_grid_data <- NULL
        final_grid_var  <- NULL
        final_grid_time <- NULL
      }
      
      # Check Expectations / Alternative Matrix Match
      if (input$expectations_name_input != "" && input$alt_name_input != "") {
        
        # Fetch the expectations table from the database
        exp_table_name <- paste0(project_name, "ExpectedCatch")
        
        exp_master_list <- tryCatch({
          unserialize_table(exp_table_name, project_name)
        }, error = function(e) return(NULL))
        
        # Extract the settings for the selected expectations matrix
        exp_settings_key <- paste0(input$expectations_name_input, "_settings")
        
        if (!is.null(exp_master_list) && !is.null(exp_master_list[[exp_settings_key]])) {
          
          # Compare the saved choice matrix to the selected one
          saved_alt_name <- exp_master_list[[exp_settings_key]]$alt_name
          
          if (!is.null(saved_alt_name) && saved_alt_name != input$alt_name_input) {
            
            # Show error and halt
            shinyjs::hide("run_format_spinner_container")
            shinyjs::enable("run_format_btn")
            
            showModal(modalDialog(
              title = "Error: Matrix Mismatch",
              paste0("The selected expected catch matrix ('", input$expectations_name_input, 
                     "') was created using a different alternative choice matrix ('",
                     saved_alt_name, "'). Please select the matching alternative matrix."),
              easyClose = TRUE,
              footer = modalButton("Close")
            ))
            return() # Halt execution
          }
        }
      }
      
      # Run Formatting Function
      tryCatch({
        format_model_data(
          project        = project_name,
          name           = input$format_name_input,
          alt_name       = input$alt_name_input,
          zone_id        = rv_selected_vars$vars$main$main_zone_id,
          unique_obs_id  = rv_selected_vars$vars$main$main_unique_obs_id,
          select_vars    = input$select_vars_input,
          aux_data       = final_aux_data,
          aux_key        = final_aux_key,
          gridded_data   = final_grid_data,
          grid_var_name  = final_grid_var,
          grid_time_var  = final_grid_time,
          main_time_var =  rv_selected_vars$vars$main$main_date,
          expectations   = input$expectations_name_input,
          distance       = input$distance_input,
          distance_units = if(input$distance_input) input$distance_units_input else NULL,
          crs            = 
            if(input$distance_input && input$crs_input != "") as.numeric(input$crs_input) else NULL,
          impute         = empty_to_null(input$impute_input)
        )
        
        # Success Feedback
        output$format_success_out <- renderText({
          paste0("Success: Data '", isolate(input$format_name_input),
                 "' formatted and saved to project.")
        })
        shinyjs::show("format_success_message")
        
        # RELOAD TABLE
        load_formatted_data()
        
      }, error = function(e) {
        output$format_error_out <- renderText({
          paste("Error:", e$message)
        })
        shinyjs::show("format_error_message")
        
      }, finally = {
        shinyjs::hide("run_format_spinner_container")
        shinyjs::enable("run_format_btn")
      })
    })
    
    # 4. Manage Table Logic (View / Remove) -------------------------------------------------------
    # Render table with names of formatted datasets
    output$existing_data_table <- DT::renderDataTable({
      # Retrieve the character vector of names
      d_names <- rv_existing_formatted_names()
      
      # Handle Empty Case
      if (length(d_names) == 0) {
        return(DT::datatable(
          data.frame(Name = character(0), Actions = character(0)),
          caption = "No Formatted Datasets found."
        ))
      }
      
      # Create buttons with embedded JS onclick events
      actions <- sapply(d_names, function(name) {
        as.character(
          tags$button(
            class = "btn btn-secondary btn-sm",
            onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})", 
                              ns("view_settings_trigger"), name),
            "View Settings"
          )
        )
      })
      
      # Force structure into a Data Frame
      df <- data.frame(
        Name = d_names,
        Actions = actions,
        stringsAsFactors = FALSE
      )
      
      # Render
      DT::datatable(df,
                    options = list(pageLength = 3, searching = TRUE, dom = 'tp',
                                   scrollX = TRUE,
                                   columnDefs = list(list(orderable = FALSE, targets = 1))
                    ),
                    rownames = FALSE,
                    escape = FALSE, # Essential for HTML buttons
                    selection = 'none',
                    caption = "Existing Formatted Datasets")
    })
    
    # Triggered by the JS in the table buttons (Settings Modal)
    observeEvent(input$view_settings_trigger, {
      selected_name <- input$view_settings_trigger
      project_name <- rv_project_name()$value
      table_name_db <- paste0(project_name, "LongFormatData")
      
      # Load the master list from DB
      master_list <- tryCatch({
        unserialize_table(table_name_db, project_name)
      }, error = function(e) return(NULL))
      
      # NOTE: Settings are stored in a separate key with suffix "_settings"
      settings_key <- paste0(selected_name, "_settings")
      
      if (is.null(master_list) || is.null(master_list[[settings_key]])) {
        showNotification("Could not load settings for this item.", type = "error")
        return()
      }
      
      # Extract specific sub-list
      settings_list <- master_list[[settings_key]]
      
      # Format values for display
      format_val <- function(p_name, x) {
        # Check specifically for select_vars being empty/NULL
        if (p_name == "select_vars" && 
            (is.null(x) || length(x) == 0 || (length(x) == 1 && x == ""))) { 
          return("All variables")
        } else if (is.data.frame(x)) {
          return(paste0("[Data Frame] ", nrow(x), " rows"))
        } else if (is.list(x)) {
          return(paste0("[List] ", length(x), " elements"))
        } else if (is.null(x)) {
          return("NULL") 
        } else if (length(x) > 10) {
          return(paste0("[Vector] ", length(x), " items"))
        } else {
          return(paste(as.character(x), collapse = ", "))
        }
      }
      
      # Construct Table UI
      settings_ui <- tags$div(
        style = "overflow-x: auto;",
        tags$table(class = "table table-sm table-striped",
                   tags$thead(tags$tr(tags$th("Parameter"), tags$th("Value"))),
                   tags$tbody(
                     lapply(names(settings_list), function(p_name) {
                       val <- settings_list[[p_name]]
                       tags$tr(
                         tags$td(tags$b(p_name)),
                         tags$td(format_val(p_name, val)) # Pass p_name here
                       )
                     })
                   )
        )
      )
      
      # Show Modal 
      showModal(modalDialog(
        title = paste("Settings for:", selected_name),
        settings_ui,
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    })
    
    # Remove Data Option
    observeEvent(input$remove_data_btn, {
      req(input$data_to_remove)
      showModal(modalDialog(
        title = "Confirm Removal",
        paste("Are you sure you want to permanently remove the dataset:", 
              input$data_to_remove, "?"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_remove_btn"), "Remove", class = "btn-danger")
        )
      ))
    })
    
    # Confirm Removal
    observeEvent(input$confirm_remove_btn, {
      removeModal()
      target_name <- input$data_to_remove
      project_name <- rv_project_name()$value
      table_name_db <- paste0(project_name, "LongFormatData")
      
      tryCatch({
        fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project = project_name))
        on.exit({ if(DBI::dbIsValid(fishset_db)) DBI::dbDisconnect(fishset_db) })
        
        if(table_exists(table_name_db, project_name)){
          master_list <- unserialize_table(table_name_db, project_name)
          
          # Remove both the Data and the Settings
          target_settings <- paste0(target_name, "_settings")
          
          if(target_name %in% names(master_list)){
            master_list[[target_name]] <- NULL
          }
          if(target_settings %in% names(master_list)){
            master_list[[target_settings]] <- NULL
          }
          
          # Remove old table
          table_remove(table_name_db, project_name)
          
          # Only recreate table if list is not empty
          if(length(master_list) > 0){
            DBI::dbExecute(fishset_db, paste("CREATE TABLE", table_name_db,
                                             "(data BLOB)"))
            DBI::dbExecute(fishset_db, paste("INSERT INTO", table_name_db, 
                                             "(data) VALUES (:data)"),
                           params = list(data = list(serialize(master_list, NULL))))
          }
          
          load_formatted_data()
          showNotification("Removed successfully.", type = "message")
        }
      }, error = function(e) {
        showNotification(paste("Error removing:", e$message), type = "error")
      })
    })
    
  })
}

# format model data sidebar ---------------------------------------------------------------------
#' format_model_data_ui
#'
#' @param id A character string that is unique to this module instance.
#' @return A tagList containing the sidebar UI elements.
format_model_data_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    shinyjs::useShinyjs(),
    
    div(id = ns("main_container"),
        
        # CARD 1: Inputs and Run Button
        bslib::card(
          class = "card-overflow",
          bslib::card_header('Format Model Data'),
          bslib::card_body(
            class = "card-overflow",
            p("This module reshapes your project data into a long format required for modeling. 
               For more information on formatting model data, see ",
              tags$a(
                href = "https://noaa-nwfsc.github.io/FishSET/articles/FishSET_User_Manual.html",
                "section 8.4"),
              "in the FishSET User Manual."),
            
            #  Configuration cards 
            bslib::layout_column_wrap(
              fill = FALSE,
              width = 1/3, 
              gap = "1rem",
              
              # 1. Core Inputs
              bslib::card(
                class = "card-overflow",
                bslib::card_header(h5("1. Core Inputs", class = "mb-0")),
                bslib::card_body(
                  class = "card-overflow d-flex flex-column gap-3", 
                  textInput(ns("format_name_input"), 
                            label = tags$span(
                              "Name of new data table ", 
                              bslib::tooltip(shiny::icon("info-circle"),
                                             "Unique name for this specific formatted model data
                                             instance.")), 
                            placeholder = "Unique name", width = "100%"),
                  div(
                    class = "p-3 bg-light border rounded",
                    h6(tags$span("Select variables from main data (recommended)", 
                                 bslib::tooltip(shiny::icon("info-circle"),
                                                "Variables to retain from the main data table. 
                                                Limit to necessary variables for computational
                                                efficiency. Note(1): if modeling multi-haul data, 
                                                be sure to include the lagged zone ID (previous 
                                                location) in this vector. Note(2): if using
                                                an Aux data table, the column that links the
                                                main data to the Aux table must be included.")), 
                       class = "card-title text-primary mb-2"),
                    selectizeInput(ns("select_vars_input"), NULL, 
                                   choices = NULL, multiple = TRUE, width = "100%",
                                   options = list(placeholder = "Default: All variables", 
                                                  plugins = list("remove_button")))
                  ),
                  div(
                    h6("Model Matrices", class = "card-title text-primary mb-2"),
                    selectizeInput(ns("alt_name_input"), 
                                   label = tags$span(
                                     "Alternative choice ",
                                     bslib::tooltip(shiny::icon("info-circle"), 
                                                    "Name of the alternative choice matrix
                                                    to use. Note: If you select an Expectations
                                                    matrix below, it must have been created using
                                                    this exact Alternative matrix.")), 
                                   choices = NULL, width = "100%"),
                    selectizeInput(ns("expectations_name_input"), 
                                   label = tags$span(
                                     "Expectations ",
                                     bslib::tooltip(shiny::icon("info-circle"), 
                                                    "Expected catch or revenue matrices to merge 
                                                    into the dataset. Important: This matrix must 
                                                    have been created using the specific 
                                                    Alternative choice matrix selected above.")), 
                                   choices = NULL, width = "100%")
                  )
                )
              ),
              
              # 2. External Data Integration
              bslib::card(
                class = "card-overflow",
                bslib::card_header(h5("2. External Data Integration", class = "mb-0")),
                bslib::card_body(
                  class = "card-overflow d-flex flex-column gap-3",
                  div(
                    class = "p-3 border rounded",
                    div(class="d-flex align-items-center mb-2",
                        shiny::icon("table", class="text-primary me-2"),
                        h6(tags$span(
                          "Auxiliary Data (optional) ",
                          bslib::tooltip(shiny::icon("info-circle"),
                                         "Select an auxiliary data table to join (e.g., vessel 
                                         characteristics). Note: The key variable linking this 
                                         table to your main dataset must be included in the 
                                         'Select variables' input.")),
                          class = "mb-0")),
                    selectInput(ns("aux_data"), NULL, choices = NULL, width = "100%")
                  ),
                  div(
                    class = "p-3 border rounded",
                    div(class="d-flex align-items-center mb-2",
                        shiny::icon("border-all", class="text-primary me-2"),
                        h6(tags$span(
                          "Gridded Data (optional) ", 
                          bslib::tooltip(shiny::icon("info-circle"),
                                         "Name of the gridded data table to join.")),
                          class = "mb-0")),
                    selectInput(ns("gridded_data"), NULL, choices = NULL, width = "100%"),
                    conditionalPanel(
                      condition = "input.gridded_data != ''",
                      ns = ns,
                      div(class = "mt-2 pt-2 border-top",
                          textInput(ns("grid_var_name"), 
                                    label = tags$span(
                                      "New Variable Name ",
                                      bslib::tooltip(shiny::icon("info-circle"),
                                                     "Name to use for the new variable 
                                                     representing the value in the gridded data.")), 
                                    placeholder = "e.g., sst_avg", width = "100%"))
                    )
                  )
                )
              ),
              
              # 3. Configuration
              bslib::card(
                fill = FALSE, 
                class = "card-overflow",
                bslib::card_header(h5("3. Configuration", class = "mb-0")),
                bslib::card_body(
                  class = "card-overflow d-flex flex-column",
                  h6("Data Cleaning", class = "card-title text-primary mb-3"),
                  selectInput(ns("impute_input"), 
                              label = tags$span(
                                "Missing Data Imputation ",
                                bslib::tooltip(shiny::icon("info-circle"), 
                                               "Method for dealing with NAs. 'Remove' drops zones 
                                               the dataset containing any missing values.")), 
                              choices = c("None" = "", "mean", "median", "mode", "remove"),
                              width = "100%"),
                  hr(),
                  div(
                    class = "d-flex justify-content-between align-items-center mb-2",
                    h6("Spatial Settings", class = "card-title text-primary mb-0"),
                    checkboxInput(ns("distance_input"), 
                                  label = tags$span(
                                    "Calculate Distance? ",
                                    bslib::tooltip(shiny::icon("info-circle"),
                                                   "Calculates and merges a distance matrix between
                                                   observations and zones.")), 
                                  value = TRUE)
                  ),
                  conditionalPanel(
                    condition = "input.distance_input == true",
                    ns = ns,
                    div(class = "p-3 bg-light border rounded",
                        selectInput(ns("distance_units_input"), 
                                    label = tags$span(
                                      "Units ", 
                                      bslib::tooltip(shiny::icon("info-circle"), 
                                                     "Units of measurement for distance 
                                                     calculations ('km' or 'mi').")), 
                                    choices = c("km", "mi"), width = "100%"),
                        textInput(ns("crs_input"), 
                                  label = tags$span(
                                    "CRS ", 
                                    bslib::tooltip(shiny::icon("info-circle"),
                                                   "Coordinate reference system for 
                                                   spatial calculations. Defaults to 4326 
                                                   if blank.")), 
                                  placeholder = "Default: 4326", 
                                  width = "100%"))
                  )
                )
              )
            ), 
            
            #  Run Button 
            fluidRow(
              column(6,
                     style = "margin-top: 25px;",
                     actionButton(ns("run_format_btn"), 
                                  "Run Format Data", 
                                  icon = icon("cogs"), 
                                  class = "btn-secondary",
                                  width = "100%")
              )
            ),
            
            # Spinner & Messages
            div(id = ns("run_format_spinner_container"),
                style = "display: none; margin-top: 15px;",
                spinner_ui(ns("run_format_spinner"),
                           spinner_type = "circle", size = "large",
                           message = "Formatting data...", overlay = TRUE)
            ),
            div(id = ns("format_success_message"), 
                style = "color: green; display: none; margin-top: 10px;",
                textOutput(ns("format_success_out"))),
            div(id = ns("format_error_message"), 
                style = "color: red; display: none; margin-top: 10px;", 
                textOutput(ns("format_error_out")))
          ) 
        ),
        
        # Manage Table
        bslib::card(
          class = "card-overflow",
          bslib::card_header("Manage Formatted Datasets"),
          bslib::card_body(
            class = "card-overflow",
            DT::dataTableOutput(ns("existing_data_table"), fill = FALSE),
            hr(),
            fluidRow(
              column(8,
                     selectizeInput(ns("data_to_remove"),
                                    "Select dataset to remove:", choices = NULL, width = "100%")
              ),
              column(4,
                     style = "margin-top: 25px;",
                     actionButton(ns("remove_data_btn"),
                                  "Remove Selected",
                                  icon = icon("trash"), 
                                  class = "btn-danger w-100")
              )
            )
          )
        )
    )
  )
}