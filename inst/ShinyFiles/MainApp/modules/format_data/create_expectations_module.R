# =================================================================================================
# File: create_expectations_module.R
# Description: This module provides the UI and server logic for creating an expected
#              catch/revenue matrix using the create_expectations() function.
#
# Authors: Paul Carvalho, Anna Abelman
# Date created: 10/29/2025
# Dependencies: shiny, DT, shinyjs, bslib
# Notes: This module interacts with the main reactive data values (rv_data) and
#        project database.
# =================================================================================================


# Server ------------------------------------------------------------------------------------------
#' create_expectations_server
#'
#' @description Server logic for the create expectations module.
#'
#' @param id id A character string that is unique to this module instance.
#' @param rv_folderpath A reactive value containing the path to the project folder.
#' @param rv_project_name A reactive value containing the current project name.
#' @param rv_data A reactiveValues object containing the loaded data frames.
#'
#' @return This module does not return a value but saves an expectations matrix
#'         to the project's database.
create_expectations_server <- function(id, rv_folderpath, rv_project_name, rv_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Reactive value to store the full list of existing matrix data (names, settings, etc.)
    rv_existing_matrix_data <- reactiveVal(list())
    
    # Function to load existing matrix data
    load_matrix_data <- function() {
      req(rv_project_name())
      project <- rv_project_name()$value
      table_name <- paste0(project, "ExpectedCatch")
      
      exp_mats <- list() # Default empty list
      
      if (table_exists(table_name, project)) {
        tryCatch({
          exp_mats <- unserialize_table(table_name, project)
        }, error = function(e) {
          showNotification(paste("Error loading existing matrices:", e$message), type = "error")
          exp_mats <- list()
        })
      }
      
      # Update the reactive value
      rv_existing_matrix_data(exp_mats)
      
      # ALSO update the remove dropdown
      mat_names <- names(exp_mats)[!names(exp_mats) %in% c('scale', 'units') & !grepl("_dummy|_settings", names(exp_mats))]
      updateSelectizeInput(session, "matrix_to_remove", choices = mat_names, selected = "")
    }
    
    # Load matrix data once when main data is first loaded
    observeEvent(rv_data$main, {
      req(rv_data$spat) # Ensure all project data is available
      load_matrix_data()
    }, once = TRUE) # Only run this observer one time
    
    # Handle manual refresh button click
    observeEvent(input$refresh_matrices_btn, {
      load_matrix_data()
    })
    
    # Update input choices based on the main data
    observe({
      req(rv_data$main)
      choices <- colnames(rv_data$main)
      date_choices <- c("None", choices[sapply(rv_data$main, function(x) inherits(x, c("Date", "POSIXt")))])
      numeric_choices <- c("None", choices[sapply(rv_data$main, is.numeric)])
      
      updateSelectizeInput(session, "catch_input", choices = choices)
      updateSelectizeInput(session, "price_input", choices = numeric_choices, selected = "None")
      updateSelectizeInput(session, "defineGroup_input", choices = c("None", choices), selected = "None")
      updateSelectizeInput(session, "temp_var_input", choices = date_choices, selected = "None")
    })
    
    # Handle the 'Create Expectations Matrix' button click
    observeEvent(input$create_exp_btn, {
      req(rv_project_name(), rv_data$main)
      project_name <- rv_project_name()$value
      
      # --- Input Validation ---
      if (input$name_input == "") {
        showNotification("Please provide a name for the new matrix.", type = "error")
        return()
      }
      
      # Check against the full list of names
      if (input$name_input %in% names(rv_existing_matrix_data())) {
        showNotification("A matrix with this name already exists. Please choose a different name.", 
                         type = "error")
        return()
      }
      
      if (is.null(input$catch_input) || input$catch_input == "") {
        showNotification("Please select a catch variable.", type = "error")
        return()
      }
      
      # Show spinner
      shinyjs::show("create_exp_spinner_container")
      on.exit(shinyjs::hide("create_exp_spinner_container"), add = TRUE)
      
      # --- Prepare Function Arguments ---
      # Helper to convert "None" string to NULL
      to_null <- function(val) {
        if (is.null(val) || val == "None" || is.na(val)) NULL else val
      }
      
      # Handle numeric/NULL conversion for empty_expectation
      empty_exp_val <- switch(input$empty_expectation_input,
                              "0.0001" = 1e-4,
                              "0" = 0,
                              "NULL" = NULL)
      
      # --- Call create_expectations function ---
      tryCatch({
        create_expectations(
          dat = rv_data$main,
          project = project_name,
          name = input$name_input,
          catch = input$catch_input,
          price = to_null(input$price_input),
          defineGroup = to_null(input$defineGroup_input),
          temp_var = to_null(input$temp_var_input),
          temporal = input$temporal_input,
          calc_method = input$calc_method_input,
          temp_window = input$temp_window_input,
          day_lag = input$day_lag_input,
          year_lag = input$year_lag_input,
          empty_catch = to_null(input$empty_catch_input),
          empty_expectation = empty_exp_val,
          dummy_exp = input$dummy_exp_input,
          weight_avg = input$weight_avg_input,
          outsample = FALSE
        )
        
        # --- Handle Success ---
        showNotification("Expected catch/revenue matrix saved successfully.", type = "message")
        # Refresh the full list of matrix data
        load_matrix_data() 
        
      }, error = function(e) {
        # --- Handle Error ---
        showNotification(paste("An error occurred:", e$message), type = "error", duration = 10)
      })
    })
    
    # Render the summary table
    output$existing_matrices_table <- DT::renderDataTable({
      mats <- rv_existing_matrix_data()
      # Get only the base names of the matrices
      mat_names <- names(mats)[!names(mats) %in% c('scale', 'units') & !grepl("_dummy|_settings", names(mats))]
      
      if (length(mat_names) == 0) {
        return(DT::datatable(data.frame(Name = character(0), Settings = character(0)), 
                             caption = "No expectation matrices found."))
      }
      
      # Create a data frame with matrix names and buttons
      df <- data.frame(
        Name = mat_names,
        Settings = vapply(mat_names, function(name) {
          as.character(
            actionButton(ns(paste0("view_settings_", name)), 
                         "View Settings", 
                         class = "btn-sm btn-light")
          )
        }, FUN.VALUE = character(1))
      )
      
      DT::datatable(df,
                    options = list(pageLength = 5, 
                                   searching = FALSE, 
                                   dom = 'tip',
                                   columnDefs = list(list(orderable = FALSE, targets = 1))),
                    rownames = FALSE,
                    escape = FALSE, # This is crucial to render the button
                    selection = 'none',
                    caption = "Existing Expectation Matrices")
    })
    
    # Observe clicks on the table cells (for "View Settings")
    observeEvent(input$existing_matrices_table_cell_clicked, {
      info <- input$existing_matrices_table_cell_clicked
      
      # Check if the click is valid and in the "Settings" column (column index 1)
      if (is.null(info) || is.null(info$value) || info$col != 1) {
        return()
      }
      
      # Get the matrix names
      mats <- rv_existing_matrix_data()
      mat_names <- names(mats)[!names(mats) %in% c('scale', 'units') & !grepl("_dummy|_settings", names(mats))]
      
      # Get the name from the clicked row
      name <- mat_names[info$row]
      
      settings_name <- paste0(name, "_settings")
      settings_list <- mats[[settings_name]]
      
      # Create the UI for the modal
      settings_ui <- tagList(
        if (is.null(settings_list) || length(settings_list) == 0) {
          p("No settings information available for this matrix.")
        } else {
          # Convert the list of settings to a UI
          tags$table(class = "table table-sm table-striped",
                     tags$thead(
                       tags$tr(
                         tags$th("Parameter"),
                         tags$th("Value")
                       )
                     ),
                     tags$tbody(
                       lapply(names(settings_list), function(setting_name) {
                         setting_value <- settings_list[[setting_name]]
                         # Convert NULL to "NULL" string for display
                         if (is.null(setting_value)) {
                           setting_value <- "NULL"
                         }
                         tags$tr(
                           tags$td(tags$b(setting_name)),
                           tags$td(as.character(setting_value))
                         )
                       })
                     )
          )
        }
      )
      
      # Show the modal dialog
      showModal(modalDialog(
        title = paste("Settings for:", name),
        settings_ui,
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    })
    
    
    # --- ADDED: Observe "Remove" button click (shows modal) ---
    observeEvent(input$remove_matrix_btn, {
      matrix_name <- input$matrix_to_remove
      if (is.null(matrix_name) || matrix_name == "") {
        showNotification("Please select a matrix to remove.", type = "warning")
        return()
      }
      
      showModal(modalDialog(
        title = "Confirm Removal",
        paste("Are you sure you want to permanently remove the matrix:", matrix_name, "?"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_remove_btn"), "Remove", class = "btn-danger")
        )
      ))
    })
    
    # --- ADDED: Observe "Confirm Remove" button click (does the work) ---
    observeEvent(input$confirm_remove_btn, {
      matrix_name <- input$matrix_to_remove
      project_name <- rv_project_name()$value
      
      # Get current data
      mats <- rv_existing_matrix_data()
      
      # Find all associated names
      names_to_remove <- c(matrix_name, 
                           paste0(matrix_name, "_dummy"), 
                           paste0(matrix_name, "_settings"))
      
      # Filter the list
      mats_new <- mats[!names(mats) %in% names_to_remove]
      
      # Save back to database
      tryCatch({
        table_name <- paste0(project_name, "ExpectedCatch")
        fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project = project_name))
        on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
        
        # Remove the table first then save again
        table_remove(table_name, project_name)
        
        # Only re-create the table if there's still data left (i.e., not just 'scale' and 'units')
        if (length(names(mats_new)) > 2 || (!"scale" %in% names(mats_new) && length(names(mats_new)) > 0)) {
          DBI::dbExecute(fishset_db, 
                         paste("CREATE TABLE IF NOT EXISTS", 
                               table_name, 
                               "(data ExpectedCatch)")
          )
          DBI::dbExecute(fishset_db, 
                         paste("INSERT INTO", 
                               table_name, 
                               "VALUES (:data)"),
                         params = list(data = list(serialize(mats_new, NULL)))
          )
        }
        
        # Refresh reactive data
        load_matrix_data()
        removeModal()
        showNotification(paste("Matrix", matrix_name, "removed successfully."), type = "message")
        
      }, error = function(e) {
        removeModal()
        showNotification(paste("Error removing matrix:", e$message), type = "error", duration = 10)
      })
    })
    
  })
}

# UI ----------------------------------------------------------------------------------------------
#' create_expectations_ui
#'
#' @description UI for the create expectations module.
#'
#' @param id A character string that is unique to this module instance.
#'
#' @return A tagList containing the UI elements for the module.
create_expectations_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    shinyjs::useShinyjs(),
    
    div(
      id = ns("main_container"),
      bslib::card(
        class="card-overflow", 
        bslib::card_header("Create Expected Catch/Revenue Matrix"),
        bslib::card_body(
          class="card-overflow", 
          p("This function creates an expectation of catch or revenue for alternative fishing
             zones. The expectation matrix is saved to the FishSET project database and 
             multiple matrices can be saved (must have different names). An expected catch matrix 
             is required for the conditional logit model."),
          
          fluidRow(
            # --- Group 1: Core Inputs ---
            column(6,
                   bslib::card(
                     class="card-overflow",
                     height = "100%", # Make card fill its parent column
                     bslib::card_header(h5("1. Core Inputs", class = "mb-0")),
                     bslib::card_body(
                       class="card-overflow d-flex flex-column",
                       fluidRow(
                         column(6,
                                textInput(ns("name_input"),
                                          "Name for new matrix:",
                                          value = "exp_matrix_1")
                         ),
                         column(6,
                                selectizeInput(ns("catch_input"),
                                               "Catch variable:",
                                               choices = NULL, multiple = FALSE)
                         )
                       ),
                       fluidRow(
                         column(6,
                                selectizeInput(ns("price_input"),
                                               "Price variable (optional):",
                                               choices = NULL, multiple = FALSE)
                         )
                       )
                     )
                   )
            ),
            
            # --- Group 2: Temporal Settings ---
            column(6,
                   bslib::card(
                     class="card-overflow",
                     height = "100%", # Make card fill its parent column
                     bslib::card_header(h5("2. Temporal Settings", class = "mb-0")),
                     bslib::card_body(
                       class="card-overflow d-flex flex-column",
                       fluidRow(
                         column(6,
                                selectizeInput(ns("temp_var_input"),
                                               "Temporal variable (optional):",
                                               choices = NULL, multiple = FALSE)
                         ),
                         column(6,
                                selectInput(ns("temporal_input"), 
                                            "Temporal method:",
                                            choices = c("daily", "sequential"),
                                            selected = "daily")
                         )
                       ),
                       fluidRow(
                         column(6,
                                numericInput(ns("temp_window_input"),
                                             "Temporal window size (days/obs):",
                                             value = 7, min = 1)
                         ),
                         column(6,
                                numericInput(ns("day_lag_input"),
                                             "Temporal lag (days/obs):",
                                             value = 1, min = 0)
                         )
                       ),
                       fluidRow(
                         column(6,
                                numericInput(ns("year_lag_input"),
                                             "Year lag:",
                                             value = 0, min = 0)
                         )
                       )
                     )
                   )
            )
          ),
          
          fluidRow(
            # --- Group 3: Grouping & Calculation ---
            column(6,
                   bslib::card(
                     class="card-overflow",
                     height = "100%", # Make card fill its parent column
                     bslib::card_header(h5("3. Grouping & Calculation Method", class = "mb-0")),
                     bslib::card_body(
                       class="card-overflow d-flex flex-column",
                       fluidRow(
                         column(6,
                                selectInput(ns("calc_method_input"), 
                                            "Calculation method:",
                                            choices = c("standardAverage", "simpleLag", "weights"),
                                            selected = "standardAverage")
                         ),
                         column(6,
                                selectizeInput(ns("defineGroup_input"),
                                               "Fleet grouping variable (optional):",
                                               choices = NULL, multiple = FALSE)
                         )
                       )
                     )
                   )
            ),
            
            # --- Group 4: Data Handling & Output Options ---
            column(6,
                   bslib::card(
                     class="card-overflow",
                     height = "100%", # Make card fill its parent column
                     bslib::card_header(h5("4. Data Handling & Output Options", class = "mb-0")),
                     bslib::card_body(
                       class="card-overflow d-flex flex-column",
                       fluidRow(
                         column(6,
                                selectInput(ns("empty_catch_input"), 
                                            "Replace empty catch with:",
                                            choices = c("None (NA)" = "None", 
                                                        "0" = "0", 
                                                        "Mean of all catch" = "allCatch", 
                                                        "Mean of grouped catch" = "groupCatch"),
                                            selected = "None")
                         ),
                         column(6,
                                selectInput(ns("empty_expectation_input"), 
                                            "Replace empty expectations with:",
                                            choices = c("0.0001 (1e-4)" = "0.0001", 
                                                        "0" = "0", 
                                                        "Do not replace" = "NULL"),
                                            selected = "0.0001")
                         )
                       ),
                       fluidRow(
                         column(6,
                                style = "margin-top: 15px;",
                                checkboxInput(ns("weight_avg_input"), 
                                              "Weight daily avg?", 
                                              value = FALSE)
                         ),
                         column(6,
                                style = "margin-top: 15px;",
                                checkboxInput(ns("dummy_exp_input"), 
                                              "Create dummy?", 
                                              value = FALSE)
                         )
                       )
                     )
                   )
            )
          ),
          
          # --- Action Button ---
          fluidRow(
            column(6,
                   style = "margin-top: 25px;",
                   actionButton(ns("create_exp_btn"),
                                "Create Expectations",
                                icon = icon("cogs"),
                                class = "btn-secondary",
                                width = "100%")
            )
          ),
          
          # Spinner container
          div(id = ns("create_exp_spinner_container"),
              style = "display: none;",
              spinner_ui(ns("create_exp_spinner"),
                         spinner_type = "circle",
                         size = "large",
                         message = "Creating expectations matrix...",
                         overlay = TRUE)
          )
        )
      ),
      
      bslib::card(
        bslib::card_header(
          "Manage Expected Catch/Revenue Matrices",
          actionButton(ns("refresh_matrices_btn"), 
                       "Refresh", 
                       icon = icon("sync"), 
                       class = "btn-sm btn-light float-end")
        ),
        bslib::card_body(
          class="card-overflow",
          DT::dataTableOutput(ns("existing_matrices_table")),
          hr(),
          fluidRow(
            column(8,
                   selectizeInput(ns("matrix_to_remove"),
                                  "Select matrix to remove:",
                                  choices = NULL)
            ),
            column(4,
                   style = "margin-top: 25px;",
                   actionButton(ns("remove_matrix_btn"),
                                "Remove Selected",
                                icon = icon("trash"),
                                class = "btn-danger w-100")
            )
          ),
        )
      )
    )
  )
}