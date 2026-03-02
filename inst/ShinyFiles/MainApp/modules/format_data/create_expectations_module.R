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
#' @param rv_shared_alt_names A reactiveVal passed from the main server to share alt matrix names.
#' @param rv_shared_exp_names A reactiveVal passed from the main server to share exp matrix names.
#'
#' @return This module does not return a value but saves an expectations matrix
#'         to the project's database.
create_expectations_server <- function(id, rv_folderpath, rv_project_name, rv_data,
                                       rv_shared_alt_names = NULL, rv_shared_exp_names = NULL){
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
      
    
      
      # Update the remove dropdown
      mat_names <- names(exp_mats)[!names(exp_mats) %in%
                                     c('scale', 'units') &
                                     !grepl("_dummy|_settings", names(exp_mats))]
      updateSelectizeInput(session, "matrix_to_remove", choices = mat_names, selected = "")
      
         # Update SHARED value (for the other module)
      if (!is.null(rv_shared_exp_names)) {
        rv_shared_exp_names(mat_names) 
      }
      
    }
    
    # Instead of querying the DB for alt names, we listen to the shared list
    observe({
      req(rv_shared_alt_names)
      
      # Get the names directly from the shared reactive
      current_names <- rv_shared_alt_names()
      
      # Update the dropdown immediately
      updateSelectizeInput(session, "alt_name_input", 
                           choices = current_names,
                           selected = input$alt_name_input)
    })
    
    # Load matrix data once when main data is first loaded
    observeEvent(rv_data$main, {
      req(rv_data$spat) # Ensure spatial data is available
      load_matrix_data()
    }, once = TRUE) # Only run this observer one time
    
    # Update input choices based on the main data
    observe({
      req(rv_data$main)
      choices <- colnames(rv_data$main)
      date_choices <- c("None", choices[sapply(rv_data$main, 
                                               function(x) inherits(x, c("Date", "POSIXt")))])
      numeric_choices <- c("None", choices[sapply(rv_data$main, is.numeric)])
      
      updateSelectizeInput(session, "catch_input", 
                           choices = choices)
      updateSelectizeInput(session, "price_input", 
                           choices = numeric_choices, selected = "None")
      updateSelectizeInput(session, "defineGroup_input", 
                           choices = c("None", choices), selected = "None")
      updateSelectizeInput(session, "temp_var_input", 
                           choices = date_choices, selected = "None")
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
      if (is.null(input$alt_name_input) || input$alt_name_input == "") {
        showNotification("Please select an Alternative Matrix (alt_name).", type = "error")
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
        if (is.null(val) || tolower(val) == "none" || is.na(val)) NULL else val
      }
      
      # Handle numeric/NULL conversion for empty_expectation
      empty_exp_val <- switch(input$empty_expectation_input,
                              "0.0001" = 1e-4,
                              "0" = 0)
      
      # --- Call create_expectations function ---
      tryCatch({
        create_expectations(
          dat = rv_data$main,
          project = project_name,
          name = input$name_input,
          alt_name = input$alt_name_input, 
          catch = input$catch_input,
          price = to_null(input$price_input),
          defineGroup = to_null(input$defineGroup_input),
          temp_var = input$temp_var_input,
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
      mat_names <- names(mats)[!names(mats) %in% c('scale', 'units') & 
                                 !grepl("_dummy|_settings", names(mats))]
      
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
                         class = "btn-sm btn-secondary")
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
      mat_names <- names(mats)[!names(mats) %in% c('scale', 'units') & 
                                 !grepl("_dummy|_settings", names(mats))]
      
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
    
    # --- Observe "Remove" button click (shows modal) ---
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
    
    # --- Observe "Confirm Remove" button click (does the work) ---
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
        if (length(names(mats_new)) > 2 || 
            (!"scale" %in% names(mats_new) && length(names(mats_new)) > 0)) {
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
        bslib::card_header("Create Expected Catch or Revenue Matrix"),
        bslib::card_body(
          class="card-overflow", 
          p("This function creates an expectation of catch or revenue for alternative fishing
             zones. The expectation matrix is saved to the FishSET project database and 
             multiple matrices can be saved (must have different names). An expected catch matrix 
             is required for the conditional logit model. For more information on calculating
             expected catch and revenue matrices, see ",
            tags$a(
              href = "https://noaa-nwfsc.github.io/FishSET/articles/FishSET_User_Manual.html",
              "section 8.2.3"),
            "in the FishSET User Manual"),
          
          fluidRow(
            # --- Group 1: Core Inputs ---
            column(
              6,
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
                    column(
                      6,
                      selectizeInput(
                        ns("alt_name_input"),
                        tagList(
                          span(
                            style = 
                              "white-space: wrap; display: inline-flex; align-items: center;",
                            HTML("Alternative Matrix (alt_name): &nbsp;"),
                            bslib::tooltip(
                              shiny::icon("circle-info", `aria-label` = "More information"),
                              HTML("Select the specific Alternative Choice matrix to match 
                                   against."),
                              options = list(delay = list(show = 0, hide = 850))
                            )
                          )
                        ),
                        choices =NULL, multiple = FALSE)
                    ),
                    column(
                      6,
                      selectizeInput(
                        ns("price_input"),
                        tagList(
                          span(
                            style = 
                              "white-space: wrap; display: inline-flex; align-items: center;",
                            HTML("Price variable (optional): &nbsp;"),
                            bslib::tooltip(
                              shiny::icon("circle-info", `aria-label` = "More information"),
                              HTML("This input is optional and used to calculate 
                                        expected revenue (price*catch). If revenue is included in
                                        the main data table, input a column of ones for catch."),
                              options = list(delay = list(show = 0, hide = 850))
                            )
                          )
                        ),
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
                         column(
                           6,
                           selectizeInput(
                             ns("temp_var_input"),
                             tagList(
                               span(
                                 style = 
                                   "white-space: wrap; display: inline-flex; align-items: center;",
                                 HTML("Temporal variable: &nbsp;"),
                                 bslib::tooltip(
                                   shiny::icon("circle-info", `aria-label` = "More information"),
                                   HTML("If NONE is selected, the expected catch will be set to
                                        the overall mean catch or revenue for each zone."),
                                   options = list(delay = list(show = 0, hide = 850))
                                 )
                               )
                             ),
                             choices = NULL, multiple = FALSE)
                         ),
                         column(
                           6,
                           selectizeInput(
                             ns("temporal_input"),
                             tagList(
                               span(
                                 style = 
                                   "white-space: wrap; display: inline-flex; align-items: center;",
                                 HTML("Temporal method: &nbsp;"),
                                 bslib::tooltip(
                                   shiny::icon("circle-info", `aria-label` = "More information"),
                                   HTML("This choice affects how the moving average is calculated.
                                        If 'daily', then the window size for the average and 
                                        temporal lag are in days (catch on dates wihtout fishing
                                        activity will be filled in with NAs, which will be ignored
                                        in the calculation of the average). If 'sequential',
                                        then averaging will occur over the specified number of 
                                        observations (dates with fishing activity). See Figure 8.1
                                        in the user manual for an example."),
                                   options = list(delay = list(show = 0, hide = 850))
                                 )
                               )
                             ),
                             choices = c("daily", "sequential"), selected = "daily")
                         )
                       ),
                       fluidRow(
                         column(6,
                                numericInput(ns("temp_window_input"),
                                             "Temporal window size (days/obs):",
                                             value = 7, min = 1)
                         ),
                         column(
                           6,
                           numericInput(
                             ns("day_lag_input"),
                             tagList(
                               span(
                                 style = 
                                   "white-space: wrap; display: inline-flex; align-items: center;",
                                 HTML("Temporal lag (days/obs): &nbsp;"),
                                 bslib::tooltip(
                                   shiny::icon("circle-info", `aria-label` = "More information"),
                                   HTML("The number of days (temporal method = 'daily') or
                                        observations (temporal method = 'sequential') to offset
                                        before the moving-window average is calculated, which 
                                        defines the position of the window average relative to the 
                                        current day/observation. For example, a zero-day lag will 
                                        include the current day/observation in the moving average 
                                        calculation, while a one-day lag defines a window
                                        where the last day/observation used in calculating the 
                                        average is the day/observation prior to the current one."),
                                   options = list(delay = list(show = 0, hide = 850))
                                 )
                               )
                             ),
                             value = 1, min = 0)
                         )
                       ),
                       fluidRow(
                         column(
                           6,
                           numericInput(
                             ns("year_lag_input"),
                             tagList(
                               span(
                                 style = 
                                   "white-space: wrap; display: inline-flex; align-items: center;",
                                 HTML("Year lag: &nbsp;"),
                                 bslib::tooltip(
                                   shiny::icon("circle-info", `aria-label` = "More information"),
                                   HTML("This input shifts the time window for averaging by 
                                        years. For example, if the current day/observation is 
                                        2024-01-03, year lag = 1 makes the base date 2023-01-03.
                                        Thus, a window size = 2, day lag = 0 calculates the average
                                        for the dates [2023-01-02, 2023-01-03]."),
                                   options = list(delay = list(show = 0, hide = 850))
                                 )
                               )
                             ),
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
                         column(
                           6,
                           selectizeInput(
                             ns("calc_method_input"),
                             tagList(
                               span(
                                 style = 
                                   "white-space: wrap; display: inline-flex; align-items: center;",
                                 HTML("Calculation method: &nbsp;"),
                                 bslib::tooltip(
                                   shiny::icon("circle-info", `aria-label` = "More information"),
                                   HTML("Method = 'standardAverage' calculates the simple 
                                        moving-window average for the specified time window. 
                                        Method = 'simpleLag' fits an autoregressive (AR) model for 
                                        catch or revenue of the specified window size. The AR model
                                        returns expected values over time by assuming the 
                                        expected current value is dependent on past values. When
                                        using the AR model, only the temporal variable, window
                                        size, and day lag inputs are required. The window size
                                        determines the number of previous days to include in 
                                        the regression."),
                                   options = list(delay = list(show = 0, hide = 850))
                                 )
                               )
                             ),
                             choices = c("standardAverage", "simpleLag"),
                             selected = "standardAverage")
                         ),
                         column(
                           6,
                           selectizeInput(
                             ns("defineGroup_input"),
                             tagList(
                               span(
                                 style = 
                                   "white-space: wrap; display: inline-flex; align-items: center;",
                                 HTML("Fleet grouping variable (optional): &nbsp;"),
                                 bslib::tooltip(
                                   shiny::icon("circle-info", `aria-label` = "More information"),
                                   HTML("Optional input variable from main data table that defines 
                                        how to split the fleet when calculating expected catch or 
                                        revenue. Selecting a grouping variable would result in
                                        expected catch or revenue for each zone, fleet combination. 
                                        The function defaults to treating entire table as a 
                                        single fleet."),
                                   options = list(delay = list(show = 0, hide = 850))
                                 )
                               )
                             ),
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
                         column(
                           6,
                           selectInput(
                             ns("empty_catch_input"),
                             tagList(
                               span(
                                 style = 
                                   "white-space: wrap; display: inline-flex; align-items: center;",
                                 HTML("Replace empty catch with: &nbsp;"),
                                 bslib::tooltip(
                                   shiny::icon("circle-info", `aria-label` = "More information"),
                                   HTML("This determines how empty catch should be handled. Note
                                        that NA values will be ignored in calculated expected
                                        values."),
                                   options = list(delay = list(show = 0, hide = 850))
                                 )
                               )
                             ),
                             choices = c("None (NA)" = "None", 
                                         "0" = "0", 
                                         "Mean of all catch" = "allCatch", 
                                         "Mean of grouped catch" = "groupCatch"),
                             selected = "None")
                         ),
                         column(
                           6,
                           selectInput(
                             ns("empty_expectation_input"),
                             tagList(
                               span(
                                 style = 
                                   "white-space: wrap; display: inline-flex; align-items: center;",
                                 HTML("Replace empty expectations with: &nbsp;"),
                                 bslib::tooltip(
                                   shiny::icon("circle-info", `aria-label` = "More information"),
                                   HTML("This determines how to handle empty expectations when
                                        there is no data within a time window for calculating
                                        the expectation. The default value 1e-4 is recommended
                                        for successful estimation in the modelling tab."),
                                   options = list(delay = list(show = 0, hide = 850))
                                 )
                               )
                             ),
                             choices = c("0.0001 (1e-4)" = "0.0001", 
                                         "0" = "0"),
                             selected = "0.0001")
                         )
                       ),
                       fluidRow(
                         column(
                           6,
                           style = "margin-top: 15px;",
                           checkboxInput(
                             ns("weight_avg_input"),
                             tagList(
                               span(
                                 style = 
                                   "white-space: wrap; display: inline-flex; align-items: center;",
                                 HTML("Weight daily avg? &nbsp;"),
                                 bslib::tooltip(
                                   shiny::icon("circle-info", `aria-label` = "More information"),
                                   HTML("Should the expected values be weighted by the number of
                                        observations on a given day? Selecting this option will
                                        include all observations for a given zone on a given day,
                                        thus giving more weight to days with more observations.
                                        If not selected (default) then the daily mean for each zone
                                        will be calculated prior to calculating the moving-window
                                        average."),
                                   options = list(delay = list(show = 0, hide = 850))
                                 )
                               )
                             ),
                             value = FALSE)
                         ),
                         column(
                           6,
                           style = "margin-top: 15px;",
                           checkboxInput(
                             ns("dummy_exp_input"),
                             tagList(
                               span(
                                 style = 
                                   "white-space: wrap; display: inline-flex; align-items: center;",
                                 HTML("Create dummy? &nbsp;"),
                                 bslib::tooltip(
                                   shiny::icon("circle-info", `aria-label` = "More information"),
                                   HTML("If selected (TRUE), the function adds a dummy variable
                                         indicating empty expectation values (1 if the expectation 
                                         is not empty, and 0 if empty). If not selected
                                         (default, FALSE), the dummy variable is not 
                                         generated."),
                                   options = list(delay = list(show = 0, hide = 850))
                                 )
                               )
                             ),
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