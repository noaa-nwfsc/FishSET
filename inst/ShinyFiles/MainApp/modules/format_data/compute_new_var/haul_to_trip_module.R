# =================================================================================================
# File: haul_to_trip_module.R
# Description: Convert haul level data to trip level. It allows the user to specify aggregation
#              methods, preview the results, and provides a summary output.
#
# Authors: Paul Carvalho, Anna Abelman
# Date created: 10/17/2025
# Dependencies: shiny, DT, shinyjs, dplyr
# Notes: This module interacts with the main reactive data values (rv_data) and saved
#        project variables.
# =================================================================================================

# Server ------------------------------------------------------------------------------------------
#' haul_to_trip_server
#'
#' @description Server logic for the haul-to-trip module.
#'
#' @param id id A character string that is unique to this module instance.
#' @param rv_folderpath A reactive value containing the path to the project folder.
#' @param rv_project_name A reactive value containing the current project name.
#' @param rv_data A reactiveValues object containing the loaded data frames.
#'
#' @return This module does not return a value but modifies the main data frame (rv_data$main).
haul_to_trip_server <- function(id, rv_folderpath, rv_project_name, rv_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Reactive value to store the aggregated dataset for preview
    rv_trip_dat <- reactiveVal()
    
    # Update input choices based on main data
    observe({
      req(rv_data$main)
      choices <- colnames(rv_data$main)
      updateSelectInput(session, "trip_id_input", choices = choices)
    })
    
    # Handle "Aggregate to trips" button click
    observeEvent(input$aggregate_btn, {
      req(rv_project_name(), rv_data$main)
      project_name <- rv_project_name()$value
      folderpath <- rv_folderpath()
      
      # Show spinner
      shinyjs::show("h2t_spinner_container")
      on.exit(shinyjs::hide("h2t_spinner_container"), add = TRUE)
      
      # Load selected variables to get the zone ID
      selected_vars <- load_gui_variables(project_name, folderpath)
      if (is.null(selected_vars$main$main_zone_id)) {
        shinyjs::hide("h2t_spinner_container")
        showModal(modalDialog(
          title = "Error: Missing Zone ID",
          "The main zone ID has not been identified in the 'Select variables' tab. 
          Please ensure a zone variable is selected for the main data table.",
          easyClose = TRUE
        ))
        return()
      }
      
      # Input validation
      if (is.null(input$trip_id_input) || input$trip_id_input == "") {
        showNotification("Please select a unique trip ID variable.", type = "error")
        return()
      }
      
      tryCatch({
        dat_aggregated <- haul_to_trip(
          dat = rv_data$main,
          project = project_name,
          trip_id = input$trip_id_input,
          zoneID_dat = selected_vars$main$main_zone_id,
          zone_fun = input$zone_fun_input,
          date_fun = input$date_fun_input,
          num_fun = input$num_fun_input,
          char_fun = input$char_fun_input,
          haul_count = input$haul_count_input,
          log_fun = TRUE
        )
        
        rv_trip_dat(dat_aggregated)
        
        # Show modal with preview
        showModal(modalDialog(
          title = "Preview: Aggregated Trip Data",
          size = 'l',
          DT::dataTableOutput(ns("preview_table")),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("confirm_save_btn"), 
                         "Confirm and Save", 
                         class = "btn-primary")
          )
        ))
        
      }, error = function(e) {
        showNotification(paste("An error occurred:", e$message), type = "error", duration = 10)      
      })
    })
    
    # Render the preview table in the modal
    output$preview_table <- DT::renderDataTable({
      req(rv_trip_dat())
      DT::datatable(head(rv_trip_dat(), 20),
                    options = list(scrollX = TRUE, pageLength = 5),
                    rownames = FALSE)
    })
    
    # Handle the 'Confirm and Save' button
    observeEvent(input$confirm_save_btn, {
      # Update the main reactive data frame
      rv_data$main <- rv_trip_dat()
      
      # Save changes to SQLite database
      table_save(table = rv_data$main,
                 project = rv_project_name()$value,
                 type = "main")
      
      # Remove the modal
      removeModal()
      
      # Show a success message
      showNotification("Data successfully aggregated to trips and saved.", type = "message")
    })
    
    # Generate and render the summary table
    output$summary_table <- DT::renderDataTable({
      req(rv_trip_dat())
      summary_df <- data.frame(
        Description = c("Original number of hauls (rows)", "Aggregated number of trips (rows)"),
        Value = c(nrow(rv_data$main), nrow(rv_trip_dat()))
      )
      
      DT::datatable(summary_df,
                    options = list(dom = 't', searching = FALSE, paging = FALSE),
                    rownames = FALSE,
                    caption = "Summary: Aggregation Results")
    })
    
  })
}

# UI ----------------------------------------------------------------------------------------------
#' haul_to_trip_ui
#'
#' @description UI for the haul-to-trip module.
#'
#' @param id A character string that is unique to this module instance.
#'
#' @return A tagList containing the UI elements for the lag zone module.
haul_to_trip_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    shinyjs::useShinyjs(),
    bslib::card(
      class = "card-overflow",
      bslib::card_header("Aggregate Hauls to Trips"),
      bslib::card_body(
        class = "card-overflow",
        p("This function collapses the main data from individual hauls (rows) to unique trips, 
          based on a trip identifier. Choose the aggregation method for each data type."),
        hr(),
        
        h5(tagList(
          "1. Select trip ID column(s)",
          bslib::tooltip(
            shiny::icon("circle-info"),
            HTML("Select the variable(s) that identify unique trips. If multiple 
                 variables are selected, a composite ID will be created.")
          )
        )),
        
        fluidRow(
          column(6,
                 selectizeInput(
                   ns("trip_id_input"),
                   label = NULL,
                   choices = NULL, 
                   multiple = TRUE
                 )),
          column(6,
                 checkboxInput(ns("haul_count_input"), 
                               "Include haul count per trip?", 
                               value = TRUE))
        ),
        h5("2. Choose aggregation methods"),
        fluidRow(
          column(3,
                 selectInput(ns("num_fun_input"), "For Numeric Data:",
                             choices = c("mean", "median", "sum", "min", "max", "mode"),
                             selected = "mean")),
          column(3,
                 selectInput(ns("char_fun_input"), "For Character Data:",
                             choices = c("mode", "first", "last", "paste"),
                             selected = "mode")),
          column(3,
                 selectInput(ns("date_fun_input"), "For Date/Time Data:",
                             choices = c("min", "max", "mean", "median"),
                             selected = "min")),
          column(3,
                 selectInput(ns("zone_fun_input"), "For Zone ID:",
                             choices = c("mode", "first", "last"),
                             selected = "mode"))
        ),
        fluidRow(
          column(4,
                 style = "margin-top: 25px;",
                 actionButton(ns("aggregate_btn"),
                              "Aggregate to Trips",
                              icon = icon("cogs"),
                              class = "btn-secondary",
                              width = "100%")
          )
        ),
        
        # Spinner container
        div(id = ns("h2t_spinner_container"),
            style = "display: none;",
            spinner_ui(ns("h2t_spinner"),
                       spinner_type = "circle",
                       size = "large",
                       message = "Aggregating data...",
                       overlay = TRUE)
        )
      )
    ),
    
    bslib::card(
      bslib::card_header("Summary Output"),
      bslib::card_body(
        DT::dataTableOutput(ns("summary_table"))
      )
    )
  )
}
