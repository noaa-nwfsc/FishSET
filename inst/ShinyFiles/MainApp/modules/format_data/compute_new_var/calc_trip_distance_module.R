# =================================================================================================
# File: calc_trip_distance_module.R
# Description: This module provides the UI and server logic for calculating total trip distance.
#              It checks for necessary data, allows users to specify parameters, previews
#              the new variable, and provides a summary of the results.
#
# Authors: Paul Carvalho, Anna Abelman
# Date created: 10/23/2025
# Dependencies: shiny, DT, shinyjs, dplyr
# Notes: This module interacts with the main reactive data values (rv_data) and saved
#        project variables.
# =================================================================================================


# Server ------------------------------------------------------------------------------------------
#' calc_trip_distance_server
#'
#' @description Server logic for the calculate trip distance module.
#'
#' @param id A character string that is unique to this module instance.
#' @param rv_folderpath A reactive value containing the path to the project folder.
#' @param rv_project_name A reactive value containing the current project name.
#' @param rv_data A reactiveValues object containing the loaded data frames.
#'
#' @return This module does not return a value but modifies the main data frame (rv_data$main).
calc_trip_distance_server <- function(id, rv_folderpath, rv_project_name, rv_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Reactive value to store the dataset with the new distance variable for preview
    rv_trip_dist_dat <- reactiveVal()
    
    # Check for prerequisites (port data)
    observe({
      if (is.null(rv_data$port)) {
        shinyjs::hide("main_container")
        shinyjs::show("port_error_msg")
      } else {
        shinyjs::show("main_container")
        shinyjs::hide("port_error_msg")
      }
    })
    
    # Update input choices based on the main data
    observe({
      req(rv_data$main)
      choices <- colnames(rv_data$main)
      updateSelectInput(session, "trip_id_input", choices = choices)
      updateSelectInput(session, "haul_order_input", choices = choices)
      updateSelectInput(session, "starting_port_input", choices = choices)
      updateSelectInput(session, "return_port_input", choices = choices)
      updateSelectInput(session, "start_haul_lat_input", choices = choices)
      updateSelectInput(session, "start_haul_lon_input", choices = choices)
      updateSelectInput(session, "end_haul_lat_input", choices = choices)
      updateSelectInput(session, "end_haul_lon_input", choices = choices)
    })
    
    # Handle the 'Calculate Trip Distance' button click
    observeEvent(input$calculate_dist_btn, {
      # UPDATED: Add new coordinate inputs to req()
      req(rv_project_name(), rv_data$main, rv_data$port,
          input$trip_id_input,
          input$haul_order_input,
          input$starting_port_input,
          input$return_port_input,
          input$start_haul_lat_input,
          input$start_haul_lon_input,
          input$end_haul_lat_input,
          input$end_haul_lon_input)
      
      project_name <- rv_project_name()$value
      folderpath <- rv_folderpath()
      
      # Show spinner
      shinyjs::show("dist_spinner_container")
      on.exit(shinyjs::hide("dist_spinner_container"), add = TRUE)
      
      # Load selected variables
      selected_vars <- load_gui_variables(project_name, folderpath)
      if (is.null(selected_vars)) {
        showModal(modalDialog(
          title = "Error: Missing Selections",
          "The selected variables file could not be found. Please select and save variables in 
          the 'Select variables' tab.",
          easyClose = TRUE
        ))
        return()
      }
      
      # Check only for required pre-selected port variables
      required_vars <- with(selected_vars, c(port$port_name, port$port_lon, port$port_lat))
      if (any(sapply(required_vars, is.null))) {
        showModal(modalDialog(
          title = "Error: Missing Required Port Variables",
          "Port name, latitude, or longitude have not been identified in the 'Select variables' 
          tab. Please ensure these port variables are selected and saved.",
          easyClose = TRUE
        ))
        return()
      }
      
      # Input validation
      if (input$dist_name_input == "") {
        showNotification("Please provide a name for the new variable.", type = "error")
        return()
      }
      if (input$dist_name_input %in% colnames(rv_data$main)) {
        showNotification("Column name already exists. Please choose a different name.", 
                         type = "error")
        return()
      }
      
      # Run calc_trip_distance function and show preview
      tryCatch({
        dat_with_dist <- calc_trip_distance(
          dat = rv_data$main,
          project = project_name,
          port = rv_data$port,
          name = input$dist_name_input,
          trip_id = input$trip_id_input,
          haul_order = input$haul_order_input,
          starting_port = input$starting_port_input,
          return_port = input$return_port_input,
          start_haul_lat = input$start_haul_lat_input,
          start_haul_lon = input$start_haul_lon_input,
          end_haul_lat = input$end_haul_lat_input,
          end_haul_lon = input$end_haul_lon_input,
          distance_unit = input$unit_input
        )
        
        rv_trip_dist_dat(dat_with_dist)
        
        showModal(modalDialog(
          title = "Preview: New Trip Distance Variable",
          size = "l",
          DT::dataTableOutput(ns("preview_table")),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("confirm_save_btn"), "Confirm and Save", class = "btn-primary")
          )
        ))
      }, error = function(e) {
        showNotification(paste("An error occurred:", e$message), type = "error", duration = 10)
      })
    })
    
    # Render the preview table in the modal
    output$preview_table <- DT::renderDataTable({
      req(rv_trip_dist_dat())
      DT::datatable(head(rv_trip_dist_dat(), 50),
                    options = list(scrollX = TRUE, pageLength = 5))
    })
    
    # Handle the 'Confirm and Save' button click
    observeEvent(input$confirm_save_btn, {
      rv_data$main <- rv_trip_dist_dat()
      table_save(table = rv_data$main, project = rv_project_name()$value, type = "main")
      removeModal()
      showNotification("Trip distance variable successfully created and saved.", type = "message")
      shinyjs::show("summary_card")
    })
    
    # Generate and render the summary table
    output$summary_table <- DT::renderDataTable({
      req(rv_trip_dist_dat())
      
      dist_col_name <- isolate(input$dist_name_input)
      trip_id_col <- isolate(input$trip_id_input) # Use isolate to prevent premature trigger
      
      if (dist_col_name %in% names(rv_trip_dist_dat()) && 
          trip_id_col %in% names(rv_trip_dist_dat())) {
        summary_df <- rv_trip_dist_dat() %>%
          dplyr::distinct(!!sym(trip_id_col), .keep_all = TRUE) %>%
          dplyr::summarise(
            "Total Trips" = n(),
            "Min Distance" = min(!!sym(dist_col_name), na.rm = TRUE),
            "Mean Distance" = mean(!!sym(dist_col_name), na.rm = TRUE),
            "Max Distance" = max(!!sym(dist_col_name), na.rm = TRUE)
          ) %>%
          round(2)
        
        DT::datatable(summary_df,
                      options = list(dom = 't', searching = FALSE, paging = FALSE),
                      rownames = FALSE,
                      caption = paste0("Summary: Calculated Trip Distances (in ", 
                                       isolate(input$unit_input), ")"))
      }
    })
  })
}

# UI ----------------------------------------------------------------------------------------------
#' calc_trip_distance_ui
#'
#' @description UI for the calculate trip distance module.
#'
#' @param id A character string that is unique to this module instance.
#'
#' @return A tagList containing the UI elements for the module.
calc_trip_distance_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    shinyjs::useShinyjs(),
    
    # Error message if port data is not available
    shinyjs::hidden(
      div(
        id = ns("port_error_msg"),
        class = "alert alert-danger",
        "A port data table is required for this function. Please load a port table and select the 
        required port variables in the 'Upload Data' -> 'Select variables' tab."
      )
    ),
    
    # Main UI container
    shinyjs::hidden(
      div(
        id = ns("main_container"),
        bslib::card(
          class = "card-overflow",
          bslib::card_header("Calculate Total Trip Distance"),
          bslib::card_body(
            class = "card-overflow",
            p("This function calculates the total distance of a fishing trip by summing the 
              distance from port to the first haul, the distance within and between all hauls, 
              and the distance from the last haul back to port. Note that port coordinate 
              variables must be pre-selected in the 'Select variables' tab."),
            hr(),
            fluidRow(
              column(4, 
                     textInput(ns("dist_name_input"), "1. New variable name:", 
                               value = "trip_distance")),
              column(4, 
                     selectizeInput(ns("trip_id_input"), "2. Unique trip ID:", choices = NULL)),
              column(4, 
                     selectizeInput(ns("haul_order_input"), "3. Haul order:", choices = NULL))
            ),
            fluidRow(
              column(4, 
                     selectizeInput(ns("starting_port_input"), "4. Starting port:", 
                                    choices = NULL)),
              column(4, 
                     selectizeInput(ns("return_port_input"), "5. Return port:", 
                                    choices = NULL)),
              column(4, 
                     selectizeInput(ns("start_haul_lat_input"), "6. Start Haul Latitude:", 
                                    choices = NULL))
            ),
            fluidRow(
              column(4, 
                     selectizeInput(ns("start_haul_lon_input"), "7. Start Haul Longitude:", 
                                    choices = NULL)),
              column(4, 
                     selectizeInput(ns("end_haul_lat_input"), "8. End Haul Latitude:", 
                                    choices = NULL)),
              column(4, 
                     selectizeInput(ns("end_haul_lon_input"), "9. End Haul Longitude:", 
                                    choices = NULL))
            ),
            fluidRow(
              column(4, 
                     selectInput(ns("unit_input"), "10. Distance unit:", 
                                 choices = c("miles", "kilometers", "meters"), selected = "miles"))
            ),
            fluidRow(
              column(4,
                     style = "margin-top: 25px;",
                     actionButton(ns("calculate_dist_btn"), 
                                  "Calculate Trip Distance", 
                                  icon = icon("ruler-combined"), 
                                  class = "btn-secondary", 
                                  width = "100%")
              )
            ),
            # Spinner container
            div(id = ns("dist_spinner_container"), style = "display: none;",
                spinner_ui(ns("dist_spinner"), spinner_type = "circle", size = "large",
                           message = "Calculating trip distances...", overlay = TRUE)
            )
          )
        ),
        
        shinyjs::hidden(
          div(
            id = ns("summary_card"),
            bslib::card(
              bslib::card_header("Summary Output"),
              bslib::card_body(
                DT::dataTableOutput(ns("summary_table"))
              )
            )    
          )
        )
      )
    )
  )
}