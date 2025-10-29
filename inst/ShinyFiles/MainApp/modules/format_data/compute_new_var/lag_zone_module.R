# =================================================================================================
# File: lag_zone_module.R
# Description: This module provides the UI and server logic for creating a lagged zone variable.
#              It checks for necessary port data, allows users to specify parameters, previews
#              the new variable, and provides a summary of the results.
#
# Authors: Paul Carvalho, Anna Abelman
# Date created: 10/10/2025
# Dependencies: shiny, DT, shinyjs, dplyr
# Notes: This module interacts with the main reactive data values (rv_data) and saved
#        project variables.
# =================================================================================================


# Server ------------------------------------------------------------------------------------------
#' lag_zone_server
#'
#' @description Server logic for the lag zone module.
#'
#' @param id id A character string that is unique to this module instance.
#' @param rv_folderpath A reactive value containing the path to the project folder.
#' @param rv_project_name A reactive value containing the current project name.
#' @param rv_data A reactiveValues object containing the loaded data frames.
#'
#' @return This module does not return a value but modifies the main data frame (rv_data$main).
lag_zone_server <- function(id, rv_folderpath, rv_project_name, rv_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Reactive value to store the dataset with the new lagged variable for preview
    rv_lag_zone_dat <- reactiveVal()
    rv_selected_vars <- reactiveValues(vars = NULL)
    
    # Check for prerequisites (port data and selected port variables) 
    observe({
      # Check if port data is loaded
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
    })
    
    # Handle the 'Create lagged zone' button click 
    observeEvent(input$create_lag_zone_btn, {
      req(rv_project_name(), rv_data$main, rv_data$spat, rv_data$port)
      req(rv_project_name())
      project_name <- rv_project_name()$value
      folderpath <- rv_folderpath()
      
      # Show spinner
      shinyjs::show("lag_zone_spinner_container")
      on.exit(shinyjs::hide("lag_zone_spinner_container"), add = TRUE)
      
      # Load selected variables
      selected_vars <- load_gui_variables(project_name, folderpath)
      if (is.null(selected_vars)) {
        # Handle the case where the RDS file does not exist.
        shinyjs::hide("lag_zone_spinner_container")
        showModal(modalDialog(
          title = "Error: Missing Data",
          "The selected variables file for the current project could not be found. 
          Please ensure you have selected and saved variables in the previous tab.",
          easyClose = TRUE
        ))
        return() # Stop execution of the observer
      }
      
      # Check specifically for port name, longitude, and latitude variables
      if (is.null(selected_vars$port$port_name) ||
          is.null(selected_vars$port$port_lon) ||
          is.null(selected_vars$port$port_lat)) {
        # Handle the case where port variables have not been selected.
        shinyjs::hide("lag_zone_spinner_container")
        showModal(modalDialog(
          title = "Error: Missing variables",
          "The port-name, -latitude, or -longitude have not been identified in the select variables 
          tab. Please ensure you have selected and saved variables in the port table.",
          easyClose = TRUE
        ))
        return() # Stop execution of the observer
      } 
      
      # Input validation
      if (input$lag_zone_name_input == "") {
        showNotification("Please provide a name for the new variable.", 
                         type = "error")
        return()
      }
      
      if (input$lag_zone_name_input %in% colnames(rv_data$main)) {
        showNotification("Column name already exists. Please choose a different name.", 
                         type = "error")
        return()
      }
      
      # Run lag_zone function and show preview
      tryCatch({
        dat_with_lag <- lag_zone(
          dat = rv_data$main,
          project = rv_project_name()$value,
          spat = rv_data$spat,
          port = rv_data$port,
          port_name = selected_vars$port$port_name,
          port_lon = selected_vars$port$port_lon,
          port_lat = selected_vars$port$port_lat,
          trip_id = input$trip_id_input,
          haul_order = input$haul_order_input,
          starting_port = input$starting_port_input,
          zoneID_dat = selected_vars$main$main_zone_id,
          zoneID_spat = selected_vars$spat$spat_zone_id,
          name = input$lag_zone_name_input)
        
        rv_lag_zone_dat(dat_with_lag)
        
        # Hide spinner and show modal
        shinyjs::hide("lag_zone_spinner_container")
        
        # Show modal with preview
        showModal(modalDialog(
          title = "Preview: Lagged Zone Variable",
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
      req(rv_lag_zone_dat())
      DT::datatable(head(rv_lag_zone_dat()),
                    options = list(scrollX = TRUE, pageLength = 5))
    })
    
    # Handle the 'Confirm and Save' button click
    observeEvent(input$confirm_save_btn, {
      # Update the main reactive data frame
      rv_data$main <- rv_lag_zone_dat()
      
      # Save changes to SQLite database
      table_save(table = rv_data$main,
                 project = rv_project_name()$value,
                 type = "main")
      
      # Remove the modal
      removeModal()
      
      # Show a success message
      showNotification("Lagged zone variable successfully created and saved.", type = "message")
    })
    
    # Generate and render the summary table
    output$summary_table <- DT::renderDataTable({
      req(rv_data$main, input$starting_port_input)
      
      # Ensure the starting port column exists before summarizing
      if (input$starting_port_input %in% names(rv_data$main)) {
        summary_df <- rv_data$main %>%
          dplyr::group_by(!!sym(input$starting_port_input)) %>%
          dplyr::summarise(Observations = n(), .groups = 'drop')
        
        DT::datatable(summary_df,
                      options = list(pageLength = 10, searching = FALSE),
                      caption = "Summary: Observations per Starting Port")
      }
    })
  })
}

# UI ----------------------------------------------------------------------------------------------
#' lag_zone_ui
#'
#' @description UI for the lag zone module.
#'
#' @param id A character string that is unique to this module instance.
#'
#' @return A tagList containing the UI elements for the lag zone module.
lag_zone_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    shinyjs::useShinyjs(),
    
    # Error message if port data is not available
    shinyjs::hidden(
      div(
        id = ns("port_error_msg"),
        class = "alert alert-danger",
        "Port data table is required for this function. Please load a port table and select the 
         required port variables in the 'Upload Data' -> 'Select variables' tab."
      )
    ),
    
    # Main UI container for inputs and outputs
    shinyjs::hidden(
      div(
        id = ns("main_container"),
        bslib::card(
          class="card-overflow", 
          bslib::card_header("Create a Lagged Zone Variable"),
          bslib::card_body(
            class="card-overflow", 
            p("The lagged zone function processes haul-level data to derive a new variable: the 
              location of the immediately preceding fishing event for each haul. For the first haul
              of every trip, the variable is assigned the location of the port of departure. This
              derived lagged variable then serves as the 'Starting location' when specifying the 
              set of alternatives in the 'Define alternatives' tab."),
            hr(),
            fluidRow(
              column(6,
                     textInput(ns("lag_zone_name_input"),
                               span("1. Enter a name for the new lagged zone variable:", 
                                    style = "white-space: nowrap;"),
                               value = "lagged_zone_id")
              ),
              column(6,
                     selectizeInput(
                       ns("trip_id_input"),
                       tagList(
                         span(
                           style = "white-space: wrap; display: inline-flex; align-items: center;",
                           HTML("2. Select the unique trip ID: &nbsp;"),
                           bslib::tooltip(
                             shiny::icon("circle-info", `aria-label` = "More information"),
                             HTML("Select the variable that identifies unique trips. Note
                                  that hauls/sets within the same trip should share trips IDs."),
                             options = list(delay = list(show = 0, hide = 850))
                           )
                         )
                       ),
                       choices = NULL, multiple = FALSE)
              )
            ),
            
            fluidRow(
              column(6,
                     selectizeInput(
                       ns("haul_order_input"),
                       tagList(
                         span(
                           style = "white-space: wrap; display: inline-flex; align-items: center;",
                           HTML("3. Select the haul order variable: &nbsp;"),
                           bslib::tooltip(
                             shiny::icon("circle-info", `aria-label` = "More information"),
                             HTML("Select the variable that indicates the sequential order of
                                  hauls/sets within a trip. If this is a haul ID variable, ensure
                                  that haul order within trips are sorted correctly when 
                                  lagging the zone ID."),
                             options = list(delay = list(show = 0, hide = 850))
                           )
                         )
                       ),
                       choices = NULL, multiple = FALSE)
              ),
              column(6,
                     selectizeInput(
                       ns("starting_port_input"),
                       tagList(
                         span(
                           style = "white-space: wrap; display: inline-flex; align-items: center;",
                           HTML("4. Select the starting port variable: &nbsp;"),
                           bslib::tooltip(
                             shiny::icon("circle-info", `aria-label` = "More information"),
                             HTML("Note: a port table is required to lag the zone ID variable. 
                                  The starting port should represent a port name than can
                                  be linked to the port table."),
                             options = list(delay = list(show = 0, hide = 850))
                           )
                         )
                       ),
                       choices = NULL, multiple = FALSE)
              )
            ),
            
            fluidRow(
              column(4,
                     style = "margin-top: 25px;",
                     actionButton(ns("create_lag_zone_btn"),
                                  "Create Lagged Zone",
                                  icon = icon("cogs"),
                                  class = "btn-secondary",
                                  width = "100%")
              )
            ),
            
            # Spinner container
            div(id = ns("lag_zone_spinner_container"),
                style = "display: none;",
                spinner_ui(ns("lag_zone_spinner"),
                           spinner_type = "circle",
                           size = "large",
                           message = "Creating lagged zone variable...",
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
    )
  )
}
