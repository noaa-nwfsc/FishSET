# =================================================================================================
# File: preview_data_module.R
# Description: This module displays an interactive table to view loaded data tables. It includes
#              a dropdown menu to select a table to view.
#
# Authors: Paul Carvalho, Anna Abelman
# Date created: 8/4/2025
# Dependencies: shiny, DT
# Notes: This module is used within qaqc_module.R
# =================================================================================================

# Server ------------------------------------------------------------------------------------------
#' preview_data_server
#'
#' @description Defines the server-side logic for the data preview module. It populates
#' a dropdown with available data frames and renders the selected data frame in an
#  interactive DT::datatable.
#'
#' @param id A character string that is unique to this module instance.
#' @param rv_project_name A reactive value containing the current project name.
#' @param rv_data A reactiveValues object containing the list of data frames to be displayed.
#'
#' @return This module does not return a value.
preview_data_server <- function(id, rv_project_name, rv_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Observe to dynamically update choice in the preview data table dropdown.
    observe({
      data_list <- reactiveValuesToList(rv_data)
      
      # If the data list is empty or NULL, clear the choices and exit.
      if (is.null(data_list) || length(data_list) == 0) {
        updateSelectInput(session, "select_data_input", choices = character(0)) # Clear choices
        return()
      }
      
      # Filter the list to get only the names of objects that are data frames
      df_names <- names(data_list)[sapply(data_list, is.data.frame)]
      
      # Add map for display names in the dropdown menu
      display_name_map <- c(
        "main" = "Main Data",
        "port" = "Port Data",
        "aux" = "Auxiliary Data",
        "spat" = "Spatial Grid",
        "grid" = "Grid Data"
      )
      
      # Set display labels
      display_labels <- display_name_map[df_names]
      choices_vec <- df_names
      names(choices_vec) <- display_labels
      
      # Update input options
      updateSelectInput(session, "select_data_input", choices = choices_vec)
    })
    
    # Render interactive data table
    output$preview_datatable <- DT::renderDT({
      req(input$select_data_input)
      
      # Get the df to display based on input
      df_to_display <- rv_data[[input$select_data_input]]
      
      # Require input from user
      req(df_to_display)
      
      # If spat is selected, remove the geometry column
      if (inherits(df_to_display, "sf")) {
        df_to_display <- as.data.frame(df_to_display)
        col_index <- which(tolower(names(df_to_display)) == "geometry")
        df_to_display <- df_to_display[, -col_index]
      }
      
      DT::datatable(
        df_to_display,
        rownames = FALSE,
        filter = "top",
        options = list(
          scrollX = TRUE
        )
      )
    })
  })
}

# UI ----------------------------------------------------------------------------------------------
#' preview_data_ui
#'
#' @description Creates the user interface for the data preview module. This includes
#' a dropdown for data selection and a placeholder for the data table.
#'
#' @param id A character string that is unique to this module instance.
#'
#' @return A tagList containing the UI elements for the preview data module.
preview_data_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    bslib::card(
      height = "1000px",
      fill = TRUE, 
      selectInput(ns("select_data_input"),
                  label = "Select data to view:",
                  choices = NULL),
      DT::DTOutput(ns("preview_datatable"))
    )
  )
}
