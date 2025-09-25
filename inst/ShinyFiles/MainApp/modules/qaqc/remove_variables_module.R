# =================================================================================================
# File: remove_variables_module.R
# Description: This module allows users to select and remove one or more variables (columns)
#              from the main dataset.
#
# Authors: Paul Carvalho, Anna Abelman
# Date created: 8/28/2025
# Dependencies: shiny, DT
# Notes: This module is used within qaqc_module.R
# =================================================================================================

# Server ------------------------------------------------------------------------------------------
#' remove_variables_server
#'
#' @description Defines the server-side logic for the remove variables module. It populates a
#' multi-select dropdown with variables from the main dataset. When the user clicks
#' the remove button, it shows a confirmation modal before deleting the columns.
#'
#' @param id A character string that is the namespace for this module.
#' @param rv_project_name A reactive value containing the name of the current project.
#' @param rv_data A reactive list containing the main dataset (`main`) and spatial data (`spat`).
#' @param rv_folderpath A reactive value containing the file path to the project's root folder.
#'
#' @return
remove_variables_server <- function(id, rv_project_name, rv_data, rv_folderpath){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Update the selectInput with variable names from rv_data$main
    observe({
      req(rv_data$main)
      updateSelectInput(session, "select_vars_to_remove", choices = names(rv_data$main))
    })
    
    # Display the main data table
    output$main_data_table <- DT::renderDataTable({
      req(rv_data$main)
      DT::datatable(head(rv_data$main),
                    options = list(scrollX = TRUE),
                    rownames = FALSE)
    })
    
    # Show a confirmation modal when the remove button is clicked
    observeEvent(input$remove_vars_btn, {
      vars_to_remove <- input$select_vars_to_remove
      
      # Check if any variables are selected
      if (is.null(vars_to_remove) || length(vars_to_remove) == 0) {
        showModal(
          modalDialog(
            title = "Error: No variables selected",
            "Select one or more variables before clicking on remove variables",
            easyClose = TRUE
          ))
        return()
      }
      
      # Show the confirmation modal
      showModal(
        modalDialog(
          title = "Confirm Variable Removal",
          p("Are you sure you want to remove the following variable(s)?"),
          tags$ul(
            lapply(vars_to_remove, tags$li)
          ),
          p(strong("This action can only be undone if the entire dataset is refreshed to its 
                   original form.")),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("confirm_remove_btn"), "Yes, Remove Them", class = "btn-danger")
          ),
          easyClose = TRUE
        ))
    })
    
    # Perform the removal after confirmation
    observeEvent(input$confirm_remove_btn, {
      vars_to_remove <- input$select_vars_to_remove
      
      # Check if any removed variables are used in 'Select Variables' and update the RDS file
      try({
        # Load the saved GUI variables
        gui_vars <- load_gui_variables(rv_project_name()$value, rv_folderpath())
        
        # Use a vectorized approach to update the list
        updated_gui_vars <- lapply(gui_vars, function(category_list) {
          lapply(category_list, function(setting_value) {
            if (!is.null(setting_value) && setting_value %in% vars_to_remove) {
              NULL # Set to NULL if the variable is being removed
            } else {
              setting_value # Otherwise, keep the original value
            }
          })
        })
        
        # Only re-save the file if a change was made
        if (!identical(gui_vars, updated_gui_vars)) {
          # Construct the file path
          saved_var_file <- paste0(rv_project_name()$value, "SavedVariables.rds")
          saved_var_filepath <- file.path(loc_data(rv_project_name()$value), saved_var_file)
          saved_var_filepath <- suppressWarnings(normalizePath(saved_var_filepath))
          saveRDS(updated_gui_vars, file = saved_var_filepath)
        }
      }, silent = TRUE)
      
      # Create a copy and remove the columns
      current_data <- rv_data$main
      current_data <- current_data[, !names(current_data) %in% vars_to_remove, drop = FALSE]
      
      # Update the reactive data
      rv_data$main <- current_data
      
      # Save changes to SQLite database
      table_save(table = rv_data$main,
                 project = rv_project_name()$value,
                 type = "main")
      
      # Close the modal and show a notification
      removeModal()
    })
  })
}

# UI ----------------------------------------------------------------------------------------------
#' remove_variables_ui
#'
#' @description Creates the user interface for the remove variables module.
#'
#' @param id A character string that is the namespace for this module.
#'
#' @return A tagList containing the UI elements for the module.
remove_variables_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 6, # Use half of the 12-column grid layout
        selectInput(ns("select_vars_to_remove"),
                    label = tagList(
                      "Select variable(s) to remove:",
                      bslib::tooltip(
                        tags$span(icon("info-circle"), style = "margin-left: 5px;"),
                        "Select one or more variables to remove. The table below shows a 
                        preview of your current data.",
                        placement = "right"
                      )
                    ),
                    choices = NULL,
                    multiple = TRUE,
                    width = "100%"),
        
        actionButton(ns('remove_vars_btn'),
                     label = 'Remove Variable(s)', 
                     class = "btn-danger",
                     icon = icon("trash"),
                     width = "50%"),  
      )
    ),
    
    bslib::card(
      height = "600px",
      fill = TRUE,
      bslib::card_header("Current Main Data"),
      DT::DTOutput(ns("main_data_table"))
    )
  )
}
