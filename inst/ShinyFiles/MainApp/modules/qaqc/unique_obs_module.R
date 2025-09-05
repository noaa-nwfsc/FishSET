# =================================================================================================
# File: unique_obs_module.R
# Description: This module allows users to identify and remove duplicate observations found in
#              the primary data
#
# Authors: Anna Abelman, Paul Carvalho
# Date created: 9/2/2025
# Dependencies: shiny, DT
# Notes: This module is used within qaqc_module.R
# =================================================================================================

# Server ------------------------------------------------------------------------------------------
#' unique_obs_server
#'
#' @description Defines the server-side logic for the unique observations module. It populates a
#' DT::datatable that contains any duplicate rows found in the primary data table. When the user 
#' clicks the remove button, it shows a confirmation modal before deleting the rows.
#'
#' @param id A character string that is the namespace for this module.
#' @param rv_project_name A reactive value containing the name of the current project.
#' @param rv_data A reactive list containing the main dataset (`main`)
#'
#' @return
#' 
unique_obs_server <- function(id, rv_project_name, rv_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Initialize reactives
    rv_duplicate_message <- reactiveVal("") # Reactive value for NA messages
    
    # Create data frame containing duplicates only
    duplicated_rows <- reactive({
      req(rv_data)
      
      # Check that main data table exist and if not return nothing
      if (is.null(rv_data$main) || length(rv_data$main) == 0) {
        return()
        
      } else{
        rv_data$main %>% 
          filter(duplicated(.) | duplicated(., fromLast = TRUE)) %>%
          arrange(across(everything()))
      }
    })
    
    # Duplicate rows data table 
    output$duplicates_table <- DT::renderDT({
      
      # Check if any variables are selected
      if (is.null(duplicated_rows()) || length(duplicated_rows()) == 0) {
        shinyjs::show("unique_obs_no_message")
        return()
      }
      
      shinyjs::hide("unique_obs_no_message")
      
      duplicated_df <- duplicated_rows()
      
      if (nrow(duplicated_df) > 0) {
        datatable(
          duplicated_df,
          options = list(pageLength = 10, scrollX = TRUE),
          rownames = FALSE
        )
        
      } else {
        datatable(
          data.frame("Note"=  "No duplicate rows found in the dataset."),
          options = list(dom = 't'), # Hides search, pagination, etc.
          rownames = FALSE
        )
      }
    })
    
    # Show a confirmation modal when the remove button is clicked
    observeEvent(input$unique_obs_btn, {
      
      # Show the confirmation modal
      showModal(
        modalDialog(
          h5("Are you sure you want to remove these rows?"),
          p(strong("This action can only be undone if the entire dataset is refreshed to its 
                   original form.")),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("confirm_duplicate_btn"), "Yes, Remove Them", class = "btn-danger")
          ),
          easyClose = TRUE
        ))
    })
    
    # Perform the removal after confirmation
    observeEvent(input$confirm_duplicate_btn, {
      
      req(rv_project_name()) # Check to ensure reactive is available
      project_name <- rv_project_name()$value
      req(rv_data) # Ensure data is not null
      main_data <- rv_data$main # Save static copy of main data from reactive input
      
      # Using fishset function `unique_filter`
      removed_dup <- unique_filter(main_data, project = project_name, remove=TRUE)
      
      # Saving message to be shown in gui
      messages_to_show <- attr(removed_dup, "messages")
      rv_duplicate_message(paste(messages_to_show[-1], collapse = "<br>"))
      
      # Update the reactive data
      rv_data$main <- removed_dup
      
      # Save changes to SQLite database
      table_save(table = rv_data$main,
                 project = rv_project_name()$value,
                 type = "main")
      
      removeModal()
      
      shinyjs::show("duplicate_message_container")
    })
    
    # Render the message for unique observations
    output$rv_duplicate_message_out <- renderUI({
      HTML(rv_duplicate_message())
    })
    
  }
  )
}


# UI ----------------------------------------------------------------------------------------------
#' unique_obs_ui
#'
#' @description Creates the user interface for the unique observations module.
#'
#' @param id A character string that is the namespace for this module.
#'
#' @return A tagList containing the UI elements for the module.
#' 
unique_obs_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    bslib::card(
      height = "600px",
      fill = TRUE,
      bslib::card_header("Unique Observations"),
      p("The table below displays all rows that are exact duplicates of another row 
        in the dataset."),
      div(id = ns("unique_obs_no_message"),
          style = "color: grey; font-style: italic; font-size: 16px;",
          p("Data needs to be loaded first.")
      ),
      # Data table output to display the duplicated rows
      DT::DTOutput(ns("duplicates_table")),
    ),
    # An action button to confirm removing duplicate rows first
    actionButton(ns("unique_obs_btn"), 
                 label="Remove duplicates",
                 width = "25%",
                 icon = icon(name="clone",lib="font-awesome")
    ),
    # Messages created in function displayed for confirmation
    div(id = ns('duplicate_message_container'),
        style = "color: green; display: none; font-size: 16px;",
        uiOutput(ns("rv_duplicate_message_out")))
    
  )
}