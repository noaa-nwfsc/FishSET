# =================================================================================================
# File: remove_na_nans_module.R
# Description: This module is to remove/replace NA or NaN values found in the primary data table.
#
# Authors: Anna Abelman, Paul Carvalho 
# Date created: 8/7/2025
# Dependencies: shiny, DT
# Notes: This module is used within qaqc_module.R
# =================================================================================================

# Server ------------------------------------------------------------------------------------------
#' summary_data_server
#'
#' @description Defines the server-side logic for removing NA/NaN values module. It renders
#' an interactive DT::datatable that displays any variables that contain these missing values or
#' non-numbers and counts them. The user can then chose to remove the values or replace them
#' with the mean
#'
#' @param id A character string that is unique to this module instance.
#' @param rv_project_name A reactive value containing the current project name.
#' @param rv_data A reactiveValues object containing the list of data frames to be displayed.
#'
#' @return This module does not return a value.

remove_na_nan_server <- function(id, rv_project_name, rv_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Initialize reactives
    rv_na_filter_message <- reactiveVal("") # Reactive value for NA messages
    rv_nan_filter_message <- reactiveVal("") # Reactive value for NaN messages
    rv_na_action_taken <- reactiveVal(FALSE) # Reactive value indicating whether remove/replace was 
                                          # clicked for NAs
    rv_nan_action_taken<- reactiveVal(FALSE)# Reactive value indicating whether remove/replace was 
                                          # clicked for NaNs

    # Data frame of variables with NAs and counts
    na_df <- reactive({
      req(rv_data) # Ensure data is not null
      main_data <- rv_data$main # Save static copy of main data from reactive input
      
      na_cols <- qaqc_helper(main_data, "NA", "names")
      if (is_value_empty(na_cols)) {
        # Return an empty data frame or NULL if no NAs
        return(NULL) 
      } else{
        
        main_data_na <- main_data %>% 
          select(all_of(c(na_cols))) %>% 
          summarise(across(everything(), ~sum(is.na(.) & !is.nan(.)))) %>% 
          pivot_longer(., cols = everything(),
            names_to="Variable",
            values_to = "NA Count")
        
        return(main_data_na)
      }
    })
    
    # Data frame of variables with NaNs and counts
    nan_df <- reactive({
      req(rv_data) # Ensure data is not null
      main_data <- rv_data$main # Save static copy of main data from reactive input
      
      nan_cols <- qaqc_helper(main_data, "NaN", "names")
      if (is_value_empty(nan_cols)) {
        # Return an empty data frame or NULL if no NaNs
        return(NULL) 
      }  else{
        
        main_data_nan <- main_data %>% 
          select(all_of(c(nan_cols))) %>% 
          summarise(across(everything(), ~sum(is.nan(.)))) %>% 
          pivot_longer(., cols = everything(),
            names_to="Variable",
            values_to = "NaN Count")
        
        return(main_data_nan)
      }
    })
    
    # Handling messages whether none found or after removal/replace actions
    observe({
      req(rv_data$main)
      
      # Check for NAs
      has_nas <- !is_value_empty(qaqc_helper(rv_data$main, "NA", "names"))
      
      if (rv_na_action_taken()) {
        # After a button click, show the message from na_filter()
        shinyjs::hide("no_na_message")
        shinyjs::show("na_filter_message_container")
      } else {
        # On initial load or data change, manage the "no NA" message
        shinyjs::hide("na_filter_message_container")
        if (has_nas) {
          shinyjs::hide("no_na_message")
        } else {
          shinyjs::show("no_na_message")
        }
      }
      
      # Handle the NaN message visibility separately
      has_nans <- !is_value_empty(qaqc_helper(rv_data$main, "NaN", "names"))
      if (rv_nan_action_taken()) {
        # After a button click, show the message from na_filter()
        shinyjs::hide("no_nan_message")
        shinyjs::show("nan_filter_message_container")
      } else {
        # On initial load or data change, manage the "no NA" message
        shinyjs::hide("nan_filter_message_container")
        if (has_nans) {
          shinyjs::hide("no_nan_message")
        } else {
          shinyjs::show("no_nan_message")
        }
      }
    })
    
    # Render the message for NA
    output$rv_na_filter_message_out <- renderUI({
      HTML(rv_na_filter_message())
    })
    
    # Render the message for NaN
    output$rv_nan_filter_message_out <- renderUI({
      HTML(rv_nan_filter_message())
    })
    
    # NA data table
    output$na_datatable <- DT::renderDataTable(na_df())
    
    # NaN data table
    output$nan_datatable <- DT::renderDataTable(nan_df())
    
    # Pop up for users to select remove all NAs or replace
    observeEvent(input$remove_na_btn, {
      # Show a modal dialog when the button is clicked
      showModal(modalDialog(
        "How would you like to handle the NA values in the table?",
        footer = tagList(
          # Button to remove all rows with NA values
          actionButton(ns("na_filter_all_btn"), "Remove All NAs", 
            class = "btn-danger"),
          # Button to replace NAs with the mean of their column
          actionButton(ns("na_filter_mean_btn"), "Replace with Mean", 
            class = "btn-success"),
          # Button to close the modal without any action
          modalButton("Cancel")
        ),
        easyClose = TRUE 
      ))
    })
    
    # Run na_filter function to remove entire row containing NAs
    # save to DB and rv_data; show message
    observeEvent(input$na_filter_all_btn, {
      
      req(rv_data) # Ensure data is not null
      main_data <- rv_data$main # Save static copy of main data from reactive input
      req(rv_project_name()) # Check to ensure reactive is available
      project_name <- rv_project_name()$value
      
      # variables with NAs
      na_names <- qaqc_helper(main_data, "NA", output = "names")
      
      if (length(na_names) > 0) {
        
        q_test <- quietly_test(na_filter)
        output <- q_test(main_data, project = project_name, x = na_names,  
          replace = FALSE, remove = TRUE, over_write = TRUE)  
        
        if (!is_value_empty(output)) {
          rv_data$main <- output[["data"]]
          
          # capturing the messages produced in the na_filter function
          messages_to_show <- output[["messages"]]
          
          # ONLY remove the first element if there is more than one message
          if (length(messages_to_show) > 1) {
            rv_na_filter_message(paste(messages_to_show, collapse = "\n"))
          } else {
            # Otherwise, just show the single message that exists
            rv_na_filter_message(paste(messages_to_show, collapse = "\n"))
          }
          rv_na_action_taken(TRUE) # Set the flag
        }
      } else {
        showNotification('No missing values to remove', type = "default", duration = 60)
      }
      removeModal()
    } )
    
    # Run na_filter function to replace NAs with mean value
    # save to DB and rv_data; show message
    observeEvent(input$na_filter_mean_btn, {
      
      req(rv_data) # Ensure data is not null
      main_data <- rv_data$main # Save static copy of main data from reactive input
      req(rv_project_name()) # Check to ensure reactive is available
      project_name <- rv_project_name()$value
      
      na_names <- qaqc_helper(rv_data$main, "NA", output = "names")
      
      if (length(na_names) > 0) {
        
        q_test <- quietly_test(na_filter)
        output <- q_test(main_data, project = project_name, x = na_names, 
          replace = TRUE, remove = FALSE, rep.value = "mean", over_write = TRUE)  
        
        if (!is_value_empty(output)) {
          rv_data$main <- output[["data"]]
          # capturing the messages produced in the na_filter function
          
          messages_to_show <- output[["messages"]]
          
          # ONLY remove the first element if there is more than one message
          if (length(messages_to_show) > 1) {
            rv_na_filter_message(paste(messages_to_show[-1], collapse = "<br>"))
          } else {
            # Otherwise, just show the single message that exists
            rv_na_filter_message(paste(messages_to_show, collapse = "<br>"))
          }
          rv_na_action_taken(TRUE) # Set the flag
        }
      } else {
        showNotification('No missing values to remove', type = "default", duration = 60)
      }
      removeModal()
    })
    
    # Pop up for users to select remove all NaNs or replace
    observeEvent(input$remove_nan_btn, {
      # Show a modal dialog when the button is clicked
      showModal(modalDialog(
        "How would you like to handle the NaN values in the table?",
        footer = tagList(
          # Button to remove all rows with NA values
          actionButton(ns("nan_filter_all_btn"), "Remove All NaNs",
            class = "btn-danger"),
          # Button to replace NAs with the mean of their column
          actionButton(ns("nan_filter_mean_btn"), "Replace with Mean", 
            class = "btn-success"),
          # Button to close the modal without any action
          modalButton("Cancel")
        ),
        easyClose = TRUE 
      ))
    })
    
    # Run nan_filter function to remove entire row containing NaNs
    # save to DB and rv_data; show message 
    observeEvent(input$nan_filter_all_btn, {
      
      req(rv_data) # Ensure data is not null
      main_data <- rv_data$main # Save static copy of main data from reactive input
      req(rv_project_name()) # Check to ensure reactive is available
      project_name <- rv_project_name()$value
      
      nan_names <- qaqc_helper(main_data, "NaN", output = "names")
      
      if (length(nan_names) > 0) {
        
        q_test <- quietly_test(nan_filter)
        output <- q_test(main_data, project = project_name, x = nan_names,
          replace = FALSE, remove = TRUE, over_write = FALSE)
        
        if (!is_value_empty(output)) {
          rv_data$main <- output[["data"]]
          # capturing the messages produced in the nan_filter function
          messages_to_show <- output[["messages"]]
          
          # ONLY remove the first element if there is more than one message
          if (length(messages_to_show) > 1) {
            rv_nan_filter_message(paste(messages_to_show[-1], collapse = "<br>"))
          } else {
            # Otherwise, just show the single message that exists
            rv_nan_filter_message(paste(messages_to_show, collapse = "<br>"))
          }
          rv_nan_action_taken(TRUE) # Set the flag
        }
      } else {
        showNotification('No missing values to remove', type = "default", duration = 60)
      }
      removeModal()
    })
    
    # Run nan_filter function to replace NaNs with mean value
    # save to DB and rv_data; show message 
    observeEvent(input$nan_filter_mean_btn, {
      
      req(rv_data) # Ensure data is not null
      main_data <- rv_data$main # Save static copy of main data from reactive input
      req(rv_project_name()) # Check to ensure reactive is available
      project_name <- rv_project_name()$value
      
      nan_names <- qaqc_helper(main_data, "NaN", output = "names")
      
      if (length(nan_names) > 0) {
        
        q_test <- quietly_test(nan_filter)
        output <- q_test(main_data, project = project_name, x = nan_names,
          replace = TRUE, remove = FALSE, rep.value = "mean", over_write = FALSE)
        
        if (!is_value_empty(output)) {
          
          rv_data$main <- output[["data"]]
          # capturing the messages produced in the nan_filter function
          messages_to_show <- output[["messages"]]
          
          # ONLY remove the first element if there is more than one message
          if (length(messages_to_show) > 1) {
            rv_nan_filter_message(paste(messages_to_show[-1], collapse = "<br>"))
          } else {
            # Otherwise, just show the single message that exists
            rv_nan_filter_message(paste(messages_to_show, collapse = "<br>"))
          }
          rv_nan_action_taken(TRUE) # Set the flag
        }
      } else {
        showNotification('No missing values to remove', type = "default", duration = 60)
      }
      removeModal()
    })
    
  })
}



# UI ----------------------------------------------------------------------------------------------
#' summary_data_ui
#'
#' @description Creates the user interface for removing/replacing the NA and NaN values in the
#' primary data table. This included a placeholder for data tables showing variables and counts of 
#' NA/NaN values and action button to open modals with options
#'
#' @param id A character string that is unique to this module instance.
#'
#' @return A tagList containing the UI elements for the summary data module.

remove_na_nan_ui <- function(id){
  ns <- NS(id)
  
  tagList(  
    bslib::layout_column_wrap(
      fill = TRUE,
      width = 1/2,
      # Card for NA values
      bslib::card( 
        bslib::card_header(
          "NA values",
          class = "bg-secondary"),
        height = "500px",
        fill = TRUE,
        div(id = ns("no_na_message"),
          style = "color: grey; font-style: italic; font-size: 19px;",
          p("No NA observation(s) from the main data table")
        ),
        div(id = ns('na_filter_message_container'),
          style = "color: green; display: none; font-size: 16px;",
          uiOutput(ns("rv_na_filter_message_out"))),
        DT::DTOutput(ns("na_datatable")),
        bslib::card_footer(
          class = "d-flex justify-content-end bg-transparent border-0",
          actionButton(inputId = ns("remove_na_btn"),  
            label = "Remove NAs",
            width = "50%"))
      ),
      
      # Card for NaN values
      bslib::card(
        bslib::card_header(
          "NaN values",
          class = "bg-secondary"),
        height = "500px",
        fill = TRUE,    
        div(id = ns("no_nan_message"),
          style = "color: grey; font-style: italic; font-size: 19px;",
          p("No NaN observation(s) from the main data table")
        ),
        div(id = ns('nan_filter_message_container'),
          style = "color: green; display: none; font-size: 16px;",
          uiOutput(ns("rv_nan_filter_message_out"))),
        DT::DTOutput(ns("nan_datatable")),
        bslib::card_footer(
          class = "d-flex justify-content-end bg-transparent border-0",
          actionButton(inputId = ns("remove_nan_btn"),  
            label = "Remove NaNs",
            width = "50%"))
      )
    )
  )
  
}
