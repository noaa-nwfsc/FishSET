# =================================================================================================
# File: summary_data_module.R
# Description: This module displays an interactive table to view loaded data tables. It includes
#              a dropdown menu to select a table to view.
#
# Authors: Paul Carvalho, Anna Abelman
# Date created: 8/4/2025
# Dependencies: shiny, DT
# Notes: This module is used within qaqc_module.R
# =================================================================================================

# Server ------------------------------------------------------------------------------------------
#' summary_data_server
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
summary_data_server <- function(id, rv_project_name, rv_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    ##Table output
    tableInputSummary <- reactive({

        req(rv_project_name()) # Check to ensure reactive is available
        project_name <- rv_project_name()$value
        req(rv_data) # Ensure data is not null
        main_data <- rv_data$main # Save static copy of main data from reactive input
      
        stable <- summary_stats(dat=main_data, project = project_name) 
        nums <- unlist(lapply(main_data, is.numeric))
        stable  <- apply(stable[nums], 2, function(x) gsub(".*:","", x))
        rownames(stable)=c('Min', 'Median','Mean', 'Max',"Missing",'Unique Obs.', "No. 0's")
        stable <- as.data.frame(as.matrix(stable))
        stable <- as.data.frame((t(stable)))
        
     #   qaqc_out_proj$sum_tab <- project_name
        
        return(stable)

    })
    
    
    output$output_table_summary <- DT::renderDataTable(
        tableInputSummary(),
      server = FALSE, rownames=TRUE,
      # Add options for scrolling and disable paging
      options = list(
        scrollY = "100%",
        scrollCollapse = TRUE,
        autoWidth=FALSE, scrollX=TRUE, responsive=FALSE, pageLength = 25
      ),
      # This ensures width and height are handled correctly
      fillContainer = TRUE
    
     # options = list(autoWidth=FALSE, scrollX=TRUE, responsive=FALSE, pageLength = 25)
    )
    
  }
 )
}



# UI ----------------------------------------------------------------------------------------------
#' summary_data_ui
#'
#' @description Creates the user interface for the data preview module. This includes
#' a dropdown for data selection and a placeholder for the data table.
#'
#' @param id A character string that is unique to this module instance.
#'
#' @return A tagList containing the UI elements for the preview data module.
summary_data_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    bslib::card(  height = "1000px", fill = TRUE, 
      DT::DTOutput(ns("output_table_summary")))
  )
}
