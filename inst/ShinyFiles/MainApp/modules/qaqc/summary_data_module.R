# =================================================================================================
# File: summary_data_module.R
# Description: This module displays an interactive table to view summary statistic of the primary 
#              data table.
#
# Authors: Paul Carvalho, Anna Abelman
# Date created: 8/7/2025
# Dependencies: shiny, DT
# Notes: This module is used within qaqc_module.R
# =================================================================================================

# Server ------------------------------------------------------------------------------------------
#' summary_data_server
#'
#' @description Defines the server-side logic for the summary statistics table module. It renders
#' an interactive DT::datatable that displays the summary stats for the primary data
#'
#' @param id A character string that is unique to this module instance.
#' @param rv_project_name A reactive value containing the current project name.
#' @param rv_data A reactiveValues object containing the list of data frames to be displayed.
#'
#' @return This module does not return a value.
#' 
summary_data_server <- function(id, rv_project_name, rv_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Creates summary stats and saves as a reactive data frame
    summary_stats_df <- reactive({
      
      req(rv_project_name()) # Check to ensure reactive is available
      project_name <- rv_project_name()$value
      req(rv_data) # Ensure data is not null
      main_data <- rv_data$main # Save static copy of main data from reactive input
      
      # Check that main data table exist and if not return nothing
      if (is.null(main_data) || length(main_data) == 0) {
        return()
      } else{
        
        # Calculate summary stats
        sum_table <- summary_stats(dat=main_data, project = project_name) 
        
        # Reconfigure results to better table layout
        nums <- unlist(lapply(main_data, is.numeric))
        sum_table  <- apply(sum_table[nums], 2, function(x) gsub(".*:","", x))
        rownames(sum_table)=c('Min', 'Median','Mean', 'Max',"Missing",'Unique Obs.', "No. 0's")
        sum_table <- as.data.frame(as.matrix(sum_table))
        sum_table <- as.data.frame((t(sum_table)))
        
        return(sum_table)
      }
      
    })
    
    
    output$summary_datatable <- DT::renderDataTable(
      summary_stats_df(),
      server = FALSE, rownames=TRUE,
      # Add options for scrolling and disable paging
      options = list(
        scrollY = "100%",
        scrollCollapse = TRUE,
        autoWidth=FALSE, scrollX=TRUE, responsive=FALSE, pageLength = 25
      ),
      # This ensures width and height are handled correctly
      fillContainer = TRUE
      
    )
    
  }
  )
}


# UI ----------------------------------------------------------------------------------------------
#' summary_data_ui
#'
#' @description Creates the user interface for the summary statistics data table module. This
#' includes a placeholder for the summary data table.
#'
#' @param id A character string that is unique to this module instance.
#'
#' @return A tagList containing the UI elements for the summary data module.

summary_data_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    bslib::card(
      height = "1000px",
      fill = TRUE, 
      DT::DTOutput(ns("summary_datatable")))
  )
}
