# =================================================================================================
# File: preview_data_module.R
# Description: 
#
# Authors: Paul Carvalho, Anna Abelman
# Date created: 8/4/2025
# Dependencies: 
# Notes: 
# =================================================================================================

# Load libraries
library(DT)

# 
preview_data_server <- function(id, rv_project_name, rv_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    observe({
      data_list <- reactiveValuesToList(rv_data)
      
      if (is.null(data_list) || length(data_list) == 0) {
        updateSelectInput(session, "select_data", choices = character(0)) # Clear choices
        return()
      }
      
      df_names <- names(data_list)[sapply(data_list, is.data.frame)]
      
      cat("\n", str(df_names), "\n")
      
      updateSelectInput(session, "select_data", choices = df_names)
    })
    
    output$preview_datatable <- DT::renderDT({
      req(input$select_data)
      
      df_to_display <- rv_data[[input$select_data]]
      
      req(df_to_display)
      
      DT::datatable(
        df_to_display,
        rownames = FALSE,
        filter = "top",
        options = list(
          scrollX = TRUE,
          pageLength = 15
        )
      )
    })
    
  })
}

#
preview_data_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    selectInput(ns("select_data"),
                label = "Select data to view:",
                choices = NULL),
    
    DT::DTOutput(ns("preview_datatable"))
  )
  
  
}
