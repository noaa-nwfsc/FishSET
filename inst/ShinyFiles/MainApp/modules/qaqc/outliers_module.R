# =================================================================================================
# File: outliers_module.R
# Description: This module allows users to identify, visualize, and remove outliers from numeric
#              variables in the main dataset.
#
# Authors: Paul Carvalho, Anna Abelman
# Date created: 8/28/2025
# Dependencies: shiny, DT, bslib, ggplot2
# Notes: This module is used within qaqc_module.R
# =================================================================================================

# Server ------------------------------------------------------------------------------------------
#' outliers_server
#'
#' @description Defines the server-side logic for the data outliers module. It allows users to
#' select a numeric variable, view a summary table of outlier checks, visualize the data
#' distribution, and remove outliers based on selected criteria.
#'
#' @param id A character string that is the namespace for this module.
#' @param rv_project_name A reactive value containing the name of the current project.
#' @param rv_data A reactive list containing the main dataset (`main`) and spatial data (`spat`).
#' @param rv_folderpath A reactive value containing the file path to the project's root folder.
#'
#' @return A reactive list containing `$ids`, the unique identifiers of observations to be removed,
#'         and `$id_col`, the name of the unique ID column.
outliers_server <- function(id, rv_project_name, rv_data, rv_folderpath){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Initialize reactives
    rv_outlier_data <- reactiveVal(NULL) # Holds data with outliers removed for previewing
    
    # Update dropdown with numeric variables from main data
    observe({
      req(rv_data$main)
      
      # Find numeric columns
      numeric_cols <- names(rv_data$main)[sapply(rv_data$main, is.numeric)]
      
      if (length(numeric_cols) == 0) {
        updateSelectInput(session, "outlier_var_input", choices = character(0))
        return()
      }
      
      updateSelectInput(session, "outlier_var_input", choices = numeric_cols)
    })
    
    # Generate outlier summary table reactively
    output$outlier_summary_table <- DT::renderDataTable({
      req(rv_project_name(),
          rv_data$main,
          input$outlier_var_input)
      
      # Use tryCatch to handle potential errors if the selected column is not suitable
      tryCatch({
        outlier_table(
          dat = rv_data$main,
          project = rv_project_name()$value,
          x = input$outlier_var_input
        )
        
      }, error = function(e) {
        # Handle error
        showModal(modalDialog(
          title = "Error: Unable to generate outlier table",
          e$message,
          easyClose = TRUE
        ))
        return()
      })
    }, options = list(dom = 't', scrollX = TRUE), server = FALSE)
    
    # Generate diagnostic plot reactively
    output$outlier_diagnostic_plot <- renderPlot({
      req(rv_project_name(), 
          rv_data$main, 
          input$outlier_var_input, 
          input$outlier_method_input, 
          input$outlier_dist_input)
      
      # Determine sd_val if user-defined method is selected
      dat_remove_val <- input$outlier_method_input
      sd_val <- if (dat_remove_val == "user_defined_sd") {
        req(input$outlier_sd_input)
        dat_remove_val <- input$outlier_sd_input # Set dat.remove to the numeric value
        input$outlier_sd_input
      } else {
        NULL
      }
      
      # Use a quiet version of the plot function to avoid stopping the app on error
      q_outlier_plot <- quietly(outlier_plot)
      
      # Call the plot function
      result <- q_outlier_plot(
        dat = rv_data$main,
        project = rv_project_name()$value,
        x = input$outlier_var_input
      )
      
      grid::grid.draw(result[[1]])
    }, height = 600, res = 100)
  })
}

# UI ----------------------------------------------------------------------------------------------
#' outliers_ui
#'
#' @description
#'
#' @param id A character string that is the namespace for this module.
#'
#' @return A tagList containing the UI elements for the module.
outliers_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    bslib::layout_column_wrap(
      fill = TRUE,
      width = 1/3,
      selectInput(ns("outlier_var_input"),
                  label = "Select a variable to check:",
                  choices = NULL),
      selectInput(ns("outlier_method_input"), 
                  label = "Select outlier method:",
                  choices = c("5-95% Quantile" = "5_95_quant", 
                              "25-75% Quantile" = "25_75_quant", 
                              "Mean +/- 2 SD" = "mean_2SD", 
                              "Median +/- 2 SD" = "median_2SD", 
                              "Mean +/- 3 SD" = "mean_3SD",
                              "Median +/- 3 SD" = "median_3SD",
                              "User-defined SD" = "user_defined_sd")),
      selectInput(ns("outlier_dist_input"),
                  label = "Select distribution for plot:",
                  choices = c("Normal" = "normal",
                              "Lognormal" = "lognormal",
                              "Exponential" = "exponential",
                              "Weibull" = "weibull",
                              "Poisson" = "poisson",
                              "Negative Binomial" = "negative binomial"))
    ),
    
    bslib::card(
      height = "650px",
      fill = TRUE,
      bslib::card_header("Outlier Analysis"),
      bslib::card_body(
        DT::DTOutput(ns("outlier_summary_table")),
        plotOutput(ns("outlier_diagnostic_plot"))
      )
    )
  )
}