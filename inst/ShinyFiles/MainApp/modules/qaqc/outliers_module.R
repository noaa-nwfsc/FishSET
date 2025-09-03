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
      dat_remove_method <- input$outlier_method_input
      user_sd <- NULL
      
      # Handle user-defined SD value
      if (dat_remove_method == "user_defined_sd") {
        req(input$outlier_sd_input)
        user_sd <- input$outlier_sd_input
        # The outlier_plot function expects the numeric value in dat.remove
        dat_remove_method <- user_sd 
      }
      
      # Use a quiet version of the plot function to avoid stopping the app on error
      q_outlier_plot <- quietly(outlier_plot)
      
      # Call the plot function
      result <- q_outlier_plot(
        dat = rv_data$main,
        project = rv_project_name()$value,
        x = input$outlier_var_input,
        dat.remove = dat_remove_method,
        sd_val = user_sd,
        x.dist = input$outlier_dist_input
      )
      
      grid::grid.draw(result[[1]])
    }, height = 600, res = 100)
    
    # Modal for reviewing and removing outliers
    observeEvent(input$review_outliers_btn, {
      req(rv_project_name(), rv_data$main, input$outlier_var_input, input$outlier_method_input)
      
      original_data <- rv_data$main
      original_n <- nrow(original_data)
      
      # Determine removal method and sd_val
      dat_remove_method <- input$outlier_method_input
      user_sd <- NULL
      if (dat_remove_method == "user_defined_sd") {
        req(input$outlier_sd_input)
        user_sd <- input$outlier_sd_input
        dat_remove_method <- user_sd
      }
      
      # Perform the removal on a temporary copy
      removed_data <- outlier_remove(
        dat = original_data,
        project = rv_project_name()$value,
        x = input$outlier_var_input,
        dat.remove = dat_remove_method,
        sd_val = user_sd,
        over_write = FALSE # IMPORTANT: Do not overwrite yet
      )
      
      # Store the processed data in the reactive value
      rv_outlier_data(removed_data)
      new_n <- nrow(removed_data)
      
      # Show the confirmation modal
      showModal(
        modalDialog(
          title = "Confirm Outlier Removal",
          p(strong("Variable: "), code(input$outlier_var_input)),
          p(strong("Method: "), code(input$outlier_method_input)),
          if (input$outlier_method_input == "user_defined_sd") {
            p(strong("SD Value: "), code(input$outlier_sd_input))
          },
          hr(),
          p(paste("This action will remove", original_n - new_n, "rows from the dataset.")),
          p(paste("Original row count:", original_n)),
          p(paste("New row count:", new_n)),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("save_removal_btn"), 
                         "Remove outliers", 
                         class = "btn-danger",
                         icon = icon("trash"))
          ),
          size = "m",
          easyClose = TRUE
        ))
    })
    
    # Save changes to session data and database
    observeEvent(input$save_removal_btn, {
      req(rv_outlier_data(), rv_project_name())
      
      # Update the main reactive data
      rv_data$main <- rv_outlier_data()
      
      # Save the updated data to the project database
      q_save <- quietly(table_save)
      saved <- q_save(rv_data$main, project = rv_project_name()$value, type = "main")
      
      if (!is.null(saved$result)) {
        showNotification("Outliers removed and data saved successfully!", type = "default")
        
      } else {
        showNotification("Failed to save data.", type = "error")
      }
      
      # Clean up and close modal
      rv_outlier_data(NULL)
      removeModal()
    })
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
                  choices = c("None" = "none",
                              "5-95% Quantile" = "5_95_quant", 
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
    
    # Conditional panel for user-defined SD
    conditionalPanel(
      condition = "input.outlier_method_input == 'user_defined_sd'",
      ns = ns,
      numericInput(ns("outlier_sd_input"), "Enter SD value:", 
                   value = 2, min = 0, step = 0.5, width = "25%")
    ),
    
    actionButton(ns('review_outliers_btn'),
                 label = 'Review & Remove Outliers', 
                 class = "btn-secondary",
                 width = "25%"),
    
    bslib::card(
      fill = TRUE,
      bslib::card_header("Outlier Analysis"),
      bslib::card_body(
        fillable = TRUE,
        DT::DTOutput(ns("outlier_summary_table")),
        # Add a div for responsive vertical spacing
        div(class = "mt-2"), # mt-4 means "margin-top of size 4"
        plotOutput(ns("outlier_diagnostic_plot"))
      )
    )
  )
}