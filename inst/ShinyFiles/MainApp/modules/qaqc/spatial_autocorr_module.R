# =================================================================================================
# File: spatial_autocorr_module.R
# Description: This module provides a user interface and server-side logic for performing global 
#              and local spatial autocorrelation analysis. It calculates Moran's I and generates a
#              corresponding plot and table.
#
# Authors: Paul Carvalho, Anna Abelman
# Date created: 8/26/2025
# Dependencies: The 'spdep' and 'ggplot2' packages are required for the statistical calculations
# Notes: 
# =================================================================================================

# Server ------------------------------------------------------------------------------------------
#' spatial_autocorr_server
#'
#' @description The server-side logic for the spatial autocorrelation module. It handles user
#' input, calculates Moran's I and LISA statistics, and renders the output table and plot.
#'
#' @param id A character string that is the namespace for this module.
#' @param rv_project_name A reactive value containing the name of the current project.
#' @param rv_data A reactive list containing the main dataset (`main`) and spatial data (`spat`).
#' @param rv_folderpath A reactive value containing the file path to the project's root folder.
#'
#' @return A module server instance.
spatial_autocorr_server <- function(id, rv_project_name, rv_data, rv_folderpath){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Initialize reactives
    rv_selected_vars <- reactiveValues(vars = NULL)
    moran_results_rv <- reactiveVal(NULL)
    
    # Observe to dynamically update choice in the spatial autocorrelation dropdown.
    observe({
      var_list <- names(rv_data$main)
      
      # If the data list is empty or NULL, clear the choices and exit.
      if (is.null(var_list) || length(var_list) == 0) {
        updateSelectInput(session, "select_var_input", choices = character(0)) # Clear choices
        return()
      }
      
      # Filter the names to get only numeric variables
      numeric_choices <- names(rv_data$main[sapply(rv_data$main, is.numeric)])
      
      # Update input options
      updateSelectInput(session, "select_var_input", choices = numeric_choices)
    })
    
    # Observe the variable input and calculate Moran's stats
    observeEvent(input$select_var_input, {
      # Ensure required reactive values are available before proceeding.
      req(rv_project_name)
      req(rv_folderpath)
      req(rv_data$main)
      project_name <- rv_project_name()$value
      folderpath <- rv_folderpath()
      
      # Load selected variables
      selected_vars <- load_gui_variables(project_name, folderpath)
      if (is.null(selected_vars)) {
        return()
      }
      rv_selected_vars$vars <- selected_vars
      
      # Calculate Moran stats and generate figures  
      moran_output <- tryCatch({
        moran_stats(dat = rv_data$main,
                    var = input$select_var_input,
                    dat_zone = rv_selected_vars$vars$main$main_zone_id,
                    spat = rv_data$spat,
                    spat_zone = rv_selected_vars$vars$spat$spat_zone_id,
                    project = project_name)
        
      }, error = function(e) {
        # Handle errors
        showModal(modalDialog(
          title = "Error",
          paste("An error occurred during the spatial autocorrelation analysis:", e$message),
          easyClose = TRUE
        ))
        return() # Return NULL to clear outputs
      })
      
      # Save output from global Moran's I test 
      moran_results_rv(moran_output)
    })
    
    # Output table of Moran's I test
    output$moran_table <- renderTable({
      # Require the reactive value to be non-NULL before rendering
      req(moran_results_rv())
      
      # Extract key values from the test result
      test_result <- moran_results_rv()[[1]]
      
      data.frame(
        Statistic = "Moran's I",
        Value = round(test_result$estimate["Moran I statistic"], 3),
        Expected = round(test_result$estimate["Expectation"], 3),
        Variance = round(test_result$estimate["Variance"], 3),
        P_Value = round(test_result$p.value, 3)
      )
    },
    digits = 3,
    width = "600px",
    caption = "Note: Moran's I ranges from -1 to +1. Values near |1| indicate strong spatial 
              autocorrelation and zero indicates no spatial autocorrelation."
    )
    
    # Plot Local Indicators of Spatial Association
    output$lisa_plot <- renderPlot({
      req(moran_results_rv())
      plot_out <- moran_results_rv()[[3]]
      
      plot_out +
        labs(title = "Local indicators of spatial association") +
        theme(
          title = element_text(size = 14),
          axis.text = element_text(size = 14),
          legend.text = element_text(size = 14)
        )
    })
    
    # Reactive output to control caption visibility
    output$show_lisa_plot <- reactive({
      !is.null(moran_results_rv()) && length(moran_results_rv()) >= 3
    })
    
    # Ensure the output is available for the conditionalPanel to check in the UI
    outputOptions(output, "show_lisa_plot", suspendWhenHidden = FALSE)
  })
}

# UI ----------------------------------------------------------------------------------------------
#' spatial_autocorr_ui
#'
#' @description The user interface for the spatial autocorrelation module. It includes a dropdown
#' for variable selection, a table for Global Moran's I results, and a plot for LISA.
#'
#' @param id A character string that is the namespace for this module.
#'
#' @return A user interface tag list.
spatial_autocorr_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    bslib::card(
      height = "1000px",
      fill = FALSE,
      
      selectInput(ns("select_var_input"),
                  label = "Select a variable to estimate spatial autocorrelation:",
                  choices = NULL),
      
      tableOutput(ns("moran_table")),
      
      conditionalPanel(
        condition = "output.show_lisa_plot",
        ns = ns,
        div(
          style = "width: 800px; height: 600px; margin-right: auto;",
          plotOutput(ns("lisa_plot"), width = "100%", height = "100%"),
          HTML(
            paste0("<p style='text-align: left; font-size: 14px; color: grey;'>",
                   "Note: Local indicators of spatial association (LISA) breaks down the
                  Moran's I to the local level, and shows <br> where clustering is happening. 
                  High-high identifies 'hot-spots', low-low identifies 'cold-spots'. Spatial
                  outliers <br> (high-low and low-high) are locations with values that are 
                  significantly different from their neighbors.",
                   "</p>"))
        )  
      ),
      
      conditionalPanel(
        condition = "!output.show_lisa_plot",
        ns = ns,
        div(
          tags$p(
            HTML("Variables must be selected in the Load Data subtab to view 
                 spatial autocorrelation outputs. <br>
                 If you completed this step, change the variable in the dropdown above."),
            style = "color: #b0b0b0; font-style: italic; font-size: 20px;")
        )
      )
    )
  )
}