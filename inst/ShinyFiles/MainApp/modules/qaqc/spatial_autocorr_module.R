# =================================================================================================
# File: spatial_autocorr_module.R
# Description: 
#
# Authors: Paul Carvalho, Anna Abelman
# Date created: 8/26/2025
# Dependencies: 
# Notes: 
# =================================================================================================

# Source module scripts ---------------------------------------------------------------------------
source("modules/spinner.R", local = TRUE)

# Server ------------------------------------------------------------------------------------------
#' spatial_autocorr_server
#'
#' @description 
#'
#' @param id A character string that is the namespace for this module.
#' @param rv_project_name A reactive value containing the name of the current project.
#' @param rv_data A reactive list containing the main dataset (`main`) and spatial data (`spat`).
#' @param rv_folderpath A reactive value containing the file path to the project's root folder.
#'
#' @return 
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
      
      #TODO: Add a function that grabs the selected values!!!!!
      
      # Construct the file path for the saved variables.
      file_name <- paste0(project_name, "SavedVariables.rds")
      file_path <- file.path(folderpath, project_name, "data", file_name)
      if (file.exists(file_path)) {
        file_path <- suppressWarnings(normalizePath(file_path))
      } else {
        return() # Stop execution of the observer
      }
      
      # Read the RDS file containing selected variable names.
      selected_vars <- readRDS(file_path)
      rv_selected_vars$vars <- selected_vars
      
      # Calculate Moran stats and generate figures      
      moran_output <- moran_stats(dat = rv_data$main,
                                  var = input$select_var_input,
                                  dat_zone = rv_selected_vars$vars$main$main_zone_id,
                                  spat = rv_data$spat,
                                  spat_zone = rv_selected_vars$vars$spat$spat_zone_id,
                                  project = project_name)
      
      # Save output from global Moran's I test 
      moran_results_rv(moran_output[[1]])
    })
    
    # Output table of Moran's I test
    output$moran_table <- renderTable({
      # Require the reactive value to be non-NULL before rendering
      req(moran_results_rv())
      
      # Extract key values from the test result
      test_result <- moran_results_rv()
      
      data.frame(
        Statistic = "Moran's I",
        Value = round(test_result$estimate["Moran I statistic"], 3),
        Expected = round(test_result$estimate["Expectation"], 3),
        Variance = round(test_result$estimate["Variance"], 3),
        P_Value = round(test_result$p.value, 3)
      )
    },
    digits = 3,
    table.attr = 'style="width:100%;"'
    )
  })
}

# UI ----------------------------------------------------------------------------------------------
#' spatial_autocorr_ui
#'
#' @description 
#'
#' @param id A character string that is the namespace for this module.
#'
#' @return 
spatial_autocorr_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    bslib::card(
      height = "1000px",
      fill = FALSE, 
      selectInput(ns("select_var_input"),
                  label = "Select a variable to estimate spatial autocorrelation:",
                  choices = NULL),
      tableOutput(ns("moran_table"))    
    )
  )
  
  
}