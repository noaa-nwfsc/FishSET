# =================================================================================================
# File: temporal_plots_module.R
# Description: 
#
# Authors: Paul Carvalho, Anna Abelman
# Date created: 9/15/2025
# Dependencies: 
# Notes: 
# =================================================================================================

# Server ------------------------------------------------------------------------------------------
#' temp_plots_server
#'
#' @description 
#'
#' @param id A character string that is unique to this module instance.
#' @param rv_project_name A reactive value containing the current project name.
#' @param rv_data A reactiveValues object containing the list of data frames to be displayed.
#'
#' @return This module does not return a value.
temp_plots_server <- function(id, rv_folderpath, rv_project_name, rv_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Initialize reactive values
    rv_tmp_main <- reactiveVal()
    
    # Observe to dynamically update choices in the dropdown.
    observe({
      var_list <- names(rv_data$main)
      
      # If the data list is empty or NULL, clear the choices and exit.
      if (is.null(var_list) || length(var_list) == 0) {
        updateSelectInput(session, "select_var_input", choices = character(0)) # Clear choices
        return()
      }
      
      # Add a placeholder prompt and set it as the default selected value.
      choices_with_placeholder <- c("Select a variable..." = "", names(rv_data$main))
      
      # Update input options
      updateSelectInput(session, 
                        "select_var_input", 
                        choices = choices_with_placeholder,
                        selected = "")
    })
    
    # Load the user-selected date variable saved from the 'Select variables' step.
    observeEvent(input$select_var_input, {
      req(rv_project_name()$value, rv_folderpath())
      vars <- try(load_gui_variables(rv_project_name()$value, rv_folderpath()), silent = TRUE)
      
      # Filter dataset
      tmp_df <- rv_data$main %>%
        dplyr::select(Date = vars$main$main_date, 
                      Var = input$select_var_input)
      
      # Save to reactive for plotting
      rv_tmp_main(tmp_df)
    })
    
    # Plot 1: Scatter plot of the selected variable over time
    output$scatter_plot <- renderPlot({
      req(rv_tmp_main)
      
      ggplot2::ggplot() +
        ggplot2::geom_point(data = rv_tmp_main(),
                            aes(x = Date, y = Var),
                            alpha = 0.6,
                            color = 'royalblue') +
        ggplot2::theme_classic() +
        ggplot2::xlab("Date") +
        ggplot2::ylab((input$select_var_input)) +
        ggplot2::theme(
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14)
        )
    })
    
    
  })
}

# UI ----------------------------------------------------------------------------------------------
#' temp_plots_ui
#'
#' @description 
#'
#' @param id A character string that is unique to this module instance.
#'
#' @return A tagList containing the UI elements for the preview data module.
temp_plots_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    bslib::card(
      height = "1000px",
      fill = FALSE,
      
      selectInput(ns("select_var_input"),
                  label = "Select a variable to plot:",
                  choices = NULL),
      
      hr(),
      fluidRow(
        column(12,
               h5("Variable by Date"),
               shinycssloaders::withSpinner(plotOutput(ns("scatter_plot")), type = 6)
        )
      )
    ))
}
