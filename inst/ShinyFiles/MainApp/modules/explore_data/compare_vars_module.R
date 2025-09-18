# =================================================================================================
# File: compare_vars_module.R
# Description: A Shiny module for creating plots to visualize the relationship between two
#              variables. It generates a scatter plot for two numeric variables or a
#              box plot for a categorical (x) and numeric (y) variable. It can also
#              display a corresponding statistical model summary (Linear Regression or ANOVA).
#
# Authors: Paul Carvalho, Anna Abelman
# Date created: 9/17/2025
# Dependencies: shiny, bslib, dplyr, ggplot2, shinycssloaders
# Notes: This module is intended for exploratory data analysis, providing a quick way
#        to visualize relationships between variables. 
# =================================================================================================

# Constants and Helpers ---------------------------------------------------------------------------
# Plot color
plot_color <- "#586A89"

# Server ------------------------------------------------------------------------------------------
#' compare_vars_server
#'
#' @description The server-side logic for the variable relationship plot module. It populates
#'              variable selections and dynamically renders the correct plot type 
#'              (scatter or box plot) and statistical summary (LM or ANOVA) based on 
#'              variable data types.
#'
#' @param id A character string that is unique to this module instance.
#' @param rv_folderpath A reactive value containing the path to the project folder.
#' @param rv_project_name A reactive value containing the current project name.
#' @param rv_data A reactiveValues object containing the list of data frames to be displayed.
#'
#' @return This module does not return a value.
compare_vars_server <- function(id, rv_folderpath, rv_project_name, rv_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Observe to dynamically update choices in the dropdown menus
    observe({
      req(rv_data$main)
      
      all_vars <- names(rv_data$main)
      numeric_vars <- names(rv_data$main[sapply(rv_data$main, is.numeric)])
      
      # Add a placeholder for inputs
      choices_x <- c("Select a variable..." = "", all_vars)
      choices_y <- c("Select a variable..." = "", numeric_vars)
      
      # Update inputs: X can be any variable, Y must be numeric
      updateSelectInput(session, "x_var_input", choices = choices_x, selected = "")
      updateSelectInput(session, "y_var_input", choices = choices_y, selected = "")
    })
    
    # Render the plot
    output$compare_vars_plot <- renderPlot({
      # Require both inputs to have a value
      validate(
        need(input$x_var_input != "", "Please select a variable for the X-axis"),
        need(input$y_var_input != "", "Please select a variable for the Y-axis")
      )
      
      df <- rv_data$main
      is_x_numeric <- is.numeric(df[[input$x_var_input]])
      
      # Base plot
      p <- ggplot2::ggplot(
        df, 
        aes(x = .data[[input$x_var_input]], y = .data[[input$y_var_input]])) +
        ggplot2::labs(x = input$x_var_input, y = input$y_var_input) +
        ggplot2::theme_classic(base_size = 14)
      
      # Conditionally add plot layers
      if (is_x_numeric) {
        # SCATTER PLOT
        p <- p +
          ggplot2::geom_point(alpha = 0.6, color = plot_color)
        
        if (input$show_lm_input == "Show") {
          p <- p +
            ggplot2::geom_smooth(method = "lm", 
                                 se = TRUE, 
                                 color = 'black', 
                                 fill = 'grey60', 
                                 alpha = 0.8)
        }
        
      } else {
        # BOX PLOT
        p <- p +
          ggplot2::geom_boxplot(fill = plot_color, alpha = 0.8)
      }
      
      p
    })
    
    # Render the linear regression summary output
    output$lm_summary <- renderPrint({
      req(input$x_var_input, input$y_var_input)
      
      # Create the formula and fit the model
      formula <- as.formula(paste(input$y_var_input, "~", input$x_var_input))
      model <- lm(formula, data = rv_data$main)
      
      # Return the summary
      summary(model)
    })
  })
}

# UI ----------------------------------------------------------------------------------------------
#' compare_vars_ui
#'
#' @description The UI-side function for the compare variables module. It provides dropdowns
#'              for variable selection, radio buttons for the regression line, and
#'              containers for the plot and summary output.
#'
#' @param id A character string that is unique to this module instance.
#'
#' @return A tagList containing the UI elements for the preview data module.
compare_vars_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    bslib::card(
      bslib::card_header("Variable relationship plot"),
      bslib::card_body(
        fluidRow(
          column(4,
                 selectInput(ns("x_var_input"),
                             label = "Select x-axis (independent) variable:",
                             choices = NULL)
          ),
          
          column(4,
                 selectInput(ns("y_var_input"),
                             label = "Select y-axis (dependent) variable:",
                             choices = NULL)
          ),
          
          column(4,
                 radioButtons(ns("show_lm_input"),
                              label = "Show linear regression:",
                              choices = c("Hide", "Show"),
                              selected = "Hide",
                              inline = TRUE)
          )
        ),
        
        # Plot output 
        shinycssloaders::withSpinner(
          plotOutput(ns("compare_vars_plot"), height = "45vh"), type = 6
        ),
        
        # Conditional output for linear regression summary
        conditionalPanel(
          condition = "input.show_lm_input == 'Show'",
          ns = ns,
          h5("Linear rergession model summary"),
          verbatimTextOutput(ns("lm_summary"))
        )
      )
    )
  )
}
