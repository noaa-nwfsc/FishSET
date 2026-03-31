# =================================================================================================
# File: temporal_plots_module.R
# Description:  A Shiny module for visualizing temporal trends. It includes a scatter 
#              plot of a selected variable over time and two bar charts that 
#              summarize data annually by observation count and other statistics.
#
# Authors: Paul Carvalho, Anna Abelman
# Date created: 9/15/2025
# Dependencies: shiny, bslib, dplyr, ggplot2, lubridate, shinycssloaders
# Notes: This module is designed to be a plug-and-play component in a larger Shiny
#        application for data exploration.
# =================================================================================================

# Constants and Helpers ---------------------------------------------------------------------------
# Temporal plot color
temp_plot_color <- "#586A89"

## Shared plot theme
temporal_plot_theme <- ggplot2::theme_classic() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(size = 14),
    axis.text.y = ggplot2::element_text(size = 14),
    axis.title.x = ggplot2::element_text(size = 14),
    axis.title.y = ggplot2::element_text(size = 14),
    plot.margin = margin(t = 5.5, r = 30, b = 5.5, l = 5.5, unit = "pt")
  )

# Server ------------------------------------------------------------------------------------------
#' temp_plots_server
#'
#' @description The server-side logic for the temporal plots module. It handles 
#'              dynamic UI updates, data filtering based on user input, and the
#'              generation of three distinct plots for temporal analysis.
#'
#' @param id A character string that is unique to this module instance.
#' @param rv_folderpath A reactive value containing the path to the project folder.
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
      
      # Filter the names to get only numeric variables
      numeric_choices <- names(rv_data$main[sapply(rv_data$main, is.numeric)])
      
      # Add a placeholder prompt and set it as the default selected value.
      choices_with_placeholder <- c("Select a variable..." = "", numeric_choices)
      
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
      
      # Check that 'vars' loaded correctly and that the date variable exists before proceeding.
      req(!inherits(vars, "try-error") && !is.null(vars$main$main_date))
      
      # Filter dataset
      tmp_df <- rv_data$main %>%
        dplyr::select(Date = vars$main$main_date, 
                      Var = input$select_var_input)
      
      # Save to reactive for plotting
      rv_tmp_main(tmp_df)
    })
    
    # Plot 1: Scatter plot of the selected variable over time
    output$scatter_plot <- renderPlot({
      validate(need(input$select_var_input != "", "Select a variable to generate plots."))
      validate(
        need(
          !is.null(rv_tmp_main()) && "Date" %in% names(rv_tmp_main()), 
          "A date column is required. Please go to the 'Select variables' tab and 
           choose a date column first."))
      
      ggplot2::ggplot() +
        ggplot2::geom_point(data = rv_tmp_main(),
                            aes(x = Date, y = Var),
                            alpha = 0.6,
                            color = temp_plot_color) +
        ggplot2::labs(x = "Date",
                      y = input$select_var_input) +
        ggplot2::scale_y_continuous(expand = c(0.01,0.01)) +
        temporal_plot_theme
    })
    
    # Plot 2: Bar chart summarizing observation counts by year
    output$count_summary_plot <- renderPlot({
      validate(need(input$select_var_input != "", "Select a variable to generate plots."))
      validate(need(!is.null(rv_tmp_main()$Date), 
                    "A date column is required. Please go to the 'Select variables' tab and 
                    choose a date column first.")
      )
      
      # Prep data for plotting
      df <- rv_tmp_main() %>% dplyr::mutate(Year = lubridate::year(Date))
      total_obs <- nrow(df)
      summary_df <- switch(
        input$count_function_input,
        "No. observations" = df %>% 
          dplyr::group_by(Year) %>% 
          dplyr::summarise(Value = dplyr::n()),
        "% of total observations" = df %>% 
          dplyr::group_by(Year) %>% 
          dplyr::summarise(Value = (dplyr::n() / total_obs) * 100)
      )
      
      ggplot2::ggplot(summary_df, ggplot2::aes(x = Year, y = Value)) +
        ggplot2::geom_col(fill = temp_plot_color) +
        ggplot2::labs(title = input$select_var_input,
                      x = "Year",
                      y = input$count_function_input) +
        ggplot2::scale_y_continuous(expand = c(0.01,0.01)) +
        temporal_plot_theme
    })
    
    # Plot 3: Bar chart with summary stats by year
    output$stat_summary_plot <- renderPlot({
      validate(need(input$select_var_input != "", "Select a variable to generate plots."))
      validate(need(!is.null(rv_tmp_main()$Date), 
                    "A date column is required. Please go to the 'Select variables' tab and 
                    choose a date column first.")
      )
      
      # Prep data for plotting
      df <- rv_tmp_main() %>% dplyr::mutate(Year = lubridate::year(Date))
      summary_df <- switch(
        input$stat_function_input,
        "Mean" = df %>% 
          dplyr::group_by(Year) %>% 
          dplyr::summarise(Value = mean(Var, na.rm = TRUE)),
        "Median" = df %>% 
          dplyr::group_by(Year) %>% 
          dplyr::summarise(Value = median(Var, na.rm = TRUE)),
        "Min" = df %>% 
          dplyr::group_by(Year) %>% 
          dplyr::summarise(Value = min(Var, na.rm = TRUE)),
        "Max" = df %>% 
          dplyr::group_by(Year) %>% 
          dplyr::summarise(Value = max(Var, na.rm = TRUE)),
        "Sum" = df %>% 
          dplyr::group_by(Year) %>% 
          dplyr::summarise(Value = sum(Var, na.rm = TRUE)),
      )
      
      ggplot2::ggplot(summary_df, ggplot2::aes(x = Year, y = Value)) +
        ggplot2::geom_col(fill = temp_plot_color) +
        ggplot2::labs(title = input$select_var_input,
                      x = "Year",
                      y = input$stat_function_input) +
        ggplot2::scale_y_continuous(expand = c(0.01,0.01)) +
        temporal_plot_theme
    })
  })
}

# UI ----------------------------------------------------------------------------------------------
#' temp_plots_ui
#'
#' @description The UI-side function for the temporal plots module. It defines the 
#'              layout of the user interface elements, including input controls 
#'              and plot containers, using the bslib package for a card-based layout.
#'
#' @param id A character string that is unique to this module instance.
#'
#' @return A tagList containing the UI elements for the preview data module.
temp_plots_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    bslib::card(
      bslib::card_header("Variable over time"),
      bslib::card_body(
        selectInput(ns("select_var_input"),
                    label = "Select a variable to plot:",
                    choices = NULL),
        fluidRow(
          column(12,
                 shinycssloaders::withSpinner(
                   plotOutput(ns("scatter_plot"), height = "33vh"), type = 6)
          )
        )
      )
    ),
    
    bslib::card(
      bslib::card_header("Annual summaries"),
      bslib::card_body(
        fluidRow(
          column(6,
                 selectInput(ns("count_function_input"),
                             label = NULL,
                             choices = c("No. observations",
                                         "% of total observations")),
                 shinycssloaders::withSpinner(
                   plotOutput(ns("count_summary_plot"), height = "33vh"), type = 6)
          ),
          column(6,
                 selectInput(ns("stat_function_input"),
                             label = NULL,
                             choices = c("Mean", "Median", "Min", "Max", "Sum")),
                 shinycssloaders::withSpinner(
                   plotOutput(ns("stat_summary_plot"), height = "33vh"), type = 6)
          )
        )      
      )
    )
  )
}
