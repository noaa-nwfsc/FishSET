# =================================================================================================
# File: correlation_module.R
# Description: A Shiny module for analyzing and visualizing the correlation between multiple
#              numeric variables. It provides a heatmap, pairs plot, and a data table of the
#              correlation matrix.
#
# Authors: Paul Carvalho, Anna Abelman
# Date created: 9/18/2025
# Dependencies: shiny, bslib, dplyr, ggplot2, tidyr, tibble, DT, shinycssloaders
# Notes: The module allows for dynamic selection of variables and visual exploration of
#        their relationships.
# =================================================================================================


# Server ------------------------------------------------------------------------------------------
#' correlation_server
#'
#' @description The server-side logic for the correlation analysis module. It handles the
#'              dynamic population of variable choices, calculates the correlation matrix
#'              based on user selections, and renders a heatmap, pairs plot, and data table.
#'
#' @param id A character string that is unique to this module instance.
#' @param rv_folderpath A reactive value containing the path to the project folder.
#' @param rv_project_name A reactive value containing the current project name.
#' @param rv_data A reactiveValues object containing the list of data frames to be displayed.
#'
#' @return This module does not return a value.
correlation_server <- function(id, rv_folderpath, rv_project_name, rv_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Dyanmically populate the variable selection input
    observe({
      req(rv_data$main)
      
      # Get names of numeric variables
      numeric_vars <- names(rv_data$main[sapply(rv_data$main, is.numeric)])
      
      # Exit if no numeric variables are available
      if (length(numeric_vars) == 0) {
        updateSelectInput(session, "select_vars_input", choices = character(0))
        return()
      }
      
      # Set the initial selection to the first 5 variables (or fewer if not enough)
      initial_selection <- head(numeric_vars, 5)
      
      updateSelectInput(session, 
                        "select_vars_input", 
                        choices = numeric_vars,
                        selected = initial_selection)
    })
    
    # Create a reactive expression for the selected data
    selected_data <- reactive({
      # Require at least two variables to proceed
      validate(
        need(length(input$select_vars_input) >= 2, 
             "Please select at least two numeric variables.")
      )
      
      # Subset the data to include only the selected numeric variables
      rv_data$main %>%
        dplyr::select(all_of(input$select_vars_input))
    })
    
    # Create a reactive expression for the correlation matrix
    corr_matrix <- reactive({
      # Calculate the correlation matrix
      cor(selected_data(), use = "pairwise.complete.obs")
    })
    
    # This calculation will be shared by both the heatmap and the pairs plot.
    dynamic_plot_height <- reactive({
      req(input$select_vars_input)
      num_vars <- length(input$select_vars_input)
      
      # Set a minimum height and add pixels for each variable.
      max(400, 150 + num_vars * 35) 
    })
    
    # Render the correlation heatmap
    output$corr_heatmap <- renderPlot({
      c_matrix <- corr_matrix()
      
      # Melt the matrix into a long format for ggplot2 using tidyr
      melted_cormat <- c_matrix %>%
        as.data.frame() %>%
        tibble::rownames_to_column(var = "Var1") %>%
        tidyr::pivot_longer(
          cols = -Var1,
          names_to = "Var2",
          values_to = "value"
        )
      
      # Preserve the order of variables from the input selection
      variable_order <- input$select_vars_input
      melted_cormat$Var1 <- factor(melted_cormat$Var1, levels = variable_order)
      melted_cormat$Var2 <- factor(melted_cormat$Var2, levels = variable_order)
      
      # Create the base heatmap plot
      ggplot2::ggplot(data = melted_cormat, aes(Var1, Var2, fill = value)) +
        geom_tile(color = "white") +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.border = element_blank(),
          axis.ticks = element_blank()
        ) +
        scale_y_discrete(limits = rev) +
        coord_fixed() + # Ensure tiles are square
        geom_text(aes(label = round(value, 2)), color = "black", size = 4) +
        ggplot2::scale_fill_viridis_c(name="Correlation", limit = c(-1,1))
    }, height = function() dynamic_plot_height())
    
    # Render the pairs plot using base R graphics
    output$corr_pairs_plot <- renderPlot({
      # Custom panel functions for the pairs plot
      # panel.hist puts histograms on the diagonal
      panel.hist <- function(x, ...) {
        usr <- par("usr")
        par(usr = c(usr[1:2], 0, 1.5))
        h <- hist(x, plot = FALSE)
        breaks <- h$breaks
        nB <- length(breaks)
        y <- h$counts 
        y <- y/max(y)
        rect(breaks[-nB], 0, breaks[-1], y, col = "lightblue", ...)
      }
      
      # panel.cor puts correlation coefficients on the upper panels
      panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
        usr <- par("usr")
        par(usr = c(0, 1, 0, 1))
        r <- cor(x, y, use = "pairwise.complete.obs")
        txt <- format(c(r, 0.123456789), digits = digits)[1]
        txt <- paste0(prefix, txt)
        if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
        # Scale text size by the absolute value of the correlation
        text(0.5, 0.5, txt, cex = cex.cor * (0.5 + abs(r)) / 1.5)
      }
      
      # Use the pairs function to create the plot
      graphics::pairs(selected_data(), 
                      lower.panel = panel.smooth, 
                      upper.panel = panel.cor,
                      diag.panel = panel.hist)
    }, height = function() dynamic_plot_height())
    
    # Render the correlation matrix as a data table
    output$corr_table <- DT::renderDataTable({
      df_corr <- as.data.frame(round(corr_matrix(), 3))
      
      DT::datatable(df_corr,
                    options = list(
                      scrollY = "300px",
                      scrollX = TRUE,
                      scrollCollapse = TRUE,
                      paging = FALSE,
                      searching = FALSE,
                      lengthChange = FALSE
                    ),
                    rownames = TRUE)
    })
  })
}

# UI ----------------------------------------------------------------------------------------------
#' correlation_ui
#'
#' @description The UI-side function for the correlation analysis module. It defines the layout,
#'              including the variable selection input and placeholders for the heatmap, 
#'              pairs plot, and data table outputs.
#'
#' @param id A character string that is unique to this module instance.
#'
#' @return A tagList containing the UI elements for the preview data module.
correlation_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    bslib::card(
      bslib::card_header("Correlation analysis"),
      bslib::card_body(
        fluidRow(
          column(8,
                 selectInput(ns("select_vars_input"),
                             label = "Select variables to analyze:",
                             choices = NULL,
                             multiple = TRUE,
                             width = "100%")
          ),
          column(4,
                 radioButtons(ns("plot_type_input"),
                              label = "Plot type:",
                              choices = c("Heatmap", "Pairs Plot"),
                              selected = "Heatmap",
                              inline = TRUE)
          )
        ),
        
        div(
          style = "max-height: 65vh; overflow-y: auto;",
          # Conditional UI for Heatmap
          conditionalPanel(
            condition = "input.plot_type_input == 'Heatmap'",
            ns = ns,
            h5("Correlation Heatmap"),
            shinycssloaders::withSpinner(
              plotOutput(ns("corr_heatmap")), type = 6
            )
          ),
          
          # Conditional UI for Pairs Plot
          conditionalPanel(
            condition = "input.plot_type_input == 'Pairs Plot'",
            ns = ns,
            h5("Pairs Plot"),
            shinycssloaders::withSpinner(
              plotOutput(ns("corr_pairs_plot")), type = 6
            )
          )
        ), 
        
        # Data table output
        h5("Correlation Matrix"),
        shinycssloaders::withSpinner(
          DT::dataTableOutput(ns("corr_table")), type = 6
        )
      )
    )
  )
}
