# =================================================================================================
# File: assign_quantiles_module.R
# Description: This module provides the UI and server logic for creating a factor variable from
# numeric data that is split into categories based on selected quantile categories
#
# Authors:  Anna Abelman, Paul Carvalho
# Date created: 10/22/2025
# Dependencies: shiny, DT, shinyjs, dplyr
# Notes: This module interacts with the main reactive data values (rv_data) and saved
#        project variables.
# =================================================================================================


# Server ------------------------------------------------------------------------------------------
#' assign_quantiles_server
#'
#' @description Server logic for the assign quantiles module.
#'
#' @param id id A character string that is unique to this module instance.
#' @param rv_project_name A reactive value containing the current project name.
#' @param rv_data A reactiveValues object containing the loaded data frames.
#'
#' @return This module does not return a value but modifies the main data frame (rv_data$main).
assign_quantiles_server <- function(id, rv_project_name, rv_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Initialize reactives
    temp_data <- reactiveVal(NULL) # Holds the temporary data for the preview modal
    new_col_name <- reactiveVal(NULL) # Holds the name of the new column
    
    # Update 'select variable' choices based on numeric columns in main data
    observe({
      req(rv_data$main)
      choices <- numeric_cols(rv_data$main) # From helpers.R
      updateSelectInput(session, "quant_var_input", choices = choices)
    })
    
    # Triggered when the "Run & Preview" button is clicked
    observeEvent(input$run_quant_btn, {
      req(rv_data$main)
      req(input$quant_var_input)
      req(rv_project_name())
      
      # Get inputs
      var_to_quant <- input$quant_var_input
      quant_cat <- as.numeric(input$quant_cat_input)
      
      # Determine new column name
      col_name <- input$quant_name_input
      if (col_name == "") {
        col_name <- paste0(var_to_quant, "_quant")
      }
      
      # Check if the column name already exists
      if (col_name %in% names(rv_data$main)) {
        showNotification(
          paste("Error: Column '", col_name, "' already exists. Please choose a different name."),
          type = "error"
        )
        return(NULL) # Stop execution of this observer
      }
      
      # Run the function (with error handling)
      result_df <- tryCatch({
        set_quants(
          dat = rv_data$main,
          project = rv_project_name()$value,
          x = var_to_quant,
          quant.cat = quant_cat,
          name = col_name
        )
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
        return(NULL)
      })
      
      # If successful, show modal
      if (!is.null(result_df)) {
        temp_data(result_df) # Store data in reactiveVal
        new_col_name(col_name) # Store name
        
        # Show Modal Dialog
        showModal(modalDialog(
          title = "Preview New Data",
          # Show a preview (e.g., first 5 rows)
          DT::DTOutput(ns("preview_table")),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("save_btn"), "Save", class = "btn-success")
          ),
          size = "l", # Large modal
          easyClose = TRUE
        ))
      }
    })
    
    # Render the data table *inside* the modal
    output$preview_table <- renderDT({
      req(temp_data())
      datatable(
        head(temp_data(), 5), # Preview first 5 rows
        options = list(scrollX = TRUE),
        caption = "Showing first 5 rows of preview."
      )
    })
    
    
    observeEvent(input$save_btn, {
      req(temp_data())
      req(rv_project_name())
      
      # Save data to the main reactive value
      rv_data$main <- temp_data()
      
      # Save changes to SQLite database
      table_save(table = rv_data$main,
                 project = rv_project_name()$value,
                 type = "main")
      
      # Close the modal
      removeModal()
      
      # Show success notification
      showNotification(
        paste("New column '", new_col_name(), "' saved successfully."),
        type = "message"
      )
      shinyjs::show("summary_wrapper")
      
    })
    
    # Summary Plot
    output$summary_plot <- renderPlot({
      # Only run if the new column name is present in the main data
      req(rv_data$main, new_col_name())
      validate(
        need(new_col_name() %in% names(rv_data$main), "Column not found. Please save first.")
      )
      
      # Convert to factor for correct plotting
      plot_data <- rv_data$main
      plot_data[[new_col_name()]] <- as.factor(plot_data[[new_col_name()]])
      
      column_name <- new_col_name()
      ggplot(plot_data, aes(x = .data[[column_name]])) +
        geom_bar(fill = "darkred", color = "black") +
        theme_minimal(base_size = 14) +
        labs(
          title = "Count per Quantile Category",
          x = new_col_name(),
          y = "Count"
        )
    })
    
    # Summary Table
    output$summary_table <- DT::renderDataTable({
      req(rv_data$main, new_col_name())
      validate(
        need(new_col_name() %in% names(rv_data$main), "Column not found.")
      )
      
      summary_df <-rv_data$main %>%
        count(!!sym(new_col_name())) %>%
        rename("Quantile Category" = 1, Count = 2)
      
      DT::datatable(summary_df,
                    options = list(pageLength = 10, searching = FALSE),
                    caption = "Summary: Observations per Quantile Category")
    })
    
  }) 
  
}


# UI ----------------------------------------------------------------------------------------------
#' assign_quantiles_ui
#'
#' @description UI for the assign quantiles module.
#'
#' @param id A character string that is unique to this module instance.
#'
#' @return A tagList containing the UI elements for the assign quantiles module.
assign_quantiles_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    shinyjs::useShinyjs(), 
    bslib::card(
      class="card-overflow", 
      bslib::card_body(
        class="card-overflow", 
        bslib::layout_column_wrap(
          fill = TRUE,
          width = 1/4,
          textInput(
            ns('quant_name_input'),
            tagList(
              span(
                style = "white-space: wrap; display: inline-flex; align-items: center;",
                HTML("Name of new variable &nbsp;"),
                bslib::tooltip(
                  shiny::icon("circle-info", `aria-label` = "More information"),
                  HTML("Name for the new column. Defaults to name of the function if
                                 not defined."),
                  options = list(delay = list(show = 0, hide = 850))
                )
              )
            ),
            placeholder = 'e.g., length_quant'
          ),
          selectInput(ns('quant_var_input'),
                      'Select numeric variable',
                      choices = NULL,
                      multiple = FALSE,
                      selectize = TRUE),
          selectInput(ns('quant_cat_input'), 'Quantile categories',
                      choices = c('0%, 20%, 40%, 60%, 80%, 100%' = '0.2',
                                  '0%, 25%, 50%, 75%, 100%' = '0.25',
                                  '0%, 33%, 66%, 100%' = '0.33',
                                  '0%, 10%, 50%, 90%, 100%' = '0.4')),
          tags$div(class = "form-group",
                   tags$label(HTML("&nbsp;")), # Empty label for spacing
                   actionButton(ns("run_quant_btn"),
                                "Run & Preview",
                                icon = icon("chart-simple"),
                                class = "btn-primary",
                                width = "100%")))
      )),
    shinyjs::hidden(
      div(id = ns("summary_wrapper"),
          bslib::layout_column_wrap(
            fill = TRUE,
            width = 1/2,
            bslib::card(
              bslib::card_header("Summary Plot"),
              plotOutput(ns("summary_plot"))),
            bslib::card(
              bslib::card_header("Summary Table"),
              DT::DTOutput(ns("summary_table")))
          )
          
      )
    )
  )
  
}