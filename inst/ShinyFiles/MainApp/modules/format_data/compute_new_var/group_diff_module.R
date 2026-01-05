# =================================================================================================
# File: group_diff_module.R
# Description: This module provides the UI and server logic for creating a within-group
#              lagged difference variable.
#
# Authors:   Anna Abelman, Paul Carvalho 
# Date created: 10/24/2025
# Dependencies: shiny, DT, shinyjs, dplyr, ggplot2, tidyr, rlang
# Notes: This module interacts with the main reactive data values (rv_data) and saved
#        project variables. (Updated by Gemini)
# =================================================================================================

# Server ------------------------------------------------------------------------------------------
#' group_diff_server
#'
#' @description Server logic for the within-group difference module.
#'
#' @param id id A character string that is unique to this module instance.
#' @param rv_project_name A reactive value containing the current project name.
#' @param rv_data A reactiveValues object containing the loaded data frames.
#'
#' @return This module does not return a value but modifies the main data frame (rv_data$main).
group_diff_server <- function(id, rv_project_name, rv_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Initialize reactives
    rv_temp_data <- reactiveVal(NULL) # Holds the temporary data for the preview modal
    rv_new_col_name <- reactiveVal(NULL) # Holds the name of the new column
    rv_grp_name <- reactiveVal(NULL) # Holds the name of grouping variable(s)
    rv_value_name <- reactiveVal(NULL) # Holds the name of value variable
    rv_sort_by_name <- reactiveVal(NULL) # Holds the name of the sort_by variable
    
    observe({
      req(rv_data$main)
      
      # Populate select inputs with choices from the main data
      updateSelectInput(session,'diff_sort_input',
                        choices = date_cols(rv_data$main))
      updateSelectInput(session,'diff_grp_input',
                        choices = colnames(rv_data$main))
      updateSelectInput(session,'diff_value_input',
                        choices = numeric_cols(rv_data$main))
    })
    
    # Triggered when the "Run & Preview" button is clicked
    observeEvent(input$run_diff_btn, {
      req(rv_data$main)
      req(input$diff_sort_input, input$diff_value_input)
      req(input$diff_grp_input, message = "Error: Please select at least one grouping variable.")
      req(rv_project_name())
      
      # Show spinner
      shinyjs::show("group_diff_spinner_container")
      on.exit(shinyjs::hide("group_diff_spinner_container"), add = TRUE)
      
      # Get inputs
      col_name <- input$diff_name_input
      
      # Check if the column name already exists
      if (col_name %in% names(rv_data$main)) {
        showNotification(
          paste("Error: Column '", col_name, "' already exists. Please choose a different name."),
          type = "error"
        )
        return(NULL) # Stop execution
      }
      
      # Run the function (with error handling)
      result_df <- tryCatch({
        group_diff(
          dat = rv_data$main,
          project = rv_project_name()$value,
          group = input$diff_grp_input, 
          sort_by = input$diff_sort_input,
          value = input$diff_value_input,
          name = col_name,
          lag = input$diff_lag_input, 
          include_total_col = input$diff_drop_input
        )
        
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
        return(NULL)
      })
      
      # Hide spinner
      shinyjs::hide("group_diff_spinner_container")
      
      # If successful, show modal
      if (!is.null(result_df)) {
        rv_temp_data(result_df) # Store data
        rv_new_col_name(col_name) # Store name
        rv_grp_name(input$diff_grp_input) # Store group
        rv_value_name(input$diff_value_input) # Store value
        rv_sort_by_name(input$diff_sort_input) # Store date
        
        # Show Modal Dialog
        showModal(modalDialog(
          title = "Preview New Data",
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
    output$preview_table <- DT::renderDT({
      req(rv_temp_data())
      datatable(
        rv_temp_data(),
        options = list(scrollX = TRUE, pageLength = 5, searching = TRUE),
      )
    })
    
    
    observeEvent(input$save_btn, {
      req(rv_temp_data())
      req(rv_project_name())
      
      # Save data to the main reactive value
      rv_data$main <- rv_temp_data()
      
      # Save changes to SQLite database
      table_save(table = rv_data$main,
                 project = rv_project_name()$value,
                 type = "main")
      
      # Close the modal
      removeModal()
      
      # Show success notification
      showNotification(
        paste("New column '", rv_new_col_name(), "' saved successfully."),
        type = "message"
      )
      shinyjs::show("summary_wrapper")
      
    })
    
    
    # Summary Table: Volatility
    output$summary_table <- DT::renderDataTable({
      req(rv_data$main, rv_new_col_name(), rv_sort_by_name())
      validate(
        need(rv_new_col_name() %in% names(rv_data$main), "Column not found.")
      )
      
      col_name <- rv_new_col_name()
      group_name <- rv_grp_name() 
      sort_name <- rv_sort_by_name()
      value_name <- rv_value_name()
      
      table_caption <- htmltools::tags$caption(
        style = "caption-side: top; text-align: left;",
        htmltools::strong(
          paste0(
            "Volatility Summary for: ", col_name, 
            " (based on '", value_name, "')"
          )
        )
      )
      
      # Base data, arranged by the sort variable
      base_data <- rv_data$main %>%
        dplyr::arrange(.data[[sort_name]])
      
      summary_df <- base_data %>%
        dplyr::group_by(.data[[group_name]]) %>%
        # Filter row 1 *within each group* to remove padding
        dplyr::filter(dplyr::row_number() > 1) %>%
        dplyr::summarise(
          n_observations = dplyr::n(),
          mean_diff = mean(.data[[col_name]], na.rm = TRUE),
          median_diff = median(.data[[col_name]], na.rm = TRUE),
          std_dev_diff = sd(.data[[col_name]], na.rm = TRUE),
          min_diff = min(.data[[col_name]], na.rm = TRUE),
          max_diff = max(.data[[col_name]], na.rm = TRUE),
          pct_positive = mean(.data[[col_name]] > 0, na.rm = TRUE) * 100,
          pct_negative = mean(.data[[col_name]] < 0, na.rm = TRUE) * 100,
          .groups = "drop"
        ) %>%
        dplyr::arrange(desc(std_dev_diff)) # Sort by most volatile
      
      
      DT::datatable(
        summary_df,
        options = list(pageLength = 10, searching = TRUE),
        rownames = FALSE,
        caption = table_caption
      ) %>% DT::formatRound(
        # Round all numeric columns except the group name and n_changes
        columns = setdiff(names(summary_df), c(group_name[1], "n_changes", "Statistic")), 
        digits = 2
      )
    })
    
  })
  
}


# UI ----------------------------------------------------------------------------------------------
#' group_diff_ui
#'
#' @description UI for the group_diff module.
#'
#' @param id A character string that is unique to this module instance.
#'
#' @return A tagList containing the UI elements for the group_diff module.
group_diff_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    shinyjs::useShinyjs(),
    div(
      bslib::card(
        class="card-overflow",
        bslib::card_body(
          class="card-overflow",
          p("This function calculates the lagged difference of a variable for specified groups. 
            First the function sums the numeric variable for each unique date/group combination.
            Then within each group, the difference between the current summed value and the summed 
            value from the previous date is calculated. Use the checkbox below to return a column 
            for date/group sums in addition to the lagged difference column."),
          hr(),
          bslib::layout_column_wrap(
            fill = TRUE,
            width = 1/3,
            selectInput(ns('diff_sort_input'), 'Select date variable',
                        choices = NULL),
            selectInput(ns('diff_grp_input'), 'Select grouping variable',
                        choices = NULL, multiple = FALSE),
            selectInput(ns('diff_value_input'), 'Select numeric variable',
                        choices = NULL)
          ),
          bslib::layout_column_wrap(
            fill = TRUE,
            width = 1/3,
            textInput(ns('diff_name_input'),
                      'New variable name',
                      value = "group_diff"),
            numericInput(ns('diff_lag_input'), 
                         span(
                           style = "white-space: wrap; display: inline-flex; align-items: center;",
                           HTML('Lag length &nbsp;'),
                           bslib::tooltip(
                             shiny::icon("circle-info", `aria-label` = "More information"),
                             HTML("Increasing the lag length changes the calculation to find the
                                  difference between the current date/group sum value and the value
                                  from X rows prior (based on dates in the dataset)."))),
                         value = 1, min = 1, step = 1),
            tags$div(class = "form-group",
                     tags$label(HTML("&nbsp;")), # Empty label for spacing
                     checkboxInput(
                       ns('diff_drop_input'), 
                       span(
                         style = "white-space: wrap; display: inline-flex; align-items: center;",
                         HTML('Include total column &nbsp;'),
                         bslib::tooltip(
                           shiny::icon("circle-info", `aria-label` = "More information"),
                           HTML("Selecting this option will add a 'group_total' column to the 
                                main data table. This column represents the summed variable
                                for each date/group combination."),
                           options = list(delay = list(show = 0, hide = 850))
                         ), 
                         value = FALSE)))
          ),
          actionButton(ns("run_diff_btn"),
                       "Run & Preview",
                       icon = icon("chart-simple"),
                       class = "btn-primary",
                       width = "100%")
        )
      ),
      
      shinyjs::hidden(
        div(id = ns("summary_wrapper"),
            bslib::card(
              bslib::card_header("Summary Table"),
              shinycssloaders::withSpinner(DT::DTOutput(ns("summary_table")))
              
            )
        )
      ),
      
      # Spinner container
      div(id = ns("group_diff_spinner_container"),
          style = "display: none;",
          spinner_ui(ns("group_diff_spinner"),
                     spinner_type = "circle",
                     size = "large",
                     message = "Calculating group difference variable...", 
                     overlay = TRUE)
      )
    )
  )
  
}