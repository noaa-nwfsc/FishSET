# =================================================================================================
# File: group_cumsum_module.R
# Description: This module provides the UI and server logic for creating a within-group
#              cumulative sum (running total) variable.
#
# Authors:   Anna Abelman, Paul Carvalho
# Date created: 11/03/2025
# Dependencies: shiny, DT, shinyjs, dplyr, ggplot2, tidyr, rlang
# Notes: This module interacts with the main reactive data values (rv_data) and saved
#        project variables.
# =================================================================================================

# Server ------------------------------------------------------------------------------------------
#' group_cumsum_server
#'
#' @description Server logic for the within-group cumulative sum module.
#'
#' @param id id A character string that is unique to this module instance.
#' @param rv_project_name A reactive value containing the current project name.
#' @param rv_data A reactiveValues object containing the loaded data frames.
#'
#' @return This module does not return a value but modifies the main data frame (rv_data$main).
group_cumsum_server <- function(id, rv_project_name, rv_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Initialize reactives
    rv_temp_data <- reactiveVal(NULL) # Holds the temporary data for the preview modal
    rv_new_col_name <- reactiveVal(NULL) # Holds the name of the new column
    rv_grp_name <- reactiveVal(NULL) # Holds the name of grouping variable
    rv_sort_name <- reactiveVal(NULL) # Holds the name of the sorting variable
    rv_value_name <- reactiveVal(NULL) # Holds the name value variable
    
    observe({
      req(rv_data$main)
      
      all_cols <- colnames(rv_data$main)
      num_cols <- numeric_cols(rv_data$main) 
      
      # Pass the valid selections back to the `selected` argument
      updateSelectInput(session, "cumsum_grp_input", 
                        choices = c("None", all_cols))
      
      # Update new sort_by input
      updateSelectInput(session, "cumsum_sort_input", 
                        choices = date_cols(rv_data$main))
      
      updateSelectInput(session, "cumsum_value_input", 
                        choices = num_cols)
    })
    
    # Triggered when the "Run & Preview" button is clicked
    observeEvent(input$cumsum_grp_btn, {
      req(rv_data$main)
      req(input$cumsum_grp_input, input$cumsum_sort_input, input$cumsum_value_input)
      req(rv_project_name())
      
      # Show spinner
      shinyjs::show("group_cumsum_spinner_container")
      on.exit(shinyjs::hide("group_cumsum_spinner_container"), add = TRUE)
      
      # Get inputs
      col_name <- input$cumsum_name_input
      sort_name <- input$cumsum_sort_input
      
      if(input$cumsum_grp_input == "None"){
        grp_name <- NULL
      } else{
        grp_name <- input$cumsum_grp_input
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
        
        group_cumsum(
          dat = rv_data$main, 
          project = rv_project_name()$value,
          group = grp_name,
          sort_by = sort_name, 
          value = input$cumsum_value_input,
          name = col_name, 
          include_total_col = input$cumsum_drop_input
        )
        
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
        return(NULL)
      })
      
      # Hide spinner and show modal
      shinyjs::hide("group_cumsum_spinner_container")
      
      # If successful, show modal
      if (!is.null(result_df)) {
        rv_temp_data(result_df) # Store data 
        rv_new_col_name(col_name) # Store name
        rv_grp_name(input$cumsum_grp_input)
        rv_sort_name(input$cumsum_sort_input) 
        rv_value_name(input$cumsum_value_input) 
        
        # Show Modal Dialog
        showModal(modalDialog(
          title = "Preview New Data",
          # Show a preview 
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
        rv_temp_data(), # Preview data frame
        options = list(scrollX = TRUE, pageLength = 5)
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
    
    # Summary plot
    output$summary_plot <- renderPlot({
      req(rv_data$main, rv_new_col_name(), rv_grp_name(), rv_value_name(), rv_sort_name())
      validate(
        need(rv_new_col_name() %in% names(rv_data$main), "Column not found. Please save first."),
        need(rv_sort_name() %in% names(rv_data$main), "Sort column not found.")
      )
      
      plot_data <- rv_data$main
      column_name <- rv_new_col_name()
      group_name <- rv_grp_name()
      value_name <- rv_value_name()
      sort_name <- rv_sort_name()
      
      plot_title <- paste("Cumulative Sum of", value_name, "(", column_name, ")")
      plot_subtitle <- paste("Ordered by:", sort_name)
      
      
      if(group_name == "None"){
        # Plot a single line of the cumulative sum over the sort_by variable
        ggplot2::ggplot(plot_data, ggplot2::aes(x = .data[[sort_name]], 
                                                y = .data[[column_name]])) +
          ggplot2::geom_line(color = "darkred", linewidth = 1) +
          ggplot2::geom_point(alpha = 0.1, color = "darkred") +
          ggplot2::theme_minimal(base_size = 14) +
          ggplot2::labs(
            title = plot_title, 
            subtitle = plot_subtitle, 
            y = column_name,
            x = sort_name
          ) +
          ggplot2::scale_y_continuous(labels = scales::comma)
        
      } else {
        # Plot multiple lines (one for each group), showing only the top N groups
        ggplot2::ggplot(plot_data,
                        ggplot2::aes(x = .data[[sort_name]],
                                     y = .data[[column_name]],
                                     color = .data[[group_name]],
                                     group = .data[[group_name]])) +
          ggplot2::geom_line(linewidth = 0.8, alpha = 0.7) +
          ggplot2::theme_minimal(base_size = 14) +
          ggplot2::labs(
            title = plot_title, 
            subtitle = paste(plot_subtitle, "| Grouped by:", group_name), 
            y = column_name,
            x = sort_name,
            color = group_name
          ) +
          ggplot2::theme(legend.position = "bottom") +
          ggplot2::scale_y_continuous(labels = scales::comma)
      }
    })
    
    
    # Summary Table 
    output$summary_table <- DT::renderDataTable({
      req(rv_data$main, rv_new_col_name(), rv_grp_name(),
          rv_value_name())
      validate(
        need(rv_new_col_name() %in% names(rv_data$main), "Column not found.")
      )
      group_name <- rv_grp_name()
      
      table_caption <- htmltools::tags$caption(
        style = "caption-side: top; text-align: left;",
        htmltools::strong(paste("Summary Statistics for:", rv_new_col_name())),
        htmltools::br(),
        paste("Calculated from:", rv_value_name())
      )
      
      if(group_name == "None"){
        summary_df <- rv_data$main %>%
          dplyr::summarise(
            dplyr::across(
              !!rlang::sym(rv_new_col_name()), 
              .fns = list(
                Min = ~min(.x, na.rm = TRUE),
                Q1 = ~quantile(.x, 0.25, na.rm = TRUE),
                Median = ~median(.x, na.rm = TRUE),
                Mean = ~mean(.x, na.rm = TRUE),
                Q3 = ~quantile(.x, 0.75, na.rm = TRUE),
                Max = ~max(.x, na.rm = TRUE),
                NAs = ~sum(is.na(.x))
              ),
              .names = "{.fn}"
            )
          ) %>%
          tidyr::pivot_longer(cols = dplyr::everything(), 
                              names_to = "Statistic", 
                              values_to = "Value") %>% 
          dplyr::mutate(Value = round(Value, 2))
        
      }else {
        summary_df <- rv_data$main %>%
          dplyr::group_by(.data[[group_name]]) %>% 
          dplyr::summarise(
            dplyr::across(
              !!rlang::sym(rv_new_col_name()), 
              .fns = list(
                Min = ~round(min(.x, na.rm = TRUE),2),
                Q1 = ~round(quantile(.x, 0.25, na.rm = TRUE),2),
                Median = ~round(median(.x, na.rm = TRUE),2),
                Mean = ~round(mean(.x, na.rm = TRUE),2),
                Q3 = ~round(quantile(.x, 0.75, na.rm = TRUE),2),
                Max = ~round(max(.x, na.rm = TRUE),2),
                NAs = ~round(sum(is.na(.x)),2)
              ),
              .names = "{.fn}"
            )
          )
      } 
      
      DT::datatable(
        summary_df,
        options = list(pageLength = 7, searching = FALSE), 
        rownames = FALSE,
        caption = table_caption
      )
    })
    
  })  
  
}


# UI ----------------------------------------------------------------------------------------------
#' group_cumsum_ui
#'
#' @description UI for the group_cumsum module.
#'
#' @param id A character string that is unique to this module instance.
#'
#' @return A tagList containing the UI elements for the group_cumsum module.
group_cumsum_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    shinyjs::useShinyjs(), 
    div(
      bslib::card(
        class="card-overflow", 
        bslib::card_body(
          class="card-overflow", 
          p("This function calculates a new column representing the within-group cumulative sum
          (or 'running total') for each group. It requires a date variable to
          define the order of the sum. It is used to see how a value accumulates
          within a group over time or another ordered variable."),
          hr(),
          bslib::layout_column_wrap(
            fill = TRUE,
            width = 1/3,
            selectInput(ns('cumsum_sort_input'), 
                        'Select date variable',
                        choices = NULL),
            selectInput(ns('cumsum_grp_input'), 
                        'Select grouping variable',
                        choices = NULL, multiple = FALSE),
            selectInput(ns('cumsum_value_input'), 
                        'Select numeric variable',
                        choices = NULL)
          ),
          bslib::layout_column_wrap(
            fill = TRUE,
            width = 1/3,
            textInput(ns('cumsum_name_input'), 
                      'New variable name',
                      value = "group_cumsum"),
            tags$div(class = "form-group",
                     tags$label(HTML("&nbsp;")), # Empty label for spacing
                     checkboxInput(
                       ns('cumsum_drop_input'), 
                       span(
                         style = "white-space: wrap; display: inline-flex; align-items: center;",
                         HTML('Include total column &nbsp;'),
                         bslib::tooltip(
                           shiny::icon("circle-info", `aria-label` = "More information"),
                           HTML("The 'group_total' variable gives the total value by group."),
                           options = list(delay = list(show = 0, hide = 850))
                         ), 
                         value = FALSE))
                     ),
                     actionButton(ns("cumsum_grp_btn"),
                                  "Run & Preview",
                                  icon = icon("chart-simple"),
                                  class = "btn-primary",
                                  width = "100%")
            )
          ),
        ),
        shinyjs::hidden(
          div(id = ns("summary_wrapper"),
              bslib::layout_column_wrap(
                fill = TRUE,
                width = 1/2,
                heights_equal= "row",
                min_height= "600px",
                bslib::card(
                  bslib::card_header("Summary Plot"),
                  shinycssloaders::withSpinner(plotOutput(ns("summary_plot")))),
                bslib::card(
                  bslib::card_header("Summary Table"),
                  shinycssloaders::withSpinner(DT::DTOutput(ns("summary_table"))))
              )
              
          )
        ),
        # Spinner container
        div(id = ns("group_cumsum_spinner_container"),
            style = "display: none;",
            spinner_ui(ns("group_cumsum_spinner"),
                       spinner_type = "circle",
                       size = "large",
                       message = "Calculating cumulative sum variable...",
                       overlay = TRUE)
        )
      )
    )
    
    }