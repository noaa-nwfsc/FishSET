# =================================================================================================
# File: group_perc_module.R
# Description: This module provides the UI and server logic for creating a within-group
#              percentage variable.
#
# Authors:  Anna Abelman, Paul Carvalho
# Date created: 10/24/2025
# Dependencies: shiny, DT, shinyjs, dplyr, ggplot2, tidyr, rlang
# Notes: This module interacts with the main reactive data values (rv_data) and saved
#        project variables.
# =================================================================================================

# Server ------------------------------------------------------------------------------------------
#' group_perc_server
#'
#' @description Server logic for the within-group percentage module.
#'
#' @param id id A character string that is unique to this module instance.
#' @param rv_project_name A reactive value containing the current project name.
#' @param rv_data A reactiveValues object containing the loaded data frames.
#'
#' @return This module does not return a value but modifies the main data frame (rv_data$main).
group_perc_server <- function(id, rv_project_name, rv_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Initialize reactives
    rv_temp_data <- reactiveVal(NULL) # Holds the temporary data for the preview modal
    rv_new_col_name <- reactiveVal(NULL) # Holds the name of the new column
    rv_grp_name <- reactiveVal(NULL) # Holds the name of grouping variable
    rv_value_name <- reactiveVal(NULL) # Holds the name value variable
    
    observe({
      req(rv_data$main)
      
      all_cols <- colnames(rv_data$main)
      num_cols <- numeric_cols(rv_data$main) 
      
      # Pass the valid selections back to the `selected` argument
      updateSelectInput(session, "perc_grp_input", 
                        choices = c("None", all_cols))
      
      updateSelectInput(session, "perc_value_input", 
                        choices = num_cols)
    })
    
    # Triggered when the "Run & Preview" button is clicked
    observeEvent(input$perc_grp_btn, {
      req(rv_data$main)
      req(input$perc_grp_input, input$perc_value_input)
      req(input$perc_name_input != "", message = "Please provide a name for the new variable.")
      req(rv_project_name())
      
      # Show spinner
      shinyjs::show("group_perc_spinner_container")
      on.exit(shinyjs::hide("group_perc_spinner_container"), add = TRUE)
      
      # Get inputs
      col_name <- input$perc_name_input
      
      if(input$perc_grp_input == "None"){
        grp_name <- NULL
      } else{
        grp_name <- input$perc_grp_input
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
        group_perc(
          dat = rv_data$main, 
          project = rv_project_name()$value,
          group = grp_name,
          value = input$perc_value_input,
          name = col_name, 
          include_total_col = input$perc_drop_input
        )
        
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
        return(NULL)
      })
      
      # Hide spinner and show modal
      shinyjs::hide("group_perc_spinner_container")
      
      # If successful, show modal
      if (!is.null(result_df)) {
        rv_temp_data(result_df) # Store data 
        rv_new_col_name(col_name) # Store name
        rv_grp_name(input$perc_grp_input)
        rv_value_name(input$perc_value_input) 
        
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
    output$preview_table <- DT::renderDT({
      req(rv_temp_data())
      datatable(
        head(rv_temp_data(), 5), # Preview first 5 rows
        options = list(scrollX = TRUE),
        caption = "Showing first 5 rows of preview."
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
    
    # Summary Plot
    output$summary_plot <- renderPlot({
      req(rv_data$main, rv_new_col_name(), rv_grp_name(), rv_value_name())
      validate(
        need(rv_new_col_name() %in% names(rv_data$main), "Column not found. Please save first.")
      )
      
      plot_data <- rv_data$main
      column_name <- rv_new_col_name()
      group_name <- rv_grp_name()
      value_name <- rv_value_name()
      
      plot_title <- paste("Distribution of", column_name)
      plot_subtitle <- paste("Calculated from:", value_name)
      
      
      if(group_name == "None"){
        ggplot2::ggplot(plot_data, ggplot2::aes(y = .data[[column_name]], x = "")) +
          # Add jittered points, slightly transparent
          ggplot2::geom_jitter(width = 0.1, alpha = 0.2, height = 0, color = "darkred") +
          # Add the boxplot, make it transparent
          ggplot2::geom_boxplot(fill = NA, color = "black", outlier.shape = NA) +
          ggplot2::coord_flip() + # Flip to horizontal
          ggplot2::theme_minimal(base_size = 14) +
          ggplot2::labs(
            title = plot_title,       
            subtitle = plot_subtitle, 
            y = column_name,
            x = ""
          ) +
          ggplot2::theme(axis.text.y = ggplot2::element_blank(), 
                         axis.ticks.y = ggplot2::element_blank())
        
      } else {
        # Define how many groups to show
        top_n_to_show <- 10 
        
        # Get the total number of unique groups *before* filtering
        total_group_count <- plot_data %>%
          dplyr::distinct(.data[[group_name]]) %>%
          nrow()
        
        # Conditionally set the caption
        plot_caption <- NULL # Default: no caption
        
        # Only set the caption if filtering will actually happen
        if (total_group_count > top_n_to_show) {
          plot_caption <- paste("Only showing the", top_n_to_show, "largest groups.")
        }
        
        # Find the names of the Top N largest groups
        top_groups <- plot_data %>%
          dplyr::count(.data[[group_name]], sort = TRUE) %>%
          dplyr::slice_head(n = top_n_to_show) %>%
          dplyr::pull(.data[[group_name]])
        
        # Filter the main data to keep only those groups
        filtered_plot_data <- plot_data %>%
          dplyr::filter(.data[[group_name]] %in% top_groups)
        
        ggplot2::ggplot(filtered_plot_data,
                        ggplot2::aes(y = .data[[column_name]],
                                     x = stats::reorder(.data[[group_name]],
                                                        .data[[group_name]], length),
                                     group = .data[[group_name]])) +
          # Add jittered points, slightly transparent
          ggplot2::geom_jitter(width = 0.1, alpha = 0.2, height = 0, color = "darkred") +
          # Add the boxplot, make it transparent
          ggplot2::geom_boxplot(fill = NA, color = "black", outlier.shape = NA) +
          ggplot2::coord_flip() + # Flip to horizontal
          ggplot2::theme_minimal(base_size = 14) +
          ggplot2::labs(
            title = plot_title,     
            subtitle = plot_subtitle, 
            caption = plot_caption,
            y = column_name,
            x = group_name
          )
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
#' group_perc_ui
#'
#' @description UI for the group_perc module.
#'
#' @param id A character string that is unique to this module instance.
#'
#' @return A tagList containing the UI elements for the group_perc module.
group_perc_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    shinyjs::useShinyjs(), 
    div(
      bslib::card(
        class="card-overflow", 
        bslib::card_body(
          class="card-overflow", 
          p("This function calculates a new column representing the within-group percentage for each
          observation (row) in the main data table. It is used to determine what portion of a 
          group's total a single row's value represents. Use the checkbox below to also include
          a column for each groups total value."),
          hr(),
          bslib::layout_column_wrap(
            fill = TRUE,
            width = 1/3,
            selectInput(ns('perc_grp_input'), 
                        'Select grouping variable',
                        choices = NULL, multiple = FALSE),
            selectInput(ns('perc_value_input'), 
                        'Select numeric variable',
                        choices = NULL),
            textInput(ns('perc_name_input'), 
                      'New variable name',
                      value = "group_perc"),
          ),
          bslib::layout_column_wrap(
            fill = TRUE,
            width = 1/3,
            actionButton(ns("perc_grp_btn"),
                         "Run & Preview",
                         icon = icon("chart-simple"),
                         class = "btn-primary",
                         width = "100%"),
            checkboxInput(ns('perc_drop_input'), 
                          span(
                            style = "white-space: wrap; display: inline-flex; align-items: center;",
                            HTML('Include total column &nbsp;'),
                            bslib::tooltip(
                              shiny::icon("circle-info", `aria-label` = "More information"),
                              HTML("The 'total_value' variable gives the total value by group."),
                              options = list(delay = list(show = 0, hide = 850))
                            ), 
                            value = FALSE)
            ),
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
      div(id = ns("group_perc_spinner_container"),
          style = "display: none;",
          spinner_ui(ns("group_perc_spinner"),
                     spinner_type = "circle",
                     size = "large",
                     message = "Calculating group percentage variable...",
                     overlay = TRUE)
      )
    )
  )
  
}




