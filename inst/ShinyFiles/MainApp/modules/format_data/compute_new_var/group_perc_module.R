
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
#' @description Server logic for the group percentage module.
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
    temp_data <- reactiveVal(NULL) # Holds the temporary data for the preview modal
    new_col_name <- reactiveVal(NULL) # Holds the name of the new column
    
    observe({
      req(rv_data$main)
      
      all_cols <- colnames(rv_data$main)
      num_cols <- numeric_cols(rv_data$main) # Assumes numeric_cols() is a helper
      
      # --- THIS IS THE FIX ---
      # 1. Store the *current* selections before they are reset
      current_id_group <- input$perc_id_grp_input
      current_group <- input$perc_grp_input
      current_value <- input$perc_value_input
      
      # 2. Keep only the selections that are still valid in the new data
      #    (This prevents errors if a column was somehow removed)
      valid_id_group <- current_id_group[current_id_group %in% all_cols]
      valid_group <- current_group[current_group %in% all_cols]
      valid_value <- current_value[current_value %in% num_cols]
      
      # 3. Pass the valid selections back to the `selected` argument
      updateSelectInput(session, "perc_id_grp_input", 
                        choices = all_cols, 
                        selected = valid_id_group)
      
      updateSelectInput(session, "perc_grp_input", 
                        choices = c("None (use total)" = "", all_cols), 
                        selected = valid_group)
      
      updateSelectInput(session, "perc_value_input", 
                        choices = num_cols,
                        selected = valid_value)
      # --- END FIX ---
      
    })
    
    # Triggered when the "Run & Preview" button is clicked
    observeEvent(input$perc_grp_btn, {
      req(rv_data$main)
      req(input$perc_id_grp_input, input$perc_value_input)
      req(input$perc_name_input != "", message = "Please provide a name for the new variable.")
      req(rv_project_name())
      
      # Get inputs
      col_name <- input$perc_name_input
      sec_group <- input$perc_grp_input
      
      # Handle NULL selection for secondary group
      if (is.null(sec_group) || sec_group == "") {
        sec_group <- NULL
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
          id_group = input$perc_id_grp_input,
          group = sec_group, 
          value = input$perc_value_input,
          name = col_name, 
          create_group_ID = input$perc_id_col, 
          drop_total_col = input$perc_drop_input
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
    output$preview_table <- DT::renderDT({
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
      
      plot_data <- rv_data$main
      column_name <- new_col_name()
      
      ggplot2::ggplot(plot_data, ggplot2::aes(y = .data[[column_name]], x = "")) +
        # Add jittered points, slightly transparent
        ggplot2::geom_jitter(width = 0.1, alpha = 0.2, height = 0, color = "darkred") +
        # Add the boxplot, make it transparent
        ggplot2::geom_boxplot(fill = NA, color = "black", outlier.shape = NA) +
        ggplot2::coord_flip() + # Flip to horizontal
        ggplot2::theme_minimal(base_size = 14) +
        ggplot2::labs(
          title = "Distribution of Percentage Variable",
          y = new_col_name(),
          x = ""
        ) +
        ggplot2::theme(axis.text.y = ggplot2::element_blank(), 
                       axis.ticks.y = ggplot2::element_blank())
    })
    
    
    # Summary Table
    output$summary_table <- DT::renderDataTable({
      req(rv_data$main, new_col_name())
      validate(
        need(new_col_name() %in% names(rv_data$main), "Column not found.")
      )
      
      summary_df <- rv_data$main %>%
        dplyr::summarise(
          dplyr::across(
            !!rlang::sym(new_col_name()), 
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
      
      
      DT::datatable(
        summary_df,
        options = list(pageLength = 7, searching = FALSE, dom = 't'), # 't' = table only
        rownames = FALSE,
        caption = "Summary Statistics for New Variable"
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
    bslib::card(
      class="card-overflow", 
      bslib::card_body(
        class="card-overflow", 
        bslib::layout_column_wrap(
          fill = TRUE,
          width = 1/3,
          height = "100px", # Set a fixed height for the input rows
          selectInput(ns('perc_id_grp_input'), 
                      'Select primary grouping variable(s)',
                      choices = NULL, multiple = TRUE),
          selectInput(ns('perc_grp_input'), 
                      'Select secondary grouping variable(s)',
                      choices = NULL, multiple = TRUE),
          selectInput(ns('perc_value_input'), 
                      'Select numeric variable',
                      choices = NULL),
          textInput(ns('perc_name_input'), 
                    'New variable name',
                    value = "group_perc"),
          checkboxInput(ns('perc_id_col'), 
                        'Create an ID variable', 
                        value = FALSE),
          checkboxInput(ns('perc_drop_input'), 
                        'Drop total columns', 
                        value = FALSE)
        ),
        actionButton(ns("perc_grp_btn"),
                     "Run & Preview",
                     icon = icon("chart-simple"),
                     class = "btn-primary",
                     width = "100%")
      )
    ),
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




