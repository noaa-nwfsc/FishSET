# =================================================================================================
# File: new_r_expression_module.R
# Description: This module defines the UI and server logic for the R expression function for 
# computing new variables. 
#
# Authors: Anna Abelman, Paul Carvalho
# Date created: 10/21/2025
# Dependencies: shiny, DT
# Notes: This module is used within compute_new_var_module.R
# =================================================================================================

# Server ------------------------------------------------------------------------------------------
#' new_r_express_server
#'
#' @description Defines the server-side logic for the r expression radio button. It allows the 
#' users to select the data frame they wish to manipulate and write the R code. The users then see
#' a preview of the data with the changes and they can save the results.
#'
#' @param id A character string that is unique to this module instance.
#' @param rv_data_load_error Tracking errors with loading data
#' @param rv_project_name A reactive value containing the current project name.
#' @param rv_data A reactiveValues object containing the loaded data frames.
#'
#' @return This module does not return a value.
#' 
new_r_express_server <- function(id, rv_data_load_error,
                                 rv_project_name = rv_project_name,
                                 rv_data = rv_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Initialize reactives
    rv_preview_data  <- reactiveVal(NULL)
    
    # Dynamically create the dropdown based on available data frames
    output$select_df_ui <- renderUI({
      req(rv_data)
      
      data_list <- reactiveValuesToList(rv_data)
      
      valid_data <- Filter(is.data.frame, data_list)
      
      df_names <- names(valid_data)
      req(length(df_names) > 0)
      
      selectInput(ns("selected_df_input"), "Select data table to modify", choices = df_names)
    })
    # Observer to update preview when a new data frame is selected
    observeEvent(input$selected_df_input, {
      req(input$selected_df_input)
      
      # Get the selected data frame
      selected_df_name <- input$selected_df_input
      df_to_show <- rv_data[[selected_df_name]]
      
      # Update the reactive value with the selected data
      rv_preview_data(df_to_show)
      
      # Show the preview container
      shinyjs::show("preview_container")
      
      # Hide any previous success messages
      shinyjs::hide("save_success_container")
    })
    
    # Observer for the 'Preview Changes' button
    observeEvent(input$run_r_expr_btn, {
      req(input$r_expr_input, input$selected_df_input)
      shinyjs::hide("preview_container")
      shinyjs::hide("save_success_container") # Hide any previous success messages
      
      
      # Get the selected data frame
      selected_df_name <- input$selected_df_input
      df_to_modify <- rv_data[[selected_df_name]]
      
      result <- tryCatch({
        # Create a temporary environment for safe evaluation.
        # This lets the user use a consistent placeholder 'df' in their code.
        eval_env <- new.env(parent = globalenv())
        assign("df", df_to_modify, envir = eval_env)
        
        eval(parse(text = input$r_expr_input), envir = eval_env)
      },
      error = function(e) {
        showNotification(paste("Error:", e$message), type = "error", duration = 10)
        return(NULL)
      })
      
      if (is.data.frame(result)) {
        rv_preview_data(result)
        shinyjs::show("preview_container")
      } else if (!is.null(result)) {
        showNotification("Expression did not return a data frame.", type = "warning")
      }
    })
    
    # Dynamically render the preview UI (table and buttons)
    output$preview_ui <- renderUI({
      req(rv_preview_data())
      tagList(
        h4("Preview Table"),
        DT::dataTableOutput(ns("preview_table")),
        div(
          style = "margin-top: 10px;",
          actionButton(ns("save_changes_btn"), "Save",
                       class = "btn-success", width = "25%",
                       icon = icon(name="upload", 
                                   lib="font-awesome")),
          actionButton(ns("cancel_changes_btn"), "Cancel",
                       class = "btn-danger", width = "25%")
        )
      )
    })
    
    # Render the data table for the preview UI.
    output$preview_table <- DT::renderDataTable({
      req(rv_preview_data())
      DT::datatable(
        rv_preview_data(),
        options = list(scrollX = TRUE, pageLength = 5),
        rownames = FALSE
        
      )
    })
    
    # Observer for the 'Save' button.
    observeEvent(input$save_changes_btn, {
      req(rv_preview_data(), input$selected_df_input)
      req(rv_project_name()$value)
      
      #  Use the selected name to update the correct data frame
      selected_df_name <- input$selected_df_input
      rv_data[[selected_df_name]] <- rv_preview_data()
      
      # Load to fishset DB
      if(selected_df_name == "main"){
        load_maindata(dat = rv_data[[selected_df_name]], project = rv_project_name()$value, 
                      over_write = TRUE)
      } else if(selected_df_name == "spat"){
        load_spatial(dat = rv_data[[selected_df_name]], project = rv_project_name()$value, 
                     over_write = TRUE)
      } else if(selected_df_name == "port"){
        load_port(dat = rv_data[[selected_df_name]], project = rv_project_name()$value, 
                  over_write = TRUE)
      }else if(selected_df_name == "grid"){
        load_grid(dat = rv_data[[selected_df_name]], project = rv_project_name()$value, 
                  over_write = TRUE)
      }else if(selected_df_name == "aux"){
        load_aux(dat = rv_data[[selected_df_name]], project = rv_project_name()$value, 
                 over_write = TRUE)
      }
      
      success_message <- paste0("Data frame '", strong(selected_df_name),
                                "' has been updated successfully!")
      
      shinyjs::html(id = "success_text", html = success_message, add = FALSE)
      shinyjs::show("save_success_container")
    })
    
    # Observer for the 'Cancel' button
    observeEvent(input$cancel_changes_btn, {
      shinyjs::hide("preview_container")
      rv_preview_data(NULL)
      
    })
  }
  )
}

# UI ----------------------------------------------------------------------------------------------
#' new_r_express_ui
#'
#' @description Creates the main panel UI for the R expression radio button. It has the users select
#' a data frame, write R code in a text box, and provides examples of fishset functions they can 
#' use.
#'
#' @param id A character string that is unique to this module instance.
#'
#' @return A tagList containing the UI elements for the preview data module.
new_r_express_ui <- function(id){
  ns <- NS(id)
  tagList(
    bslib::layout_column_wrap(
      fill = TRUE,
      width = 1/2,
      bslib::card(
        bslib::card_header("Data Modifications"),
        bslib::card_body(
          # UI output for the dynamic dropdown
          uiOutput(ns("select_df_ui")),
          textAreaInput(
            ns("r_expr_input"),
            label = "Enter an R expression (use 'df' to refer to the selected data)",
            value = "df %>% ",   # The value now uses 'df' as the placeholder
            rows = 2, width = "100%"),
          actionButton(ns("run_r_expr_btn"), "Run", class = "btn-primary",
                       width = "100%"),
          shinyjs::hidden(
            div(
              id = ns("save_success_container"),
              role = "alert",
              style = "color: green; display: none; font-size: 20px;",
              span(id = ns("success_text")) 
            )
          )
        )),
      bslib::card(
        bslib::card_header("Examples"),
        h6("To learn more about how to use dplyr and the different data transformations, check
           out the", 
           tags$a("dplyr::cheatsheet",
                  href = "https://nyu-cdsc.github.io/learningr/assets/data-transformation.pdf",
                  target = "_blank")),
        bslib::accordion(
          class = "accordion-flush",
          bslib::accordion_panel(
            'Artithmetic functions',
            h6("Numeric function: "),
            tags$li(
              code("df %>% dplyr::mutate(new_variable = var1 + var2)")),
            h6("CPUE: "),
            tags$li(
              code("cpue(df, project, xWeight, xTime, name = 'cpue')")),
            tags$li(
              code("df %>% dplyr::mutate(cpue = (catch_variable/time_variable))")
            )),
          bslib::accordion_panel(
            'Dummy variables',
            p("A dummy variable is a numerical variable that uses 0 or 1 to represent the 
              presence (1) or absence (0) of a categorical quality."),
            tags$li(
              code('dummy_num(df, project, var, value, opts = "more_less", name = "dummy_var")')),
            tags$li(
              code("df %>% dplyr::mutate(dummy_var = ifelse(year == 2009, 1,0))")
            ),
            tags$li(
              code("model.matrix(~ 0 + variable, data = df) %>% cbind(., df)")
            )), 
          bslib::accordion_panel(
            'Temporal variables',
            h6("Transform date: "),
            tags$li(
              code('temporal_mod(df, project, x, define_format = "%Y%m%d", name)')),
            h6("Duration of time variable: "),
            tags$li(
              code("create_duration(df, project, start, end, units = 'minute', name = 'TripDur')"))
          ))
      )
    ),
    shinyjs::hidden(
      div(id = ns("preview_container"),
          bslib::card(uiOutput(ns("preview_ui")))
      )
    )
  )
}
