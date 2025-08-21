# =================================================================================================
# File: variable_class_module.R
# Description: This module displays a table showing all the variables in the primary data table, the 
#              class of the variable, and the first value. The user can update the class type 
#              (numeric, factor, character, date) for any of the variables. Before saving, the user
#              will get a preview of the first few values after the change and decide whether or not 
#              to continue.
#
# Authors: Paul Carvalho, Anna Abelman
# Date created: 8/4/2025
# Dependencies: shiny, DT
# Notes: This module is used within qaqc_module.R
# =================================================================================================

# Server ------------------------------------------------------------------------------------------
#' variable_class_server
#'
#' @description Defines the server-side logic for the changing variable class module. It populates
#' a dropdown with available variables from the primary data and one containing the available class 
#' types for the user to select. Then renders a list of the variables, original class type,
#' and first value as an interactive DT::datatable.
#'
#' @param id A character string that is unique to this module instance.
#' @param rv_project_name A reactive value containing the current project name.
#' @param rv_data A reactiveValues object containing the list of data frames to be displayed.
#'
#' @return This module does not return a value.
#' 
variable_class_server <- function(id, rv_project_name, rv_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    
    # Initialize reactives
    rv_change_var_df <- reactiveVal(NULL) # Reactive value for updated data frame with new class 
    # types
    
    # updating dropdown with primary data variables
    observe({
      req(rv_data) # Ensure data is not null
      main_data <- rv_data$main # Save static copy of main data from reactive input
      
      # If the data list is empty or NULL, clear the choices and exit.
      if (is.null(main_data) || length(main_data) == 0) {
        updateSelectInput(session, "change_class_var", choices = character(0)) # Clear choices
        return()
      }
      
      updateSelectInput(session, "change_class_var", 
        choices = names(main_data))
      
    })
    
    # Creating reactive data frame of variables, class types, and first values using rv_data$main
    change_class_tbl <- reactive({
      
      req(rv_data) # Ensure data is not null
      
        # Check that main data table exist and if not return nothing
      if (is.null(rv_data$main) || length(rv_data$main) == 0) {
        return()
      } else{
      
      first_class <- function(x) class(x)[1]
      int = t(t(vapply(rv_data$main, first_class, character(1))))
      df = matrix(as.character(1:2), nrow = nrow(int), ncol = 2, byrow = TRUE,
        dimnames = list(rownames(int), c('Variable class', 'First value')))
      df[,1] = int
      df[,2] = t(rv_data$main[1,])
      
      return(df)
      }
    })
    
    # Displaying change class table using DT
    output$change_var_table <- DT::renderDataTable( 
      change_class_tbl()
    )
    
    # Converting class and preview data in a pop up
    observeEvent(input$var_class_btn, {
      
      req(rv_project_name()) # Check to ensure reactive is available
      project_name <- rv_project_name()$value
      req(rv_data) # Ensure data is not null
      
      original_data <- rv_data$main # Create a temporary copy of the data to apply the change to
      var_to_change <- input$change_class_var # Save as static copy of variable
      new_class <- input$change_class # Save as static copy of class type
      
      # Store the original column for the "before" preview
      original_column <- original_data[[var_to_change]]
      
      #  temp_data <- original_data
      
      # Use tryCatch to gracefully handle conversion errors
      converted_column <- tryCatch({
        
        # Perform the conversion
        temp_col <- switch(new_class,
          "numeric"   = as.numeric(as.character(original_column)),
          "character" = as.character(original_column),
          "factor"    = as.factor(original_column),
          "date"      = as.Date(original_column))
        
        # Check if the conversion created all NAs where there were none before.
        # This catches failed numeric conversions from text.
        if (all(is.na(temp_col)) && !all(is.na(original_column))) {
          stop("Conversion resulted in a loss of all data (coerced to NA).")
        }
        
        temp_col # Return the successfully converted column
        
      }, error = function(e) {
        # If conversion fails, show an error modal and stop
        showModal(modalDialog(
          title = "Conversion Error",
          p("Failed to convert", code(var_to_change), "to", code(new_class), "."),
          p(em(e$message)), # This now shows our custom message for NA coercion
          footer = modalButton("Dismiss")
        ))
        return(NULL)
      })
      
      req(converted_column) # Stop if the conversion returned NULL
      
      # Update the column in our temporary data and save it to the reactive value
      original_data[[var_to_change]] <- converted_column
      rv_change_var_df(original_data)
      
      # Show the confirmation and preview modal
      showModal(modalDialog(
        title = "Preview Variable Change",
        p(strong("Variable: "), code(var_to_change)),
        p(strong("Change Class From "), code(class(original_column)[1]), strong(" to "), 
          code(new_class)),
        hr(),
        h4("Data Preview (First 6 Rows)"),
        DT::renderDataTable({
          # Create a data frame for the preview
          preview_df <- data.frame(
            Original = as.character(head(original_column)),
            New = as.character(head(converted_column)),
            stringsAsFactors = FALSE
          )
          
          # Add a summary row for NAs introduced by the conversion
          na_summary <- data.frame(
            Original = paste(sum(is.na(original_column)), "NAs"),
            New = paste(sum(is.na(converted_column)), "NAs")
          )
          rownames(na_summary) <- "--- Total NAs ---"
          
          # Combine and render the table
          DT::datatable(
            rbind(preview_df, na_summary),
            options = list(dom = 't', paging = FALSE, ordering = FALSE),
            rownames = TRUE
          )
        }),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("save_class_btn"), "Save Changes", class = "btn-success")
        ),
        size = "l",
        easyClose = TRUE
      ))
      
    }, ignoreInit = TRUE) # Essential: prevents firing when the app starts
    
    # Save to session and to FishSET db
    observeEvent(input$save_class_btn, {
      req(rv_change_var_df())
      req(rv_data) # Ensure data is not null
      req(rv_project_name()) # Check to ensure reactive is available
      project_name <- rv_project_name()$value
      
      # Update session data
      rv_data$main <- rv_change_var_df()
      
      showNotification("Data successfully saved to database!", type = "default")
      
      load_maindata(dat = rv_data$main, project = project_name, over_write = TRUE)
      
      
      # Clean up the temporary data and close the modal
      rv_change_var_df(NULL)
      removeModal()
      
      # Reset the class selector to prevent re-triggering the modal
      updateSelectInput(session, "change_class", selected = "")
      
    })
    
  }
  )
}





# UI ----------------------------------------------------------------------------------------------
#' variable_class_ui
#'
#' @description Creates the user interface for the changing variable class module. This includes
#' a dropdown for variable selection from main table, class type selection, and a placeholder for
#'  the data table.
#'
#' @param id A character string that is unique to this module instance.
#'
#' @return A tagList containing the UI elements for the preview data module.
variable_class_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    bslib::layout_column_wrap(
      fill = TRUE,
      width = 1/3,
      selectInput(ns("change_class_var"),
        label = "Select data to view:",
        choices = NULL),
      selectInput(ns("change_class"), 
        label = "Select new class type",
        choices = c('numeric', 'character', 'factor', 'date'))),
      actionButton(ns('var_class_btn'),
        label = 'Change variable classes', 
        class = "btn-secondary",
        width = "25%"),
    bslib::card(
         height = "750px",
      fill = TRUE, DT::DTOutput(ns("change_var_table"))
    )
    
  )
}
