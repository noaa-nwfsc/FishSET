# =================================================================================================
# File: other_actions_server.R
# Description: Defines the server-side logic for the other action functions found in the sidebar 
#              including adding and saving notes in text output, create R expression to test 
#              reactive functions with output, and close app button
#
# Package: FishSET
# Authors: Anna Abelman, Paul Carvalho
# Date created: 4/23/2025
# Dependencies: - Input/output bindings defined in load_files_ui.RQ
#               - Additional input/output bindings defined in modules.
# Notes: - Avoid using hard-coded file paths; use system.file() when accessing
#          package resources.
#
# =================================================================================================

other_actions_server <- function(id, rv_project_name, rv_data_load_error, 
                                 current_tab, values = NULL){
  moduleServer(id, function(input, output, session){
    
    ns <- session$ns
    
    # Initialize reactives
    rv_manage_db <- reactiveValues(tbl = NULL) # creating reactive dataframe for existing db tables
    rv_r_expr <- reactiveValues(output = "")
    rv_r_expr_output <- reactiveVal("")
    rv_r_expr_status <- reactiveVal(FALSE)
    
    # Managing saved database tables --------------------------------------------------------------
    
    # enable/disable manage tables button based on data loading status
    observeEvent(rv_data_load_error(), {
      # Save reactive value in a static variable
      data_load_error <- rv_data_load_error()
      
      # Enable buttons if no load data errors
      shinyjs::toggleState("manage_tables_btn", 
                           condition = !data_load_error)
    })
    
    # creating modal function so it can be reused
    db_tbl_modal <- function() {
      showModal(
        modalDialog(title = "Manage Database Tables",
                    h5("Click on rows to select tables."),
                    DT::DTOutput(ns("db_tables")),
                    footer = tagList(
                      modalButton("Close"),
                      actionButton(ns("delete_db_tbl_btn"), "Next", 
                                   class = "btn-secondary")
                    ),
                    easyClose = FALSE, size = "l"))
    }
    
    # checks if projects exist and opens modal
    observeEvent(input$manage_tables_btn, {
      req(rv_project_name()) # Ensure rv_project_name is not NULL
      project_name <- rv_project_name() # Retrieve current project info
      
      #save db tables to reactive data frame
      rv_manage_db$tbl <- fishset_tables(project = project_name$value)
      
      # open modal
      db_tbl_modal()
      
      # display list of db tables in table for user to select
      output$db_tables <- DT::renderDT(
        DT::datatable(rv_manage_db$tbl)
      )
    })
    
    # open new modal that only shows selected tables that user wants to remove from project
    observeEvent(input$delete_db_tbl_btn, {
      showModal(
        modalDialog(title = "Delete these tables?",
                    tagList(
                      # if user selects table with final/raw and gives warning message
                      div(id = ns("delete_db_warn_msg"),
                          style = "display: none;",
                          h4("Warning: final and/or raw tables selected.", 
                             style = "color: red;")
                      ),
                      shinycssloaders::withSpinner(
                        DT::DTOutput(ns("db_tables_confirm")),
                        type = 6
                      )),
                    footer = tagList(
                      actionButton(ns("cancel_db_delete_tbl_btn"), "Cancel"),
                      actionButton(ns("confirm_db_delete_tbl_btn"), "Delete")
                    ),
                    easyClose = FALSE, size = "m"))
      
      # T/F if final/raw table is selected
      warn_ind <- which(grepl("final|raw", rv_manage_db$tbl$type))
      
      # show table with only selected tables to confirm before deleting
      output$db_tables_confirm <- DT::renderDT({
        
        if (length(input$db_tables_rows_selected) > 0) {
          
          DT::formatStyle(
            DT::datatable(rv_manage_db$tbl[input$db_tables_rows_selected, ]),
            "table", target = "row")
          
        } else {
          data.frame(table = "No tables selected")
        } 
      })
      
      # hide/show warning message about final/raw table type
      if (any(warn_ind %in% input$db_tables_rows_selected)) {
        shinyjs::show("delete_db_warn_msg") 
      } else{
        shinyjs::hide("delete_db_warn_msg")
      }
    })
    
    # delete selected table from project's database
    observeEvent(input$confirm_db_delete_tbl_btn, {
      
      req(rv_project_name()) # Ensure rv_project_name is not NULL
      project_name <- rv_project_name() # Retrieve current project info
      
      # get project name and selected table
      tabs_to_delete <- rv_manage_db$tbl$table[input$db_tables_rows_selected]
      
      # remove table from database
      lapply(seq_along(tabs_to_delete), function(i) {
        
        table_remove(tabs_to_delete[i], project = project_name$value)
      })
      
      # save new list of database tables and display 
      rv_manage_db$tbl <-  fishset_tables(project = project_name$value)
      showNotification("Table(s) deleted", type = "message", duration = 60)
      db_tbl_modal()
    })
    
    # return user to initial modal if user clicks cancel
    observeEvent(input$cancel_db_delete_tbl_btn,{
      db_tbl_modal()
    })
    
    # Adding and saving notes in txt file ---------------------------------------------------------
    observeEvent(input$download_notes_btn,{
      req(rv_project_name()) # Ensure rv_project_name is not NULL
      project_name <- rv_project_name() # Retrieve current project info
      
      req(current_tab())
      
      notes_file_path <- paste0(locoutput(project_name$value), current_tab(), "_upload_notes.txt")
      
      # File already exists
      if(file.exists(notes_file_path)) {
        if (!is.null(input$add_notes_input) & !is_empty(input$add_notes_input)) {
          tmp_note <- paste0(format(Sys.time(), 
                                    "%Y-%m-%d %H:%M"), ": ", input$add_notes_input, "\n")
          write(tmp_note,
                file = notes_file_path, 
                append = TRUE)
          shinyjs::show("notes_success_container") 
          shinyjs::hide("notes_error_container") 
          
        } else{
          shinyjs::hide("notes_success_container") 
          shinyjs::show("notes_error_container") 
          
        }
        
        # Create new file
      } else {
        if (!is.null(input$add_notes_input) & !is_empty(input$add_notes_input)) {
          tmp_note <- paste0("Upload Data Notes: \n")
          tmp_note <- paste0(tmp_note, format(Sys.time(),
                                              "%Y-%m-%d %H:%M"), ": ", input$add_notes_input, "\n")
          write(tmp_note, file = notes_file_path)
          shinyjs::show("notes_success_container") 
          shinyjs::hide("notes_error_container") 
          
        } else{
          shinyjs::hide("notes_success_container") 
          shinyjs::show("notes_error_container") 
          
        }
      }
    })
    
    # Run R expression ----------------------------------------------------------------------------
    # Output for results from running R expression - initially hidden
    output$r_expr_result <- renderText({
      rv_r_expr_output()
    })
    
    # Observer run R expression
    observeEvent(input$run_r_expr_btn, {
      req(input$r_expr_input) # ensure access to code input
      
      rv_r_expr_status(FALSE)
      
      tryCatch(
        {
          rv_r_expr$output <- isolate(
            paste(utils::capture.output(eval(parse(text = input$r_expr_input))), collapse = '\n')
          )
          
          rv_r_expr_status(TRUE)
        },
        error = function(e) {rv_r_expr$output <- e$message}
      )
    })
    
    # Generate output for R expression
    observe({
      req(rv_r_expr$output)
      
      rv_r_expr_output(
        paste(paste(">", isolate(input$r_expr_input)), rv_r_expr$output, sep = '\n')
      )
      
      shinyjs::show("r_expr_container")
      
      # Add or remove "error-text" class on the container div based on status
      if (rv_r_expr_status()) {
        shinyjs::removeClass(session$ns("r_expr_container"), "error-text")
      } else {
        shinyjs::addClass(session$ns("r_expr_container"), "error-text")
      }
    })
    
    # Stop app ------------------------------------------------------------------------------------
    observeEvent(input$close_app_btn, {
      stopApp()
    }, ignoreInit = TRUE)
    
  })
}