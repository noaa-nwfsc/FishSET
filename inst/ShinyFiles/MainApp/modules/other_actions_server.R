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


other_actions_server <- function(id, rv_project_name, rv_data_load_error){
  moduleServer(id, function(input, output, session){
    
    ns <- session$ns
    
     # Initialize reactives
    rv_manage_db <- reactiveValues(tbl = NULL) # creating reactive dataframe for existing db tables

    # Managing saved database tables ---------------------------------------------------------------
    
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
      
      # First check to make sure a project was selected
      if(is.null(project_name$value) | is_empty(project_name$value)){
        showNotification("Error: project name not specified", type = "error", duration = 60)
        
      } else {
        if (length(suppressWarnings(projects())) == 0) {
          showNotification("No project tables found", type = "warning", duration = 60)
          
        } else {
          #save db tables to reactive data frame
          rv_manage_db$tbl <- fishset_tables(project = project_name$value)
          
          # open modal
          db_tbl_modal()
          
          # display list of db tables in table for user to select
          output$db_tables <- DT::renderDT(
            DT::datatable(rv_manage_db$tbl)
          )
        }  
      }
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
      tab_proj <- as.character(rv_manage_db$tbl$project[input$db_tables_rows_selected])
      
      # remove table from database
      lapply(seq_along(tabs_to_delete), function(i) {
        
        table_remove(tabs_to_delete[i], project = tab_proj[i])
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
    
    # Adding and saving notes in txt file ----------------------------------------------------------
    observeEvent(input$download_notes_btn,{
      
      req(rv_project_name()) # Ensure rv_project_name is not NULL
      project_name <- rv_project_name() # Retrieve current project info
      
      # File already exists
      if(file.exists(paste0(locoutput(project_name$value),"upload_notes.txt"))) {
        if (!is.null(input$add_notes_input) & !is_empty(input$add_notes_input)) {
          tmp_note <- paste0(format(Sys.time(), 
                                    "%Y-%m-%d %H:%M"), ": ", input$add_notes_input, "\n")
          write(tmp_note,
                file = paste0(locoutput(project_name$value),"upload_notes.txt"), 
                append = TRUE)
          showNotification("Data notes saved.", type='message', duration=60)  
          
        } else{
          showNotification("Input cannot be empty or null.", type='error', duration=60)  
          
        }
        # Create new file
      } else {
        if (!is.null(input$add_notes_input) & !is_empty(input$add_notes_input)) {
          tmp_note <- paste0("Upload Data Notes: \n")
          tmp_note <- paste0(tmp_note, format(Sys.time(),
                                              "%Y-%m-%d %H:%M"), ": ", input$add_notes_input, "\n")
          write(tmp_note, file = paste0(locoutput(project_name$value),
                                        "upload_notes.txt"))
          showNotification("Data notes saved.", type='message', duration=60)  
          
        } else{
          showNotification("Input cannot be empty or null.", type='error', duration=60)  
        }
      }
      
    })
    
    # Stop app -------------------------------------------------------------------------------------
    observeEvent(input$close_app_btn, {
      stopApp()
    }, ignoreInit = TRUE)
    

    
  })
}