# =================================================================================================
# File: load_files_server.R
# Description: Defines the server-side logic for the load files subtab in the FishSET Shiny app.
#              This function is sourced into lite_app.R and passed to shinyApp().
#
# Package: FishSET
# Authors: Paul Carvalho, Anna Abelman
# Date created: 4/23/2025
# Dependencies: - Input/output bindings defined in load_files_ui.R
#               - Additional input/output bindings defined in modules.
# Notes: - Avoid using hard-coded file paths; use system.file() when accessing
#          package resources.
#
# =================================================================================================

# Server for sidebar ------------------------------------------------------------------------------
load_sidebar_server <- function(id, rv_project_name, values, confid_vals){
  moduleServer(id, function(input, output, session){
   
    ns <- session$ns
    # # refresh data   
    # observeEvent(input$refresh_data_btn, {
    #   
    #   req(rv_project_name())# Ensure rv_project_name is not NULL
    #   project_name <- rv_project_name()  # Retrieve current project info
    #   
    #   if(!is.null(project_name$value)){
    #     tmp_tabs <- tables_database(project_name$value)[grep(paste0(project_name$value,
    #                                                                 'MainDataTable\\d+'),
    #                                                    tables_database(project_name$value))]
    #     # all dates following MainDataTable
    #     tab_dates1 <- unlist(stringi::stri_extract_all_regex(tmp_tabs, "\\d{6,}"))
    #     tab_dates2 <- max(tab_dates1) # max date
    #     tmp_tabs <- tmp_tabs[which(tab_dates1 == tab_dates2)] # get the latest table
    #     
    #     ref_err <- FALSE
    #     tryCatch(
    #       values$dataset <- table_view(tmp_tabs, project_name$value),
    #       error = function(e) {ref_err <<- TRUE}
    #     )
    #     
    #     if(ref_err){
    #       showNotification("Error refreshing data", type='error', duration=60)
    #     } else {
    #       showNotification("Data refreshed", type='message', duration=60)  
    #     }
    #   }
    #   
    # }, ignoreInit = TRUE, ignoreNULL=TRUE) 
    
    observeEvent(rv_project_name(), {
      
      
      req(rv_project_name())# Ensure rv_project_name is not NULL
      project_name <- rv_project_name()  # Retrieve current project info
      
      conf_rv <- reactiveValues(current_len = NULL,
                                last_len = NULL)
      
      conf_rv$current_len <- length(get_confid_cache(project_name$value))
      conf_rv$last_len <- conf_rv$current_len
      
      conf <- get_confid_check(project_name$value)
      confid_vals$check <- conf$check
      confid_vals$v_id <- conf$v_id
      confid_vals$rule <- conf$rule
      confid_vals$value <- conf$value
      
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    
    
   
    observeEvent(input$confid_modal_btn, {
      
      req(rv_project_name())# Ensure rv_project_name is not NULL
      project_name <- rv_project_name()  # Retrieve current project info
      
      showModal(
        modalDialog(title = "Check Confidentiality",
                    checkboxInput(ns("confid_chk_input"), "Do you have data that is confidential?",
                                  value = FALSE),
                    div(
                      id = ns("more_text_div"),
                      style = "display: none;",
                      selectInput(ns("confid_vid_input"),
                                  h6("Select vessel identifier variable"),
                                  choices = names(values$dataset),
                                  selected = confid_vals$v_id),
                      selectInput(ns("confid_rule_input"), 
                                  label=list(h6('Select rule',
                                                bslib::tooltip( 
                                                  bsicons::bs_icon("info-circle"),
                                                  "The n rule (“rule of n”) 
                                                              defaults to 3, at least three unique
                                                              observational units (e.g., vessels).
                                                              The k rule (“identification of majority 
                                                              allocation”) defaults to 90; no single
                                                              observationalunit can account for 90%
                                                              or more of the value.", 
                                                  id = "tip", 
                                                  placement = "right"))), 
                                  choices = c("n", "k"),
                                  selected = confid_vals$rule),
                      numericInput(ns("confid_value_input"),
                                   h6("Threshold"), 
                                   value = confid_vals$value,
                                   min = 0, max = 100),
                    ),
                    footer = tagList(
                      modalButton("Close"),
                      actionButton(ns("save_confid_btn"), "Save",
                                   class = "btn-secondary")
                    ),
                    easyClose = TRUE
        )
      )
    }, ignoreInit = TRUE)
    
    observeEvent(input$confid_chk_input,{
      if(input$confid_chk_input) {
        shinyjs::show("more_text_div") 
      } else{
        shinyjs::hide("more_text_div")
      }
    })
    
    observeEvent(input$save_confid_btn, {
      
      req(input$confid_chk_input)
      req(input$confid_rule_input)
      req(input$confid_vid_input)
      req(input$confid_value_input)
      req(rv_project_name())
      
      project_name <- rv_project_name()
      
      pass_check <-
        set_confid_check(project_name$value,
                         check = input$confid_chk_input,
                         v_id = input$confid_vid_input,
                         rule = input$confid_rule_input,
                         value = input$confid_value_input)
      
      if (pass_check) {
        
        showNotification("Confidentiality settings saved", type = "message", duration = 60)
        confid_vals$check <- input$confid_chk_input
        confid_vals$v_id <- input$confid_vid_input
        confid_vals$rule <- input$confid_rule_input
        confid_vals$value <- input$confid_value_input
        
      } else {
        
        showNotification("Confidentiality settings not saved - invalid threshold value",
                         type = "error", duration = 60)
      }
      
      removeModal()
    }, ignoreInit=FALSE)
    
    # Return the project name
    return(reactive({
      list(
        check = confid_vals$check,
        v_id = confid_vals$v_id,
        rule = confid_vals$rule,
        value = confid_vals$value
      )
      
    }))
    
  })
}
# Server for main panel ---------------------------------------------------------------------------

## Change folder path -----------------------------------------------------------------------------
## Description: Update a reactive value for the FishSET folderpath, create an output to display
##              the selected path, and return the folderpath to make if available in the main app.
folder_path_server <- function(id, fs_folder_exist){
  moduleServer(id, function(input, output, session){
    # Create a reactive for folderpath
    rv_out_folderpath <- reactiveVal({
      # if path is in global env then use that path
      if(fs_folder_exist) get("folderpath", envir = .GlobalEnv) else NULL
    })  
    
    # Update FS folderpath
    observeEvent(input$change_fs_folder_btn, {
      if(getOption("shiny.testmode", FALSE)){ # If running shiny tests - use test_path()
        fs_path <- testthat::test_path("data/FishSETFolder")
        rv_out_folderpath(fs_path)
        
      } else {
        fs_path <- update_folderpath()
        rv_out_folderpath(fs_path)
      }
    })
    
    # Output to display the folderpath
    output$display_folderpath <- renderText({
      req(rv_out_folderpath())
      paste("Selected folder:", rv_out_folderpath())
    })
    
    # Expose the path as a reactive
    return(rv_out_folderpath)
  })
}

## Select project ---------------------------------------------------------------------------------
## Description: Add a new project name, or select an existing project based on projects available
##              in the FishSET folderpath. Folderpath is a reactive input to observe changes in
##              the value and look for FishSET projects in the path.
select_project_server <- function(id, rv_folderpath){
  moduleServer(id, function(input, output, session){
    # Update the list of project names when the folderpath changes
    observe({
      req(rv_folderpath()) # ensure reactive is available
      folderpath <- rv_folderpath() # observe changes in folderpath
      
      if(getOption("shiny.testmode", FALSE)){ # If running shiny tests - set checkbox to TRUE
        updateCheckboxInput(session, "load_existing_proj_input", value = TRUE)
        
      } else if (!is.null(folderpath) && !is.null(FishSET::projects())){
        proj_list <- FishSET::projects() # update project list
        updateSelectInput(session, "proj_select_input", choices = proj_list)
      }
    })
    
    # Initialize with appropriate visibility based on checkbox value
    observeEvent(input$load_existing_proj_input, {
      if(getOption("shiny.testmode", FALSE)){ # If running shiny tests - set existing project
        shinyjs::show("proj_select_container")
        shinyjs::hide("proj_name_container")
        updateSelectInput(session, "proj_select_input", choices = "scallop_shiny_test")
        
      } else {
        if(input$load_existing_proj_input) {
          shinyjs::show("proj_select_container") # Display existing projects
          shinyjs::hide("proj_name_container")
          
        } else {
          shinyjs::hide("proj_select_container") # Get a new project name
          shinyjs::show("proj_name_container")
        }
      }
    }, ignoreInit = FALSE) # Process this on initialization
    
    # Return the project name
    return(reactive({
      if(getOption("shiny.testmode", FALSE)){
        list(type = "select", value = input$proj_select_input)
      } else if(input$load_existing_proj_input){
        list(type = "select", value = input$proj_select_input)
      } else {
        list(type = "text", value = input$proj_name_input)
      }
    }))
  })
}

## Select data ------------------------------------------------------------------------------------
## Description: Provide user with a drop-down menu of tables if loading an existing project AND
##              tables exist in the project folder. If this is a new project, or a data type
##              is not present in an existing project, give a file input. Return the table name 
##              and type of input.
select_data_server <- function(id, data_type, rv_project_name){
  moduleServer(id, function(input, output, session){
    rv_data_input_type <- reactiveVal() # indicates which input value to return
    
    # Observer project name reactive
    observeEvent(rv_project_name(), {
      req(rv_project_name()) # Check to ensure reactive is available
      project_name <- rv_project_name()
      
      # If running shiny tests - assign test table names
      if(getOption("shiny.testmode", FALSE)){
        shiny_test_table <- switch(data_type,
                                   "main" = "scallop_shiny_testMainDataTable",
                                   "port" = "scallop_shiny_testPortTable",
                                   "aux" = "scallop_shiny_testAuxTable",
                                   "spat" = "scallop_shiny_testSpatTable",
                                   "grid" = "scallop_shiny_testGridTable")
        
        shinyjs::show(paste0(data_type, "_select_container")) # Show dropdown menu
        shinyjs::hide(paste0(data_type, "_upload_container"))
        
        updateSelectInput(session, # Update option to the test table name 
                          paste0(data_type, "_select_input"), 
                          choices = shiny_test_table)
        rv_data_input_type("select")
        
        # Select an existing table
      } else if(project_name$type == "select" & !is.null(project_name$value)) {
        data_table_list <- list_tables(project_name$value, data_type) # Get existing tables
        
        # if no tables previously loaded, show the file input
        if(all(is_empty(data_table_list))){
          shinyjs::hide(paste0(data_type, "_select_container"))
          shinyjs::show(paste0(data_type, "_upload_container")) # Show file input
          rv_data_input_type("upload")
          
        } else {
          shinyjs::show(paste0(data_type, "_select_container")) # Show dropdown menu
          shinyjs::hide(paste0(data_type, "_upload_container"))
          
          updateSelectInput(session, 
                            paste0(data_type, "_select_input"),
                            choices = data_table_list)  # Populate choices
          rv_data_input_type("select")
        }
        
        # Upload a new file
      } else if (project_name$type == "text") {
        shinyjs::hide(paste0(data_type, "_select_container"))
        shinyjs::show(paste0(data_type, "_upload_container")) # Show file input
        rv_data_input_type("upload")
      }
    })
    
    # Hide/Show spatial unload containers based on the checkbox input
    observeEvent(input$spat_shp_chk_input, {
      # Only execute code for spatial data
      if(data_type == "spat"){ 
        if (input$spat_shp_chk_input == FALSE) {
          shinyjs::show("spat_file_container")  # Show single file upload
          shinyjs::hide("spat_shp_container")
          rv_data_input_type("spat_file")
          
        } else if (input$spat_shp_chk_input == TRUE) {
          shinyjs::show("spat_shp_container")   # Show shapefile uploader
          shinyjs::hide("spat_file_container")
          rv_data_input_type("spat_shp")
          
        }  
      }
    })
    
    # Return the data table type (select existing or upload new file) and file/table name
    return(reactive({
      req(rv_project_name())
      if(rv_data_input_type() == "select"){
        list(type = "select", value = input[[paste0(data_type, "_select_input")]])
        
      } else if(rv_data_input_type() == "upload"){
        list(type = "upload", value = input[[paste0(data_type, "_upload_input")]])
        
      } else if(rv_data_input_type() == "spat_file"){
        list(type = "upload", value = input$spat_file_input)
        
      } else if(rv_data_input_type() == "spat_shp"){
        list(type = "upload", value = input$spat_shp_input)  
      }
    }))
  })
}
