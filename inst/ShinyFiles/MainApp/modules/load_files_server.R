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
# Description: Server logic for side bar buttons in the upload data tab. Buttons are enabled/
#              disabled based on output from load_data_server.
load_sidebar_server <- function(id, rv_project_name, rv_data_load_error, rv_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # enable/disable confidentiality and reset log buttons based on data loading status
    observeEvent(rv_data_load_error(), {
      # Save reactive value in a static variable
      data_load_error <- rv_data_load_error()
      
      # Enable buttons if no load data errors
      shinyjs::toggleState("confid_modal_btn", 
                           condition = !data_load_error)
      shinyjs::toggleState("reset_log_modal_btn", 
                           condition = !data_load_error)
      shinyjs::toggleState("refresh_data_btn", 
                           condition = !data_load_error)
    })
    
    # create a modal for creating confidentiality rules
    observeEvent(input$confid_modal_btn, {
      req(rv_project_name()) # Ensure rv_project_name is not NULL
      req(rv_data) # Ensure data is not null
      project_name <- rv_project_name() # Retrieve current project info
      main_data <- rv_data$main # Save static copy of main data from reactive input
      
      # return confidentiality settings from project settings file if exists (created in 
      # set_confid_check() function below when user saves)
      existing_conf <- get_confid_check(project_name$value)
      
      # Confidentiality modal 
      showModal(
        modalDialog(
          title = "Confidentiality settings",
          checkboxInput(ns("confid_chk_input"), 
                        "Do you want to suppress confidential data in project
                        outputs (tables and figures)?", value = FALSE),
          # inputs if user has confidential data and needs to set rules
          div(id = ns("confid_container"),
              style = "display: none;",
              selectInput(ns("confid_vid_input"),
                          h6("Select vessel identifier variable"),
                          choices = names(main_data), # list variables from primary data
                          selected = existing_conf$v_id),
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
                          selected = existing_conf$rule),
              numericInput(ns("confid_value_input"),
                           h6("Threshold"), 
                           value = existing_conf$value,
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
    
    # hide/show confidential inputs when user clicks checkbox
    observeEvent(input$confid_chk_input,{
      if(input$confid_chk_input) {
        shinyjs::show("confid_container") 
      } else{
        shinyjs::hide("confid_container")
      }
    })
    
    # Save the confidentiality rules
    observeEvent(input$save_confid_btn, {
      # required inputs
      req(input$confid_chk_input)
      req(input$confid_rule_input)
      req(input$confid_vid_input)
      req(input$confid_value_input)
      req(rv_project_name())
      
      project_name <- rv_project_name() # Retrieve current project info
      
      # check the confidentiality rules and save
      pass_check <- set_confid_check(project_name$value,
                                     check = input$confid_chk_input,
                                     v_id = input$confid_vid_input,
                                     rule = input$confid_rule_input,
                                     value = input$confid_value_input)
      
      # Show status of confidentiality settings
      if (pass_check) {
        showNotification("Confidentiality settings saved", 
                         type = "message", 
                         duration = 60)
      } else {
        showNotification("Confidentiality settings not saved - invalid threshold value",
                         type = "error", 
                         duration = 60)
      }
      
      removeModal()
    }, ignoreInit=FALSE)
    
    # Reactive value for resetting log (T/F for overwriting existing log)
    rv_log_overwrite <- reactiveVal(NULL)
    
    # Resetting log or overwriting existing modal
    observeEvent(input$reset_log_modal_btn, {
      req(rv_project_name()) # Ensure rv_project_name is not NULL
      project_name <- rv_project_name() # Retrieve current project info
      
      last_log <- current_log(project_name$value) # names of the most recent log file
      today_log <- paste0(project_name$value, "_", Sys.Date(), ".json") # creates current log name
      # return T/F if most recent and current log name match
      rv_log_overwrite(last_log == today_log) 
      
      # Reset log modal
      showModal(
        modalDialog(title = "Reset Log",
                    div(id = ns("log_overwrite_container"),
                        style = "display: none;",
                        checkboxInput(ns("log_overwrite_chk"), 
                                      paste("Overwrite", last_log), value = FALSE)
                    ),
                    DT::DTOutput(ns("logreset_table")),
                    footer = tagList(
                      modalButton("Close"),
                      actionButton(ns("reset_log_btn"), "Reset log", 
                                   class = "btn-secondary")),
                    easyClose = TRUE)
      )
      # hide/show log overwrite checkbox based on the last log in the project
        if(rv_log_overwrite() == TRUE) {
          shinyjs::show("log_overwrite_container") 
        } else{
          shinyjs::hide("log_overwrite_container")
        }
      
      # Retrieve all logs in project
      log_tab <- project_logs(project_name$value, modified = TRUE)
      # Display table with all logs listed
      output$logreset_table <- DT::renderDT(log_tab)
    })
    
    # Resetting log action button
    observeEvent(input$reset_log_btn, {
      
      req(rv_project_name()) # Ensure rv_project_name is not NULL
      project_name <- rv_project_name() # Retrieve current project info
      
      # if user checks overwrite checkbox, then user input in log_reset function
      if (rv_log_overwrite()== TRUE) overwrite <- input$log_overwrite_chk
      else overwrite <- FALSE
      
      # check for errors and reset log
      q_test <- quietly_test(log_reset)
      log_reset_pass <- q_test(project_name$value, over_write = overwrite)
      
      if (log_reset_pass) {
        showNotification(paste0("Log has been reset for project \"",
                                project_name$value, "\""),
                         type = "default", duration = 60)
        removeModal()
      }
    })
    
    # refresh data actions
    observeEvent(input$refresh_data_btn, {
      
      req(rv_project_name()) # Ensure rv_project_name is not NULL
      req(rv_data) # Ensure data is not null
      project_name <- rv_project_name() # Retrieve current project info
      main_data <- rv_data$main # Save static copy of main data from reactive input
      
      # Ensure project name exists
      if(!is.null(project_name$value)){
        
        # Start spinner while it loads
        shinyjs::show("refresh_data_spinner_container")
        
        # get MainDataTable with date loaded
        tmp_tabs <- tables_database(project_name$value)[grep(paste0(project_name$value, 
                                                                    'MainDataTable\\d+'),
                                                             tables_database(project_name$value))]
        # all dates following MainDataTable
        tab_dates1 <- unlist(stringi::stri_extract_all_regex(tmp_tabs, "\\d{6,}"))
        tab_dates2 <- max(tab_dates1) # max date
        tmp_tabs <- tmp_tabs[which(tab_dates1 == tab_dates2)] # get the latest table
        
        # reset main data table with initial data
        ref_err <- FALSE
        tryCatch(
          main_data <- table_view(tmp_tabs, project_name$value),
          error = function(e) {ref_err <<- TRUE}
        )
        
        # once finished refreshing hide the spinner
        shinyjs::hide("refresh_data_spinner_container")
        
        if(ref_err){
          showNotification("Error refreshing data", type='error', duration=60)
        } else {
          showNotification("Data refreshed", type='message', duration=60)  
        }
      }
    }, ignoreInit = TRUE, ignoreNULL=TRUE) 
    
    
    
    # Return the confidentiality settings
    return(reactive({
      list(
        check = input$confid_chk_input,
        v_id = input$confid_vid_input,
        rule = input$confid_rule_input,
        value = input$confid_value_input
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
      if (getOption("shiny.testmode", FALSE)) {
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
      } else if (project_name$type == "select" & !is.null(project_name$value)) {
        data_table_list <- list_tables(project_name$value, data_type) # Get existing tables
        
        # if no tables previously loaded, show the file input
        if (all(is_empty(data_table_list))){
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
        
        # Upload a new file - handle spatial data input separately
      } else if (project_name$type == "text") {
        if(data_type != "spat"){
          shinyjs::hide(paste0(data_type, "_select_container"))
          shinyjs::show(paste0(data_type, "_upload_container")) # Show file input
          rv_data_input_type("upload")    
          
        } else {
          shinyjs::hide(paste0(data_type, "_select_container")) # Hide select
          shinyjs::show("spat_upload_container")  # Show spat upload
          updateCheckboxInput(session, "spat_shp_chk_input", value = FALSE) # reset check box
          rv_data_input_type("spat_file")    
        }
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

## Load data --------------------------------------------------------------------------------------
## Description: Run input checks to make sure required data (main and spatial) have been selected
##              and that the project name is valid. If all checks pass, then load all selected 
##              data.
load_data_server <- function(id, rv_project_name, rv_data_names, parent_session){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Initialize reactives
    rv_load_error_message <- reactiveVal("") # Store error messages
    rv_load_success_message <- reactiveVal("") # Store success message
    rv_all_data_output <- reactiveValues() # Store all of the loaded data - return to main server
    
    # Outputs for error and success messages
    output$load_error_message_out <- renderText({
      rv_load_error_message()
    })
    output$load_success_message_out <- renderText({
      rv_load_success_message()
    })
    
    ### Loading helper functions ------------------------------------------------------------------
    # Check validity of project name
    check_project_name <- function(name){
      if(!is.character(name)) return(FALSE)
      if(!grepl("^[A-Za-z0-9_]+$", name)) return(FALSE) 
      if(grepl("^\\d+$", name)) return(FALSE)
      return(TRUE)
    }
    
    # Load all of the selected data
    load_project_data <- function(data_type, load_data_input, project_name){
      # Skip the function if optional data input is empty
      if ((data_type %in% c("port", "aux", "grid")) && 
          is.null(load_data_input$value)) {
        return()
      }
      
      load_warning_error <- FALSE # flag in case an error or warning occurs when loading data
      
      # Load from FishSET database
      if (load_data_input$type == "select"){
        table_name <- load_data_input$value # Save table name
        
        tryCatch(
          {
            data_out <- table_view(table_name, project_name)    
          },
          warning = function(w) {
            load_warning_error <<- TRUE
            rv_load_error_message(
              paste0("⚠️ ", table_name, " not found in the ", project_name, " database.")
            )
            shinyjs::show("load_error_message")
          },
          error = function(e) {
            load_warning_error <<- TRUE
            rv_load_error_message(
              paste0("⚠️ ERROR: ", table_name, " not able to load.")
            )
            shinyjs::show("load_error_message")
          }
        )
      }
      
      # Only proceed if data loaded without warning or error
      if (!load_warning_error){
        # Edit project settings in the output folder
        edit_proj_settings(project = project_name,
                           tab_name = table_name,
                           tab_type = data_type)
        
        # Save package version and recent git commit to the output folder
        fishset_commit <- packageDescription("FishSET")$GithubSHA1
        fishset_version <- packageDescription("FishSET")$Version
        fishset_version <- paste0("v", fishset_version, " / commit ", fishset_commit)
        version_file <- paste0(locoutput(project_name), "fishset_version_history.txt")
        cat(c("Date: ", as.character(Sys.Date()), "\n", "FishSET", fishset_version, "\n\n"), 
            file = version_file, append = TRUE)
      }
      
      return(data_out)
    }
    
    ### Observe load button -----------------------------------------------------------------------
    # Handle load button click
    observeEvent(input$load_data_btn, {
      # Ensure that reactives are available
      req(rv_project_name())
      req(rv_data_names)
      
      # Assign static values from the reactives
      project_name <- rv_project_name()
      main_data_info <- rv_data_names$main()
      port_data_info <- rv_data_names$port()
      aux_data_info <- rv_data_names$aux()
      spat_data_info <- rv_data_names$spat()
      grid_data_info <- rv_data_names$grid()
      
      # Hide success and error messages initially
      shinyjs::hide("load_success_message")
      shinyjs::hide("load_error_message")
      
      # Check to make sure the reactive inputs are valid
      if(is_empty(project_name$value)){
        rv_load_error_message("⚠️ Project name is required")
        shinyjs::show("load_error_message")
        # Reset reactiveValues to NULL
        invisible(lapply(names(rv_all_data_output),function(x) rv_all_data_output[[x]] <<- NULL))
        return(rv_all_data_output$error <- TRUE)
        
      }
      
      # Check for invalid project names
      if(!check_project_name(project_name$value)){
        rv_load_error_message("⚠️ Project name is invalid. 
                              Must contain only letters, underscores, or numbers.
                              Spaces are invalid.")
        shinyjs::show("load_error_message")
        # Reset reactiveValues to NULL
        invisible(lapply(names(rv_all_data_output),function(x) rv_all_data_output[[x]] <<- NULL))
        return(rv_all_data_output$error <- TRUE)
      }
      
      # Check for main data input
      if(is.null(main_data_info$value)){
        rv_load_error_message("⚠️ Main data file/table is required")
        shinyjs::show("load_error_message")
        # Reset reactiveValues to NULL
        invisible(lapply(names(rv_all_data_output),function(x) rv_all_data_output[[x]] <<- NULL))
        return(rv_all_data_output$error <- TRUE)
      }
      
      # Check for spat data input
      if(is.null(spat_data_info$value)){
        rv_load_error_message("⚠️ Spatial file/table is required")
        shinyjs::show("load_error_message")
        # Reset reactiveValues to NULL
        invisible(lapply(names(rv_all_data_output),function(x) rv_all_data_output[[x]] <<- NULL))
        return(rv_all_data_output$error <- TRUE)
      }
      
      # Show local spinner
      shinyjs::show("load_data_spinner_container")
      
      # Load each data type
      rv_all_data_output$main <- load_project_data(data_type = "main", 
                                                   load_data_input = main_data_info,
                                                   project_name = project_name$value)
      
      rv_all_data_output$port <- load_project_data(data_type = "port", 
                                                   load_data_input = port_data_info,
                                                   project_name = project_name$value)
      
      rv_all_data_output$aux <- load_project_data(data_type = "aux", 
                                                  load_data_input = aux_data_info,
                                                  project_name = project_name$value)
      
      rv_all_data_output$spat <- load_project_data(data_type = "spat",
                                                   load_data_input = spat_data_info,
                                                   project_name = project_name$value)
      
      rv_all_data_output$grid <- load_project_data(data_type = "grid", 
                                                   load_data_input = grid_data_info,
                                                   project_name = project_name$value)
      
      # If any items in list is a character, then it contains a warning or error message
      # return empty value
      if(any(sapply(reactiveValuesToList(rv_all_data_output), is.character))){
        shinyjs::hide("load_data_spinner_container") # hide spinner then return empty value
        # Reset reactiveValues to NULL
        invisible(lapply(names(rv_all_data_output),function(x) rv_all_data_output[[x]] <<- NULL))
        return(rv_all_data_output$error <- TRUE)
      }
      
      # Hide local spinner
      shinyjs::hide("load_data_spinner_container")
      
      # Show success message
      rv_load_success_message("Data loaded successfully! 😁")
      shinyjs::show("load_success_message")
      
      rv_all_data_output$error <- FALSE # loaded successfully
      
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    
    # Next button to move user to Select variables sub-tab
    observeEvent(input$load_data_next_btn, {
      bslib::nav_show(
        id = "tabs", target = "Select variables", select = TRUE,
        session = parent_session
      )
    })
    
    
    # Return to main server
    return(rv_all_data_output)
    
    
  })
  
  
}