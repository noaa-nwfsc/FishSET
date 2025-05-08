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

# Server for main panel ---------------------------------------------------------------------------

## Change folder path -----------------------------------------------------------------------------
## Description: Update a reactive value for the FishSET folderpath, create an output to display
##              the selected path, and return the folderpath to make if available in the main app.
folder_path_server <- function(id){
  moduleServer(id, function(input, output, session){
    # Create a reactive for folderpath
    rv_folderpath <- reactiveVal(NULL)
    
    # Update FS folderpath
    observeEvent(input$change_fs_folder_btn, {
      if(getOption("shiny.testmode", FALSE)){ # If running shiny tests - use test_path()
        fs_path <- testthat::test_path("data/FishSETFolder")
        rv_folderpath(fs_path)
        
      } else {
        fs_path <- update_folderpath()
        rv_folderpath(fs_path)  
      }
    })
    
    # Output to display the folderpath
    output$display_folderpath <- renderText({
      req(rv_folderpath())
      paste("Selected folder:", rv_folderpath())
    })
    
    # Expose the path as a reactive
    return(rv_folderpath)
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

## Load primary data ------------------------------------------------------------------------------
## Description: Provide user with a drop-down menu of primary tables if loading an existing 
##              project, but if this is a new project have the user upload a new file. Return the
##              table name and type of input.
load_primary_server <- function(id, rv_project_name){
  moduleServer(id, function(input, output, session){
    # Observe project name reactive
    observeEvent(rv_project_name(), {
      req(rv_project_name())
      project_name <- rv_project_name()
      
      # If running shiny tests - set primary table name
      if(getOption("shiny.testmode", FALSE)){ 
        shinyjs::show("primary_select_container") # Set shiny test table name
        shinyjs::hide("primary_upload_container")
        updateSelectInput(session, 
                          "primary_select_input", 
                          choices = "scallop_shiny_testMainDataTable")
        
        # Select an existing table
      } else if(project_name$type == "select" & !is.null(project_name$value)) {
        shinyjs::show("primary_select_container") # Show dropdown menu of existing tables
        shinyjs::hide("primary_upload_container")
        primary_data_list <- list_tables(project_name$value, "main") # Get list of primary tables
        updateSelectInput(session, 
                          "primary_select_input", 
                          choices = primary_data_list)
        
        # Upload a new file
      } else if (project_name$type == "text") {
        shinyjs::hide("primary_select_container")
        shinyjs::show("primary_upload_container") # Show file input for uploading a new file
        
      }
    })
    
    # Return the primary data table type (select existing or upload new file) and file/table name
    return(reactive({
      req(rv_project_name())
      if(rv_project_name()$type == "select"){
        list(type = "select", value = input$primary_select_input)
      } else {
        list(type = "upload", value = input$primary_upload_input)
      }
    }))
  })
}

## Load port data ---------------------------------------------------------------------------------
## Description: Provide user with a drop-down menu of port tables if loading an existing 
##              project, but if this is a new project have the user upload a new file. Return the
##              table name and type of input.
load_port_server <- function(id, rv_project_name){
  moduleServer(id, function(input, output, session){
    # Observe project name reactive
    observeEvent(rv_project_name(), {
      req(rv_project_name())
      project_name <- rv_project_name()
      
      # If running shiny tests - set port table name
      if(getOption("shiny.testmode", FALSE)){ 
        shinyjs::show("port_select_container") # Set shiny test table name
        shinyjs::hide("port_upload_container")
        updateSelectInput(session, 
                          "port_select_input", 
                          choices = "scallop_shiny_testPortTable")
        
        # Select an existing table
      } else if(project_name$type == "select" & !is.null(project_name$value)) {
        port_data_list <- list_tables(project_name$value, "port") # Get list of port tables
        
        # if there is no port table previously loaded, show the file input
        if(all(is_empty(port_data_list))){
          shinyjs::hide("port_select_container")
          shinyjs::show("port_upload_container") # Show file input
        } else {
          shinyjs::show("port_select_container") # Show port table dropdown
          shinyjs::hide("port_upload_container")
          updateSelectInput(session, 
                            "port_select_input",
                            choices = port_data_list)  # Populate choices
        }
        
        # Upload a new file
      } else if (project_name$type == "text") {
        shinyjs::hide("port_select_container")
        shinyjs::show("port_upload_container") # Show file input for uploading a new file
        
      }
    })
    
    # Return the port data table type (select existing or upload new file) and file/table name
    return(reactive({
      req(rv_project_name())
      if(rv_project_name()$type == "select"){
        list(type = "select", value = input$port_select_input)
      } else {
        list(type = "upload", value = input$port_upload_input)
      }
    }))
  })
}

## Upload spatial data ----------------------------------------------------------------------------
## Description: Server module for handling spatial data uploads or selection. Relies on the project 
##              name reactive variable.
load_spatial_server <- function(id, rv_project_name){
  moduleServer(id, function(input, output, session){
    # React to changes in the reactive project name input
    observeEvent(rv_project_name(), {
      req(rv_project_name())# Ensure rv_project_name is not NULL
      project_name <- rv_project_name()  # Retrieve current project info
      
      # If the app is running in test mode, set up test-specific UI
      if(getOption("shiny.testmode", FALSE)){
        shinyjs::show("spat_select_container")  # Show the dropdown for selecting a spatial table
        shinyjs::hide("spat_upload_container")
        updateSelectInput(session, "spat_select_input",
                          choices = "scallop_shiny_testSpatTable")  # Use a test table
        
      } else {
        # If the project was selected from existing projects list and has a valid value
        if (project_name$type == "select" & !is.null(project_name$value)) {
          # Retrieve list of existing spatial tables fishset database
          spat_list <- list_tables(project_name$value, "spat")
          
          # if there is no spat tables previously loaded, show the file input
          if(all(is_empty(spat_list))){
            shinyjs::hide("spat_select_container")
            shinyjs::show("spat_upload_container") # Show file input
          } else {
            shinyjs::show("spat_select_container") # Show spat table dropdown
            shinyjs::hide("spat_upload_container")
            updateSelectInput(session, "spat_select_input",
                              choices = spat_list)  # Populate choices
          }
          
        } else if (project_name$type == "text"){
          # If the user is entering a new project name (free text), show upload UI
          shinyjs::hide("spat_select_container")
          shinyjs::show("spat_upload_container")
        }
      }
    })
    
    # Hide/Show spatial unload containers based on the checkbox input
    observeEvent(input$spat_shp_chk_input, {
      if (input$spat_shp_chk_input == FALSE) {
        shinyjs::show("spat_file_container")  # Show single file upload
        shinyjs::hide("spat_shp_container")
        
      } else if (input$spat_shp_chk_input == TRUE) {
        shinyjs::show("spat_shp_container")   # Show shapefile uploader
        shinyjs::hide("spat_file_container")
      }
    })
    
    # Return the spatial data table type (select existing or upload new file) and file/table name
    return(reactive({
      req(rv_project_name())
      if(rv_project_name()$type == "select"){
        list(type = "select", value = input$spat_select_input)
      } else if(rv_project_name()$type == "text" & input$spat_shp_chk_input == FALSE) {
        list(type = "upload", value = input$spat_file_input)
      } else if(rv_project_name()$type == "text" & input$spat_shp_chk_input == TRUE) {
        list(type = "upload", value = input$spat_shp_input)
      }
    })
    )
    
  })
}

## Upload gridded data ----------------------------------------------------------------------------
## Description: Server module for handling grid (1D/2D data) upload or selection. Relies on the  
##              project name reactive variable.
load_grid_server <- function(id, rv_project_name){
  moduleServer(id, function(input, output, session){
    # React to changes in the reactive project name input
    observeEvent(rv_project_name(), {
      req(rv_project_name())# Ensure rv_project_name is not NULL
      project_name <- rv_project_name()  # Retrieve current project info
      
      # If app is running in test mode (e.g., automated testing)
      if(getOption("shiny.testmode", FALSE)){
        shinyjs::show("grid_select_container")  # Show UI for selecting an existing grid table
        shinyjs::hide("grid_upload_container")  # Hide file upload section
        updateSelectInput(session, "grid_select_input",
                          choices = "scallop_shiny_testGridTable")  # Set test choice
        
      } else {
        # If the project was selected from existing projects list and has a valid value
        if (project_name$type == "select" & !is.null(project_name$value)) {
          # Retrieve list of existing grid data tables for the project
          grid_list <- list_tables(project_name$value, "grid")
          
          # if there is no grid tables previously loaded, show the upload grid input
          if(all(is_empty(grid_list))){
            shinyjs::hide("grid_select_container")
            shinyjs::show("grid_upload_container") # Show file input
          } else {
            shinyjs::show("grid_select_container") # Show grid selection
            shinyjs::hide("grid_upload_container")
            updateSelectInput(session, "grid_select_input",
                              choices = grid_list)  # Populate choices
          }
          
        } else if (project_name$type == "text") {
          # If user is creating a new project, show upload UI
          shinyjs::hide("grid_select_container")
          shinyjs::show("grid_upload_container")
        }
      }
    })
    
    # Return the gridded data table type (select existing or upload new file) and file/table name
    return(reactive({
      req(rv_project_name())
      if(rv_project_name()$type == "select"){
        list(type = "select", value = input$grid_select_input)
      } else if(rv_project_name()$type == "text") {
        list(type = "upload", value = input$grid_file_input)
      }
    })
    )
  })
}
