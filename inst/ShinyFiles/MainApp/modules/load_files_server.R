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
        fs_path <- testthat::test_path("data")
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
##              in the FishSET folderpath
select_project_server <- function(id, rv_folderpath){
  moduleServer(id, function(input, output, session){
    # Update the list of project names when the folderpath changes
    observe({
      tmp_folderpath <- rv_folderpath() # observe changes in folderpath
      
      if(getOption("shiny.testmode", FALSE)){ # If running shiny tests - hardcode input values
        updateCheckboxInput(session, "load_existing_proj_input", value = TRUE)
        updateTextInput(session, "proj_name_input", value = "scallop_shiny_test")
        
      } else if (!is.null(tmp_folderpath) && !is.null(FishSET::projects())){
        proj_list <- FishSET::projects() # update project list
        updateSelectInput(session, "proj_select_input", choices = proj_list)
      }
    })
    
    # Initialize with appropriate visibility based on checkbox value
    observeEvent(input$load_existing_proj_input, {
      if(getOption("shiny.testmode", FALSE)){ # If running shiny tests - do nothing here
        
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
    
    
    # Return the current input value
    return(reactive({
      if(getOption("shiny.testmode", FALSE)){
        list(type = "text", value = input$proj_name_input)
      } else if(input$load_existing_proj_input){
        list(type = "select", value = input$proj_select_input)
      } else {
        list(type = "text", value = input$proj_name_input)
      }
    }))
  })
}