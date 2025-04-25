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
## Description: Update a reactive value for the FishSET folder path, create an output to display
##              the selected path, and return the folder path to make if available in the main app.
folder_path_server <- function(id){
  moduleServer(id, function(input, output, session){
    # Create a reactive for folderpath
    rv_folderpath <- reactiveVal(NULL)
    
    # Update FS folder path
    observeEvent(input$change_fs_folder_btn, {
      if(getOption("shiny.testmode", FALSE)){ # If running shiny tests - use test_path()
        fs_path <- testthat::test_path("data")
        rv_folderpath(fs_path)
        
      } else {
        fs_path <- update_folderpath()
        rv_folderpath(fs_path)  
      }
    })
    
    # Output to display the folder path
    output$display_folderpath <- renderText({
      req(rv_folderpath())
      paste("Selected folder:", rv_folderpath())
    })
    
    # Expose the path as a reactive
    return(rv_folderpath)
  })
}