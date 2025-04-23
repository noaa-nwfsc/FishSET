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
folder_path_server <- function(id){
  moduleServer(id, function(input, output, session){
    # Create a reactive for folderpath
    rv_folderpath <- reactiveVal(NULL)
    
    # update FS folder path
    observeEvent(input$change_fs_folder_btn, {
      fs_path <- update_folderpath()
      rv_folderpath(fs_path)
    })
    
    # output to display the folder path
    output$display_folderpath <- renderText({
      req(rv_folderpath())
      paste("Selected folder:", rv_folderpath())
    })
    
    # Expose the path as a reactive
    return(rv_folderpath)
  })
}