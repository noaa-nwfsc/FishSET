# =================================================================================================
# File: server.R
# Description: Defines the server-side logic for the FishSET Shiny app.
#              This function is sourced into app.R and passed to shinyApp().
#
# Package: FishSET
# Authors: Paul Carvalho, Anna Abelman, et al. from previous Shiny app
# Date created: 4/18/2025
# Dependencies: - Input/output bindings defined in ui.R
#               - Additional input/output bindings defined in modules.
# Notes: - Avoid using hard-coded file paths; use system.file() when accessing
#          package resources.
#        - Access this app via the wrapper function:
#          run_fishset_gui()
#
# =================================================================================================

# Source module scripts ---------------------------------------------------------------------------

# Server settings ---------------------------------------------------------------------------------
options(shiny.maxRequestSize = 8000*1024^2) # set the max file upload size

# Initialize global values ------------------------------------------------------------------------
# check for FishSET folder 
fs_exist <- exists("folderpath", where = ".GlobalEnv")

# Server function definition
server <- function(input, output, session) {
  
  
  
}