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
source("modules/load_files_server.R", local = TRUE) # Upload data - load files subtab

# Server settings ---------------------------------------------------------------------------------
options(shiny.maxRequestSize = 8000*1024^2) # set the max file upload size

# Initialize global values ------------------------------------------------------------------------
fs_folder_exist <- exists("folderpath", where = ".GlobalEnv") # Check for FishSET folder 

# Server function definition
server <- function(input, output, session) {
  
  # Define reactives ------------------------------------------------------------------------------
  # Allow users to change FishSET folders easily.
  rv_folderpath <- reactiveVal({
    if (fs_folder_exist) get("folderpath", envir = as.environment(1L))
  })
  rv_project_name <- reactiveVal() # Project name
  rv_primary_data_name <- reactiveVal() # Primary data name
  
  # Upload data -----------------------------------------------------------------------------------
  ## Load files subtab ----------------------------------------------------------------------------
  ### Change folderpath
  rv_folderpath <- folder_path_server("folderpath") 
  
  ### Select project name
  rv_project_name <- select_project_server("select_project", rv_folderpath = rv_folderpath)
  
  ### Load primary data
  rv_primary_data_name <- load_primary_server("load_primary", rv_project_name = rv_project_name)
}