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
source("modules/other_actions_server.R", local = TRUE) # Other actions in sidebar 

# Server settings ---------------------------------------------------------------------------------
options(shiny.maxRequestSize = 8000*1024^2) # set the max file upload size

# Initialize global values ------------------------------------------------------------------------
fs_folder_exist <- exists("folderpath", where = ".GlobalEnv") # Check for FishSET folder 

# Server function definition
server <- function(input, output, session) {
  
  # Define reactives ------------------------------------------------------------------------------
  rv_folderpath <- reactiveVal() # Folder path to FishSETFolder
  rv_project_name <- reactiveVal() # Project name
  rv_data_names <- reactiveValues() # Data file/table names for uploading
  rv_data <- reactiveValues() # All data loaded in load_data_server
  rv_data_load_error <- reactiveVal(TRUE) # Track errors with loading data for sidebar
  rv_confid_vals <- reactiveValues(check = FALSE, v_id = NULL, 
                                   rule = "n", value = 3) # basic default
  
  # Upload data -----------------------------------------------------------------------------------
  ## Load files subtab ----------------------------------------------------------------------------
  ### Sidebar
  #### Set confidentiality rules (popup)
  rv_confid_vals <- load_sidebar_server("upload_data_sidebar",
                                        rv_project_name = rv_project_name, 
                                        rv_data_load_error = reactive(rv_data_load_error()),
                                        rv_data = rv_data)
  
  #### Other actions (notes, close app)
  other_actions_server("upload_data_actions")
  
  ### Main panel 
  #### Change folderpath
  rv_folderpath <- folder_path_server("folderpath", fs_folder_exist = fs_folder_exist) 
  
  #### Select project name
  rv_project_name <- select_project_server("select_project", rv_folderpath = rv_folderpath)
  
  #### Select main data
  rv_data_names$main <- select_data_server("select_main",
                                           data_type = "main",
                                           rv_project_name = rv_project_name)
  
  #### Select port data (optional)
  rv_data_names$port <- select_data_server("select_port",
                                           data_type = "port",
                                           rv_project_name = rv_project_name)
  
  ### #Select aux data (optional)
  rv_data_names$aux <- select_data_server("select_aux",
                                          data_type = "aux",
                                          rv_project_name = rv_project_name)
  
  #### Select spatial data
  rv_data_names$spat <- select_data_server("select_spatial",
                                           data_type = "spat",
                                           rv_project_name = rv_project_name)
  
  #### Select gridded data (optional)
  rv_data_names$grid <- select_data_server("select_grid",
                                           data_type = "grid",
                                           rv_project_name = rv_project_name)
  
  ### Load data
  rv_data <- load_data_server("load_data",
                              rv_project_name = rv_project_name,
                              rv_data_names = rv_data_names)
  
  observe({rv_data_load_error(rv_data$error)}) # observe rv_data$error to update the sidebar
  
}