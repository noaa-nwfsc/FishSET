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
  rv_folderpath <- reactiveVal() # Folder path to FishSETFolder
  rv_project_name <- reactiveVal() # Project name
  rv_data_names <- reactiveValues() # Data file/table names for uploading
  
  # Upload data -----------------------------------------------------------------------------------
  ## Select files subtab --------------------------------------------------------------------------
  ### Change folderpath
  rv_folderpath <- folder_path_server("folderpath", fs_folder_exist = fs_folder_exist) 
  
  ### Select project name
  rv_project_name <- select_project_server("select_project", rv_folderpath = rv_folderpath)
  
  ### Select main data
  rv_data_names$main <- select_data_server("select_main",
                                           data_type = "main",
                                           rv_project_name = rv_project_name)
  
  ### Select port data (optional)
  rv_data_names$port <- select_data_server("select_port",
                                           data_type = "port",
                                           rv_project_name = rv_project_name)
  
  ### Select aux data (optional)
  rv_data_names$aux <- select_data_server("select_aux",
                                          data_type = "aux",
                                          rv_project_name = rv_project_name)
  
  ### Select spatial data
  rv_data_names$spat <- select_data_server("select_spatial",
                                              data_type = "spat",
                                              rv_project_name = rv_project_name)
  
  ### Select gridded data (optional)
  rv_data_names$grid <- select_data_server("select_grid",
                                           data_type = "grid",
                                           rv_project_name = rv_project_name)
  
  ##### TEST CODE #####
  count <- reactiveVal(0)
  
  observeEvent(input$test_btn,{
    new_count <- count() + 1
    count(new_count)
    cat(file=stderr(), "\n", new_count, "\n")
    
    output$display_main <- renderPrint(
      str(rv_data_names$main())
    )
    
    output$display_port <- renderPrint(
      str(rv_data_names$port())
    )
    
    output$display_aux <- renderPrint(
      str(rv_data_names$aux())
    )
    
    output$display_spat <- renderPrint(
      str(rv_data_names$spat())
    )
    
    output$display_grid <- renderPrint(
      str(rv_data_names$grid())
    )
  })
  #####################
}