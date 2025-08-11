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
source("modules/select_variables_server.R", local = TRUE) # Other actions in sidebar 
source("modules/qaqc_module.R", local = TRUE)

# Server settings ---------------------------------------------------------------------------------
options(shiny.maxRequestSize = 8000*1024^2) # set the max file upload size

# Initialize global values ------------------------------------------------------------------------
fs_folder_exist <- exists("folderpath", where = ".GlobalEnv") # Check for FishSET folder 

# Server function definition
server <- function(input, output, session) {
  
  # Load the entire package if in test mode (for shinytest2)
  is_testing <- getOption("shiny.testmode", FALSE) ||
    Sys.getenv("TESTTHAT") == "true" ||
    Sys.getenv("_R_CHECK_PACKAGE_NAME_") != "" ||
    !is.null(getOption("shinytest2.app")) ||
    identical(Sys.getenv("NOT_CRAN"), "true")
  if (is_testing) {
    # Try to load the package development version
    tryCatch({
      pkgload::load_all()
    }, error = function(e) {
      # If load_all fails, fall back to library
      library(FishSET)
    })
  } else {
    library(FishSET)
  }
  
  # Define reactives ------------------------------------------------------------------------------
  rv_folderpath <- reactiveVal() # Folder path to FishSETFolder
  rv_project_name <- reactiveVal() # Project name
  rv_data <- reactiveValues() # All data loaded in load_data_server
  rv_data_load_error <- reactiveVal(TRUE) # Track errors with loading data for sidebar
  rv_confid_vals <- reactiveValues(check = FALSE, 
                                   v_id = NULL, 
                                   rule = "n", 
                                   value = 3) # basic default
  
  # Upload data -----------------------------------------------------------------------------------
  ## Load files subtab ----------------------------------------------------------------------------
  ### Sidebar
  #### Set confidentiality rules (popup)
  rv_confid_vals <- load_sidebar_server("upload_data_sidebar",
                                        rv_project_name = rv_project_name, 
                                        rv_data_load_error = reactive(rv_data_load_error()),
                                        rv_data = rv_data)
  
  #### Other actions (notes, close app)
  other_actions_server("upload_data_actions", 
                       values = list(project_name = rv_project_name,
                                     data = rv_data),
                       rv_project_name = rv_project_name,
                       rv_data_load_error = reactive(rv_data_load_error()),
                       current_tab = reactive(input$tabs))
  
  ### Main panel 
  #### Change folderpath
  rv_folderpath <- folder_path_server("folderpath", fs_folder_exist = fs_folder_exist) 
  
  #### Select project name
  rv_project_name <- select_project_server("select_project", rv_folderpath = rv_folderpath)
  
  #### Load data
  rv_data <- load_data_server("load_data",
                              rv_project_name = rv_project_name,
                              rv_data_names = rv_data_names,
                              parent_session = session)
  
  # Export shiny test values
  exportTestValues(
    main = rv_data$main,
    port = rv_data$port,
    aux = rv_data$aux,
    spat = rv_data$spat,
    grid = rv_data$grid
  )
  
  observe({rv_data_load_error(rv_data$error)}) # observe rv_data$error to update the sidebar
  
  ## Selecting variables subtab -------------------------------------------------------------------
  ### Sidebar
  checklist_server("select_var_checklist", rv_project_name, rv_data)
  
  other_actions_server("selecting_variables_actions", 
                       values = list(project_name = rv_project_name,
                                     data = rv_data),
                       rv_project_name = rv_project_name,
                       rv_data_load_error = reactive(rv_data_load_error()),
                       current_tab = reactive(input$tabs))
  
  ### Main panel
  #### Save all selected variables to project data folder 
  save_var_server("saving_all_variables", 
                  rv_project_name = rv_project_name,
                  rv_data = rv_data,
                  parent_session = session)
  
  # QAQC ------------------------------------------------------------------------------------------
  ## Quality checks -------------------------------------------------------------------------------
  ### Sidebar
  checklist_server("quality_check_checklist", rv_project_name, rv_data)
  
  other_actions_server("quality_check_actions",
                       values = list(project_name = rv_project_name,
                                     data = rv_data),
                       rv_project_name = rv_project_name,
                       rv_data_load_error = reactive(rv_data_load_error()),
                       current_tab = reactive(input$tabs))
  
  ### Main panel
  qaqc_server("qaqc_checks", rv_project_name, rv_data, rv_folderpath)
}