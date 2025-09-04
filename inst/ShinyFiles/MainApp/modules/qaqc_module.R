# =================================================================================================
# File: qaqc_module.R
# Description: This module defines the UI and server logic for the QAQC tab. It allows the user to 
#              preview data in tables, run various quality checks, and visualize data spatially.
#
# Authors: Paul Carvalho, Anna Abelman
# Date created: 8/4/2025
# Dependencies: shiny, DT, preview_data_module.R
# Notes: This module sources other qaqc modules saved in the /modules/qaqc/ folder in FishSET.
# =================================================================================================

# Source module scripts ---------------------------------------------------------------------------
source("modules/qaqc/preview_data_module.R", local = TRUE) # Preview data in table format
source("modules/qaqc/summary_data_module.R", local = TRUE) # Summary stats data table
<<<<<<< HEAD
source("modules/qaqc/change_variable_module.R", local = TRUE) # Change variable class
=======
source("modules/qaqc/remove_na_nan_module.R", local = TRUE) # Summary stats data table
source("modules/qaqc/change_variable_module.R", local = TRUE) # Preview data in table format
>>>>>>> b031ecb20d46b29cd29e39fc57d61af87d315f62
source("modules/qaqc/remove_variables_module.R", local = TRUE) # Remove variables
source("modules/qaqc/unique_obs_module.R", local = TRUE) # Ensure only unique observations
source("modules/qaqc/spatial_checks_module.R", local = TRUE) # Spatial checks


# QAQC server -------------------------------------------------------------------------------------
#' qaqc_server
#'
#' @description Defines the server-side logic for the QAQC tab. It handles the
#' rendering of the different UI components based on the user's selection in the sidebar.
#'
#' @param id A character string that is unique to this module instance.
#' @param rv_project_name A reactive value containing the current project name.
#' @param rv_data A reactiveValues object containing the loaded data frames.
#'
#' @return This module does not return a value.
qaqc_server <- function(id, rv_project_name, rv_data, rv_folderpath){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Preview data tables
    preview_data_server("preview_data", rv_project_name, rv_data)
    
    # Summary statistics table for primary data
    summary_data_server("summary_table", rv_project_name, rv_data)
    
    # Change variable class for primary data
    variable_class_server("change_variable_class", rv_project_name, rv_data)
    
    # Remove/replace NA/NaNs values
    remove_na_nan_server("na_and_nan", rv_project_name, rv_data)
    
    # Remove variables
    remove_variables_server("rm_variables", rv_project_name, rv_data, rv_folderpath)
    
    # Identifying unique observations in primary data
    unique_obs_server("unique_observations", rv_project_name, rv_data)
    
    # Spatial checks
    rv_ids_to_remove <- spatial_checks_server("spat_checks", 
                                              rv_project_name, 
                                              rv_data, 
                                              rv_folderpath)
    
    # Return the captured reactive so the main server can use it
    return(rv_ids_to_remove)
  })
}

# QAQC ui sidebar ---------------------------------------------------------------------------------
#' qaqc_sidebar_ui
#'
#' @description Creates the sidebar UI for the QAQC tab. This includes radio buttons
#' that allow the user to select between quality check functions.
#'
#' @param id A character string that is unique to this module instance.
#'
#' @return A tagList containing the sidebar UI elements.
qaqc_sidebar_ui <- function(id) {
  ns <- NS(id)
  tagList(
    radioButtons(ns("qaqc_options"), 
                 h6("Data quality checks:"),
                 choices = c("Preview data" = "preview", 
                             "Summary table"="summary",
                             "Change variable class" = "variable_class",
                             "Check NAs and NaNs" = "na_nan",
                             "Remove variables" = "remove_vars",
                             "Unique observations" = "unique_obs",
                             "Spatial checks" = "spat_checks"),
                 selected = "preview")
  )
}

# QAQC ui main panel ------------------------------------------------------------------------------
#' qaqc_ui
#'
#' @description Creates the main panel UI for the QAQC tab. It uses conditionalPanels
#' to show or hide content based on the radio button selection in the sidebar for quality checks.
#'
#' @param id A character string that is unique to this module instance.
#'
#' @return A tagList containing the main panel UI elements.
qaqc_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    # Conditionally display the preview data UI
    conditionalPanel(
      condition = "input.qaqc_options == 'preview'",
      ns = ns,
      preview_data_ui(ns("preview_data"))
    ),
    
    # Conditionally display the summary data table UI
    conditionalPanel(
      condition = "input.qaqc_options == 'summary'",
      ns = ns,
      summary_data_ui(ns("summary_table"))
    ),
    
    # Conditionally display the change class UI
    conditionalPanel(
      condition = "input.qaqc_options == 'variable_class'",
      ns = ns,
      variable_class_ui(ns("change_variable_class"))
    ),
    
    # Conditionally display the NA/NaN values UI
    conditionalPanel(
      condition = "input.qaqc_options == 'na_nan'",
      ns = ns,
      remove_na_nan_ui(ns("na_and_nan"))
    ),
    
    # Conditionally display remove variables UI
    conditionalPanel(
      condition = "input.qaqc_options == 'remove_vars'",
      ns = ns,
      remove_variables_ui(ns("rm_variables"))
    ),
    # Conditionally display unique observations UI
    conditionalPanel(
      condition = "input.qaqc_options == 'unique_obs'",
      ns = ns,
      unique_obs_ui(ns("unique_observations"))
    ),
    
    # Conditionally display spatial checks
    conditionalPanel(
      condition = "input.qaqc_options == 'spat_checks'",
      ns = ns,
      spatial_checks_ui(ns("spat_checks"))
    )
  )
}
