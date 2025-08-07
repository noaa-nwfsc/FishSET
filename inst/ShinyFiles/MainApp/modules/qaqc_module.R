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
qaqc_server <- function(id, rv_project_name, rv_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Preview data tables
    preview_data_server("preview_data", rv_project_name, rv_data)
    
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
                 "Data quality checks:",
                 choices = c("Preview data" = "preview"),
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
    )
  )
}
