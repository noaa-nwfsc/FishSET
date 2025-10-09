# =================================================================================================
# File: format_data_module.R
# Description: This module defines the UI and server logic for the format data tab.
#
# Authors: Anna Abelman,  Paul Carvalho
# Date created: 10/8/2025
# Dependencies: shiny, DT 
# Notes: This module sources other explore the data modules saved in the /modules/format_data/
#        folder in FishSET.
# =================================================================================================

# Source module scripts ---------------------------------------------------------------------------
source("modules/format_data/compute_new_var_module.R", local = TRUE) # Compute new variables

# Format data server -------------------------------------------------------------------------
#' format_data_server
#'
#' @description Defines the server-side logic for the format data tab. It handles the
#' rendering of the different UI components based on the user's selection in the sidebar.
#'
#' @param id A character string that is unique to this module instance.
#' @param rv_project_name A reactive value containing the current project name.
#' @param rv_data A reactiveValues object containing the loaded data frames.
#'
#' @return This module does not return a value.
format_data_server <- function(id, rv_folderpath, rv_project_name, rv_data ){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    
    
  })
}

# Format data ui sidebar ---------------------------------------------------------------------
#' format_data_sidebar_ui
#'
#' @description Creates the sidebar UI for the format data tab. This includes radio buttons
#' that allow the user to select between format data functions.
#'
#' @param id A character string that is unique to this module instance.
#'
#' @return A tagList containing the sidebar UI elements.
format_data_sidebar_ui <- function(id) {
  ns <- NS(id)
  tagList(
    
    
  )
}


# Format data ui main panel ------------------------------------------------------------------
#' format_data_ui
#'
#' @description Creates the main panel UI for the format data tab. It uses conditionalPanels
#' to show or hide content based on the radio button selection in the sidebar.
#'
#' @param id A character string that is unique to this module instance.
#'
#' @return A tagList containing the main panel UI elements.
format_data_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    
  )
}