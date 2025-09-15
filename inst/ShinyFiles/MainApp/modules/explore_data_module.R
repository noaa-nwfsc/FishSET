# =================================================================================================
# File: explore_data_module.R
# Description: This module defines the UI and server logic for the explore the data tab. 
# Authors: Anna Abelman,  Paul Carvalho
# Date created: 9/8/2025
# Dependencies: shiny, DT, leaflet, 
# Notes: This module sources other explore the data modules saved in the /modules/explore_the_data/
#        folder in FishSET.
# =================================================================================================

source("modules/explore_data/zone_summary_module.R", local = TRUE) # Spatial checks

# Explore the data server -------------------------------------------------------------------------
#' explore_data_server
#'
#' @description Defines the server-side logic for the explore the data tab. It handles the
#' rendering of the different UI components based on the user's selection in the sidebar.
#'
#' @param id A character string that is unique to this module instance.
#' @param rv_project_name A reactive value containing the current project name.
#' @param rv_data A reactiveValues object containing the loaded data frames.
#'
#' @return This module does not return a value.
explore_data_server <- function(id, rv_folderpath, rv_project_name, rv_data ){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    zone_summary_server("zone_summ_plot",rv_folderpath, rv_project_name, rv_data )
    
  })
}


# Explore the data ui sidebar ---------------------------------------------------------------------
#' explore_data_sidebar_ui
#'
#' @description Creates the sidebar UI for the explore the data tab. This includes radio buttons
#' that allow the user to select between quality check functions.
#'
#' @param id A character string that is unique to this module instance.
#'
#' @return A tagList containing the sidebar UI elements.
explore_data_sidebar_ui <- function(id) {
  ns <- NS(id)
  tagList(
     radioButtons(ns("explore_data_options"), 
       label = "Explore options",
                 choices = c("Zone summary" = "zone_summary"),
                   selected = "zone_summary")
    
  )
}

# Explore the data ui main panel ------------------------------------------------------------------
#' explore_data_ui
#'
#' @description Creates the main panel UI for the explore the data tab. It uses conditionalPanels
#' to show or hide content based on the radio button selection in the sidebar for quality checks.
#'
#' @param id A character string that is unique to this module instance.
#'
#' @return A tagList containing the main panel UI elements.
explore_data_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    zone_summary_ui(ns("zone_summ_plot"))
    
  )
}