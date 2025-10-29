# =================================================================================================
# File: compute_new_var_module.R
# Description: This module defines the UI and server logic for the compute new variables subtab.
#
# Authors: Anna Abelman,  Paul Carvalho
# Date created: 10/8/2025
# Dependencies: shiny, DT 
# Notes: This module sources other explore the data modules saved in the 
#        /modules/format_data/compute_new_variables/ folder in FishSET.
# =================================================================================================

# Source module scripts ---------------------------------------------------------------------------
source("modules/format_data/compute_new_var/lag_zone_module.R", local = TRUE)
source("modules/format_data/compute_new_var/group_perc_module.R", local = TRUE)


# compute new variables server --------------------------------------------------------------------
#' compute_new_var_server
#'
#' @description Defines the server-side logic for the compute new variables tab. It handles the
#' rendering of the different UI components based on the user's selection in the sidebar.
#'
#' @param id A character string that is unique to this module instance.
#' @param rv_project_name A reactive value containing the current project name.
#' @param rv_data A reactiveValues object containing the loaded data frames.
#'
#' @return This module does not return a value.
compute_new_var_server <- function(id, rv_folderpath, rv_project_name, rv_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # lag zone
    lag_zone_server("lag_zone", rv_folderpath, rv_project_name, rv_data )
    
    
    group_perc_server("group_perc", rv_project_name, rv_data )

  })
}

# Compute new variables ui sidebar ----------------------------------------------------------------
#' compute_new_var_sidebar_ui
#'
#' @description Creates the sidebar UI for the compute new variables tab. This includes radio 
#' buttons that allow the user to select between format data functions.
#'
#' @param id A character string that is unique to this module instance.
#'
#' @return A tagList containing the sidebar UI elements.
compute_new_var_sidebar_ui <- function(id) {
  ns <- NS(id)
  tagList(
    radioButtons(ns("comp_new_var_options"), 
                 label = h6("Functions:"),
                 choices = c("Lag zone ID" = "lag_zone_id",
                             "Group Percentages" = "group_perc_id"),
                 selected = "")
  )
  
}


# Compute new variables ui main panel -------------------------------------------------------------
#' compute_new_var_ui
#'
#' @description Creates the main panel UI for the compute new variables tab. It uses 
#' conditionalPanels to show or hide content based on the radio button selection in the sidebar.
#'
#' @param id A character string that is unique to this module instance.
#'
#' @return A tagList containing the main panel UI elements.
compute_new_var_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    # Conditionally display option to lag zone variable
    conditionalPanel(
      condition = "input.comp_new_var_options == 'lag_zone_id'",
      ns = ns,
      lag_zone_ui(ns("lag_zone"))
    ),
     # Conditionally display option to group percentage variable
    conditionalPanel(
      condition = "input.comp_new_var_options == 'group_perc_id'",
      ns = ns,
       group_perc_ui(ns("group_perc" ))
  )
  )
}