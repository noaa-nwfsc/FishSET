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
source("modules/format_data/compute_new_var/new_r_expression_module.R", local = TRUE) # R expression
source("modules/format_data/compute_new_var/lag_zone_module.R", local = TRUE)
source("modules/format_data/compute_new_var/haul_to_trip_module.R", local = TRUE)
source("modules/format_data/compute_new_var/calc_trip_distance_module.R", local = TRUE)
source("modules/format_data/compute_new_var/calc_trip_centroid_module.R", local = TRUE)
source("modules/format_data/compute_new_var/assign_quantiles_module.R", local = TRUE)
source("modules/format_data/compute_new_var/group_perc_module.R", local = TRUE)
source("modules/format_data/compute_new_var/group_diff_module.R", local = TRUE)
source("modules/format_data/compute_new_var/group_cumsum_module.R", local = TRUE)

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
compute_new_var_server <- function(id, rv_data_load_error, #values = NULL,
                                   rv_folderpath, 
                                   rv_project_name, rv_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    new_r_express_server("new_r_express",                         
                         rv_data_load_error = reactive(rv_data_load_error()),
                         rv_project_name = rv_project_name,
                         rv_data = rv_data)
    
    # lag zone
    lag_zone_server("lag_zone", rv_folderpath, rv_project_name, rv_data )
    
    # haul to trip
    haul_to_trip_server("haul_to_trip", rv_folderpath, rv_project_name, rv_data)
    
    # Calculate trip distance
    calc_trip_distance_server("calc_trip_dist", rv_folderpath, rv_project_name, rv_data)
    
    # Calculate trip centroid
    calc_trip_centroid_server("calc_trip_cent", rv_folderpath, rv_project_name, rv_data)
    
    # Assigning quantiles 
    assign_quantiles_server("assign_quantiles", rv_project_name, rv_data)
    
    # Calculate within-group percentage
    group_perc_server("group_perc", rv_project_name, rv_data)
    
    # Within group lagged difference
    group_diff_server("group_diff", rv_project_name, rv_data)
    
    # Calculate within-group cumulative sum
    group_cumsum_server("group_cumsum", rv_project_name, rv_data)
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
                 choices = c("R expression" = "new_r_express",
                             "Lag zone ID" = "lag_zone_id",
                             "Haul to trip" = "haul_to_trip",
                             "Calculate trip distance" = "calc_trip_dist",
                             "Calculate trip centroid" = "calc_trip_centroid",
                             "Assign quantiles" = "assign_quantiles_id",
                             "Within-group percentages" = "group_perc_id",
                             "Within-group lagged difference" = "group_diff",
                             "Within-group cumulative sum" = "group_cumsum_id"),
                 selected = "new_r_express")
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
    # Conditionally display option to r expression
    conditionalPanel(
      condition = "input.comp_new_var_options == 'new_r_express'",
      ns = ns,
      new_r_express_ui(ns("new_r_express"))
    ),
    # Conditionally display option to lag zone variable
    conditionalPanel(
      condition = "input.comp_new_var_options == 'lag_zone_id'",
      ns = ns,
      lag_zone_ui(ns("lag_zone"))
    ),
    # Conditionally display option to aggregate hauls to trips
    conditionalPanel(
      condition = "input.comp_new_var_options == 'haul_to_trip'", 
      ns = ns,
      haul_to_trip_ui(ns("haul_to_trip"))
    ),
    # Conditionally display option to calculate trip distance
    conditionalPanel(
      condition = "input.comp_new_var_options == 'calc_trip_dist'", 
      ns = ns,
      calc_trip_distance_ui(ns("calc_trip_dist"))
    ),
    # Conditionally display option to calculate trip centroid
    conditionalPanel(
      condition = "input.comp_new_var_options == 'calc_trip_centroid'", 
      ns = ns,
      calc_trip_centroid_ui(ns("calc_trip_cent"))
    ),
    # Conditionally display option to assign quantiles 
    conditionalPanel(
      condition = "input.comp_new_var_options == 'assign_quantiles_id'",
      ns = ns,
      assign_quantiles_ui(ns("assign_quantiles"))
    ),
    # Conditionally display option: within-group percentage variable
    conditionalPanel(
      condition = "input.comp_new_var_options == 'group_perc_id'",
      ns = ns,
      group_perc_ui(ns("group_perc" ))
    ),
    # Conditionally display option: within group lagged difference
    conditionalPanel(
      condition = "input.comp_new_var_options == 'group_diff'",
      ns = ns,
      group_diff_ui(ns("group_diff"))
    ),
    # Conditionally display option: within group cumulative sum
    conditionalPanel(
      condition = "input.comp_new_var_options == 'group_cumsum_id'",
      ns = ns,
      group_cumsum_ui(ns("group_cumsum"))
    )
  )
}