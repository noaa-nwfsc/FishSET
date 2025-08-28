# =================================================================================================
# File: outliers_module.R
# Description: 
#
# Authors: Paul Carvalho, Anna Abelman
# Date created: 8/28/2025
# Dependencies: 
# Notes: 
# =================================================================================================

# Server ------------------------------------------------------------------------------------------
#' outliers_server
#'
#' @description 
#'
#' @param id A character string that is the namespace for this module.
#' @param rv_project_name A reactive value containing the name of the current project.
#' @param rv_data A reactive list containing the main dataset (`main`) and spatial data (`spat`).
#' @param rv_folderpath A reactive value containing the file path to the project's root folder.
#'
#' @return A reactive list containing `$ids`, the unique identifiers of observations to be removed,
#'         and `$id_col`, the name of the unique ID column.
outliers_server <- function(id, rv_project_name, rv_data, rv_folderpath){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    cat("TEST")
       
  })
}

# UI ----------------------------------------------------------------------------------------------
#' outliers_ui
#'
#' @description
#'
#' @param id A character string that is the namespace for this module.
#'
#' @return A tagList containing the UI elements for the module.
outliers_ui <- function(id){
  ns <- NS(id)
  
  p("TEST")
  
}