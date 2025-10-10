# =================================================================================================
# File: lagged_zone_module.R
# Description: 
#
# Authors: Paul Carvalho, Anna Abelman
# Date created: 10/10/2025
# Dependencies: 
# Notes: 
# =================================================================================================


# Server ------------------------------------------------------------------------------------------
#' lagged_zone_server
#'
#' @description 
#'
#' @param id 
#' @param rv_folderpath 
#' @param rv_project_name 
#' @param rv_data 
#'
#' @return 
lagged_zone_server <- function(id, rv_folderpath, rv_project_name, rv_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    cat("TEST")
  })
}

# UI ----------------------------------------------------------------------------------------------
#' lagged_zone_ui
#'
#' @description 
#'
#' @param id 
#'
#' @return 
lagged_zone_ui <- function(id){
  ns <- NS(id)
  
  p("TEST")
  
}
