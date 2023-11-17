library(dplyr)

zone_outsample_mapUI <- function(id){
  ns <- NS(id)
  
  leaflet::leafletOutput(ns("map"))
}

zone_outsample_tableUI <- function(id){
  ns <- NS(id)
  
  shiny::dataTableOutput(ns("table"))
}

zone_outsample_saveUI <- function(id){
  ns <- NS(id)
  
  actionButton(ns('saveZones'), 'Save out-of-sample zones',
               style = "color: white; background-color: blue;")
}

zone_outsample_closeUI <- function(id){
  ns <- NS(id)
  
  actionButton(ns('close'), 'Close zone selection window',
               style="color: #fff; background-color: #FF6347; border-color: #800000;")
}


