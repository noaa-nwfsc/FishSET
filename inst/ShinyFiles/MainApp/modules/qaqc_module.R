# =================================================================================================
# File: qaqc_module.R
# Description: 
#
# Authors: Paul Carvalho, Anna Abelman
# Date created: 8/4/2025
# Dependencies: 
# Notes: 
# =================================================================================================

# Source module scripts ---------------------------------------------------------------------------
source("modules/qaqc/preview_data_module.R", local = TRUE) # Preview data in table format

# 
qaqc_server <- function(id, rv_project_name, rv_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    preview_data_server("preview_data", rv_project_name, rv_data)
  })
}

#
qaqc_ui <- function(id){
  ns <- NS(id)
  
  preview_data_ui(ns("preview_data"))
}
