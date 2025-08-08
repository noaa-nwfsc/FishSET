# =================================================================================================
# File: spatial_checks_module.R
# Description: 
#
# Authors: Paul Carvalho, Anna Abelman
# Date created: 8/8/2025
# Dependencies: 
# Notes: 
# =================================================================================================

spatial_checks_server <- function(id, rv_project_name, rv_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
  })
}

spatial_checks_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    fluidRow(
      # Spatial checks card
      column(
        6,
        bslib::card(
          bslib::card_header("1. Spatial Checks"),
          bslib::card_body(
            textInput(ns("epsg_code"), 
                      value = NULL,
                      label = 
                        tagList(
                          HTML("(<b>OPTIONAL</b>) Enter spatial reference EPSG code: &nbsp;"),
                          bslib::tooltip(
                            shiny::icon("circle-info", `aria-label` = "More information"),
                            HTML("<h4>EPSG Information</h4><hr>
                               Option to manually set the spatial reference EPSG code for
                               spatial and primary datasets. If EPSG is specified in the spatial 
                               data and this box is left empty, then the EPSG of the spatial data 
                               will be automatically applied to primary data. For more information 
                               on spatial reference systems visit - 
                               <a href='https://spatialreference.org/' 
                               target='_blank'>https://spatialreference.org/</a>"),
                            options = list(delay = list(show = 0, hide = 850))
                          ),
                        )
            ),
            
            actionButton(ns("run_spat_checks"),
                         "Run spatial checks"
            )
          )
        )
      ),
      
      # Spatial corrections card
      column(
        6,
        bslib::card(
          bslib::card_header("2. Spatial Corrections")
        )
      )
    )
  )
}
