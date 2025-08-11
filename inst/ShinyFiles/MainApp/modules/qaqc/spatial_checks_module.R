# =================================================================================================
# File: spatial_checks_module.R
# Description: 
#
# Authors: Paul Carvalho, Anna Abelman
# Date created: 8/8/2025
# Dependencies: 
# Notes: 
# =================================================================================================

# Source module scripts ---------------------------------------------------------------------------
source("modules/spinner.R", local = TRUE)

spatial_checks_server <- function(id, rv_project_name, rv_data, rv_folderpath){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    observeEvent(input$run_spat_checks, {
      req(rv_project_name)
      req(rv_folderpath)
      project_name <- rv_project_name()$value
      folderpath <- rv_folderpath()
      
      # Start spinner while it loads
      shinyjs::show("spat_checks_spinner_container")
      
      # Load selected variables
      file_name <- paste0(project_name, "SavedVariables.rds")
      file_path <- normalizePath(file.path(folderpath, project_name, "data", file_name))
      
      # Read selected variables RDS
      selected_vars <- readRDS(file_path)
      
      cat(str(selected_vars))
      
      # q_test <- quietly_test(spatial_qaqc)
      # 
      # out <- q_test(dat = rv_data$main, project = project_name, spat = rv_data$spat,
      #               lon.dat = selected_vars$main$main_zone_lon, 
      #               lat.dat = selected_vars$main$main_zone_lat,
      #               date = selected_vars$main$main_zone_date,
      #                group = input$spat_qaqc_grp, epsg = input$spat_qaqc_epsg)
      
      Sys.sleep(2)
      
      shinyjs::hide("spat_checks_spinner_container")
      
      
    })
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
            
            # Overlay spinner for this section
            div(id = ns("spat_checks_spinner_container"),
                style = "display: none;",
                spinner_ui(ns("spat_checks_spinner"),
                           spinner_type = "circle",
                           size = "large",
                           message = "Running spatial checks...",
                           overlay = TRUE)
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
