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
    
    # Initialize reactives
    rv_spat_check <- reactiveValues(flag = FALSE, spat_checks = NULL, c_tab = NULL)
    
    # Settings to hide outputs until run_spat_checks_btn
    output$show_output_cards <- reactive({
      !is.null(rv_spat_check$spat_checks)
    })
    
    # Ensure the output is available for the conditionalPanel to check in the ui
    outputOptions(output, "show_output_cards", suspendWhenHidden = FALSE)
    
    # Run spatial data checks
    observeEvent(input$run_spat_checks_btn, {
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
      
      q_test <- quietly_test(spatial_qaqc)
      
      spat_check_out <- q_test(dat = rv_data$main, 
                               project = project_name, 
                               spat = rv_data$spat,
                               lon.dat = selected_vars$main$main_lon,
                               lat.dat = selected_vars$main$main_lat,
                               date = selected_vars$main$main_date,
                               epsg = input$epsg_code_input)
      
      if (!is_value_empty(spat_check_out)) {
        # Check for data quality issues
        flag_nms <- c("ON_LAND", "OUTSIDE_ZONE", "ON_ZONE_BOUNDARY", "EXPECTED_LOC")
        rv_spat_check$flag <- flag_nms %in% names(spat_check_out$dataset)
        
        # Save output to reactive value (and remove year from dataframe)
        rv_spat_check$spat_checks <- spat_check_out
        rv_spat_check$spat_checks$dataset <- subset(rv_spat_check$spat_checks$dataset, 
                                                    select = -c(YEAR))
        
        # Update dataset based on data quality flags
      }
      
      shinyjs::hide("spat_checks_spinner_container")
    })
    
    # Render interactive summary data table
    output$spat_check_summary <- DT::renderDT({
      DT::datatable(
        rv_spat_check$spat_checks$spatial_summary,
        rownames = FALSE,
        filter = "top",
        options = list(
          scrollX = TRUE
        )
      )
    })
    
    # Render interactive data table with all data
    output$spat_check_alldata <- DT::renderDT({
      DT::datatable(
        rv_spat_check$spat_checks$dataset,
        rownames = FALSE,
        filter = "top",
        options = list(
          scrollX = TRUE
        )
      )
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
            textInput(ns("epsg_code_input"), 
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
            
            actionButton(ns("run_spat_checks_btn"),
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
    ),
    
    conditionalPanel(
      # Check that run spatial checks has been clicked
      condition = "output.show_output_cards",
      ns = ns,
      bslib::card(
        bslib::card_header("Spatial data quality checks"),
        bslib::card_body(
          # Options for display tables
          radioButtons(ns("spat_check_output_view"),
                       label = "Select an output to view:",
                       choices = c("Annual summary table" = "summary",
                                   "All data table" = "all_data"),
                       selected = "summary"),
          
          conditionalPanel(
            condition = "input.spat_check_output_view == 'summary'",
            ns = ns,
            DT::DTOutput(ns("spat_check_summary"))  
          ),
          
          conditionalPanel(
            condition = "input.spat_check_output_view == 'all_data'",
            ns = ns,
            DT::DTOutput(ns("spat_check_alldata"))  
          )
        )
      )  
    )
  )
}
