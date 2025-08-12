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
    
    ## Run spatial data checks --------------------------------------------------------------------
    observeEvent(input$run_spat_checks_btn, {
      req(rv_project_name)
      req(rv_folderpath)
      project_name <- rv_project_name()$value
      folderpath <- rv_folderpath()
      
      # Start spinner while running spatial checks
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
        if (any(rv_spat_check$flag)) {
          flag_cols <- flag_nms[which(rv_spat_check$flag)]
          rv_spat_check$spat_checks$dataset <- 
            rv_spat_check$spat_checks$dataset[,c(selected_vars$main$main_date, 
                                                 selected_vars$main$main_zone_id, 
                                                 selected_vars$main$main_lat,
                                                 selected_vars$main$main_lon,
                                                 flag_cols)]
        }
      }
      
      # Create the list of spatial check outputs based on flags
      new_choices <- c("Annual summary table" = "summary",
                       "All data table" = "all_data")
      
      if ("ON_LAND" %in% names(rv_spat_check$spat_checks$dataset)) {
        new_choices <- c(new_choices, "Obs on land" = "on_land")
      }
      
      if ("OUTSIDE_ZONE" %in% names(rv_spat_check$spat_checks$dataset)) {
        new_choices <- c(new_choices, "Obs out of spatial bounds" = "out_bounds")
      }
      
      # Update radio buttons
      updateRadioButtons(session,
                         "spat_check_output_view",
                         label = "Select an output to view:",
                         choices = new_choices,
                         selected = "summary")
      
      shinyjs::hide("spat_checks_spinner_container")
    })
    
    ## Display spatial check outputs --------------------------------------------------------------
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
    
    # On land plot
    output$on_land_plot <- renderPlot({
      req(rv_spat_check$spat_checks$land_plot)
    })
    
    # Out of spatial bounds plot outside_plot
    output$out_bounds_plot <- renderPlot({
      req(rv_spat_check$spat_checks$outside_plot)
    })
    
    ## Spatial corrections ------------------------------------------------------------------------
    # Show button to remove observations on land if present
    output$show_remove_land_btn <- reactive({
      "ON_LAND" %in% names(rv_spat_check$spat_checks$dataset)
    })
    
    outputOptions(output, "show_remove_land_btn", suspendWhenHidden = FALSE)
    
    # Show button to remove observations out of spatial bounds if present
    output$show_remove_out_bounds_btn <- reactive({
      "OUTSIDE_ZONE" %in% names(rv_spat_check$spat_checks$dataset)
    })
    
    outputOptions(output, "show_remove_out_bounds_btn", suspendWhenHidden = FALSE)
    
    observeEvent(input$remove_land_obs_btn,{
      req(rv_spat_check$spat_checks$land_plot)
      
      # Start spinner while removing observations
      shinyjs::show("remove_land_obs_spinner_container")
      
      land_rm_ind <- which(rv_spat_check$spat_checks$dataset$ON_LAND)
      
      if(length(land_rm_ind) > 0){
        # First change spat_check tables
        rv_spat_check$spat_checks$dataset <- rv_spat_check$spat_checks$dataset[-land_rm_ind, ]
        
        date_col_name <- names(rv_spat_check$spat_checks$dataset)[1]
        
        flag_nms <- c("ON_LAND", "OUTSIDE_ZONE", "ON_ZONE_BOUNDARY", "EXPECTED_LOC")
        flag_cols <- flag_nms[flag_nms %in% names(rv_spat_check$spat_checks$dataset)]
        
        rv_spat_check$spat_checks$spatial_summary <- 
          rv_spat_check$spat_checks$dataset %>%
          dplyr::mutate(YEAR = as.integer(format(!!rlang::sym(date_col_name), "%Y"))) %>%
          dplyr::group_by(YEAR) %>%
          dplyr::summarize(
            n = n(),
            dplyr::across(
              .cols = all_of(flag_cols),
              .fns = ~sum(., na.rm = TRUE)
            )
          ) %>%
          dplyr::ungroup()
        
        total_n <- sum(rv_spat_check$spat_checks$spatial_summary$n)
        
        rv_spat_check$spat_checks$spatial_summary <-
          rv_spat_check$spat_checks$spatial_summary %>%
          dplyr::mutate(perc = round((n / total_n) * 100, 2))

        # values$dataset <- values$dataset[-land_remove_i,] # change primary data table
        # spat_qaqc$out_df <- spat_qaqc$out_df[-land_remove_i,] # need to update the reactive
      }
      
      # showNotification(paste0(length(land_remove_i), " points removed"), type = "message", duration = 60)
      
      shinyjs::hide("remove_land_obs_spinner_container")
      
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
                          span(style = "white-space: nowrap; display: inline-flex; 
                               align-items: center;",
                               HTML("(<b>OPTIONAL</b>) Enter spatial reference EPSG code: &nbsp;"),
                               bslib::tooltip(
                                 shiny::icon("circle-info", `aria-label` = "More information"),
                                 HTML("<h4>EPSG Information</h4><hr>
                                      Option to manually set the spatial reference EPSG code for
                                      spatial and primary datasets. If EPSG is specified in the 
                                      spatial data and this box is left empty, then the EPSG of 
                                      the spatial data will be automatically applied to primary 
                                      data. For more information on spatial reference systems 
                                      visit - <a href='https://spatialreference.org/' 
                                      target='_blank'>https://spatialreference.org/</a>"),
                                 options = list(delay = list(show = 0, hide = 850))
                               )
                          )
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
                         "Run spatial checks",
                         style = "display: inline-block; width: 315px;"
            )
          )
        )
      ),
      
      # Spatial corrections card
      column(
        6,
        bslib::card(
          bslib::card_header("2. Spatial Corrections"),
          bslib::card_body(
            conditionalPanel(
              condition = "output.show_remove_land_btn",
              ns = ns,
              actionButton(ns("remove_land_obs_btn"), 
                           "Remove observations on land",
                           style = "display: inline-block; width: 315px;"
              ),
            ),
            
            conditionalPanel(
              condition = "output.show_remove_out_bounds_btn",
              ns = ns,
              actionButton(ns("remove_out_bounds_obs_btn"),
                           "Remove out-of-bounds observations",
                           style = "display: inline-block; width: 315px;"
              )
            ),
            
            # Overlay spinner for this section
            div(id = ns("remove_land_obs_spinner_container"),
                style = "display: none;",
                spinner_ui(ns("remove_land_obs_spinner"),
                           spinner_type = "circle",
                           size = "large",
                           message = "Removing observations on land...",
                           overlay = TRUE)
            ),
          )
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
          ),
          
          conditionalPanel(
            condition = "input.spat_check_output_view == 'on_land'",
            ns = ns,
            plotOutput(ns("on_land_plot"), width = "650px", height = "450px")
          ),
          
          conditionalPanel(
            condition = "input.spat_check_output_view == 'out_bounds'",
            ns = ns,
            plotOutput(ns("out_bounds_plot"), width = "650px", height = "450px")
          )
        )
      )  
    )
  )
}
