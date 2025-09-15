# =================================================================================================
# File: zone_summary_module.R
# Description: This module allows users to select and remove one or more variables (columns)
#              from the main dataset.
#
# Authors: Paul Carvalho, Anna Abelman
# Date created: 8/28/2025
# Dependencies: shiny, DT
# Notes: This module is used within qaqc_module.R
# =================================================================================================

# Server ------------------------------------------------------------------------------------------
#' zone_summary_server
#'
#' @description Defines the server-side logic for the zone_summary module. 
#'
#' @param id A character string that is the namespace for this module.
#' @param rv_project_name A reactive value containing the name of the current project.
#' @param rv_data A reactive list containing the main dataset (`main`) and spatial data (`spat`).
#'
#' @return
zone_summary_server <- function(id, rv_folderpath, rv_project_name, rv_data  ){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Initialize reactives
    rv_selected_vars <- reactiveValues(vars = NULL)
    # Initialize output plot as a reactive
    rv_output <- reactiveValues(z_plot = NULL)
    
    # Update the selectInput with variable names from rv_data$main
    observe({
      req(rv_data$main)
      
      updateSelectInput(session, "zone_summ_var_input", 
                        choices =  c("observations",
                                     numeric_cols(rv_data$main)), selected = "observations"
      )
      updateSelectInput(session, "zone_summ_filter_var_input",
                        choices = c('none', colnames(rv_data$main)), selected = 'none')
    })
    
    # used for value input in zone summary
    zs_unique_values <- reactive({
      req(rv_data$main)
      if (is.null(input$zone_summ_filter_var)) {
        NULL
      } else {
        out <- unique(rv_data$main[[input$zone_summ_filter_var]])
        if (length(out) > 50)  out <- out[1:50]
        out
      }
    })
    
    # select value input for zone summary
    output$zone_summ_val_input <- renderUI({
      if (is.null(input$zone_summ_operator_input)) {
        selectizeInput(ns("zone_summ_value_input"), "Value", choices = "") # placeholder widget
      } else {
        selectizeInput(ns("zone_summ_value_input"), "Value",
                       choices = zs_unique_values(),
                       multiple = TRUE, options = list(maxOptions = 15, maxItems = 1,
                                                       placeholder = "Select or type value name",
                                                       create = TRUE))
      }
    })
    
    observeEvent(input$run_zone_summ_btn, {
      
      req(rv_data)
      zone_summ_df <- rv_data$main
      req(rv_project_name)
      req(rv_folderpath)
      project_name <- rv_project_name()$value
      folderpath <- rv_folderpath()
      
      # Load selected variables
      selected_vars <- load_gui_variables(project_name, folderpath)
      if (is.null(selected_vars)) {
        return()
      }
      rv_selected_vars$vars <- selected_vars
      # browser() 
      
      
      # If user wants to filter dataset
      if(!is.null(input$zone_summ_value) && input$zone_summ_value != ""){
        # Check if value should be wrapped in quotes
        if(any(class(zone_summ_df[[input$zone_summ_filter_var_input]]) %in% c("character", "factor", "Date", "POSIXct", "POSIXt"))){
          zone_summ_expr <- paste0(input$zone_summ_filter_var_input, input$zone_summ_operator_input, '"', input$zone_summ_value, '"')
        } else {
          zone_summ_expr <- paste0(input$zone_summ_filter_var_input, input$zone_summ_operator_input, input$zone_summ_value)
        }
        zone_summ_df <- eval(parse(text = paste0("subset(rv_data$main, ", zone_summ_expr, ")")))
      }
      
      if(input$zone_summ_var_input == 'observations'){
        zone_summ_count <- TRUE
        zone_summ_varIN <- NULL
        zone_summ_fun <- input$zone_summ_obs_input
      } else {
        zone_summ_count <- FALSE
        zone_summ_varIN <- input$zone_summ_var_input
        zone_summ_fun <- input$zone_summ_fun_input
      }
      
      if(zone_summ_fun == "number of obs"){
        fun_option <- NULL
      } else {
        fun_option <- zone_summ_fun
      }
      
      q_test <- quietly_test(zone_summary)
      rv_output$z_plot <- q_test(dat = zone_summ_df, project = project_name, spat = rv_data$spat,
                                 zone.dat = rv_selected_vars$vars$main$main_zone_id,
                                 zone.spat = rv_selected_vars$vars$spat$spat_zone_id,
                                 output = "plot", count = zone_summ_count, breaks = NULL, n.breaks = 10, na.rm = TRUE,
                                 fun = fun_option, var = zone_summ_varIN)
      # rv_output$z_plot <- zone_summary(dat = zone_summ_df, project = "scallopscallop", 
      #   spat = rv_data$spat,
      #   zone.dat = "ZoneID", 
      #   zone.spat = "TEN_ID",
      #   output = "plot", plot_type = "dynamic", count = TRUE, breaks = NULL, n.breaks = 10, na.rm = TRUE,
      #   fun = NULL, var = NULL)
      
    })
    
    output$plot_zone_summary <- leaflet::renderLeaflet({
      req(rv_output$z_plot) 
      rv_output$z_plot
      
    })
    
  })
}

# UI ----------------------------------------------------------------------------------------------
#' zone_summary_ui
#'
#' @description Creates the user interface for the remove variables module.
#'
#' @param id A character string that is the namespace for this module.
#'
#' @return A tagList containing the UI elements for the module.
zone_summary_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    selectInput(ns('zone_summ_var_input'), 
                'Select a variable to plot',
                choices = NULL),
    conditionalPanel(
      condition = "input.zone_summ_var_input == 'observations'",
      ns = ns,
      selectizeInput(ns('zone_summ_obs_input'),
                     'Select a function to summarize observations',
                     choices = c("number of obs","percent"))
    ),
    conditionalPanel(
      condition = "input.zone_summ_var_input != 'observations'",
      ns = ns,
      selectizeInput(ns('zone_summ_fun_input'),
                     'Select a function to summarize variable',
                     choices = c("sum","percent","mean","median","min","max"))
    ),
    selectInput(ns('zone_summ_filter_var_input'),
                'Select a variable for filtering the dataset',
                choices = NULL),
    conditionalPanel("input.zone_summ_filter_var_input!='none'",
                     ns = ns,
                     selectizeInput('zone_summ_operator_input', 'Select an operator',
                                    choices = c("less than" = "<",
                                                "greater than" = ">",
                                                "less than or equal to" = "<=",
                                                "greater than or equal to" = ">=",
                                                "equal to" = "==", "not equal to" = "!=")),
                     uiOutput(ns("zone_summ_val_input"))),
    actionButton(ns("run_zone_summ_btn"), "Run zone summary", width = "100%",
                 class = "btn-secondary"),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(strong("Map"), class = "bg-info"), 
      bslib::card_body(shinycssloaders::withSpinner(
        leaflet::leafletOutput(ns("plot_zone_summary"))
        ,type = 6))
    )
  )
}
