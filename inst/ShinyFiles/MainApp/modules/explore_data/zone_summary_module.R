# =================================================================================================
# File: zone_summary_module.R
# 
# Description: This module allows users to counts observations and aggregates values in data frame
#'            by regulatory zone or closure area and preview as an interactive map or static
#
# Authors:  Anna Abelman, Paul Carvalho
# Date created: 9/22/2025
# Dependencies: shiny, leaflet, ggplot2, sf
# Notes: This module is used within explore_data_module.R
# =================================================================================================

# Server ------------------------------------------------------------------------------------------
#' zone_summary_server
#'
#' @description Defines the server-side logic for the zone_summary module. 
#'
#' @param id A character string that is the namespace for this module.
#' @param rv_project_name A reactive value containing the name of the current project.
#' @param rv_data A reactive list containing the main dataset (`main`) and spatial data (`spat`).
#' @param rv_folderpath A reactive value containing the file path to the project's root folder.

#' @return A module server instance.

zone_summary_server <- function(id, rv_folderpath, rv_project_name, rv_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Initialize reactives
    rv_selected_vars <- reactiveValues(vars = NULL)
    rv_output <- reactiveValues(z_plot = NULL) #output plot as a reactive
    rv_zone_variables <- reactiveValues(
      dat = NULL,
      count = NULL, 
      group = NULL,
      fun = NULL,
      var = NULL
    )
    rv_zone_map_static <- reactiveValues(z_plot = NULL)
    
    # Update the selectInput with variable names from rv_data$main
    observe({
      req(rv_data$main)
      
      updateSelectInput(session, "zone_summ_var_input", 
                        choices =  c("observations", numeric_cols(rv_data$main)), 
                        selected = "observations")
      
      updateSelectInput(session, "zone_summ_filter_var_input",
                        choices = c('none', colnames(rv_data$main)), 
                        selected = 'none')
      
      updateSelectInput(session, "zone_summ_group_input",
                        choices = c('none', colnames(rv_data$main)), 
                        selected = 'none')
    })
    
    # used for value input in zone summary
    zs_unique_values <- reactive({
      req(rv_data$main)
      if (is.null(input$zone_summ_filter_var_input)) {
        NULL
        
      } else {
        out <- unique(rv_data$main[[input$zone_summ_filter_var_input]])
        
        if (length(out) > 50)  out <- out[1:50]
        out
      }
    })
    
    # select value input for zone summary
    output$zone_summ_val_input <- renderUI({
      selectizeInput(ns("zone_summ_value_input"), "Value",
                     choices = zs_unique_values(),
                     multiple = FALSE, 
                     options = list(maxOptions = 15, maxItems = 1,
                                    placeholder = "Select or type value name",
                                    create = TRUE))
    })
    
    # run zone_summary function for dynamic plot
    observeEvent(input$run_zone_summ_btn, {
      req(rv_data)
      rv_zone_variables$dat <- rv_data$main
      req(rv_project_name)
      req(rv_folderpath)
      project_name <- rv_project_name()$value
      folderpath <- rv_folderpath()
      
      shinyjs::show("zone_summ_spinner_container")
      
      # Load selected variables
      selected_vars <- load_gui_variables(project_name, folderpath)
      if (is.null(selected_vars)) {
        # Handle the case where the RDS file does not exist.
        shinyjs::hide("zone_summ_spinner_container")
        showModal(modalDialog(
          title = "Error: Missing Data",
          "The selected variables file for the current project could not be found. 
          Please ensure you have selected and saved variables in the previous tab.",
          easyClose = TRUE
        ))
        return() # Stop execution of the observer
      }
      rv_selected_vars$vars <- selected_vars
      
      # If user wants to filter dataset
      if(!is.null(input$zone_summ_value_input) && input$zone_summ_value_input != ""){
        # Check if value should be wrapped in quotes
        if(any(class(rv_zone_variables$dat[[input$zone_summ_filter_var_input]]) %in% 
               c("character", "factor", "Date", "POSIXct", "POSIXt"))){
          zone_summ_expr <- paste0("`",input$zone_summ_filter_var_input,"`", 
                                   input$zone_summ_operator_input, '"',  
                                   input$zone_summ_value_input, '"')
        } else {
          zone_summ_expr <- paste0(input$zone_summ_filter_var_input,
                                   input$zone_summ_operator_input,
                                   input$zone_summ_value_input)
        }
        rv_zone_variables$dat <- eval(parse(text = paste0("subset(rv_data$main, ",
                                                          zone_summ_expr, ")")))
      }
      
      # Saving user selected values
      if(input$zone_summ_var_input == 'observations'){
        rv_zone_variables$count <- TRUE
        rv_zone_variables$var <- NULL
        rv_zone_variables$fun <- input$zone_summ_obs_input
        
      } else {
        rv_zone_variables$count <- FALSE
        rv_zone_variables$var <- input$zone_summ_var_input 
        rv_zone_variables$fun <- input$zone_summ_fun_input
      }
      
      if(rv_zone_variables$fun == "number of obs"){
        rv_zone_variables$fun <- NULL
        
      } else {
        rv_zone_variables$fun <- rv_zone_variables$fun
      }
      
      if(input$zone_summ_group_input == "none"){
        rv_zone_variables$group <- NULL
        
      } else{
        rv_zone_variables$group <- input$zone_summ_group_input
      }
      
      # Run zone_summary function
      q_test <- quietly_test(zone_summary)
      rv_output$z_plot <- q_test(dat = rv_zone_variables$dat, 
                                 project = project_name,
                                 spat = rv_data$spat,
                                 zone.dat = rv_selected_vars$vars$main$main_zone_id,
                                 zone.spat = rv_selected_vars$vars$spat$spat_zone_id,
                                 dat_lat = rv_selected_vars$vars$main$main_lat,
                                 dat_lon = rv_selected_vars$vars$main$main_lon,
                                 output = "plot", 
                                 count = rv_zone_variables$count, 
                                 na.rm = TRUE,
                                 group = rv_zone_variables$group,
                                 fun = rv_zone_variables$fun,
                                 var = rv_zone_variables$var)
      
      shinyjs::hide("zone_summ_spinner_container")
    })
    
    output$plot_zone_summary <- leaflet::renderLeaflet({
      req(rv_output$z_plot) 
      rv_output$z_plot
    })
    
    # shows a modal that contains the plot preview of static plot
    observeEvent(input$show_modal_btn, {
      req(rv_data)
      req(rv_project_name()$value)
      req(rv_folderpath)  
      req(rv_zone_variables)
      
      rv_zone_map_static$z_plot <- zone_summary(
        dat = rv_zone_variables$dat, 
        project = rv_project_name()$value,
        spat = rv_data$spat,
        zone.dat = rv_selected_vars$vars$main$main_zone_id,
        zone.spat = rv_selected_vars$vars$spat$spat_zone_id,
        output = "plot", 
        count = rv_zone_variables$count, 
        na.rm = TRUE,
        plot_type = "static",
        group = rv_zone_variables$group,
        fun = rv_zone_variables$fun,
        var = rv_zone_variables$var)
      
      showModal(modalDialog(
        title = "Preview saved static map",
        # Renders the plot generated by output$preview_plot
        plotOutput(ns("preview_plot"), height = "450px"),
        footer = modalButton("Close"),
        easyClose = TRUE,
        size = "l" 
      ))
    })
    
    # Render the plot for the preview window
    output$preview_plot <- renderPlot({
      rv_zone_map_static$z_plot
    })
  })
}

# UI ----------------------------------------------------------------------------------------------
#' zone_summary_ui
#'
#' @description Creates the user interface for the zone summary module.
#'
#' @param id A character string that is the namespace for this module.
#'
#' @return A tagList containing the UI elements for the module.
#' 
zone_summary_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    bslib::layout_column_wrap(
      width = 1/2,
      bslib::card(
        selectInput(ns('zone_summ_var_input'), 
                    'Select a variable to plot',
                    choices = NULL,
                    width = "100%")),
      bslib::card(
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
          bslib::layout_column_wrap(
            width = 1/2,
            selectizeInput(ns('zone_summ_fun_input'),
                           'Select a function to summarize variable',
                           choices = c("sum","percent","mean","median","min","max")),
            selectInput(ns('zone_summ_group_input'), 
                        'Select a variable to group by',
                        choices = NULL))
        ))),
    
    bslib::card(
      bslib::layout_column_wrap(
        width = 1/2,
        selectInput(ns('zone_summ_filter_var_input'),
                    'Select a variable for filtering the dataset',
                    choices = NULL),
        conditionalPanel("input.zone_summ_filter_var_input!='none'",
                         ns = ns,
                         bslib::layout_column_wrap(
                           width = 1/2,
                           selectizeInput(ns('zone_summ_operator_input'), 'Select an operator',
                                          choices = c("less than" = "<",
                                                      "greater than" = ">",
                                                      "less than or equal to" = "<=",
                                                      "greater than or equal to" = ">=",
                                                      "equal to" = "==", "not equal to" = "!=")),
                           uiOutput(ns("zone_summ_val_input")))))),
    actionButton(ns("run_zone_summ_btn"), 
                 "Run zone summary", width = "25%",
                 class = "btn-secondary"),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(strong("Zone Summary"), class = "bg-info"), 
      bslib::card_body(
        leaflet::leafletOutput(ns("plot_zone_summary"))) ),
    actionButton(ns("show_modal_btn"),
                 "Save as static map", 
                 icon = icon("save"), 
                 width = "25%"),
    # Container for the loading spinner, shown while checks are running.
    div(id = ns("zone_summ_spinner_container"),
        style = "display: none;",
        spinner_ui(ns("zone_summ_spinner"),
                   spinner_type = "circle",
                   size = "large",
                   message = "Running zone summary...",
                   overlay = TRUE)
    )
  )
}
