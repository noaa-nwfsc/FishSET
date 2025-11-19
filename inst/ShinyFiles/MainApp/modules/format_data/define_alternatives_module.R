# =================================================================================================
# File: define_alternatives_module.R
# Description: This module defines the UI and server logic for.
#
# Authors: Anna Abelman,  Paul Carvalho
# Date created: 11/6/2025
# Dependencies: shiny, DT 
# Notes: This module sources other explore the data modules saved in the 
# =================================================================================================


# define alternatives server --------------------------------------------------------------------
#' define_alt_server
#'
#' @description Defines the server-side logic for the 
#'
#' @param id A character string that is unique to this module instance.
#' @param rv_project_name A reactive value containing the current project name.
#' @param rv_data A reactiveValues object containing the loaded data frames.
#'
#' @return This module does not return a value.
define_alt_server <- function(id, rv_folderpath, rv_project_name, rv_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Initialize reactives
    rv_selected_vars <- reactiveValues(vars = NULL)
    # Initialize DT output for modal preview
    output$preview_table <- DT::renderDataTable(NULL)
    
    observe({
      req(rv_data$main)
      
      # Pass the valid selections back to the `selected` argument
      updateSelectInput(session, "occ_lon_input", 
                        choices = find_lon(rv_data$main))
      
      updateSelectInput(session, "occ_lat_input", 
                        choices = find_lat(rv_data$main))
      
      updateSelectInput(session, "occ_lag_zone_id_input", 
                        choices = colnames(rv_data$main))
    })
    
    # Save alternative choice 
    observeEvent(input$altc_save_btn, {
      
      project_name <- rv_project_name()$value
      folderpath <- rv_folderpath()  
      req(rv_data$main)
      req(rv_data$spat)
      
      # Show spinner
      shinyjs::show("define_alt_container")
      on.exit(shinyjs::hide("define_alt_container"), add = TRUE)
      
      # Load selected variables
      selected_vars <- load_gui_variables(project_name, folderpath)
      
      if (is.null(selected_vars)) {
        showModal(modalDialog(
          title = "Error: Missing Selections",
          "The selected variables file could not be found. Please select and save variables in 
          the 'Select variables' tab.",
          easyClose = TRUE
        ))
        return()
      }
      
      rv_selected_vars$vars <- selected_vars
      
      cent_table_name <- paste0(project_name, "_ZoneCentroid")
      
      if (!table_exists(cent_table_name, project_name)) {
        showModal(modalDialog(
          title = "Error: Missing zonal centroid table",
          "The zonal centroid table could not be found. Please select and save variables in 
          the 'Select variables' tab for the zonal centroid table to be created.",
          easyClose = TRUE
        ))
        return()
      }
      
      
      # switch to values that function accepts
      occ_type <- switch(input$altc_occasion_input, 
                         'port' = 'port', 
                         'zonal centroid' = 'zonal centroid',
                         'lon-lat' = 'lon-lat')
      
      q_test <- quietly_test(create_alternative_choice, show_msg = TRUE)
      
      run_result <- NULL
      
      if(input$altc_occasion_input == "zonal centroid"){
        run_result <-q_test(dat=rv_data$main, 
                            project=project_name,
                            occasion=occ_type,
                            occasion_var=input$occ_lag_zone_id_input,
                            alt_var="zonal centroid", 
                            min.haul=input$altc_min_haul_input, 
                            zoneID=rv_selected_vars$vars$main$main_zone_id,
                            spatID=rv_selected_vars$vars$spat$spat_zone_id,
                            zone.cent.name=cent_table_name)
        
      } else if(input$altc_occasion_input == "lon-lat"){
        run_result <- q_test(dat=rv_data$main,
                             project=project_name, 
                             occasion=occ_type,
                             occasion_var=c(input$occ_lon_input, input$occ_lat_input),
                             alt_var="zonal centroid", 
                             min.haul=input$altc_min_haul_input, 
                             zoneID=rv_selected_vars$vars$main$main_zone_id,
                             spatID=rv_selected_vars$vars$spat$spat_zone_id,
                             zone.cent.name=cent_table_name)
        
      } else if(input$altc_occasion_input == "port"){
        run_result <- q_test(dat=rv_data$main,
                             project=project_name, 
                             occasion=occ_type,
                             occasion_var=rv_selected_vars$vars$port$port_name,
                             alt_var="zonal centroid", 
                             min.haul=input$altc_min_haul_input, 
                             zoneID=rv_selected_vars$vars$main$main_zone_id,
                             spatID=rv_selected_vars$vars$spat$spat_zone_id,
                             zone.cent.name=cent_table_name)
        
      }
      
      if (!is.null(run_result) && (!is.null(run_result$error) || length(run_result$warnings) > 0)) {
        # The error message was already shown by q_test(show_msg = TRUE)
        shinyjs::hide("preview_card_container")
        return()
      } 
      
      # 1. Get the 'Alt' list from the function's result
      alt_list <- unserialize_table(paste0(project_name, "AltMatrix"), project_name)
      
      # 2. Convert the complex list into a simple data frame for preview
      param_names <- names(alt_list)
      param_values <- sapply(alt_list, function(x) {
        # This helper function creates a simple text summary for each list item
        if (is.data.frame(x)) {
          return(paste0("[data.frame] with ", nrow(x), " rows"))
        } else if (is.list(x)) {
          return("[list object]")
        } else if (is.null(x)) {
          return("NULL")
        } else if (length(x) > 10) {
          # Truncate long vectors
          return(paste0("[vector] ", length(x), " items. E.g: ", paste(head(x, 3), collapse=", "), "..."))
        } else {
          # Show short vectors/single values
          return(paste(as.character(x), collapse = ", "))
        }
      }, USE.NAMES = FALSE)
      
      preview_df <- data.frame(
        Item = param_names,
        Value = param_values
      )
      
      output$preview_table <- DT::renderDataTable({
        DT::datatable(
          preview_df,
          options = list(pageLength = 20, scrollX = TRUE),
          rownames = FALSE,
          caption = "Summary of 'Alt' list contents."
        )
      })
      
      # 5. Show the hidden card
      shinyjs::show("preview_card_container")
      
      # You could also add a success notification
      showNotification(
        "Alternative choices saved successfully. Preview updated below.",
        type = "message"
      )
    })
  })
}




# Define alternative sidebar ---------------------------------------------------------------------
#' define_alt_ui
#'
#' @description Creates the sidebar UI for the compute new variables tab. This includes radio 
#' buttons that allow the user to select between format data functions.
#'
#' @param id A character string that is unique to this module instance.
#'
#' @return A tagList containing the sidebar UI elements.
define_alt_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4(tags$b('Define how alternative fishing choices are calculated between occurrence and
             alternative location')),
    bslib::layout_column_wrap( 
      width = 1/2,
      bslib::card(
        bslib::card_header("Starting location"), 
        bslib::card_body(
          fillable = FALSE, 
          bslib::layout_column_wrap(
            width = 1/2,
            fillable = FALSE,
            padding = 0,
            selectInput(ns('altc_occasion_input'), '', 
                        choices=c('Port' = 'port',
                                  'Lagged haul location' = 'zonal centroid', 
                                  'Lon-Lat Coordinates' = 'lon-lat')),
          ),
          tags$br(),
          conditionalPanel(
            "input.altc_occasion_input=='lon-lat'",
            ns = ns,
            selectInput(ns("occ_lon_input"),
                        "Choose longitude occasion columns",
                        choices = NULL),
            selectInput(ns("occ_lat_input"), 
                        "Choose latitude occasion columns",
                        choices = NULL)
          ),
          conditionalPanel(
            "input.altc_occasion_input=='zonal centroid'",
            ns = ns,
            selectInput(ns("occ_lag_zone_id_input"),
                        "Select the lagged zone ID",
                        choices = NULL)
          )
        )
      ),
      bslib::card(
        bslib::card_header("Set threshold for inclusion of alternatives"), 
        bslib::card_body(
          fillable = TRUE, 
          bslib::layout_column_wrap(
            width = 1/2,
            fillable = FALSE,
            numericInput(ns('altc_min_haul_input'), 
                         'Include zones with more observations than', 
                         min = 1, max = 1000, value = 1),
          )
        )
      ),
      actionButton(ns('altc_save_btn'),'Save choices', class= "btn-primary"), 
      # This card will hold the preview table and is hidden on load
      shinyjs::hidden(
        div(id = ns("preview_card_container"),
            bslib::card(
              bslib::card_header("Preview of Saved 'Alt' List Contents"),
              bslib::card_body(
                DT::dataTableOutput(ns("preview_table"))
              )
            )
        )
      )
      # bslib::card(
      #   bslib::card_header("Set threshold for inclusion of alternatives"), 
      #   bslib::card_body(
      #     fillable = TRUE, plotOutput('zone_include_plot')
      #   )
      # )
    )
  )
  
}