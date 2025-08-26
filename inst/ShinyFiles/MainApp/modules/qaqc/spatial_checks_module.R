# =================================================================================================
# File: spatial_checks_module.R
# Description: This file contains a Shiny module for executing and visualizing spatial checks,
#              and correcting spatial data quality issues (observations on land or outside of
#              spatial boundaries). The module allows users to remove observations with quality
#              issues, which updates the main data table in the SQLite database.
#
# Authors: Paul Carvalho, Anna Abelman
# Date created: 8/8/2025
# Dependencies: shiny, shinyjs, bslib, DT, dplyr, rlang
# Notes: This module is intended to be used within a larger Shiny app and communicates with 
#        qaqc_module.R and the main app (server.R and ui.R) through inputs and outpus.
# =================================================================================================

# Source module scripts ---------------------------------------------------------------------------
source("modules/spinner.R", local = TRUE)

# Server ------------------------------------------------------------------------------------------
#' spatial_checks_server
#'
#' @description Manages the server-side logic for running spatial checks, displaying results,
#' and handling data correction actions.
#'
#' @param id A character string that is the namespace for this module.
#' @param rv_project_name A reactive value containing the name of the current project.
#' @param rv_data A reactive list containing the main dataset (`main`) and spatial data (`spat`).
#' @param rv_folderpath A reactive value containing the file path to the project's root folder.
#'
#' @return A reactive list containing `$ids`, the unique identifiers of observations to be removed,
#'         and `$id_col`, the name of the unique ID column.
spatial_checks_server <- function(id, rv_project_name, rv_data, rv_folderpath){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Initialize reactives
    rv_selected_vars <- reactiveValues(vars = NULL)
    rv_spat_check <- reactiveValues(flag = FALSE, spat_checks = NULL, c_tab = NULL)
    rv_remove_ids <- reactiveValues(ids = NULL)
    rv_status <- reactiveVal("pending") # Overall status: pending, passed, failed
    rv_land_obs_message <- reactiveVal("")
    rv_out_obs_message <- reactiveVal("")
    
    # Reactive flag to control the visibility of data quality check output UI
    output$show_output_cards <- reactive({
      !is.null(rv_spat_check$spat_checks)
    })
    
    # Ensure the output is available for the conditionalPanel to check in the ui
    outputOptions(output, "show_output_cards", suspendWhenHidden = FALSE)
    
    ## Load and check status on startup -----------------------------------------------------------
    observeEvent(rv_data$main, {
      req(rv_project_name()$value)
      req(rv_folderpath())
      
      # Define the path for the status file
      status_file_path <- file.path(rv_folderpath(),
                                    rv_project_name()$value,
                                    "data",
                                    "SpatialChecksStatus.rds")
      status_file_path <- suppressWarnings(normalizePath(status_file_path))
      
      if (file.exists(status_file_path)) {
        saved_status <- readRDS(status_file_path)
        
        # Create a "fingerprint" of the current data using row and size
        current_data_metrics <- list(
          nrows = nrow(rv_data$main),
          size = object.size(rv_data$main)
        )
        
        # If values in RDS are null then something went wrong with saving data
        if (is.null(saved_status$data_metrics$nrow) | 
            saved_status$data_metrics$size == 0) {
          rv_status("pending")
          # If metrics match, the data is unchanged, and can load the "passed" status
        } else if ((current_data_metrics$nrows == saved_status$data_metrics$nrow) &&
                   (saved_status$status == "passed")) {
          rv_status("passed")
          
        } else {
          # If metrics don't match, the data has changed and spatial checks have to run again
          file.remove(status_file_path)
          rv_status("pending")
        }
        
      } else {
        # If no status file exists, checks need to be run
        rv_status("pending")
      }
    })
    
    # Reactive to control UI elements based on rv_status()
    output$rv_status_controls <- reactive({
      rv_status() == "passed"
    })
    
    # Ensure the output is available for the conditionalPanel to check in the ui
    outputOptions(output, "rv_status_controls", suspendWhenHidden = FALSE)
    
    ## Run spatial data checks --------------------------------------------------------------------
    observeEvent(input$run_spat_checks_btn, {
      # Ensure required reactive values are available before proceeding.
      req(rv_project_name)
      req(rv_folderpath)
      project_name <- rv_project_name()$value
      folderpath <- rv_folderpath()
      
      # Hide any messages from previous correction actions.
      shinyjs::hide("remove_land_obs_message")
      shinyjs::hide("remove_out_obs_message")
      
      # Show a spinner to indicate that the checks are running.
      shinyjs::show("spat_checks_spinner_container")
      
      # Define the path for the status file for saving/deleting.
      status_file_path <- file.path(folderpath, project_name, "data", "SpatialChecksStatus.rds")
      status_file_path <- suppressWarnings(normalizePath(status_file_path))
      
      # Construct the file path for the saved variables.
      file_name <- paste0(project_name, "SavedVariables.rds")
      file_path <- normalizePath(file.path(folderpath, project_name, "data", file_name))
      
      # Read the RDS file containing selected variable names.
      selected_vars <- readRDS(file_path)
      rv_selected_vars$vars <- selected_vars
      
      # Wrap the main spatial QA/QC function to catch any errors or warnings quietly.
      q_test <- quietly_test(spatial_qaqc)
      
      # Execute the spatial QA/QC function with the required data and parameters.
      spat_check_out <- q_test(dat = rv_data$main, 
                               project = project_name, 
                               spat = rv_data$spat,
                               lon.dat = selected_vars$main$main_lon,
                               lat.dat = selected_vars$main$main_lat,
                               date = selected_vars$main$main_date,
                               epsg = input$epsg_code_input)
      
      # Process the output if the checks ran successfully.
      if (!is_value_empty(spat_check_out)) {
        # Identify which data quality flags were generated.
        flag_nms <- c("ON_LAND", "OUTSIDE_ZONE", "ON_ZONE_BOUNDARY", "EXPECTED_LOC")
        rv_spat_check$flag <- flag_nms %in% names(spat_check_out$dataset)
        
        # Save the output to a reactive value and remove the 'YEAR' column
        rv_spat_check$spat_checks <- spat_check_out
        rv_spat_check$spat_checks$dataset <- subset(rv_spat_check$spat_checks$dataset, 
                                                    select = -c(YEAR))
        
        # If any flags were raised, subset the dataset to show only key and flag columns.
        if (any(rv_spat_check$flag)) {
          flag_cols <- flag_nms[which(rv_spat_check$flag)]
          
          rv_spat_check$spat_checks$dataset <- 
            rv_spat_check$spat_checks$dataset[,c(selected_vars$main$main_unique_obs_id,
                                                 selected_vars$main$main_date, 
                                                 selected_vars$main$main_zone_id, 
                                                 selected_vars$main$main_lat,
                                                 selected_vars$main$main_lon,
                                                 flag_cols)]
        }
      }
      
      # Save or delete status file based on results
      checks_passed <- !any(c("ON_LAND", "OUTSIDE_ZONE") %in% names(spat_check_out$dataset))
      if (checks_passed) {
        # If checks passed, save the status and the data metrics
        current_data_metrics <- list(
          nrows = nrow(spat_check_out$dataset),
          size = object.size(spat_check_out$dataset)
        )
        
        # Save RDS and reactive to indicate spatial checks passed
        status_to_save <- list(status = "passed", data_metrics = current_data_metrics)
        saveRDS(status_to_save, file = status_file_path)
        rv_status("passed")
        
      } else {
        # If checks failed, delete pre-existing status file
        if (file.exists(status_file_path)) {
          file.remove(status_file_path)
        }
        rv_status("failed")
      }
      
      # Dynamically create the list of choices for the output view radio buttons.
      new_choices <- c("Annual summary table" = "summary",
                       "All data table" = "all_data")
      
      # Add options to view plots if corresponding flags exist.
      if ("ON_LAND" %in% names(rv_spat_check$spat_checks$dataset)) {
        new_choices <- c(new_choices, "Obs on land" = "on_land")
      }
      
      if ("OUTSIDE_ZONE" %in% names(rv_spat_check$spat_checks$dataset)) {
        new_choices <- c(new_choices, "Obs out of spatial bounds" = "out_bounds")
      }
      
      # Update the radio buttons in the UI with the new choices.
      updateRadioButtons(session,
                         "spat_check_output_view",
                         label = "Select an output to view:",
                         choices = new_choices,
                         selected = "summary")
      
      # Hide the spinner now that processing is complete.
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
      rv_spat_check$spat_checks$land_plot # Display the plot object.
    })
    
    # Out of spatial bounds plot outside_plot
    output$out_bounds_plot <- renderPlot({
      req(rv_spat_check$spat_checks$outside_plot)
      rv_spat_check$spat_checks$outside_plot # Display the plot object.
    })
    
    ## Spatial corrections ------------------------------------------------------------------------
    ### Remove land observations ------------------------------------------------------------------
    # Reactive to determine if the "Remove observations on land" button should be shown.
    output$show_remove_land_btn <- reactive({
      "ON_LAND" %in% names(rv_spat_check$spat_checks$dataset)
    })
    
    # Ensure the output is available for the UI's conditionalPanel.
    outputOptions(output, "show_remove_land_btn", suspendWhenHidden = FALSE)
    
    # Render the message that confirms removal of land observations.
    output$land_obs_message_out <- renderText({
      rv_land_obs_message()
    })
    
    # Observer for the "Remove observations on land" button click.
    observeEvent(input$remove_land_obs_btn,{
      req(rv_spat_check$spat_checks$land_plot)
      
      # Hide any previous messages
      shinyjs::hide("remove_land_obs_message")
      
      # Show spinner while processing removal.
      shinyjs::show("remove_land_obs_spinner_container")
      
      # Find the indices of rows where the ON_LAND flag is TRUE.
      land_rm_ind <- which(rv_spat_check$spat_checks$dataset$ON_LAND)
      
      if(length(land_rm_ind) > 0){
        # Get the unique ID variable name and extract the IDs to be removed.
        id_var <- rv_selected_vars$vars$main$main_unique_obs_id
        tmp_ids <- dplyr::pull(rv_spat_check$spat_checks$dataset, id_var) 
        rv_remove_ids$ids <- c(rv_remove_ids$ids, tmp_ids[land_rm_ind]) 
        
        # Remove the flagged rows from the dataset displayed in the module.
        rv_spat_check$spat_checks$dataset <- rv_spat_check$spat_checks$dataset[-land_rm_ind, ]
        
        # Recalculate the spatial summary table after removal.
        date_var <- rv_selected_vars$vars$main$main_date
        date_ind <- which(names(rv_spat_check$spat_checks$dataset) == date_var)
        date_col_name <- names(rv_spat_check$spat_checks$dataset)[date_ind]
        
        flag_nms <- c("ON_LAND", "OUTSIDE_ZONE", "ON_ZONE_BOUNDARY", "EXPECTED_LOC")
        flag_cols <- flag_nms[flag_nms %in% names(rv_spat_check$spat_checks$dataset)]
        
        # Group by year and summarize the flags.
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
        
        # Recalculate percentages.
        total_n <- sum(rv_spat_check$spat_checks$spatial_summary$n)
        rv_spat_check$spat_checks$spatial_summary <-
          rv_spat_check$spat_checks$spatial_summary %>%
          dplyr::mutate(perc = round((n / total_n) * 100, 2))
        
        # Set and show the confirmation message.
        rv_land_obs_message(
          paste0("Removed ", length(land_rm_ind), " observation(s) from the main data table")
        )
        shinyjs::show("remove_land_obs_message")
      }
      
      # Hide the spinner.
      shinyjs::hide("remove_land_obs_spinner_container")
    })
    
    ### Remove out-of-bounds observations ---------------------------------------------------------
    # Reactive to determine if the "Remove out-of-bounds observations" button should be shown.
    output$show_remove_out_bounds_btn <- reactive({
      "OUTSIDE_ZONE" %in% names(rv_spat_check$spat_checks$dataset)
    })
    
    # Ensure the output is available for the UI's conditionalPanel.
    outputOptions(output, "show_remove_out_bounds_btn", suspendWhenHidden = FALSE)
    
    # Render the message that confirms removal of out-of-bounds observations.
    output$out_obs_message_out <- renderText({
      rv_out_obs_message()
    })
    
    # Observer for the "Remove out-of-bounds observations" button click.
    observeEvent(input$remove_out_bounds_obs_btn,{
      req(rv_spat_check$spat_checks$outside_plot)
      
      # Hide any previous messages
      shinyjs::hide("remove_out_obs_message")
      
      # Show spinner while processing removal.
      shinyjs::show("remove_out_obs_spinner_container")
      
      # Find the indices of rows where the OUTSIDE_ZONE flag is TRUE.
      out_rm_ind <- which(rv_spat_check$spat_checks$dataset$OUTSIDE_ZONE)
      
      if(length(out_rm_ind) > 0){
        # Get the unique ID variable name and extract the IDs to be removed.
        id_var <- rv_selected_vars$vars$main$main_unique_obs_id
        tmp_ids <- dplyr::pull(rv_spat_check$spat_checks$dataset[, id_var], id_var) 
        rv_remove_ids$ids <- c(rv_remove_ids$ids, tmp_ids[out_rm_ind]) 
        
        # Remove the flagged rows from the dataset displayed in the module.
        rv_spat_check$spat_checks$dataset <- rv_spat_check$spat_checks$dataset[-out_rm_ind, ]
        
        # Recalculate the spatial summary table after removal.
        date_var <- rv_selected_vars$vars$main$main_date
        date_ind <- which(names(rv_spat_check$spat_checks$dataset) == date_var)
        date_col_name <- names(rv_spat_check$spat_checks$dataset)[date_ind]
        
        flag_nms <- c("ON_LAND", "OUTSIDE_ZONE", "ON_ZONE_BOUNDARY", "EXPECTED_LOC")
        flag_cols <- flag_nms[flag_nms %in% names(rv_spat_check$spat_checks$dataset)]
        
        # Group by year and summarize the flags.
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
        
        # Recalculate percentages.
        total_n <- sum(rv_spat_check$spat_checks$spatial_summary$n)
        rv_spat_check$spat_checks$spatial_summary <-
          rv_spat_check$spat_checks$spatial_summary %>%
          dplyr::mutate(perc = round((n / total_n) * 100, 2))
        
        # Set and show the confirmation message.
        rv_out_obs_message(
          paste0("Removed ", length(out_rm_ind), " observation(s) from the main data table")
        )
        shinyjs::show("remove_out_obs_message")
      }
      
      # Hide the spinner.
      shinyjs::hide("remove_out_obs_spinner_container")
    })
    
    # Return the reactive containing the IDs to be removed to the main app. 
    return(
      reactive({
        list(
          ids = rv_remove_ids$ids,
          id_col = rv_selected_vars$vars$main$main_unique_obs_id,
          status = rv_status()
        )
      })
    )
  })
}

# UI ----------------------------------------------------------------------------------------------
#' spatial_checks_ui
#'
#' @description Defines the user interface for the spatial checks module, including input controls,
#' correction buttons, and output displays for tables and plots.
#'
#' @param id A character string that is the namespace for this module.
#'
#' @return A tagList containing the UI elements for the module.
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
            # Optional text input for user to specify an EPSG code.
            div(style = "flex-grow: 1;", # Allow text input to take up available space
                textInput(
                  ns("epsg_code_input"), 
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
                )
            ), 
            
            # Button to trigger the spatial checks.
            actionButton(ns("run_spat_checks_btn"),
                         "Run spatial checks",
                         style = "display: inline-block; width: 315px;"
                         
            ),
            
            # Container for the loading spinner, shown while checks are running.
            div(id = ns("spat_checks_spinner_container"),
                style = "display: none;",
                spinner_ui(ns("spat_checks_spinner"),
                           spinner_type = "circle",
                           size = "large",
                           message = "Running spatial checks...",
                           overlay = TRUE)
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
            # Show initial message before spatial checks button clicked
            conditionalPanel(
              condition = "!output.show_output_cards & !output.rv_status_controls",
              ns = ns,
              tags$p(
                "Corrective actions will be available here after running spatial checks.",
                style = "color: grey; font-style: italic; font-size: 19px;"
              )
            ),
            
            # Show message when there are no spatial corrections to be made
            conditionalPanel(
              condition = "(!output.show_remove_land_btn & 
                           !output.show_remove_out_bounds_btn &
                           output.show_output_cards) | 
                           output.rv_status_controls",
              ns = ns,
              tags$p(
                "All spatial checks passed successfully. No corrective actions are needed.",
                style = "color: green; font-style: italic; font-size: 19px;"
              )
            ),
            
            # Show message to view data quality check outputs if corrections needed
            conditionalPanel(
              condition = "output.show_remove_land_btn | output.show_remove_out_bounds_btn",
              ns = ns,
              tags$p(
                "View data quality checks below before taking corrective action:",
                style = "color: black; font-size: 16.5px;"
              )
            ),
            
            # Conditional panel with the button to remove observations on land.
            conditionalPanel(
              condition = "output.show_remove_land_btn",
              ns = ns,
              actionButton(ns("remove_land_obs_btn"), 
                           "Remove observations on land",
                           style = "display: inline-block; width: 315px;")    
            ),
            
            # Success message after removing land observations.
            div(id = ns("remove_land_obs_message"),
                style = "color: green; display: none; font-size: 20px;",
                textOutput(ns("land_obs_message_out"))
            ),
            
            # Spinner for the "remove on land" action.
            div(id = ns("remove_land_obs_spinner_container"),
                style = "display: none;",
                spinner_ui(ns("remove_land_obs_spinner"),
                           spinner_type = "circle",
                           size = "large",
                           message = "Removing observations on land...",
                           overlay = TRUE)
            ),
            
            # Conditional panel with the button to remove out-of-bounds observations.
            conditionalPanel(
              condition = "output.show_remove_out_bounds_btn",
              ns = ns,
              actionButton(ns("remove_out_bounds_obs_btn"),
                           "Remove out-of-bounds observations",
                           style = "display: inline-block; width: 315px;")
            ),
            
            # Success message after removing out-of-bounds observations.
            div(id = ns("remove_out_obs_message"),
                style = "color: green; display: none; font-size: 20px;",
                textOutput(ns("out_obs_message_out"))
            ),
            
            # Spinner for the "remove out-of-bounds" action.
            div(id = ns("remove_out_obs_spinner_container"),
                style = "display: none;",
                spinner_ui(ns("remove_out_obs_spinner"),
                           spinner_type = "circle",
                           size = "large",
                           message = "Removing out-of-bounds observations...",
                           overlay = TRUE)
            )
          )
        )
      )
    ),
    
    # This entire card is conditional on the spatial checks having been run.
    conditionalPanel(
      # Check that run spatial checks has been clicked
      condition = "output.show_output_cards",
      ns = ns,
      bslib::card(
        height = "60%",
        bslib::card_header("Spatial data quality checks"),
        bslib::card_body(
          # Radio buttons to select which output to view.
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
            plotOutput(ns("on_land_plot"), width = "750px", height = "550px")
          ),
          
          conditionalPanel(
            condition = "input.spat_check_output_view == 'out_bounds'",
            ns = ns,
            plotOutput(ns("out_bounds_plot"), width = "750px", height = "550px")
          )
        )
      )  
    )
  )
}
