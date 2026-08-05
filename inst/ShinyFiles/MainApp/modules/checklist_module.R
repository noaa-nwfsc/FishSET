# =================================================================================================
# File: checklist_module.R
# Description: A reusable Shiny module to display a project progress checklist. 
#              This module provides a UI button and corresponding server logic to show a modal
#              dialog that visually represents the user's progress through various stages of
#              a FishSET project (e.g., data loading, QAQC, modeling).
#
# Authors: Paul Carvalho, Anna Abelman
# Date created: 7/29/2025
# Dependencies: shiny, font-awesome
# Notes: The module uses a reactive list (`rv_project_checklist`) to track the completion
#        status of different tasks. Checks for stages other than "Load Data" are currently
#        placeholders and need to be implemented.
# =================================================================================================

# Checklist helper functions ----------------------------------------------------------------------

#' Generate Status Text
#'
#' @description Creates a styled "Passed" or "Incomplete" span tag based on a boolean condition.
#'
#' @param passed logical. A boolean indicating if the check has passed.
#'
#' @returns A `shiny.tag` object (a span) with the appropriate text and color.
#'
status_text <- function(passed) {
  if (passed) {
    tags$span("Passed", style = "color: green; font-weight: bold;")
  } else {
    tags$span("Incomplete", style = "color: #E74C3C;")
  }
}

#' Create a Styled Font Awesome Icon
#'
#' @description A wrapper for `shiny::icon` to simplify creating icons with consistent styling.
#'
#' @param icon_name character. The name of the Font Awesome icon (e.g., "file-arrow-up").
#' @param icon_color character. The color name or hex code for the icon (e.g., "green", "#F5CF27").
#'
#' @returns A `shiny.tag` object representing the HTML for the icon.
#'
icon_wrapper <- function(icon_name, icon_color) {
  icon(name = icon_name, 
       lib = "font-awesome", 
       style = paste0("color: ", icon_color, "; font-size: 30px;"))
}

#' Determine Progress Icon and Message
#'
#' @description Evaluates the status of checks for a specific project tab (e.g., "load_data")
#'              and returns an appropriate icon and an optional message.
#'
#' @param tab character. The name of the checklist section to evaluate (e.g., "load_data", "qaqc").
#' @param checklist list. A named list where each element corresponds to a tab and contains
#'                  the pass/fail status of its checkpoints.
#'
#' @returns A list containing two elements:
#'          1. A `shiny.tag` icon object.
#'          2. A character string message (or NULL if no message is needed).
#'
pass_icon <- function(tab, checklist, previous_check = NULL) {
  # Get progress checks for tab
  list_index <- which(names(checklist) == tab)
  list_name <- names(checklist)[list_index]
  list_checks <- checklist[[list_index]]
  
  # Initialize empty output message
  out_icon_message <- NULL
  
  # Load data
  if (tab == "load_data") {
    if (all(unlist(list_checks))) {
      # All checks passed: green icon
      out_icon <- icon_wrapper(icon_name = "file-arrow-up", icon_color = "green")
      
    } else if (any(unlist(list_checks))) {
      # Some checks passed: yellow icone and a message
      out_icon <- icon_wrapper(icon_name = "file-arrow-up", icon_color = "#F5CF27")
      
      # Identify failed checks and generate appropriate message
      failed_checks <- names(which(list_checks == FALSE))
      
      if (any(failed_checks %in% c("main_data", "spat_data"))){
        out_icon_message <- "NEXT STEP: Load the required data"
        
      } else if (any(failed_checks %in% ("sel_vars"))) {
        out_icon_message <- "NEXT STEP: Select and save variables"
      }
      
    } else {
      # No checks passed: black icon
      out_icon <- icon_wrapper(icon_name = "file-arrow-up", icon_color = "black")
      
      out_icon_message <- "NEXT STEP: Add/select project and load data in the Upload Data tab"
    }
  }
  
  # QAQC
  if (tab == 'qaqc') {
    if (all(unlist(list_checks))) {
      # All qaqc checks passed: green icon
      out_icon <- icon_wrapper(icon_name = "magnifying-glass-chart", icon_color = "green")
      
    } else if (any(unlist(list_checks))) {
      out_icon <- icon_wrapper(icon_name = "magnifying-glass-chart", icon_color = "#F5CF27")
      
      # Identity failed checks
      failed_checks <- names(which(list_checks == FALSE))
      
      if ("na_check" %in% failed_checks) {
        out_icon_message <- "NEXT STEP: Check for NAs and NaNs in the main data table"
        
      } else if ("unique_check" %in% failed_checks) {
        out_icon_message <- "NEXT STEP: Check that each observation is unique 
                             and remove duplicates"
    
      } else if ("spatial_check" %in% failed_checks) {
        out_icon_message <- "NEXT STEP: Run spatial checks and make corrections if needed"
        
      }
      
    } else {
      # No checks passed: black icon
      out_icon <- icon_wrapper(icon_name = "magnifying-glass-chart", icon_color = "black")
      
      if (previous_check) {
        out_icon_message <- "NEXT STEP: Run QAQC checks"
      }
    }
    
  }
  
  # Format data
  if (tab == "format_data"){
    if (all(unlist(list_checks))) {
      # All format data checks passed
      out_icon <- icon_wrapper(icon_name = "file-lines", icon_color = "green")
      
    } else if (any(unlist(list_checks))) {
      out_icon <- icon_wrapper(icon_name = "file-lines", icon_color = "#F5CF27")
      
      # Identify failed checks
      failed_checks <- names(which(!unlist(list_checks)))
      
      if ("alt_check" %in% failed_checks) {
        out_icon_message <- "NEXT STEP: Define alternative choice set"
        
      } else if ("format_check" %in% failed_checks) {
        out_icon_message <- "NEXT STEP: Create a long formatted data table"
      }
      
    } else {
      # No checks passed: black icon
      out_icon <- icon_wrapper(icon_name = "file-lines", icon_color = "black")
      
      if (isTRUE(previous_check)) {
        out_icon_message <- "NEXT STEP: Format data"
      }
    }
  }
  
  # Models data
  if (tab == "models"){
    if (all(unlist(list_checks))) {
      # All model checks passed
      out_icon <- icon_wrapper(icon_name = "gears", icon_color = "green")
      
    } else if (any(unlist(list_checks))) {
      out_icon <- icon_wrapper(icon_name = "gears", icon_color = "#F5CF27")
      
      # Identify failed checks
      failed_checks <- names(which(list_checks == FALSE))
      
      if ("design_check" %in% failed_checks) {
        out_icon_message <- "NEXT STEP: Design a model"
        
      } else if ("fit_check" %in% failed_checks) {
        out_icon_message <- "NEXT STEP: Fit model parameters"
      }
      
    } else {
      # No checks passed: black icon
      out_icon <- icon_wrapper(icon_name = "gears", icon_color = "black")
      
      if (previous_check) {
        out_icon_message <- "NEXT STEP: Design and fit model"
      }
    }
  }
  
  # Policy data
  if (tab == "policy"){
    if (all(unlist(list_checks))) {
      # All model checks passed
      out_icon <- icon_wrapper(icon_name = "fish-fins", icon_color = "green")
      
    } else if (any(unlist(list_checks))) {
      out_icon <- icon_wrapper(icon_name = "fish-fins", icon_color = "#F5CF27")
      
      # Identify failed checks
      failed_checks <- names(which(list_checks == FALSE))
      
      if ("closure_check" %in% failed_checks) {
        out_icon_message <- "NEXT STEP: Design a closure scenario"
        
      } else if ("simulation_check" %in% failed_checks) {
        out_icon_message <- "NEXT STEP: Run policy simulation"
      }
      
    } else {
      # No checks passed: black icon
      out_icon <- icon_wrapper(icon_name = "fish-fins", icon_color = "black")
      
      if (previous_check) {
        out_icon_message <- "NEXT STEP: Policy design and simulation"
      }
    } 
  }
  
  # Return the icon and any corresponding message 
  return(list(
    out_icon,
    out_icon_message
  ))
}

# Checklist UI ------------------------------------------------------------------------------------
#' Checklist Module UI
#'
#' @description Creates a button that, when clicked, reveals the project progress modal.
#'
#' @param id character. A unique namespace identifier for the module.
#'
#' @returns A `shiny::actionButton` UI object.
#'
checklist_ui <- function(id){
  ns <- NS(id)
  
  actionButton(ns("checklist_btn"), "Check progress", class = "btn-primary") 
}


# Checklist Server --------------------------------------------------------------------------------
#' Checklist Module Server
#'
#' @description Handles the server-side logic for checking project progress and displaying
#'              the results in a modal dialog.
#'
#' @param id character. A unique namespace identifier for the module (must match the UI).
#' @param rv_project_name reactive. A reactive expression that returns the current project name.
#' @param rv_data reactive. A reactive expression that returns a list containing the project's
#'                data (e.g., `rv_data$main`, `rv_data$spat`).
#'
#' @returns The server-side module logic. This function does not return a value but has the
#'          side effect of showing a modal dialog.
#'
checklist_server <- function(id, rv_project_name, rv_data, rv_folderpath){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Initialize reactive values
    rv_project_checklist <- reactiveValues(
      checklist = list(
        load_data = list(
          main_data = FALSE,
          spat_data = FALSE,
          sel_vars = FALSE),
        
        qaqc = list(
          na_check = FALSE,
          unique_check = FALSE,
          spatial_check = FALSE),
        
        format_data = list(
          alt_check = FALSE,
          format_check = FALSE),
        
        models = list(
          design_check = FALSE,
          fit_check = FALSE),
        
        policy = list(
          closure_check = FALSE,
          simulation_check = FALSE)
      )
    )
    
    # Display progress check popup for current project
    observeEvent(input$checklist_btn, {
      # Check input reactives are available
      req(rv_project_name)
      req(rv_data) 
      project_name <- rv_project_name()$value
      
      ## Load data checks -------------------------------------------------------------------------
      # 1. Check if main data has been loaded
      if (!is.null(rv_data$main)) {
        rv_project_checklist$checklist$load_data$main_data <- TRUE
      }
      
      # 2. Check if spat data has been loaded
      if (!is.null(rv_data$spat)) {
        rv_project_checklist$checklist$load_data$spat_data <- TRUE
      }
      
      # 3. Check if "SavedVariables.rds" file exists for the project
      if (!is_empty(project_name)) {
        tab_name <- paste0(project_name, "SavedVariables.rds")
        file_name <- file.path(loc_data(project_name), tab_name)  
        
        if (file.exists(file_name)) {
          rv_project_checklist$checklist$load_data$sel_vars <- TRUE
          
        } else {
          rv_project_checklist$checklist$load_data$sel_vars <- FALSE
        }
        
      } else {
        rv_project_checklist$checklist$load_data$sel_vars <- FALSE
      }
      
      ## QAQC checks ------------------------------------------------------------------------------
      # First check that all load_data checks have passed
      if (all(unlist(rv_project_checklist$checklist$load_data))) {
        
        # 1. Check for NAs and NaNs
        has_na <- !is_value_empty(qaqc_helper(rv_data$main, "NA", "names"))
        has_nan <- !is_value_empty(qaqc_helper(rv_data$main, "NaN", "names"))
        if (!has_na && !has_nan) {
          rv_project_checklist$checklist$qaqc$na_check <- TRUE
        } 
        
        # 2. Check for duplicate rows.
        if (!any(duplicated(rv_data$main))) {
          rv_project_checklist$checklist$qaqc$unique_check <- TRUE
        } 
        
        # 3. Check spatial check status
        if (!is.null(rv_folderpath) && !is.null(rv_folderpath())) {
          status_file_path <- file.path(rv_folderpath(),
                                        project_name,
                                        "data",
                                        "SpatialChecksStatus.rds")
          
          if (file.exists(status_file_path)) {
            saved_status <- readRDS(status_file_path)
            
            # Create a fingerprint of the current data to ensure it hasn't changed
            current_data_metrics <- list(
              nrows = nrow(rv_data$main),
              size = object.size(rv_data$main)
            )
            
            # "Passes" if the file exists, the data fingerprint matches, and status is "passed"
            if (!is.null(saved_status$data_metrics$nrows) &&
                current_data_metrics$nrows == saved_status$data_metrics$nrows &&
                saved_status$status == "passed") {
              
              rv_project_checklist$checklist$qaqc$spatial_check <- TRUE
              
            } else {
              rv_project_checklist$checklist$qaqc$spatial_check <- FALSE
            }
          } else {
            rv_project_checklist$checklist$qaqc$spatial_check <- FALSE
          }
        } else {
          rv_project_checklist$checklist$qaqc$spatial_check <- FALSE
        }
      }
      
      ## Format data checks -----------------------------------------------------------------------
      # First check that all qaqc checks have passed
      if (all(unlist(rv_project_checklist$checklist$qaqc))) {
        
        # 1. Check that an alternative choice matrix was created
        test_alt <- tryCatch({
          unserialize_table(paste0(project_name, "AltMatrix"), project_name)
        }, error = function(e) {
          NULL
        })
        
        # Check that it isn't NULL and has at least one element in the list
        if (!is.null(test_alt) && length(test_alt) > 0) {
          rv_project_checklist$checklist$format_data$alt_check <- TRUE
        } else {
          rv_project_checklist$checklist$format_data$alt_check <- FALSE
        }
        
        # 2. Check that a formatted data table was created
        if (!is.null(rv_folderpath)) {
          project_dir <- file.path(rv_folderpath(), project_name)
          formatted_dir <- file.path(project_dir, "Models", "FormattedData")
          table_name <- paste0(project_name, "LongFormatData")
          
          qs2_path <- file.path(formatted_dir, paste0(table_name, ".qs2"))
          rds_path <- file.path(formatted_dir, paste0(table_name, ".rds"))
          
          # Pass check if either file format exists
          if (file.exists(qs2_path) || file.exists(rds_path)) {
            rv_project_checklist$checklist$format_data$format_check <- TRUE
          } else {
            rv_project_checklist$checklist$format_data$format_check <- FALSE
          }
          
        } else {
          rv_project_checklist$checklist$format_data$format_check <- FALSE
        }
        
      } else {
        # If QAQC checks are no longer fully passed, reset format data checks
        rv_project_checklist$checklist$format_data$alt_check <- FALSE
        rv_project_checklist$checklist$format_data$format_check <- FALSE
      }
      
      ## Models checks ----------------------------------------------------------------------------
      # First check that all format data checks have passed
      if (all(unlist(rv_project_checklist$checklist$format_data))) {
        
        # 1. Check that a design matrix has been created
        if (!is.null(rv_folderpath)) {
          project_dir <- file.path(rv_folderpath(), project_name)
          designs_dir <- file.path(project_dir, "Models", "ModelDesigns")
          
          if (dir.exists(designs_dir)) {
            design_files <- list.files(designs_dir, pattern = "\\.(rds|qs2)$")
            if (length(design_files) > 0) {
              rv_project_checklist$checklist$models$design_check <- TRUE
            } else {
              rv_project_checklist$checklist$models$design_check <- FALSE
            }
          } else {
            rv_project_checklist$checklist$models$design_check <- FALSE
          }
        } else {
          rv_project_checklist$checklist$models$design_check <- FALSE
        }
        
        # 2. Check that model parameters have been estimated for a model
        table_name <- paste0(project_name, "ModelFit")
        fits <- tryCatch({
          unserialize_table(table_name, project_name)
        }, error = function(e) {
          list()
        })
        
        if (!is.null(fits) && length(fits) > 0) {
          rv_project_checklist$checklist$models$fit_check <- TRUE
        } else {
          rv_project_checklist$checklist$models$fit_check <- FALSE
        }
        
      } else {
        rv_project_checklist$checklist$models$design_check <- FALSE
        rv_project_checklist$checklist$models$fit_check <- FALSE
      }
      
      ## Policy checks ----------------------------------------------------------------------------
      # First check that all model checks have passed
      if (all(unlist(rv_project_checklist$checklist$models))) {
        
        # 1. Check that closure scenarios have been created
        closures <- tryCatch({
          get_closure_scenario(project_name)
        }, error = function(e) {
          list()
        })
        
        if (!is.null(closures) && length(closures) > 0) {
          rv_project_checklist$checklist$policy$closure_check <- TRUE
        } else {
          rv_project_checklist$checklist$policy$closure_check <- FALSE
        }
        
        # 2. Check that simulations have been run
        sim_table_name <- paste0(project_name, "PolicySimulations")
        sims <- tryCatch({
          unserialize_table(sim_table_name, project_name)
        }, error = function(e) {
          list()
        })
        
        if (!is.null(sims) && length(sims) > 0) {
          rv_project_checklist$checklist$policy$simulation_check <- TRUE
        } else {
          rv_project_checklist$checklist$policy$simulation_check <- FALSE
        }
        
      } else {
        rv_project_checklist$checklist$policy$closure_check <- FALSE
        rv_project_checklist$checklist$policy$simulation_check <- FALSE
      }
      
      # Generate check list icons based on checks
      load_icon <- 
        pass_icon("load_data", 
                  rv_project_checklist$checklist)
      qaqc_icon <- 
        pass_icon("qaqc", 
                  rv_project_checklist$checklist, 
                  all(unlist(rv_project_checklist$checklist$load_data)))
      format_icon <- 
        pass_icon("format_data", 
                  rv_project_checklist$checklist,
                  all(unlist(rv_project_checklist$checklist$qaqc)))
      models_icon <- 
        pass_icon("models", 
                  rv_project_checklist$checklist,
                  all(unlist(rv_project_checklist$checklist$format_data)))
      
      policy_icon <- 
        pass_icon("policy", 
                  rv_project_checklist$checklist,
                  all(unlist(rv_project_checklist$checklist$models)))
      
      # Display progress check UI
      showModal(
        modalDialog(
          title = "Project progress",
          
          tags$div(
            if (!is_empty(load_icon[[2]])) {
              p(load_icon[[2]], style = "color: black; font-size: 16px;")
              
            } else if (!is_empty(qaqc_icon[[2]])) {
              p(qaqc_icon[[2]], style = "color: black; font-size: 16px;")
              
            } else if (!is_empty(format_icon[[2]])) {
              p(format_icon[[2]], style = "color: black; font-size: 16px;")
              
            } else if (!is_empty(models_icon[[2]])) {
              p(models_icon[[2]], style = "color: black; font-size: 16px;")
              
            } else if (!is_empty(policy_icon[[2]])) {
              p(policy_icon[[2]], style = "color: black; font-size: 16px;")
              
            },
            
            fluidRow(
              # Load data check
              column(1, load_icon[[1]]), 
              column(1, icon(name = "arrow-right", 
                             lib = "font-awesome", 
                             style = "color: black; font-size: 30px;")),
              
              # QAQC check
              column(1, qaqc_icon[[1]]), 
              column(1, icon(name = "arrow-right", 
                             lib = "font-awesome", 
                             style = "color: black; font-size: 30px;")),
              
              # Format data check
              column(1, format_icon[[1]]), 
              column(1, icon(name = "arrow-right", 
                             lib = "font-awesome", 
                             style = "color: black; font-size: 30px;")),
              
              # Models check
              column(1, models_icon[[1]]), 
              column(1, icon(name = "arrow-right", 
                             lib = "font-awesome", 
                             style = "color: black; font-size: 30px;")),
              
              # Policy check
              column(1, policy_icon[[1]])
            )
          ),
          
          br(),
        
          tags$div(
            style = "font-size: 16px;",
            p(
              style = "margin-bottom: 3px;", # Reduced bottom margin
              tags$b("Upload data: "),
              status_text(all(unlist(rv_project_checklist$checklist$load_data)))
            ),
            
            p(
              style = "margin-bottom: 3px;", # Reduced bottom margin
              tags$b("QAQC: "),
              status_text(all(unlist(rv_project_checklist$checklist$qaqc))),
            ),
            
            p(
              style = "margin-bottom: 3px;", # Reduced bottom margin
              tags$b("Format data: "),
              status_text(all(unlist(rv_project_checklist$checklist$format_data))),
            ), 
            
            p(
              style = "margin-bottom: 3px;", # Reduced bottom margin
              tags$b("Models: "),
              status_text(all(unlist(rv_project_checklist$checklist$models))),
            ),
            
            p(
              style = "margin-bottom: 3px;", # Reduced bottom margin
              tags$b("Policy: "),
              status_text(all(unlist(rv_project_checklist$checklist$policy))),
            )
          ),
          
          easyClose = FALSE,
          footer = tagList(
            modalButton("Close")
          ))
      )
    })
  })
} 