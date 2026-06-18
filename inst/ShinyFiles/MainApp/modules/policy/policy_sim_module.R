# =================================================================================================
# File: policy_simulation_module.R
# Description: This module defines the UI and server logic for running Policy and Data 
#              Simulations. It allows users to simulate the impact of policy changes 
#              on redistributed fishing effort and economic welfare.
#              
# Dependencies: shiny, DT, shinyjs, bslib, yaml
# Notes: This module interacts with ModelFits (input), Policy YAMLs (input), and 
#        PolicySimulations (output).
# =================================================================================================

# policy simulation server ------------------------------------------------------------------------
#' policy_sim_server
#'
#' @param id A character string that is unique to this module instance.
#' @param rv_folderpath A reactive value containing the current root folder path.
#' @param rv_project_name A reactive value containing the current project name.
#' @param rv_data A reactiveValues object containing the loaded data frames.
#'
#' @return This module does not return a value.
policy_sim_server <- function(id, rv_folderpath, rv_project_name, rv_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values for dynamic UI population
    rv_existing_sims <- reactiveVal(character(0))
    rv_available_fits <- reactiveVal(character(0))
    rv_available_closures <- reactiveVal(character(0))
    
    # 1. Load Available Model Fits ----------------------------------------------------------------
    load_model_fits <- function() {
      req(rv_project_name())
      project <- rv_project_name()$value
      
      tryCatch({
        fit_list <- unserialize_table(paste0(project, "ModelFit"), project)
        if (length(fit_list) > 0) {
          # Strip "_fit" from the names to match mod_name convention
          fit_names <- gsub("_fit$", "", names(fit_list))
          rv_available_fits(fit_names)
        } else {
          rv_available_fits(character(0))
        }
      }, error = function(e) {
        rv_available_fits(character(0))
      })
    }
    
    # 2. Load Available Closures ------------------------------------------------------------------
    load_closures <- function() {
      req(rv_project_name())
      project <- rv_project_name()$value
      
      tryCatch({
        # Mirror the logic used in run_simulation to find the YAML
        yaml_file <- paste0(locoutput(project), pull_output(project, 
                                                            type = "zone", 
                                                            fun = "closures"))
        if (utils::file_test("-f", yaml_file)) {
          all_closures <- yaml::read_yaml(yaml_file)
          scen_names <- vapply(all_closures, function(x) x$scenario, character(1))
          rv_available_closures(scen_names)
        } else {
          rv_available_closures(character(0))
        }
      }, error = function(e) {
        rv_available_closures(character(0))
      })
    }
    
    # 3. Load Existing Simulations (Manage Table) -------------------------------------------------
    load_simulations <- function() {
      req(rv_project_name())
      project <- rv_project_name()$value
      
      tryCatch({
        sim_list <- unserialize_table(paste0(project, "PolicySimulations"), project)
        if (length(sim_list) > 0) {
          rv_existing_sims(names(sim_list))
        } else {
          rv_existing_sims(character(0))
        }
      }, error = function(e) {
        rv_existing_sims(character(0))
      })
    }
    
    # Initialize data on project load
    observeEvent(rv_data$main, {
      load_model_fits()
      load_closures()
      load_simulations()
    })
    
    # Update UI Dropdowns dynamically
    observe({
      updateSelectizeInput(session, "mod_name_input", choices = rv_available_fits(), selected = "")
    })
    
    observe({
      updateSelectizeInput(session, "closures_input", choices = rv_available_closures(), selected = character(0))
    })
    
    observe({
      updateSelectizeInput(session, "sim_to_remove", choices = rv_existing_sims(), selected = "")
    })
    
    # 4. Execution Logic (Run Simulation) ---------------------------------------------------------
    observeEvent(input$run_sim_btn, {
      req(rv_project_name(), rv_folderpath())
      project_name <- rv_project_name()$value
      
      # Validation
      if (input$mod_name_input == "") {
        showNotification("Please select a fitted model.", type = "warning")
        return()
      }
      
      # UI State Update
      shinyjs::hide("sim_success_message")
      shinyjs::hide("sim_error_message")
      shinyjs::show("run_sim_spinner_container")
      shinyjs::disable("run_sim_btn")
      
      tryCatch({
        # Prepare arguments list
        args <- list(
          project = project_name,
          mod_name = input$mod_name_input,
          betadraws = input$betadraws_input,
          income_cost = input$income_cost_input
        )
        
        # Add optional arguments
        if (!is.null(input$closures_input) && length(input$closures_input) > 0) {
          args$closures <- input$closures_input
        }
        
        if (input$marg_util_income_input != "") {
          args$marg_util_income <- input$marg_util_income_input
        }
        
        # Execute the simulation
        do.call(run_simulation, args)
        
        # Success Feedback
        output$sim_success_out <- renderText({
          "Success: Policy simulations completed and saved to the database."
        })
        shinyjs::show("sim_success_message")
        
        # Refresh the management table
        load_simulations()
        
      }, error = function(e) {
        output$sim_error_out <- renderText({
          paste("Simulation Error:", e$message)
        })
        shinyjs::show("sim_error_message")
        
      }, finally = {
        shinyjs::hide("run_sim_spinner_container")
        shinyjs::enable("run_sim_btn")
      })
    })
    
    # 5. Manage Table Logic -----------------------------------------------------------------------
    output$existing_sims_table <- DT::renderDataTable({
      s_names <- rv_existing_sims()
      
      if (length(s_names) == 0) {
        return(DT::datatable(
          data.frame(Name = character(0), Actions = character(0)),
          caption = "No Policy Simulations found."
        ))
      }
      
      # Create view buttons with embedded JS onclick events
      actions <- sapply(s_names, function(name) {
        as.character(
          tags$button(
            class = "btn btn-secondary btn-sm",
            onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})", 
                              ns("view_sim_trigger"), name),
            "View Meta"
          )
        )
      })
      
      df <- data.frame(
        Simulation_Name = s_names,
        Actions = actions,
        stringsAsFactors = FALSE
      )
      
      DT::datatable(df,
                    options = list(pageLength = 5, searching = TRUE, dom = 'tp',
                                   columnDefs = list(list(orderable = FALSE, targets = 1))),
                    rownames = FALSE,
                    escape = FALSE,
                    selection = 'none')
    })
    
    # View Details Modal
    observeEvent(input$view_sim_trigger, {
      selected_name <- input$view_sim_trigger
      project <- rv_project_name()$value
      
      sim_list <- tryCatch({
        unserialize_table(paste0(project, "PolicySimulations"), project)
      }, error = function(e) list())
      
      sim_obj <- sim_list[[selected_name]]
      
      if (is.null(sim_obj)) {
        showNotification("Could not load simulation details.", type = "error")
        return()
      }
      
      meta <- sim_obj$metadata
      
      details_ui <- tagList(
        tags$div(
          class = "table-responsive",
          tags$table(class = "table table-sm table-striped",
                     tags$thead(tags$tr(tags$th("Metadata"), tags$th("Value"))),
                     tags$tbody(
                       tags$tr(tags$td("Base Model"), tags$td(sim_obj$model_name)),
                       tags$tr(tags$td("Scenario Name"), tags$td(sim_obj$scenario)),
                       tags$tr(tags$td("Distribution"), tags$td(meta$distribution)),
                       tags$tr(tags$td("Beta Draws"), tags$td(meta$betadraws)),
                       tags$tr(tags$td("Observations (N)"), tags$td(meta$N_obs)),
                       tags$tr(tags$td("Alternatives (J)"), tags$td(meta$J_alts)),
                       tags$tr(tags$td("Timestamp"), tags$td(as.character(meta$timestamp)))
                     )
          )
        )
      )
      
      showModal(modalDialog(
        title = paste("Simulation Meta:", selected_name),
        details_ui,
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    })
    
    # Remove Simulation Logic
    observeEvent(input$remove_sim_btn, {
      req(input$sim_to_remove)
      showModal(modalDialog(
        title = "Confirm Removal",
        paste("Are you sure you want to permanently remove the simulation:", 
              input$sim_to_remove, "?"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_remove_btn"), "Remove", class = "btn-danger")
        )
      ))
    })
    
    observeEvent(input$confirm_remove_btn, {
      removeModal()
      target_name <- input$sim_to_remove
      project <- rv_project_name()$value
      table_name <- paste0(project, "PolicySimulations")
      
      tryCatch({
        # Load the master list, remove the specific item, and overwrite
        sim_list <- unserialize_table(table_name, project)
        if (target_name %in% names(sim_list)) {
          sim_list[[target_name]] <- NULL
          
          # Connect and overwrite the BLOB
          fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project = project))
          on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
          
          if (table_exists(table_name, project)) table_remove(table_name, project)
          
          DBI::dbExecute(fishset_db, paste("CREATE TABLE IF NOT EXISTS", table_name, "(data BLOB)"))
          DBI::dbExecute(fishset_db, paste("INSERT INTO", table_name, "(data) VALUES (:data)"),
                         params = list(data = list(serialize(sim_list, NULL))))
          
          showNotification("Simulation removed successfully.", type = "message")
          load_simulations()
        }
      }, error = function(e) {
        showNotification(paste("Error removing simulation:", e$message), type = "error")
      })
    })
    
  })
}

# policy simulation UI ----------------------------------------------------------------------------
#' policy_sim_ui
#'
#' @param id A character string that is unique to this module instance.
#' @return A tagList containing the UI elements.
policy_sim_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    shinyjs::useShinyjs(),
    
    div(id = ns("main_container"),
        
        # CARD 1: Run Simulation
        bslib::card(
          class = "card-overflow",
          bslib::card_header('Run Policy & Data Simulation'),
          bslib::card_body(
            class = "card-overflow",
            p("Simulate the impact of policy changes (e.g., area closures) on redistributed fishing effort and economic welfare."),
            
            bslib::layout_columns(
              col_widths = c(6, 6),
              gap = "2rem",
              
              # LEFT COLUMN: Core Setup
              div(class = "d-flex flex-column gap-3",
                  
                  selectizeInput(ns("mod_name_input"), 
                                 label = tags$span(
                                   "Fitted Model ", 
                                   bslib::tooltip(shiny::icon("info-circle"), 
                                                  "Select the fitted model to use as the simulation baseline.")),
                                 choices = NULL, width = "100%"),
                  
                  selectizeInput(ns("closures_input"), 
                                 label = tags$span(
                                   "Closure Scenarios (Optional) ", 
                                   bslib::tooltip(shiny::icon("info-circle"), 
                                                  "Select one or more spatial closure scenarios generated via zone_closure(). Leave blank for a baseline run.")),
                                 choices = NULL, multiple = TRUE, width = "100%")
              ),
              
              # RIGHT COLUMN: Econ Parameters
              div(class = "d-flex flex-column gap-3",
                  
                  numericInput(ns("betadraws_input"), 
                               label = tags$span(
                                 "Multivariate Normal Draws ", 
                                 bslib::tooltip(shiny::icon("info-circle"), 
                                                "Number of draws for the simulation. Higher numbers increase accuracy but slow computation.")),
                               value = 500, min = 10, step = 100, width = "100%"),
                  
                  textInput(ns("marg_util_income_input"), 
                            label = tags$span(
                              "Marginal Utility of Income (Standard Logit Only) ", 
                              bslib::tooltip(shiny::icon("info-circle"), 
                                             "Name of the coefficient representing the marginal utility of income. Ignore if using an Expected Profit Model (EPM).")),
                            placeholder = "e.g., expected_catch", width = "100%"),
                  
                  checkboxInput(ns("income_cost_input"), 
                                label = tags$span(
                                  "Treat Income Parameter as a Cost? ",
                                  bslib::tooltip(shiny::icon("info-circle"), 
                                                 "Check this if the specified marginal utility of income represents a cost (flips the sign).")),
                                value = FALSE)
              )
            )
          ),
          
          # Run Button
          fluidRow(
            column(6, style = "margin-top: 25px; padding-left: 30px;",
                   actionButton(ns("run_sim_btn"), "Run Simulation", 
                                icon = icon("play"), 
                                class = "btn-secondary",
                                width = "100%")
            )
          ),
          
          # Spinner & Messages
          div(id = ns("run_sim_spinner_container"),
              style = "display: none; margin-top: 15px; padding-left: 15px;",
              spinner_ui(ns("run_sim_spinner"), spinner_type = "circle", 
                         message = "Calculating Simulations & Expected Welfare...", overlay = TRUE)
          ),
          div(id = ns("sim_success_message"), 
              style = "color: green; display: none; margin-top: 10px; padding-left: 15px;",
              textOutput(ns("sim_success_out"))),
          div(id = ns("sim_error_message"), 
              style = "color: red; display: none; margin-top: 10px; padding-left: 15px;", 
              textOutput(ns("sim_error_out")))
        ),
        
        # CARD 2: Manage Simulations
        bslib::card(
          class = "card-overflow mt-4",
          bslib::card_header("Manage Simulation Results"),
          bslib::card_body(
            class = "card-overflow",
            DT::dataTableOutput(ns("existing_sims_table"), fill = FALSE),
            hr(),
            fluidRow(
              column(8,
                     selectizeInput(ns("sim_to_remove"),
                                    "Select simulation to remove:", choices = NULL, width = "100%")
              ),
              column(4, style = "margin-top: 25px;",
                     actionButton(ns("remove_sim_btn"), "Remove Selected",
                                  icon = icon("trash"), class = "btn-danger w-100")
              )
            )
          )
        )
    )
  )
}