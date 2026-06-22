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
    
    # NEW: Caching structures to speed up UI
    rv_fit_list <- reactiveVal(list())
    rv_model_meta_cache <- reactiveVal(list())
    
    # State for dynamic Marginal Utility of Income UI
    rv_current_vars <- reactiveVal(character(0))
    rv_is_epm <- reactiveVal(FALSE)
    
    # 1. Real-time Polling Setup ------------------------------------------------------------------
    
    # Shared check function for the SQLite database (used for Fits and Simulations)
    db_check_func <- function() {
      if (is.null(rv_project_name())) return(NULL)
      project <- rv_project_name()$value
      if (is.null(project) || project == "") return(NULL)
      
      db_path <- tryCatch(locdatabase(project), error = function(e) NULL)
      if (is.null(db_path) || !file.exists(db_path)) return(NULL)
      
      # Return the database file's modification time
      return(file.info(db_path)$mtime)
    }
    
    # Poll for Model Fits
    poll_available_fits <- reactivePoll(
      intervalMillis = 1000,
      session = session,
      checkFunc = db_check_func,
      valueFunc = function() {
        if (is.null(rv_project_name())) return(character(0))
        project <- rv_project_name()$value
        if (is.null(project) || project == "") return(character(0))
        
        tryCatch({
          fit_list <- unserialize_table(paste0(project, "ModelFit"), project)
          # Cache the full table in memory so we don't have to read it on click
          rv_fit_list(fit_list) 
          
          if (length(fit_list) > 0) {
            return(gsub("_fit$", "", names(fit_list)))
          }
          return(character(0))
        }, error = function(e) character(0))
      }
    )
    
    # Poll for Policy Simulations
    poll_existing_sims <- reactivePoll(
      intervalMillis = 1000,
      session = session,
      checkFunc = db_check_func,
      valueFunc = function() {
        if (is.null(rv_project_name())) return(character(0))
        project <- rv_project_name()$value
        if (is.null(project) || project == "") return(character(0))
        
        tryCatch({
          sim_list <- unserialize_table(paste0(project, "PolicySimulations"), project)
          if (length(sim_list) > 0) {
            return(names(sim_list))
          }
          return(character(0))
        }, error = function(e) character(0))
      }
    )
    
    # Poll for Closures (YAML file)
    poll_closures <- reactivePoll(
      intervalMillis = 1000,
      session = session,
      checkFunc = function() {
        if (is.null(rv_project_name())) return(NULL)
        project <- rv_project_name()$value
        if (is.null(project) || project == "") return(NULL)
        
        yaml_file <- tryCatch(
          paste0(locoutput(project), pull_output(project, type = "zone", fun = "closures")), 
          error = function(e) NULL
        )
        if (is.null(yaml_file) || !utils::file_test("-f", yaml_file)) return(NULL)
        
        # Return the YAML file's modification time
        return(file.info(yaml_file)$mtime)
      },
      valueFunc = function() {
        if (is.null(rv_project_name())) return(character(0))
        project <- rv_project_name()$value
        if (is.null(project) || project == "") return(character(0))
        
        yaml_file <- tryCatch(
          paste0(locoutput(project), pull_output(project, type = "zone", fun = "closures")), 
          error = function(e) NULL
        )
        if (is.null(yaml_file) || !utils::file_test("-f", yaml_file)) return(character(0))
        
        tryCatch({
          all_closures <- yaml::read_yaml(yaml_file)
          return(vapply(all_closures, function(x) x$scenario, character(1)))
        }, error = function(e) character(0))
      }
    )
    
    # 2. Update UI State Reactively ---------------------------------------------------------------
    observe({
      fits <- poll_available_fits()
      rv_available_fits(fits)
      
      # Retain current selection if it still exists
      current_sel <- isolate(input$mod_name_input)
      selected <- if (!is.null(current_sel) && current_sel %in% fits) current_sel else ""
      updateSelectizeInput(session, "mod_name_input", choices = fits, selected = selected)
    })
    
    observe({
      closures <- poll_closures()
      rv_available_closures(closures)
      
      current_sel <- isolate(input$closures_input)
      selected <- if (!is.null(current_sel) && all(current_sel %in% closures)) current_sel else character(0)
      updateSelectizeInput(session, "closures_input", choices = closures, selected = selected)
    })
    
    observe({
      sims <- poll_existing_sims()
      rv_existing_sims(sims)
      
      current_sel <- isolate(input$sim_to_remove)
      selected <- if (!is.null(current_sel) && current_sel %in% sims) current_sel else ""
      updateSelectizeInput(session, "sim_to_remove", choices = sims, selected = selected)
    })
    
    # NEW: Build a lightweight cache of variables and EPM status in the background
    observe({
      fit_list <- rv_fit_list()
      project <- rv_project_name()$value
      
      if (is.null(project) || length(fit_list) == 0) return()
      
      cache <- list()
      db_path <- tryCatch(locdatabase(project), error = function(e) NULL)
      designs_dir <- if (!is.null(db_path)) file.path(dirname(db_path), "Models", "ModelDesigns") else NULL
      
      # Pre-compute the UI needs for every available model
      for (fit_name in names(fit_list)) {
        mod <- fit_list[[fit_name]]
        
        # 1. Get variables
        vars <- if (!is.null(rownames(mod$coef_table))) rownames(mod$coef_table) else character(0)
        
        # 2. Get EPM Status by reading the file
        is_epm <- FALSE
        d_name <- mod$model_name
        if (is.null(d_name)) d_name <- mod$metadata$model_name
        if (is.null(d_name)) d_name <- gsub("_fit$", "", fit_name)
        
        if (!is.null(designs_dir)) {
          qs2_path <- file.path(designs_dir, paste0(d_name, ".qs2"))
          rds_path <- file.path(designs_dir, paste0(d_name, ".rds"))
          
          tryCatch({
            if (file.exists(qs2_path) && requireNamespace("qs2", quietly = TRUE)) {
              d_obj <- qs2::qs_read(qs2_path)
              if (isTRUE(d_obj$epm$is_epm)) is_epm <- TRUE
            } else if (file.exists(rds_path)) {
              d_obj <- readRDS(rds_path)
              if (isTRUE(d_obj$epm$is_epm)) is_epm <- TRUE
            }
          }, error = function(e) {})
        }
        
        # Fallback check
        distro <- if(!is.null(mod$distribution)) mod$distribution else mod$metadata$distribution
        if (!is_epm && !is.null(distro) && distro != "none" && distro != "") {
          is_epm <- TRUE
        }
        
        # Save to dictionary using the clean name
        clean_name <- gsub("_fit$", "", fit_name)
        cache[[clean_name]] <- list(vars = vars, is_epm = is_epm)
      }
      
      rv_model_meta_cache(cache)
    })
    
    # UPDATED: React to model selection instantly using the pre-built cache
    observeEvent(input$mod_name_input, {
      # Reset to defaults
      rv_current_vars(character(0))
      rv_is_epm(FALSE)
      
      if (is.null(input$mod_name_input) || input$mod_name_input == "") {
        return()
      }
      
      # Pull instantly from memory instead of the database
      cache <- rv_model_meta_cache()
      target <- input$mod_name_input
      
      if (!is.null(cache[[target]])) {
        rv_current_vars(cache[[target]]$vars)
        rv_is_epm(cache[[target]]$is_epm)
      }
      
    }, ignoreInit = TRUE, ignoreNULL = FALSE)

    # NEW: Render the Inputs ONLY if it is a Standard Logit
    output$marg_util_ui <- renderUI({
      if (rv_is_epm()) return(NULL) # If it's an EPM, render absolutely nothing
      
      tagList(
        selectizeInput(ns("marg_util_income_input"), 
                       label = tags$span(
                         "Marginal Utility of Income ", 
                         bslib::tooltip(shiny::icon("info-circle"), 
                                        "Select the variable representing the marginal utility of income.")),
                       choices = c("", rv_current_vars()), # Add blank option at top
                       selected = "", 
                       width = "100%"),
        
        checkboxInput(ns("income_cost_input"), 
                      label = tags$span(
                        "Treat Income Parameter as a Cost? ",
                        bslib::tooltip(shiny::icon("info-circle"), 
                                       "Check this if the specified marginal utility of income represents a cost (flips the sign).")),
                      value = FALSE)
      )
    })
    
    # 3. Execution Logic (Run Simulation) ---------------------------------------------------------
    observeEvent(input$run_sim_btn, {
      req(rv_project_name(), rv_folderpath())
      project_name <- rv_project_name()$value
      
      # Validation
      if (is.null(input$mod_name_input) || input$mod_name_input == "") {
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
          income_cost = isTRUE(input$income_cost_input)
        )
        
        # Add optional arguments
        if (!is.null(input$closures_input) && length(input$closures_input) > 0) {
          args$closures <- input$closures_input
        }
        
        # Handle dynamic input properly
        if (!is.null(input$marg_util_income_input) && input$marg_util_income_input != "") {
          args$marg_util_income <- input$marg_util_income_input
        }
        
        # Execute the simulation
        do.call(run_simulation, args)
        
        # Success Feedback
        output$sim_success_out <- renderText({
          "Success: Policy simulations completed and saved to the database."
        })
        shinyjs::show("sim_success_message")
        
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
    
    # 4. Manage Table Logic -----------------------------------------------------------------------
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
          # No manual reload call needed here; reactivePoll handles the UI update
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
                  
                  # Rendered dynamically: Hidden if model is an EPM
                  uiOutput(ns("marg_util_ui"))
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