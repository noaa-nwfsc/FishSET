# =================================================================================================
# File: model_design_module.R
# Description: This module defines the UI and server logic for creating Model Design objects.
#              It constructs the design matrices (X) and choice vector (y) required for
#              discrete choice modeling.
#              Optional mixed-logit controls append random slopes and their distributions.
#              

# Notes: This module interacts with Models/FormattedData (input) and 
#        Models/ModelDesigns (output).

# =================================================================================================

# model design server -----------------------------------------------------------------------------
#' model_design_server
#' @param id A character string that is unique to this module instance.
#' @param rv_folderpath A reactive value containing the current root folder path.
#' @param rv_project_name A reactive value containing the current project name.
#' @param rv_data A reactiveValues object containing the loaded data frames.
#' @return This module does not return a value.

model_design_server <- function(id, rv_folderpath, rv_project_name, rv_data,
                                rv_current_tab = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive value to store names of existing designs
    rv_existing_design_names <- reactiveVal(character(0))
    # Reactive value to store selected variables from project settings
    rv_selected_vars <- reactiveValues(vars = NULL)
    # Reactive to store the currently loaded formatted dataframe (for column extraction)
    rv_current_formatted_data <- reactiveVal(NULL)
    
    observeEvent(rv_project_name(), {
      rv_current_formatted_data(NULL)
    }, ignoreInit = TRUE, priority = 100)
    
    # Helper to read formatted data from flat files -----------------------------------------------
    read_long_format_file <- function(project_dir, project_name) {
      formatted_dir <- file.path(project_dir, "Models", "FormattedData")
      table_name <- paste0(project_name, "LongFormatData")
      qs2_path <- file.path(formatted_dir, paste0(table_name, ".qs2"))
      rds_path <- file.path(formatted_dir, paste0(table_name, ".rds"))
      
      if (file.exists(qs2_path) && requireNamespace("qs2", quietly = TRUE)) {
        return(tryCatch(qs2::qs_read(qs2_path), error = function(e) list()))
      } else if (file.exists(rds_path)) {
        return(tryCatch(readRDS(rds_path), error = function(e) list()))
      }
      return(list())
    }
    
    # 1. Load Manage Table Data -------------------------------------------------------------------
    load_designs <- function() {
      req(rv_project_name(), rv_folderpath())
      
      project <- rv_project_name()$value
      project_dir <- file.path(rv_folderpath(), project) 
      designs_dir <- file.path(project_dir, "Models", "ModelDesigns")
      just_names <- character(0)
      
      if (dir.exists(designs_dir)) {
        files <- list.files(designs_dir, pattern = "\\.(rds|qs2)$")
        just_names <- tools::file_path_sans_ext(files)
      }
      
      rv_existing_design_names(just_names)
      updateSelectizeInput(session, "design_to_remove", 
                           choices = just_names, selected = "")
    }
    
    observe({
      req(rv_data$main, rv_project_name())
      if (!is.null(rv_current_tab) && rv_current_tab() != "model_design") return()
      load_designs()
    })
    
    # 2. Input Updates (Dropdowns) ----------------------------------------------------------------
    formatted_data_choices <- reactivePoll(
      intervalMillis = 1000, 
      session = session,
      checkFunc = function() {
        if (is.null(rv_data$main) || is.null(rv_project_name())) return("none")
        project <- rv_project_name()$value
        if (is.null(project) || project == "") return("none")
        
        project_dir <- file.path(locproject(), project)
        formatted_dir <- file.path(project_dir, "Models", "FormattedData")
        table_name <- paste0(project, "LongFormatData")
        
        qs2_file <- file.path(formatted_dir, paste0(table_name, ".qs2"))
        rds_file <- file.path(formatted_dir, paste0(table_name, ".rds"))
        
        state_qs2 <- if (file.exists(qs2_file)) paste(file.mtime(qs2_file), 
                                                      file.size(qs2_file)) else "none"
        state_rds <- if (file.exists(rds_file)) paste(file.mtime(rds_file), 
                                                      file.size(rds_file)) else "none"
        return(paste(state_qs2, state_rds, sep = "|")) 
      },
      valueFunc = function() {
        if (is.null(rv_data$main) || is.null(rv_project_name())) return(character(0))
        project <- rv_project_name()$value
        if (is.null(project) || project == "") return(character(0))
        
        db_path <- tryCatch(locdatabase(project), error = function(e) NULL)
        if (is.null(db_path)) return(character(0))
        
        project_dir <- dirname(db_path)
        data_list <- read_long_format_file(project_dir, project)
        
        if (length(data_list) > 0) {
          all_keys <- names(data_list)
          return(all_keys[!grepl("_settings$", all_keys)])
        }
        return(character(0))
      }
    )
    
    observe({
      if (!is.null(rv_current_tab) && rv_current_tab() != "model_design") return()
      choices <- formatted_data_choices()
      current_selection <- isolate(input$formatted_data_input)
      
      if (!is.null(current_selection) && current_selection %in% choices) {
        updateSelectizeInput(session, "formatted_data_input", 
                             choices = choices, selected = current_selection)
      } else {
        updateSelectizeInput(session, "formatted_data_input", choices = choices, selected = "")
      }
    })
    
    # Update Column Selectors based on chosen Formatted Data
    observeEvent(input$formatted_data_input, {
      rv_current_formatted_data(NULL)
      req(rv_project_name(), input$formatted_data_input)
      
      project <- rv_project_name()$value
      data_name <- input$formatted_data_input
      project_dir <- file.path(locproject(), project)
      
      tryCatch({
        full_lf_list <- read_long_format_file(project_dir, project)
        if (data_name %in% names(full_lf_list)) {
          df <- full_lf_list[[data_name]]
          rv_current_formatted_data(df)
          
          cols <- colnames(df)
          updateSelectizeInput(session, "price_var_input", choices = cols)
          output$avail_vars_list <- renderText({ paste(cols, collapse = ", ") })
        }
      }, error = function(e) {
        showNotification("Error loading dataset columns.", type = "error")
      })
    }, ignoreNULL = FALSE)
    
    # Populate Random Variables Dropdowns
    observeEvent(rv_current_formatted_data(), {
      df <- rv_current_formatted_data()
      req(df)
      cols <- colnames(df)
      numeric_cols <- cols[vapply(df, is.numeric, logical(1))]
      
      updateSelectizeInput(session, "group_id_input", choices = cols, selected = "")
      updateSelectizeInput(session, "random_vars_input", choices = numeric_cols,
                           selected = character(0))
    }, ignoreNULL = FALSE)
    
    # Generate UI for Distributions
    output$random_dists_ui <- renderUI({
      req(input$model_type_input == "mixed_logit")
      df <- rv_current_formatted_data()
      req(df)
      
      vars <- intersect(input$random_vars_input, colnames(df))
      if (length(vars) == 0) return(NULL)
      
      # Build a neat grid of inputs
      ui_elements <- lapply(vars, function(variable) {
        input_id <- paste0("dist_", make.names(variable))
        selected <- isolate(input[[input_id]])
        if (is.null(selected)) selected <- "normal"
        
        fluidRow(
          class = "align-items-center mb-2",
          column(5, tags$strong(variable, style = "font-size: 0.9rem;")),
          column(7, 
                 selectInput(ns(input_id), label = NULL,
                             choices = c("Normal" = "normal", "Lognormal" = "lognormal"),
                             selected = selected, width = "100%")
          )
        )
      })
      
      tagList(ui_elements)
    })
    
    # 3. Execution Logic (Run Design) -------------------------------------------------------------
    observeEvent(input$run_design_btn, {
      req(rv_project_name(), rv_folderpath())
      
      project_name <- rv_project_name()$value
      folderpath <- rv_folderpath()
      
      # Validation
      if (input$model_name_input == "") {
        showNotification("Please enter a Model Name.", type = "warning")
        return()
      }
      if (input$formatted_data_input == "") {
        showNotification("Please select a Formatted Dataset.", type = "warning")
        return()
      }
      if (input$formula_input == "") {
        showNotification("Please enter a Formula.", type = "warning")
        return()
      }
      
      is_epm <- input$model_type_input == "epm"
      if (is_epm) {
        if (input$catch_formula_input == "") {
          showNotification("Please enter a Catch Formula for the Expected Profit Model.",
                           type = "warning")
          return()
        }
        if (input$price_var_input == "") {
          showNotification("Please select a Price Variable for the Expected Profit Model.",
                           type = "warning")
          return()
        }
      }
      
      shinyjs::hide("design_success_message")
      shinyjs::hide("design_error_message")
      shinyjs::show("run_design_spinner_container")
      shinyjs::disable("run_design_btn")
      
      # Load selected variables from project settings
      selected_vars <- load_gui_variables(project_name, folderpath)
      if (is.null(selected_vars)) {
        shinyjs::hide("run_design_spinner_container")
        shinyjs::enable("run_design_btn")
        showModal(modalDialog(
          title = "Error: Missing Data",
          "The selected variables file could not be found. Please ensure variables have been
          selected in the previous steps.",
          easyClose = TRUE
        ))
        return()
      }
      rv_selected_vars$vars <- selected_vars
      
      tryCatch({
        form_str <- input$formula_input
        if (!grepl("~", form_str)) stop("Invalid formula format. Must contain '~'.")
        
        random_dists <- NULL
        
        # Handle Random Parameters logic for Mixed Logit
        if (input$model_type_input == "mixed_logit") {
          df <- rv_current_formatted_data()
          group_id <- input$group_id_input
          random_vars <- input$random_vars_input
          
          if (is.null(df)) stop("Please load a Formatted Dataset for random parameters.")
          if (is.null(group_id) || !nzchar(group_id) || !(group_id %in% colnames(df))) {
            stop("Please select a grouping variable from the Formatted Dataset.")
          }
          if (length(random_vars) == 0 || !all(random_vars %in% colnames(df))) {
            stop("Please select at least one random parameter from the Formatted Dataset.")
          }
          if (!all(vapply(random_vars, function(v) is.numeric(df[[v]]), logical(1)))) {
            stop("Random parameters must be numeric covariates.")
          }
          
          # Pull the chosen distributions
          random_dists <- stats::setNames(vapply(random_vars, function(v) {
            input[[paste0("dist_", make.names(v))]] %||% "normal"
          }, character(1)), random_vars)
          
          # Check if user already appended random effects manually
          if (grepl("\\([^)]+\\|[^)]+\\)", form_str)) {
            stop("Mixed model syntax (var | group) detected in the Utility Formula box. Please
                  remove it and let the Random Parameters UI build it, or select a different 
                  model type.")
          } else {
            form_str <- paste0(form_str, " + (0 + ", 
                               paste(random_vars, collapse = " + "), " | ", group_id, ")")
          }
        }
        
        # Prepare arguments
        args <- list(
          formula = as.formula(form_str),
          project = project_name,
          model_name = input$model_name_input,
          formatted_data_name = input$formatted_data_input,
          unique_obs_id = rv_selected_vars$vars$main$main_unique_obs_id, 
          zone_id = rv_selected_vars$vars$main$main_zone_id,
          scale = input$scale_input
        )
        if (input$model_type_input == "mixed_logit") args$random_dists <- random_dists
        
        if (is_epm) {
          if (!grepl("~", input$catch_formula_input)) stop("Invalid Catch Formula format.
                                                           Must contain '~'.")
          args$catch_formula <- as.formula(input$catch_formula_input)
          args$price_var <- input$price_var_input
        }
        
        # Run the main function
        do.call(fishset_design, args)
        
        output$design_success_out <- renderText({
          paste0("Success: Design '", isolate(input$model_name_input), "' created and saved.")
        })
        shinyjs::show("design_success_message")
        load_designs()
        
      }, error = function(e) {
        output$design_error_out <- renderText({ paste("Error:", e$message) })
        shinyjs::show("design_error_message")
      }, finally = {
        shinyjs::hide("run_design_spinner_container")
        shinyjs::enable("run_design_btn")
      })
    })
    
    # 4. Manage Table Logic -----------------------------------------------------------------------
    output$existing_designs_table <- DT::renderDataTable({
      d_names <- rv_existing_design_names()
      if (length(d_names) == 0) {
        return(DT::datatable(
          data.frame(Name = character(0), Actions = character(0)),
          caption = "No Design Objects found."
        ))
      }
      
      actions <- sapply(d_names, function(name) {
        as.character(tags$button(
          class = "btn btn-secondary btn-sm",
          onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})", 
                            ns("view_design_trigger"), name),
          "View Details"
        ))
      })
      
      df <- data.frame(Name = d_names, Actions = actions, stringsAsFactors = FALSE)
      
      DT::datatable(df,
                    options = list(pageLength = 5, searching = TRUE, dom = 'tp',
                                   columnDefs = list(list(orderable = FALSE, targets = 1))),
                    rownames = FALSE, escape = FALSE, selection = 'none',
                    caption = "Existing Design Objects")
    })
    
    # View Details Modal
    observeEvent(input$view_design_trigger, {
      selected_name <- input$view_design_trigger
      project_name <- rv_project_name()$value
      project_dir <- file.path(rv_folderpath(), project_name)
      designs_dir <- file.path(project_dir, "Models", "ModelDesigns")
      
      qs2_path <- file.path(designs_dir, paste0(selected_name, ".qs2"))
      rds_path <- file.path(designs_dir, paste0(selected_name, ".rds"))
      
      d_obj <- NULL
      tryCatch({
        if (file.exists(qs2_path) && requireNamespace("qs2", quietly = TRUE)) {
          d_obj <- qs2::qs_read(qs2_path)
        } else if (file.exists(rds_path)) {
          d_obj <- readRDS(rds_path)
        }
      }, error = function(e) {})
      
      if (is.null(d_obj)) {
        showNotification("Could not load details. File missing or unreadable.", type = "error")
        return()
      }
      
      f_text <- deparse(d_obj$formula)
      dims <- d_obj$settings
      
      # EPM properties
      epm_text <- "No"
      catch_f_text <- NULL
      if (!is.null(d_obj$epm) && isTRUE(d_obj$epm$is_epm)) {
        epm_text <- "Yes"
        if (!is.null(d_obj$epm$catch_formula)) {
          catch_f_text <- paste(deparse(d_obj$epm$catch_formula), collapse = " ")
        }
      }
      
      # Mixed Logit / Random Parameters properties
      mixed_text <- "No"
      group_id_text <- "N/A"
      random_params_ui <- NULL
      
      if (!is.null(d_obj$random_effects)) {
        mixed_text <- "Yes"
        group_id_text <- d_obj$random_effects$group_id
        
        # 1 = Normal, 2 = Lognormal in the backend code
        dist_map <- c("1" = "Normal", "2" = "Lognormal")
        dist_codes <- d_obj$random_effects$dist_codes
        
        if (length(dist_codes) > 0) {
          param_rows <- lapply(names(dist_codes), function(p) {
            dist_name <- dist_map[as.character(dist_codes[p])]
            tags$tr(tags$td(p), tags$td(dist_name))
          })
          
          random_params_ui <- tagList(
            tags$br(),
            tags$b("Random Parameters:", style = "margin-bottom: 5px; display: inline-block;"),
            tags$div(
              style = "overflow-x: auto;",
              tags$table(class = "table table-sm table-bordered",
                         tags$thead(tags$tr(tags$th("Variable"), tags$th("Distribution"))),
                         tags$tbody(param_rows)
              )
            )
          )
        }
      }
      
      details_ui <- tagList(
        tags$b("Utility Formula:"), pre(paste(f_text, collapse = " ")),
        if (epm_text == "Yes" && !is.null(catch_f_text)) tagList(tags$b("Catch Formula:"),
                                                                 pre(catch_f_text)) else NULL,
        hr(),
        tags$div(
          style = "overflow-x: auto;",
          tags$table(class = "table table-sm table-striped",
                     tags$thead(tags$tr(tags$th("Metric"), tags$th("Value"))),
                     tags$tbody(
                       tags$tr(tags$td("Formatted Data Source"), tags$td(dims$formatted_data_name)),
                       tags$tr(tags$td("Observations (N)"), tags$td(dims$N_obs)),
                       tags$tr(tags$td("Alternatives (J)"), tags$td(dims$J_alts)),
                       tags$tr(tags$td("Parameters (K)"), tags$td(dims$K_vars)),
                       tags$tr(tags$td("Is EPM?"), tags$td(epm_text)),
                       tags$tr(tags$td("Is Mixed Logit?"), tags$td(mixed_text)),
                       tags$tr(tags$td("Obs ID Column"), tags$td(dims$unique_obs_id)),
                       tags$tr(tags$td("Zone ID Column"), tags$td(dims$zone_id)),
                       if (mixed_text == "Yes") tags$tr(tags$td("Grouping ID"), 
                                                        tags$td(group_id_text)) else NULL
                     ))
        ),
        random_params_ui
      )
      
      showModal(modalDialog(
        title = paste("Design Details:", selected_name),
        details_ui, easyClose = TRUE, footer = modalButton("Close")
      ))
    })
    
    # Remove Design Logic
    observeEvent(input$remove_design_btn, {
      req(input$design_to_remove)
      showModal(modalDialog(
        title = "Confirm Removal",
        paste("Are you sure you want to permanently remove the design:", 
              input$design_to_remove, "? This cannot be undone."),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_remove_btn"), "Remove", class = "btn-danger")
        )
      ))
    })
    
    observeEvent(input$confirm_remove_btn, {
      removeModal()
      target_name <- input$design_to_remove
      project <- rv_project_name()$value
      project_dir <- file.path(rv_folderpath(), project) 
      designs_dir <- file.path(project_dir, "Models", "ModelDesigns")
      
      qs2_path <- file.path(designs_dir, paste0(target_name, ".qs2"))
      rds_path <- file.path(designs_dir, paste0(target_name, ".rds"))
      
      removed <- FALSE
      tryCatch({
        if (file.exists(qs2_path)) { file.remove(qs2_path); removed <- TRUE }
        if (file.exists(rds_path)) { file.remove(rds_path); removed <- TRUE }
        
        if (removed) {
          showNotification("Design removed.", type = "message")
          load_designs()
        } else {
          showNotification("File not found on disk.", type = "warning")
        }
      }, error = function(e) {
        showNotification(paste("Error removing:", e$message), type = "error")
      })
    })
  })
}

# model design UI ---------------------------------------------------------------------------------
#' model_design_ui
#' @param id A character string that is unique to this module instance.
#' @return A tagList containing the sidebar UI elements.

model_design_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    shinyjs::useShinyjs(),
    
    div(id = ns("main_container"),
        
        bslib::card(
          class = "card-overflow",
          bslib::card_header('Create Model Design'),
          bslib::card_body(
            class = "card-overflow",
            p("This module constructs the design matrices required for discrete choice modeling
              using formatted FishSET data."),
            
            # 1. Source and Naming
            bslib::card(
              class = "card-overflow mb-4", 
              bslib::card_header(h5("1. Source Data & Naming", class = "mb-0")),
              bslib::card_body(
                class = "card-overflow",
                bslib::layout_column_wrap(
                  width = 1/3, gap = "1rem",
                  
                  textInput(ns("model_name_input"), 
                            label = tags$span(
                              "Model Design Name ", 
                              bslib::tooltip(shiny::icon("info-circle"),
                                             "Unique name for this model design configuration.")), 
                            placeholder = "e.g., clogit_base_design", width = "100%"),
                  
                  selectizeInput(ns("formatted_data_input"), 
                                 label = tags$span(
                                   "Input Formatted Data ", 
                                   bslib::tooltip(shiny::icon("info-circle"),
                                                  "Select a dataset created in the Format
                                                  Model Data module.")),
                                 choices = NULL, width = "100%"),
                  
                  div(
                    selectInput(ns("model_type_input"), 
                                label = tags$span("Model Type ",
                                                  bslib::tooltip(shiny::icon("info-circle"),
                                                                 "Select the type of
                                                                 discrete choice model.")),
                                choices = c("Standard Logit", 
                                            "Expected Profit Model (EPM)" = "epm", 
                                            "Mixed Logit" = "mixed_logit"),
                                width = "100%"),
                    shinyjs::hidden(
                      div(id = ns("distribution_container"),
                          selectInput(ns("distribution_input"), 
                                      label = tags$span("EPM Distribution ", 
                                                        bslib::tooltip(
                                                          shiny::icon("info-circle"), 
                                                          "Required for Expected Profit Models.")),
                                      choices = c("Normal" = "normal",
                                                  "Lognormal" = "lognormal",
                                                  "Weibull" = "weibull"),
                                      selected = "none", width = "100%")
                      )
                    )
                  )
                )
              )
            ),
            
            # 2. Model Specification 
            bslib::card(
              class = "card-overflow",
              bslib::card_header(h5("2. Model Specification", class = "mb-0")),
              bslib::card_body(
                class = "card-overflow",
                
                # TOP ROW: Full-width Available Variables
                div(class = "mb-4 p-3 border rounded", style = "background-color: #f1f3f5;",
                    h6(shiny::icon("list"), 
                       " Available Variables",
                       class = "text-primary mb-2",
                       style = "font-size: 0.95rem;"),
                    div(class = "text-muted",
                        style = "font-size: 0.85rem; max-height: 80px; overflow-y: auto;", 
                        textOutput(ns("avail_vars_list")))
                ),
                
                # BOTTOM ROW: 50/50 Split for Formulas vs Options
                bslib::layout_columns(
                  col_widths = c(6, 6), gap = "2rem",
                  
                  # LEFT COLUMN: The Equations
                  div(class = "d-flex flex-column gap-3",
                      
                      div(
                        textAreaInput(
                          ns("formula_input"), 
                          label = tags$span(
                            "Utility Formula ", 
                            bslib::tooltip(shiny::icon("info-circle"),
                                           "Use '|' to separate alternative-specific 
                                                       vars from trip-specific vars.")),
                          placeholder = "chosen ~ exp_matrix_1 + distance | vessel_length", 
                          rows = 3, width = "100%"),
                        # Moved help text directly under the input it relates to
                        helpText(
                          tags$small("Format: ",
                                     tags$code("chosen ~ Alt_Vars | Ind_Vars"), 
                                     br(),
                                     "Note: LHS is always 'chosen', and independent vars are 
                                     interacted with zones automatically."))
                      )
                  ),
                  
                  # RIGHT COLUMN: Model Modifiers & Specific Settings
                  div(class = "d-flex flex-column gap-3",
                      
                      div(class = "p-3 border rounded shadow-sm bg-white",
                          h6(shiny::icon("sliders"), " Model Options", 
                             class = "text-primary mb-3"),
                          
                          checkboxInput(
                            ns("scale_input"), 
                            tags$span(
                              "Scale Numeric Covariates", 
                              bslib::tooltip(
                                shiny::icon("info-circle"),
                                "Standardizes continuous variables (z-score) prior to 
                                estimation.")), 
                            value = FALSE),
                          
                          # EPM settings
                          conditionalPanel(
                            condition = "input.model_type_input == 'epm'",
                            ns = ns,
                            div(class = "pt-3 mt-2 border-top",
                                h6(shiny::icon("fish"),
                                   " EPM Settings",
                                   class = "text-primary mb-3"),
                                textAreaInput(ns("catch_formula_input"), 
                                              label = "Catch Formula",
                                              placeholder = "actual_catch ~ exp_catch_matrix",
                                              rows = 2, width = "100%"),
                                selectizeInput(ns("price_var_input"), 
                                               label = "Price Variable",
                                               choices = NULL, width = "100%")
                            )
                          ),
                          
                          # Mixed logit
                          conditionalPanel(
                            condition = "input.model_type_input == 'mixed_logit'",
                            ns = ns,
                            div(class = "pt-3 mt-2 border-top",
                                h6(shiny::icon("shuffle"), 
                                   " Random Parameters (Mixed Logit)", 
                                   class = "text-primary mb-3"),
                                selectizeInput(ns("group_id_input"), 
                                               "Grouping Variable (e.g. Vessel ID)",
                                               choices = NULL, 
                                               width = "100%"),
                                selectizeInput(ns("random_vars_input"),
                                               "Select Random Variables", 
                                               choices = NULL,
                                               multiple = TRUE, 
                                               width = "100%"),
                                tags$p("Distributions:", 
                                       style = "font-weight: 500; font-size: 0.9rem; margin-top: 
                                       15px; margin-bottom: 10px;"),
                                uiOutput(ns("random_dists_ui"))
                            )
                          )
                      )
                  )
                )
              )
            )
          ),
          
          # Run Button & Messages
          fluidRow(
            column(6, style = "margin-top: 25px; padding-left: 30px;",
                   actionButton(ns("run_design_btn"), "Create Design Object", 
                                icon = icon("layer-group"), class = "btn-primary", width = "100%")
            )
          ),
          div(id = ns("run_design_spinner_container"), 
              style = "display: none; margin-top: 15px; padding-left: 15px;",
              spinner_ui(ns("run_design_spinner"), 
                         spinner_type = "circle",
                         message = "Generating Design Matrices...", overlay = TRUE)),
          div(id = ns("design_success_message"),
              style = "color: green; display: none; margin-top: 10px; padding-left: 15px;",
              textOutput(ns("design_success_out"))),
          div(id = ns("design_error_message"), 
              style = "color: red; display: none; margin-top: 10px; padding-left: 15px;", 
              textOutput(ns("design_error_out")))
        ),
        
        # Manage Designs
        bslib::card(
          class = "card-overflow mt-4",
          bslib::card_header("Manage Design Objects"),
          bslib::card_body(
            class = "card-overflow",
            DT::dataTableOutput(ns("existing_designs_table"), fill = FALSE),
            hr(),
            fluidRow(
              column(8, 
                     selectizeInput(ns("design_to_remove"), 
                                    "Select design to remove:", 
                                    choices = NULL, 
                                    width = "100%")),
              column(4, 
                     style = "margin-top: 25px;", 
                     actionButton(ns("remove_design_btn"),
                                  "Remove Selected", 
                                  icon = icon("trash"),
                                  class = "btn-danger w-100"))
            )
          )
        )
    )
  )
}