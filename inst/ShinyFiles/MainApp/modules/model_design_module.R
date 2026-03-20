# =================================================================================================
# File: model_design_module.R
# Description: This module defines the UI and server logic for creating Model Design objects.
#              It constructs the design matrices (X) and choice vector (y) required for
#              discrete choice modeling.
#              
# Dependencies: shiny, DT, shinyjs, bslib, qs2
# Notes: This module interacts with Models/FormattedData (input) and 
#        Models/ModelDesigns (output).
# =================================================================================================

# model design server -------------------------------------------------------------------------
#' model_design_server
#'
#' @param id A character string that is unique to this module instance.
#' @param rv_folderpath A reactive value containing the current root folder path.
#' @param rv_project_name A reactive value containing the current project name.
#' @param rv_data A reactiveValues object containing the loaded data frames.
#'
#' @return This module does not return a value.
model_design_server <- function(id, rv_folderpath, rv_project_name,  rv_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive value to store names of existing designs
    rv_existing_design_names <- reactiveVal(character(0))
    
    # Reactive value to store selected variables from project settings
    rv_selected_vars <- reactiveValues(vars = NULL)
    
    # Reactive to store the currently loaded formatted dataframe (for column extraction)
    rv_current_formatted_data <- reactiveVal(NULL)
    
    # Helper to read formatted data from flat files --------------------------------------------
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
    
    # 1. Load Manage Table Data ----------------------------------------------------------------
    load_designs <- function() {
      req(rv_project_name(), rv_folderpath())
      
      # Extract the project name string
      project <- rv_project_name()$value
      
      project_dir <- file.path(rv_folderpath(), project) 
      # Point to nested Models/ModelDesigns ---
      designs_dir <- file.path(project_dir, "Models", "ModelDesigns")
      
      just_names <- character(0)
      
      if (dir.exists(designs_dir)) {
        files <- list.files(designs_dir, pattern = "\\.(rds|qs2)$")
        just_names <- tools::file_path_sans_ext(files)
      }
      
      # Update reactive value - this triggers the table refresh
      rv_existing_design_names(just_names)
      
      # Update removal dropdown
      updateSelectizeInput(session, "design_to_remove", 
                           choices = just_names, selected = "")
    }
    
    # Load data on init
    observeEvent(rv_project_name()$value, {
      load_designs()
    })
    
    # 2. Input Updates (Dropdowns) -------------------------------------------------------------
    # Update Formatted Data Dropdown (Ultra-sensitive file watcher)
    formatted_data_choices <- reactivePoll(
      intervalMillis = 1000, 
      session = session,
      checkFunc = function() {
        if (is.null(rv_project_name())) return("")
        project <- rv_project_name()$value
        if (is.null(project) || project == "") return("")
        
        # Use locdatabase() exactly like the format_model_data function does!
        db_path <- tryCatch(locdatabase(project), error = function(e) NULL)
        if (is.null(db_path)) return("")
        
        project_dir <- dirname(db_path)
        formatted_dir <- file.path(project_dir, "Models", "FormattedData")
        table_name <- paste0(project, "LongFormatData")
        
        qs2_file <- file.path(formatted_dir, paste0(table_name, ".qs2"))
        rds_file <- file.path(formatted_dir, paste0(table_name, ".rds"))
        
        # Use file.mtime AND file.size to guarantee we catch the exact moment it saves
        state_qs2 <- if (file.exists(qs2_file)) paste(file.mtime(qs2_file),
                                                      file.size(qs2_file)) else "none"
        state_rds <- if (file.exists(rds_file)) paste(file.mtime(rds_file), 
                                                      file.size(rds_file)) else "none"
        
        return(paste(state_qs2, state_rds, sep = "|")) 
      },
      valueFunc = function() {
        if (is.null(rv_project_name())) return(character(0))
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
      choices <- formatted_data_choices()
      current_selection <- isolate(input$formatted_data_input)
      
      # Update dropdown, keeping the current selection if it still exists
      if (!is.null(current_selection) && current_selection %in% choices) {
        updateSelectizeInput(session, "formatted_data_input", 
                             choices = choices, selected = current_selection)
      } else {
        updateSelectizeInput(session, "formatted_data_input", choices = choices, selected = "")
      }
    })
    
    # Update Column Selectors based on chosen Formatted Data
    observeEvent(input$formatted_data_input, {
      req(rv_project_name(), input$formatted_data_input)
      project <- rv_project_name()$value
      data_name <- input$formatted_data_input
      
      db_path <- tryCatch(locdatabase(project), error = function(e) NULL)
      if (is.null(db_path)) return()
      project_dir <- dirname(db_path)
      
      # Load the specific dataframe to get column names
      tryCatch({
        full_lf_list <- read_long_format_file(project_dir, project)
        if (data_name %in% names(full_lf_list)) {
          df <- full_lf_list[[data_name]]
          rv_current_formatted_data(df)
          
          cols <- colnames(df)
          
          # Populate Price Variable choices if EPM selected
          updateSelectizeInput(session, "price_var_input", choices = cols)
          
          # Also help the user by listing available variables for the formula
          output$avail_vars_list <- renderText({
            paste("Available variables:", paste(cols, collapse = ", "))
          })
        }
      }, error = function(e) {
        showNotification("Error loading dataset columns.", type = "error")
      })
    })
    
    # 3. Execution Logic (Run Design) ----------------------------------------------------------
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
      
      # Validate EPM specific inputs
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
      
      # UI State
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
        # Convert string input to formula
        form_str <- input$formula_input
        # Basic check to ensure it's a formula string
        if (!grepl("~", form_str)) {
          stop("Invalid formula format. Must contain '~'.")
        }
        
        # Prepare arguments
        args <- list(
          formula = as.formula(form_str),
          project = project_name,
          model_name = input$model_name_input,
          formatted_data_name = input$formatted_data_input,
          unique_obs_id = rv_selected_vars$vars$main$main_unique_obs_id, 
          zone_id = rv_selected_vars$vars$main$main_zone_id
        )
        
        # Add EPM specific arguments if applicable
        if (is_epm) {
          if (!grepl("~", input$catch_formula_input)) {
            stop("Invalid Catch Formula format. Must contain '~'.")
          }
          args$catch_formula <- as.formula(input$catch_formula_input)
          args$price_var <- input$price_var_input
          args$scale <- input$scale_input
        }
        
        # Run the main function using do.call
        do.call(fishset_design, args)
        
        # Success Feedback
        output$design_success_out <- renderText({
          paste0("Success: Design '", isolate(input$model_name_input), "' created and saved.")
        })
        shinyjs::show("design_success_message")
        
        # Reload Table
        load_designs()
        
      }, error = function(e) {
        output$design_error_out <- renderText({
          paste("Error:", e$message)
        })
        shinyjs::show("design_error_message")
        
      }, finally = {
        shinyjs::hide("run_design_spinner_container")
        shinyjs::enable("run_design_btn")
      })
    })
    
    # 4. Manage Table Logic --------------------------------------------------------------------
    output$existing_designs_table <- DT::renderDataTable({
      d_names <- rv_existing_design_names()
      
      if (length(d_names) == 0) {
        return(DT::datatable(
          data.frame(Name = character(0), Actions = character(0)),
          caption = "No Design Objects found."
        ))
      }
      
      # Create buttons with embedded JS onclick events
      actions <- sapply(d_names, function(name) {
        as.character(
          tags$button(
            class = "btn btn-secondary btn-sm",
            onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})", 
                              ns("view_design_trigger"), name),
            "View Details"
          )
        )
      })
      
      df <- data.frame(
        Name = d_names,
        Actions = actions,
        stringsAsFactors = FALSE
      )
      
      DT::datatable(df,
                    options = list(pageLength = 5, searching = TRUE, dom = 'tp',
                                   columnDefs = list(list(orderable = FALSE, targets = 1))),
                    rownames = FALSE,
                    escape = FALSE,
                    selection = 'none',
                    caption = "Existing Design Objects")
    })
    
    # View Details Modal
    observeEvent(input$view_design_trigger, {
      
      selected_name <- input$view_design_trigger
      project_name <- rv_project_name()$value
      
      project_dir <- file.path(rv_folderpath(), project_name)
      # Point to nested Models/ModelDesigns ---
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
      }, error = function(e) {
        # Fall through to NULL check
      })
      
      if (is.null(d_obj)) {
        showNotification("Could not load details. File missing or unreadable.", type = "error")
        return()
      }
      
      # Extract info for display
      f_text <- deparse(d_obj$formula)
      dims <- d_obj$settings
      
      # Check if EPM and extract catch formula
      epm_text <- "No"
      catch_f_text <- NULL
      if (!is.null(d_obj$epm) && isTRUE(d_obj$epm$is_epm)) {
        epm_text <- "Yes"
        if (!is.null(d_obj$epm$catch_formula)) {
          catch_f_text <- paste(deparse(d_obj$epm$catch_formula), collapse = " ")
        }
      }
      
      # UI for Modal
      details_ui <- tagList(
        tags$b("Utility Formula:"), pre(paste(f_text, collapse = " ")),
        # Conditionally render the catch formula if it's an EPM
        if (epm_text == "Yes" && !is.null(catch_f_text)) {
          tagList(tags$b("Catch Formula:"), pre(catch_f_text))
        } else NULL,
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
                       tags$tr(tags$td("Obs ID Column"), tags$td(dims$unique_obs_id)),
                       tags$tr(tags$td("Zone ID Column"), tags$td(dims$zone_id))
                     )
          )
        )
      )
      
      showModal(modalDialog(
        title = paste("Design Details:", selected_name),
        details_ui,
        easyClose = TRUE,
        footer = modalButton("Close")
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
      
      # Extract the project name string
      project <- rv_project_name()$value
      
      project_dir <- file.path(rv_folderpath(), project) 
      # Point to nested Models/ModelDesigns ---
      designs_dir <- file.path(project_dir, "Models", "ModelDesigns")
      
      # Identify potential files
      qs2_path <- file.path(designs_dir, paste0(target_name, ".qs2"))
      rds_path <- file.path(designs_dir, paste0(target_name, ".rds"))
      
      removed <- FALSE
      tryCatch({
        if (file.exists(qs2_path)) {
          file.remove(qs2_path)
          removed <- TRUE
        }
        if (file.exists(rds_path)) {
          file.remove(rds_path)
          removed <- TRUE
        }
        
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

# model design UI -----------------------------------------------------------------------------
#' model_design_ui
#'
#' @param id A character string that is unique to this module instance.
#' @return A tagList containing the sidebar UI elements.
model_design_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    shinyjs::useShinyjs(),
    
    div(id = ns("main_container"),
        
        # CARD 1: Create Design Object
        bslib::card(
          class = "card-overflow",
          bslib::card_header('Create Model Design'),
          bslib::card_body(
            class = "card-overflow",
            p("This module constructs the design matrices required for discrete choice modeling 
            using formatted FishSET data."),
            
            bslib::layout_column_wrap(
              fill = FALSE,
              width = 1/2, 
              gap = "1rem",
              
              # 1. Source and Naming
              bslib::card(
                class = "card-overflow",
                bslib::card_header(h5("1. Source Data & Naming", class = "mb-0")),
                bslib::card_body(
                  class = "card-overflow d-flex flex-column gap-3",
                  
                  textInput(ns("model_name_input"), 
                            label = tags$span("Model Design Name ", 
                                              bslib::tooltip(
                                                shiny::icon("info-circle"),
                                                "Unique name for this model design 
                                                configuration.")), 
                            placeholder = "e.g., clogit_base_design", width = "100%"),
                  
                  selectizeInput(ns("formatted_data_input"), 
                                 label = tags$span(
                                   "Input Formatted Data ", 
                                   bslib::tooltip(
                                     shiny::icon("info-circle"),
                                     "Select a dataset created in the Format Model Data module.")),
                                 choices = NULL, width = "100%"),
                  
                  selectInput(ns("model_type_input"), 
                              label = tags$span(
                                "Model Type ",
                                bslib::tooltip(
                                  shiny::icon("info-circle"),
                                  "Select the type of discrete choice model.")),
                              choices = c("Conditional Logit", 
                                          "Zonal Logit", 
                                          "Expected Profit Model (EPM)" = "epm"),
                              width = "100%")
                )
              ),
              
              # 2. Model Specification
              bslib::card(
                class = "card-overflow",
                bslib::card_header(h5("2. Model Specification", class = "mb-0")),
                bslib::card_body(
                  class = "card-overflow d-flex flex-column gap-3",
                  
                  textAreaInput(ns("formula_input"), 
                                label = tags$span(
                                  "Utility Formula ", 
                                  bslib::tooltip(
                                    shiny::icon("info-circle"),
                                    "Use '|' to separate alternative-specific vars from
                                    individual-specific vars.")),
                                placeholder = "chosen ~ catch + distance | income", 
                                rows = 3, width = "100%"),
                  
                  helpText(tags$small(
                    "Format: ", tags$code("LHS ~ Alt_Vars | Ind_Vars"), br(),
                    "Note: LHS is usually 'chosen'. Ind_Vars are interacted with zones 
                    automatically."
                  )),
                  
                  # Conditional Panel for EPM inputs
                  conditionalPanel(
                    condition = "input.model_type_input == 'epm'",
                    ns = ns,
                    div(
                      class = "p-3 bg-light border rounded mt-3",
                      h6("EPM Settings", class = "card-title text-primary mb-2"),
                      
                      textAreaInput(ns("catch_formula_input"), 
                                    label = tags$span(
                                      "Catch Formula ", 
                                      bslib::tooltip(
                                        shiny::icon("info-circle"),
                                        "Formula specifying expected catch.")),
                                    placeholder = "actual_catch ~ catch_var:ZoneID",
                                    rows = 2, width = "100%"),
                      
                      selectizeInput(ns("price_var_input"), 
                                     label = "Price Variable", 
                                     choices = NULL, width = "100%"),
                      
                      checkboxInput(ns("scale_input"), 
                                    label = tags$span(
                                      "Scale Numeric Predictors? ",
                                      bslib::tooltip(
                                        shiny::icon("info-circle"),
                                        "Center and scale numeric X variables.")),
                                    value = FALSE)
                    )
                  ),
                  
                  div(class = "text-muted", style = "font-size: 0.8rem;",
                      textOutput(ns("avail_vars_list")))
                )
              )
            ),
            
            # Run Button
            fluidRow(
              column(6, style = "margin-top: 25px;",
                     actionButton(ns("run_design_btn"), "Create Design Object", 
                                  icon = icon("layer-group"), 
                                  class = "btn-secondary",
                                  width = "100%")
              )
            ),
            
            # Spinner & Messages
            div(id = ns("run_design_spinner_container"),
                style = "display: none; margin-top: 15px;",
                spinner_ui(ns("run_design_spinner"), spinner_type = "circle", 
                           message = "Generating Design Matrices...", overlay = TRUE)
            ),
            div(id = ns("design_success_message"), 
                style = "color: green; display: none; margin-top: 10px;",
                textOutput(ns("design_success_out"))),
            div(id = ns("design_error_message"), 
                style = "color: red; display: none; margin-top: 10px;", 
                textOutput(ns("design_error_out")))
          )
        ),
        
        #  Manage Designs
        bslib::card(
          class = "card-overflow",
          bslib::card_header("Manage Design Objects"),
          bslib::card_body(
            class = "card-overflow",
            DT::dataTableOutput(ns("existing_designs_table"), fill = FALSE),
            hr(),
            fluidRow(
              column(8,
                     selectizeInput(ns("design_to_remove"),
                                    "Select design to remove:", choices = NULL, width = "100%")
              ),
              column(4, style = "margin-top: 25px;",
                     actionButton(ns("remove_design_btn"), "Remove Selected",
                                  icon = icon("trash"), class = "btn-danger w-100")
              )
            )
          )
        )
    )
  )
}