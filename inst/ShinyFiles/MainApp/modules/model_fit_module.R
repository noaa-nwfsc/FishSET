# File: model_fit_module.R
# Description: This module defines the UI and server logic for fitting FishSET Discrete Choice Models.
#              It takes a saved model design, optimizes the negative log-likelihood via RTMB,
#              and saves the results to the project database.
#              
# Dependencies: shiny, DT, shinyjs, bslib, RSQLite, DBI
# Notes: This module interacts with Models/ModelDesigns (input) and 
#        the project SQLite Database '<Project>ModelFit' table (output).
# =================================================================================================

# model fit server ----------------------------------------------------------------------------
#' model_fit_server
#'
#' @param id A character string that is unique to this module instance.
#' @param rv_folderpath A reactive value containing the current root folder path.
#' @param rv_project_name A reactive value containing the current project name.
#' @param rv_data A reactiveValues object containing the loaded data frames.
#'
#' @return This module does not return a value.
model_fit_server <- function(id, rv_folderpath, rv_project_name, rv_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive value to store names of existing designs and fits
    rv_existing_designs <- reactiveVal(character(0))
    rv_existing_fits <- reactiveVal(character(0))
    rv_fit_list <- reactiveVal(list())
    
    # 1. Real-time Polling for Model Designs ---------------------------------------------------
    # This watches the directory and updates the dropdown instantly when a new design is saved
    available_designs <- reactivePoll(
      intervalMillis = 1000, # Check every 1 second
      session = session,
      checkFunc = function() {
        if (is.null(rv_project_name())) return(NULL)
        project <- rv_project_name()$value
        if (is.null(project) || project == "") return(NULL)
        
        db_path <- tryCatch(locdatabase(project), error = function(e) NULL)
        if (is.null(db_path)) return(NULL)
        
        designs_dir <- file.path(dirname(db_path), "Models", "ModelDesigns")
        
        # We check the max modified time of files in the directory to trigger an update
        if (dir.exists(designs_dir)) {
          files <- list.files(designs_dir, pattern = "\\.(rds|qs2)$", full.names = TRUE)
          if (length(files) > 0) {
            return(max(file.info(files)$mtime, na.rm = TRUE))
          } else {
            return("empty")
          }
        }
        return("no_dir")
      },
      valueFunc = function() {
        if (is.null(rv_project_name())) return(character(0))
        project <- rv_project_name()$value
        if (is.null(project) || project == "") return(character(0))
        
        db_path <- tryCatch(locdatabase(project), error = function(e) NULL)
        if (is.null(db_path)) return(character(0))
        
        designs_dir <- file.path(dirname(db_path), "Models", "ModelDesigns")
        
        if (dir.exists(designs_dir)) {
          files <- list.files(designs_dir, pattern = "\\.(rds|qs2)$")
          return(unique(tools::file_path_sans_ext(files)))
        }
        return(character(0))
      }
    )
    
    # Update the UI dropdown whenever the polled designs change
    observe({
      d_names <- available_designs()
      rv_existing_designs(d_names) 
      
      current_sel <- isolate(input$design_input)
      
      # Keep the current selection if it still exists, otherwise reset
      if (!is.null(current_sel) && current_sel %in% d_names) {
        updateSelectizeInput(session, "design_input", choices = d_names, selected = current_sel)
      } else {
        updateSelectizeInput(session, "design_input", choices = d_names, selected = "")
      }
    })
    
    # 2. Load Manage Table Data (Model Fits) ---------------------------------------------------
    load_fits <- function() {
      req(rv_project_name())
      project <- rv_project_name()$value
      table_name <- paste0(project, "ModelFit")
      
      fits <- tryCatch({
        unserialize_table(table_name, project) 
      }, error = function(e) {
        list()
      })
      
      fit_names <- names(fits)
      rv_fit_list(fits)
      rv_existing_fits(fit_names)
      
      # Update the removal dropdown
      updateSelectizeInput(session, "fit_to_remove", 
                           choices = fit_names, selected = "")
      
      # Update the Comparison Filter dropdown
      current_sel <- isolate(input$models_to_compare)
      
      if (is.null(current_sel) || length(current_sel) == 0) {
        updateSelectizeInput(session, "models_to_compare", choices = fit_names, selected = fit_names)
      } else {
        new_fits <- setdiff(fit_names, isolate(rv_existing_fits()))
        updateSelectizeInput(session, "models_to_compare", choices = fit_names, selected = c(current_sel, new_fits))
      }
    }
    
    # Trigger loading fits on project change
    observeEvent(rv_data$main, {
      load_fits()
    })
    
    # 3. Dynamically Show/Hide EPM Distribution Input -----------------------------------------
    observeEvent(input$design_input, {
      req(rv_project_name(), rv_folderpath(), input$design_input)
      
      project <- rv_project_name()$value
      design_name <- input$design_input
      
      project_dir <- file.path(rv_folderpath(), project)
      designs_dir <- file.path(project_dir, "Models", "ModelDesigns")
      
      qs2_path <- file.path(designs_dir, paste0(design_name, ".qs2"))
      rds_path <- file.path(designs_dir, paste0(design_name, ".rds"))
      
      d_obj <- NULL
      tryCatch({
        if (file.exists(qs2_path) && requireNamespace("qs2", quietly = TRUE)) {
          d_obj <- qs2::qs_read(qs2_path)
        } else if (file.exists(rds_path)) {
          d_obj <- readRDS(rds_path)
        }
      }, error = function(e) {
        # Silently fail if unreadable, UI will just default to hiding the input
      })
      
      # Check if it is an EPM and toggle the UI
      if (!is.null(d_obj) && isTRUE(d_obj$epm$is_epm)) {
        shinyjs::show("distribution_container")
        
        # If it was set to 'none', auto-select 'normal' so it has a valid default
        if (input$distribution_input == "none") {
          updateSelectInput(session, "distribution_input", selected = "normal")
        }
      } else {
        shinyjs::hide("distribution_container")
        updateSelectInput(session, "distribution_input", selected = "none")
      }
    })
    
    # 4. Execution Logic (Run Fit) -------------------------------------------------------------
    observeEvent(input$run_fit_btn, {
      req(rv_project_name(), rv_folderpath())
      
      project_name <- rv_project_name()$value
      
      # Validation
      if (input$design_input == "") {
        showNotification("Please select a Model Design.", type = "warning")
        return()
      }
      
      # UI State
      shinyjs::hide("fit_success_message")
      shinyjs::hide("fit_error_message")
      shinyjs::show("run_fit_spinner_container")
      shinyjs::disable("run_fit_btn")
      
      tryCatch({
        # Prepare optional arguments
        fit_val <- if (trimws(input$fit_name_input) == "") NULL else trimws(input$fit_name_input)
        dist_val <- if (input$distribution_input == "none") NULL else input$distribution_input
        
        # Run the main fitting function
        res <- fishset_fit(
          project = project_name,
          model_name = input$design_input,
          fit_name = fit_val,
          distribution = dist_val
        )
        
        # Success Feedback
        saved_name <- if (is.null(fit_val)) paste0(input$design_input, "_fit") else fit_val
        output$fit_success_out <- renderText({
          paste0("Success: Model fit '", saved_name, "' estimated and saved to database.")
        })
        shinyjs::show("fit_success_message")
        
        # Reload Table
        load_fits()
        
      }, error = function(e) {
        output$fit_error_out <- renderText({
          paste("Error:", e$message)
        })
        shinyjs::show("fit_error_message")
        
      }, finally = {
        shinyjs::hide("run_fit_spinner_container")
        shinyjs::enable("run_fit_btn")
      })
    })
    
    # 5. Consolidated DataTables Generation ----------------------------------------------------
    
    # Helper for stats formatting
    fmt <- function(n, d = 2) if(is.null(n) || is.na(n)) "NA" else format(round(n, d), nsmall = d)
    
    # Table 1: Model Statistics
    output$combined_stats_table <- DT::renderDataTable({
      fits_list <- rv_fit_list()
      selected_models <- input$models_to_compare
      
      if (length(fits_list) == 0 || is.null(selected_models) || length(selected_models) == 0) return(NULL)
      
      # Subset the list to only the models selected in the UI
      fits_list <- fits_list[names(fits_list) %in% selected_models]
      
      stats_df <- data.frame(
        Model = names(fits_list),
        `Log-Likelihood` = sapply(fits_list, function(x) fmt(x$logLik)),
        AIC = sapply(fits_list, function(x) fmt(x$AIC)),
        BIC = sapply(fits_list, function(x) fmt(x$BIC)),
        `Pseudo R-Squared` = sapply(fits_list, function(x) fmt(x$pseudo_R2, 3)),
        Accuracy = sapply(fits_list, function(x) paste0(fmt(x$accuracy * 100, 1), "%")),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      
      DT::datatable(stats_df,
                    options = list(pageLength = 10, dom = 't', scrollX = TRUE),
                    rownames = FALSE,
                    class = 'cell-border stripe hover')
    })
    
    # Table 2: Model Coefficients
    output$combined_coef_table <- DT::renderDataTable({
      fits_list <- rv_fit_list()
      selected_models <- input$models_to_compare
      
      if (length(fits_list) == 0 || is.null(selected_models) || length(selected_models) == 0) return(NULL)
      
      # Subset the list to only the models selected in the UI
      fits_list <- fits_list[names(fits_list) %in% selected_models]
      
      # Extract all unique parameter names across the *filtered* saved models
      all_params <- unique(unlist(lapply(fits_list, function(x) rownames(x$coef_table))))
      
      coef_df <- data.frame(Model = names(fits_list), stringsAsFactors = FALSE)
      
      # Build columns for each parameter
      for (p in all_params) {
        coef_df[[p]] <- sapply(names(fits_list), function(m_name) {
          x <- fits_list[[m_name]]
          if (p %in% rownames(x$coef_table)) {
            est <- x$coef_table[p, "Estimate"]
            if (is.na(est)) return("NA")
            
            # Format with SE and Significance Stars if available
            if ("Std_Error" %in% colnames(x$coef_table)) {
              se <- x$coef_table[p, "Std_Error"]
              pval <- x$coef_table[p, "Pr_z"]
              
              star <- ""
              if (!is.null(pval) && !is.na(pval)) {
                if (pval < 0.001) star <- "***"
                else if (pval < 0.01) star <- "**"
                else if (pval < 0.05) star <- "*"
                else if (pval < 0.1) star <- "."
              }
              return(sprintf("%.4f (%.4f) %s", est, se, star))
            } else {
              return(sprintf("%.4f", est))
            }
          } else {
            return("-") # Return a dash if the model doesn't use this parameter
          }
        })
      }
      
      # Render with FixedColumns, ColVis, and custom Select/Deselect All buttons
      DT::datatable(coef_df,
                    extensions = c('FixedColumns', 'Buttons'),
                    options = list(
                      pageLength = 10, 
                      dom = 'Brtip',
                      scrollX = TRUE,
                      fixedColumns = list(leftColumns = 1),
                      buttons = list(
                        list(
                          extend = 'colvis', 
                          text = 'Show/Hide Parameters',
                          columns = ':gt(0)' # Prevents hiding the first "Model" column
                        ),
                        list(
                          extend = 'colvisGroup', 
                          text = 'Show All', 
                          show = ':gt(0)'
                        ),
                        list(
                          extend = 'colvisGroup', 
                          text = 'Hide All', 
                          hide = ':gt(0)'
                        )
                      )
                    ),
                    rownames = FALSE,
                    class = 'cell-border stripe hover')
    })
    
    # Aggregated Scaling Alerts 
    output$scaling_alerts_ui <- renderUI({
      fits_list <- rv_fit_list()
      selected_models <- input$models_to_compare
      
      if (length(fits_list) == 0 || is.null(selected_models) || length(selected_models) == 0) return(NULL)
      
      # Subset the list so alerts only show if the *filtered* models triggered scaling
      fits_list <- fits_list[names(fits_list) %in% selected_models]
      
      alerts <- tagList()
      
      has_catch_div <- any(sapply(fits_list, function(x) !is.null(x$Y_catch_divisor) && x$Y_catch_divisor > 1))
      has_price_div <- any(sapply(fits_list, function(x) !is.null(x$price_divisor) && x$price_divisor > 1))
      
      if (has_catch_div || has_price_div) {
        alerts[[1]] <- tags$div(
          class = "alert alert-info py-2 mb-0", 
          style = "font-size: 0.85rem; clear: both; margin-top: 25px; position: relative; z-index: 10;",
          tags$strong("Note: "), "One or more of your models utilize magnitude scaling for computational stability. ",
          "Ensure you check the specific divisor outputs when applying coefficients to raw data."
        )
      }
      alerts
    })
    
    # 6. Remove Fit Logic ----------------------------------------------------------------------
    observeEvent(input$remove_fit_btn, {
      req(input$fit_to_remove)
      showModal(modalDialog(
        title = "Confirm Removal",
        paste("Are you sure you want to permanently remove the fit:", 
              input$fit_to_remove, "from the database? This cannot be undone."),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_remove_btn"), "Remove", class = "btn-danger")
        )
      ))
    })
    
    observeEvent(input$confirm_remove_btn, {
      removeModal()
      target_name <- input$fit_to_remove
      project <- rv_project_name()$value
      table_name <- paste0(project, "ModelFit")
      
      tryCatch({
        existing_fits <- unserialize_table(table_name, project)
        
        if (target_name %in% names(existing_fits)) {
          # Remove from list
          existing_fits[[target_name]] <- NULL
          
          # Overwrite table
          db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project = project))
          DBI::dbExecute(db, paste("DROP TABLE IF EXISTS", table_name))
          
          if (length(existing_fits) > 0) {
            DBI::dbExecute(db, paste("CREATE TABLE", table_name, "(data fit_wrapper)"))
            DBI::dbExecute(db, paste("INSERT INTO", table_name, "VALUES (:data)"),
                           params = list(data = list(serialize(existing_fits, NULL))))
          }
          DBI::dbDisconnect(db)
          
          showNotification("Model fit removed.", type = "message")
          load_fits()
        } else {
          showNotification("Fit not found in database.", type = "warning")
        }
      }, error = function(e) {
        showNotification(paste("Error removing:", e$message), type = "error")
      })
    })
    
  })
}

# model fit UI --------------------------------------------------------------------------------
#' model_fit_ui
#'
#' @param id A character string that is unique to this module instance.
#' @return A tagList containing the sidebar UI elements.
model_fit_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    shinyjs::useShinyjs(),
    
    div(id = ns("main_container"),
        
        # 1. Fit Model Card
        bslib::card(
          class = "card-overflow",
          bslib::card_header('Fit Discrete Choice Model'),
          bslib::card_body(
            class = "card-overflow",
            p("This module estimates the parameters of a discrete choice model using a 
              saved model design and the RTMB framework."),
            
            # Inputs & Naming
            bslib::card(
              class = "card-overflow",
              bslib::card_header(h5("1. Target & Naming", class = "mb-0")),
              bslib::card_body(
                class = "card-overflow",
                
                # Using layout_column_wrap to perfectly size 3 columns
                bslib::layout_column_wrap(
                  width = 1/3, # Forces 3 equal columns
                  
                  # Slot 1: Model Design
                  selectizeInput(ns("design_input"), 
                                 label = tags$span(
                                   "Model Design ", 
                                   bslib::tooltip(
                                     shiny::icon("info-circle"),
                                     "Select a design matrix created in the Model Design module.")),
                                 choices = NULL, width = "100%"),
                  
                  # Slot 2: Fit Name
                  textInput(ns("fit_name_input"), 
                            label = tags$span(
                              "Fit Name (Optional) ", 
                              bslib::tooltip(
                                shiny::icon("info-circle"),
                                "Defaults to <model_name>_fit if left blank.")), 
                            placeholder = "e.g., clogit_base_fit", width = "100%"),
                  
                  # Slot 3: EPM Distribution
                  # Wrapped in a plain div so layout_column_wrap always sees 3 items 
                  # even when the input itself is hidden by shinyjs.
                  div(
                    shinyjs::hidden(
                      div(id = ns("distribution_container"),
                          selectInput(ns("distribution_input"), 
                                      label = tags$span(
                                        "EPM Distribution ", 
                                        bslib::tooltip(
                                          shiny::icon("info-circle"),
                                          "Required for Expected Profit Models.")),
                                      choices = c("Normal" = "normal", 
                                                  "Lognormal" = "lognormal", 
                                                  "Weibull" = "weibull"),
                                      selected = "none",
                                      width = "100%")
                      )
                    )
                  )
                )
              )
            ),
            
            # Run Button
            fluidRow(
              column(4, style = "margin-top: 15px;",
                     actionButton(ns("run_fit_btn"), "Fit Model", 
                                  icon = icon("calculator"), 
                                  class = "btn-secondary",
                                  width = "100%")
              )
            ),
            # Spinner & Messages
            div(id = ns("run_fit_spinner_container"),
                style = "display: none; margin-top: 15px;",
                spinner_ui(ns("run_fit_spinner"), spinner_type = "circle", 
                           message = "Optimizing Model (This may take a moment)...", overlay = TRUE)
            ),
            div(id = ns("fit_success_message"), 
                style = "color: green; display: none; margin-top: 10px;",
                textOutput(ns("fit_success_out"))),
            div(id = ns("fit_error_message"), 
                style = "color: red; display: none; margin-top: 10px;", 
                textOutput(ns("fit_error_out")))
          )
        ),
        
        hr(class = "my-4"),
        
        # Filtering Control Row
        bslib::card(
          class = "card-overflow border-secondary bg-light mt-4 mb-4", 
          bslib::card_body(
            class = "p-3 card-overflow d-flex flex-column", 
            selectizeInput(ns("models_to_compare"),
                           label = tags$span(
                             shiny::icon("filter"), 
                             tags$strong(" Filter Models to Compare "),
                             bslib::tooltip(
                               shiny::icon("info-circle"),
                               "Select specific models to view them side-by-side. 
                               Delete tags to hide them from the tables."
                             )
                           ),
                           choices = NULL, 
                           multiple = TRUE, 
                           width = "100%",
                           options = list(
                             placeholder = 'Click here to add models to the comparison...',
                             plugins = list('remove_button')
                           )
            )
          )
        ),
        
        # 2. Model Statistics Card
        bslib::card(
          class = "card-overflow",
          bslib::card_header("Model Statistics"),
          bslib::card_body(
            class = "card-overflow",
            
            div(style = "width: 100%; overflow-x: auto;",
                DT::dataTableOutput(ns("combined_stats_table"))
            )
          )
        ),
        
        # 3. Coefficient Estimates Card
        bslib::card(
          class = "card-overflow",
          bslib::card_header("Coefficient Estimates"),
          bslib::card_body(
            class = "card-overflow",
            
            div(style = "width: 100%; position: relative;",
                DT::dataTableOutput(ns("combined_coef_table"))
            ),
            
            div(class = "text-muted mt-2", style = "font-size: 0.8rem; clear: both;",
                "Cells show: Estimate (Standard Error) Significance. Codes:  0 '***' 0.001 '**'
                0.01 '*' 0.05 '.' 0.1 ' ' 1"),
            
            uiOutput(ns("scaling_alerts_ui")),
            
            hr(class = "my-4", style = "clear: both;"),
            
            # Removal Action
            fluidRow(
              column(8,
                     selectizeInput(ns("fit_to_remove"),
                                    "Select fit to permanently remove from database:", 
                                    choices = NULL, width = "100%")
              ),
              column(4, style = "margin-top: 25px;",
                     actionButton(ns("remove_fit_btn"), "Remove Selected",
                                  icon = icon("trash"), class = "btn-danger w-100")
              )
            )
          )
        )
    )
  )
}

