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
      
      updateSelectizeInput(session, "fit_to_remove", 
                           choices = fit_names, selected = "")
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
    
    # 3. Execution Logic (Run Fit) -------------------------------------------------------------
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
    
    # 4. Manage Table Logic --------------------------------------------------------------------
    output$existing_fits_table <- DT::renderDataTable({
      f_names <- rv_existing_fits()
      
      if (length(f_names) == 0) {
        return(DT::datatable(
          data.frame(Name = character(0), Actions = character(0)),
          caption = "No Model Fits found."
        ))
      }
      
      # Create buttons with embedded JS onclick events
      actions <- sapply(f_names, function(name) {
        as.character(
          tags$button(
            class = "btn btn-secondary btn-sm",
            onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})", 
                              ns("view_fit_trigger"), name),
            "View Results"
          )
        )
      })
      
      df <- data.frame(
        Name = f_names,
        Actions = actions,
        stringsAsFactors = FALSE
      )
      
      DT::datatable(df,
                    options = list(pageLength = 5, searching = TRUE, dom = 'tp',
                                   columnDefs = list(list(orderable = FALSE, targets = 1))),
                    rownames = FALSE,
                    escape = FALSE,
                    selection = 'none',
                    caption = "Existing Model Fits")
    })
    
    # View Details Modal
    observeEvent(input$view_fit_trigger, {
      selected_name <- input$view_fit_trigger
      fits_list <- rv_fit_list()
      
      if (!(selected_name %in% names(fits_list))) {
        showNotification("Could not load details. Fit not found in cache.", type = "error")
        return()
      }
      
      fit_obj <- fits_list[[selected_name]]
      
      # Helper for formatting numeric summary stats
      fmt <- function(n, d = 2) if(is.null(n) || is.na(n)) "NA" else format(round(n, d),
                                                                            nsmall = d)
      
      # 1. Build an HTML Bootstrap Table for Summary Stats
      summary_table <- tags$div(
        class = "table-responsive mb-4",
        tags$table(
          class = "table table-sm table-striped table-bordered",
          tags$thead(
            tags$tr(class = "table-light", tags$th("Statistic"), tags$th("Value"))
          ),
          tags$tbody(
            tags$tr(tags$td(tags$b("Log-Likelihood")), tags$td(fmt(fit_obj$logLik, 2))),
            tags$tr(tags$td(tags$b("AIC")), tags$td(fmt(fit_obj$AIC, 2))),
            tags$tr(tags$td(tags$b("BIC")), tags$td(fmt(fit_obj$BIC, 2))),
            tags$tr(tags$td(tags$b("Pseudo R-Squared")), tags$td(fmt(fit_obj$pseudo_R2, 3))),
            tags$tr(tags$td(tags$b("Accuracy")), 
                    tags$td(paste0(fmt(fit_obj$accuracy * 100, 1), "%")))
          )
        )
      )
      
      # 2. Render the Coefficients as a DT Data Table
      output$modal_coef_table <- DT::renderDataTable({
        req(fit_obj$coef_table)
        df <- fit_obj$coef_table
        
        # Bring rownames into the dataframe as a column
        df <- cbind(Parameter = rownames(df), df)
        rownames(df) <- NULL
        
        # Round numeric columns
        df$Estimate <- round(df$Estimate, 4)
        df$Std_Error <- round(df$Std_Error, 4)
        df$z_value <- round(df$z_value, 2)
        
        # Format P-values and append significance stars
        df$Pr_z <- sapply(df$Pr_z, function(p) {
          if (is.na(p)) return("NA")
          star <- ""
          if (p < 0.001) star <- "***"
          else if (p < 0.01) star <- "**"
          else if (p < 0.05) star <- "*"
          else if (p < 0.1) star <- "."
          paste0(format.pval(p, eps = 0.001, digits = 3), " ", star)
        })
        
        # Return cleanly styled DT
        DT::datatable(df,
                      options = list(pageLength = 10, dom = 'tip', searching = FALSE),
                      rownames = FALSE,
                      class = 'cell-border stripe hover')
      })
      
      # 3. Create Scaling Alerts 
      scaling_alerts <- tagList()
      
      if (!is.null(fit_obj$Y_catch_divisor) && fit_obj$Y_catch_divisor > 1) {
        scaling_alerts[[length(scaling_alerts) + 1]] <- tags$div(
          class = "alert alert-info py-2 mb-2",
          style = "font-size: 0.9rem;",
          tags$strong("Catch Units: "), 
          paste("Modeled in 1 /", format(fit_obj$Y_catch_divisor, scientific = FALSE), "units")
        )
      }
      
      if (!is.null(fit_obj$price_divisor) && fit_obj$price_divisor > 1) {
        scaling_alerts[[length(scaling_alerts) + 1]] <- tags$div(
          class = "alert alert-info py-2 mb-2",
          style = "font-size: 0.9rem;",
          tags$strong("Price Units: "), 
          paste("Modeled in 1 /", format(fit_obj$price_divisor, scientific = FALSE), "units")
        )
      }
      
      # 4. Combine into final UI for the Modal
      details_ui <- tagList(
        
        h5("Coefficient Estimates", class = "text-primary mt-3"),
        DT::dataTableOutput(ns("modal_coef_table")),
        
        h5("Model Statistics", class = "text-primary"),
        summary_table,
        
        # Add the scaling alerts near the bottom of the modal
        scaling_alerts, 
        
        div(class = "text-muted mt-2", style = "font-size: 0.8rem;",
            "Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1")
      )
      
      showModal(modalDialog(
        title = paste("Fit Details:", selected_name),
        details_ui,
        size = "l",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    })
    
    # Remove Fit Logic
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
        
        # Fit Model
        bslib::card(
          class = "card-overflow",
          bslib::card_header('Fit Discrete Choice Model'),
          bslib::card_body(
            class = "card-overflow",
            p("This module estimates the parameters of a discrete choice model using a 
              saved model design and the RTMB framework."),
            
            # 1. Inputs & Naming
            bslib::card(
              class = "card-overflow",
              bslib::card_header(h5("1. Target & Naming", class = "mb-0")),
              bslib::card_body(
                class = "card-overflow d-flex flex-column gap-3",
                
                selectizeInput(ns("design_input"), 
                               label = tags$span(
                                 "Model Design ", 
                                 bslib::tooltip(
                                   shiny::icon("info-circle"),
                                   "Select a design matrix created in the Model Design module.")),
                               choices = NULL, width = "100%"),
                
                textInput(ns("fit_name_input"), 
                          label = tags$span(
                            "Fit Name (Optional) ", 
                            bslib::tooltip(
                              shiny::icon("info-circle"),
                              "Defaults to <model_name>_fit if left blank.")), 
                          placeholder = "e.g., clogit_base_fit", width = "100%"),
                
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
        
        # Manage Fits
        bslib::card(
          class = "card-overflow",
          bslib::card_header("Manage Model Fits"),
          bslib::card_body(
            class = "card-overflow",
            DT::dataTableOutput(ns("existing_fits_table"), fill = FALSE),
            hr(),
            fluidRow(
              column(8,
                     selectizeInput(ns("fit_to_remove"),
                                    "Select fit to remove:", choices = NULL, width = "100%")
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