# File: model_cv_module.R
# Description: This module defines the UI and server logic for running K-Fold Cross Validation
#              on FishSET Discrete Choice Models. It takes a saved model design, partitions
#              the data, fits the models, and returns predictive performance metrics.
# 
# Dependencies: shiny, DT, shinyjs, bslib, tools
# Notes: This module interacts with Models/ModelDesigns (input). Results are held in memory.
# =================================================================================================

# model cv server ---------------------------------------------------------------------------------
#' model_cv_server
#'
#' @param id A character string that is unique to this module instance.
#' @param rv_folderpath A reactive value containing the current root folder path.
#' @param rv_project_name A reactive value containing the current project name.
#'
#' @return This module does not return a value.
model_cv_server <- function(id, rv_folderpath, rv_project_name) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive value to store names of existing designs and the cv results
    rv_existing_designs <- reactiveVal(character(0))
    rv_cv_results <- reactiveVal(NULL)
    
    # 1. Real-time Polling for Model Designs ------------------------------------------------------
    available_designs <- reactivePoll(
      intervalMillis = 1000, 
      session = session,
      checkFunc = function() {
        if (is.null(rv_project_name())) return(NULL)
        project <- rv_project_name()$value
        if (is.null(project) || project == "") return(NULL)
        
        db_path <- tryCatch(locdatabase(project), error = function(e) NULL)
        if (is.null(db_path)) return(NULL)
        
        designs_dir <- file.path(dirname(db_path), "Models", "ModelDesigns")
        
        if (dir.exists(designs_dir)) {
          files <- list.files(designs_dir, pattern = "\\.(rds|qs2)$", full.names = TRUE)
          if (length(files) > 0) return(max(file.info(files)$mtime, na.rm = TRUE))
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
    
    observe({
      d_names <- available_designs()
      rv_existing_designs(d_names) 
      
      current_sel <- isolate(input$design_input)
      if (!is.null(current_sel) && current_sel %in% d_names) {
        updateSelectizeInput(session, "design_input", choices = d_names, selected = current_sel)
      } else {
        updateSelectizeInput(session, "design_input", choices = d_names, selected = "")
      }
    })
    
    # 2. Cache metadata to check for EPMs ---------------------------------------------------------
    rv_design_metadata <- reactive({
      d_names <- available_designs() 
      project <- rv_project_name()$value
      
      meta <- list(epm_designs = character(0))
      if (is.null(project) || length(d_names) == 0) return(meta)
      
      db_path <- tryCatch(locdatabase(project), error = function(e) NULL)
      if (is.null(db_path)) return(meta)
      
      designs_dir <- file.path(dirname(db_path), "Models", "ModelDesigns")
      if (!dir.exists(designs_dir)) return(meta)
      
      for (d_name in d_names) {
        qs2_path <- file.path(designs_dir, paste0(d_name, ".qs2"))
        rds_path <- file.path(designs_dir, paste0(d_name, ".rds"))
        
        d_obj <- NULL
        tryCatch({
          if (file.exists(qs2_path) && requireNamespace("qs2", quietly = TRUE)) {
            d_obj <- qs2::qs_read(qs2_path)
          } else if (file.exists(rds_path)) {
            d_obj <- readRDS(rds_path)
          }
        }, error = function(e) {})
        
        if (!is.null(d_obj) && !is.null(d_obj$formula)) {
          if (isTRUE(d_obj$epm$is_epm)) {
            meta$epm_designs <- c(meta$epm_designs, d_name)
          }
        }
      }
      return(meta)
    })
    
    # 3. Dynamically Show/Hide EPM Distribution Input ---------------------------------------------
    observeEvent(input$design_input, {
      req(input$design_input)
      meta <- rv_design_metadata()
      
      if (input$design_input %in% meta$epm_designs) {
        shinyjs::show("distribution_container")
        if (input$distribution_input == "none") {
          updateSelectInput(session, "distribution_input", selected = "normal")
        }
      } else {
        shinyjs::hide("distribution_container")
        updateSelectInput(session, "distribution_input", selected = "none")
      }
    })
    
    # 4. Execution Logic (Run CV) -----------------------------------------------------------------
    observeEvent(input$run_cv_btn, {
      req(rv_project_name(), rv_folderpath())
      project_name <- rv_project_name()$value
      
      if (input$design_input == "") {
        showNotification("Please select a Model Design.", type = "warning")
        return()
      }
      
      shinyjs::hide("cv_success_message")
      shinyjs::hide("cv_error_message")
      
      # Show the custom spinner
      shinyjs::show("run_cv_spinner_container")
      shinyjs::disable("run_cv_btn")
      
      # Clear previous results
      rv_cv_results(NULL)
      
      tryCatch({
        dist_val <- if (input$distribution_input == "none") NULL else input$distribution_input
        
        res <- fishset_cv(
          project = project_name,
          base_model_name = input$design_input,
          k = input$k_input,
          distribution = dist_val,
          update_shiny = TRUE # Keeps your loop hooks active if you added them
        )
        
        rv_cv_results(res)
        
        output$cv_success_out <- renderText({
          paste0("Success: ", isolate(input$k_input), "-Fold Cross-Validation completed.")
        })
        shinyjs::show("cv_success_message")
        
      }, error = function(e) {
        output$cv_error_out <- renderText({ paste("Error:", e$message) })
        shinyjs::show("cv_error_message")
      }, finally = {
        # Hide the custom spinner
        shinyjs::hide("run_cv_spinner_container")
        shinyjs::enable("run_cv_btn")
      })
    })
    
    # 5. Result Rendering -------------------------------------------------------------------------
    fmt <- function(n, d = 2) if(is.null(n) || is.na(n)) "NA" else format(round(n, d), nsmall = d)
    
    # Render Overall Summary UI
    output$cv_summary_ui <- renderUI({
      res <- rv_cv_results()
      if (is.null(res)) return(tags$p("Run cross-validation to view summary statistics.",
                                      class = "text-muted"))
      
      tagList(
        tags$ul(class = "list-group list-group-flush",
                tags$li(class = "list-group-item d-flex justify-content-between align-items-center",
                        "Average Out-of-Sample Log-Likelihood:",
                        tags$strong(fmt(res$avg_out_sample_logLik))),
                tags$li(class = "list-group-item d-flex justify-content-between align-items-center",
                        "Average Out-of-Sample AIC:", 
                        tags$strong(fmt(res$avg_out_sample_AIC))),
                tags$li(class = "list-group-item d-flex justify-content-between align-items-center",
                        "Average Out-of-Sample Accuracy:", 
                        tags$strong(paste0(fmt(res$avg_out_sample_accuracy * 100, 1), "%"))),
                tags$li(class = "list-group-item d-flex justify-content-between align-items-center",
                        "Average Out-of-Sample PAPE:", 
                        tags$strong(paste0(fmt(res$avg_out_sample_PAPE * 100, 1), "%")))
        )
      )
    })
    
    # Render Fold Details Table
    output$fold_details_table <- DT::renderDataTable({
      res <- rv_cv_results()
      if (is.null(res)) return(NULL)
      
      df <- res$fold_details
      df$In_Sample_LL    <- sapply(df$In_Sample_LL, fmt)
      df$Out_Sample_LL   <- sapply(df$Out_Sample_LL, fmt)
      df$In_Sample_AIC   <- sapply(df$In_Sample_AIC, fmt)
      df$Out_Sample_AIC  <- sapply(df$Out_Sample_AIC, fmt)
      df$In_Sample_Acc   <- sapply(df$In_Sample_Acc, function(x) paste0(fmt(x * 100, 1), "%"))
      df$Out_Sample_Acc  <- sapply(df$Out_Sample_Acc, function(x) paste0(fmt(x * 100, 1), "%"))
      df$Out_Sample_PAPE <- sapply(df$Out_Sample_PAPE, function(x) paste0(fmt(x * 100, 1), "%"))
      
      DT::datatable(df, options = list(pageLength = 10, dom = 't', scrollX = TRUE),
                    rownames = FALSE, class = 'cell-border stripe hover')
    })
    
    # Render Coefficient Table
    output$fold_coef_table <- DT::renderDataTable({
      res <- rv_cv_results()
      if (is.null(res) || is.null(res$fold_coefficients)) return(NULL)
      
      coef_df <- as.data.frame(res$fold_coefficients)
      # Calculate averages and append as the last row
      avgs <- colMeans(coef_df, na.rm = TRUE)
      coef_df <- rbind(coef_df, avgs)
      rownames(coef_df)[nrow(coef_df)] <- "Average"
      
      # Format all numerics to 4 decimal places
      coef_df[] <- lapply(coef_df, function(x) sapply(x, fmt, d = 4))
      
      # Add fold names as a proper column for DT
      coef_df <- cbind(Fold = rownames(coef_df), coef_df)
      
      DT::datatable(coef_df, options = list(pageLength = 15, dom = 't', scrollX = TRUE),
                    rownames = FALSE, class = 'cell-border stripe hover') %>%
        DT::formatStyle('Fold', target = 'row',
                        fontWeight = DT::styleEqual('Average', 'bold'),
                        backgroundColor = DT::styleEqual('Average', '#f8f9fa'))
    })
  })
}

# model cv UI -------------------------------------------------------------------------------------
#' model_cv_ui
#'
#' @param id A character string that is unique to this module instance.
#' @return A tagList containing the UI elements.
model_cv_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    shinyjs::useShinyjs(),
    div(id = ns("main_container"),
        
        # Configuration and Run
        bslib::card(
          class = "card-overflow",
          bslib::card_header('Cross-Validate Discrete Choice Model'),
          bslib::card_body(
            class = "card-overflow",
            p("This module performs k-fold cross-validation on a selected Model Design. 
              It assesses out-of-sample predictive performance, calculating accuracy and PAPE 
              (Percent Absolute Prediction Error)."),
            
            bslib::card(
              class = "card-overflow bg-light",
              bslib::card_body(
                class = "card-overflow",
                bslib::layout_column_wrap(
                  width = 1/3, 
                  selectizeInput(ns("design_input"), 
                                 label = tags$span("Model Design ", 
                                                   bslib::tooltip(shiny::icon("info-circle"), 
                                                                  "Select a base design matrix.")),
                                 choices = NULL, width = "100%"),
                  numericInput(ns("k_input"), 
                               label = "Number of Folds (k)", 
                               value = 5, min = 2, max = 20, step = 1, width = "100%"),
                  div(
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
            
            fluidRow(
              column(4, style = "margin-top: 15px;",
                     actionButton(ns("run_cv_btn"), "Run Cross-Validation",
                                  icon = icon("layer-group"), class = "btn-secondary",
                                  width = "100%")
              )
            ),
            
            # Added spinner_ui 
            div(id = ns("run_cv_spinner_container"),
                style = "display: none; margin-top: 15px;",
                spinner_ui(ns("run_cv_spinner"),
                           spinner_type = "circle",
                           message = "Running Cross-Validation (This may take a moment)...", 
                           overlay = TRUE)),
            div(id = ns("cv_success_message"),
                style = "color: green; display: none; margin-top: 10px;",
                textOutput(ns("cv_success_out"))),
            div(id = ns("cv_error_message"),
                style = "color: red; display: none; margin-top: 10px;", 
                textOutput(ns("cv_error_out")))
          )
        ),
        
        hr(class = "my-4"),
        
        # Summary Stats
        bslib::card(
          class = "card-overflow",
          bslib::card_header("Overall Out-of-Sample Performance"),
          bslib::card_body(
            uiOutput(ns("cv_summary_ui"))
          )
        ),
        
        # Fold Metrics
        bslib::card(
          class = "card-overflow mt-4",
          bslib::card_header("Metrics by Fold"),
          bslib::card_body(
            class = "card-overflow", 
            div(style = "width: 100%; overflow-x: auto;", 
                DT::dataTableOutput(ns("fold_details_table"))
            )
          )
        ),
        
        # Fold Coefficients
        bslib::card(
          class = "card-overflow mt-4",
          bslib::card_header("Estimated Coefficients by Fold"),
          bslib::card_body(
            class = "card-overflow",
            div(style = "width: 100%; position: relative; overflow-x: auto;", 
                DT::dataTableOutput(ns("fold_coef_table"))
            )
          )
        )
    )
  )
}