# =================================================================================================
# File: policy_welfare_module.R
# Description: This module defines the UI and server logic for summarizing and visualizing 
#              expected economic welfare changes from policy simulations using Plotly.
# Dependencies: shiny, DT, shinyjs, bslib, plotly, ggplot2, grDevices
# Notes: This module interacts with PolicySimulations (input).
# =================================================================================================

# policy welfare server ---------------------------------------------------------------------------
#' policy_welfare_server
#'
#' @param id A character string that is unique to this module instance.
#' @param rv_folderpath A reactive value containing the current root folder path.
#' @param rv_project_name A reactive value containing the current project name.
#' @param rv_data A reactiveValues object containing the loaded data frames.
#'
#' @return This module does not return a value.
policy_welfare_server <- function(id, rv_folderpath, rv_project_name, rv_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive state
    rv_available_models <- reactiveVal(character(0))
    rv_available_scenarios <- reactiveVal(character(0))
    
    # Cache for results
    rv_welfare <- reactiveVal(NULL)
    
    # Real-time Polling Setup ---------------------------------------------------------------------
    db_check_func <- function() {
      if (is.null(rv_project_name())) return(NULL)
      project <- rv_project_name()$value
      if (is.null(project) || trimws(project) == "") return(NULL)
      db_path <- tryCatch(locdatabase(project), error = function(e) NULL)
      if (is.null(db_path) || !file.exists(db_path)) return(NULL)
      return(file.info(db_path)$mtime)
    }
    
    poll_simulations <- reactivePoll(
      intervalMillis = 1000,
      session = session,
      checkFunc = db_check_func,
      valueFunc = function() {
        if (is.null(rv_project_name())) return(list(models = character(0), 
                                                    scenarios = character(0)))
        project <- rv_project_name()$value
        if (is.null(project) || trimws(project) == "") return(list(models = character(0),
                                                                   scenarios = character(0)))
        
        tryCatch({
          table_name <- paste0(project, "PolicySimulations")
          sim_list <- unserialize_table(table_name, project)
          
          if (length(sim_list) > 0) {
            all_models <- c()
            all_scenarios <- c()
            for (nm in names(sim_list)) {
              x <- sim_list[[nm]]
              mod <- if (!is.null(x$model_name)) as.character(x$model_name)[1] else if 
              (!is.null(x$metadata$model_name)) as.character(x$metadata$model_name)[1] else NA
              scen <- if (!is.null(x$scenario)) as.character(x$scenario)[1] else if
              (!is.null(x$metadata$scenario)) as.character(x$metadata$scenario)[1] else NA
              
              # Automatically filter out baseline models/scenarios
              if (!is.na(mod) && 
                  !grepl("baseline", mod, ignore.case = TRUE)) all_models <- c(all_models, mod)
              if (!is.na(scen) && !grepl("baseline", scen, 
                                         ignore.case = TRUE)) all_scenarios <- c(all_scenarios, scen)
            }
            return(list(models = unique(all_models), scenarios = unique(all_scenarios)))
          }
          return(list(models = character(0), scenarios = character(0)))
        }, error = function(e) {
          return(list(models = character(0), scenarios = character(0)))
        })
      }
    )
    
    observe({
      sim_data <- poll_simulations()
      models <- sim_data$models
      scenarios <- sim_data$scenarios
      
      rv_available_models(models)
      rv_available_scenarios(scenarios)
      
      current_models <- isolate(input$models_filter)
      if (is.null(current_models) && length(models) > 0) {
        updateSelectizeInput(session, "models_filter", choices = models, selected = models[1])
      } else {
        valid_selections <- intersect(current_models, models)
        updateSelectizeInput(session, "models_filter", choices = models,
                             selected = valid_selections)
      }
      
      current_scenarios <- isolate(input$scenarios_filter)
      if (is.null(current_scenarios) && length(scenarios) > 0) {
        updateSelectizeInput(session, "scenarios_filter", choices = scenarios, 
                             selected = scenarios[1])
      } else {
        valid_selections <- intersect(current_scenarios, scenarios)
        updateSelectizeInput(session, "scenarios_filter", choices = scenarios, 
                             selected = valid_selections)
      }
    })
    
    observeEvent(input$select_all_models, {
      req(length(rv_available_models()) > 0)
      updateSelectizeInput(session, "models_filter", selected = rv_available_models())
    })
    
    observeEvent(input$select_all_scenarios, {
      req(length(rv_available_scenarios()) > 0)
      updateSelectizeInput(session, "scenarios_filter", selected = rv_available_scenarios())
    })
    
    # Run Summarize Welfare Logic -----------------------------------------------------------------
    observeEvent(input$run_welfare_btn, {
      req(rv_project_name())
      project_name <- rv_project_name()$value
      
      if (is.null(input$models_filter) || is.null(input$scenarios_filter)) {
        showNotification("Please select at least one model and one scenario.", type = "warning")
        return()
      }
      
      shinyjs::hide("welfare_success_message")
      shinyjs::hide("welfare_error_message")
      shinyjs::show("run_welfare_spinner_container")
      shinyjs::disable("run_welfare_btn")
      
      rv_welfare(NULL)
      
      tryCatch({
        args <- list(project = project_name)
        if (!is.null(input$models_filter) && length(input$models_filter) > 0) 
          args$plot_models <- input$models_filter
        if (!is.null(input$scenarios_filter) && length(input$scenarios_filter) > 0) 
          args$plot_scenarios <- input$scenarios_filter
        
        res <- NULL
        withCallingHandlers({
          res <- do.call(summarize_policy_welfare, args)
        }, warning = function(w) {
          showNotification(paste("Summary Warning:", w$message), type = "warning", duration = 10)
          invokeRestart("muffleWarning")
        })
        
        rv_welfare(res)
        
        output$welfare_success_out <- renderText({ "Success: Welfare summaries generated." })
        shinyjs::show("welfare_success_message")
        shinyjs::show("results_container")
        
      }, error = function(e) {
        output$summary_error_out <- renderText({ paste("Error:", e$message) })
        shinyjs::show("welfare_error_message")
        shinyjs::hide("results_container")
      }, finally = {
        shinyjs::hide("run_welfare_spinner_container")
        shinyjs::enable("run_welfare_btn")
      })
    })
    
    # Render Data Table ---------------------------------------------------------------------------
    output$welfare_table <- DT::renderDataTable({
      req(rv_welfare())
      df <- rv_welfare()$summary_data 
      
      # Clean baseline from the data table as a safeguard
      if ("Simulation" %in% names(df)) {
        df <- df[!grepl("baseline", df$Simulation, ignore.case = TRUE), ]
      } else if ("Scenario" %in% names(df)) {
        df <- df[!grepl("baseline", df$Scenario, ignore.case = TRUE), ]
      }
      
      numeric_cols <- c("Mean_Welfare_Per_Trip", "Lower_95", "Median", "Upper_95")
      for (col in numeric_cols) {
        if (col %in% names(df)) df[[col]] <- round(df[[col]], 2)
      }
      
      DT::datatable(df,
                    options = list(pageLength = 10, scrollX = TRUE, dom = 'Bfrtip'),
                    rownames = FALSE,
                    class = 'cell-border stripe hover')
    })
    
    # Connect Dynamic Plot Renderers --------------------------------------------------------------
    output$welfare_bar_plot <- plotly::renderPlotly({
      req(rv_welfare())
      
      # Build a clean, unbroken string for the parser
      title_text <- paste0(
        "Expected Welfare Impacts of Policy Scenarios<br>",
        "<span style='font-size:13px; color:#555;'>",
        "Mean compensating variation per trip/haul (with 95% intervals)</span>"
      )
      
      plotly::ggplotly(rv_welfare()$plot_bar) %>% 
        plotly::layout(
          title = list(
            text = title_text,
            x = 0.5,             # Center horizontally
            xanchor = "center",
            y = 0.95             # Push slightly down from the very top edge
          ),
          legend = list(
            orientation = "h",   
            xanchor = "center",  
            x = 0.5,             
            y = -0.3             
          ),
          margin = list(t = 80, b = 100) # Ensures room for the 2-line title and bottom legend
        )
    })
    
    output$welfare_density_plot <- plotly::renderPlotly({
      req(rv_welfare())
      
      # Build a clean, unbroken string for the parser
      title_text <- paste0(
        "Welfare Uncertainty Distribution<br>",
        "<span style='font-size:13px; color:#555;'>",
        "Full distribution of simulated welfare draws showing skew and risk</span>"
      )
      
      plotly::ggplotly(rv_welfare()$plot_density) %>% 
        plotly::layout(
          title = list(
            text = title_text,
            x = 0.5,
            xanchor = "center",
            y = 0.95
          ),
          showlegend = TRUE,     
          legend = list(
            orientation = "h",
            xanchor = "center",
            x = 0.5,
            y = -0.3
          ),
          margin = list(t = 80, b = 100)
        )
    })
    
    # Download Logic ------------------------------------------------------------------------------
    output$download_welfare_btn <- downloadHandler(
      filename = function() { paste0("Policy_Welfare_Static_", Sys.Date(), ".pdf") },
      content = function(file) {
        req(rv_welfare())
        grDevices::pdf(file, width = 11, height = 8.5)
        print(rv_welfare()$plot_bar)
        print(rv_welfare()$plot_density)
        grDevices::dev.off()
      }
    )
  })
}

# policy welfare UI -------------------------------------------------------------------------------
policy_welfare_ui <- function(id) {
  ns <- NS(id)
  
  custom_css <- "
    .nav-pills .nav-link { 
      font-size: 0.85rem !important; 
      padding: 0.3rem 0.8rem !important; 
      margin-left: 5px; 
      font-weight: 500;
    }
  "
  
  tagList(
    shinyjs::useShinyjs(),
    tags$head(tags$style(HTML(custom_css))),
    
    div(id = ns("main_container"),
        
        # CARD 1: Configuration
        bslib::card(
          class = "card-overflow",
          bslib::card_header('Summarize Welfare Impacts'),
          bslib::card_body(
            class = "card-overflow",
            p("Extract and visualize the expected economic welfare changes (Compensating Variation) 
              across your policy simulations."),
            
            fluidRow(
              class = "mt-2",
              column(6,
                     div(class = "d-flex justify-content-between align-items-end",
                         tags$label("Filter by Model(s):",
                                    style = "margin-bottom: 2px; font-weight: bold;"),
                         actionLink(ns("select_all_models"), "Select All", 
                                    style = "font-size: 0.85em; text-decoration: none;")
                     ),
                     selectizeInput(ns("models_filter"), label = NULL, choices = NULL,
                                    multiple = TRUE, width = "100%",
                                    options = list(plugins = list("remove_button")))
              ),
              column(6,
                     div(class = "d-flex justify-content-between align-items-end",
                         tags$label("Filter by Scenario(s):", 
                                    style = "margin-bottom: 2px; font-weight: bold;"),
                         actionLink(ns("select_all_scenarios"), "Select All", 
                                    style = "font-size: 0.85em; text-decoration: none;")
                     ),
                     selectizeInput(ns("scenarios_filter"), label = NULL, choices = NULL, 
                                    multiple = TRUE, width = "100%",
                                    options = list(plugins = list("remove_button")))
              )
            )
          )
        ),
        
        fluidRow(
          column(6, style = "margin-top: 15px; padding-left: 30px;",
                 actionButton(ns("run_welfare_btn"), "Generate Summaries",
                              icon = icon("chart-line"), class = "btn-secondary", width = "100%")
          )
        ),
        
        div(id = ns("run_welfare_spinner_container"), 
            style = "display: none; margin-top: 15px; padding-left: 15px;",
            spinner_ui(ns("run_welfare_spinner"), 
                       spinner_type = "circle",
                       message = "Calculating welfare distributions and generating plots...", 
                       overlay = TRUE)
        ),
        div(id = ns("welfare_success_message"), 
            style = "color: green; display: none; margin-top: 10px; padding-left: 15px;", 
            textOutput(ns("welfare_success_out"))),
        div(id = ns("welfare_error_message"), 
            style = "color: red; display: none; margin-top: 10px; padding-left: 15px;", 
            textOutput(ns("welfare_error_out"))),
        
        # CARD 2: Output Presentation
        shinyjs::hidden(
          div(
            id = ns("results_container"), class = "mt-4",
            bslib::navset_card_pill(
              id = ns("results_tabs"),
              title = "Welfare Analysis",
              bslib::nav_spacer(),
              
              # Tab A: Interactive Graphic Panels
              bslib::nav_panel(
                "Visualizations", 
                bslib::layout_columns(
                  class = "mt-3",
                  col_widths = c(6, 6),
                  
                  # Figure 1: Mean Impact
                  bslib::card(
                    height = "600px", 
                    bslib::card_body(
                      plotly::plotlyOutput(ns("welfare_bar_plot"), height = "100%")
                    )
                  ),
                  
                  # Figure 2: Uncertainty
                  bslib::card(
                    height = "600px", 
                    bslib::card_body(
                      plotly::plotlyOutput(ns("welfare_density_plot"), height = "100%")
                    )
                  )
                ),
                
                div(class = "text-center mt-4 mb-2",
                    downloadButton(ns("download_welfare_btn"),"View & Save Static Plots", 
                                   icon = icon("image"), class = "btn-info")
                )
              ),
              
              # Tab B: Data Table
              bslib::nav_panel("Data Table", 
                               div(class = "mt-3", DT::dataTableOutput(ns("welfare_table"))))
            )
          )
        )
    )
  )
}