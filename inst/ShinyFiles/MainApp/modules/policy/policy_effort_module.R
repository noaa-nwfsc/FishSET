# =================================================================================================
# File: policy_effort_module.R
# Description: This module defines the UI and server logic for summarizing and visualizing 
#              spatial effort redistribution from policy simulations.
# Dependencies: shiny, DT, shinyjs, bslib, plotly, leaflet, sf
# Notes: This module interacts with PolicySimulations and spatial data (input).
# =================================================================================================

# policy effort server ----------------------------------------------------------------------------
#' policy_effort_server
#'
#' @param id A character string that is unique to this module instance.
#' @param rv_folderpath A reactive value containing the current root folder path.
#' @param rv_project_name A reactive value containing the current project name.
#' @param rv_data A reactiveValues object containing the loaded data frames.
#'
#' @return This module does not return a value.

policy_effort_server <- function(id, rv_folderpath, rv_project_name, rv_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive state
    rv_available_models <- reactiveVal(character(0))
    rv_available_scenarios <- reactiveVal(character(0))
    rv_selected_vars <- reactiveValues(vars = NULL)
    
    # Caches
    rv_effort_dyn <- reactiveVal(NULL)
    rv_effort_stat <- reactiveVal(NULL)
    rv_highlight_zone <- reactiveVal(NULL)
    
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
              if (!is.na(mod)) all_models <- c(all_models, mod)
              if (!is.na(scen)) all_scenarios <- c(all_scenarios, scen)
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
    
    # Run Summarize Policy Effort Logic -----------------------------------------------------------
    observeEvent(input$run_summary_btn, {
      req(rv_project_name())
      project_name <- rv_project_name()$value
      
      if (is.null(input$models_filter) || is.null(input$scenarios_filter)) {
        showNotification("Please select at least one model and one scenario.", type = "warning")
        return()
      }
      
      selected_vars <- load_gui_variables(project_name, folderpath)
      if (is.null(selected_vars)) {
        shinyjs::hide("run_design_spinner_container")
        shinyjs::enable("run_design_btn")
        showModal(modalDialog(
          title = "Error: Missing Data",
          "The selected variables file could not be found.",
          easyClose = TRUE
        ))
        return()
      }
      rv_selected_vars$vars <- selected_vars
      
      shinyjs::hide("summary_success_message")
      shinyjs::hide("summary_error_message")
      shinyjs::show("run_summary_spinner_container")
      shinyjs::disable("run_summary_btn")
      
      rv_effort_dyn(NULL)
      rv_effort_stat(NULL)
      rv_highlight_zone(NULL)
      
      tryCatch({
        args <- list(
          project = project_name,
          spat = rv_data$spat,
          zone_spat = rv_selected_vars$vars$spat$spat_zone_id
        )
        
        if (!is.null(input$models_filter) && length(input$models_filter) > 0) {
          args$plot_models <- input$models_filter
        }
        
        if (!is.null(input$scenarios_filter) && length(input$scenarios_filter) > 0) {
          args$plot_scenarios <- input$scenarios_filter
        }
        
        res_dyn <- NULL
        res_stat <- NULL
        
        withCallingHandlers({
          res_dyn <- do.call(summarize_policy_effort,
                             c(args, list(output_type = "dynamic", 
                                          plotly_source = ns("effort_scatter"))))
          res_stat <- do.call(summarize_policy_effort,
                              c(args, list(output_type = "static")))
        }, warning = function(w) {
          showNotification(paste("Summary Warning:", w$message), type = "warning", duration = 10)
          invokeRestart("muffleWarning")
        })
        
        rv_effort_dyn(res_dyn)
        rv_effort_stat(res_stat)
        
        sim_names <- unique(res_dyn$summary_data$Simulation)
        updateSelectizeInput(session, "sim_viewer_input", choices = sim_names,
                             selected = sim_names[1])
        
        output$summary_success_out <- renderText({ "Success: Summaries and plots generated." })
        shinyjs::show("summary_success_message")
        shinyjs::show("results_container")
        
      }, error = function(e) {
        output$summary_error_out <- renderText({ paste("Error:", e$message) })
        shinyjs::show("summary_error_message")
        shinyjs::hide("results_container")
      }, finally = {
        shinyjs::hide("run_summary_spinner_container")
        shinyjs::enable("run_summary_btn")
      })
    })
    
    # Render Data Table ---------------------------------------------------------------------------
    output$effort_table <- DT::renderDataTable({
      req(rv_effort_dyn())
      df <- rv_effort_dyn()$summary_data 
      
      df$Baseline_Effort <- round(df$Baseline_Effort, 2)
      df$Counterfactual_Effort <- round(df$Counterfactual_Effort, 2)
      df$Effort_Change <- round(df$Effort_Change, 2)
      df$Pct_Effort_Change <- round(df$Pct_Effort_Change, 2)
      
      DT::datatable(df,
                    options = list(pageLength = 10, scrollX = TRUE, dom = 'Bfrtip'),
                    rownames = FALSE,
                    class = 'cell-border stripe hover')
    })
    
    observeEvent(c(input$sim_viewer_input, input$map_type_input), {
      rv_highlight_zone(NULL)
    })
    
    # Connect Dynamic Renderers -------------------------------------------------------------------
    output$selected_map_dyn <- leaflet::renderLeaflet({
      req(rv_effort_dyn(), input$sim_viewer_input, input$map_type_input)
      
      leaflet::leafletProxy("selected_map_dyn") %>% leaflet::clearGroup("click_highlight")
      
      if (input$map_type_input == "Absolute Change") {
        rv_effort_dyn()$plots_absolute_map[[input$sim_viewer_input]]
      } else {
        rv_effort_dyn()$plots_percent_map[[input$sim_viewer_input]]
      }
    })
    
    output$scatter_dyn <- plotly::renderPlotly({
      req(rv_effort_dyn(), input$sim_viewer_input)
      
      p <- rv_effort_dyn()$plots_scatter[[input$sim_viewer_input]]
      
      # Inject custom JS to handle click events directly, bypassing plotly::event_data
      js_code <- sprintf("
        function(el, x) {
          el.on('plotly_click', function(data) {
            if (data && data.points && data.points.length > 0) {
              var zone = data.points[0].customdata;
              // Send the customdata to a standard Shiny input
              Shiny.setInputValue('%s', zone, {priority: 'event'});
            }
          });
          el.on('plotly_doubleclick', function(data) {
            // Send a reset trigger on double click
            Shiny.setInputValue('%s', 'reset_trigger', {priority: 'event'});
          });
        }
      ", ns("scatter_click"), ns("scatter_dblclick"))
      
      p <- htmlwidgets::onRender(p, js_code)
      return(p)
    })
    
    # Interactive Cross-Talk Logic ----------------------------------------------------------------
    # Update state on click
    observeEvent(input$scatter_click, {
      new_zone <- input$scatter_click
      req(new_zone)
      
      current_zone <- isolate(rv_highlight_zone())
      
      # Clean Toggle Logic: Click once to turn on, click again to turn off
      if (!is.null(current_zone) && current_zone == new_zone) {
        rv_highlight_zone(NULL) 
      } else {
        rv_highlight_zone(new_zone) 
      }
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    
    # Reset state on double click (Backup mechanism)
    observeEvent(input$scatter_dblclick, {
      rv_highlight_zone(NULL)
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    
    # Execute Map Draw 
    observe({
      clicked_zone <- rv_highlight_zone()
      leaflet::leafletProxy("selected_map_dyn") %>% leaflet::clearGroup("click_highlight")
      
      req(clicked_zone) 
      req(rv_project_name(), rv_data$spat, rv_selected_vars$vars$spat$spat_zone_id)
      
      project_name <- rv_project_name()$value
      zone_spat <- rv_selected_vars$vars$spat$spat_zone_id
      
      tryCatch({
        spat_out <- data_pull(rv_data$spat, project_name)
        spat_sf <- check_spatdat(spat_out$dataset, id = zone_spat)
        selected_poly <- spat_sf[spat_sf[[zone_spat]] == clicked_zone, ]
        
        if (nrow(selected_poly) > 0) {
          selected_poly_wgs <- sf::st_transform(selected_poly, 4326)
          
          leaflet::leafletProxy("selected_map_dyn") %>%
            leaflet::addPolygons(
              data = selected_poly_wgs,
              color = "#00E5FF", 
              weight = 5,
              fillColor = "transparent",
              opacity = 1,
              group = "click_highlight"
            )
        }
      }, error = function(e) { })
    })
    
    # Static Plots Modal --------------------------------------------------------------------------
    observeEvent(input$view_static_btn, {
      req(rv_effort_stat(), input$sim_viewer_input)
      
      showModal(modalDialog(
        title = paste("Static Plots:", input$sim_viewer_input),
        size = "l",
        fluidRow(
          column(6, plotOutput(ns("modal_abs_map"), height = "400px")),
          column(6, plotOutput(ns("modal_pct_map"), height = "400px"))
        ),
        fluidRow(column(12, plotOutput(ns("modal_scatter"), height = "500px"))),
        footer = tagList(
          downloadButton(ns("download_static_btn"), "Download as PDF", class = "btn-success"),
          modalButton("Close")
        )
      ))
    })
    
    output$modal_abs_map <- renderPlot({ req(rv_effort_stat());
      rv_effort_stat()$plots_absolute_map[[input$sim_viewer_input]] })
    output$modal_pct_map <- renderPlot({ req(rv_effort_stat()); 
      rv_effort_stat()$plots_percent_map[[input$sim_viewer_input]] })
    output$modal_scatter <- renderPlot({ req(rv_effort_stat()); 
      rv_effort_stat()$plots_scatter[[input$sim_viewer_input]] })
    
    output$download_static_btn <- downloadHandler(
      filename = function() { paste0("Policy_Effort_Static_", 
                                     input$sim_viewer_input, "_", Sys.Date(), ".pdf") },
      content = function(file) {
        req(rv_effort_stat(), input$sim_viewer_input)
        grDevices::pdf(file, width = 11, height = 8.5)
        print(rv_effort_stat()$plots_absolute_map[[input$sim_viewer_input]])
        print(rv_effort_stat()$plots_percent_map[[input$sim_viewer_input]])
        print(rv_effort_stat()$plots_scatter[[input$sim_viewer_input]])
        grDevices::dev.off()
      }
    )
  })
}

# policy effort UI --------------------------------------------------------------------------------
policy_effort_ui <- function(id) {
  ns <- NS(id)
  
  custom_css <- "
    .header-input-wrapper .form-group { margin-bottom: 0px !important; }
    .selectize-dropdown { z-index: 99999 !important; }
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
          bslib::card_header('Summarize Effort Redistribution'),
          bslib::card_body(
            class = "card-overflow",
            p("Extract and visualize the spatial redistribution of expected fishing effort across 
              your policy simulations."),
            
            fluidRow(class = "mt-2",
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
                 actionButton(ns("run_summary_btn"), "Generate Summaries",
                              icon = icon("chart-pie"), class = "btn-secondary", width = "100%")
          )
        ),
        
        div(id = ns("run_summary_spinner_container"), 
            style = "display: none; margin-top: 15px; padding-left: 15px;",
            spinner_ui(ns("run_summary_spinner"), spinner_type = "circle",
                       message = "Processing spatial data and generating plots...", overlay = TRUE)
        ),
        div(id = ns("summary_success_message"), 
            style = "color: green; display: none; margin-top: 10px; padding-left: 15px;", 
            textOutput(ns("summary_success_out"))
        ),
        div(id = ns("summary_error_message"), 
            style = "color: red; display: none; margin-top: 10px; padding-left: 15px;", 
            textOutput(ns("summary_error_out"))
        ),
        
        # CARD 2: Output Presentation
        shinyjs::hidden(
          div(id = ns("results_container"), class = "mt-4",
              bslib::navset_card_pill(
                id = ns("results_tabs"),
                title = "Fishing Effort Redistribution",
                
                bslib::nav_spacer(),
                
                # Tab A: Interactive Graphic Panels
                bslib::nav_panel(
                  "Visualizations", 
                  div(class = "mt-3 mb-2",
                      selectizeInput(ns("sim_viewer_input"), 
                                     label = "Select policy simulation:", 
                                     choices = NULL, width = "50%")
                  ),
                  
                  bslib::layout_columns(
                    col_widths = c(6, 6),
                    
                    # MAP CARD
                    bslib::card(
                      height = "550px", 
                      class = "overflow-visible",
                      bslib::card_header(
                        class = "d-flex justify-content-between align-items-center py-1",
                        tags$span("Spatial Distribution", style = "font-weight: bold;"),
                        div(class = "header-input-wrapper", style = "width: 180px;", 
                            selectInput(ns("map_type_input"), label = NULL, 
                                        choices = c("Absolute Change", "Percent Change"),
                                        width = "100%")
                        )
                      ),
                      bslib::card_body(
                        class = "p-0", 
                        leaflet::leafletOutput(ns("selected_map_dyn"), height = "100%")
                      )
                    ),
                    
                    # SCATTER CARD
                    bslib::card(
                      height = "550px", 
                      bslib::card_header(
                        class = "py-2",
                        tags$span("Effort Relative to Baseline", style = "font-weight: bold;")
                      ),
                      bslib::card_body(
                        plotly::plotlyOutput(ns("scatter_dyn"), height = "100%")
                      ),
                      bslib::card_footer(
                        style = "font-size: 0.9em; background-color: #e9f7fd; border-top: 1px solid
                        #b8e0ed; color: #31708f;",
                        shiny::icon("hand-pointer"), tags$strong(" Interactive Plot:"),
                        " Click a point to highlight its location on the map. Click again to clear."
                      )
                    )
                  ),
                  
                  div(class = "text-center mt-4 mb-2",
                      actionButton(ns("view_static_btn"), "View & Save Static Plots", 
                                   icon = icon("image"), class = "btn-info")
                  )
                ),
                
                # Tab B: Filtered Raw Metrics Matrix Table
                bslib::nav_panel("Data Table", 
                                 div(class = "mt-3", DT::dataTableOutput(ns("effort_table"))))
              )
          )
        )
    )
  )
}