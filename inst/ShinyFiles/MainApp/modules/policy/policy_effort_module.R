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
    
    # Interaction state for up to 4 dynamic viewers
    rv_viewer_count <- reactiveVal(1)
    rv_highlights <- reactiveValues(z1 = NULL, z2 = NULL, z3 = NULL, z4 = NULL)
    
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
              if (!is.na(scen) &&!grepl("baseline", scen, 
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
      
      # Clear highlights and reset viewers when new summaries are run
      rv_highlights$z1 <- NULL; rv_highlights$z2 <- NULL; rv_highlights$z3 <- NULL 
      rv_highlights$z4 <- NULL
      rv_viewer_count(1)
      shinyjs::hide("viewer_container_2")
      shinyjs::hide("viewer_container_3")
      shinyjs::hide("viewer_container_4")
      shinyjs::show("add_viewer_btn")
      shinyjs::hide("clear_viewers_btn")
      
      tryCatch({
        args <- list(
          project = project_name,
          spat = rv_data$spat,
          zone_spat = rv_selected_vars$vars$spat$spat_zone_id
        )
        
        if (!is.null(input$models_filter) && 
            length(input$models_filter) > 0) args$plot_models <- input$models_filter
        if (!is.null(input$scenarios_filter) && 
            length(input$scenarios_filter) > 0) args$plot_scenarios <- input$scenarios_filter
        
        res_dyn <- NULL
        res_stat <- NULL
        
        withCallingHandlers({
          res_dyn <- do.call(summarize_policy_effort,
                             c(args, list(output_type = "dynamic",
                                          plotly_source = ns("effort_scatter"))))
          res_stat <- do.call(summarize_policy_effort, c(args, list(output_type = "static")))
        }, warning = function(w) {
          showNotification(paste("Summary Warning:", w$message), type = "warning", duration = 10)
          invokeRestart("muffleWarning")
        })
        
        rv_effort_dyn(res_dyn)
        rv_effort_stat(res_stat)
        
        # Pull simulation names and filter out any remaining baselines
        sim_names <- unique(res_dyn$summary_data$Simulation)
        sim_names <- sim_names[!grepl("baseline", sim_names, ignore.case = TRUE)]
        
        if (length(sim_names) == 0) {
          showNotification("No non-baseline simulations found for the selected filters.",
                           type = "warning")
        }
        
        # Populate dropdowns for ALL 4 viewers, even if hidden
        for(i in 1:4) {
          target_id <- if(i == 1) "sim_viewer_input" else paste0("sim_viewer_input_", i)
          sel <- 
            if(length(sim_names) >= i) sim_names[i] else
              if(length(sim_names) > 0) sim_names[1] else character(0)
          updateSelectizeInput(session, target_id, choices = sim_names, selected = sel)
        }
        
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
    
    # Manage dynamic UI additions -----------------------------------------------------------------
    observeEvent(input$add_viewer_btn, {
      current <- rv_viewer_count()
      if (current < 4) {
        new_count <- current + 1
        rv_viewer_count(new_count)
        shinyjs::show(paste0("viewer_container_", new_count))
        shinyjs::show("clear_viewers_btn")
        
        # Hide the button completely when max viewers reached
        if (new_count == 4) {
          shinyjs::hide("add_viewer_btn")
        }
      }
    })
    
    # Manage clearing viewers ---------------------------------------------------------------------
    observeEvent(input$clear_viewers_btn, {
      rv_viewer_count(1)
      shinyjs::hide("viewer_container_2")
      shinyjs::hide("viewer_container_3")
      shinyjs::hide("viewer_container_4")
      
      shinyjs::show("add_viewer_btn")
      shinyjs::hide("clear_viewers_btn")
      
      # Clear highlights in hidden viewers
      rv_highlights$z2 <- NULL
      rv_highlights$z3 <- NULL
      rv_highlights$z4 <- NULL
    })
    
    # Render Data Table ---------------------------------------------------------------------------
    output$effort_table <- DT::renderDataTable({
      req(rv_effort_dyn())
      df <- rv_effort_dyn()$summary_data 
      
      df$Baseline_Effort <- round(df$Baseline_Effort, 2)
      df$Counterfactual_Effort <- round(df$Counterfactual_Effort, 2)
      df$Effort_Change <- round(df$Effort_Change, 2)
      df$Pct_Effort_Change <- round(df$Pct_Effort_Change, 2)
      
      # Clean baseline from the data table as well
      df <- df[!grepl("baseline", df$Simulation, ignore.case = TRUE), ]
      
      DT::datatable(df, options = list(pageLength = 10, scrollX = TRUE, dom = 'Bfrtip'),
                    rownames = FALSE, class = 'cell-border stripe hover')
    })
    
    # Setup Logic for all 4 Viewers ---------------------------------------------------------------
    setup_viewer_logic <- function(index) {
      sim_id         <- if(index == 1) "sim_viewer_input" else paste0("sim_viewer_input_", index)
      map_type_id    <- if(index == 1) "map_type_input" else paste0("map_type_input_", index)
      map_out_id     <- if(index == 1) "selected_map_dyn" else paste0("selected_map_dyn_", index)
      scatter_out_id <- if(index == 1) "scatter_dyn" else paste0("scatter_dyn_", index)
      click_id       <- if(index == 1) "scatter_click" else paste0("scatter_click_", index)
      dblclick_id    <- if(index == 1) "scatter_dblclick" else paste0("scatter_dblclick_", index)
      group_id       <- if(index == 1) "click_highlight" else paste0("click_highlight_", index)
      zone_key       <- paste0("z", index)
      
      observeEvent(c(input[[sim_id]], input[[map_type_id]]), { rv_highlights[[zone_key]] <- NULL })
      
      output[[map_out_id]] <- leaflet::renderLeaflet({
        req(rv_effort_dyn(), input[[sim_id]], input[[map_type_id]])
        leaflet::leafletProxy(map_out_id) %>% leaflet::clearGroup(group_id)
        
        if (input[[map_type_id]] == "Absolute Change") {
          rv_effort_dyn()$plots_absolute_map[[input[[sim_id]]]]
        } else {
          rv_effort_dyn()$plots_percent_map[[input[[sim_id]]]]
        }
      })
      
      output[[scatter_out_id]] <- plotly::renderPlotly({
        req(rv_effort_dyn(), input[[sim_id]])
        p <- rv_effort_dyn()$plots_scatter[[input[[sim_id]]]]
        
        js_code <- sprintf("
          function(el, x) {
            el.on('plotly_click', function(data) {
              if (data && data.points && data.points.length > 0) {
                var zone = data.points[0].customdata;
                Shiny.setInputValue('%s', zone, {priority: 'event'});
              }
            });
            el.on('plotly_doubleclick', function(data) {
              Shiny.setInputValue('%s', 'reset_trigger', {priority: 'event'});
            });
          }
        ", ns(click_id), ns(dblclick_id))
        
        htmlwidgets::onRender(p, js_code)
      })
      
      observeEvent(input[[click_id]], {
        new_zone <- input[[click_id]]
        req(new_zone)
        current_zone <- isolate(rv_highlights[[zone_key]])
        
        if (!is.null(current_zone) && current_zone == new_zone) {
          rv_highlights[[zone_key]] <- NULL 
        } else {
          rv_highlights[[zone_key]] <- new_zone 
        }
      }, ignoreNULL = TRUE, ignoreInit = TRUE)
      
      observeEvent(input[[dblclick_id]], { rv_highlights[[zone_key]] <- NULL }, 
                   ignoreNULL = TRUE, ignoreInit = TRUE)
      
      observe({
        clicked_zone <- rv_highlights[[zone_key]]
        leaflet::leafletProxy(map_out_id) %>% leaflet::clearGroup(group_id)
        req(clicked_zone, rv_project_name(), rv_data$spat, rv_selected_vars$vars$spat$spat_zone_id)
        
        tryCatch({
          project_name <- rv_project_name()$value
          zone_spat <- rv_selected_vars$vars$spat$spat_zone_id
          spat_out <- data_pull(rv_data$spat, project_name)
          spat_sf <- check_spatdat(spat_out$dataset, id = zone_spat)
          selected_poly <- spat_sf[spat_sf[[zone_spat]] == clicked_zone, ]
          
          if (nrow(selected_poly) > 0) {
            selected_poly_wgs <- sf::st_transform(selected_poly, 4326)
            leaflet::leafletProxy(map_out_id) %>%
              leaflet::addPolygons(
                data = selected_poly_wgs, color = "#00E5FF", weight = 5,
                fillColor = "transparent", opacity = 1, group = group_id
              )
          }
        }, error = function(e) { })
      })
    }
    
    for(i in 1:4) setup_viewer_logic(i)
    # Static Plots Modal --------------------------------------------------------------------------
    observeEvent(input$view_static_btn, {
      req(rv_effort_stat())
      
      sim_choices <- unique(rv_effort_stat()$summary_data$Simulation)
      sim_choices <- sim_choices[!grepl("baseline", sim_choices, ignore.case = TRUE)]
      
      default_sel <- isolate(input$sim_viewer_input)
      if (is.null(default_sel) || !(default_sel %in% sim_choices)) default_sel <- sim_choices[1]
      
      showModal(modalDialog(
        title = "Export Static Plots",
        size = "l",
        
        fluidRow(
          column(12, 
                 div(class = "mb-3",
                     selectInput(ns("modal_sim_select"), "Select Simulation to View/Save:", 
                                 choices = sim_choices, selected = default_sel, width = "50%")
                 )
          )
        ),
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
    
    output$modal_abs_map <- renderPlot({ 
      req(rv_effort_stat(), input$modal_sim_select)
      rv_effort_stat()$plots_absolute_map[[input$modal_sim_select]] 
    })
    output$modal_pct_map <- renderPlot({ 
      req(rv_effort_stat(), input$modal_sim_select)
      rv_effort_stat()$plots_percent_map[[input$modal_sim_select]] 
    })
    output$modal_scatter <- renderPlot({ 
      req(rv_effort_stat(), input$modal_sim_select)
      rv_effort_stat()$plots_scatter[[input$modal_sim_select]] 
    })
    
    output$download_static_btn <- downloadHandler(
      filename = function() { paste0("Policy_Effort_Static_", input$modal_sim_select, "_",
                                     Sys.Date(), ".pdf") },
      content = function(file) {
        req(rv_effort_stat(), input$modal_sim_select)
        grDevices::pdf(file, width = 11, height = 8.5)
        print(rv_effort_stat()$plots_absolute_map[[input$modal_sim_select]])
        print(rv_effort_stat()$plots_percent_map[[input$modal_sim_select]])
        print(rv_effort_stat()$plots_scatter[[input$modal_sim_select]])
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
  
  # UI Generator Helper Function for Viewers
  create_viewer_ui <- function(ns, index) {
    sim_id         <- if(index == 1) "sim_viewer_input" else paste0("sim_viewer_input_", index)
    map_type_id    <- if(index == 1) "map_type_input" else paste0("map_type_input_", index)
    map_out_id     <- if(index == 1) "selected_map_dyn" else paste0("selected_map_dyn_", index)
    scatter_out_id <- if(index == 1) "scatter_dyn" else paste0("scatter_dyn_", index)
    
    # App Theme Colors
    theme_info    <- "#274472" # Darkest blue for top-level headers
    theme_primary <- "#41729F" # Medium blue for nested plot headers
    
    viewer_title <- if(index > 1) paste("Comparison", index - 1) else "Primary Simulation"
    
    # Overarching Master Card
    bslib::card(
      class = "mt-4 overflow-visible shadow-sm",
      
      # Master Card Header (Viewer Level) - Uses h5 & theme_info
      bslib::card_header(
        class = "d-flex justify-content-between align-items-center py-2 bg-light", 
        tags$h5(viewer_title, class = "mb-0", 
                style = paste0("color: ", theme_info, "; font-weight: bold;")),
        div(class = "header-input-wrapper", style = "width: 350px;", 
            selectizeInput(ns(sim_id), label = NULL, choices = NULL, width = "100%")
        )
      ),
      
      # Master Card Body
      bslib::card_body(
        class = "bg-white",
        
        bslib::layout_columns(
          col_widths = c(6, 6),
          
          # NESTED MAP CARD
          bslib::card(
            height = "550px", class = "overflow-visible", 
            
            # Nested Plot Header - Uses h6 & theme_primary
            bslib::card_header(
              class = "d-flex justify-content-between align-items-center py-1 bg-light", 
              tags$h6("Spatial Distribution", class = "mb-0", 
                      style = paste0("color: ", theme_primary, "; font-weight: 700;")),
              div(class = "header-input-wrapper", style = "width: 180px;", 
                  selectInput(ns(map_type_id), label = NULL, 
                              choices = c("Absolute Change", "Percent Change"), width = "100%")
              )
            ),
            bslib::card_body(
              class = "p-0", leaflet::leafletOutput(ns(map_out_id), height = "100%")
            )
          ),
          
          # NESTED SCATTER CARD
          bslib::card(
            height = "550px", 
            
            # Nested Plot Header - Uses h6 & theme_primary
            bslib::card_header(
              class = "py-2 bg-light", 
              tags$h6("Effort Relative to Baseline", class = "mb-0", 
                      style = paste0("color: ", theme_primary, "; font-weight: 700;"))
            ),
            bslib::card_body(
              plotly::plotlyOutput(ns(scatter_out_id), height = "100%")
            ),
            bslib::card_footer(
              style = "font-size: 0.9em; background-color: #e9f7fd;
              border-top: 1px solid #b8e0ed; color: #31708f;",
              shiny::icon("hand-pointer"), tags$strong(" Interactive Plot:"),
              " Click a point to highlight its location on the map. Click again to clear."
            )
          )
        )
      )
    )
  }
  
  tagList(
    shinyjs::useShinyjs(),
    tags$head(tags$style(HTML(custom_css))),
    
    div(id = ns("main_container"),
        
        # MODULE MASTER CARD: Configuration
        bslib::card(
          class = "card-overflow",
          
          # Module Master Header - Uses h4 & theme_info
          bslib::card_header(
            tags$h4('Summarize Effort Redistribution', class = "mb-0",
                    style = "color: #274472; font-weight: bold;")
          ),
          bslib::card_body(
            class = "card-overflow",
            p("Extract and visualize the spatial redistribution of expected fishing effort across 
              your policy simulations."),
            
            fluidRow(class = "mt-2",
                     column(6,
                            div(class = "d-flex justify-content-between align-items-end",
                                tags$label("Filter by Model(s):",
                                           style = "margin-bottom: 2px; font-weight: bold;"),
                                actionLink(ns("select_all_models"),
                                           "Select All",
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
                                actionLink(ns("select_all_scenarios"),
                                           "Select All", 
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
            spinner_ui(ns("run_summary_spinner"), 
                       spinner_type = "circle", 
                       message = "Processing spatial data and generating plots...",
                       overlay = TRUE)
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
                  
                  # Primary Viewer (Always Visible)
                  create_viewer_ui(ns, 1),
                  
                  # Hidden Viewers 2-4
                  shinyjs::hidden(div(id = ns("viewer_container_2"), create_viewer_ui(ns, 2))),
                  shinyjs::hidden(div(id = ns("viewer_container_3"), create_viewer_ui(ns, 3))),
                  shinyjs::hidden(div(id = ns("viewer_container_4"), create_viewer_ui(ns, 4))),
                  
                  # Action Controls Container
                  div(class = "d-flex align-items-center mt-4 mb-2", style = "gap: 15px;",
                      actionButton(ns("add_viewer_btn"), "Add Comparison Plot", 
                                   icon = icon("plus"), class = "btn-outline-primary"),
                      shinyjs::hidden(
                        actionButton(ns("clear_viewers_btn"), "Clear Comparisons", 
                                     icon = icon("eraser"), class = "btn-outline-danger")
                      ),
                      div(style = "margin-left: auto;",
                          actionButton(ns("view_static_btn"), "View & Save Static Plots", 
                                       icon = icon("image"), class = "btn-info")
                      )
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