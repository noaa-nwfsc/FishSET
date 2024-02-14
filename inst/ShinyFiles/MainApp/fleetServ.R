source("fleetUI.R", local = TRUE)
source("fleet_helpers.R", local = TRUE)
  

# r expression module
RexpressionServ <- function(id, values) {
  
  moduleServer(id, function(input, output, session) {
    
    r <- reactiveValues(done = 0, ok = TRUE, output = "")
    
    observeEvent(input$run, {
      shinyjs::hide("error")
      r$ok <- FALSE
      tryCatch(
        {
          r$output <- isolate(
            paste(utils::capture.output(eval(parse(text = input$expr))), collapse = '\n')
          )
          r$ok <- TRUE
        },
        error = function(err) {r$output <- err$message}
      )
      r$done <- r$done + 1
    })
    
    exp_out <- reactive({
      
      if (r$done > 0 ) { 
        
        content <- paste(paste(">", isolate(input$expr)), r$output, sep = '\n')
        
        if (r$ok) {
          
          pre(content)
          
        } else {
          pre( style = "color: red; font-weight: bold;", content)
        }
      }
    })
    
    output$result <- renderUI({exp_out()})
  })
}


# Save buttons ====
sort_save_outputs <- function(x = NULL) {
  
  # used in tabPlotServ()
  if (!is.null(x)) {
    
    # detect single plot/table v. list of plots/tables, 
    # e.g. output = "tab_plot" for fleet functions
    ind <- vapply(x, rlang::is_bare_list, logical(1))
    
    if (any(ind)) { # list(s) present
      
      out <- unlist(x[ind], recursive = FALSE) 
      
      if (any(!ind)) { # single plots/tables present?
        
        out <- append(out, x[!ind]) # merge into one list
      }
      
      out 
      
    } else x
  }
}


# used w/ tabPlotServ
add_names <- function(out, output = "names", type) {
  # adds missing names to the output save menu
  p_nm <- names(out)
  nm_missing <- FALSE
  out_lab <- switch(type, "plot" = "plot ", "table" = "table ", "mod" = "mod ")
  
  if (is.null(p_nm)) {
    
    p_nm <- paste0(out_lab, seq_along(out))
    names(out) <- p_nm
    nm_missing <- TRUE
    
  } else if (any(stringi::stri_isempty(names(out)))) {
    
    p_empty <- which(stringi::stri_isempty(names(out)))
    p_nm <- paste0(out_lab, p_empty)
    
    names(out)[p_empty] <- p_nm
    nm_missing <- TRUE
  }
  
  if (output == "names") names(out)
  else if (output == "list") out
  else if (output == "logical") nm_missing
}


plotSaveServ <- function(id, proj, plots = NULL) { 
  
  moduleServer(id, function(input, output, session) {
  
    ns <- session$ns
    
    plot_r <- reactiveValues(names = NULL, out = NULL)
    
    plots_r <- eventReactive(input$save_plot, {
     
      sort_save_outputs(plots())
      })
    
    in_to_px <- function(x) x * 96 # convert inches to pixels
    
    r_plot_size_w <- reactive(in_to_px(input$plot_size_w))
    r_plot_size_h <- reactive(in_to_px(input$plot_size_h))
    
    r_plot_set_w <- reactive(get_proj_settings(proj())$plot_size[1]) # default plot size
    r_plot_set_h <- reactive(get_proj_settings(proj())$plot_size[2])
    
    r_plot_set_w_px <- reactive(in_to_px(r_plot_set_w())) # passed to plot output
    r_plot_set_h_px <- reactive(in_to_px(r_plot_set_h()))
    
    show_save_modal <- function() {

      showModal(
        modalDialog(title = "Select plot to save",
                    
            tabsetPanel(id = "plot_tabset",
                tabPanel("Select Plot",
                         radioButtons(ns("select_plot"), "Plots", 
                                      choices = plot_r$names),
                         shinycssloaders::withSpinner(
                           plotOutput(ns("display_plot"), width = "auto", height = "auto"), type = 6)
                         ),
                tabPanel("Plot Size",
                         sliderInput(ns("plot_size_w"), "Width (in)", min = 1, max = 20, 
                                     value = r_plot_set_w(), step = .1),
                         sliderInput(ns("plot_size_h"), "Height (in)", min = 1, max = 20, 
                                     value = r_plot_set_h(), step = .1),
                         plotOutput(ns("plot_size"), width = "auto", height = "auto"))
                ),
                   
            footer = tagList(
              modalButton("Close"),
              actionButton(ns("downloadPlot"), "Save",
                           style = "color: white; background-color: blue;")),
            easyClose = FALSE, size = "xl"))
    }
    
    output$plot_size <- renderPlot({
      
      p_out <- plots_r()[[input$select_plot]]
      
      if ("gtable" %in% class(p_out)) gridExtra::grid.arrange(p_out)
      else p_out
      
    }, width = r_plot_size_w, height = r_plot_size_h)
    
    
    output$display_plot <- renderPlot({
      
      p_out <- plots_r()[[input$select_plot]]
      
      if ("gtable" %in% class(p_out)) gridExtra::grid.arrange(p_out)
      else p_out
    }, width = r_plot_set_w_px, height = r_plot_set_h_px)
    
    is_plot <- function(x) ggplot2::is.ggplot(x) || grid::is.grob(x)
    
    # detect plots ----
    observeEvent(input$save_plot, {
      
      # Need to check whether plots_r() exists and let code execution continue if it doesn't exist
      tryCatch({
        plots_r <- plots_r()
      },
      shiny.silent.error = function(e){
      })
      
      plot_ind <- vapply(plots_r, is_plot, logical(1))
      
      if (sum(plot_ind) > 0) {
        
        plot_r$names <- add_names(plots_r[plot_ind], "names", "plot") # for selecting plots
        show_save_modal()
        
      } else {
        
        showNotification("No plots found.", type = "message")
      }
    })
    
    # save plots ----
    
    observeEvent(input$downloadPlot, {
      
      plot_r$out <- plots_r()[[input$select_plot]]
      
      output$downloadPlotHIDE <<- downloadHandler(
        
        filename = function() {
          
          paste0(locoutput(proj()), proj(), "_", input$select_plot, ".png")
        },
        content = function(file) {
          ggplot2::ggsave(file, plot = plot_r$out,
                          width = input$plot_size_w, height = input$plot_size_h)
        })
      
      jsinject <-  paste0("setTimeout(function(){window.open($('#", ns("downloadPlotHIDE"), 
                          "').attr('href'))}, 100);")
      session$sendCustomMessage(type = 'jsCode', list(value = jsinject))
      
      showNotification('Plot saved.', type = 'message', duration = 10)
      
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    
  })
}


tableSaveServ <- function(id, proj, tabs = NULL) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # track table names and outputs
    tab_r <- reactiveValues(names = NULL, out = NULL)
    
    # simplify list of reactive expressions/values into one list
    tabs_r <- reactive(sort_save_outputs(tabs()))
    
    show_save_modal <- function() {
      
      showModal(
        modalDialog(title = "Select table to save",
                    
                    radioButtons(ns("select_tab"), "Tables", choices = tab_r$names),
                    
                    tags$div(
                      shinycssloaders::withSpinner(
                        DT::DTOutput(ns("display_table")), type = 6),
                      style = "font-size: 75%; width: 100%"),
                    
                    footer = tagList(
                      modalButton("Close"),
                      
                      actionButton(ns("downloadTable"), "Save",
                                   style = "color: white; background-color: blue;")
                    ),
                    easyClose = FALSE, size = "l"))
    }
    
    output$display_table <- DT::renderDT(tabs_r()[[input$select_tab]])
    
    is_tab <- function(x) is.data.frame(x) || is.matrix(x) || is.table(x)
    
    # detect savable tables ----
    observeEvent(input$save_table, {
      
      # Need to check whether tabs_r() exists and let code execution continue if it doesn't exist
      tryCatch({
        tabs_r <- tabs_r()
      },
      shiny.silent.error = function(e){
      })
      
      tab_ind <- vapply(tabs_r, is_tab, logical(1))
      
      if(sum(tab_ind) > 0) {
        tab_r$names <- add_names(tabs_r[tab_ind], "names", "tab") # for selecting plots
        show_save_modal()
        
      } else {
        showNotification("No tables found.", type = "message")

      }
    })
    
    # save table window ----
    observeEvent(input$downloadTable, {
        
        tab_r$out <- tabs_r()[[input$select_tab]]
        
        output$downloadTableHIDE <<- downloadHandler(
          
          filename = function() {
            
            paste0(locoutput(proj()), proj(), "_", input$select_tab, ".csv")
          },
          content = function(file) {
            write.csv(tab_r$out, file, row.names = TRUE)
          })
        
        jsinject <-  paste0("setTimeout(function(){window.open($('#", ns("downloadTableHIDE"),
                            "').attr('href'))}, 100);")
        session$sendCustomMessage(type = 'jsCode', list(value = jsinject))
        
        showNotification('Table saved.', type = 'message', duration = 10)
      # }
      
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    
  })
}


# direct output to tab/plot modules 
tabPlotServ <- function(id, proj, out = NULL, type = "tab_plot") { 
    
  tab_id <- paste0(id, "_tab")
  plot_id <- paste0(id, "_plot")
  
  if (type == "tab_plot") {
    
    tableSaveServ(tab_id, proj, tabs = out)
    plotSaveServ(plot_id, proj, plots = out)
    
    
  } else if (type == "table") {
    
    tableSaveServ(tab_id, proj, tabs = out)
    
  } else {
    
    plotSaveServ(plot_id, proj, plots = out)
  }
}

# saving model summary output
modSaveServ <- function(id, proj, outs = NULL) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # track mod names and outputs
    mod_r <- reactiveValues(names = NULL, out = NULL)
    
    show_save_modal <- function() {
      
      showModal(
        modalDialog(title = "Select model summary to save",
                    
                    radioButtons(ns("select_mod"), "Summaries", choices = mod_r$names),
                    
                    tags$div(
                      shinycssloaders::withSpinner(
                        verbatimTextOutput(ns("display_mod")), type = 6),
                      style = "font-size: 75%; width: 100%"),
                    
                    footer = tagList(
                      modalButton("Close"),
                      
                      actionButton(ns("downloadMod"), "Save",
                                   style = "color: white; background-color: blue;")
                    ),
                    easyClose = FALSE, size = "l"))
    }
    
    output$display_mod <- renderPrint(outs()[[input$select_mod]])
    
    is_mod <- function(x) any(class(x) %in% c("summary.lm", "htest")) # expand 
    
    # detect savable mod summaries ----
    observeEvent(input$save_mod, {
      
      outs <- outs()
      mod_ind <- vapply(outs, is_mod, logical(1))
      
      if (sum(mod_ind) > 0) {
        
        mod_r$names <- add_names(outs[mod_ind], "names", "mod") 
        show_save_modal()
        
      } else {
        
        showNotification("No tables found.", type = "message")
      }
    })
    
    # save mod window ----
    observeEvent(input$downloadMod, {

      mod_r$out <- outs()[[input$select_mod]]
      
      output$downloadModHIDE <<- downloadHandler(
        
        filename = function() {
          
          paste0(locoutput(proj()), proj(), "_", mod_r$selected, ".txt")
        },
        content = function(file) {
          sink(file)
          print(mod_r$out)
          sink()
        })
      
      jsinject <-  paste0("setTimeout(function(){window.open($('#", ns("-downloadModHIDE"),
                          "').attr('href'))}, 100);")
      session$sendCustomMessage(type = 'jsCode', list(value = jsinject))
      showNotification('Model summary saved.', type = 'message', duration = 10)

    }, ignoreInit = TRUE, ignoreNULL = TRUE)

  })
}


saveDataTableServ <- function(id, values, project) {
  
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$saveData, {
      suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project())))
      DBI::dbWriteTable(fishset_db, paste0(project(), 'MainDataTable'), values$dataset, overwrite = TRUE)
      DBI::dbDisconnect(fishset_db)
      showNotification('Data saved to FishSET database', type = 'message', duration = 10)
    })
  })
}

refreshServ <- function(id, values, project) {
  
  moduleServer(id, function(input, output, session) {
    # TODO: simplify/clean this
    observeEvent(input$refresh, {
      req(project)
      tmp_tabs <- tables_database(project())[grep(paste0(project(), 'MainDataTable\\d+'), tables_database(project()))]
      tab_dates1 <- unlist(stringi::stri_extract_all_regex(tmp_tabs, "\\d{6,}")) # all dates following MainDataTable
      tab_dates2 <- max(tab_dates1) # max date
      tmp_tabs <- tmp_tabs[which(tab_dates1 == tab_dates2)] # get the latest table
      
      values$dataset <- table_view(tmp_tabs, project())
      showNotification("Data refreshed", type = 'message', duration = 10)
    }, ignoreInit = TRUE, ignoreNULL = TRUE) 
  })
}


closeAppServ <-  function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$close, { stopApp() })
  })
}

mergeServer <- function(id, main, other, project, merge_type, dat_type, show = NULL) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    dat_type_lab <- switch(dat_type, "aux" = "Auxiliary", "port" = "Port", 
                       "grid" = "Gridded", "spat" = "Spatial")
    
    output$mergeUI <- renderUI({
      
      tagList(
        conditionalPanel("input.merge_cb", ns = ns, 
                         
         fluidRow(
           column(3,
                  selectInput(ns("main_key"), "Main table keys",
                              choices = colnames(main$dataset), multiple = TRUE)),
           column(3, 
                  selectInput(ns("other_key"), paste(dat_type_lab, "table keys"),
                              choices = colnames(other$dataset), multiple = TRUE))),
         fluidRow(
           column(8,
                  verbatimTextOutput(ns("mergeBy")))),
         fluidRow(
           column(3,
                  actionButton(ns("mergeOK"), "Merge", 
                               style = "background-color: blue; color: #FFFFFF;"))),
         tags$br(), tags$br()) 
      )
      
    })
    
    # show keys in app
    show_merge_by <- reactive({
      req(input$main_key, input$other_key)
      
      unlist(purrr::pmap(list(m = input$main_key, o = input$other_key), 
                         function(m, o) {
                           paste0(m, " = ", o, "\n")
                         }))
      
    })
    
    output$mergeBy <- renderText(show_merge_by())
    
    merge_by <- reactive({
      req(input$main_key, input$other_key)
      if (length(input$main_key) != length(input$other_key)) {
        
        showNotification("Key lengths must match.", type = "error")
        return()
      } else {
        
        stats::setNames(input$other_key, c(input$main_key))
      }
    })
    
    observeEvent(input$mergeOK, {
      
      if (!is.null(merge_by())) {
        
        main$dataset <- merge_dat(main$dataset, other$dataset, project = project(),  
                                  input$main_key, input$other_key, dat_type, merge_type)
        
        if (!is.null(show$save)) {
          show$save <- TRUE
        }
        
        showNotification(paste(dat_type_lab, "table merged to primary table."), 
                         type = "message")
      }
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    
    # reset keys if merge box is unchecked
    observeEvent(input$merge_cb == FALSE, {
      
      updateSelectInput(session, "main_key", choices = colnames(main$dataset))
      updateSelectInput(session, "other_key", choices = colnames(other$dataset))
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    
  })
}


# Density Plot ====

density_serv <- function(id, values, project) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    tabPlotServ("save", proj = project,
                out = reactive(list(density_plot = den_out())), type = "tab_plot")
    
    tabPlotServ("save-summary", proj = project,
                out = reactive(list(density_plot = den_out())), type = "tab_plot")
    
    output$var_select <- renderUI({
      
      selectInput(ns("var"), "Select variable",
                  choices = c(FishSET:::numeric_cols(values$dataset)))
    })
    
    output$grp_select <- renderUI({
      
      selectizeInput(ns("grp"), "Group by",
                     choices = c("year", "month", "week", category_cols(values$dataset)),
                     multiple = TRUE)
    })
    
    output$grp_date_UI <- renderUI({
      
      if (!is.null(input$grp)) {
        if (any(input$grp %in% c("year", "month", "week"))) {
          if(is.null(input$date) & is.null(input$fct_date)) {
            
            selectizeInput(ns("grp_date"), "Date variable",
                           choices = c(date_cols(values$dataset)),
                           multiple = TRUE, options = list(maxItems = 1))
          }
        }
      }
    })
    
    output$date_select <- renderUI({
      
      selectizeInput(ns("date"), "Date variable",
                     choices = date_cols(values$dataset),
                     multiple = TRUE, options = list(maxItems = 1))
    })
    
    output$fct_select <- renderUI({
      
      selectizeInput(ns("fct"), "Split plot by",
                     choices = c("year", "month", "week", category_cols(values$dataset)),
                     selected = fleet_col(category_cols(values$dataset)),
                     multiple = TRUE, options = list(maxItems = 2))
    })
    
    output$fct_date_UI <- renderUI({
      
      if (!is.null(input$fct)) {
        if (any(input$fct %in% c("year", "month", "week"))) {
          if (is.null(input$date) & is.null(input$grp_date)) {
            
            selectizeInput(ns("fct_date"), "Date variable",
                           choices = c(date_cols(values$dataset)),
                           multiple = TRUE, options = list(maxItems = 1))
          }
        }
      }
    })
    
    filter_val <- reactive({
      
      if (is.null(input$filter_by)) {
        
        NULL
        
      } else {
        
        out <- unique(values$dataset[[input$filter_by]])
        
        if (length(out) > 50)  out <- out[1:50]
        
        out
      }
    })
    
    output$filter_by_UIOutput <- renderUI({
      
      selectizeInput(ns("filter_by"), "Select variable",
                     choices = names(values$dataset), multiple = TRUE, options = list(maxItems = 1)) 
    })
    
    output$filter_by_val_UIOutput <- renderUI({
      
      tagList(  
        conditionalPanel("typeof input.filter_by !== 'undefined' && input.filter_by.length > 0", ns = ns,
                         style = "margin-left:19px;",
                         
                         selectizeInput(ns("filter_by_val"), "Select values",
                                        choices = filter_val(),
                                        multiple = TRUE, 
                                        options = list(maxOptions = 15, 
                                                       placeholder = "Select or type value name",
                                                       create = TRUE)))
      )
    })
    
    output$filter_date_UIOutput <- renderUI({
      
      if (!is.null(input$date)) {
        if (!is.null(input$filter_date)) {
          if (input$filter_date == "date_range") {
            
            dateRangeInput(ns("date_range"), label = "Date range",
                           start = min(values$dataset[[input$date]], na.rm = TRUE),
                           end = max(values$dataset[[input$date]], na.rm = TRUE))
            
          } else if (input$filter_date != "date_range") {
            
            filter_periodUI(id, values$dataset, input$date, input$filter_date)
          }
        }
      }
    })
    
    date_value <- reactive({
      
      if (!is.null(input$date) & !is.null(input$filter_date)) {
        
        if (input$filter_date == "date_range") {
          
          return(input$date_range)
          
        } else if (input$filter_date != "date_range") {
          
          filter_periodOut(id, input$filter_date, input)
        }
        
      } else {
        
        NULL
      }
    })
    
    # date col for subset, group, and split sections
    date_sgs <- reactive({
      
      cols <- c(input$date, input$grp_date, input$fct_date)
      if (!is.null(cols)) cols
      else NULL
    }) 
    
    # reset if subset is unchecked
    observeEvent(input$subset_cb == FALSE, {
      
      updateCheckboxInput(session, "date_subset_cb", value = FALSE)
      updateCheckboxInput(session, "var_subset_cb", value = FALSE)
    })
    
    observeEvent(input$date_subset_cb == FALSE, {
      
      updateSelectizeInput(session, "date", choices = date_cols(values$dataset),
                           options = list(maxItems = 1), selected = NULL)
      
      updateSelectizeInput(session, "filter_date",
                           choices = c("date range" = "date_range", "year-month",
                                       "year-week", "year-day", "year", "month", "week", "day"),
                           options = list(maxItems = 1), selected = NULL)
    })
    
    observeEvent(input$var_subset_cb == FALSE, {
      
      updateSelectizeInput(session, "filter_by", choices = names(values$dataset),
                           options = list(maxItems = 1), selected = NULL)
    })
    
    # reset if group is unchecked
    observeEvent(input$group_cb == FALSE, {
      
      updateSelectizeInput(session, "grp", choices = c("year", "month", "week", 
                                                       category_cols(values$dataset)), 
                           options = list(create = TRUE))
      
      updateSelectizeInput(session, "grp_date", choices = date_cols(values$dataset),
                           options = list(maxItems = 1, create = TRUE, 
                                          placeholer = "select or type column name"),
                           selected = NULL)
    })
    
    # reset if split is unchecked
    observeEvent(input$split_cb == FALSE, {
      
      updateSelectizeInput(session, "fct", choices = c("year", "month", "week", 
                                                       category_cols(values$dataset)),
                           selected = NULL, 
                           options = list(maxItems = 2))
      
      updateSelectizeInput(session, "fct_date", choices = date_cols(values$dataset),
                           options = list(maxItems = 1), selected = NULL)
    })
    
    den_out <- eventReactive(input$fun_run, {
      
      validate_date(sub_date = date_sgs(), filter_date = input$filter_date, 
                    fct = input$fct, grp = input$grp)
      
      q_test <- quietly_test(density_plot)
      q_test(values$dataset, project = project(), var = input$var, type = input$type, 
             group = input$grp, combine = input$combine, date = date_sgs(), 
             filter_date = input$filter_date, date_value = date_value(), 
             filter_by = input$filter_by, filter_value = input$filter_by_val, 
             facet_by = input$fct, scale = input$scale, conv = input$conv,
             tran = input$tran, format_lab = input$f_lab, bw = input$bw, 
             position = input$position, pages = input$pages) 
    })
    
    
    plot_output <- eventReactive(input$fun_run, {
      n_plot_output(den_out())
    })
    
    output$output <- renderUI(plot_output())
  })
}


# Vessel Count ====

vessel_serv <- function(id, values, project) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    tabPlotServ("save", proj = project,
                out = reactive(list(vessel_count = v_out(), test=NULL)), type = "tab_plot")
    
    tabPlotServ("save-summary", proj = project,
                out = reactive(list(vessel_count = v_out(), test=NULL)), type = "tab_plot")
    
    
    output$var_select <- renderUI({
      
      selectizeInput(ns("var"), "Vessel identifier", choices = colnames(values$dataset), multiple = FALSE)
    })
    
    output$date_select <- renderUI({
      
      if (input$period != "no_period") {
        
        selectizeInput(ns("date"), "Date variable",
                       choices = FishSET:::date_cols(values$dataset), multiple = TRUE,
                       options = list(maxItems = 1, create = TRUE,
                                      placeholder = "Select or type variable name"))
      }
    
    })
    
    output$sub_date_select <- renderUI({
      
      selectizeInput(ns("sub_date"), "Date variable", 
                     choices = FishSET:::date_cols(values$dataset), multiple = TRUE,
                     options = list(maxItems = 1, create = TRUE,
                                    placeholder = "Select or type variable name"))
    })
    
    output$grp_select <- renderUI({
      
      selectizeInput(ns("grp"), "Group by", 
                     choices = c("year", "month", "week", category_cols(values$dataset)), 
                     multiple = TRUE, options = list(create = TRUE, 
                                                     placeholder = "Select or type variable name"))
    })
    
    output$grp_date_UI <- renderUI({
      
      if (!is.null(input$grp)) {
        if (any(input$grp %in% c("year", "month", "week"))) {
          if(is.null(input$sub_date) & is.null(input$fct_date)) {
            
            selectizeInput(ns("grp_date"), "Date variable",
                           choices = FishSET:::date_cols(values$dataset),
                           multiple = TRUE, options = list(maxItems = 1, create = TRUE, 
                                                           placeholder = "Select or type variable name"))
          }
        }
      }
    })
    
    output$fct_select <- renderUI({
      
      selectizeInput(ns("fct"), "Split plot by", 
                     choices = c("year", "month", "week", category_cols(values$dataset)),
                     selected = fleet_col(category_cols(values$dataset)),
                     multiple = TRUE, options = list(maxItems = 2, create = TRUE, 
                                                     placeholder = "Select or type variable name"))
    })
    
    output$fct_date_UI <- renderUI({
      
      if (!is.null(input$fct)) {
        if (any(input$fct %in% c("year", "month", "week"))) {
          if (is.null(input$sub_date) & is.null(input$grp_date)) {
            
            selectizeInput(ns("fct_date"), "Date variable",
                           choices = FishSET:::date_cols(values$dataset),
                           multiple = TRUE, options = list(maxItems = 1, create = TRUE, 
                                                           placeholder = "Select or type variable name"))
          }
        }
      }
    })
    
    filter_val <- reactive({
      
      if (is.null(input$filter_by)) NULL
        
      else {
        
        out <- unique(values$dataset[[input$filter_by]])
        
        if (length(out) > 50)  out <- out[1:50]
        
        out
      }
    })
    
    output$filter_by_UIOutput <- renderUI({
      
      selectizeInput(ns("filter_by"), "Select variable",
                     choices = names(values$dataset), multiple = TRUE, options = list(maxItems = 1)) 
    })
    
    output$filter_by_val_UIOutput <- renderUI({
      
      tagList(  
        conditionalPanel("typeof input.filter_by !== 'undefined' && input.filter_by.length > 0", ns = ns,
                         style = "margin-left:19px;",
                         
                         selectizeInput(ns("filter_by_val"), "Select value(s)",
                                        choices = filter_val(),
                                        multiple = TRUE, 
                                        options = list(maxOptions = 15, 
                                                       placeholder = "Select or type value name",
                                                       create = TRUE)))
      )
    })
    
    output$filter_date_UIOutput <- renderUI({
      
        if (!is.null(input$sub_date)) {
          if (!is.null(input$filter_date)) {
            if (input$filter_date == "date_range") {
              
              dateRangeInput(ns("date_range"), label = "Date range",
                             start = min(values$dataset[[input$sub_date]], na.rm = TRUE),
                             end = max(values$dataset[[input$sub_date]], na.rm = TRUE))
              
            } else if (input$filter_date != "date_range") {
              
              filter_periodUI(id, values$dataset, input$sub_date, input$filter_date)
            }
          }
        }
    })
    
    # reset date if not summing over period
    observeEvent(req(input$period == "no_period"), {
        cat("\nno_period selected")
        updateSelectizeInput(session, "date", choices = FishSET:::date_cols(values$dataset))
    
    })
    
    # reset if subset is unchecked
    observeEvent(req(input$subset_cb == FALSE), {
      
      updateCheckboxInput(session, "date_subset_cb", value = FALSE)
      updateCheckboxInput(session, "var_subset_cb", value = FALSE)
    })
    
    observeEvent(req(input$date_subset_cb == FALSE), {
      
      updateSelectizeInput(session, "sub_date", choices = FishSET:::date_cols(values$dataset))
      
      updateSelectizeInput(session, "filter_date",
                           choices = c("date range" = "date_range", "year-month",
                                       "year-week", "year-day", "year", "month", "week", "day"),
                          selected = NULL)
    })
    
    observeEvent(req(input$var_subset_cb == FALSE), {
      
      updateSelectizeInput(session, "filter_by", choices = names(values$dataset),
                           selected = NULL)
    })
    
    # reset if group is unchecked
    observeEvent(req(input$group_cb == FALSE), {
      
      updateSelectizeInput(session, "grp", 
                           choices = c("year", "month", "week", category_cols(values$dataset)))
      
      updateSelectizeInput(session, "grp_date", choices = FishSET:::date_cols(values$dataset))
      
      updateCheckboxInput(session, "combine", value = FALSE)
    })
    
    # reset if split is unchecked
    observeEvent(req(input$split_cb == FALSE), {
      
      updateSelectizeInput(session, "fct", choices = c("year", "month", "week", 
                                                       category_cols(values$dataset)))
      
      updateSelectizeInput(session, "fct_date", choices = FishSET:::date_cols(values$dataset))
    })
    
    date_value <- reactive({
      
      if (!is.null(input$sub_date) & !is.null(input$filter_date)) {
        
        if (input$filter_date == "date_range") {
          
          return(input$date_range)
          
        } else if (input$filter_date != "date_range") {
          
          filter_periodOut(id, input$filter_date, input)
        }
        
      } else NULL
    })
    
    # date col for subset, group, and split sections
    sub_date_col <- reactive({
      
      cols <- c(input$sub_date, input$grp_date, input$fct_date)
      if (!is.null(cols)) cols
      else NULL
    }) 
    
    v_out <- eventReactive(input$fun_run, {
      
      validate_date(date = input$date, sub_date = sub_date_col(), period = input$period,
                    filter_date = input$filter_date, fct = input$fct, grp = input$grp)
      
      q_test <- quietly_test(vessel_count)
      q_test(values$dataset, project = project(), v_id = input$var, date = input$date,
             period = input$period, group = input$grp, sub_date = sub_date_col(),
             filter_date = input$filter_date, date_value = date_value(), 
             filter_by = input$filter_by, filter_value = input$filter_by_val, 
             filter_expr = input$filter_expr, facet_by = input$fct, combine = input$combine,
             position = input$position, tran = input$tran, format_lab = input$f_lab,
             value = input$value, scale = input$scale, type = input$type, 
             output = input$out)
    })
    
    tabplot <- eventReactive(input$fun_run, {
      
      tabplot_output(v_out(), input$out)
    })
    
    output$output <- renderUI({tabplot()})
  })
}

# Species Catch ====

species_serv <- function(id, values, project) {
  
  moduleServer(id, function(input, output, session) {
    # 
    ns <- session$ns
    
    tabPlotServ("save", proj = project,
                out = reactive(list(species_catch = spec_out())), type = "tab_plot")
    
    tabPlotServ("save-summary", proj = project,
                out = reactive(list(species_catch = spec_out())), type = "tab_plot")
    
    output$var_select <- renderUI({
      
      selectizeInput(ns("var"), "Catch variable",
                     choices = c(FishSET:::numeric_cols(values$dataset)), multiple = TRUE)
    })
    
    output$date_select <- renderUI({
      
       selectizeInput(ns("date"), "Date variable",
                    choices = c(FishSET:::date_cols(values$dataset)), multiple = TRUE,
                    options = list(maxItems = 1, create = TRUE,
                                   placeholder = "Select or type variable name"))
    })
    
    output$sub_date_select <- renderUI({
      
      selectizeInput(ns("sub_date"), "Date variable", 
                     choices = FishSET:::date_cols(values$dataset), multiple = TRUE,
                     options = list(maxItems = 1, create = TRUE,
                                    placeholder = "Select or type variable name"))
    })
    
    output$grp_select <- renderUI({
      
      selectizeInput(ns("grp"), "Group by",
                     choices = c("year", "month", "week", category_cols(values$dataset)), 
                     multiple = TRUE, options = list(create = TRUE, 
                                                     placeholder = "Select or type variable name"))
    })
    
    output$grp_date_UI <- renderUI({
      
      if (!is.null(input$grp)) {
        if (any(input$grp %in% c("year", "month", "week"))) {
          if(is.null(input$sub_date) & is.null(input$fct_date)) {
            
            selectizeInput(ns("grp_date"), "Date variable",
                           choices = FishSET:::date_cols(values$dataset),
                           multiple = TRUE, options = list(maxItems = 1, create = TRUE, 
                                                           placeholder = "Select or type variable name"))
          }
        }
      }
    })
    
    output$fct_select <- renderUI({
      
      selectizeInput(ns("fct"), "split plot by",
                     choices = c("year", "month", "week", "species", category_cols(values$dataset)),
                     selected = fleet_col(category_cols(values$dataset)),
                     multiple = TRUE, options = list(maxItems = 2, create = TRUE, 
                                                     placeholder = "Select or type variable name"))
    })
    
    output$fct_date_UI <- renderUI({
      
      if (!is.null(input$fct)) {
        if (any(input$fct %in% c("year", "month", "week"))) {
          if (is.null(input$sub_date) & is.null(input$grp_date)) {
            
            selectizeInput(ns("fct_date"), "Date variable",
                           choices = FishSET:::date_cols(values$dataset),
                           multiple = TRUE, options = list(maxItems = 1, create = TRUE, 
                                                           placeholder = "Select or type variable name"))
          }
        }
      }
    })
    
    filter_val <- reactive({

      if (is.null(input$filter_by)) NULL

      else {
        
        out <- unique(values$dataset[[input$filter_by]])
        
        if (length(out) > 50)  out <- out[1:50]

        out
      }
    })
    
    output$filter_by_UIOutput <- renderUI({
     
        selectizeInput(ns("filter_by"), "Select variable",
        choices = names(values$dataset), multiple = TRUE, options = list(maxItems = 1)) 
    })
    
    output$filter_by_val_UIOutput <- renderUI({
    
      tagList(  
        conditionalPanel("typeof input.filter_by !== 'undefined' && input.filter_by.length > 0", ns = ns,
                         style = "margin-left:19px;",
        
        selectizeInput(ns("filter_by_val"), "Select values",
                    choices = filter_val(),
                    multiple = TRUE, options = list(maxOptions = 15, 
                                                    placeholder = "Select or type value name",
                                                    create = TRUE)))
      )
    })
    
    output$filter_date_UIOutput <- renderUI({
     
      
       if (!is.null(input$sub_date)) {
        if (!is.null(input$filter_date)) {
          if (input$filter_date == "date_range") {

            dateRangeInput(ns("date_range"), label = "Date range",
                           start = min(values$dataset[[input$sub_date]], na.rm = TRUE),
                           end = max(values$dataset[[input$sub_date]], na.rm = TRUE))

          } else if (input$filter_date != "date_range") {

            filter_periodUI(id, values$dataset, input$sub_date, input$filter_date)
          }
        }
       }
    })
    
    # reset date if not summing over period
    observeEvent(input$period == "no_period", {
      
      updateSelectInput(session, "date",  choices = FishSET:::date_cols(values$dataset))
    })
    
    # reset if subset is unchecked
    observeEvent(input$subset_cb == FALSE, {
      
      updateCheckboxInput(session, "date_subset_cb", value = FALSE)
      updateCheckboxInput(session, "var_subset_cb", value = FALSE)
    })
    
    observeEvent(input$date_subset_cb == FALSE, {
      
      updateSelectizeInput(session, "sub_date", choices = FishSET:::date_cols(values$dataset), 
                           options = list(maxItems = 1, create = TRUE,
                                          placeholder = "Select or type variable name"))
      
      updateSelectizeInput(session, "filter_date",
                           choices = c("date range" = "date_range", "year-month",
                                       "year-week", "year-day", "year", "month", "week", "day"),
                           options = list(maxItems = 1), selected = NULL)
    })
    
    observeEvent(input$var_subset_cb == FALSE, {
      
      updateSelectizeInput(session, "filter_by", choices = names(values$dataset),
                           options = list(maxItems = 1), selected = NULL)
    })
    
    # reset if group is unchecked
    observeEvent(input$group_cb == FALSE, {
      
      updateSelectizeInput(session, "grp", 
                           choices = c("year", "month", "week", category_cols(values$dataset)), 
                           options = list(create = TRUE, placeholder = "Select or type variable name"))
      
      updateSelectizeInput(session, "grp_date", choices = FishSET:::date_cols(values$dataset),
                           options = list(maxItems = 1, create = TRUE,
                                          placeholder = "Select or type variable name"))
      
      updateCheckboxInput(session, "combine", value = FALSE)
    })
    
    # reset if split is unchecked
    observeEvent(input$split_cb == FALSE, {
      
      updateSelectizeInput(session, "fct", choices = c("year", "month", "week", "species",
                                                       category_cols(values$dataset)),
                           selected = NULL,
                           options = list(maxItems = 2))
      
      updateSelectizeInput(session, "fct_date", choices = FishSET:::date_cols(values$dataset),
                           options = list(maxItems = 1, create = TRUE, 
                                          placeholder = "Select or type variable name"))
    })
    
    date_value <- reactive({
      
      if (!is.null(input$sub_date) & !is.null(input$filter_date)) {
      
      if (input$filter_date == "date_range") {
        
        return(input$date_range)
        
      } else if (input$filter_date != "date_range") {
        
        filter_periodOut(id, input$filter_date, input)
      }
        
      } else NULL
    })
    
    # date col for subset, group, and split sections
    sub_date_col <- reactive({
      
      cols <- c(input$sub_date, input$grp_date, input$fct_date)
      if (!is.null(cols)) cols
      else NULL
    }) 
    
    spec_out <- eventReactive(input$fun_run, {
      
      validate(need(input$var, "Please select a catch variable."))
      
      validate_date(date = input$date, sub_date = sub_date_col(), period = input$period, 
                    filter_date = input$filter_date, fct = input$fct, grp = input$grp)
      
      q_test <- quietly_test(species_catch)
      q_test(values$dataset, project = project(), species = input$var, date = input$date,
             period = input$period, fun = input$fun, group = input$grp, sub_date = sub_date_col(),
             filter_date = input$filter_date, date_value = date_value(),
             filter_by = input$filter_by, filter_value = input$filter_by_val, 
             filter_expr = input$filter_expr, facet_by = input$fct, combine = input$combine,
             position = input$position, conv = input$conv, tran = input$tran, 
             format_lab = input$f_lab, value = input$value, scale = input$scale, 
             type = input$type, output = input$out, format_tab = input$format)
    })
    
    tabplot <- eventReactive(input$fun_run, {

      tabplot_output(spec_out(), input$out)
    })
    
    output$output <- renderUI({tabplot()})
  })
}

# Rolling Catch ====

roll_serv <- function(id, values, project) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    tabPlotServ("save", proj = project,
                out = reactive(list(roll_catch = roll_out())), type = "tab_plot")
    
    tabPlotServ("save-summary", proj = project,
                out = reactive(list(roll_catch = roll_out())), type = "tab_plot")
    
    output$var_select <- renderUI({
      
      selectizeInput(ns("var"), "Select catch variable",
                     choices = FishSET:::numeric_cols(values$dataset), multiple = TRUE)
    })
    
    output$date_select <- renderUI({
      
      selectInput(ns("date"), "Date variable (x-axis)",
                  choices = FishSET:::date_cols(values$dataset))
    })
    
    output$sub_date_select <- renderUI({
      
      selectizeInput(ns("sub_date"), "Date variable", 
                     choices = FishSET:::date_cols(values$dataset), multiple = TRUE,
                     options = list(maxItems = 1, create = TRUE,
                                    placeholder = "Select or type variable name"))
    })
    
    output$grp_select <- renderUI({
      
      selectizeInput(ns("grp"), "Group by",
                     choices = c("year", "month", "week", category_cols(values$dataset)), 
                     multiple = TRUE, options = list(create = TRUE, 
                                                     placeholder = "Select or type variable name"))
    })
    
    output$grp_date_UI <- renderUI({
      
      if (!is.null(input$grp)) {
        if (any(input$grp %in% c("year", "month", "week"))) {
          if(is.null(input$sub_date) & is.null(input$fct_date)) {
            
            selectizeInput(ns("grp_date"), "Date variable",
                           choices = FishSET:::date_cols(values$dataset),
                           multiple = TRUE, options = list(maxItems = 1, create = TRUE, 
                                                           placeholder = "Select or type variable name"))
          }
        }
      }
    })
    
    output$fct_select <- renderUI({
      
      selectizeInput(ns("fct"), "split plot by",
                     choices = c("year", "month", "week", "species", category_cols(values$dataset)),
                     selected = fleet_col(category_cols(values$dataset)),
                     multiple = TRUE, options = list(maxItems = 2, create = TRUE, 
                                                     placeholder = "Select or type variable name"))
    })
    
    output$fct_date_UI <- renderUI({
      
      if (!is.null(input$fct)) {
        if (any(input$fct %in% c("year", "month", "week"))) {
          if (is.null(input$sub_date) & is.null(input$grp_date)) {
            
            selectizeInput(ns("fct_date"), "Date variable",
                           choices = FishSET:::date_cols(values$dataset),
                           multiple = TRUE, options = list(maxItems = 1, create = TRUE, 
                                                           placeholder = "Select or type variable name"))
          }
        }
      }
    })
    
    filter_val <- reactive({
      
      if (is.null(input$filter_by)) {
        
        NULL
        
      } else {
        
        out <- unique(values$dataset[[input$filter_by]])
        
        if (length(out) > 50)  out <- out[1:50]
        
        out
      }
    })
    
    output$filter_by_UIOutput <- renderUI({
      
      selectizeInput(ns("filter_by"), "Select variable",
                     choices = names(values$dataset), multiple = TRUE, options = list(maxItems = 1)) 
    })
    
    output$filter_by_val_UIOutput <- renderUI({
      
      tagList(  
        conditionalPanel("typeof input.filter_by !== 'undefined' && input.filter_by.length > 0", ns = ns,
                         style = "margin-left:19px;",
                         
                         selectizeInput(ns("filter_by_val"), "Select values",
                                        choices = filter_val(),
                                        multiple = TRUE, 
                                        options = list(maxOptions = 15,
                                                       placeholder = "Select or type value name",
                                                       create = TRUE)))
      )
    })
    
    output$filter_date_UIOutput <- renderUI({
        
      if (!is.null(input$filter_date)) {
        if (input$filter_date == "date_range") {
          
          dateRangeInput(ns("date_range"), label = "Date range",
                         start = min(values$dataset[[input$date]], na.rm = TRUE),
                         end = max(values$dataset[[input$date]], na.rm = TRUE))
          
        } else if (input$filter_date != "date_range") {
          
          filter_periodUI(id, values$dataset, input$date, input$filter_date)
        }
      }
    })
    
    date_value <- reactive({
      
      if (!is.null(input$date) & !is.null(input$filter_date)) {
        
        if (input$filter_date == "date_range") {
          
          return(input$date_range)
          
        } else if (input$filter_date != "date_range") {
          
          filter_periodOut(id, input$filter_date, input)
        }
        
      } else {
        
        NULL
      }
    })
    
    # reset if subset is unchecked
    observeEvent(input$subset_cb == FALSE, {
      
      updateCheckboxInput(session, "date_subset_cb", value = FALSE)
      updateCheckboxInput(session, "var_subset_cb", value = FALSE)
    })
    
    observeEvent(input$date_subset_cb == FALSE, {
      
      updateSelectizeInput(session, "sub_date", choices = FishSET:::date_cols(values$dataset), 
                           options = list(maxItems = 1, create = TRUE,
                                          placeholder = "Select or type variable name"))
      
      updateSelectizeInput(session, "filter_date",
                           choices = c("date range" = "date_range", "year-month",
                                       "year-week", "year-day", "year", "month", "week", "day"),
                           options = list(maxItems = 1), selected = NULL)
    })
    
    observeEvent(input$var_subset_cb == FALSE, {
      
      updateSelectizeInput(session, "filter_by", choices = names(values$dataset),
                           options = list(maxItems = 1), selected = NULL)
    })
    
    # reset if group is unchecked
    observeEvent(input$group_cb == FALSE, {
      
      updateSelectizeInput(session, "grp", 
                           choices = c("year", "month", "week", category_cols(values$dataset)), 
                           options = list(create = TRUE, placeholder = "Select or type variable name"))
      
      updateSelectizeInput(session, "grp_date", choices = FishSET:::date_cols(values$dataset),
                           options = list(maxItems = 1, create = TRUE,
                                          placeholder = "Select or type variable name"))
      
      updateCheckboxInput(session, "combine", value = FALSE)
    })
    
    # reset if split is unchecked
    observeEvent(input$split_cb == FALSE, {
      
      updateSelectizeInput(session, "fct", choices = c("year", "month", "week", "species",
                                                       category_cols(values$dataset)),
                           selected = NULL,
                           options = list(maxItems = 2))
      
      updateSelectizeInput(session, "fct_date", choices = date_cols(values$dataset),
                           options = list(maxItems = 1, create = TRUE, 
                                          placeholder = "Select or type variable name"))
    })
    
    # date col for subset, group, and split sections
    sub_date_col <- reactive({
      
      cols <- c(input$sub_date, input$grp_date, input$fct_date)
      if (!is.null(cols)) {
        cols
      } else {
        NULL
      }
    }) 
    
    roll_out <- eventReactive(input$fun_run, {
      
      validate(need(input$date, "Please select a date variable."),
               need(input$var, "Please select a catch variable."))
      
      validate_date(date = input$date, sub_date = sub_date_col(), 
                    filter_date = input$filter_date, fct = input$fct, grp = input$grp)
      
      q_test <- quietly_test(roll_catch)
      q_test(values$dataset, project = project(), catch = input$var, date = input$date,
             fun = input$fun, group = input$grp, combine = input$combine, 
             sub_date = sub_date_col(), filter_date = input$filter_date,
             date_value = date_value(), filter_by = input$filter_by, 
             filter_value = input$filter_by_val, filter_expr = input$filter_expr, 
             facet_by = input$fct, conv = input$conv, tran = input$tran, 
             format_lab = input$f_lab, scale = input$scale, output = input$out)
    })
    
    tabplot <- eventReactive(input$fun_run, {
      
      tabplot_output(roll_out(), input$out)
    })
    
    output$output <- renderUI({tabplot()})
  })
}

# Weekly Catch =====

weekly_catch_serv <- function(id, values, project) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    tabPlotServ("save", proj = project,
                out = reactive(list(weekly_catch = wc_out())), type = "tab_plot")
    
    tabPlotServ("save-summary", proj = project,
                out = reactive(list(weekly_catch = wc_out())), type = "tab_plot")
    
    output$var_select <- renderUI({
      
      selectizeInput(ns("var"), "Select catch variable",
                     choices = c(FishSET:::numeric_cols(values$dataset)), multiple = TRUE)
    })
    
    output$date_select <- renderUI({
      
      selectInput(ns("date"), "Date variable (x-axis)", 
                  choices = date_cols(values$dataset), multiple = FALSE)
    })
    
    output$sub_date_select <- renderUI({
      
      selectizeInput(ns("sub_date"), "Date variable", 
                     choices = date_cols(values$dataset), multiple = TRUE,
                     options = list(maxItems = 1, create = TRUE,
                                    placeholder = "Select or type variable name"))
    })
    
    output$grp_select <- renderUI({
      
      selectizeInput(ns("grp"), "Group by",
                     choices = c("year", "month", "week", category_cols(values$dataset)), 
                     multiple = TRUE, options = list(create = TRUE, 
                                                     placeholder = "Select or type variable name"))
    })
    
    output$grp_date_UI <- renderUI({
      
      if (!is.null(input$grp)) {
        if (any(input$grp %in% c("year", "month", "week"))) {
          if(is.null(input$sub_date) & is.null(input$fct_date)) {
            
            selectizeInput(ns("grp_date"), "Date variable",
                           choices = date_cols(values$dataset),
                           multiple = TRUE, options = list(maxItems = 1, create = TRUE, 
                                                           placeholder = "Select or type variable name"))
          }
        }
      }
    })
    
    output$fct_select <- renderUI({
      
      selectizeInput(ns("fct"), "split plot by",
                     choices = c("year", "month", "week", "species", category_cols(values$dataset)),
                     selected = fleet_col(category_cols(values$dataset)),
                     multiple = TRUE, options = list(maxItems = 2, create = TRUE, 
                                                     placeholder = "Select or type variable name"))
    })
    
    output$fct_date_UI <- renderUI({
      
      if (!is.null(input$fct)) {
        if (any(input$fct %in% c("year", "month", "week"))) {
          if (is.null(input$sub_date) & is.null(input$grp_date)) {
            
            selectizeInput(ns("fct_date"), "Date variable",
                           choices = date_cols(values$dataset),
                           multiple = TRUE, options = list(maxItems = 1, create = TRUE, 
                                                           placeholder = "Select or type variable name"))
          }
        }
      }
    })
    
    filter_val <- reactive({
      
      if (is.null(input$filter_by)) {
        
        NULL
        
      } else {
        
        out <- unique(values$dataset[[input$filter_by]])
        
        if (length(out) > 50)  out <- out[1:50]
        
        out
      }
    })
    
    output$filter_by_UIOutput <- renderUI({
      
      selectizeInput(ns("filter_by"), "Select variable",
                     choices = names(values$dataset), multiple = TRUE, options = list(maxItems = 1)) 
    })
    
    output$filter_by_val_UIOutput <- renderUI({
      
      tagList(  
        conditionalPanel("typeof input.filter_by !== 'undefined' && input.filter_by.length > 0", ns = ns,
                         style = "margin-left:19px;",
                         
                         selectizeInput(ns("filter_by_val"), "Select values",
                                        choices = filter_val(),
                                        multiple = TRUE, 
                                        options = list(maxOptions = 15, 
                                                       placeholder = "Select or type value name",
                                                       create = TRUE)))
        )
    })
    
    output$filter_date_UIOutput <- renderUI({
      
      if (!is.null(input$filter_date)) { 
        if (input$filter_date == "date_range") {
          
          dateRangeInput(ns("date_range"), label = "Date range",
                         start = min(values$dataset[[input$sub_date]], na.rm = TRUE),
                         end = max(values$dataset[[input$sub_date]], na.rm = TRUE))
          
        } else {
          
          filter_periodUI(id, values$dataset, input$sub_date, input$filter_date)
        }
      }
    })
    
    date_value <- reactive({
      
      if (!is.null(input$filter_date)) {
        
        if (input$filter_date == "date_range") {
          
          return(input$date_range)
          
        } else if (input$filter_date != "date_range") {
          
          filter_periodOut(id, input$filter_date, input)
        }
        
      } else {
        
        NULL
      }
    })
    
    # reset if subset is unchecked
    observeEvent(input$subset_cb == FALSE, {
      
      updateCheckboxInput(session, "date_subset_cb", value = FALSE)
      updateCheckboxInput(session, "var_subset_cb", value = FALSE)
    })
    
    # reset subset inputs if unchecked
    observeEvent(input$date_subset_cb == FALSE, {
      
      updateSelectizeInput(session, "sub_date", choices = date_cols(values$dataset), 
                           options = list(maxItems = 1, create = TRUE,
                                          placeholder = "Select or type variable name"))
      
      updateSelectizeInput(session, "filter_date",
                           choices = c("date range" = "date_range", "year-month",
                                       "year-week", "year-day", "year", "month", "week", "day"),
                           options = list(maxItems = 1), selected = NULL)
    })
    
    # reset if group is unchecked
    observeEvent(input$group_cb == FALSE, {
      
      updateSelectizeInput(session, "grp", 
                           choices = c("year", "month", "week", category_cols(values$dataset)), 
                           options = list(create = TRUE, placeholder = "Select or type variable name"))
      
      updateSelectizeInput(session, "grp_date", choices = date_cols(values$dataset),
                           options = list(maxItems = 1, create = TRUE,
                                          placeholder = "Select or type variable name"))
      
      updateCheckboxInput(session, "combine", value = FALSE)
    })
    
    # reset if split is unchecked
    observeEvent(input$split_cb == FALSE, {
      
      updateSelectizeInput(session, "fct", choices = c("year", "month", "week", "species",
                                                       category_cols(values$dataset)),
                           selected = NULL,
                           options = list(maxItems = 2))
      
      updateSelectizeInput(session, "fct_date", choices = date_cols(values$dataset),
                           options = list(maxItems = 1, create = TRUE, 
                                          placeholder = "Select or type variable name"))
    })
    
    # date col for subset, group, and split sections
    sub_date_col <- reactive({
      
      cols <- c(input$sub_date, input$grp_date, input$fct_date)
      
      if (!is.null(cols)) cols
      else NULL
    }) 
    
    wc_out <- eventReactive(input$fun_run, {
      
      validate(need(input$date, "Please select a date variable."),
               need(input$var, "Please select a catch variable."))
      
      validate_date(date = input$date, sub_date = sub_date_col(), 
                    filter_date = input$filter_date, fct = input$fct, grp = input$grp)
      
      q_test <- quietly_test(weekly_catch)
      q_test(values$dataset, project = project(), species = input$var, date = input$date,
             fun = input$fun, group = input$grp, sub_date = sub_date_col(),
             filter_date = input$filter_date, date_value = date_value(),
             filter_by = input$filter_by, filter_value = input$filter_by_val, 
             filter_expr = input$filter_expr, tran = input$tran, format_lab = input$f_lab,
             value = input$value, scale = input$scale, type = input$type, output = input$out, 
             format_tab = input$format)
    })
    
    tabplot <- eventReactive(input$fun_run, {
      
      tabplot_output(wc_out(), input$out)
    })
    
    output$output <- renderUI({tabplot()})
  })
}

# Weekly Effort ====

weekly_effort_serv <- function(id, values, project) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    tabPlotServ("save", proj = project,
                out = reactive(list(weekly_effort = we_out())), type = "tab_plot")
    
    tabPlotServ("save-summary", proj = project,
                out = reactive(list(weekly_effort = we_out())), type = "tab_plot")
    
    output$var_select <- renderUI({
      
      selectizeInput(ns("var"), "Select CPUE variable",
                     choices = FishSET:::numeric_cols(values$dataset), multiple = TRUE)
    })
    
    output$date_select <- renderUI({
      selectInput(ns("date"), "Date variable (x-axis)",
                  choices = date_cols(values$dataset))
    })
    
    output$sub_date_select <- renderUI({
      
      selectizeInput(ns("sub_date"), "Date variable", 
                     choices = date_cols(values$dataset), multiple = TRUE,
                     options = list(maxItems = 1, create = TRUE,
                                    placeholder = "Select or type variable name"))
    })
    
    output$grp_select <- renderUI({
      
      selectizeInput(ns("grp"), "Group by",
                     choices = c("year", "month", "weeks", category_cols(values$dataset)), 
                     multiple = TRUE, options = list(create = TRUE, 
                                                     placeholder = "Select or type variable name"))
    })
    
    output$grp_date_UI <- renderUI({
      
      if (!is.null(input$grp)) {
        if (any(input$grp %in% c("year", "month", "weeks"))) {
          if(is.null(input$sub_date) & is.null(input$fct_date)) {
            
            selectizeInput(ns("grp_date"), "Date variable",
                           choices = date_cols(values$dataset),
                           multiple = TRUE, options = list(maxItems = 1, create = TRUE, 
                                                           placeholder = "Select or type variable name"))
          }
        }
      }
    })
    
    output$fct_select <- renderUI({
      
      selectizeInput(ns("fct"), "split plot by",
                     choices = c("year", "month", "weeks", "species", category_cols(values$dataset)),
                     selected = fleet_col(category_cols(values$dataset)),
                     multiple = TRUE, options = list(maxItems = 2, create = TRUE, 
                                                     placeholder = "Select or type variable name"))
    })
    
    output$fct_date_UI <- renderUI({
      
      if (!is.null(input$fct)) {
        if (any(input$fct %in% c("year", "month", "weeks"))) {
          if (is.null(input$sub_date) & is.null(input$grp_date)) {
            
            selectizeInput(ns("fct_date"), "Date variable",
                           choices = date_cols(values$dataset),
                           multiple = TRUE, options = list(maxItems = 1, create = TRUE, 
                                                           placeholder = "Select or type variable name"))
          }
        }
      }
    })
    
    filter_val <- reactive({
      
      if (is.null(input$filter_by)) {
        
        NULL
        
      } else {
        
        out <- unique(values$dataset[[input$filter_by]])
        
        if (length(out) > 50)  out <- out[1:50]
        
        out
      }
    })
    
    output$filter_by_UIOutput <- renderUI({
      
      selectizeInput(ns("filter_by"), "Select variable",
                     choices = names(values$dataset), multiple = TRUE, options = list(maxItems = 1)) 
    })
    
    output$filter_by_val_UIOutput <- renderUI({
      
      tagList(  
        conditionalPanel("typeof input.filter_by !== 'undefined' && input.filter_by.length > 0", ns = ns,
                         style = "margin-left:19px;",
                         
                         selectizeInput(ns("filter_by_val"), "Select values",
                                        choices = filter_val(),
                                        multiple = TRUE, 
                                        options = list(maxOptions = 15,
                                                       placeholder = "Select or type value name",
                                                       create = TRUE)))
      )
    })
    
    output$filter_date_UIOutput <- renderUI({
      
      if (!is.null(input$filter_date)) { 
        if (input$filter_date == "date_range") {
          
          dateRangeInput(ns("date_range"), label = "Date range",
                         start = min(values$dataset[[input$sub_date]], na.rm = TRUE),
                         end = max(values$dataset[[input$sub_date]], na.rm = TRUE))
          
        } else {
          
          filter_periodUI(id, values$dataset, input$sub_date, input$filter_date)
        }
      }
    })
    
    date_value <- reactive({
      
      if (!is.null(input$filter_date)) {
        
        if (input$filter_date == "date_range") {
          
          return(input$date_range)
          
        } else if (input$filter_date != "date_range") {
          
          filter_periodOut(id, input$filter_date, input)
        }
        
      } else {
        
        NULL
      }
    })
    
    # reset if subset is unchecked
    observeEvent(input$subset_cb == FALSE, {
      
      updateCheckboxInput(session, "date_subset_cb", value = FALSE)
      updateCheckboxInput(session, "var_subset_cb", value = FALSE)
    })
    
    # reset subset inputs if unchecked
    observeEvent(input$date_subset_cb == FALSE, {
      
      updateSelectizeInput(session, "sub_date", choices = date_cols(values$dataset), 
                           options = list(maxItems = 1, create = TRUE,
                                          placeholder = "Select or type variable name"))
      
      updateSelectizeInput(session, "filter_date",
                           choices = c("date range" = "date_range", "year-month",
                                       "year-week", "year-day", "year", "month", "week", "day"),
                           options = list(maxItems = 1), selected = NULL)
    })
    
    # reset if group is unchecked
    observeEvent(input$group_cb == FALSE, {
      
      updateSelectizeInput(session, "grp", 
                           choices = c("year", "month", "weeks", category_cols(values$dataset)), 
                           options = list(create = TRUE, placeholder = "Select or type variable name"))
      
      updateSelectizeInput(session, "grp_date", choices = date_cols(values$dataset),
                           options = list(maxItems = 1, create = TRUE,
                                          placeholder = "Select or type variable name"))
      
      updateCheckboxInput(session, "combine", value = FALSE)
    })
    
    # reset if split is unchecked
    observeEvent(input$split_cb == FALSE, {
      
      updateSelectizeInput(session, "fct", choices = c("year", "month", "weeks", "species",
                                                       category_cols(values$dataset)),
                           selected = NULL,
                           options = list(maxItems = 2))
      
      updateSelectizeInput(session, "fct_date", choices = date_cols(values$dataset),
                           options = list(maxItems = 1, create = TRUE, 
                                          placeholder = "Select or type variable name"))
    })
    
    # date col for subset, group, and split sections
    sub_date_col <- reactive({
      
      cols <- c(input$sub_date, input$grp_date, input$fct_date)
      if (!is.null(cols)) {
        cols
      } else {
        NULL
      }
    }) 
    
    we_out <- eventReactive(input$fun_run, {
      
      validate(need(input$date, "Please select a date variable."),
               need(input$var, "Please select a cpue variable."))
      
      validate_date(date = input$date, sub_date = sub_date_col(), 
                    filter_date = input$filter_date, fct = input$fct, grp = input$grp)
      
      q_test <- quietly_test(weekly_effort)
      q_test(values$dataset, project = project(), cpue = input$var, date = input$date,
             group = input$grp, sub_date = sub_date_col(), filter_date = input$filter_date, 
             date_value = date_value(), filter_by = input$filter_by,
             filter_value = input$filter_by_val,  filter_expr = input$filter_expr, 
             facet_by = input$fct, combine = input$combine, conv = input$conv, 
             tran = input$tran, format_lab = input$f_lab, scale = input$scale, 
             output = input$out, format_tab = input$format)
    })
    
    tabplot <- eventReactive(input$fun_run, {
      
      tabplot_output(we_out(), input$out)
    })
    
    output$output <- renderUI({tabplot()})
  })
}

# Bycatch ====

bycatch_serv <- function(id, values, project) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    tabPlotServ("save", proj = project,
                out = reactive(list(bycatch = by_out())), type = "tab_plot")
    
    tabPlotServ("save-summary", proj = project,
                out = reactive(list(bycatch = by_out())), type = "tab_plot")
    
    output$cpue_select <- renderUI({
      selectizeInput(ns("cpue"), "Select CPUE variable(s)",
                     choices = FishSET:::numeric_cols(values$dataset), multiple = TRUE)
    })
    
    output$catch_select <- renderUI({
      selectizeInput(ns("catch"), "Select catch variable(s)",
                     choices = FishSET:::numeric_cols(values$dataset), multiple = TRUE)
    })
    
    output$date_select <- renderUI({
      selectInput(ns("date"), "Date variable (x-axis)",
                  choices = date_cols(values$dataset))
    })
    
    output$sub_date_select <- renderUI({
      
      selectizeInput(ns("sub_date"), "Date variable", 
                     choices = date_cols(values$dataset), multiple = TRUE,
                     options = list(maxItems = 1, create = TRUE,
                                    placeholder = "Select or type variable name"))
    })
    
    output$grp_select <- renderUI({
      
      selectizeInput(ns("grp"), "Group by",
                     choices = c("year", "month", "weeks", category_cols(values$dataset)), 
                     multiple = TRUE, options = list(create = TRUE, 
                                                     placeholder = "Select or type variable name"))
    })
    
    output$grp_date_UI <- renderUI({
      
      if (!is.null(input$grp)) {
        if (any(input$grp %in% c("year", "month", "weeks"))) {
          if(is.null(input$sub_date) & is.null(input$fct_date)) {
            
            selectizeInput(ns("grp_date"), "Date variable",
                           choices = date_cols(values$dataset),
                           multiple = TRUE, options = list(maxItems = 1, create = TRUE, 
                                                           placeholder = "Select or type variable name"))
          }
        }
      }
    })
    
    output$fct_select <- renderUI({
      
      selectizeInput(ns("fct"), "split plot by",
                     choices = c("year", "month", "weeks", "species", category_cols(values$dataset)),
                     selected = fleet_col(category_cols(values$dataset)),
                     multiple = TRUE, options = list(maxItems = 2, create = TRUE, 
                                                     placeholder = "Select or type variable name"))
    })
    
    output$fct_date_UI <- renderUI({
      
      if (!is.null(input$fct)) {
        if (any(input$fct %in% c("year", "month", "weeks"))) {
          if (is.null(input$sub_date) & is.null(input$grp_date)) {
            
            selectizeInput(ns("fct_date"), "Date variable",
                           choices = date_cols(values$dataset),
                           multiple = TRUE, options = list(maxItems = 1, create = TRUE, 
                                                           placeholder = "Select or type variable name"))
          }
        }
      }
    })
    
    filter_val <- reactive({
      
      if (is.null(input$filter_by)) {
        
        NULL
        
      } else {
        
        out <- unique(values$dataset[[input$filter_by]])
        
        if (length(out) > 50)  out <- out[1:50]
        
        out
      }
    })
    
    output$filter_by_UIOutput <- renderUI({
      
      selectizeInput(ns("filter_by"), "Select variable",
                     choices = names(values$dataset), multiple = TRUE, options = list(maxItems = 1)) 
    })
    
    output$filter_by_val_UIOutput <- renderUI({
      
      tagList(  
        conditionalPanel("typeof input.filter_by !== 'undefined' && input.filter_by.length > 0", ns = ns,
                         style = "margin-left:19px;",
                         
                         selectizeInput(ns("filter_by_val"), "Select values",
                                        choices = filter_val(),
                                        multiple = TRUE, 
                                        options = list(maxOptions = 15, 
                                                       placeholder = "Select or type value name",
                                                       create = TRUE)))
      )
    })
    
    output$filter_date_UIOutput <- renderUI({
      
      if (!is.null(input$filter_date)) { 
        if (input$filter_date == "date_range") {
          
          dateRangeInput(ns("date_range"), label = "Date range",
                         start = min(values$dataset[[input$date]], na.rm = TRUE),
                         end = max(values$dataset[[input$date]], na.rm = TRUE))
          
        } else {
          
          filter_periodUI(id, values$dataset, input$date, input$filter_date)
        }
      }
    })
    
    date_value <- reactive({
      
      if (!is.null(input$filter_date)) {
        
        if (input$filter_date == "date_range") {
          
          return(input$date_range)
          
        } else if (input$filter_date != "date_range") {
          
          filter_periodOut(id, input$filter_date, input)
        }
        
      } else {
        
        NULL
      }
    })
    
    # reset if subset is unchecked
    observeEvent(input$subset_cb == FALSE, {
      
      updateCheckboxInput(session, "date_subset_cb", value = FALSE)
      updateCheckboxInput(session, "var_subset_cb", value = FALSE)
    })
    
    # reset subset inputs if unchecked
    observeEvent(input$date_subset_cb == FALSE, {
      
      updateSelectizeInput(session, "sub_date", choices = date_cols(values$dataset), 
                           options = list(maxItems = 1, create = TRUE,
                                          placeholder = "Select or type variable name"))
      
      updateSelectizeInput(session, "filter_date",
                           choices = c("date range" = "date_range", "year-month",
                                       "year-week", "year-day", "year", "month", "week", "day"),
                           options = list(maxItems = 1), selected = NULL)
    })
    
    # reset if group is unchecked
    observeEvent(input$group_cb == FALSE, {
      
      updateSelectizeInput(session, "grp", 
                           choices = c("year", "month", "weeks", category_cols(values$dataset)), 
                           options = list(create = TRUE, placeholder = "Select or type variable name"))
      
      updateSelectizeInput(session, "grp_date", choices = date_cols(values$dataset),
                           options = list(maxItems = 1, create = TRUE,
                                          placeholder = "Select or type variable name"))
      
      updateCheckboxInput(session, "combine", value = FALSE)
    })
    
    # reset if split is unchecked
    observeEvent(input$split_cb == FALSE, {
      
      updateSelectizeInput(session, "fct", choices = c("year", "month", "weeks", 
                                                       category_cols(values$dataset)),
                           selected = NULL,
                           options = list(maxItems = 2))
      
      updateSelectizeInput(session, "fct_date", choices = date_cols(values$dataset),
                           options = list(maxItems = 1, create = TRUE, 
                                          placeholder = "Select or type variable name"))
    })
    
    # date col for subset, group, and split sections
    sub_date_col <- reactive({
      
      cols <- c(input$sub_date, input$grp_date, input$fct_date)
      if (!is.null(cols)) {
        cols
      } else {
        NULL
      }
    }) 
    
    names <- reactiveValues(names = NULL)
    
    observeEvent(input$nms_add, {
      
      names$names <- c(names$names, input$nms)
    })
    
    observeEvent(input$nms_clear, { names$names <- NULL })
    
    output$caption <- renderText({names$names})
    
    by_out <- eventReactive(input$fun_run, {
      
      validate(need(input$date, "Please select a date variable."))
      
      validate_date(date = input$date, sub_date = sub_date_col(), 
                    filter_date = input$filter_date, fct = input$fct, grp = input$grp)
      
      q_test <- quietly_test(bycatch)
      q_test(values$dataset, project = project(), cpue = input$cpue, catch = input$catch, 
             date = input$date, group = input$grp, names = names$names, period = input$period,
             sub_date = sub_date_col(), filter_date = input$filter_date, date_value = date_value(),
             filter_by = input$filter_by, filter_value = input$filter_by_val, 
             filter_expr = input$filter_expr,  facet_by = input$fct,value = input$value, 
             combine = input$combine, conv = input$conv, tran = input$tran,
             format_lab = input$f_lab, scale = input$scale, output = input$out, 
             format_tab = input$format)
    })
    
    tabplot <- eventReactive(input$fun_run, {
      
      tabplot_output(by_out(), input$out)
    })
    
    output$output <- renderUI({tabplot()})
  })
}

# Trip Duration ====

trip_serv <- function(id, values, project) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    tabPlotServ("save", proj = project,
                out = reactive(list(trid_dur_out = trip_out())), type = "tab_plot")
    
    tabPlotServ("save-summary", proj = project,
                out = reactive(list(trid_dur_out = trip_out())), type = "tab_plot")
    
    output$start_select <- renderUI({
      selectInput(ns("start"), "Trip start date",
                  choices = c(date_cols(values$dataset)))
    })
    
    output$end_select <- renderUI({
      selectInput(ns("end"), "Trip end date",
                  choices = c(date_cols(values$dataset)))
    })
    
    output$vpue_select <- renderUI({
      selectizeInput(ns("vpue"), "Calculate value per unit effort (optional)",
                     choices = c(FishSET:::numeric_cols(values$dataset)), multiple = TRUE)
    })
    
    output$sub_date_select <- renderUI({
      
      selectizeInput(ns("sub_date"), "Date variable", 
                     choices = date_cols(values$dataset), multiple = TRUE,
                     options = list(maxItems = 1, create = TRUE,
                                    placeholder = "Select or type variable name"))
    })
    
    output$grp_select <- renderUI({
      
      selectizeInput(ns("grp"), "Group by",
                     choices = c("year", "month", "week", category_cols(values$dataset)), 
                     multiple = TRUE, options = list(create = TRUE, 
                                                     placeholder = "Select or type variable name"))
    })
    
    output$grp_date_UI <- renderUI({
      
      if (!is.null(input$grp)) {
        if (any(input$grp %in% c("year", "month", "week"))) {
          if(is.null(input$sub_date) & is.null(input$fct_date)) {
            
            selectizeInput(ns("grp_date"), "Date variable",
                           choices = date_cols(values$dataset),
                           multiple = TRUE, options = list(maxItems = 1, create = TRUE, 
                                                           placeholder = "Select or type variable name"))
          }
        }
      }
    })
    
    output$fct_select <- renderUI({
      
      selectizeInput(ns("fct"), "split plot by",
                     choices = c("year", "month", "week", category_cols(values$dataset)),
                     selected = fleet_col(category_cols(values$dataset)),
                     multiple = TRUE, options = list(maxItems = 2, create = TRUE, 
                                                     placeholder = "Select or type variable name"))
    })
    
    output$fct_date_UI <- renderUI({
      
      if (!is.null(input$fct)) {
        if (any(input$fct %in% c("year", "month", "week"))) {
          if (is.null(input$sub_date) & is.null(input$grp_date)) {
            
            selectizeInput(ns("fct_date"), "Date variable",
                           choices = date_cols(values$dataset),
                           multiple = TRUE, options = list(maxItems = 1, create = TRUE, 
                                                           placeholder = "Select or type variable name"))
          }
        }
      }
    })
    
    output$htp_select <- renderUI({
      selectizeInput(ns("tripID"), "Columns indentifying unique trips",
                     choices = colnames(values$dataset), multiple = TRUE)
    })
    
    filter_val <- reactive({
      
      if (is.null(input$filter_by)) {
        
        NULL
        
      } else {
        
        out <- unique(values$dataset[[input$filter_by]])
        
        if (length(out) > 50)  out <- out[1:50]
        
        out
      }
    })
    
    output$filter_by_UIOutput <- renderUI({
      
      selectizeInput(ns("filter_by"), "Select variable",
                     choices = names(values$dataset), multiple = TRUE, options = list(maxItems = 1)) 
    })
    
    output$filter_by_val_UIOutput <- renderUI({
      
      tagList(  
        conditionalPanel("typeof input.filter_by !== 'undefined' && input.filter_by.length > 0", ns = ns,
                         style = "margin-left:19px;",
                         
                         selectizeInput(ns("filter_by_val"), "Select values",
                                        choices = filter_val(),
                                        multiple = TRUE, 
                                        options = list(maxOptions = 15, 
                                                       placeholder = "Select or type value name",
                                                       create = TRUE)))
      )
    })
    
    output$filter_date_UIOutput <- renderUI({
      
      if (!is.null(input$filter_date)) { 
        if (input$filter_date == "date_range") {
          
          dateRangeInput(ns("date_range"), label = "Date range",
                         start = min(values$dataset[[input$sub_date]], na.rm = TRUE),
                         end = max(values$dataset[[input$sub_date]], na.rm = TRUE))
          
        } else {
          
          filter_periodUI(id, values$dataset, input$sub_date, input$filter_date)
        }
      }
    })
    
    date_value <- reactive({
      
      if (!is.null(input$filter_date)) {
        
        if (input$filter_date == "date_range") {
          
          return(input$date_range)
          
        } else if (input$filter_date != "date_range") {
          
          filter_periodOut(id, input$filter_date, input)
        }
        
      } else {
        
        NULL
      }
    })
    
    # reset haul to trip 
    observeEvent(input$haul_trp == FALSE, {
      
      updateSelectizeInput(session, "tripID", choices = colnames(values$dataset))
      updateCheckboxInput(session, "haul_count", value = FALSE)
    })
    
    # reset if subset is unchecked
    observeEvent(input$subset_cb == FALSE, {
      
      updateCheckboxInput(session, "date_subset_cb", value = FALSE)
      updateCheckboxInput(session, "var_subset_cb", value = FALSE)
    })
    
    observeEvent(input$date_subset_cb == FALSE, {
      
      updateSelectizeInput(session, "sub_date", choices = date_cols(values$dataset), 
                           options = list(maxItems = 1, create = TRUE,
                                          placeholder = "Select or type variable name"))
      
      updateSelectizeInput(session, "filter_date",
                           choices = c("date range" = "date_range", "year-month",
                                       "year-week", "year-day", "year", "month", "week", "day"),
                           options = list(maxItems = 1), selected = NULL)
    })
    
    observeEvent(input$var_subset_cb == FALSE, {
      
      updateSelectizeInput(session, "filter_by", choices = names(values$dataset),
                           options = list(maxItems = 1), selected = NULL)
    })
    
    # reset if group is unchecked
    observeEvent(input$group_cb == FALSE, {
      
      updateSelectizeInput(session, "grp", 
                           choices = c("year", "month", "week", category_cols(values$dataset)), 
                           options = list(create = TRUE, placeholder = "Select or type variable name"))
      
      updateSelectizeInput(session, "grp_date", choices = date_cols(values$dataset),
                           options = list(maxItems = 1, create = TRUE,
                                          placeholder = "Select or type variable name"))
      
      updateCheckboxInput(session, "combine", value = FALSE)
    })
    
    # reset if split is unchecked
    observeEvent(input$split_cb == FALSE, {
      
      updateSelectizeInput(session, "fct", choices = c("year", "month", "week",
                                                       category_cols(values$dataset)),
                           selected = NULL,
                           options = list(maxItems = 2))
      
      updateSelectizeInput(session, "fct_date", choices = date_cols(values$dataset),
                           options = list(maxItems = 1, create = TRUE, 
                                          placeholder = "Select or type variable name"))
    })
    
    # date col for subset, group, and split sections
    sub_date_col <- reactive({
      
      cols <- c(input$sub_date, input$grp_date, input$fct_date)
      if (!is.null(cols)) cols
      else NULL
    }) 
    
    trip_out <- eventReactive(input$fun_run, {
      
      validate(need(input$start, "Please select a start date variable."),
               need(input$end, "Please select an end date variable."))
      
      q_test <- quietly_test(trip_dur_out)
      q_test(values$dataset, project = project(), start = input$start, end = input$end,
             units = input$unit, vpue = input$vpue, haul_count = input$haul_count,
             group = input$grp, sub_date = sub_date_col(), filter_date = input$filter_date, 
             date_value = date_value(), filter_by = input$filter_by, 
             filter_value = input$filter_by_val, filter_expr = input$filter_expr, 
             facet_by = input$fct, density = input$dens, tran = input$tran, 
             format_lab = input$f_lab, scale = input$scale, output = input$out, 
             pages = input$pages, remove_neg = input$rm_neg, type = input$type, 
             bins = input$bins, tripID = input$tripID, fun.time = input$fun_time, 
             fun.numeric = input$fun_numeric)
    })
    
    tabplot <- eventReactive(input$fun_run, {
      
      tabplot_output(trip_out(), input$out)
    })
    
    output$output <- renderUI({tabplot()})
  })
}
# Fleet Table ====

# expr builder add row server
nexpr_row_server <- function(id, values) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    output$varUI <- renderUI({
      selectizeInput(ns("var"), "", 
                  choices = colnames(values$dataset), multiple = TRUE, 
                  options = list(maxItems = 1))
    })
    
    unique_values  <- reactive({
      
      if (is.null(input$var)) {
        
        NULL
        
      } else {
        
        out <- unique(values$dataset[[input$var]])
        
        out
      }
    })
    
    output$valueUI <- renderUI({
      
      if (!is.null(input$oper)) {
      
        if (input$oper == "%in%") {
          
          selectizeInput(ns("value"), "",
                         choices = unique_values(),
                         multiple = TRUE, options = list(maxOptions = 15,
                                                         placeholder = "Select or type value name",
                                                         create = TRUE))
        } else {
          
          selectizeInput(ns("value"), "",
                         choices = unique_values(),   
                         multiple = TRUE, options = list(maxOptions = 15, maxItems = 1, 
                                                         placeholder = "Select or type value name",
                                                         create = TRUE))
        }
        
      } else {
        selectizeInput(ns("value"), "", choices = "") # placeholder widget
      }
    })
  })
}

fleet_table_serv <- function(id, values, project) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # expression builder ----
    
    rv <- reactiveValues(expr = character(0),
                         expr_num = 1,
                         select = list(target = "row"))
    
    output$select_var <- renderUI({
      
      selectizeInput(ns("nexpr_1-var"), "Variable", 
                  choices = colnames(values$dataset), multiple = TRUE, 
                  options = list(maxItems = 1))
    })
    
    # used for value input
    unique_values <- reactive({
      
      if (is.null(input[["nexpr_1-var"]])) {
        
        NULL
        
      } else {
        
        out <- unique(values$dataset[[input[["nexpr_1-var"]]]])
        
        if (length(out) > 50)  out <- out[1:50]
        
        out
      }
    })
    
    output$valueUI <- renderUI({
      
      if (is.null(input[["nexpr_1-oper"]])) {
        
        selectizeInput(ns("nexpr_1-value"), "Value", choices = "") # placeholder widget
      
        } else {
        
          if (input[["nexpr_1-oper"]] == "%in%") {
            
            selectizeInput(ns("nexpr_1-value"), "Value",
                           choices = unique_values(),
                           multiple = TRUE, options = list(maxOptions = 15, # no maxItems
                                                           placeholder = "Select or type value name",
                                                           create = TRUE))
          } else {
            
            selectizeInput(ns("nexpr_1-value"), "Value",
                           choices = unique_values(),
                           multiple = TRUE, options = list(maxOptions = 15, maxItems = 1,
                                                           placeholder = "Select or type value name",
                                                           create = TRUE))
          }
      }
    })
    
    # insert new expression line when blue plus button is clicked
    observeEvent(input$add_expr, {
      
      rv$expr_num <- rv$expr_num + 1
      ui_id <- ns(paste0("nexpr_", rv$expr_num)) # different namespaces for ui 
      server_id <- paste0("nexpr_", rv$expr_num) # and server (otherwise server 
                                                 # gets a duplicate ns)
      insertUI(
        selector = "#n_expr_container",
        where = "beforeEnd",
        ui = nexpr_row_ui(ui_id) # run expr row UI module
      )
      
      nexpr_row_server(server_id, values) # run expr row server module
    })
    
    
    # reactive containing expression that updates whenever an expr selector is changed
    expr <- reactive({
      
      req(input[["nexpr_1-var"]]) # prevents reactive from running early
      
      expr_list <- lapply(seq(rv$expr_num), function(x) {
        
        new_id <- paste0("nexpr_", x)
        log_oper <- paste0(new_id, "-log_oper")
        var <- paste0(new_id, "-var")
        oper <- paste0(new_id, "-oper")
        val <- paste0(new_id, "-value")
        value <- "" # empty placeholder value
        
        if (is_value_empty(input[[val]]) == FALSE) {
          
          var_class <- class(values$dataset[[input[[var]]]])
          
          if (input[[oper]] == "%in%") { # if "Contains" selected (multiple values)
            
            value <- input[[val]]
            value <- as.list(value)
            value <- rlang::expr(c(!!!value))
            value <- rlang::expr_text(value)
            
            # remove quotes if numeric
            if (any(var_class %in% c("numeric", "integer", "double"))) {
              value <- gsub('"', "", value)
            }
          
          } else {
            
            # check if value should be wrapped in quotes
            if (any(var_class %in% c("character", "factor", "Date", "POSIXct", "POSIXt"))) {
              
              value <- paste0('"', input[[val]], '"') # add quotes
            } else {
              value <- input[[val]] # otherwise, assign original value
            }
          }
        }
        
        paste(input[[log_oper]], input[[var]], input[[oper]], value)
      })
      
      paste(expr_list, collapse = " ")
    })
   
    # display current expression 
    output$expr_txt <- renderText(expr())
    
    # validate that all expr inputs have been filled out
    validate_expr <- reactive({
      
      expr_list <- lapply(seq(rv$expr_num), function(x) {
        
        new_id <- paste0("nexpr_", x)
        expr_inputs <- grep(new_id, names(input), value = TRUE)

        vapply(expr_inputs, function(e) is_value_empty(input[[e]]), logical(1))
        
      })

       any(unlist(expr_list)) # are any exprs empty? 
    })
    
    # remove expression UI
    observeEvent(input$reset_expr, {
     
      rv$expr_num <- 1
      removeUI(selector = ".n-expr-section", multiple = TRUE)
      
      # reset first row of widgets 
      updateSelectizeInput(session, "nexpr_1-var", choices = colnames(values$dataset), 
                           options = list(maxIems = 1), selected = NULL)
      
      updateSelectizeInput(session, "nexpr_1-oper",  
                           choices = c("less than" = "<","greater than" = ">", "less than or equal to" = "<=", 
                                       "greater than or equal to" = ">=", "equal to" = "==", "not equal to" = "!=",
                                       "contains" = "%in%"), options = list(maxIems = 1), selected = NULL)
    })

    
    # fleet table builder ----
    f_r <- reactiveValues()
    empty_row <- data.frame(condition = "enter condition", fleet = "enter fleet name",
                            stringsAsFactors = FALSE)
    empty_col <- data.frame(condition = "enter condition", stringsAsFactors = FALSE)
    f_r$row <- empty_row
    f_r$f_DT <- empty_row
    proxy_f <- DT::dataTableProxy("f_tab")
    
    # upload existing table
    observeEvent(input$upload, {
      
      req(input$file)
      
      upload <- read_dat(input$file$datapath)
      
      # convert factors to strings
      if (any(vapply(upload, is.factor, FUN.VALUE = logical(1)))) {
        
        fac <- vapply(upload, is.factor, FUN.VALUE = logical(1))
        
        for (i in names(upload[fac])) {
          
          upload[[i]] <- as.character(upload[[i]])
        }
      }
        
      f_r$f_DT <- upload
      f_r$row <- upload[1, ]
      
      for (i in seq_along(f_r$row)) {
        f_r$row[[i]] <- "enter value"
      }
    })
    
    # Select rows, columns, cells, or deselect
    observeEvent(input$deselect, rv$select <- "none")
    
    observeEvent(input$select_row, rv$select <- list(target = "row"))
    
    observeEvent(input$select_column, rv$select <- list(target = "column"))
    
    observeEvent(input$select_cell, rv$select <- list(target = "cell"))
    
    # add or remove columns/rows
    observeEvent(input$addrow, {
      
      colnames(f_r$row) <- colnames(f_r$f_DT)
      
      f_r$f_DT <- rbind(f_r$f_DT, f_r$row)
    })
    
    observeEvent(input$addcol, {
      
      f_r$f_DT <- cbind(empty_col, f_r$f_DT)
      f_r$row <- cbind(empty_col, f_r$row)
    })
    
    observeEvent(input$deleterow, {
      
      if (!is.null(input$f_tab_rows_selected)) {
        
        f_r$f_DT <- f_r$f_DT[-input$f_tab_rows_selected, ]
      }
    })
    
    observeEvent(input$deletecol, {
      
      if (!is.null(input$f_tab_columns_selected)) {
        
        f_r$f_DT <- f_r$f_DT[, -input$f_tab_columns_selected]
        f_r$row <- f_r$row[, -input$f_tab_columns_selected]
      }
    })
    
    # edit fleet table cells 
    observeEvent(input$f_tab_cell_edit, {
      
      f_r$f_DT <<- DT::editData(f_r$f_DT, input$f_tab_cell_edit, "f_tab")
      rownames(f_r$f_DT) <- 1:nrow(f_r$f_DT)
      
      rv$empty_cell <- f_r$f_DT == ""
      rv$fleet_col <- grep("fleet", names(f_r$f_DT), ignore.case = TRUE)
      
      rv$empty_cond <- rv$empty_cell
      
      if (rv$fleet_col != 0) {
        
        rv$empty_cond[, rv$fleet_col] <- FALSE # prevent adding "enter condition" to fleet column 
        rv$empty_fleet <- rv$empty_cell
        rv$empty_fleet[, -rv$fleet_col] <- FALSE
        
        f_r$f_DT[rv$empty_fleet] <- "enter fleet name"
      } 
      
      f_r$f_DT[rv$empty_cond] <- "enter condition"
      
      DT::replaceData(proxy_f, f_r$f_DT, resetPaging = FALSE)
    })
    
    # insert expression from builder
    observeEvent(input$insert_expr, {
      
      if (validate_expr()) {
        
        showNotification("Please complete the expression. Expression not inserted.", type = "warning")
      
        } else {
        
          if (dim(input$f_tab_cells_selected)[1] != 0) { # check if user selected a cell
            
            f_r$f_DT[input$f_tab_cells_selected] <- expr() 
            
          } else {
            
            rv$cond_ind <- which(f_r$f_DT == "enter condition", arr.ind = TRUE) # index of condition cells (matrix)
            
            if (nrow(rv$cond_ind) == 0) {
              
              showNotification("No empty condition cells available. Use 'select cell' to overwrite an expression.", 
                               type = "warning")
            } else {
              
              if (nrow(rv$cond_ind) > 1) rv$cond_ind <- rv$cond_ind[order(rv$cond_ind[, 1]), ] # order by row 
           
              f_r$f_DT[rv$cond_ind[1, 1], rv$cond_ind[1, 2]] <- expr() # insert expr into next available cell
              
              # add row
              colnames(f_r$row) <- colnames(f_r$f_DT)
              f_r$f_DT <- rbind(f_r$f_DT, f_r$row)
            }
          }
      }
     
    })
    
    # render fleet table
    output$f_tab <- DT::renderDataTable(f_r$f_DT, 
                                        editable  = "all",
                                        selection = rv$select,
                                        escape = FALSE)
    
    # change column name 
    observeEvent(input$colname_btn, {
      req(input$colname)
      names(f_r$f_DT)[input$f_tab_columns_selected] <- input$colname
    })
    
    # reference table
    output$reference <- renderTable({
      data.frame(operator = c("<", ">", "<=", ">=", "==", "!=", "%in%", "!", "&", "|"), 
                 description = c("Less than", "Greater than", "Less than or equal to", 
                                 "Greater than or equal to", "Equal to (single value)", "Not equal to", 
                                 "Contains (multiple values)", "Logical NOT", "Logical AND", "Logical OR"))
    })
    # run fleet_table/save table
    observeEvent(input$save, {
      
      q_test <- quietly_test(fleet_table)
      
      # TODO: return invisible(TRUE) if saved, invisible(FALSE) if not
      q_test(values$dataset, project = project(), table = f_r$f_DT, save = TRUE)
      showNotification("Table saved.", type = 'message', duration = 10)
    })
  })
}

# Fleet Assign ====

fleet_assign_serv <- function(id, values, project) {
  
  moduleServer(id, function(input, output, session) {
    
    fleet_tab_db <- reactive({
      
      input$refresh_tabs
      
      grep("FleetTable", tables_database(project()), value = TRUE)
    })
    
    output$available_tabs <- renderUI({
      
      selectInput(session$ns("tab"), "Available fleet tables",
                  choices = fleet_tab_db()) 
    })
    
    f_tab <- reactiveValues()
    
    observeEvent(input$load_btn, {
      
      f_tab$tab <- table_view(input$tab, project())
    })
    
    output$tab_preview <- DT::renderDT(f_tab$tab,
                                       selection = list(target = "row"),
                                       escape = FALSE)
    
    fa_out <- eventReactive(input$fun_run, {
      
      validate(need(f_tab$tab, "Please upload a fleet table."))
      
      q_test <- quietly_test(fleet_assign)
      
      q_test(values$dataset, project = project(), fleet_tab = input$tab, 
             overlap = input$overlap, format_var = input$format, 
             assign = input$tab_preview_rows_selected)
    })
    
    observeEvent(input$fun_run, {
      
      # if (is.null(fa_out())) {
      #   
      #   showNotification("Overlap detected, no fleets assigned.", type = "warning")
      # 
      # } else {
      #   
      #   values$dataset <- fa_out()
      # }
      
      if (!is_value_empty(fa_out())) {
        
        values$dataset <- fa_out() 
      }
      
    })
    
    output$final_tab <- DT::renderDT({fa_out()})
    
    # table of fleet counts for plot
    observeEvent(input$fun_run, {
      
      if (!is.null(fa_out())) {
        
        if (input$format == "string") {
          # assumes last column is fleet column
          fleet_col <- names(fa_out())[ncol(fa_out())]
          
          fleet_names <- f_tab$tab$fleet
          
          if (!is.null(input$tab_preview_rows_selected)) {
            
            fleet_names <- fleet_names[input$tab_preview_rows_selected]
          }
          # add "other" category
          fleet_names <- c(fleet_names, "other")
          
          fleet_count <- vapply(fleet_names, function(x) {
            sum(fa_out()[fleet_col] == x)
          }, numeric(1))
          
          fleet_count <- sort(fleet_count, decreasing = TRUE)
          fleet_count <- data.frame(fleet = names(fleet_count), counts = fleet_count)
          row.names(fleet_count) <- NULL
          f_tab$count <- fleet_count
          
        } else {
          
          fleet_cols <- gsub(" ", "_", trimws(f_tab$tab$fleet))
          
          if (!is.null(input$tab_preview_rows_selected)) {
            
            fleet_cols <- fleet_cols[input$tab_preview_rows_selected]
          }
          # include "other" fleet
          fleet_cols <- c(fleet_cols, "other")
          
          fleet_count <- vapply(fa_out()[fleet_cols], sum, FUN.VALUE = numeric(1))
          df <- data.frame(fleet = fleet_cols, counts = fleet_count)
          df <- df[order(df$counts, decreasing = TRUE), ] 
          row.names(df) <- NULL
          
          f_tab$count <- df
        }
      }
    })
    
    # table of fleet frequencies
    output$tab_count <- DT::renderDT(
      
      if (!is.null(fa_out())) {
       
        f_tab$count 
      }
     )
    
    # plot of fleet frequencies 
    output$plot_count <- renderPlot({
      
      if (!is.null(fa_out())) {
      
        ggplot2::ggplot(f_tab$count, ggplot2::aes(x = reorder(fleet, counts), y = counts, fill = fleet)) +
          ggplot2::geom_col() + ggplot2::coord_flip() +
          ggplot2::labs(x = "", y = "# of obs.") +
          fishset_theme() + ggplot2::theme(legend.position = "none")
      }
    })
  })
}