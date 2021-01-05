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
    
    return(exp_out)
  })
}


# Save buttons ====

saveOutputServ <- function(id, fun_id, project, fun_name, tab_plot, out) {
  
  moduleServer(id, function(input, output, session) {
    
    table_save <- reactive({
      if (out() == "table") {
        
        tab_plot() 
      } else if (out()  == "tab_plot") {
        
        tab_plot()$table
      } else {
        NULL 
      }
    })
    
    plot_save <- reactive({
      if (out() == "plot") {
        
        tab_plot() 
      } else if (out() == "tab_plot") {
        
        tab_plot()$plot
      } else {
        NULL 
      }
    })
    
    observeEvent(input$downloadplot, {
      
      if (any(out() %in% c("plot", "tab_plot"))) {
        
        output$downloadplotHIDE <<- downloadHandler(
          filename = function() {
            paste0(locoutput(),  project(), "_", fun_name, '.png')
          },
          content = function(file) {
            ggplot2::ggsave(file, plot = plot_save())
          })
        jsinject <- paste0("setTimeout(function(){window.open($('#", fun_id, "-", id, "-downloadplotHIDE').attr('href'))}, 100);")
        session$sendCustomMessage(type = 'jsCode', list(value = jsinject))
        showNotification('Plot saved.', type = 'message', duration = 10)
        
      } else {
        showNotification('Plot not found.', type = 'message', duration = 10)
      }
    })
    
    observeEvent(input$downloadTable, {
      
      if (any(out() %in% c("table", "tab_plot"))) {
        
        write.csv(table_save(), paste0(locoutput(), project(), "_", fun_name, '.csv'))
        showNotification('Table saved.', type = 'message', duration = 10)
        
      } else {
        showNotification('Table not found.', type = 'message', duration = 10)
      }
    })
  })
}


saveDataTableServ <- function(id, values, project) {
  
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$saveData, {
      suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase()))
      DBI::dbWriteTable(fishset_db, paste0(project(), 'MainDataTable'), values$dataset, overwrite = TRUE)
      DBI::dbDisconnect(fishset_db)
      showNotification('Data saved to FishSET database', type = 'message', duration = 10)
    })
  })
}

refreshServ <- function(id, values, project) {
  
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$refresh, {
      req(project())
      temp <- tables_database()[grep(paste0(project(), 'MainDataTable\\d+'), tables_database())][which(
        unlist(stringr::str_extract_all(tables_database()[grep(paste0(project(), 'MainDataTable\\d+'), 
                                                               tables_database())], "\\d+"))==max((unlist(stringr::str_extract_all(tables_database()[grep(paste0(project(), 
                                                                                                                                                                 'MainDataTable\\d+'), tables_database())], "\\d+")))))]
      values$dataset <- table_view(temp)
      showNotification("Data refreshed", type = 'message', duration = 10)
    }, ignoreInit = TRUE, ignoreNULL = TRUE) 
  })
}


closeAppServ <-  function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$close, { stopApp() })
  })
}


# Density Plot ====

density_serv <- function(id, values, project) {
  
  moduleServer(id, function(input, output, session) {
    
    closeAppServ("close")
    
    refreshServ("refresh", values, project)
    
    observeEvent(input$downloadplot, {
      output$downloadplotHIDE <<- downloadHandler(
        filename = function() {
          paste0(locoutput(), project(), "_density_plot.png")
        },
        content = function(file) {
          ggplot2::ggsave(file, plot = den_out())
        })
      jsinject <- "setTimeout(function(){window.open($('#den-downloadplotHIDE').attr('href'))}, 100);"
      session$sendCustomMessage(type = 'jsCode', list(value = jsinject))
    })
    
    ns <- session$ns
    
    output$var_select <- renderUI({
      
      selectInput(ns("var"), "Select variable",
                  choices = c(numeric_cols(values$dataset)))
    })
    
    output$grp_select <- renderUI({
      
      selectizeInput(ns("grp"), "grouping variables (optional)",
                     choices = c("year", "month", "week", colnames(values$dataset)),
                     multiple = TRUE)
    })
    
    output$date_select <- renderUI({
      
      selectizeInput(ns("date"), "Subset, split, or group by date (optional)",
                     choices = c(date_cols(values$dataset)),
                     multiple = TRUE, options = list(maxItems = 1))
    })
    
    output$fct_select <- renderUI({
      
      selectizeInput(ns("fct"), "Split plot by (optional)",
                     choices = c("year", "month", "week", colnames(values$dataset)),
                     multiple = TRUE, options = list(maxItems = 2))
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
      
      selectizeInput(ns("filter_by"), "Subset by variable",
                     choices = names(values$dataset), multiple = TRUE, options = list(maxItems = 1)) 
    })
    
    output$filter_by_val_UIOutput <- renderUI({
      
      tagList(  
        conditionalPanel("typeof input.filter_by !== 'undefined' && input.filter_by.length > 0", ns = ns,
                         style = "margin-left:19px;",
                         
                         selectizeInput(ns("filter_by_val"), "Select values to subset by",
                                        choices = filter_val(),
                                        multiple = TRUE, options = list(maxOptions = 15, placeholder = "Select or type value name")))
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
    
    den_out <- eventReactive(input$fun_run, {
      
      validate_date(input$date, input$filter_date, input$fct, input$grp)
      
      density_plot(values$dataset, project = project(), var = input$var, type = input$type, 
                   group = input$grp,date = input$date, filter_date = input$filter_date, 
                   date_value = date_value(), filter_by = input$filter_by, filter_value = input$filter_by_val, 
                   filter_expr = input$filter_expr, facet_by = input$fct, scale = input$scale, 
                   tran = input$tran, bw = input$bw, position = input$position) 
    })
    
    output$plot <- renderPlot({den_out()})
    
    exp_out <- RexpressionServ("exp", values)
    output$result <- renderUI({exp_out()})
  })
}


# Vessel Count ====

vessel_serv <- function(id, values, project) {
  
  moduleServer(id, function(input, output, session) {
    
    
    saveOutputServ("saveOut", fun_id = "ves", project = project, 
                   fun_name = "vessel_count", tab_plot = v_out, 
                   out = reactive(input$out))
    
    closeAppServ("close")
    
    refreshServ("refresh", values, project)
    
    ns <- session$ns
    output$var_select <- renderUI({
      
      selectizeInput(ns("var"), "Vessel identifier", choices = colnames(values$dataset), multiple = FALSE)
    })
    
    output$date_select <- renderUI({
      
     # tagList(
        conditionalPanel("input.date_cb", ns = ns, 
      
          selectizeInput(ns("date"), "Date variable (x-axis)", 
                      choices = date_cols(values$dataset), multiple = TRUE,
                      options = list(maxItems = 1, create = TRUE,
                                     placeholder='Select or type variable name')))
    #  )
    })
    
    output$grp_select <- renderUI({
      
      selectizeInput(ns("grp"), "Select group variables (optional)",choices = colnames(values$dataset), 
                     multiple = TRUE, options = list(create = TRUE))
    })
    
    output$fct_select <- renderUI({
      
      selectizeInput(ns("fct"), "Split plot by (optional)", choices = c("year", "month", "week", colnames(values$dataset)),
                     multiple = TRUE, options = list(maxItems = 2))
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
      
      selectizeInput(ns("filter_by"), "Subset by variable",
                     choices = names(values$dataset), multiple = TRUE, options = list(maxItems = 1)) 
    })
    
    output$filter_by_val_UIOutput <- renderUI({
      
      tagList(  
        conditionalPanel("typeof input.filter_by !== 'undefined' && input.filter_by.length > 0", ns = ns,
                         style = "margin-left:19px;",
                         
                         selectizeInput(ns("filter_by_val"), "Select values to subset by",
                                        choices = filter_val(),
                                        multiple = TRUE, options = list(maxOptions = 15, placeholder = "Select or type value name")))
      )
    })
    
    output$filter_date_UIOutput <- renderUI({
      
      if (input$date_cb == TRUE) {
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
      }
    })
    
    observeEvent(input$date_cb == FALSE, {
      
      updateSelectizeInput(session, "period", choices = c("year-month" = "year_month", "month-year" = "month_year", 
                                                          "year", "month", "weeks", "day of the month" = "day",
                                                          "day of the year" = "day_of_year", "weekday"),
                           options = list(maxItems = 1), selected = NULL)
      
      updateSelectizeInput(session, "date", choices = date_cols(values$dataset),
                           options = list(maxItems = 1), selected = NULL)
      
      updateSelectizeInput(session, "filter_date",choices = c("date range" = "date_range", "year-month", 
                                                              "year-week", "year-day", "year", "month", "week", "day"),
                           options = list(maxItems = 1), selected = NULL)
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
    
    v_out <- eventReactive(input$fun_run, {
      
      validate_date(input$date, input$period, input$filter_date, input$fct, input$grp)
      
      vessel_count(values$dataset, project = project(), v_id = input$var, date = input$date,
                   period = input$period, group = input$grp, filter_date = input$filter_date,
                   date_value = date_value(), filter_by = input$filter_by, filter_value = input$filter_by_val, 
                   filter_expr = input$filter_expr, facet_by = input$fct, combine = input$combine,
                   position = input$position, tran = input$tran, value = input$value,
                   scale = input$scale, type = input$type, output = input$out)
    })
    
    tabplot <- eventReactive(input$fun_run, {
      
      tabplot_output(v_out(), input$out)
    })
    
    output$output <- renderUI({tabplot()})
    
    exp_out <- RexpressionServ("exp", values)
    output$result <- renderUI({exp_out()})
    
  })
}

# Species Catch ====

species_serv <- function(id, values, project) {
  
  moduleServer(id, function(input, output, session) {
    
    saveOutputServ("saveOut", "spec", project = project, fun_name = "species_catch", 
                   tab_plot = spec_out, out = reactive(input$out))
    
    closeAppServ("close")
    
    refreshServ("refresh", values, project)
    
    ns <- session$ns
    
    output$var_select <- renderUI({
      
      selectizeInput(ns("var"), "Catch variable",
                     choices = c(numeric_cols(values$dataset)), multiple = TRUE)
    })
    
    output$date_select <- renderUI({
      tagList(
      conditionalPanel("input.date_cb", ns = ns, 
      
        selectizeInput(ns("date"), "Date variable (x-axis)",
                    choices = c(date_cols(values$dataset)), multiple = TRUE,
                    options = list(maxItems = 1, create = TRUE)))
      )
    })
    
    output$grp_select <- renderUI({
      
      selectizeInput(ns("grp"), "Select group variables (optional)",
                     choices = c(colnames(values$dataset)), multiple = TRUE)
    })
    
    output$fct_select <- renderUI({
      
      selectizeInput(ns("fct"), "split plot by (optional)",
                     choices = c("year", "month", "week", "species", colnames(values$dataset)),
                     multiple = TRUE, options = list(maxItems = 2))
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
     
        selectizeInput(ns("filter_by"), "Subset by variable",
        choices = names(values$dataset), multiple = TRUE, options = list(maxItems = 1)) 
    })
    
    output$filter_by_val_UIOutput <- renderUI({
    
      tagList(  
        conditionalPanel("typeof input.filter_by !== 'undefined' && input.filter_by.length > 0", ns = ns,
                         style = "margin-left:19px;",
        
        selectizeInput(ns("filter_by_val"), "Select values to subset by",
                    choices = filter_val(),
                    multiple = TRUE, options = list(maxOptions = 15, placeholder = "Select or type value name")))
      )
    })
    
    output$filter_date_UIOutput <- renderUI({
     
      if (input$date_cb == TRUE) {
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
      }
    })
    
    observeEvent(input$date_cb == FALSE, {
      
      updateSelectizeInput(session, "period", choices = c("year-month" = "year_month", "month-year" = "month_year",
                                                          "year", "month", "weeks", "day of the month" = "day",
                                                         "day of the year" = "day_of_year", "weekday"),
                           options = list(maxItems = 1), selected = NULL)
      
      updateSelectizeInput(session, "date", choices = date_cols(values$dataset),
                           options = list(maxItems = 1), selected = NULL)
      
      updateSelectizeInput(session, "filter_date",choices = c("date range" = "date_range", "year-month", 
                                                              "year-week", "year-day", "year", "month", "week", "day"),
                           options = list(maxItems = 1), selected = NULL)
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
    
    spec_out <- eventReactive(input$fun_run, {
      
      validate(need(input$var, "Please select a species catch variable."))
      
      validate_date(input$date, input$period, input$filter_date, input$fct, input$grp)
      
      species_catch(values$dataset, project = project(), species = input$var, date = input$date,
                    period = input$period, fun = input$fun, group = input$grp,
                    filter_date = input$filter_date, date_value = date_value(),
                    filter_by = input$filter_by, filter_value = input$filter_by_val, 
                    filter_expr = input$filter_expr, facet_by = input$fct, combine = input$combine,
                    position = input$position, tran = input$tran, value = input$value,
                    scale = input$scale, type = input$type, output = input$out,
                    format_tab = input$format)
    })
    
    tabplot <- eventReactive(input$fun_run, {
      
      tabplot_output(spec_out(), input$out)
    })
    
    output$output <- renderUI({tabplot()})
    
    exp_out <- RexpressionServ("exp", values)
    output$result <- renderUI({exp_out()})
  })
}

# Rolling Catch ====

roll_serv <- function(id, values, project) {
  
  moduleServer(id, function(input, output, session) {
    
    saveOutputServ("saveOut", "roll", project = project, fun_name = "roll_catch",
                   tab_plot = roll_out, out = reactive(input$out))
    
    closeAppServ("close")
    
    refreshServ("refresh", values, project)
    
    ns <- session$ns
    
    output$var_select <- renderUI({
      
      selectizeInput(ns("var"), "Select catch variable",
                     choices = c(numeric_cols(values$dataset)), multiple = TRUE)
    })
    
    output$date_select <- renderUI({
      
      selectInput(ns("date"), "Date variable (x-axis)",
                  choices = c(date_cols(values$dataset)))
    })
    
    output$grp_select <- renderUI({
      
      selectizeInput(ns("grp"), "Select group variables (optional)",
                     choices = c(colnames(values$dataset)), multiple = TRUE)
    })
    
    output$fct_select <- renderUI({
      
      selectizeInput(ns("fct"), "split plot by (optional)",
                     choices = c("year", "month", "week", "species", colnames(values$dataset)),
                     multiple = TRUE, options = list(maxItems = 2))
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
      
      selectizeInput(ns("filter_by"), "Subset by variable",
                     choices = names(values$dataset), multiple = TRUE, options = list(maxItems = 1)) 
    })
    
    output$filter_by_val_UIOutput <- renderUI({
      
      tagList(  
        conditionalPanel("typeof input.filter_by !== 'undefined' && input.filter_by.length > 0", ns = ns,
                         style = "margin-left:19px;",
                         
                         selectizeInput(ns("filter_by_val"), "Select values to subset by",
                                        choices = filter_val(),
                                        multiple = TRUE, options = list(maxOptions = 15, placeholder = "Select or type value name")))
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
    
    roll_out <- eventReactive(input$fun_run, {
      
      validate(need(input$date, "Please select a date variable."),
               need(input$var, "Please select a species catch variable."))
      
      validate_date(input$date, input$filter_date, input$fct, input$grp)
      
      roll_catch(values$dataset, project = project(), catch = input$var, date = input$date,
                 fun = input$fun, group = input$grp, filter_date = input$filter_date,
                 date_value = date_value(), filter_by = input$filter_by, 
                 filter_value = input$filter_by_val, filter_expr = input$filter_expr, 
                 facet_by = input$fct, tran = input$tran, scale = input$scale, output = input$out)
    })
    
    tabplot <- eventReactive(input$fun_run, {
      
      tabplot_output(roll_out(), input$out)
    })
    
    output$output <- renderUI({tabplot()})
    
    exp_out <- RexpressionServ("exp", values)
    output$result <- renderUI({exp_out()})
  })
}

# Weekly Catch =====

weekly_catch_serv <- function(id, values, project) {
  
  moduleServer(id, function(input, output, session) {
    
    saveOutputServ("saveOut", "wc", project = project, fun_name = "weekly_catch", 
                   tab_plot = wc_out, out = reactive(input$out))
    
    closeAppServ("close")
    
    refreshServ("refresh", values, project)
    
    ns <- session$ns
    
    output$var_select <- renderUI({
      
      selectizeInput(ns("var"), "Select catch variable",
                     choices = c(numeric_cols(values$dataset)), multiple = TRUE)
    })
    
    output$date_select <- renderUI({
      
      selectInput(ns("date"), "Date variable (x-axis)",
                  choices = c(date_cols(values$dataset)))
    })
    
    output$grp_select <- renderUI({
      
      selectizeInput(ns("grp"), "Select group variables (optional)",
                     choices = c(colnames(values$dataset)), multiple = TRUE)
    })
    
    output$fct_select <- renderUI({
      
      selectizeInput(ns("fct"), "Split plot by (optional)",
                     choices = c("year", "month", "species", colnames(values$dataset)),
                     multiple = TRUE, options = list(maxItems = 2))
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
      
      selectizeInput(ns("filter_by"), "Subset by variable",
                     choices = names(values$dataset), multiple = TRUE, options = list(maxItems = 1)) 
    })
    
    output$filter_by_val_UIOutput <- renderUI({
      
      tagList(  
        conditionalPanel("typeof input.filter_by !== 'undefined' && input.filter_by.length > 0", ns = ns,
                         style = "margin-left:19px;",
                         
                         selectizeInput(ns("filter_by_val"), "Select values to subset by",
                                        choices = filter_val(),
                                        multiple = TRUE, options = list(maxOptions = 15, 
                                                                        placeholder = "Select or type value name")))
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
    
    wc_out <- eventReactive(input$fun_run, {
      
      validate(need(input$date, "Please select a date variable."),
               need(input$var, "Please select a species catch variable."))
      
      validate_date(input$date, input$filter_date, input$fct, input$grp)
      
      weekly_catch(values$dataset, project = project(), species = input$var, date = input$date,
                   fun = input$fun, group = input$grp, filter_date = input$filter_date, 
                   date_value = date_value(), filter_by = input$filter_by, filter_value = input$filter_by_val, 
                   filter_expr = input$filter_expr, facet_by = input$fct, combine = input$combine,
                   position = input$position, tran = input$tran, value = input$value,
                   scale = input$scale, type = input$type, output = input$out,
                   format_tab = input$format)
    })
    
    tabplot <- eventReactive(input$fun_run, {
      
      tabplot_output(wc_out(), input$out)
    })
    
    output$output <- renderUI({tabplot()})
    
    exp_out <- RexpressionServ("exp", values)
    output$result <- renderUI({exp_out()})
  })
}

# Weekly Effort ====

weekly_effort_serv <- function(id, values, project) {
  
  moduleServer(id, function(input, output, session) {
    
    saveOutputServ("saveOut", fun_id = "we", project = project, fun_name = "weekly_effort", 
                   tab_plot = we_out, out = reactive(input$out))
    
    closeAppServ("close")
    
    refreshServ("refresh", values, project)
    
    ns <- session$ns
    
    output$var_select <- renderUI({
      
      selectizeInput(ns("var"), "Select CPUE variable",
                     choices = c(numeric_cols(values$dataset)), multiple = TRUE)
    })
    
    output$date_select <- renderUI({
      selectInput(ns("date"), "Date variable (x-axis)",
                  choices = c(date_cols(values$dataset)))
    })
    
    output$grp_select <- renderUI({
      selectizeInput(ns("grp"), "Select group variables (optional)",
                     choices = c(colnames(values$dataset)), multiple = TRUE)
    })
    
    output$fct_select <- renderUI({
      selectizeInput(ns("fct"), "Split plot by (optional)",
                     choices = c("year", "month", "species", colnames(values$dataset)),
                     multiple = TRUE, options = list(maxItems = 2))
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
      
      selectizeInput(ns("filter_by"), "Subset by variable",
                     choices = names(values$dataset), multiple = TRUE, options = list(maxItems = 1)) 
    })
    
    output$filter_by_val_UIOutput <- renderUI({
      
      tagList(  
        conditionalPanel("typeof input.filter_by !== 'undefined' && input.filter_by.length > 0", ns = ns,
                         style = "margin-left:19px;",
                         
                         selectizeInput(ns("filter_by_val"), "Select values to subset by",
                                        choices = filter_val(),
                                        multiple = TRUE, options = list(maxOptions = 15, 
                                                                        placeholder = "Select or type value name")))
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
    
    we_out <- eventReactive(input$fun_run, {
      
      validate(need(input$date, "Please select a date variable."),
               need(input$var, "Please select a cpue variable."))
      
      validate_date(input$date, input$filter_date, input$fct, input$grp)
      
      weekly_effort(values$dataset, project = project(), cpue = input$var, date = input$date,
                    group = input$grp, filter_date = input$filter_date, date_value = date_value(),
                    filter_by = input$filter_by, filter_value = input$filter_by_val, 
                    filter_expr = input$filter_expr, facet_by = input$fct, combine = input$combine, 
                    tran = input$tran, scale = input$scale, output = input$out, format_tab = input$format)
    })
    
    tabplot <- eventReactive(input$fun_run, {
      
      tabplot_output(we_out(), input$out)
    })
    
    output$output <- renderUI({tabplot()})
    
    exp_out <- RexpressionServ("exp", values)
    output$result <- renderUI({exp_out()})
  })
}

# Bycatch ====

bycatch_serv <- function(id, values, project) {
  
  moduleServer(id, function(input, output, session) {
    
    saveOutputServ("saveOut", fun_id = "by", project = project, fun_name = "bycatch",
                   tab_plot = by_out, out = reactive(input$out))
    
    closeAppServ("close")
    
    refreshServ("refresh", values, project)
    
    ns <- session$ns
    
    output$cpue_select <- renderUI({
      selectizeInput(ns("cpue"), "Select CPUE variable(s)",
                     choices = c(numeric_cols(values$dataset)), multiple = TRUE)
    })
    
    output$catch_select <- renderUI({
      selectizeInput(ns("catch"), "Select catch variable(s)",
                     choices = c(numeric_cols(values$dataset)), multiple = TRUE)
    })
    
    output$date_select <- renderUI({
      selectInput(ns("date"), "Date variable (x-axis)",
                  choices = c(date_cols(values$dataset)))
    })
    
    output$grp_select <- renderUI({
      selectizeInput(ns("grp"), "Select group variables (optional)",
                     choices = c(colnames(values$dataset)), multiple = TRUE)
    })
    
    output$fct_select <- renderUI({
      selectizeInput(ns("fct"), "Split plot by (optional)",
                     choices = c("year", "month", "week", colnames(values$dataset)),
                     multiple = TRUE, 
                     options = list(maxItems = 2,
                                    placeholder = "Limit 2 (facet 1 x facet2)"))
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
      
      selectizeInput(ns("filter_by"), "Subset by variable",
                     choices = names(values$dataset), multiple = TRUE, options = list(maxItems = 1)) 
    })
    
    output$filter_by_val_UIOutput <- renderUI({
      
      tagList(  
        conditionalPanel("typeof input.filter_by !== 'undefined' && input.filter_by.length > 0", ns = ns,
                         style = "margin-left:19px;",
                         
                         selectizeInput(ns("filter_by_val"), "Select values to subset by",
                                        choices = filter_val(),
                                        multiple = TRUE, options = list(maxOptions = 15, 
                                                                        placeholder = "Select or type value name")))
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
    
    names <- reactiveValues(names = NULL)
    
    observeEvent(input$nms_add, {
      
      names$names <- c(names$names, input$nms)
    })
    
    observeEvent(input$nms_clear, {
      
      names$names <- NULL
    })
    
    output$caption <- renderText({names$names})
    
    by_out <- eventReactive(input$fun_run, {
      
      validate(need(input$date, "Please select a date variable."))
      
      validate_date(input$date, input$filter_date, input$fct, input$grp)
      
      bycatch(values$dataset, project = project(), cpue = input$cpue, catch = input$catch, 
              date = input$date, group = input$grp, names = names$names, period = input$period,
              filter_date = input$filter_date, date_value = date_value(), filter_by = input$filter_by, 
              filter_value = input$filter_by_val, filter_expr = input$filter_expr,  facet_by = input$fct,
              value = input$value, combine = input$combine, tran = input$tran,
              scale = input$scale, output = input$out, format_tab = input$format)
    })
    
    tabplot <- eventReactive(input$fun_run, {
      
      tabplot_output(by_out(), input$out)
    })
    
    output$output <- renderUI({tabplot()})
    
    exp_out <- RexpressionServ("exp", values)
    output$result <- renderUI({exp_out()})
  })
}

# Trip Length ====

trip_serv <- function(id, values, project) {
  
  moduleServer(id, function(input, output, session) {
    
    saveOutputServ("saveOut", fun_id = "trip", project = project, 
                   fun_name = "trip_length", tab_plot = trip_out, 
                   out = reactive(input$out))
    
    closeAppServ("close")
    
    refreshServ("refresh", values, project)
    
    ns <- session$ns
    
    output$start_select <- renderUI({
      selectInput(ns("start"), "Trip start date",
                  choices = c(date_cols(values$dataset)))
    })
    
    output$end_select <- renderUI({
      selectInput(ns("end"), "Trip end date",
                  choices = c(date_cols(values$dataset)))
    })
    
    output$catch_select <- renderUI({
      selectizeInput(ns("catch"), "Select catch variable(s) (optional)",
                     choices = c(numeric_cols(values$dataset)), multiple = TRUE)
    })
    
    output$haul_select <- renderUI({
      selectizeInput(ns("haul"), "Select haul variable (optional)",
                     choices = c(numeric_cols(values$dataset)), multiple = TRUE,
                     options = list(maxItems = 1))
    })
    
    output$grp_select <- renderUI({
      selectizeInput(ns("grp"), "Group variables (optional)",
                     choices = c("year", "month", "week", colnames(values$dataset)), multiple = TRUE)
    })
    
    output$fct_select <- renderUI({
      selectizeInput(ns("fct"), "Split plot by (optional)",
                     choices = c("year", "month", "week", colnames(values$dataset)),
                     multiple = TRUE, options = list(maxItems = 2, placeholder = "Limit 2"))
    })
    
    output$kwargs_select <- renderUI({
      selectizeInput(ns("htp_kwargs"), "Columns indentifying unique trips (optional)",
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
      
      selectizeInput(ns("filter_by"), "Subset by variable",
                     choices = names(values$dataset), multiple = TRUE, options = list(maxItems = 1)) 
    })
    
    output$filter_by_val_UIOutput <- renderUI({
      
      tagList(  
        conditionalPanel("typeof input.filter_by !== 'undefined' && input.filter_by.length > 0", ns = ns,
                         style = "margin-left:19px;",
                         
                         selectizeInput(ns("filter_by_val"), "Select values to subset by",
                                        choices = filter_val(),
                                        multiple = TRUE, options = list(maxOptions = 15, 
                                                                        placeholder = "Select or type value name")))
      )
    })
    
    output$filter_date_UIOutput <- renderUI({
      
      if (!is.null(input$filter_date)) { 
        if (input$filter_date == "date_range") {
          
          dateRangeInput(ns("date_range"), label = "Date range",
                         start = min(values$dataset[[input$start]], na.rm = TRUE),
                         end = max(values$dataset[[input$start]], na.rm = TRUE))
          
        } else {
          
          filter_periodUI(id, values$dataset, input$start, input$filter_date)
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
    
    trip_out <- eventReactive(input$fun_run, {
      
      validate(need(input$start, "Please select a start date variable."),
               need(input$end, "Please select an end date variable."))
      
      trip_length(values$dataset, project = project(), start = input$start, end = input$end,
                  units = input$unit, catch = input$catch, hauls = input$haul,
                  group = input$grp, filter_date = input$filter_date, date_value = date_value(),
                  filter_by = input$filter_by, filter_value = input$filter_by_val, 
                  filter_expr = input$filter_expr, facet_by = input$fct, density = input$dens, tran = input$tran,
                  scale = input$scale, output = input$out, pages = input$pages, remove_neg = input$rm_neg,
                  type = input$type, format_tab = input$format, bins = input$bins,
                  haul_to_trip = input$haul_trp, input$htp_kwargs)
    })
    
    tabplot <- eventReactive(input$fun_run, {
      
      tabplot_output(trip_out(), input$out)
    })
    
    output$output <- renderUI({tabplot()})
    
    exp_out <- RexpressionServ("exp", values)
    output$result <- renderUI({exp_out()})
  })
}
# Fleet Table ====

# expr builder add row server
nexpr_row_server <- function(id, values) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    output$varUI <- renderUI({
      selectInput(ns("var"), "", 
                  choices = colnames(values$dataset), multiple = FALSE)
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
      
      if (input$oper == "%in%") {
        textInput(ns("value"), "",
                  value = "c( add choices here )",
                  placeholder = 'e.g. c("Port A", "Port D")')
      } else {
        
        #textInput(ns("value"), "")
        selectizeInput(ns("value"), "",
                       choices = unique_values(),   
                       multiple = TRUE, options = list(maxOptions = 15, maxItems = 1, 
                                                       placeholder = "Select or type value name",
                                                       create = TRUE))
      }
    })
  })
}

fleet_table_serv <- function(id, values, project) {
  
  moduleServer(id, function(input, output, session) {
    
    closeAppServ("close")
    
    refreshServ("refresh", values, project)
    
    ns <- session$ns
    
    # expression builder ----
    
    rv <- reactiveValues(expr = character(0),
                         expr_num = 1,
                         select = list(target = "row"))
    
    output$select_var <- renderUI({
      
      selectInput(ns("nexpr_1-var"), "Variable", 
                  choices = colnames(values$dataset), multiple = FALSE)
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
      
      if (input[["nexpr_1-oper"]] == "%in%") {
        textInput(ns("nexpr_1-value"), "Value", value = "c( add choices here )",
                  placeholder = 'e.g. c("Port A", "Port D")')
      #   selectizeInput(ns("nexpr_1-value"), "Value",
      #                  choices = unique_values(),
      #                  multiple = TRUE, options = list(maxOptions = 15,
      #                                                  placeholder = "Select or type value name",
      #                                                  create = TRUE))
      } else {
        #textInput(ns("nexpr_1-value"), "Value")
        selectizeInput(ns("nexpr_1-value"), "Value",
                       choices = unique_values(),
                       multiple = TRUE, options = list(maxOptions = 15, maxItems = 1,
                                                       placeholder = "Select or type value name",
                                                       create = TRUE))
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
        ui = nexpr_row_ui(ui_id) # run expr row module
      )
      
      nexpr_row_server(server_id, values) # run expr row server
    })
    
    
    # reactive containing expression that updates whenever an expr selector is changed
    expr <- reactive({
      
      req(input[["nexpr_1-var"]]) # prevents reactive from running early
      
      expr_list <- lapply(seq(rv$expr_num), function(x) {
        
        new_id <- paste0("nexpr_", x)
        value <- ""
        var_class <- class(values$dataset[[input[[paste0(new_id, "-var")]]]])
        
        if (is_empty(input[[paste0(new_id, "-value")]]) == FALSE) {
          
         # check if var should be wrapped in quotes
          if (any(var_class %in% c("character", "factor", "date", "POSIXct", "POSIXt"))) {
            
            value <- paste0("'", input[[paste0(new_id, "-value")]], "'") # add single quotes
          } else {
            value <- input[[paste0(new_id, "-value")]] # otherwise, assign original value
          }
        }
        
        paste(input[[paste0(new_id, "-log_oper")]], input[[paste0(new_id, "-var")]], 
              input[[paste0(new_id,"-oper")]], value)
      })
      
      paste(expr_list, collapse = " ")
    })
   
    # display current expression 
    output$expr_txt <- renderText(expr())
    
    # remove expression UI
    observeEvent(input$reset_expr, {
     
      rv$expr_num <- 1
      removeUI(selector = ".n-expr-section", multiple = TRUE)
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
      
      upload <- FishSET::read_dat(input$file$datapath)
      
      # convert factors to strings
      if (any(vapply(upload, is.factor, FUN.VALUE = logical(1)))) {
        
        fac <- vapply(upload, is.factor, FUN.VALUE = logical(1))
        
        for (i in names(upload[fac])) {
          
          upload[[i]] <- as.character(upload[[i]])
        }
        
        f_r$f_DT <- upload
        f_r$row <- upload[1, ]
        
        for (i in seq_along(f_r$row)) {
          f_r$row[[i]] <- "enter value"
        }
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
      DT::replaceData(proxy_f, f_r$f_DT, resetPaging = FALSE)
    })
    
    # insert expression from builder
    observeEvent(input$insert_expr, {
      
      if (dim(input$f_tab_cells_selected)[1] == 0) {
        
        showNotification("Select a condition cell", type = "warning")
        
      } else {
        
        f_r$f_DT[input$f_tab_cells_selected] <- expr() #rv$expr
      }
    })
    
    # render fleet table
    output$f_tab <- DT::renderDataTable(f_r$f_DT, editable  = "all", 
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
                                 "Greater than or equal to", "Equal to", "Not equal to", 
                                 "Contains", "Logical NOT", "Logical AND", "Logical OR"))
    })
    # run fleet_table/save table
    observeEvent(input$save, {
      
      fleet_table(values$dataset, project = project(), table = f_r$f_DT, save = TRUE)
      showNotification("Table saved.", type = 'message', duration = 10)
    })
    
    exp_out <- RexpressionServ("exp", values)
    output$result <- renderUI({exp_out()})
  })
}

# Fleet Assign ====

fleet_assign_serv <- function(id, values, project) {
  
  moduleServer(id, function(input, output, session) {
    
    closeAppServ("close")
    
    refreshServ("refresh", values, project)
    
    #saveDataTableServ("saveDat", values, project)
    
    fleet_tab_db <- reactive({
      
      input$refresh_tabs
      
      grep("FleetTable", tables_database(), value = TRUE)
    })
    
    output$available_tabs <- renderUI({
      
      selectInput(session$ns("tab"), "Available fleet tables",
                  choices = fleet_tab_db()) 
    })
    
    
    tab_view <- eventReactive(input$load_btn, {
      
      table_view(input$tab)
    })
    
    output$tab_preview <- DT::renderDT({tab_view()})
    
    fa_out <- eventReactive(input$fun_run, {
      
      fleet_assign(values$dataset, project = project(), input$tab, 
                   overlap = input$overlap, format_tab = input$format)
    })
    
    output$final_tab <- DT::renderDT({fa_out()})
    
    observeEvent(input$saveData, {
        
        values$dataset <- fa_out()
        suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase()))
        DBI::dbWriteTable(fishset_db, paste0(project(), 'MainDataTable'), values$dataset, overwrite = TRUE)
        DBI::dbDisconnect(fishset_db)
        showNotification('Data saved to FishSET database', type = 'message', duration = 10)
    })
    
    output$plot <- renderPlot({
      
      if (input$format == "long") {
        
        ggplot2::ggplot(fa_out(), ggplot2::aes(fleet, fill = fleet)) + 
          ggplot2::geom_bar() + ggplot2::coord_flip()
        
      } else {
        
        value <- vapply(fa_out()[tab_view()$fleet], sum, FUN.VALUE = numeric(1))
        df <- data.frame(fleet = tab_view()$fleet, value = value)
        ggplot2::ggplot(df, ggplot2::aes(fleet, value, fill = fleet)) + 
          ggplot2::geom_col() + ggplot2::coord_flip()
      }
    })
    
    exp_out <- RexpressionServ("exp", values)
    output$result <- renderUI({exp_out()})
  })
}