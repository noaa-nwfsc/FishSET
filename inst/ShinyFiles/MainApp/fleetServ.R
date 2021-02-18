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
      req(project)
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
    
    saveOutputServ("saveOut", fun_id = "den", project = project, 
                   fun_name = "density_plot", tab_plot = den_out, 
                   out = function() "plot")
    
    ns <- session$ns
    
    output$var_select <- renderUI({
      
      selectInput(ns("var"), "Select variable",
                  choices = c(numeric_cols(values$dataset)))
    })
    
    output$grp_select <- renderUI({
      
      selectizeInput(ns("grp"), "grouping variables",
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
    
    # date col for subset, group, and split sections
    date_sgs <- reactive({
      
      cols <- c(input$date, input$grp_date, input$fct_date)
      if (!is.null(cols)) {
        cols
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
      
      density_plot(values$dataset, project = project(), var = input$var, type = input$type, 
                   group = input$grp, combine = input$combine, date = date_sgs(), 
                   filter_date = input$filter_date, date_value = date_value(), 
                   filter_by = input$filter_by, filter_value = input$filter_by_val, 
                   facet_by = input$fct, scale = input$scale, tran = input$tran, 
                   bw = input$bw, position = input$position, pages = input$pages) 
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
    
    
    saveOutputServ("saveOut", fun_id = "ves", project = project, 
                   fun_name = "vessel_count", tab_plot = v_out, 
                   out = reactive(input$out))
    
    ns <- session$ns
    output$var_select <- renderUI({
      
      selectizeInput(ns("var"), "Vessel identifier", choices = colnames(values$dataset), multiple = FALSE)
    })
    
    output$date_select <- renderUI({
      
      selectizeInput(ns("date"), "Date variable",
                     choices = c(date_cols(values$dataset)), multiple = TRUE,
                     options = list(maxItems = 1, create = TRUE,
                                    placeholder = "Select or type variable name"))
    })
    
    output$sub_date_select <- renderUI({
      
      selectizeInput(ns("sub_date"), "Date variable", 
                     choices = date_cols(values$dataset), multiple = TRUE,
                     options = list(maxItems = 1, create = TRUE,
                                    placeholder = "Select or type variable name"))
    })
    
    output$grp_select <- renderUI({
      
      selectizeInput(ns("grp"), "Select group variables", 
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
      
      updateSelectInput(session, "date",  choices = date_cols(values$dataset))
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
    
    date_value <- reactive({
      
      if (!is.null(input$sub_date) & !is.null(input$filter_date)) {
        
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
    sub_date_col <- reactive({
      
      cols <- c(input$sub_date, input$grp_date, input$fct_date)
      if (!is.null(cols)) {
        cols
      } else {
        NULL
      }
    }) 
    
    v_out <- eventReactive(input$fun_run, {
      
      validate_date(date = input$date, sub_date = sub_date_col(), period = input$period, 
                    filter_date = input$filter_date, fct = input$fct, grp = input$grp)
      
      vessel_count(values$dataset, project = project(), v_id = input$var, date = input$date,
                   period = input$period, group = input$grp, sub_date = sub_date_col(),
                   filter_date = input$filter_date, date_value = date_value(), 
                   filter_by = input$filter_by, filter_value = input$filter_by_val, 
                   filter_expr = input$filter_expr, facet_by = input$fct, combine = input$combine,
                   position = input$position, tran = input$tran, value = input$value,
                   scale = input$scale, type = input$type, output = input$out)
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
    
    saveOutputServ("saveOut", "spec", project = project, fun_name = "species_catch",
                   tab_plot = spec_out, out = reactive(input$out))
    
    ns <- session$ns
    
    output$var_select <- renderUI({
      
      selectizeInput(ns("var"), "Catch variable",
                     choices = c(numeric_cols(values$dataset)), multiple = TRUE)
    })
    
    output$date_select <- renderUI({
      
       selectizeInput(ns("date"), "Date variable",
                    choices = c(date_cols(values$dataset)), multiple = TRUE,
                    options = list(maxItems = 1, create = TRUE,
                                   placeholder = "Select or type variable name"))
    })
    
    output$sub_date_select <- renderUI({
      
      selectizeInput(ns("sub_date"), "Date variable", 
                     choices = date_cols(values$dataset), multiple = TRUE,
                     options = list(maxItems = 1, create = TRUE,
                                    placeholder = "Select or type variable name"))
    })
    
    output$grp_select <- renderUI({
      
      selectizeInput(ns("grp"), "Select group variables",
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
      
      updateSelectInput(session, "date",  choices = date_cols(values$dataset))
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
      
      updateSelectizeInput(session, "fct", choices = c("year", "month", "week", "species",
                                                       category_cols(values$dataset)),
                           selected = NULL,
                           options = list(maxItems = 2))
      
      updateSelectizeInput(session, "fct_date", choices = date_cols(values$dataset),
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
        
      } else {
        
        NULL
      }
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
    
    spec_out <- eventReactive(input$fun_run, {
      
      validate(need(input$var, "Please select a catch variable."))
      
      validate_date(date = input$date, sub_date = sub_date_col(), period = input$period, 
                    filter_date = input$filter_date, fct = input$fct, grp = input$grp)
      
      species_catch(values$dataset, project = project(), species = input$var, date = input$date,
                    period = input$period, fun = input$fun, group = input$grp, sub_date = sub_date_col(),
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
  })
}

# Rolling Catch ====

roll_serv <- function(id, values, project) {
  
  moduleServer(id, function(input, output, session) {
    
    saveOutputServ("saveOut", "roll", project = project, fun_name = "roll_catch",
                   tab_plot = roll_out, out = reactive(input$out))
    
    ns <- session$ns
    
    output$var_select <- renderUI({
      
      selectizeInput(ns("var"), "Select catch variable",
                     choices = numeric_cols(values$dataset), multiple = TRUE)
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
      
      selectizeInput(ns("grp"), "Select group variables",
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
      
      roll_catch(values$dataset, project = project(), catch = input$var, date = input$date,
                 fun = input$fun, group = input$grp, combine = input$combine, 
                 sub_date = sub_date_col(), filter_date = input$filter_date,
                 date_value = date_value(), filter_by = input$filter_by, 
                 filter_value = input$filter_by_val, filter_expr = input$filter_expr, 
                 facet_by = input$fct, tran = input$tran, scale = input$scale, output = input$out)
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
    
    saveOutputServ("saveOut", "wc", project = project, fun_name = "weekly_catch", 
                   tab_plot = wc_out, out = reactive(input$out))
    
    ns <- session$ns
    
    output$var_select <- renderUI({
      
      selectizeInput(ns("var"), "Select catch variable",
                     choices = c(numeric_cols(values$dataset)), multiple = TRUE)
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
      
      selectizeInput(ns("grp"), "Select group variables",
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
      if (!is.null(cols)) {
        cols
      } else {
        NULL
      }
    }) 
    
    wc_out <- eventReactive(input$fun_run, {
      
      validate(need(input$date, "Please select a date variable."),
               need(input$var, "Please select a catch variable."))
      
      validate_date(date = input$date, sub_date = sub_date_col(), 
                    filter_date = input$filter_date, fct = input$fct, grp = input$grp)
      
      weekly_catch(values$dataset, project = project(), species = input$var, date = input$date,
                   fun = input$fun, group = input$grp, sub_date = sub_date_col(),
                   filter_date = input$filter_date, filter_expr = input$filter_expr, 
                   facet_by = input$fct, combine = input$combine, position = input$position, 
                   tran = input$tran, value = input$value, scale = input$scale, 
                   type = input$type, output = input$out, format_tab = input$format)
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
    
    saveOutputServ("saveOut", fun_id = "we", project = project, fun_name = "weekly_effort", 
                   tab_plot = we_out, out = reactive(input$out))
    
    ns <- session$ns
    
    output$var_select <- renderUI({
      
      selectizeInput(ns("var"), "Select CPUE variable",
                     choices = numeric_cols(values$dataset), multiple = TRUE)
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
      
      selectizeInput(ns("grp"), "Select group variables",
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
      
      weekly_effort(values$dataset, project = project(), cpue = input$var, date = input$date,
                    group = input$grp, sub_date = sub_date_col(), filter_date = input$filter_date, 
                    date_value = date_value(), filter_by = input$filter_by, 
                    filter_value = input$filter_by_val,  filter_expr = input$filter_expr, 
                    facet_by = input$fct, combine = input$combine, tran = input$tran, 
                    scale = input$scale, output = input$out, format_tab = input$format)
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
    
    saveOutputServ("saveOut", fun_id = "by", project = project, fun_name = "bycatch",
                   tab_plot = by_out, out = reactive(input$out))
    
    ns <- session$ns
    
    output$cpue_select <- renderUI({
      selectizeInput(ns("cpue"), "Select CPUE variable(s)",
                     choices = numeric_cols(values$dataset), multiple = TRUE)
    })
    
    output$catch_select <- renderUI({
      selectizeInput(ns("catch"), "Select catch variable(s)",
                     choices = numeric_cols(values$dataset), multiple = TRUE)
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
      
      selectizeInput(ns("grp"), "Select group variables",
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
      
      bycatch(values$dataset, project = project(), cpue = input$cpue, catch = input$catch, 
              date = input$date, group = input$grp, names = names$names, period = input$period,
              sub_date = sub_date_col(), filter_date = input$filter_date, date_value = date_value(),
              filter_by = input$filter_by, filter_value = input$filter_by_val, 
              filter_expr = input$filter_expr,  facet_by = input$fct,value = input$value, 
              combine = input$combine, tran = input$tran,scale = input$scale, 
              output = input$out, format_tab = input$format)
    })
    
    tabplot <- eventReactive(input$fun_run, {
      
      tabplot_output(by_out(), input$out)
    })
    
    output$output <- renderUI({tabplot()})
  })
}

# Trip Length ====

trip_serv <- function(id, values, project) {
  
  moduleServer(id, function(input, output, session) {
    
    saveOutputServ("saveOut", fun_id = "trip", project = project, 
                   fun_name = "trip_length", tab_plot = trip_out, 
                   out = reactive(input$out))
    
    ns <- session$ns
    
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
                     choices = c(numeric_cols(values$dataset)), multiple = TRUE)
    })
    
    output$sub_date_select <- renderUI({
      
      selectizeInput(ns("sub_date"), "Date variable", 
                     choices = date_cols(values$dataset), multiple = TRUE,
                     options = list(maxItems = 1, create = TRUE,
                                    placeholder = "Select or type variable name"))
    })
    
    output$grp_select <- renderUI({
      
      selectizeInput(ns("grp"), "Select group variables",
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
      if (!is.null(cols)) {
        cols
      } else {
        NULL
      }
    }) 
    
    trip_out <- eventReactive(input$fun_run, {
      
      validate(need(input$start, "Please select a start date variable."),
               need(input$end, "Please select an end date variable."))
      
      trip_length(values$dataset, project = project(), start = input$start, end = input$end,
                  units = input$unit, vpue = input$vpue, haul_count = input$haul_count,
                  group = input$grp, sub_date = sub_date_col(), filter_date = input$filter_date, 
                  date_value = date_value(), filter_by = input$filter_by, 
                  filter_value = input$filter_by_val, filter_expr = input$filter_expr, 
                  facet_by = input$fct, density = input$dens, tran = input$tran,
                  scale = input$scale, output = input$out, pages = input$pages, 
                  remove_neg = input$rm_neg, type = input$type, bins = input$bins,
                  tripID = input$tripID, fun.time = input$fun_time, fun.numeric = input$fun_numeric)
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
                  options = list(maxIems = 1))
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
                  options = list(maxIems = 1))
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
      
      fleet_table(values$dataset, project = project(), table = f_r$f_DT, save = TRUE)
      showNotification("Table saved.", type = 'message', duration = 10)
    })
  })
}

# Fleet Assign ====

fleet_assign_serv <- function(id, values, project) {
  
  moduleServer(id, function(input, output, session) {
    
    fleet_tab_db <- reactive({
      
      input$refresh_tabs
      
      grep("FleetTable", tables_database(), value = TRUE)
    })
    
    output$available_tabs <- renderUI({
      
      selectInput(session$ns("tab"), "Available fleet tables",
                  choices = fleet_tab_db()) 
    })
    
    f_tab <- reactiveValues()
    
    observeEvent(input$load_btn, {
      
      f_tab$tab <- table_view(input$tab)
    })
    
    output$tab_preview <- DT::renderDT(f_tab$tab,
                                       selection = list(target = "row"),
                                       escape = FALSE)
    
    fa_out <- eventReactive(input$fun_run, {
      validate(need(f_tab$tab, "Please upload a fleet table."))
      
      fleet_assign(values$dataset, project = project(), fleet_tab = input$tab, 
                   overlap = input$overlap, format_var = input$format, 
                   assign = input$tab_preview_rows_selected)
    })
    
    observeEvent(input$fun_run, {
      
      if (is.null(fa_out())) {
        showNotification("Overlap detected, no fleets assigned.", type = "warning")
      
      } else {
        
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