source("fleetUI.R", local = TRUE)
source("fleet_helpers.R", local = TRUE)

  
  # Density Plot ====
  
  density_serv <- function(id, dat, project) {
    
    moduleServer(id, function(input, output, session) {
      
      ns <- session$ns
      
      output$var_select <- renderUI({
        
        selectInput(ns("var"), "Select variable",
                    choices = c(names(dat$dataset)))
      })
      
      output$grp_select <- renderUI({
        
        selectizeInput(ns("grp"), "grouping variables",
                       choices = c("year", "month", "week", colnames(dat$dataset)),
                       multiple = TRUE)
      })
      
      output$date_select <- renderUI({
        
        selectizeInput(ns("date"), "Date variable",
                       choices = c(colnames(dat$dataset)), multiple = TRUE, 
                       options = list(maxItems = 1))
      })
      
      output$fct_select <- renderUI({
        
        selectizeInput(ns("fct"), "facet by",
                       choices = c("year", "month", "week", colnames(dat$dataset)),
                       multiple = TRUE, options = list(maxItems = 2))
      })
      
      output$filter_UI <- renderUI({
        
        filter_sliderUI(id, dat$dataset, input$date, input$ftype)
      })
      
      filter_val <- reactive({
        
        filter_sliderOut(id, input$ftype, input)
      })
      
      args <- eventReactive(input$run, {
        
        density_plot(dat$dataset, project = project(), var = input$var, type = input$type, group = input$grp,
                     date = input$date, filter_date = input$ftype, filter_value = filter_val(), 
                     facet_by = input$fct, scale = input$scale, combine = input$combine, 
                     tran = input$tran, bw = input$bw, position = input$position) 
      })
      
      output$plot <- renderPlot({args()})
    })
  }
  

 # Vessel Count ====

vessel_serv <- function(id, dat, project) {
    
    moduleServer(id, function(input, output, session) {
      
      ns <- session$ns
        output$var_select <- renderUI({
          
          selectizeInput(ns("var"), "Select variable", choices = colnames(dat$dataset), multiple = FALSE)
        })
        
        output$date_select <- renderUI({
          
          selectInput(ns("date"), "Select date variable", choices = colnames(dat$dataset), 
                      selected = date_select(dat$dataset))
        })
        
        output$grp_select <- renderUI({

          selectizeInput(ns("grp"), "Select group variables",choices = colnames(dat$dataset), multiple = TRUE)
        })
       
        output$fct_select <- renderUI({
 
          selectizeInput(ns("fct"), "facet by", choices = c("year", "month", "week", colnames(dat$dataset)),
                         multiple = TRUE, options = list(maxItems = 2))
        })

      output$filter_UI <- renderUI({
    
        filter_sliderUI(id, dat$dataset, input$date, input$ftype)
      })
    
      filter_val <- reactive({
    
        filter_sliderOut(id, input$ftype, input)
      })
    
      args <- eventReactive(input$run, {
    
        vessel_count(dat$dataset, project = project(), v_id = input$var, date = input$date,
                     period = input$period, group = input$grp, filter_date = input$ftype,
                     filter_value = filter_val(), facet_by = input$fct, combine = input$combine,
                     position = input$position, tran = input$tran, value = input$value,
                     scale = input$scale, type = input$type, output = input$out)
      })
    
      tabplot <- eventReactive(input$run, {
    
        tabplot_output(args(), input$out)
      })
    
      output$output <- renderUI({tabplot()})
  })
}

  # Species Catch ====

species_serv <- function(id, dat, project) {
  
  moduleServer(id, function(input, output, session) {

    ns <- session$ns
    
    output$var_select <- renderUI({
      
      selectizeInput(ns("var"), "Select variable",
                     choices = c(colnames(dat$dataset)), multiple = TRUE)
    })
    
    output$date_select <- renderUI({
      
      selectInput(ns("date"), "Select date variable",
                  choices = c(colnames(dat$dataset)), selected = date_select(dat$dataset))
    })
    
    output$grp_select <- renderUI({
      
      selectizeInput(ns("grp"), "Select group variables",
                     choices = c(colnames(dat$dataset)), multiple = TRUE)
    })
    
    output$fct_select <- renderUI({
      
      selectizeInput(ns("fct"), "facet by",
                     choices = c("year", "month", "week", "species", colnames(dat$dataset)),
                     multiple = TRUE, options = list(maxItems = 2))
    })
    
    output$filter_UI <- renderUI({
  
      filter_sliderUI("filter", dat$dataset, input$date, input$ftype)
    })
  
    filter_val <- reactive({
  
      filter_sliderOut("filter", input$ftype, input)
    })
  
    args <- eventReactive(input$run, {
  
      species_catch(dat$dataset, project = project(), species = input$var, date = input$date,
                    period = input$period, fun = input$fun, group = input$grp,
                    filter_date = input$ftype, filter_value = filter_val(),
                    facet_by = input$fct, combine = input$combine,
                    position = input$position, tran = input$tran, value = input$value,
                    scale = input$scale, type = input$type, output = input$out,
                    format_tab = input$format)
    })
  
    tabplot <- eventReactive(input$run, {
  
      tabplot_output(args(), input$out)
    })
  
    output$output <- renderUI({tabplot()})
  })
}

  # Rolling Catch ====

roll_serv <- function(id, dat, project) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    output$var_select <- renderUI({
      
      selectizeInput(ns("var"), "Select variable",
                     choices = c(colnames(dat$dataset)), multiple = TRUE)
    })
    
    output$date_select <- renderUI({
      
      selectInput(ns("date"), "Select date variable",
                  choices = c(colnames(dat$dataset)), selected = date_select(dat$dataset))
    })
    
    output$grp_select <- renderUI({
      
      selectizeInput(ns("grp"), "Select group variables",
                     choices = c(colnames(dat$dataset)), multiple = TRUE)
    })
    
    output$fct_select <- renderUI({
      
      selectizeInput(ns("fct"), "facet by",
                     choices = c("year", "month", "week", "species", colnames(dat$dataset)),
                     multiple = TRUE, options = list(maxItems = 2))
    })

    output$filter_UI <- renderUI({
  
      filter_sliderUI("filter", dat$dataset, input$date, input$ftype)
    })
    
    filter_val <- reactive({
  
      filter_sliderOut("filter", input$ftype, input)
    })
  
    args <- eventReactive(input$run, {
  
      roll_catch(dat$dataset, project = project(), catch = input$var, date = input$date,
                 fun = input$fun, group = input$grp, filter_date = input$ftype,
                 filter_value = filter_val(), facet_by = input$fct,
                 tran = input$tran, scale = input$scale, output = input$out)
    })
  
    tabplot <- eventReactive(input$run, {
  
      tabplot_output(args(), input$out)
    })
  
    output$output <- renderUI({tabplot()})
  })
}

 # Weekly Catch =====

weekly_catch_serv <- function(id, dat, project) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    output$var_select <- renderUI({
      
      selectizeInput(ns("var"), "Select variable",
                     choices = c(colnames(dat$dataset)), multiple = TRUE)
    })
    
    output$date_select <- renderUI({
      
      selectInput(ns("date"), "Select date variable",
                  choices = c(colnames(dat$dataset)), selected = date_select(dat$dataset))
    })
    
    output$grp_select <- renderUI({
      
      selectizeInput(ns("grp"), "Select group variables",
                     choices = c(colnames(dat$dataset)), multiple = TRUE)
    })
    
    output$fct_select <- renderUI({
      
      selectizeInput(ns("fct"), "facet by",
                     choices = c("year", "month", "species", colnames(dat$dataset)),
                     multiple = TRUE, options = list(maxItems = 2))
    })

    output$filter_UI <- renderUI({
  
      filter_sliderUI("filter", dat$dataset, input$date, input$ftype)
    })
  
    filter_val <- reactive({
  
      filter_sliderOut("filter", input$ftype, input)
    })
  
    args <- eventReactive(input$run, {
  
      weekly_catch(dat$dataset, project = project(), species = input$var, date = input$date,
                   fun = input$fun, group = input$grp, filter_date = input$ftype,
                   filter_value = filter_val(),facet_by = input$fct, combine = input$combine,
                   position = input$position, tran = input$tran, value = input$value,
                   scale = input$scale, type = input$type, output = input$out,
                   format_tab = input$format)
    })
  
    tabplot <- eventReactive(input$run, {
  
      tabplot_output(args(), input$out)
    })
  
    output$output <- renderUI({tabplot()})
  })
}

 # Weekly Effort ====

weekly_effort_serv <- function(id, dat, project) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    output$var_select <- renderUI({
    
      selectizeInput(ns("var"), "Select variable",
                     choices = c(colnames(dat$dataset)), multiple = TRUE)
    })
    
    output$date_select <- renderUI({
    selectInput(ns("date"), "Select date variable",
                choices = c(colnames(dat$dataset)), selected = date_select(dat$dataset))
    })
    
    output$grp_select <- renderUI({
      selectizeInput(ns("grp"), "Select group variables",
                     choices = c(colnames(dat$dataset)), multiple = TRUE)
    })
    
    output$fct_select <- renderUI({
      selectizeInput(ns("fct"), "facet by",
                     choices = c("year", "month", "species", colnames(dat$dataset)),
                     multiple = TRUE, options = list(maxItems = 2))
    })
    
    output$filter_UI <- renderUI({
  
      filter_sliderUI("filter", dat$dataset, input$date, input$ftype)
    })
  
    filter_val <- reactive({
  
      filter_sliderOut("filter", input$ftype, input)
    })
  
    args <- eventReactive(input$run, {
  
      weekly_effort(dat$dataset, project = project(), cpue = input$var, date = input$date,
                    group = input$grp, filter_date = input$ftype,filter_value = filter_val(),
                    facet_by = input$fct, combine = input$combine, tran = input$tran,
                    scale = input$scale, output = input$out, format_tab = input$format)
    })
  
    tabplot <- eventReactive(input$run, {
  
      tabplot_output(args(), input$out)
    })
  
    output$output <- renderUI({tabplot()})
  })
}

 # Bycatch ====

bycatch_serv <- function(id, dat, project) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    output$cpue_select <- renderUI({
      selectizeInput(ns("cpue"), "Select CPUE variable(s)",
                     choices = c(colnames(dat$dataset)), multiple = TRUE)
    })
  
    output$catch_select <- renderUI({
      selectizeInput(ns("catch"), "Select Catch variable(s)",
                     choices = c(colnames(dat$dataset)), multiple = TRUE)
    })
  
    output$date_select <- renderUI({
      selectInput(ns("date"), "Date variable",
                  choices = c(colnames(dat$dataset)), selected = date_select(dat$dataset))
    })

    output$grp_select <- renderUI({
  selectizeInput(ns("grp"), "Select group variables",
                 choices = c(colnames(dat$dataset)), multiple = TRUE)
    })
  
    output$fct_select <- renderUI({
      selectizeInput(ns("fct"), "facet by",
                     choices = c("year", "month", "week", colnames(dat$dataset)),
                     multiple = TRUE, 
                     options = list(maxItems = 2,
                                    placeholder = "Limit 2 (facet 1 x facet2)"))
    })

  output$filter_UI <- renderUI({

    filter_sliderUI("filter", dat$dataset, input$date, input$ftype)
  })

  filter_val <- reactive({

    filter_sliderOut("filter", input$ftype, input)
  })

  names <- reactiveValues(names = NULL)

  observeEvent(input$nms_add, {

    names$names <- c(names$names, input$nms)
  })

  observeEvent(input$nms_clear, {

    names$names <- NULL
  })

  output$caption <- renderText({names$names})

  args <- eventReactive(input$run, {

    bycatch(dat$dataset, project = project(), cpue = input$cpue, catch = input$catch, 
            date = input$date, group = input$grp, names = names$names, period = input$period,
            filter_date = input$ftype, filter_value = filter_val(), facet_by = input$fct,
            value = input$value, combine = input$combine, tran = input$tran,
            scale = input$scale, output = input$out, format_tab = input$format)
  })

  tabplot <- eventReactive(input$run, {

    tabplot_output(args(), input$out)
  })

  output$output <- renderUI({tabplot()})
  })
}

 # Trip Length ====

trip_serv <- function(id, dat, project) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    output$start_select <- renderUI({
    selectInput(ns("start"), "Start date",
                choices = c(colnames(dat$dataset)), selected = date_select(dat$dataset))
    })
    
    output$end_select <- renderUI({
    selectInput(ns("end"), "End date",
                choices = c(colnames(dat$dataset)), selected = date_select(dat$dataset))
    })
    
    output$catch_select <- renderUI({
    selectizeInput(ns("catch"), "Select Catch variable(s)",
                   choices = c(colnames(dat$dataset)), multiple = TRUE)
    })
    
    output$haul_select <- renderUI({
    selectizeInput(ns("haul"), "Select Haul variable",
                   choices = c(colnames(dat$dataset)), multiple = TRUE,
                   options = list(maxItems = 1))
    })
    
    output$grp_select <- renderUI({
    selectizeInput(ns("grp"), "Group variables",
                   choices = c("year", "month", "week",colnames(dat$dataset)), multiple = TRUE)
    })
    
    output$fct_select <- renderUI({
    selectizeInput(ns("fct"), "facet by",
                   choices = c("year", "month", "week", colnames(dat$dataset)),
                   multiple = TRUE, options = list(maxItems = 2, placeholder = "Limit 2 (facet 1 x facet 2)"))
    })
    
    output$kwargs_select <- renderUI({
    selectizeInput(ns("htp_kwargs"), "Columns indentifying unique trips",
                   choices = colnames(dat$dataset), multiple = TRUE)
    })
    
    output$filter_UI <- renderUI({
  
      filter_sliderUI("filter", dat$dataset, input$start, input$ftype)
    })
  
    filter_val <- reactive({
  
      filter_sliderOut("filter", input$ftype, input)
    })
  
    args <- eventReactive(input$run, {
  
      trip_length(dat$dataset, project = project(), start = input$start, end = input$end,
                  units = input$unit, catch = input$catch, hauls = input$haul,
                  group = input$grp, filter_date = input$ftype, filter_value = filter_val(),
                  facet_by = input$fct, density = input$dens, tran = input$tran,
                  scale = input$scale, output = input$out, pages = "single",
                  type = input$type, format_tab = input$format, bins = input$bins,
                  haul_to_trip = input$haul_trp, input$htp_kwargs)
    })
  
    tabplot <- eventReactive(input$run, {
  
      tabplot_output(args(), input$out)
    })

    output$output <- renderUI({tabplot()})
  })
}
 # Fleet Table ====

fleet_table_serv <- function(id, dat, project) {
  
  moduleServer(id, function(input, output, session) {

    f_r <- reactiveValues()
    empty_row <- data.frame(condition = "enter condition", fleet = "enter fleet name",
                            stringsAsFactors = FALSE)
    empty_col <- data.frame(condition = "enter condition", stringsAsFactors = FALSE)
    f_r$row <- empty_row
    f_r$f_DT <- empty_row
    proxy_f <- DT::dataTableProxy("f_tab")
  
    observeEvent(input$upload, {
  
      req(input$file)
      type <- sub('.*\\.', '', input$file$name)
      if (type == 'RData') { type <- 'R'} else { type <- type}
      upload <- FishSET::read_dat(input$file$datapath, type)
  
      if (any(vapply(upload, is.factor, FUN.VALUE = logical(1)))) {
  
        fac <- vapply(upload, is.factor, FUN.VALUE = logical(1))
  
        for (i in names(upload[fac])) {
  
          upload[[i]] <- as.character(upload[[i]])
        }
  
        f_r$f_DT <- upload
      }
    })
  
    observeEvent(input$addrow, {
  
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
  
    observeEvent(input$f_tab_cell_edit, {
  
      f_r$f_DT <<- DT::editData(f_r$f_DT, input$f_tab_cell_edit, "f_tab")
      rownames(f_r$f_DT) <- 1:nrow(f_r$f_DT)
      DT::replaceData(proxy_f, f_r$f_DT, resetPaging = FALSE)
    })
  
    output$f_tab <- DT::renderDataTable(f_r$f_DT, editable  = "all", 
                                        selection = list(target = 'row+column'))
  
    observeEvent(input$colname_btn, {
      req(input$colname)
      names(f_r$f_DT)[input$f_tab_columns_selected] <- input$colname
    })
  
    observeEvent(input$save, {
  
      fleet_table(dat$dataset, project = project(), table = f_r$f_DT, save = TRUE)
      #showNotification("Table saved.", type = 'message', duration = 10)
    })
  })
}

 # Fleet Assign ====

fleet_assign_serv <- function(id, dat, project) {
  
  moduleServer(id, function(input, output, session) {

    fleet_tab_db <- eventReactive(input$refresh, {
  
      grep("FleetTable", tables_database(), value = TRUE)
    })
  
    observe({
      updateSelectInput(session, session$ns("tab"),
                        choices =  fleet_tab_db())
    })
  
    tab_view <- eventReactive(input$view_btn, {
  
      table_view(input$tab)
    })
  
    output$tab_preview <- DT::renderDT({tab_view()})
  
    args <- eventReactive(input$run, {
  
      fleet_assign(dat$dataset, project = project(), input$tab, 
                   overlap = input$overlap, format_tab = input$format)
    })
  
    output$final_tab <- DT::renderDT({args()})
  
    output$plot <- renderPlot({

      if (input$format == "long") {

        ggplot2::ggplot(args(), ggplot2::aes(fleet, fill = fleet)) + ggplot2::geom_bar()

      } else {

        value <- vapply(args()[tab_view()$fleet], sum, FUN.VALUE = numeric(1))
        df <- data.frame(fleet = tab_view()$fleet, value = value)
        ggplot2::ggplot(df, ggplot2::aes(fleet, value, fill = fleet)) + ggplot2::geom_col()
      }
    })
  })
}