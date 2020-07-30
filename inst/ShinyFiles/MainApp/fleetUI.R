source("fleet_helpers.R", local = TRUE)


saveOutputUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    downloadLink(ns('downloadplotHIDE'), label = ''),
    actionButton(ns('downloadplot'), label = 'Save plot to folder'),
    downloadLink(ns('downloadTableHIDE'), label = ''),
    actionButton(ns('downloadTable'), label = 'Save table to folder as csv')
  )
}

noteUI <- function(id) {
  
  ns <- NS(id)
  tagList(
    downloadLink(ns("downloadText"), label = ''),
    actionButton(ns('callTextDownload'),'Save notes'),
    textInput(ns('notes'), "Notes", value = NULL, 
              placeholder = 'Write notes to store in text output file. Text can be inserted into report later.')
  )
}

RexpressionUI <- function(id) {
  
  ns <- NS(id)
  tagList(
    textInput(ns("expr"), label = "Enter an R expression",
              value = "values$dataset"),
    actionButton(ns("run"), "Run", class = "btn-success")
  )
}

saveDataTableUI <- function(id) {
  ns <- NS(id)
  actionButton(ns('saveData'),'Save data to fishset_db database',
               style = "color: #fff; background-color: #6EC479; border-color:#000000;")
}

refreshUI <- function(id) {
  
  ns <- NS(id)
  actionButton(ns("refresh"), "Refresh data", 
               icon = icon("fa fa-refresh"),
               style = "color: white; background-color: blue;")
}

closeAppUI <- function(id) {
  
  ns <- NS(id)
  
  tags$button(
    id = ns('close'),
    type = "button",
    style ="color: #fff; background-color: #FF6347; border-color: #800000;",
    class = "btn action-button",
    onclick = "setTimeout(function(){window.close();},500);",  # close browser
    "Close app"
  )
}




filter_sliderUI <- function(id, dat, date, type) {
  
  if (!is.null(type)) {
    
    ns <- NS(id)
    
    if (grepl("year", type)) {
      
      min <- yearRange(dat, date, "min")
      max <- yearRange(dat, date, "max")
      
      if (type == "year-month") {
        
        tagList(
          
          sliderInput(ns("yr"), "Select year",
                      min = min, max = max, value = c(min, max),
                      sep = "", step = 1),
          sliderInput(ns("per"), "Select month",
                      min = 1, max = 12, value = c(5,7)))
        
      } else if (type == "year-week") {
        
        tagList(
          
          sliderInput(ns("yr"), "Select year",
                      min = min, max = max, value = c(min, max),
                      sep = "", step = 1),
          sliderInput(ns("per"), "Select week",
                      min = 0, max = 53, value = c(20, 30)))
        
      } else if (type == "year-day") {
        
        tagList(
          
          sliderInput(ns("yr"), "Select year",
                      min = min, max = max, value = c(min, max),
                      sep = "", step = 1),
          sliderInput(ns("per"), "Select day",
                      min = 1, max = 365, value = c(150, 200)))
        
      } else if (type == "year") {
        
        sliderInput(ns("yr"), "Select year",
                    min = min, max = max, value = c(min, max),
                    sep = "", step = 1)
      } 
      
    } else { 
      
      if (type == "month") {
        
        sliderInput(ns("per2"), "Select month",
                    min = 1, max = 12, value = c(5,7))
        
      } else if (type == "week") {
        
        sliderInput(ns("per2"), "Select week",
                    min = 0, max = 53, value = c(20, 30))
        
      } else if (type == "day") {
        
        sliderInput(ns("per2"), "Select day",
                    min = 1, max = 365, value = c(150, 200))
      }
    }
  } else {
    
    NULL
  }
}


filter_sliderOut <- function(id, type, input) {
  
  if (!is.null(type)) {
    
    if (grepl("year", type)) {
      
      if (grepl("-", type)) {
        
        list(seq(min(input$yr), max(input$yr), 1), 
             seq(min(input$per), max(input$per), 1))
        
      } else {
        
        seq(min(input$yr), max(input$yr), 1)
      }
      
    } else {
      
      seq(min(input$per2), max(input$per2), 1)
    }
  } else {
    NULL
  }
}


density_plotUI <- function(id, dat) {
  
  ns <- NS(id)
  
  tagList(
    
    downloadLink(ns('downloadplotHIDE'), label = ''),
    actionButton(ns('downloadplot'), label = 'Save plot to folder'),
    closeAppUI(ns("close")), 
    refreshUI(ns("refresh")), 
    
    tags$br(), tags$br(),
    
    noteUI(ns("note")),
    
    actionButton(ns("fun_run"), "Run function",
                 style = "color: #fff; background-color: #6da363; border-color: #800000;"),
    
    selectInput(ns("type"), "Plot type",
                choices = c("kde", "ecdf", "cdf")),
    
    uiOutput(ns("var_select")),
    
    uiOutput(ns("date_select")),
    
    selectizeInput(ns("ftype"), "Subset data by (optional)",
                   choices = c("none", "date range" = "date_range", "year-month", "year-week", "year-day",
                               "year", "month", "week", "day")),
    
    uiOutput(ns("filter_UI")),
    
    uiOutput(ns("grp_select")),
    
    checkboxInput(ns("combine"), "Combine grouping variables?",
                  value = FALSE),
    
    uiOutput(ns("fct_select")),
    
    numericInput(ns("bw"), "Kernel binwidth",
                 value = 1),
    
    selectInput(ns("tran"), "Transformation function (optional)",
                choices = c("none" = "identity", "log", "log2", "log10", "sqrt")),
    
    selectInput(ns("scale"), "Split plot scale",
                choices = c("fixed", "free y-axis" = "free_y", 
                            "free x-axis" = "free_x", "free")),
    
    selectInput(ns("position"), "Position of grouping variables",
                choices = c("identity", "fill", "stack"), selected = "identity"),
    
    RexpressionUI(ns("exp")),
    div( style = "margin-top: 2em;",
         uiOutput(ns("result"))
    )
  )
}



vessel_countUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    saveOutputUI(ns("saveOut")),
    
    closeAppUI(ns("close")), 
    refreshUI(ns("refresh")),
    
    tags$br(), tags$br(),
    
    noteUI(ns("note")),
    
    actionButton(ns("fun_run"), "Run function",
                 style = "color: #fff; background-color: #6da363; border-color: #800000;"),
    
    selectInput(ns("out"), "View table and/or plot",
                choices = c("plot and table" = "tab_plot", "plot", "table"),
                selected = "tab_plot"),
    
    uiOutput(ns("var_select")),
    
    uiOutput(ns("date_select")),
    
    selectInput(ns("period"), "Show counts by",
                choices = c("year", "month", "weeks", "day of the month" = "day",
                            "day of the year" = "day_of_year", "weekday"),
                selected = "year"),
    
    selectizeInput(ns("ftype"), "Subset data by (optional)",
                   choices = c("none", "date range" = "date_range", "year-month", 
                               "year-week", "year-day", "year", "month", "week", "day")),
    
    uiOutput(ns("filter_UI")),
    
    uiOutput(ns("grp_select")),
    
    checkboxInput(ns("combine"), "Combine grouping variables?",
                  value = FALSE),
    
    uiOutput(ns("fct_select")),
    
    selectInput(ns("value"), "Select value type",
                choices = c("count", "percent")),
    
    selectInput(ns("tran"), "Transformation function (optional)",
                choices = c("none" = "identity", "log", "log2", "log10", "sqrt")),
    
    selectInput(ns("type"), "Plot type",
                choices = c("bar", "line")),
    
    selectInput(ns("scale"), "Split plot scale",
                choices = c("fixed", "free y-axis" = "free_y", 
                            "free x-axis" = "free_x", "free")),
    
    selectInput(ns("position"), "Position of grouping variables",
                choices = c("identity", "dodge", "fill", "stack"),
                selected = "stack"),
    
    RexpressionUI(ns("exp")),
    #RexpressionUI(id),
    div( style = "margin-top: 2em;",
         uiOutput(ns("result"))
    )
  )
}


species_catchUI <- function(id) {
  
  ns <- NS(id)
  tagList(
    saveOutputUI(ns("saveOut")),
    closeAppUI(ns("close")), 
    refreshUI(ns("refresh")),
    
    tags$br(), tags$br(),
    
    noteUI(ns("note")),
    actionButton(ns("fun_run"), "Run function",
                 style = "color: #fff; background-color: #6da363; border-color: #800000;"),
    
    selectInput(ns("out"), "View table and/or plot",
                choices = c("plot and table" = "tab_plot", "plot", "table"),
                selected = "tab_plot"),
    
    uiOutput(ns("var_select")),
    
    uiOutput(ns("date_select")),
    
    selectInput(ns("period"), "Show counts by",
                choices = c("year", "month", "weeks", "day of the month" = "day",
                            "day of the year" = "day_of_year", "weekday"),
                selected = "year"),
    
    selectizeInput(ns("ftype"), "Subset data by (optional)",
                   choices = c("none", "date range" = "date_range", "year-month", 
                               "year-week", "year-day", "year", "month", "week", "day")),
    
    uiOutput(ns("filter_UI")),
    
    selectInput(ns("fun"), "Summary function",
                choices = c("sum", "mean", "sd", "median", "min", "max", "IQR")),
    
    uiOutput(ns("grp_select")),
    
    checkboxInput(ns("combine"), "Combine grouping variables?",
                  value = FALSE),
    
    uiOutput(ns("fct_select")),
    
    selectInput(ns("value"), "Select value type",
                choices = c("count", "percent")),
    
    selectInput(ns("conv"), "Convert catch (optional)",
                choices = c("none", "tons", "metric tons" = "metric_tons", "custom")),
    
    conditionalPanel("input.conv == 'custom'",
                     
                     textInput(ns("conv"), "Enter function")),
    
    selectInput(ns("tran"), "Transform y-axis (optional)",
                choices = c("none" = "identity", "log", "log2", "log10", "sqrt")),
    
    selectInput(ns("type"), "Plot type",
                choices = c("bar", "line")),
    
    selectInput(ns("scale"), "Split plot scale",
                choices = c("fixed", "free y-axis" = "free_y",
                            "free x-axis" = "free_x", "free")),
    
    selectInput(ns("position"), "Position of grouping variables",
                choices = c("identity", "dodge", "fill", "stack"),
                selected = "stack"),
    
    selectInput(ns("format"), "Table format",
                choices = c("wide", "long")),
    
    RexpressionUI(ns("exp")),
    div( style = "margin-top: 2em;",
         uiOutput(ns("result"))
    )
  )
}


roll_catchUI <- function(id) { 
  
  ns <- NS(id)
  tagList(
    saveOutputUI(ns("saveOut")),
    closeAppUI(ns("close")), 
    refreshUI(ns("refresh")),
    
    tags$br(), tags$br(),
    
    noteUI(ns("note")),
    actionButton(ns("fun_run"), "Run function",
                 style = "color: #fff; background-color: #6da363; border-color: #800000;"),
    
    selectInput(ns("out"), "View table and/or plot",
                choices = c("plot and table" = "tab_plot", "plot", "table")),
    
    uiOutput(ns("var_select")),
    
    uiOutput(ns("date_select")),
    
    selectizeInput(ns("ftype"), "Subset data by (optional)",
                   choices = c("none", "date range" = "date_range", "year-month", 
                               "year-week", "year-day", "year", "month", "week", "day")),
    
    uiOutput(ns("filter_UI")),
    
    selectInput(ns("fun"), "Summary function",
                choices = c("mean", "sum", "sd", "median", "min", "max", "IQR")),
    
    uiOutput(ns("grp_select")),
    
    uiOutput(ns("fct_select")),
    
    selectInput(ns("scale"), "Split plot scale",
                choices = c("fixed", "free y-axis" = "free_y",
                            "free x-axis" = "free_x", "free")),
    
    numericInput(ns("win"), "Window width (days)",
                 value = 10),
    
    selectInput(ns("align"), "Window alignment",
                choices = c("center", "left", "right")),
    
    selectInput(ns("conv"), "Convert catch (optional)",
                choices = c("none", "tons", "metric tons" = "metric_tons", "custom")),
    
    conditionalPanel("input.conv == 'custom'",
                     
                     textInput(ns("conv"), "Enter function")),
    
    selectInput(ns("tran"), "Transform y-axis (optional)",
                choices = c("none" = "identity", "log", "log2", "log10", "sqrt")),
    
    RexpressionUI(ns("exp")),
    div( style = "margin-top: 2em;",
         uiOutput(ns("result"))
    )
  )
}


weekly_catchUI <- function(id) {
  
  ns <- NS(id)
  tagList(
    saveOutputUI(ns("saveOut")),
    closeAppUI(ns("close")), 
    refreshUI(ns("refresh")),
    
    tags$br(), tags$br(),
    
    noteUI(ns("note")),
    actionButton(ns("fun_run"), "Run function",
                 style = "color: #fff; background-color: #6da363; border-color: #800000;"),
    
    selectInput(ns("out"), "View table and/or plot",
                choices = c("plot and table" = "tab_plot", "plot", "table"),
                selected = "tab_plot"),
    
    uiOutput(ns("var_select")),
    
    uiOutput(ns("date_select")),
    
    selectizeInput(ns("ftype"), "Subset data by (optional)",
                   choices = c("none", "date range" = "date_range", "year-month", 
                               "year-week", "year-day", "year", "month", "week", "day")),
    
    uiOutput(ns("filter_UI")),
    
    uiOutput(ns("grp_select")),
    
    uiOutput(ns("fct_select")),
    
    selectInput(ns("fun"), "Summary function",
                choices = c("sum", "mean", "sd", "median", "min", "max", "IQR")),
    
    checkboxInput(ns("combine"), "Combine grouping variables?",
                  value = FALSE),
    
    selectInput(ns("value"), "Select value type",
                choices = c("count", "percent")),
    
    selectInput(ns("conv"), "Convert catch (optional)",
                choices = c("none", "tons", "metric tons" = "metric_tons", "custom")),
    
    conditionalPanel("input.conv == 'custom'",
                     
                     textInput(ns("conv"), "Enter function")),
    
    selectInput(ns("tran"), "Transform y-axis (optional)",
                choices = c("none" = "identity", "log", "log2", "log10", "sqrt")),
    
    selectInput(ns("type"), "Plot type",
                choices = c("bar", "line")),
    
    selectInput(ns("scale"), "Split plot scale",
                choices = c("fixed", "free y-axis" = "free_y",
                            "free x-axis" = "free_x", "free")),
    
    selectInput(ns("position"), "Position of grouping variables",
                choices = c("identity", "dodge", "fill", "stack"),
                selected = "stack"),
    
    selectInput(ns("format"), "Table format",
                choices = c("wide", "long")),
    
    RexpressionUI(ns("exp")),
    div( style = "margin-top: 2em;",
         uiOutput(ns("result"))
    )
  )
}


weekly_effortUI <- function(id) {
  
  ns <- NS(id)
  tagList(
    saveOutputUI(ns("saveOut")),
    closeAppUI(ns("close")), 
    refreshUI(ns("refresh")),
    
    tags$br(), tags$br(),
    
    noteUI(ns("note")),
    actionButton(ns("fun_run"), "Run function",
                 style = "color: #fff; background-color: #6da363; border-color: #800000;"),
    
    selectInput(ns("out"), "View table and/or plot",
                choices = c("plot and table" = "tab_plot", "plot", "table"),
                selected = "tab_plot"),
    
    uiOutput(ns("var_select")),
    
    uiOutput(ns("date_select")),
    
    selectizeInput(ns("ftype"), "Subset data by (optional)",
                   choices = c("none", "date range" = "date_range", "year-month", 
                               "year-week", "year-day", "year", "month", "week", "day")),
    
    uiOutput(ns("filter_UI")),
    
    uiOutput(ns("grp_select")),
    
    checkboxInput(ns("combine"), "Combine grouping variables?",
                  value = FALSE),
    
    uiOutput(ns("fct_select")),
    
    selectInput(ns("conv"), "Convert catch (optional)",
                choices = c("none", "tons", "metric tons" = "metric_tons", "custom")),
    
    conditionalPanel("input.conv == 'custom'",
                     
                     textInput(ns("conv"), "Enter function")),
    
    selectInput(ns("tran"), "Transform y-axis (optional)",
                choices = c("none" = "identity", "log", "log2", "log10", "sqrt")),
    
    selectInput(ns("scale"), "Split plot scale",
                choices = c("fixed", "free y-axis" = "free_y",
                            "free x-axis" = "free_x", "free")),
    
    selectInput(ns("format"), "Table format",
                choices = c("wide", "long")),
    
    RexpressionUI(ns("exp")),
    div( style = "margin-top: 2em;",
         uiOutput(ns("result"))
    )
  )
}

bycatchUI <- function(id) {
  
  ns <- NS(id)
  tagList(
    saveOutputUI(ns("saveOut")),
    closeAppUI(ns("close")), 
    refreshUI(ns("refresh")),
    
    tags$br(), tags$br(),
    
    noteUI(ns("note")),
    p("Note: CPUE and catch variables should be added in the same order."),
    
    actionButton(ns("fun_run"), "Run function",
                 style = "color: #fff; background-color: #6da363; border-color: #800000;"),
    
    selectInput(ns("out"), "View table and/or plot",
                choices = c("plot and table" = "tab_plot", "plot", "table"),
                selected = "tab_plot"),
    
    uiOutput(ns("cpue_select")),
    
    uiOutput(ns("catch_select")),
    
    uiOutput(ns("date_select")),
    
    selectInput(ns("period"), "Show counts by",
                choices = c("year", "month", "weeks")),
    
    selectizeInput(ns("ftype"), "Subset data by (optional)",
                   choices = c("none", "date range" = "date_range", "year-month", 
                               "year-week", "year-day", "year", "month", "week", "day")),
    
    uiOutput(ns("filter_UI")),
    
    textInput(ns("nms"), "Names", value = NULL,
              placeholder = "optional names to be used in plot/table"),
    
    fluidRow(actionButton(ns("nms_add"), "Add name"),
             actionButton(ns("nms_clear"), "Clear names")),
    
    textOutput(ns("caption")),
    
    uiOutput(ns("grp_select")),
    
    checkboxInput(ns("combine"), "Combine grouping variables?",
                  value = FALSE),
    
    uiOutput(ns("fct_select")),
    
    selectInput(ns("value"), "Catch value type",
                choices = c("total", "share of total catch" = "stc")),
    
    selectInput(ns("tran"), "Transform y-axis (optional)",
                choices = c("none" = "identity", "log", "log2", "log10", "sqrt")),
    
    selectInput(ns("scale"), "Split plot scale",
                choices = c("fixed", "free y-axis" = "free_y",
                            "free x-axis" = "free_x", "free")),
    
    selectInput(ns("format"), "Table format",
                choices = c("wide", "long")),
    
    RexpressionUI(ns("exp")),
    div( style = "margin-top: 2em;",
         uiOutput(ns("result"))
    )
  )
}

trip_lengthUI <- function(id) {
  
  ns <- NS(id)
  tagList(
    saveOutputUI(ns("saveOut")),
    closeAppUI(ns("close")), 
    refreshUI(ns("refresh")),
    
    tags$br(), tags$br(),
    
    noteUI(ns("note")),
    actionButton(ns("fun_run"), "Run function",
                 style = "color: #fff; background-color: #6da363; border-color: #800000;"),
    
    selectInput(ns("out"), "View table and/or plot",
                choices = c("plot and table" = "tab_plot", "plot", "table")),
    
    uiOutput(ns("start_select")),
    
    uiOutput(ns("end_select")),
    
    selectInput(ns("unit"), "Unit of time",
                choices = c("minutes" = "mins", "hours", "days", "weeks"),
                selected = "hours"),
    
    selectizeInput(ns("ftype"), "Subset data by (optional)",
                   choices = c("none", "date range" = "date_range", "year-month", 
                               "year-week", "year-day", "year", "month", "week", "day")),
    
    uiOutput(ns("filter_UI")),
    
    uiOutput(ns("catch_select")),
    
    uiOutput(ns("haul_select")),
    
    uiOutput(ns("grp_select")),
    
    uiOutput(ns("fct_select")),
    
    selectInput(ns("type"), "Plot type",
                choices = c("histogram" = "hist", "frequency polygon" = "freq_poly")),
    
    sliderInput(ns("bins"), "Bins",
                min = 1, max = 60, value = 30),
    
    checkboxInput(ns("dens"), "Density (y-axis)", value = TRUE),
    
    selectInput(ns("tran"), "Transform x-axis (optional)",
                choices = c("none" = "identity", "log", "log2", "log10", "sqrt")),
    
    selectInput(ns("scale"), "Split plot scale",
                choices = c("fixed", "free y-axis" = "free_y",
                            "free x-axis" = "free_x", "free")),
    
    checkboxInput(ns("haul_trp"), "Convert to trip-level?",
                  value = FALSE),
    
    conditionalPanel("input.haul_trp",
                     
                     uiOutput(ns("kwargs_select"))),
    
    selectInput(ns("format"), "Table format",
                choices = c("wide", "long")),
    
    RexpressionUI(ns("exp")),
    div( style = "margin-top: 2em;",
         uiOutput(ns("result"))
    )
  )
}

fleet_tableUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    p("Fleet tables must be saved to the FishSET database before they can be used to assign observations to fleets."),
    
    actionButton(ns("save"), "Save table to FishSET database",
                 style = "color: #fff; background-color: #6EC479; border-color:#000000;"),
    
    closeAppUI(ns("close")), 
    refreshUI(ns("refresh")),
    
    tags$br(), tags$br(),
    
    noteUI(ns("note")),
    
    fluidRow(
      actionButton(ns("addrow"), "Add row",
                   style = "color: #fff; background-color: #6EC479; border-color:#000000;"),
      
      actionButton(ns("deleterow"), "Delete row",
                   style = "color: #fff; background-color: #EB8C34; border-color:#000000;")),
    
    tags$br(), 
    
    fluidRow(
      actionButton(ns("addcol"), "Add column",
                   style = "color: #fff; background-color: #6EC479; border-color:#000000;"),
      
      actionButton(ns("deletecol"), "Delete column",
                   style = "color: #fff; background-color: #EB8C34; border-color:#000000;")),
    
    tags$br(),
    
    fileInput(ns("file"), "Browse file"),
    
    actionButton(ns("upload"), "Upload table"),
    
    tags$br(), tags$br(),
    
    textInput(ns("colname"), "New column name"),
    
    actionButton(ns("colname_btn"), "Change column name"),
    
    tags$br(), tags$br(),
    
    RexpressionUI(ns("exp")),
    div( style = "margin-top: 2em;",
         uiOutput(ns("result"))
    )
  )
}


fleet_assignUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    #saveDataTableUI(ns("saveDat")),
    
    actionButton(ns('saveData'),'Save data to fishset_db database',
                 style = "color: #fff; background-color: #6EC479; border-color:#000000;"),
    closeAppUI(ns("close")), 
    refreshUI(ns("refresh")),
    
    tags$br(), tags$br(),
    
    noteUI(ns("note")),
    
    actionButton(ns("refresh"), "", icon = icon("refresh")),
    
    uiOutput(ns("available_tabs")),
    
    actionButton(ns("view_btn"), "View table"),
    
    checkboxInput(ns("overlap"), "Allow overlapping fleet assignments?"),
    
    selectInput(ns("format"), "Table format",
                choices = c("long", "wide")),
    
    actionButton(ns("fun_run"), "Run function",
                 style = "color: #fff; background-color: #6da363; border-color: #800000;"),
    
    actionButton(ns("save"), "Save table"),
    
    tags$br(), tags$br(),
    
    RexpressionUI(ns("exp")),
    div(style = "margin-top: 2em;",
        uiOutput(ns("result"))
    )
  )
}


density_plotOut <- function(id) {
  
  ns <- NS(id)
  shinycssloaders::withSpinner(plotOutput(ns("plot")))
}

fleetOut <- function(id) { 
  ns <- NS(id)
  
  tagList(
    verbatimTextOutput(ns("filter_out")),
    uiOutput(ns("output")) 
  )
}

fleet_tableOut <- function(id) {
  ns <- NS(id)
  DT::DTOutput(ns("f_tab"))
}

fleet_assignOut <- function(id) {
  
  ns <- NS(id)
  tagList(
    h5("Fleet Table"),
    DT::DTOutput(ns("tab_preview")),
    h5("Dataset"),
    DT::DTOutput(ns("final_tab")),
    h5("Frequency of fleets"),
    plotOutput(ns("plot"))
  )
}