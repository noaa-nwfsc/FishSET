source("fleet_helpers.R", local = TRUE)



filter_sliderUI <- function(id, dat, date, type) {
  
  #req(type)
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
  
  # req(type) 
 
  if (!is.null(type)) {
    
    if (grepl("-", type)) {
      
      list(seq(min(input$yr), max(input$yr), 1), 
           seq(min(input$per), max(input$per), 1))
      
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
    actionButton(ns("run"), "Run",
                 style = "color: #fff; background-color: #6da363; border-color: #800000;"),
    
    uiOutput(ns("var_select")),
    
    selectInput(ns("type"), "Plot type",
                choices = c("kde", "ecdf", "cdf")),
    
    uiOutput(ns("grp_select")),
    
    checkboxInput(ns("combine"), "Combine grouping variables",
                  value = FALSE),
    
    uiOutput(ns("date_select")),
    
    uiOutput(ns("fct_select")),
    
    selectizeInput(ns("ftype"), "filter type",
                   choices = c("year-month", "year-week", "year-day",
                               "year", "month", "week", "day"), multiple = TRUE,
                   options = list(maxItems = 1)),
    
    uiOutput(ns("filter_UI")),
    
    selectInput(ns("tran"), "Transformation function",
                choices = c("none" = "identity", "asn", "atanh", "boxcox", "date", "exp", "hms",
                            "log", "log10", "log1p", "log2", "logit", "modulus", 
                            "probability", "probit", "pseudo_log", "reciprocal", 
                            "reverse", "sqrt")),
    
    selectInput(ns("scale"), "Facet scale",
                choices = c("fixed", "free y-axis" = "free_y", 
                            "free x-axis" = "free_x", "free")),
    
    numericInput(ns("bw"), "Kernel binwidth",
                 value = 1),
    
    selectInput(ns("position"), "Position of grouping variables",
                choices = c("identity", "fill", "stack"), selected = "identity")
  )
}



vessel_countUI <- function(id) {
  
  ns <- NS(id)
  
    tagList(
      actionButton(ns("run"), "Run",
                   style = "color: #fff; background-color: #6da363; border-color: #800000;"),
      
      uiOutput(ns("var_select")),
      
      uiOutput(ns("date_select")),
      
      selectInput(ns("period"), "Select period",
                  choices = c("year", "month", "weeks", "day of the month" = "day",
                              "day of the year" = "day_of_year", "weekday"),
                  selected = "year"),
     
      uiOutput(ns("grp_select")),
      
      checkboxInput(ns("combine"), "Combine grouping variables",
                    value = FALSE),
      
      uiOutput(ns("fct_select")),
      
      selectizeInput(ns("ftype"), "filter type",
                     choices = c("year-month", "year-week", "year-day",
                                 "year", "month", "week", "day"), multiple = TRUE,
                     options = list(maxItems = 1)),
      
      uiOutput(ns("filter_UI")),
      
      selectInput(ns("value"), "Select value type",
                  choices = c("count", "percent")),
      
      selectInput(ns("tran"), "Transformation funcion",
                  choices = c("none" = "identity", "log", "log2", "log10", "sqrt")),
      
      selectInput(ns("type"), "Plot type",
                  choices = c("bar", "line")),
      
      selectInput(ns("scale"), "Facet scale",
                  choices = c("fixed", "free y-axis" = "free_y", 
                              "free x-axis" = "free_x", "free")),
      
      selectInput(ns("position"), "Position of grouping variables",
                  choices = c("identity", "dodge", "fill", "stack"),
                  selected = "stack"),
      
      selectInput(ns("out"), "Output type",
                  choices = c("plot and table" = "tab_plot", "plot", "table"),
                  selected = "tab_plot")
    )
}

  
species_catchUI <- function(id) {
  
  ns <- NS(id)
  tagList(

    actionButton(ns("run"), "Run",
                 style = "color: #fff; background-color: #6da363; border-color: #800000;"),
  
    uiOutput(ns("var_select")),
  
    uiOutput(ns("date_select")),
  
    selectInput(ns("period"), "Select period",
                choices = c("year", "month", "weeks", "day of the month" = "day",
                            "day of the year" = "day_of_year", "weekday"),
                selected = "year"),
  
    selectInput(ns("fun"), "Aggregate function",
                choices = c("sum", "mean", "sd", "median", "min", "max", "IQR")),
  
    uiOutput(ns("grp_select")),
  
    checkboxInput(ns("combine"), "Combine grouping variables",
                  value = FALSE),
  
    uiOutput(ns("fct_select")),
  
    selectizeInput(ns("ftype"), "filter type",
                   choices = c("year-month", "year-week", "year-day",
                               "year", "month", "week", "day"), multiple = TRUE,
                   options = list(maxItems = 1)),
  
    uiOutput(ns("filter_UI")),
  
    selectInput(ns("value"), "Select value type",
                choices = c("count", "percent")),
  
    selectInput(ns("conv"), "Convert catch",
                choices = c("none", "tons", "metric tons" = "metric_tons", "custom")),
  
    conditionalPanel("input.conv == 'custom'",
  
                     textInput(ns("conv"), "Enter function")),
  
    selectInput(ns("tran"), "Transform y-axis",
                choices = c("none" = "identity", "log", "log2", "log10", "sqrt")),
  
    selectInput(ns("type"), "Plot type",
                choices = c("bar", "line")),
  
    selectInput(ns("scale"), "Facet scale",
                choices = c("fixed", "free y-axis" = "free_y",
                            "free x-axis" = "free_x", "free")),
  
    selectInput(ns("position"), "Position of grouping variables",
                choices = c("identity", "dodge", "fill", "stack"),
                selected = "stack"),
  
    selectInput(ns("out"), "Output type",
                choices = c("plot and table" = "tab_plot", "plot", "table"),
                selected = "tab_plot"),
  
    selectInput(ns("format"), "Table format",
                choices = c("wide", "long"))
  )
}


roll_catchUI <- function(id) { 
  
  ns <- NS(id)
  tagList(

    actionButton(ns("run"), "Run",
                 style = "color: #fff; background-color: #6da363; border-color: #800000;"),
  
    uiOutput(ns("var_select")),
    
    uiOutput(ns("date_select")),
  
    uiOutput(ns("grp_select")),
  
    numericInput(ns("win"), "Window",
                 value = 10),
  
    selectInput(ns("fun"), "Aggregate function",
                choices = c("sum", "mean", "sd", "median", "min", "max", "IQR")),
  
    uiOutput(ns("fct_select")),
  
    selectizeInput(ns("ftype"), "filter type",
                   choices = c("year-month", "year-week", "year-day",
                               "year", "month", "week", "day"), multiple = TRUE,
                   options = list(maxItems = 1)),
  
    uiOutput(ns("filter_UI")),
  
    selectInput(ns("scale"), "Facet scale",
                choices = c("fixed", "free y-axis" = "free_y",
                            "free x-axis" = "free_x", "free")),
  
    selectInput(ns("align"), "Window alignment",
                choices = c("center", "left", "right")),
  
    selectInput(ns("conv"), "Convert catch",
                choices = c("none", "tons", "metric tons" = "metric_tons", "custom")),
  
    conditionalPanel("input.conv == 'custom'",
  
                     textInput(ns("conv"), "Enter function")),
  
    selectInput(ns("tran"), "Transform y-axis",
                choices = c("none" = "identity", "log", "log2", "log10", "sqrt")),
  
  
    selectInput(ns("out"), "Output type",
                choices = c("plot and table" = "tab_plot", "plot", "table"))
  )
}


weekly_catchUI <- function(id) {
  
  ns <- NS(id)
  tagList(

    actionButton(ns("run"), "Run",
                 style = "color: #fff; background-color: #6da363; border-color: #800000;"),
  
    uiOutput(ns("var_select")),
    
    uiOutput(ns("date_select")),
    
    uiOutput(ns("grp_select")),
    
    uiOutput(ns("fct_select")),
  
    selectInput(ns("fun"), "Aggregate function",
                choices = c("sum", "mean", "sd", "median", "min", "max", "IQR")),
  
    checkboxInput(ns("combine"), "Combine grouping variables",
                  value = FALSE),
  
    selectizeInput(ns("ftype"), "filter type",
                   choices = c("year-month", "year-week", "year-day",
                               "year", "month", "week", "day"), multiple = TRUE,
                   options = list(maxItems = 1)),
  
    uiOutput(ns("filter_UI")),
  
    selectInput(ns("value"), "Select value type",
                choices = c("count", "percent")),
  
    selectInput(ns("conv"), "Convert catch",
                choices = c("none", "tons", "metric tons" = "metric_tons", "custom")),
  
    conditionalPanel("input.conv == 'custom'",
  
                     textInput(ns("conv"), "Enter function")),
  
    selectInput(ns("tran"), "Transform y-axis",
                choices = c("none" = "identity", "log", "log2", "log10", "sqrt")),
  
    selectInput(ns("type"), "Plot type",
                choices = c("bar", "line")),
  
    selectInput(ns("scale"), "Facet scale",
                choices = c("fixed", "free y-axis" = "free_y",
                            "free x-axis" = "free_x", "free")),
  
    selectInput(ns("position"), "Position of grouping variables",
                choices = c("identity", "dodge", "fill", "stack"),
                selected = "stack"),
  
    selectInput(ns("out"), "Output type",
                choices = c("plot and table" = "tab_plot", "plot", "table"),
                selected = "tab_plot"),
  
    selectInput(ns("format"), "Table format",
                choices = c("wide", "long"))
  )
}


weekly_effortUI <- function(id) {
  
  ns <- NS(id)
  tagList(

    actionButton(ns("run"), "Run",
                 style = "color: #fff; background-color: #6da363; border-color: #800000;"),
  
    uiOutput(ns("var_select")),
    
    uiOutput(ns("date_select")),
    
    uiOutput(ns("grp_select")),
    
    uiOutput(ns("fct_select")),
  
    checkboxInput(ns("combine"), "Combine grouping variables",
                  value = FALSE),

    selectizeInput(ns("ftype"), "filter type",
                   choices = c("year-month", "year-week", "year-day",
                               "year", "month", "week", "day"), multiple = TRUE,
                   options = list(maxItems = 1)),
  
    uiOutput(ns("filter_UI")),
  
    selectInput(ns("conv"), "Convert catch",
                choices = c("none", "tons", "metric tons" = "metric_tons", "custom")),
  
    conditionalPanel("input.conv == 'custom'",
  
                     textInput(ns("conv"), "Enter function")),
  
    selectInput(ns("tran"), "Transform y-axis",
                choices = c("none" = "identity", "log", "log2", "log10", "sqrt")),
  
    selectInput(ns("scale"), "Facet scale",
                choices = c("fixed", "free y-axis" = "free_y",
                            "free x-axis" = "free_x", "free")),
  
    selectInput(ns("out"), "Output type",
                choices = c("plot and table" = "tab_plot", "plot", "table"),
                selected = "tab_plot"),
  
    selectInput(ns("format"), "Table format",
                choices = c("wide", "long"))
  )
}

bycatchUI <- function(id) {
  
  ns <- NS(id)
  tagList(

    p("Note: CPUE and catch variables should be added in the same order."),
  
    actionButton(ns("run"), "Run",
                 style = "color: #fff; background-color: #6da363; border-color: #800000;"),
  
    uiOutput(ns("cpue_select")),
    
    uiOutput(ns("catch_select")),
  
    uiOutput(ns("date_select")),
  
    selectInput(ns("period"), "Period",
                choices = c("year", "month", "weeks")),
  
    textInput(ns("nms"), "Names", value = NULL,
              placeholder = "optional names to be used in plot/table"),
  
    fluidRow(actionButton(ns("nms_add"), "Add name"),
             actionButton(ns("nms_clear"), "Clear names")),
  
    textOutput(ns("caption")),
  
    uiOutput(ns("grp_select")),
  
    checkboxInput(ns("combine"), "Combine grouping variables",
                  value = FALSE),
  
    uiOutput(ns("fct_select")),
    
    selectizeInput(ns("ftype"), "filter type",
                   choices = c("year-month", "year-week", "year-day",
                               "year", "month", "week", "day"), multiple = TRUE,
                   options = list(maxItems = 1)),
  
    uiOutput(ns("filter_UI")),
  
    selectInput(ns("value"), "Catch value type",
                choices = c("total", "share of total catch" = "stc")),
  
    selectInput(ns("tran"), "Transform y-axis",
                choices = c("none" = "identity", "log", "log2", "log10", "sqrt")),
  
    selectInput(ns("scale"), "Facet scale",
                choices = c("fixed", "free y-axis" = "free_y",
                            "free x-axis" = "free_x", "free")),
  
    selectInput(ns("out"), "Output type",
                choices = c("plot and table" = "tab_plot", "plot", "table"),
                selected = "tab_plot"),
  
    selectInput(ns("format"), "Table format",
                choices = c("wide", "long"))
  )
}

trip_lengthUI <- function(id) {
  
  ns <- NS(id)
  tagList(

  actionButton(ns("run"), "Run",
               style = "color: #fff; background-color: #6da363; border-color: #800000;"),

  uiOutput(ns("start_select")),
  
  uiOutput(ns("end_select")),

  selectInput(ns("unit"), "Unit of time",
              choices = c("minutes" = "mins", "hours", "days", "weeks"),
              selected = "hours"),

  uiOutput(ns("catch_select")),

  uiOutput(ns("haul_select")),

  uiOutput(ns("grp_select")),
  
  uiOutput(ns("fct_select")),
  
  selectizeInput(ns("ftype"), "filter type",
                 choices = c("year-month", "year-week", "year-day",
                             "year", "month", "week", "day"), multiple = TRUE,
                 options = list(maxItems = 1)),

  uiOutput(ns("filter_UI")),

  selectInput(ns("type"), "Plot type",
              choices = c("histogram" = "hist", "frequency polygon" = "freq_poly")),

  sliderInput(ns("bins"), "Bins",
              min = 1, max = 60, value = 30),

  checkboxInput(ns("dens"), "Density", value = TRUE),

  selectInput(ns("tran"), "Transform x-axis",
              choices = c("none" = "identity", "log", "log2", "log10", "sqrt")),

  selectInput(ns("scale"), "Facet scale",
              choices = c("fixed", "free y-axis" = "free_y",
                          "free x-axis" = "free_x", "free")),

  checkboxInput(ns("haul_trp"), "Convert to trip-level",
                value = FALSE),

  conditionalPanel("input.haul_trp",

                   uiOutput(ns("kwargs_select"))),

  selectInput(ns("format"), "Table format",
              choices = c("wide", "long")),

  selectInput(ns("out"), "Output type",
              choices = c("plot and table" = "tab_plot", "plot", "table"))
  )
}

fleet_tableUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
  
    p("Fleet tables must be saved to the FishSET database before they can be used to assign observations to fleets."),
    
    actionButton(ns("save"), "save to FishSET database",
                 style = "color: #fff; background-color: #6EC479; border-color:#000000;"),
    
    tags$br(), tags$br(),
    
    fluidRow(
      actionButton(ns("addrow"), "add row",
                   style = "color: #fff; background-color: #6EC479; border-color:#000000;"),
      
      actionButton(ns("deleterow"), "delete row",
                   style = "color: #fff; background-color: #EB8C34; border-color:#000000;")),
    
    tags$br(), 
    
    fluidRow(
      actionButton(ns("addcol"), "add column",
                   style = "color: #fff; background-color: #6EC479; border-color:#000000;"),
      
      actionButton(ns("deletecol"), "delete column",
                   style = "color: #fff; background-color: #EB8C34; border-color:#000000;")),
    
    tags$br(),
    
    fileInput(ns("file"), "browse file"),
    
    actionButton(ns("upload"), "upload table"),
    
    tags$br(), tags$br(),
    
    textInput(ns("colname"), "New column name"),
    
    actionButton(ns("colname_btn"), "change column name")
  )
}
  
  
fleet_assignUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
  
    actionButton(ns("refresh"), "", icon = icon("refresh")),
    
    selectInput(ns("tab"), "Available fleet tables",
                choices = grep("FleetTable", tables_database(), value = TRUE)),
    
    actionButton(ns("view_btn"), "view table"),
    
    checkboxInput(ns("overlap"), "allow overlapping fleet assignments?"),
    
    selectInput(ns("format"), "fleet format",
                choices = c("long", "wide")),
    
    actionButton(ns("run"), "run",
                 style = "color: #fff; background-color: #6da363; border-color: #800000;"),
    
    actionButton(ns("save"), "save table")
  )
}


density_plotOut <- function(id) {
  
  ns <- NS(id)
  shinycssloaders::withSpinner(plotOutput(ns("plot")))
}

fleetOut <- function(id) { 
  ns <- NS(id)
  uiOutput(ns("output")) 
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