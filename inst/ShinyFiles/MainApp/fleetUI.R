source("fleet_helpers.R", local = TRUE)

saveOutputUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    downloadLink(ns('downloadplotHIDE'), label = ''),
    actionButton(ns('downloadplot'), label = 'Save plot to folder',
                 style = "color: #fff; background-color: #6EC479; border-color:#000000;"),
    downloadLink(ns('downloadTableHIDE'), label = ''),
    actionButton(ns('downloadTable'), label = 'Save table to folder as csv',
                 style = "color: #fff; background-color: #6EC479; border-color:#000000;")
  )
}

plotSaveUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    downloadLink(ns('downloadplotHIDE'), label = ''),
    actionButton(ns('downloadplot'), label = 'Save plot to folder',
                 style = "color: #fff; background-color: #6EC479; border-color:#000000;")
  )
}

tableSaveUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    downloadLink(ns('downloadTableHIDE'), label = ''),
    actionButton(ns('downloadTable'), label = 'Save table to folder as csv',
                 style = "color: #fff; background-color: #6EC479; border-color:#000000;")
  )
}

noteUI <- function(id) {
  
  ns <- NS(id)
  tagList(
    actionButton(ns('callTextDownload'),'Save notes',
                 style = "color: #fff; background-color: #6EC479; border-color:#000000;"),
    textInput(ns('notes'), "Notes", value = NULL, 
              placeholder = 'Write notes to store in text output file. Text can be inserted into report later.')
  )
}

RexpressionUI <- function(id) {
  
  ns <- NS(id)
  tagList(
    textInput(ns("expr"), label = "Enter an R expression",
              value = "values$dataset"),
    actionButton(ns("run"), "Run", class = "btn-success"),
    
    div( style = "margin-top: 2em;",
         uiOutput(ns("result"))
    )
  )
}

saveDataTableUI <- function(id) {
  ns <- NS(id)
  actionButton(ns('saveData'),'Save data to FishSET database',
               style = "color: white; background-color: blue;")
}

refreshUI <- function(id) {
  
  ns <- NS(id)
  actionButton(ns("refresh"), "Refresh data", 
               icon = icon("refresh"),
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

runFunUI <- function(id) {
  ns <- NS(id)
  actionButton(ns("fun_run"), "Run function",
               style = "color: #fff; background-color: #6da363; border-color: #800000;")
}

filter_periodUI <- function(id, dat, date, type) {
  
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


filter_periodOut <- function(id, type, input) {
  
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

mergeUI <- function(id, dat_type) {
  
  dat_type <- switch(dat_type, "aux" = "auxiliary", "port" = "port", "grid" = "gridded", 
                     "spat" = "spatial")
  ns <- NS(id)
  tagList(
    checkboxInput(ns("merge_cb"), label = paste("Merge", dat_type, "table")),
    uiOutput(ns("mergeUI"))
  )
}


density_plotUI <- function(id, dat) {
  
  ns <- NS(id)
  
  tagList(
    
    selectInput(ns("type"), "Plot type",
                choices = c("Kernel density estimate" = "kde",
                            "Empirical cumulative distribution function" = "ecdf", 
                            "Cumulative distribution function" = "cdf", "all"), 
                multiple = TRUE, selected = "kde"),
    
    uiOutput(ns("var_select")),
    
    checkboxInput(ns("subset_cb"), strong("Subset (optional)"), value = FALSE),
    
    conditionalPanel("input.subset_cb", ns = ns, style = "margin-left:19px;",
                     
                     checkboxInput(ns("date_subset_cb"), strong("Subset by date")),
                     
                     conditionalPanel("input.date_subset_cb", ns = ns, 
                                    style = "margin-left:19px;",
                            
                                    uiOutput(ns("date_select")),
                                    
                                    conditionalPanel("typeof input.date !== 'undefined' && input.date.length > 0", ns = ns,
                                                     selectizeInput(ns("filter_date"), "Subset type",
                                                                    choices = c("date range" = "date_range", "year-month", 
                                                                                "year-week", "year-day", "year", "month", "week", "day"),
                                                                    multiple = TRUE, options = list(maxItems = 1))
                                    ),
                                    
                                    uiOutput(ns("filter_date_UIOutput"))
                                    ),
                     
                     checkboxInput(ns("var_subset_cb"), strong("Subset by variable")),
                     
                     conditionalPanel("input.var_subset_cb", ns = ns, 
                                    style = "margin-left:19px;",
                                    
                                    uiOutput(ns("filter_by_UIOutput")), 
                                    
                                    uiOutput(ns("filter_by_val_UIOutput")),
                     )
     ),
    
    checkboxInput(ns("group_cb"), strong("Group (optional)"), value = FALSE),
    
    conditionalPanel("input.group_cb",  ns = ns, style = "margin-left:19px;",
                     
                     uiOutput(ns("grp_select")),
                     
                     uiOutput(ns("grp_date_UI")),
                     
                     checkboxInput(ns("combine"), "Combine group variables", value = FALSE)),
    
    checkboxInput(ns("split_cb"), strong("Split (optional)"), value = FALSE),
    
    conditionalPanel("input.split_cb",  ns = ns, style = "margin-left:19px;",
                     
                     uiOutput(ns("fct_select")),
                     uiOutput(ns("fct_date_UI")),
    ),
    
    checkboxInput(ns("options_cb"), strong("Plot options"), value = FALSE),
    
    conditionalPanel("input.options_cb", ns = ns, style = "margin-left:19px;",
                     
      tagList(
    
        numericInput(ns("bw"), "Kernel bandwidth",
                     value = 1),
        
        selectInput(ns("tran"), "Transformation function",
                    choices = c("none" = "identity", "log", "log2", "log10", "sqrt")),
        
        selectInput(ns("scale"), "Split plot scale",
                    choices = c("fixed", "free y-axis" = "free_y", 
                                "free x-axis" = "free_x", "free")),
        
        selectInput(ns("position"), "Position of grouping variables",
                    choices = c("identity", "fill", "stack"), selected = "identity"),
        selectInput(ns("pages"), "single or multiple plots",
                    choices = c("single", "multiple" = "multi")))
    )
  )
}



vessel_countUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
     selectInput(ns("out"), "View table and/or plot",
                choices = c("plot and table" = "tab_plot", "plot", "table"),
                selected = "tab_plot"),
     
    uiOutput(ns("var_select")),
              
    selectInput(ns("period"), "Show counts by (optional)",
                choices = c("do not count by period" = "no_period", "year-month" = "year_month", "month-year" = "month_year",
                            "year", "month", "week", "day of the month" = "day_of_month",
                            "day of the year" = "day_of_year", "calender date" = "cal_date", 
                            "weekday"),
                multiple = FALSE, selected = "year_month"),
    
    # conditionalPanel("input.period !== 'no_period'", 
    #                  ns = ns, style = "margin-left:19px;",
      
      uiOutput(ns("date_select")),#),
    
    checkboxInput(ns("subset_cb"), strong("Subset (optional)"), value = FALSE),
    
    conditionalPanel("input.subset_cb", ns = ns, style = "margin-left:19px;",
                     
                     checkboxInput(ns("date_subset_cb"), strong("Subset by date")),
                     
                     conditionalPanel("input.date_subset_cb", ns = ns, 
                                      style = "margin-left:19px;",
                                      
                                      uiOutput(ns("sub_date_select")),
                                      
                                      conditionalPanel("typeof input.sub_date !== 'undefined' && input.sub_date.length > 0", ns = ns,
                                                       selectizeInput(ns("filter_date"), "Subset type",
                                                                      choices = c("date range" = "date_range", "year-month", 
                                                                                  "year-week", "year-day", "year", "month", "week", "day"),
                                                                      multiple = TRUE, options = list(maxItems = 1))
                                      ),
                                      
                                      uiOutput(ns("filter_date_UIOutput"))
                     ),
                     
                     checkboxInput(ns("var_subset_cb"), strong("Subset by variable")),
                     
                     conditionalPanel("input.var_subset_cb", ns = ns, 
                                      style = "margin-left:19px;",
                                      
                                      uiOutput(ns("filter_by_UIOutput")), 
                                      
                                      uiOutput(ns("filter_by_val_UIOutput")),
                     )
    ),
    
    checkboxInput(ns("group_cb"), strong("Group (optional)"), value = FALSE),
    
    conditionalPanel("input.group_cb",  ns = ns, style = "margin-left:19px;",
                     
                     uiOutput(ns("grp_select")),
                     
                     uiOutput(ns("grp_date_UI")),
                     
                     checkboxInput(ns("combine"), "Combine group variables", value = FALSE)),
    
    checkboxInput(ns("split_cb"), strong("Split (optional)"), value = FALSE),
    
    conditionalPanel("input.split_cb",  ns = ns, style = "margin-left:19px;",
                     
                     uiOutput(ns("fct_select")),
                     uiOutput(ns("fct_date_UI")),
    ),
    
    checkboxInput(ns("options_cb"), strong("Plot options"), value = FALSE),
    
    conditionalPanel("input.options_cb", ns = ns, style = "margin-left:19px;",
                     
      tagList(
    
        selectInput(ns("value"), "Select value type",
                    choices = c("count", "percent")),
        
        selectInput(ns("tran"), "Transformation function",
                    choices = c("none" = "identity", "log", "log2", "log10", "sqrt")),
        
        selectInput(ns("type"), "Plot type",
                    choices = c("bar", "line")),
        
        selectInput(ns("scale"), "Split plot scale",
                    choices = c("fixed", "free y-axis" = "free_y", 
                                "free x-axis" = "free_x", "free")),
        
        selectInput(ns("position"), "Position of grouping variables",
                    choices = c("identity", "dodge", "fill", "stack"),
                    selected = "stack"))
      )
  )
}


species_catchUI <- function(id) {
  
  ns <- NS(id)
  tagList(
    selectInput(ns("out"), "View table and/or plot",
                choices = c("plot and table" = "tab_plot", "plot", "table"),
                selected = "tab_plot"),
    
    selectInput(ns("fun"), "Summary function",
                choices = c("sum", "mean", "median", "min", "max")),
    
    uiOutput(ns("var_select")),
    
    selectizeInput(ns("period"), "Show counts by (optional)",
                   choices = c("do not count by period" = "no_period", "year-month" = "year_month", "month-year" = "month_year",
                               "year", "month", "week", "day of the month" = "day_of_month",
                               "day of the year" = "day_of_year", "calender date" = "cal_date", 
                               "weekday"),
                   multiple = FALSE, selected = "year_month"),
    
    conditionalPanel("input.period !== 'no_period'", 
                     ns = ns, style = "margin-left:19px;",
                     
                     uiOutput(ns("date_select"))),
    
    checkboxInput(ns("subset_cb"), strong("Subset (optional)"), value = FALSE),
    
    conditionalPanel("input.subset_cb", ns = ns, style = "margin-left:19px;",
                     
                     checkboxInput(ns("date_subset_cb"), strong("Subset by date")),
                     
                     conditionalPanel("input.date_subset_cb", ns = ns, 
                                      style = "margin-left:19px;",
                                      
                                      uiOutput(ns("sub_date_select")),
                                      
                                      conditionalPanel("typeof input.sub_date !== 'undefined' && input.sub_date.length > 0", ns = ns,
                                                       selectizeInput(ns("filter_date"), "Subset type",
                                                                      choices = c("date range" = "date_range", "year-month", 
                                                                                  "year-week", "year-day", "year", "month", "week", "day"),
                                                                      multiple = TRUE, options = list(maxItems = 1))
                                      ),
                                      
                                      uiOutput(ns("filter_date_UIOutput"))
                     ),
                     
                     checkboxInput(ns("var_subset_cb"), strong("Subset by variable")),
                     
                     conditionalPanel("input.var_subset_cb", ns = ns, 
                                      style = "margin-left:19px;",
                                      
                                      uiOutput(ns("filter_by_UIOutput")), 
                                      
                                      uiOutput(ns("filter_by_val_UIOutput")),
                     )
    ),
    
    checkboxInput(ns("group_cb"), strong("Group (optional)"), value = FALSE),
    
    conditionalPanel("input.group_cb",  ns = ns, style = "margin-left:19px;",
                     
                     uiOutput(ns("grp_select")),
                     
                     uiOutput(ns("grp_date_UI")),
                     
                     checkboxInput(ns("combine"), "Combine group variables", value = FALSE)),
    
    checkboxInput(ns("split_cb"), strong("Split (optional)"), value = FALSE),
    
    conditionalPanel("input.split_cb",  ns = ns, style = "margin-left:19px;",
                     
                     uiOutput(ns("fct_select")),
                     uiOutput(ns("fct_date_UI")),
    ),
    
    checkboxInput(ns("options_cb"), strong("Plot options"), value = FALSE),
    
    conditionalPanel("input.options_cb", ns = ns, style = "margin-left:19px;",
        
      tagList(
    
        selectInput(ns("value"), "Select value type",
                    choices = c("count", "percent")),
        
        selectInput(ns("conv"), "Convert catch",
                    choices = c("none", "tons", "metric tons" = "metric_tons", "custom")),
        
        conditionalPanel("input.conv == 'custom'", ns = ns, style = "margin-left:19px;",
                         
                         textInput(ns("conv"), "Enter function")),
        
        selectInput(ns("tran"), "Transform y-axis",
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
                    choices = c("wide", "long"))
      )
    )
  )
}


roll_catchUI <- function(id) { 
  
  ns <- NS(id)
  tagList(
    selectInput(ns("out"), "View table and/or plot",
                choices = c("plot and table" = "tab_plot", "plot", "table")),
    
    selectInput(ns("fun"), "Summary function",
                choices = c("mean", "sum", "sd", "median", "min", "max", 
                            "interquartile range")),
    
    uiOutput(ns("var_select")),
    
    uiOutput(ns("date_select")),
    
  checkboxInput(ns("subset_cb"), strong("Subset (optional)"), value = FALSE),
    
  conditionalPanel("input.subset_cb", ns = ns, style = "margin-left:19px;",
                   
                   checkboxInput(ns("date_subset_cb"), strong("Subset by date")),
                   
                   conditionalPanel("input.date_subset_cb", ns = ns, 
                                    style = "margin-left:19px;",
                                    
                                    uiOutput(ns("sub_date_select")),
                                    
                                    conditionalPanel("typeof input.sub_date !== 'undefined' && input.sub_date.length > 0", ns = ns,
                                                     selectizeInput(ns("filter_date"), "Subset type",
                                                                    choices = c("date range" = "date_range", "year-month", 
                                                                                "year-week", "year-day", "year", "month", "week", "day"),
                                                                    multiple = TRUE, options = list(maxItems = 1))
                                    ),
                                    
                                    uiOutput(ns("filter_date_UIOutput"))
                   ),
                   
                   checkboxInput(ns("var_subset_cb"), strong("Subset by variable")),
                   
                   conditionalPanel("input.var_subset_cb", ns = ns, 
                                    style = "margin-left:19px;",
                                    
                                    uiOutput(ns("filter_by_UIOutput")), 
                                    
                                    uiOutput(ns("filter_by_val_UIOutput")),
                   )
  ),
  
  checkboxInput(ns("group_cb"), strong("Group (optional)"), value = FALSE),
  
  conditionalPanel("input.group_cb",  ns = ns, style = "margin-left:19px;",
                   
                   uiOutput(ns("grp_select")),
                   
                   uiOutput(ns("grp_date_UI")),
                   
                   checkboxInput(ns("combine"), "Combine group variables", value = FALSE)),
  
  checkboxInput(ns("split_cb"), strong("Split (optional)"), value = FALSE),
  
  conditionalPanel("input.split_cb",  ns = ns, style = "margin-left:19px;",
                   
                   uiOutput(ns("fct_select")),
                   uiOutput(ns("fct_date_UI")),
  ),
    
    checkboxInput(ns("options_cb"), strong("Plot options"), value = FALSE),
    
    conditionalPanel("input.options_cb", ns = ns, style = "margin-left:19px;",
                     
      tagList(
    
        selectInput(ns("scale"), "Split plot scale",
                    choices = c("fixed", "free y-axis" = "free_y",
                                "free x-axis" = "free_x", "free")),
        
        numericInput(ns("win"), "Window width (days)",
                     value = 10),
        
        selectInput(ns("align"), "Window alignment",
                    choices = c("center", "left", "right")),
        
        selectInput(ns("conv"), "Convert catch",
                    choices = c("none", "tons", "metric tons" = "metric_tons", "custom")),
        
        conditionalPanel("input.conv == 'custom'", ns = ns, style = "margin-left:19px;",
                         
                         textInput(ns("conv"), "Enter function")),
        
        selectInput(ns("tran"), "Transform y-axis",
                    choices = c("none" = "identity", "log", "log2", "log10", "sqrt")))
    )
  )
}


weekly_catchUI <- function(id) {
  
  ns <- NS(id)
  tagList(
    selectInput(ns("out"), "View table and/or plot",
                choices = c("plot and table" = "tab_plot", "plot", "table"),
                selected = "tab_plot"),
    
    selectInput(ns("fun"), "Summary function",
                choices = c("sum", "mean", "median", "min", "max")),
    
    uiOutput(ns("var_select")),
    
    uiOutput(ns("date_select")),
    
    checkboxInput(ns("subset_cb"), strong("Subset (optional)"), value = FALSE),
    
    conditionalPanel("input.subset_cb", ns = ns, style = "margin-left:19px;",
                     
                     checkboxInput(ns("date_subset_cb"), strong("Subset by date")),
                     
                     conditionalPanel("input.date_subset_cb", ns = ns, 
                                      style = "margin-left:19px;",
                                      
                                      uiOutput(ns("sub_date_select")),
                                      
                                      conditionalPanel("typeof input.sub_date !== 'undefined' && input.sub_date.length > 0", ns = ns,
                                                       selectizeInput(ns("filter_date"), "Subset type",
                                                                      choices = c("date range" = "date_range", "year-month", 
                                                                                  "year-week", "year-day", "year", "month", "week", "day"),
                                                                      multiple = TRUE, options = list(maxItems = 1))
                                      ),
                                      
                                      uiOutput(ns("filter_date_UIOutput"))
                     ),
                     
                     checkboxInput(ns("var_subset_cb"), strong("Subset by variable")),
                     
                     conditionalPanel("input.var_subset_cb", ns = ns, 
                                      style = "margin-left:19px;",
                                      
                                      uiOutput(ns("filter_by_UIOutput")), 
                                      
                                      uiOutput(ns("filter_by_val_UIOutput")),
                     )
    ),
    
    checkboxInput(ns("group_cb"), strong("Group (optional)"), value = FALSE),
    
    conditionalPanel("input.group_cb",  ns = ns, style = "margin-left:19px;",
                     
                     uiOutput(ns("grp_select")),
                     
                     uiOutput(ns("grp_date_UI")),
                     
                     checkboxInput(ns("combine"), "Combine group variables", value = FALSE)),
    
    checkboxInput(ns("split_cb"), strong("Split (optional)"), value = FALSE),
    
    conditionalPanel("input.split_cb",  ns = ns, style = "margin-left:19px;",
                     
                     uiOutput(ns("fct_select")),
                     uiOutput(ns("fct_date_UI")),
    ),
    
    checkboxInput(ns("options_cb"), strong("Plot options"), value = FALSE),
    
    conditionalPanel("input.options_cb", ns = ns, style = "margin-left:19px;",
                     
      tagList(
    
        selectInput(ns("value"), "Select value type",
                    choices = c("count", "percent")),
        
        selectInput(ns("conv"), "Convert catch",
                    choices = c("none", "tons", "metric tons" = "metric_tons", "custom")),
        
        conditionalPanel("input.conv == 'custom'", ns = ns, style = "margin-left:19px;",
                         
                         textInput(ns("conv"), "Enter function")),
        
        selectInput(ns("tran"), "Transform y-axis",
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
                    choices = c("wide", "long")))
    )
  )
}


weekly_effortUI <- function(id) {
  
  ns <- NS(id)
  tagList(
    selectInput(ns("out"), "View table and/or plot",
                choices = c("plot and table" = "tab_plot", "plot", "table"),
                selected = "tab_plot"),
    
    uiOutput(ns("var_select")),
    
    uiOutput(ns("date_select")),
    
    checkboxInput(ns("subset_cb"), strong("Subset (optional)"), value = FALSE),
    
    conditionalPanel("input.subset_cb", ns = ns, style = "margin-left:19px;",
                     
                     checkboxInput(ns("date_subset_cb"), strong("Subset by date")),
                     
                     conditionalPanel("input.date_subset_cb", ns = ns, 
                                      style = "margin-left:19px;",
                                      
                                      uiOutput(ns("sub_date_select")),
                                      
                                      conditionalPanel("typeof input.sub_date !== 'undefined' && input.sub_date.length > 0", ns = ns,
                                                       selectizeInput(ns("filter_date"), "Subset type",
                                                                      choices = c("date range" = "date_range", "year-month", 
                                                                                  "year-week", "year-day", "year", "month", "week", "day"),
                                                                      multiple = TRUE, options = list(maxItems = 1))
                                      ),
                                      
                                      uiOutput(ns("filter_date_UIOutput"))
                     ),
                     
                     checkboxInput(ns("var_subset_cb"), strong("Subset by variable")),
                     
                     conditionalPanel("input.var_subset_cb", ns = ns, 
                                      style = "margin-left:19px;",
                                      
                                      uiOutput(ns("filter_by_UIOutput")), 
                                      
                                      uiOutput(ns("filter_by_val_UIOutput")),
                     )
    ),
    
    checkboxInput(ns("group_cb"), strong("Group (optional)"), value = FALSE),
    
    conditionalPanel("input.group_cb",  ns = ns, style = "margin-left:19px;",
                     
                     uiOutput(ns("grp_select")),
                     
                     uiOutput(ns("grp_date_UI")),
                     
                     checkboxInput(ns("combine"), "Combine group variables", value = FALSE)),
    
    checkboxInput(ns("split_cb"), strong("Split (optional)"), value = FALSE),
    
    conditionalPanel("input.split_cb",  ns = ns, style = "margin-left:19px;",
                     
                     uiOutput(ns("fct_select")),
                     uiOutput(ns("fct_date_UI")),
    ),
    
    checkboxInput(ns("options_cb"), strong("Plot options"), value = FALSE),
    
    conditionalPanel("input.options_cb", ns = ns, style = "margin-left:19px;",
                     
      tagList(
        
        selectInput(ns("tran"), "Transform y-axis",
                    choices = c("none" = "identity", "log", "log2", "log10", "sqrt")),
        
        selectInput(ns("scale"), "Split plot scale",
                    choices = c("fixed", "free y-axis" = "free_y",
                                "free x-axis" = "free_x", "free")),
        
        selectInput(ns("format"), "Table format",
                    choices = c("wide", "long")))
    )
  )
}

bycatchUI <- function(id) {
  
  ns <- NS(id)
  tagList(
    selectInput(ns("out"), "View table and/or plot",
                choices = c("plot and table" = "tab_plot", "plot", "table"),
                selected = "tab_plot"),
    
    p("Note: CPUE and catch variables should be added in the same order. CPUE can
      be created in the Compute New Variables tab."),
    uiOutput(ns("cpue_select")),
    
    uiOutput(ns("catch_select")),
    
    uiOutput(ns("date_select")),
    
    selectInput(ns("period"), "Show counts by",
                choices = c("year", "month", "week")),
    
    checkboxInput(ns("subset_cb"), strong("Subset (optional)"), value = FALSE),
    conditionalPanel("input.subset_cb", ns = ns, style = "margin-left:19px;",
                     
                     checkboxInput(ns("date_subset_cb"), strong("Subset by date")),
                     
                     conditionalPanel("input.date_subset_cb", ns = ns, 
                                      style = "margin-left:19px;",
                                      
                                      uiOutput(ns("sub_date_select")),
                                      
                                      conditionalPanel("typeof input.sub_date !== 'undefined' && input.sub_date.length > 0", ns = ns,
                                                       selectizeInput(ns("filter_date"), "Subset type",
                                                                      choices = c("date range" = "date_range", "year-month", 
                                                                                  "year-week", "year-day", "year", "month", "week", "day"),
                                                                      multiple = TRUE, options = list(maxItems = 1))
                                      ),
                                      
                                      uiOutput(ns("filter_date_UIOutput"))
                     ),
                     
                     checkboxInput(ns("var_subset_cb"), strong("Subset by variable")),
                     
                     conditionalPanel("input.var_subset_cb", ns = ns, 
                                      style = "margin-left:19px;",
                                      
                                      uiOutput(ns("filter_by_UIOutput")), 
                                      
                                      uiOutput(ns("filter_by_val_UIOutput")),
                     )
    ),
    
    checkboxInput(ns("group_cb"), strong("Group (optional)"), value = FALSE),
    
    conditionalPanel("input.group_cb",  ns = ns, style = "margin-left:19px;",
                     
                     uiOutput(ns("grp_select")),
                     
                     uiOutput(ns("grp_date_UI")),
                     
                     checkboxInput(ns("combine"), "Combine group variables", value = FALSE)),
    
    checkboxInput(ns("split_cb"), strong("Split (optional)"), value = FALSE),
    
    conditionalPanel("input.split_cb",  ns = ns, style = "margin-left:19px;",
                     
                     uiOutput(ns("fct_select")),
                     uiOutput(ns("fct_date_UI")),
    ),
    
    checkboxInput(ns("options_cb"), strong("Plot options"), value = FALSE),
    
    conditionalPanel("input.options_cb", ns = ns, style = "margin-left:19px;",
                       
        tagList(
          
          textInput(ns("nms"), "Names", value = NULL,
                    placeholder = "optional names to be used in plot/table"),
          
          fluidRow(actionButton(ns("nms_add"), "Add name"),
                   actionButton(ns("nms_clear"), "Clear names")),
          
          tags$br(),
          
          conditionalPanel("typeof input.nms !== 'undefined' && input.nms.length > 0", ns = ns,
                           
                           div(class = "well well-sm", style = "background-color: white",
                               textOutput(ns("caption")))
          ),
          
          tags$br(),
      
          selectInput(ns("value"), "Catch value type",
                      choices = c("total", "share of total catch" = "stc")),
          
          selectInput(ns("tran"), "Transform y-axis",
                      choices = c("none" = "identity", "log", "log2", "log10", "sqrt")),
          
          selectInput(ns("scale"), "Split plot scale",
                      choices = c("fixed", "free y-axis" = "free_y",
                                  "free x-axis" = "free_x", "free")),
          
          selectInput(ns("format"), "Table format",
                      choices = c("wide", "long")))
    )
  )
}

trip_lengthUI <- function(id) {
  
  ns <- NS(id)
  tagList(
    p("Trip_length assumes the data is at trip level."),
    selectInput(ns("out"), "View table and/or plot",
                choices = c("plot and table" = "tab_plot", "plot", "table")),
    
    checkboxInput(ns("haul_trp"), strong("Convert to trip-level (optional)"),
                  value = FALSE),
    
    conditionalPanel("input.haul_trp", ns = ns, style = "margin-left:19px;",
                     
                     uiOutput(ns("htp_select")),
                     selectInput(ns("fun_time"), "Collapse temporal variables using", 
                                 choices = c("min", "mean", "max")),
                     selectInput(ns("fun_numeric"), "Collapse numeric variables using",
                                 choices = c("min", "mean", "max", "sum")),
                     checkboxInput(ns("haul_count"), strong("Create hauls per trip variable"), value = TRUE)),
    
    uiOutput(ns("start_select")),
    
    uiOutput(ns("end_select")),
    
    selectInput(ns("unit"), "Unit of time",
                choices = c("minutes" = "mins", "hours", "days", "weeks"),
                selected = "hours"),
    
    uiOutput(ns("vpue_select")),
    
    checkboxInput(ns("subset_cb"), strong("Subset (optional)"), value = FALSE),
    
    conditionalPanel("input.subset_cb", ns = ns, style = "margin-left:19px;",
                     
                     checkboxInput(ns("date_subset_cb"), strong("Subset by date")),
                     
                     conditionalPanel("input.date_subset_cb", ns = ns, 
                                      style = "margin-left:19px;",
                                      
                                      uiOutput(ns("sub_date_select")),
                                      
                                      conditionalPanel("typeof input.sub_date !== 'undefined' && input.sub_date.length > 0", ns = ns,
                                                       selectizeInput(ns("filter_date"), "Subset type",
                                                                      choices = c("date range" = "date_range", "year-month", 
                                                                                  "year-week", "year-day", "year", "month", "week", "day"),
                                                                      multiple = TRUE, options = list(maxItems = 1))
                                      ),
                                      
                                      uiOutput(ns("filter_date_UIOutput"))
                     ),
                     
                     checkboxInput(ns("var_subset_cb"), strong("Subset by variable")),
                     
                     conditionalPanel("input.var_subset_cb", ns = ns, 
                                      style = "margin-left:19px;",
                                      
                                      uiOutput(ns("filter_by_UIOutput")), 
                                      
                                      uiOutput(ns("filter_by_val_UIOutput")),
                     )
    ),
    
    checkboxInput(ns("group_cb"), strong("Group (optional)"), value = FALSE),
    
    conditionalPanel("input.group_cb",  ns = ns, style = "margin-left:19px;",
                     
                     uiOutput(ns("grp_select")),
                     
                     uiOutput(ns("grp_date_UI")),
                     
                     checkboxInput(ns("combine"), "Combine group variables", value = FALSE)),
    
    checkboxInput(ns("split_cb"), strong("Split (optional)"), value = FALSE),
    
    conditionalPanel("input.split_cb",  ns = ns, style = "margin-left:19px;",
                     
                     uiOutput(ns("fct_select")),
                     uiOutput(ns("fct_date_UI")),
    ),
    
    checkboxInput(ns("options_cb"), strong("Plot options"), value = FALSE),
    
    conditionalPanel("input.options_cb", ns = ns, style = "margin-left:19px;",
                     
      tagList(
    
        selectInput(ns("type"), "Plot type",
                    choices = c("histogram" = "hist", "frequency polygon" = "freq_poly")),
        
        selectInput(ns("pages"), "Plot output", choices = c("single", "mulitple" = "multi"), 
                    selected = "multi"),
        
        sliderInput(ns("bins"), "Bins",
                    min = 2, max = 60, value = 30),
        
        checkboxInput(ns("dens"), "Density (y-axis)", value = FALSE),
        
        checkboxInput(ns("rm_neg"), "Remove negative durations", value = FALSE),
        
        selectInput(ns("tran"), "Transform x-axis",
                    choices = c("none" = "identity", "log", "log2", "log10", "sqrt")),
        
        selectInput(ns("scale"), "Split plot scale",
                    choices = c("fixed", "free y-axis" = "free_y",
                                "free x-axis" = "free_x", "free")),
        
        selectInput(ns("format"), "Table format",
                    choices = c("wide", "long")))
    )
  )
}


fleet_tableUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    p("Fleet definitions must be saved to the FishSET database before they can be 
      used to assign vessels to fleets."),
    p("Create fleet table on the right or upload from local file below."),
    p(strong("Steps for Expression Builder:")),
    p(strong("1)"), "Select a variable, operator, and value to create an expression. 
      Select \"add to expression\" to expand the expression if needed."),
    p(strong("2)"), "Select \"Insert expression\" to insert an expression into the Fleet Definition Table. 
      To insert into a specific cell, click \"select cell\", choose a cell, then select \"Insert expression\"."),
    p(strong("3)"), "Double-click the definition table and enter a fleet name. Press ctrl+Enter to save the name."),
    p(strong("4)"), "Click \"Reset expression\" button to create a new expression."),
    p("To edit the definition table double-click on the cell. Press crtl+Enter 
      to save changes."),
    p(strong("5)"), "Click the \"Save table to FishSET database\" button to save the table."),
    
    tags$br(),
    
    actionButton(ns("save"), "Save table to FishSET database",
                 style = "color: #fff; background-color: #6EC479; border-color:#000000;"),
    tags$br(), tags$br(),
    
    fileInput(ns("file"), "Import table from local file"),
    
    actionButton(ns("upload"), "Import table",
                 style = "color: white; background-color: blue;"),
    
    tags$br(), tags$br(),
    
    conditionalPanel("input.upload > 0", ns = ns,
      textInput(ns("colname"), label = NULL, placeholder = "Enter new column name"),
      
      actionButton(ns("colname_btn"), "Change column name",
                   style = "color: white; background-color: blue;")
    ),
    
    tags$br(), tags$br()
  )
}

# expression builder module (add expr row)
nexpr_row_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    div(class = "n-expr-section", style = "width: 85%; padding: 0px;",
        fluidRow(
          column(2, selectInput(ns("log_oper"), "", 
                                choices = c("AND" = "&", "OR" = "|"))),
          
          column(3, uiOutput(ns("varUI"))),
          
          column(3, selectizeInput(ns("oper"), "", 
                                choices = c("less than" = "<", "greater than" = ">", "less than or equal to" = "<=", 
                                            "greater than or equal to" = ">=", "equal to" = "==", "not equal to" = "!=",
                                            "contains" = "%in%"),  multiple = TRUE, options = list(maxItems = 1))),
          
          column(4, uiOutput(ns("valueUI")))
        )
    )
  )
} 

fleet_exprUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h4(strong("Expression Builder")),
    
    div(style = "width: 85%; padding: 0px;",
        fluidRow(
          column(3, uiOutput(ns("select_var")), offset = 2),
          
          column(3,
                 selectizeInput(ns("nexpr_1-oper"), "Operator", 
                             choices = c("less than" = "<","greater than" = ">", "less than or equal to" = "<=", 
                                         "greater than or equal to" = ">=", "equal to" = "==", "not equal to" = "!=",
                                         "contains" = "%in%"), multiple = TRUE, options = list(maxItems = 1))
                 ),
          
          column(4, uiOutput(ns("valueUI")))
        )),
    
    div(id = "n_expr_container"),
    
    actionButton(ns("add_expr"), label = "add to expression", icon = icon(name = "plus"), 
                 style = "background-color: blue; color: white;"), 
    
    tags$br(), tags$br(),
    h6(strong("Expression")),
    verbatimTextOutput(ns("expr_txt")),
    tags$br(), tags$br(),
    
    actionButton(ns("reset_expr"), "Reset expression", style = "color: white; background-color: blue;"),
    actionButton(ns("insert_expr"), "Insert expression", style = "color: white; background-color: green;"),
    
    tags$br(), tags$br()
  )
}


fleet_assignUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    actionButton(ns("fun_run"), "Assign fleets",
                 style = "color: #fff; background-color: #6da363; border-color: #800000;"),
    
    tags$br(), tags$br(),
    
    p(strong("Steps:")),
    p(strong("1)"), "Load definition table from database."),
    p(strong("2)"), "Assign fleets."),
    
    tags$br(), 
    
    p(strong("Import table")),
    
    actionButton(ns("refresh_tabs"), "Load tables", icon = icon("refresh"), style = "color: blue;"),
    
    uiOutput(ns("available_tabs")),
    
    actionButton(ns("load_btn"), "Load table",
                 style = "color: white; background-color: blue;"),
    
    tags$br(), tags$br(),
    
    p(strong("Options")),
    
    checkboxInput(ns("overlap"), "Allow overlapping fleet assignments"),
    
    selectInput(ns("format"), "Fleet variable type",
                choices = c("string (single column)" = "string", "dummy variable (multi columns)" = "dummy")),
    
    tags$br(), tags$br()
  )
}


density_plotOut <- function(id) {
  
  ns <- NS(id)
  shinycssloaders::withSpinner(plotOutput(ns("plot")))
}

fleetOut <- function(id) { 
  ns <- NS(id)
  
  tagList(
    shinycssloaders::withSpinner(uiOutput(ns("output"))) 
  )
}

fleet_tableOut <- function(id) {
  ns <- NS(id)
  tagList(
    h4(strong("Fleet Definition Table")),
    
    fluidRow(
     
      column(6,
        div(style = "background-color: yellow; border: 1px solid #999; margin: 5px; margin-bottom: 2em;",
          p("Double-click to edit table. Press Crtl + Enter to save changes, Esc to exit edit mode.")))
    ),
    
    fluidRow(
      
      div(style = "margin-left: 19px;",
          actionButton(ns("addrow"), "Add row",
                   style = "color: #fff; background-color: #6EC479; border-color:#000000;"),
          
          actionButton(ns("deleterow"), "Delete row",
                       style = "color: #fff; background-color: #EB8C34; border-color:#000000;"),
          
          conditionalPanel("input.upload > 0", ns = ns,
            
            actionButton(ns("addcol"), "Add column",
                         style = "color: #fff; background-color: #6EC479; border-color:#000000;"),
            
            actionButton(ns("deletecol"), "Delete column",
                         style = "color: #fff; background-color: #EB8C34; border-color:#000000;"))
          )
      ),
    
    tags$br(),
    
    fluidRow(column(8,
                    actionButton(ns("deselect"), "Deselect", 
                                 style = "background-color:#E8E8E8;
                                    color:#000000; border-color:#BEBEBE;
                                    border-style:double; border-width:3px;
                                    border-radius:0%; font-size:14px;"),
                    
                    actionButton(ns("select_row"), "Select row", 
                                 style = "background-color:#E8E8E8;
                                    color:#000000; border-color:#BEBEBE;
                                    border-style:double; border-width:3px;
                                    border-radius:0%; font-size:14px;"),
                    
                    actionButton(ns("select_column"), "Select column", 
                                 style = "background-color:#E8E8E8;
                                    color:#000000; border-color:#BEBEBE;
                                    border-style:double; border-width:3px;
                                    border-radius:0%; font-size:14px;"),
                    
                    actionButton(ns("select_cell"), "Select cell", 
                                 style = "background-color:#E8E8E8;
                                   color:#000000; border-color:#BEBEBE;
                                   border-style:double; border-width:3px;
                                   border-radius:0%; font-size:14px;"))),
  DT::DTOutput(ns("f_tab")),
  
  tags$br(), tags$br(),
  
  fluidRow(column(4, 
                  h4(strong("Reference Table")),
                  tableOutput(ns("reference"))))
  )
}

fleet_assignOut <- function(id) {
  
  ns <- NS(id)
  tagList(
    h4(strong("Fleet Definition Table")),
    fluidRow(
      column(6, 
        div(style = "background-color: yellow; border: 1px solid #999; margin: 5px; margin-bottom: 2em;", 
          p("Assign specific definitions by clicking on the table row.
            The entire table will be used if no rows are selected.")))
      ),
    shinycssloaders::withSpinner(DT::DTOutput(ns("tab_preview"))),
    h4(strong("Dataset")),
    shinycssloaders::withSpinner(DT::DTOutput(ns("final_tab"))),
    h4(strong("Fleet Frequency")),
    fluidRow(
    column(8,
           shinycssloaders::withSpinner(plotOutput(ns("plot_count")))),
    column(4,
           shinycssloaders::withSpinner(DT::DTOutput(ns("tab_count"))))
    )
  )
}