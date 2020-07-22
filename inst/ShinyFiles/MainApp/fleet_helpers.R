
date_select <- function(dat) {
  
  if (any(grepl("date", colnames(dat), ignore.case = TRUE))) {
    
    grep("date", colnames(dat), ignore.case = TRUE, value = TRUE)[1]
    
  } else {
    
    colnames(dat)[1]
  }
}

date_cols <- function(dat) {
  
  if (any(grepl("date", colnames(dat), ignore.case = TRUE))) {
    
    grep("date", colnames(dat), ignore.case = TRUE, value = TRUE)
    
  }
}

numeric_cols <- function(dat) {
  
  colnames(dat)[vapply(dat,  FUN = is.numeric, FUN.VALUE = logical(1))]
}


yearRange <- function(dat, yr, type) {
  
  if (!is.null(yr)) {
    
    yr_mod <- date_parser(dat[[yr]])
    
    if (type == "min") {
      
      as.integer(format(min(yr_mod, na.rm = TRUE), "%Y"))
    } else if (type == "max") {
      as.integer(format(max(yr_mod, na.rm = TRUE), "%Y"))
    }
    
  } else {
    
    NULL
  }
}

tabplot_output <- function(out, out_type) {
  
  if (out_type == "plot") {
    
    shinycssloaders::withSpinner(list(renderPlot({ out })))
    
  } else if (out_type == "table") {
    
    shinycssloaders::withSpinner(list(DT::renderDT({ out })))
    
  } else if (out_type == "tab_plot") {
    
    shinycssloaders::withSpinner(
      tagList(
        renderPlot({out$plot}),
        DT::renderDT({out$table})
      ))
  }
}