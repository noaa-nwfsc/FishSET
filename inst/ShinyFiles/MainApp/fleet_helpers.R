
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

validate_date <- function(date = NULL, period = NULL, filter_date = NULL, fct = NULL, grp = NULL) {
  
  # if (!is.null(date) & is.null(period)) {
  #   
  #   validate("Please enter a period to summarize by.")
  # }
  
  if (is.null(date) & !is.null(period)) {
    
    validate("Please enter a date variable.")
  }
  
  if (!is.null(filter_date) & is.null(date)) {
    
    validate("Please enter a date variable.")
  }
  
  if (!is.null(fct)) {
    
    if (fct %in% c("week", "month", "year") & is.null(date)) {
      
      validate("Please enter a date variable.")
    }
  }
  
  if (!is.null(grp)) {
    
    if (grp %in% c("week", "month", "year") & is.null(date)) {
      
      validate("Please enter a date variable.")
    }
  }
}

n_plot_output <- function(out) {
    
    if ("ggplot" %in% class(out)) {
       
      tagList(renderPlot({ out }))
      
    } else if ("gtable" %in% class(out)) {
      
      tagList(renderPlot(gridExtra::grid.arrange(out)))
        
      
    } else if (all(class(out) == "list")) {
      
      lapply(out, function(x) renderPlot(x))
    }
}

n_tab_output <- function(out) {
  
  if (is.data.frame(out)) {
    
    tagList(DT::renderDT({ out }))
    
  } else {
    
    lapply(seq_along(out), function(x) DT::renderDT(out[[x]]))
  }
}

tabplot_output <- function(out, out_type) {
  
  if (out_type == "plot") {
    
    n_plot_output(out)
    
  } else if (out_type == "table") {
      
    n_tab_output(out)
      
  } else if (out_type == "tab_plot") {
    
   tagList(n_plot_output(out$plot), n_tab_output(out$table))
  }
}


filter_select <- function(dataset, col) {
  
  if (is.null(col)) {
    
    NULL
  
  } else {
    
   out <- unique(dataset[[col]])
   
   if (length(out) < 15) out else out[1:15]
   
  }
}
