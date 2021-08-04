
date_select <- function(dat) {
  
  if (any(grepl("date", colnames(dat), ignore.case = TRUE))) {
    
    grep("date", colnames(dat), ignore.case = TRUE, value = TRUE)
    
  } else {
    
    colnames(dat)
  }
}

category_cols <- function(dat) {
  
  class_list <- lapply(dat, class) 
  cat_class <- c("factor", "character", "integer", "logical")
  choices <- vapply(class_list, function(x) any(cat_class %in% x), FUN.VALUE = logical(1))
  
  names(choices[choices])
}

fleet_col <- function(cols) {
  
  if (any(grepl("fleet", cols, ignore.case = TRUE))) {
    grep("fleet", cols, ignore.case = TRUE, value = TRUE)[1]
  } else {
    cols[1]
  }
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

validate_date <- function(date = NULL, period = NULL, sub_date = NULL, 
                          filter_date = NULL, fct = NULL, grp = NULL) {
  
  if (!is.null(period)) {
    if (period == "no_period") {
      period <- NULL
    }
  }

  if (!is.null(period)) {
    
    if (is.null(date)) {
      
      validate("Please enter a date variable.")
    }
  }
  
  if (!is.null(filter_date) & is.null(sub_date)) {
    
    validate("Please enter a date variable.")
  }
  
  if (!is.null(fct)) {
    
    if (fct %in% c("week", "month", "year") & is.null(sub_date)) {
      
      validate("Please enter a date variable.")
    }
  }
  
  if (!is.null(grp)) {
    
    if (any(grp %in% c("week", "month", "year")) & is.null(sub_date)) {
     
      validate("Please enter a date variable.")
    }
  }
}

n_plot_output <- function(out, ...) {
    
    if ("ggplot" %in% class(out)) {
       
      tagList(renderPlot(out, ...))
      
    } else if ("gtable" %in% class(out)) {
      
      tagList(renderPlot(gridExtra::grid.arrange(out), ...))
        
      
    } else if (all(class(out) == "list")) {
      
      lapply(out, function(x) renderPlot(x, ...))
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



