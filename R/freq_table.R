freq_table <- function(dataset, var, group = NULL, bins = 30, type = "dens",
                       format_lab = "decimal", format_tab = "wide") {
  
  #' Create a binned frequency table
  #' 
  #' Create a binned frequency, relative frequency, or density table.
  #' 
  #' @param dataset Primary data containing information on hauls or trips. Table in FishSET
  #'   database should contain the string `MainDataTable`.
  #' @param var String, name of numeric variable to bin.
  #' @param group String, name of variable(s) to group \code{var} by. 
  #' @param bins Integer, the number of bins to create. 
  #' @param type String, the type of binned frequency table to create. \code{"freq"}
  #'   creates a frequency table, \code{"perc"} creates a relative frequency table,
  #'   and \code{"dens"} creates a density table. 
  #' @param format_lab Formatting option for bin labels. Options include 
  #'   \code{"decimal"} or \code{"scientific"}.
  #' @param format_tab Format table "wide" or "long"
  #' @export
  #' @keywords internal
  #' @importFrom graphics hist
  #' @importFrom dplyr lead anti_join bind_rows
  #' @importFrom tidyr pivot_wider
  #' @importFrom stats setNames
  
  end <- FALSE
  
  if (!is.numeric(dataset[[var]])) {
    
    warning("'var must be numeric.") 
    end <- TRUE
  }
  
  if (end == FALSE) {
    
    type_nm <- switch(type, "perc" = "percent", "dens" = "density", 
                      "freq" = "frequency")
    
    f_lab <- switch(format_lab, "decimal" = "d", "scientific" = "g")
    
    h_freq <- graphics::hist(dataset[[var]], breaks = bins,
                             include.lowest = TRUE, plot = FALSE)
    
    brks <- formatC(h_freq$breaks, format = f_lab)
    
    h_labs <- paste(brks, "-", dplyr::lead(brks))
    h_labs <- h_labs[-length(h_labs)]
    
    if (is.null(group)) {
      
      if (type == "freq") {
        
        table_out <- data.frame(h_labs, h_freq$counts)
        
      } else if (type == "perc") {
        
        table_out <- data.frame(h_labs, ((h_freq$counts/sum(h_freq$counts)) * 100))
        
      } else if (type == "dens") {
        
        table_out <- data.frame(h_labs, h_freq$density)
      }
      
      table_out <- stats::setNames(table_out, c(var, type_nm))
      # if group present
    } else {
      
      dataset$breaks <- cut(dataset[[var]], h_freq$breaks,
                            include.lowest = TRUE, labels = h_labs)
      
      table_out <- agg_helper(dataset, value = "breaks",
                              group = c("breaks", group), fun = length)
      # table_out <- stats::setNames(table_out, c("breaks", group, type_nm))
      table_out <- stats::setNames(table_out, c(var, group, type_nm))
      
      if (type == "perc") {
        
        table_out$percent <- (table_out$percent/sum(table_out$percent)) * 100
      }
      
      if (type == "dens") {
        
        n <- length(dataset[[var]][is.finite(dataset[[var]])])
        h <- as.double(diff(h_freq$breaks))[1] # binwidth
        dens <- table_out$density / (n * h) # frequency/(n*h)
        table_out$density <- dens
      }
      
      missing <- lapply(table_out[group], unique)
      missing[[var]] <- levels(dataset$breaks)
      missing <- do.call(expand.grid, missing)
      missing <- dplyr::anti_join(missing, table_out[c(var, group)])
      
      if (nrow(missing) > 0) {
        
        missing[[type_nm]] <- 0
        table_out <- dplyr::bind_rows(table_out, missing)
        table_out <- table_out[order(table_out[[var]]), ]
        
        if (format_tab == "wide") {
          
          table_out <- tidyr::pivot_wider(table_out, id_cols = !!var,
                                          names_from = !!group,
                                          values_from = !!type_nm, 
                                          names_repair = "unique")
        }
      }
    }
    
    table_out
  }
}

nfreq_table <- function(dataset, var, group = NULL, bins = 30, type = "dens", 
                        format_lab = "decimal", format_tab = "wide") {
  
  #' Create one or more binned frequency tables
  #' 
  #' Create one or more binned frequency, relative frequency, or density table.
  #' 
  #' @param dataset Primary data containing information on hauls or trips. Table in FishSET
  #'   database should contain the string `MainDataTable`.
  #' @param var String, name of numeric variable to bin.
  #' @param group String, name of variable(s) to group \code{var} by. 
  #' @param bins Integer, the number of bins to create. 
  #' @param type String, the type of binned frequency table to create. \code{"freq"}
  #'   creates a frequency table, \code{"perc"} creates a relative frequency table,
  #'   and \code{"dens"} creates a density table. 
  #' @param format_lab Formatting option for bin labels. Options include 
  #'   \code{"decimal"} or \code{"scientific"}.
  #' @param format_tab Format table "wide" or "long"
  #' @export
  
  if (length(var) > 1) {
    
    freq_out <- lapply(var, function(x) {
      
      freq_table(dataset, x, group, bins, type, format_lab, format_tab)
    })
    
    names(freq_out) <- var
    
  } else {
    
    freq_out <- freq_table(dataset, var, group, bins, type, format_lab, format_tab)
  }
  
  freq_out
}