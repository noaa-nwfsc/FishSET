freq_table <- function(dataset, var, group = NULL, bins = 30, type = "dens",
                       v_id = NULL, format_lab = "decimal", format_tab = "wide") {
  
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
  #' @param v_id String, name of vessel ID column (used to detect confidential
  #'   information). 
  #' @param format_lab Formatting option for bin labels. Options include 
  #'   \code{"decimal"} or \code{"scientific"}.
  #' @param format_tab Format table "wide" or "long"
  #' @export
  #' @keywords internal
  #' @importFrom graphics hist
  #' @importFrom dplyr lead anti_join bind_rows
  #' @importFrom tidyr pivot_wider
  #' @importFrom stats setNames
  
  if (!is.numeric(dataset[[var]])) stop("'var must be numeric.", call. = FALSE) 
   
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
    
    # If v_id is not NULL, count unique vessels (confid check)
    if (!is.null(v_id)) {
      
      table_out <- agg_helper(dataset, value = v_id, group = c("breaks", group), 
                              fun = dplyr::n_distinct, count = FALSE)
    } else {
      # count unique breaks
      table_out <- agg_helper(dataset, value = "breaks", fun = NULL,
                               group = c("breaks", group), count = TRUE)
    }
    
    
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
    # fill missing levels
    brk_list <- list(levels(dataset$breaks))
    names(brk_list) <- var
    missing <- lapply(table_out[group], unique)
    missing <- c(brk_list, missing)
    missing <- do.call(tidyr::expand_grid, missing)
    table_out <- dplyr::full_join(missing, table_out)
    
    # replace NA with zero
    r_list <- list(0)
    names(r_list) <- type_nm
    table_out <- tidyr::replace_na(table_out, replace = r_list)
    
    if (format_tab == "wide") {
      
      table_out <- tidyr::pivot_wider(table_out, id_cols = !!var,
                                      names_from = !!group,
                                      values_from = !!type_nm, 
                                      names_repair = "unique")
    }
  }
  
  table_out
}

nfreq_table <- function(dataset, var, group = NULL, bins = 30, type = "dens", 
                        v_id = NULL, format_lab = "decimal", format_tab = "wide") {
  
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
  #' @param v_id String, name of vessel ID column (used to detect confidential
  #'   information). 
  #' @param format_lab Formatting option for bin labels. Options include 
  #'   \code{"decimal"} or \code{"scientific"}.
  #' @param format_tab Format table "wide" or "long"
  #' @export
  
  if (length(var) > 1) {
    
    freq_out <- lapply(var, function(x) {
      
      freq_table(dataset, var = x, group = group,bins = bins, type = type,
                 v_id = v_id, format_lab = format_lab, format_tab = format_tab)
    })
    
    names(freq_out) <- var
    
  } else {
    
    freq_out <- 
      freq_table(dataset, var = var, group = group,bins = bins, type = type,
                 v_id = v_id, format_lab = format_lab, format_tab = format_tab)
  }
  
  freq_out
}
