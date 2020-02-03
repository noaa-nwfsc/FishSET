#' Vessel count
#' 
#' Generates a table or plot of the number of unique vessels by time period
#' 
#' @param dat Main data frame over which to apply function. Table in fishset_db database should contain the string `MainDataTable`.
#' @param v vessel ID variable to count.
#' @param t time variable containing dates to aggregate by.
#' @param period Time period to count by. Options include "year", "year_abv", "month", "month_abv", "month_num", "weeks", 
#' "weekday", "weekday_abv", "weekday_num", "day", and "day_of_year".
#' @param output table or plot
#' @return Table or plot of the number of unique vessels within a time period
#' @example 
#' 
#' @export vessel_count
#' @import ggplot2
#' @importFrom stats aggregate




vessel_count <- function(dat, v, t, period = "month", group = NULL, output = c("table", "plot"), ...) {
  
  #Call in datasets
  out <- data_pull(dat)
  dat <- out$dat
  dataset <- out$dataset
  
  
  periods <- c("year", "year_abv", "month", "month_abv", "month_num", "weeks", 
               "weekday", "weekday_abv", "weekday_num", "day", "day_of_year")
  
  if (period %in% periods == FALSE) {
    
    stop("Invalid period. Please select a valid period name (see documentation for details).")
    
  } else {
    
    p <- switch(period, "year" = "%Y", "year_abv" = "%y", "month" = "%B",
                "month_abv" = "%b", "month_num" = "%m", "weeks" = "%U", 
                "weekday" = "%A", "weekday_abv" = "%a", "weekday_num" = "%w", 
                "day" = "%d", "day_of_year" = "%j")
  }
  
  if (is.null(group)) {
    
    count <- stats::aggregate(dataset[[v]], 
                              by = list(format(date_parser(dataset[[t]]), p)), 
                              FUN = sum,
                              ...)
    
    v_freq <- c("vessel_freq")
    
    colnames(count) <- c(t, v_freq)
    
  } else {
    
    count <- stats::aggregate(dataset[[v]], 
                              by = list(format(date_parser(dataset[[t]]), p), dataset[[group]]), 
                              FUN = sum,
                              ...) 
    v_freq <- c("vessel_freq")
    
    colnames(count) <- c(t, group, v_freq)
  }
  
  if (p %in% c("%a", "%A", "%b", "%B")) { 
    
    if (p == "%b") {
      
      count[[t]] <- factor(count[[t]], 
                           levels = month.abb, 
                           ordered = T)
      
    } else if (p == "%B") {
      
      count[[t]] <- factor(count[[t]], 
                           levels = month.name, 
                           ordered = T)
      
    } else if (p == "%a") {
      
      count[[t]] <- factor(count[[t]], 
                           levels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"), 
                           ordered = T)
      
    } else {
      
      count[[t]] <- factor(count[[t]], 
                           levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"), 
                           ordered = T)
    }
    
    if (output == "table") {
      
      count
      
    } else {
      
      if (is.null(group)) {
        
        ggplot2::ggplot(data = count, aes(x = count[[t]], y = count[[v_freq]])) + 
                 geom_col() +
                 labs(x = paste(t, paste0("(", period, ")")),
                      y = v_freq)  
        
      } else {
        
        ggplot2::ggplot(data = count, aes(x = count[[t]], y = count[[v_freq]], fill = count[[group]])) + 
                 geom_col() +
                 labs(x = paste(t, paste0("(", period, ")")),
                      y = v_freq) + 
                 scale_fill_discrete(name = group)
      }
    } 
    
  } else {
    
    count[[t]] <- as.numeric(count[[t]])
    
    if (output == "table") {
      
      count
      
    } else {
      
      if (is.null(group)) {
        
        ggplot2::ggplot(data = count, aes(x = count[[t]], y = count[[v_freq]])) + 
                 geom_col() + 
                 labs(x = paste(t, paste0("(", period, ")")), 
                      y = v_freq) +
                 scale_x_continuous(breaks = seq(from = min(count[[t]]), 
                                                 to = max(count[[t]]), 
                                                 by = round(nrow(count) * .2)))
        
      } else {
        
        ggplot2::ggplot(data = count, aes(x = count[[t]], y = count[[v_freq]], fill = count[[group]])) + 
                 geom_col() + 
                 labs(x = paste(t, paste0("(", period, ")")), 
                      y = v_freq) +
                 scale_x_continuous(breaks = seq(from = min(count[[t]]), 
                                                 to = max(count[[t]]), 
                                                 by = round(nrow(count) * .2))) +
                 scale_fill_discrete(name = group)
      }
    }
  }

  #Log the function 
  if (output == "table") {
    vessel_count_table_function <- list()
    vessel_count_table_function$functionID <- "vessel_count"
    vessel_count_table_function$args <- c(dat, v, t, period = "month", group = NULL, output = c("table", "plot"))
    vessel_count_table_function$kwargs <- ""
    log_call(vessel_count_table_function)
  } else {
    vessel_count_plot_function <- list()
    vessel_count_plot_function$functionID <- "vessel_count"
    vessel_count_plot_function$args <- c(dat, v, t, period = "month", group = NULL, output = c("table", "plot"))
    vessel_count_plot_function$kwargs <- ""
    log_call(vessel_count_plot_function)
  }
  
}
