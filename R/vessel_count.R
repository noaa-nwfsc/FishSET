#' Vessel count
#' 
#' Generates a table or plot of the number of unique vessels by time period
#' 
#' @param dat Main data frame over which to apply function. Table in fishset_db 
#'   database should contain the string `MainDataTable`.
#' @param project name of project.
#' @param v vessel ID variable to count.
#' @param t time variable containing dates to aggregate by.
#' @param period Time period to count by. Options include "year", "year_abv", 
#'   "month", "month_abv", "month_num", "weeks" (weeks in the year), 
#'   "weekday", "weekday_abv", "weekday_num", "day" (day of the month), 
#'   and "day_of_year".
#' @param group factor variable to group count by.
#' @param position_grp Positioning of bar plot. Options include "stack", "dodge", 
#'   and "fill". 
#' @param output table or plot.
#' @param ... other arguments passed on to \code{\link{aggregate}}. 
#' @return Table or plot of the number of unique vessels within a time period.
#' @example 
#' \dontrun{
#' 
#' vessel_count('pollockMainDataTable', "VESSEL_ID", "DATE_FISHING_BEGAN", period = "month", 
#'              group = "DISEMBARKED_PORT", position_grp = "dodge", output = "plot")
#' }
#' @export vessel_count
#' @import ggplot2
#' @importFrom stats aggregate




vessel_count <- function(dat, project, v, t, period = "month", group = NULL, position_grp = "stack", output = c("table", "plot"), ...) {
  
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
    
    mytheme <- ggplot2::theme(panel.grid.major = ggplot2::element_blank(), 
                              panel.grid.minor = ggplot2::element_blank(), 
                              panel.background = ggplot2::element_blank(), 
                              axis.line = ggplot2::element_line(colour = "black"), 
                              axis.text = ggplot2::element_text(size = 11), 
                              axis.title = ggplot2::element_text(size = 11))
    
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
                           levels = c("Sunday", "Monday", "Tuesday", "Wednesday", 
                                      "Thursday", "Friday", "Saturday"), 
                           ordered = T)
    }
    
    if (output == "table") {
      
      count
      
    } else {
      
      if (is.null(group)) {
        
        ggplot2::ggplot(data = count, ggplot2::aes(x = count[[t]], 
                                                   y = count[[v_freq]])) + 
          ggplot2::geom_col() + 
          ggplot2::labs(x = paste(t, paste0("(", period, ")")), 
                        y = v_freq) + 
          mytheme  
        
      } else {
        
        ggplot2::ggplot(data = count, ggplot2::aes(x = count[[t]], 
                                                   y = count[[v_freq]],
                                                   fill = count[[group]])) + 
          ggplot2::geom_col(position = position_grp) +
          ggplot2::labs(x = paste(t, paste0("(", period, ")")),
                        y = v_freq) + 
          ggplot2::scale_fill_discrete(name = group) +
          mytheme
      }
    } 
    
  } else {
    
    count[[t]] <- as.numeric(count[[t]])
    
    if (output == "table") {
      
      count
      
    } else {
      
      if (is.null(group)) {
        
        ggplot2::ggplot(data = count, ggplot2::aes(x = count[[t]], 
                                                   y = count[[v_freq]])) + 
          ggplot2::geom_col() + 
          ggplot2::labs(x = paste(t, paste0("(", period, ")")), 
                        y = v_freq) +
          ggplot2::scale_x_continuous(breaks = seq(from = min(count[[t]]), 
                                                   to = max(count[[t]]), 
                                                   by = round(nrow(count) * .2))) +
          mytheme
        
      } else {
        
        ggplot2::ggplot(data = count, ggplot2::aes(x = count[[t]], 
                                                   y = count[[v_freq]], 
                                                   fill = count[[group]])) + 
          ggplot2::geom_col(position = position_grp) + 
          ggplot2::labs(x = paste(t, paste0("(", period, ")")), 
                        y = v_freq) +
          ggplot2::scale_x_continuous(breaks = seq(from = min(count[[t]]), 
                                                   to = max(count[[t]]), 
                                                   by = round(nrow(count) * .2))) +
          ggplot2::scale_fill_discrete(name = group) +
          mytheme
      }
    }
  }

  #Log the function 
  
    vessel_count_function <- list()
    vessel_count_function$functionID <- "vessel_count"
    vessel_count_function$args <- c(dat, project, v, t, period, group, position_grp, output)
    vessel_count_function$kwargs <- ""
    log_call(vessel_count_function)
  
  # Output folder
    write.csv(count, paste0(locoutput(), project,'_vessel_count', Sys.Date(), '.csv'))
    
    ggplot2::ggsave(paste0(locoutput(), project,'_vessel_count', Sys.Date(), '.png'))
}
