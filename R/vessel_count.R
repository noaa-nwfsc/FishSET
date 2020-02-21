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
#' @param position Positioning of bar plot. Options include "stack", "dodge", 
#'   and "fill". 
#' @param output table or plot.
#' @param ... other arguments passed on to \code{\link{aggregate}}. 
#' @return Table or plot of the number of unique vessels within a time period.
#' @examples 
#' \dontrun{
#' vessel_count('pollockMainDataTable', "VESSEL_ID", "DATE_FISHING_BEGAN", period = "month", 
#'              group = "DISEMBARKED_PORT", position = "dodge", output = "plot")
#' }
#' @export vessel_count
#' @import ggplot2
#' @importFrom stats aggregate


vessel_count <- function(dat, project, v, t, period = "month", group = NULL, 
                         position = c("stack", "dodge", "fill"), output = c("table", "plot"), ...) {
  
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
    
    count <- date_factorize(count, t, p)
    
        plot <- ggplot2::ggplot(data = count, 
                                ggplot2::aes_string(x = t, y = v_freq, fill = if (!is.null(group)) group else NULL)) + 
                ggplot2::geom_col(position = position) +
                ggplot2::labs(x = paste(t, paste0("(", period, ")")),
                              y = "active vessels") + 
                fishset_theme + if (!is.null(group)) { ggplot2::scale_fill_discrete(name = group) }
                else { geom_blank() }
    
  } else {
    
    count[[t]] <- as.numeric(count[[t]])
        
        plot <- ggplot2::ggplot(data = count, 
                                ggplot2::aes_string(x = t, y = v_freq, fill = if (!is.null(group)) group else NULL)) + 
                ggplot2::geom_col(position = position) + 
                ggplot2::labs(x = paste(t, paste0("(", period, ")")), 
                              y = "active vessels") +
                ggplot2::scale_x_continuous(breaks = seq(from = min(count[[t]]), 
                                                         to = max(count[[t]]), 
                                                         by = round(nrow(count) * .2))) +
                fishset_theme + if (!is.null(group)) { ggplot2::scale_fill_discrete(name = group) }
                else { geom_blank() }
    
  }

  # Log the function 
  
    vessel_count_function <- list()
    vessel_count_function$functionID <- "vessel_count"
    vessel_count_function$args <- c(dat, project, v, t, period, group, position, output)
    vessel_count_function$kwargs <- ""
    log_call(vessel_count_function)
  
  # Output folder
    save_table(count, project, "vessel_count")
    
    save_plot(project, "vessel_count", plot)
    
    if (output == "table") {
      
      count
      
    } else {
      
      plot
    }
}
