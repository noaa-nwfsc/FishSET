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
#' @param group Up to two variables to group by. For plots, 
#'   the first variable is passed to "fill" and second is passed to "facet_grid".
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
  
  if (!is.null(group)) {
    
    group1 <- group[1]
    
    if (length(group) == 2) {
      
      group2 <- group[2] 
      
    } else if (length(group) > 2) {
      
      warning("Too many grouping variables included, selecting first two.")
      
    }
    
  }
  
  v_freq <- c("vessel_freq")
  
  if (is.null(group)) {
    
    count <- stats::aggregate(dataset[[v]], 
                              by = list(format(date_parser(dataset[[t]]), p)), 
                              FUN = sum)
    
    colnames(count) <- c("date", v_freq)
    
  } else if (!is.null(group)) {
    
    if (length(group) == 1) {
      
      count <- stats::aggregate(dataset[[v]], 
                                by = list(format(date_parser(dataset[[t]]), p), 
                                          dataset[[group1]]), 
                                FUN = sum) 
      
      colnames(count) <- c("date", "group_1", v_freq)
      
    } else {
      
      count <- stats::aggregate(dataset[[v]], 
                                by = list(format(date_parser(dataset[[t]]), p), 
                                          dataset[[group1]],
                                          dataset[[group2]]), 
                                FUN = sum) 
      
      colnames(count) <- c("date", "group_1", "group_2", v_freq)  
      
    }
    
  }
  
  if (p %in% c("%a", "%A", "%b", "%B")) { 
    
    count <- date_factorize(count, "date", p)
    
  } else {
    
    count$date <- as.integer(count$date)
    
  }
  
  ind <- periods_list[[p]][which(!(periods_list[[p]] %in% unique(count$date)))]
  
  if (is.null(group)) {
    
    missing_periods <- data.frame(date = ind)
    
  } else if (!is.null(group) & length(group) == 1) {
    
    missing_periods <- expand.grid(date = ind, 
                                   group_1 = unique(count$group_1))
    
    
  } else if (!is.null(group) & length(group) > 1) {
    
    missing_periods <- expand.grid(date = ind, 
                                   group_1 = unique(count$group_1), 
                                   group_2 = unique(count$group_2))
    
  }
  
  missing_periods$vessel_freq <- 0
  
  count <- rbind(count, missing_periods)
  
  count <- count[order(count$date), ]
  
  
  plot <- ggplot2::ggplot(data = count, 
                          ggplot2::aes(x = date, y = vessel_freq, fill = if (!is.null(group)) group_1 else NULL)) + 
    ggplot2::geom_col(position = if (position == "dodge") position_dodge2(preserve = "single") else position) +
    ggplot2::labs(x = paste(t, paste0("(", period, ")")),
                  y = "active vessels") + 
    fishset_theme 
  
  if (!is.null(group)) {
    
    plot <- plot + ggplot2::guides(fill = ggplot2::guide_legend(title = group[1], 
                                                                title.theme = ggplot2::element_text(
                                                                  size = 10),
                                                                label.theme = ggplot2::element_text(
                                                                  size = 8))) +
      ggplot2::theme(legend.position = "bottom")
    
  }
  
  if (!is.null(group) & length(group) == 2) {
    
    plot <- plot + ggplot2::facet_grid(group_2 ~ ., scales = "free_y")
    
  } 
  
  if (p %in% c("%a", "%A", "%b", "%B")) {
    
    plot <- plot + ggplot2::scale_x_discrete(breaks = levels(count$date))
    
  } else {
    
    plot <- plot + ggplot2::scale_x_continuous(breaks = if (length(unique(count$date)) <= 12){seq(1,length(unique(count$date)), by = 1)
    } else {seq(1, length(unique(count$date)), by = round(length(unique(count$date)) * 0.08))})
    
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
    
    if (is.null(group)) {
      
      count
      
    } else if (!is.null(group) & length(group) == 1) {
      
      names(count)[names(count) == "group_1"] <- group1
      
      count
      
    } else {
      
      names(count)[names(count) == "group_1"] <- group1
      
      names(count)[names(count) == "group_2"] <- group2
      
      count
      
    }
    
  } else {
    
    plot
  }
}
