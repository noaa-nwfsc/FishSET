#' Density Plot
#' 
#' Creates a density or cdf plot of selected variable
#' 
#' @param dat Main data frame over which to apply function. Table in FishSET 
#'   database should contain the string `MainDataTable`.
#' @param project name of project.
#' @param var Name of variable to plot.
#' @param type Type of density plot. Options include "kde" (kernel density estimate),
#'   "ecdf" (empirical cdf), or "cdf".
#' @param group A grouping variable. 
#' @param date Date variable to subset and/or facet by.
#' @param facet_date Logical; whether to facet the plot by date. If \code{filter_date = "year"} 
#'   or \code{"month"}, plot is passed to \code{facet_wrap()}. \code{filter_date = "year-month"} 
#'   is passed to \code{facet_grid()} with year faceted by row and month by column. 
#'   If \code{facet_date = FALSE}, the month(s) and/or year(s) used to filter the data
#'   are added to the plot's subtitle. 
#' @param filter_date Whether to filter data table by "year", "month", or 
#'   "year-month". \code{date} and \code{filter_value} must be provided. 
#' @param filter_value Integer (4 digits if year, 1-2 if month). The year, month,
#'   or year-month to filter data table by. Use a list if using "year-month"
#'   with the format: \code{list(year(s), month(s))}. For example, \code{list(2011:2013, 5:7)} 
#'   will filter the data table from May to July for years 2011-2013.
#' @param trans String; name of function to transform variable, for example "log" or 
#'   "sqrt".  
#' @param bw Adjusts KDE bandwidth. Defaults to 1. 
#' @param position The position of the grouped variable for KDE plot. Options include 
#'   "identity", "stack", and "fill". 
#' @return \code{density_plot} returns a KDE, emperical CDF, or CDF of a selected variable.
#'   the \code{group} and \code{trans} arguments are not applied to the CDF plot.
#' @export density_plot
#' @examples 
#' \dontrun{
#' density_plot("pollockMainDataTable", "pollock", var = "OFFICIAL_TOTAL_CATCH_MT", 
#' "kde", date = "FISHING_START_DATE", filter_date = "year-month",
#' filter_value = list(2011, 9:11), trans = "log", facet_date = TRUE, 
#' group = "GEAR_TYPE")
#' }
#' @import ggplot2
#' @importFrom stats pnorm
#' @importFrom lubridate is.Date
#' 

density_plot <- function(dat, project, var, type = c("kde", "ecdf", "cdf"), 
                         group = NULL, date = NULL, facet_date = FALSE, 
                         filter_date = NULL, filter_value = NULL,
                         trans = NULL, bw = 1,  position = "identity") {
  
  out <- data_pull(dat)
  dat <- out$dat
  dataset <- out$dataset
  
  x <- 0
  
  if (!is.null(date) & !is.null(filter_date) & !is.null(filter_value)) {
    
    if (lubridate::is.Date(dataset[[date]]) == FALSE) {
      
      warning("Date variable is in incorrect format. Converting to date format.")
      
      dataset[[date]] <- date_parser(dataset[[date]])
    }
    
    if (filter_date == "year-month") {
      
      dataset$Year <- as.integer(format(dataset[[date]], "%Y"))
      dataset$month <- as.integer(format(dataset[[date]], "%m"))
      
      dataset <- subset(dataset, (Year %in% filter_value[[1]]) & (month %in% filter_value[[2]]))
      
      dataset$month <- factor(format(dataset[[date]], "%b"), 
                              levels = month.abb, ordered = TRUE)
      
    } else if (filter_date == "year") {
      
      dataset$Year <- as.integer(format(dataset[[date]], "%Y"))
      
      dataset <- subset(dataset, Year %in% filter_value)
      
    } else if (filter_date == "month") {
      
      dataset$month <- as.integer(format(dataset[[date]], "%m"))
      
      dataset <- subset(dataset, month %in% filter_value)
      
      dataset$month <- factor(format(dataset[[date]], "%b"), 
                              levels = month.abb, ordered = TRUE)
      
    } else {
      
      warning("Invalid filter type. Available options are 'year-month', 'year', and 'month'.")
      x <- 1
    }
    
    if (nrow(dataset) == 0) {
      
      warning("Filtered data table has zero rows. Check filter parameters.")
      x <- 1
    }
  } 
  
  if (x == 0) {
  
    if (!is.null(group) & !is.factor(group)) {
    
      dataset[[group]] <- factor(dataset[[group]])
      
    }
  
    if (!is.null(trans)) {
    
      dataset[[var]] <- do.call(match.fun(trans), list(dataset[[var]]))
    }
  
    # Plots  
    if (type == "kde") {
    
      plot <- ggplot2::ggplot(dataset, 
                              ggplot2::aes_string(x = var, 
                                                  fill = if (!is.null(group)) group else NULL )) + 
        ggplot2::stat_density(position = position, color = "black", alpha = .7,
                              adjust = bw) +
        ggplot2::labs(title = paste("KDE of", var),
                      x = if (!is.null(trans)) paste0(var, " (", trans, ")") else var ,
                      caption = paste("kernel bandwidth:", bw)) + 
        fishset_theme + 
        ggplot2::theme(legend.position = "bottom") 
    
    } else if (type == "ecdf") {
    
      if (is.null(group)) {
      
        plot <- ggplot2::ggplot(dataset, ggplot2::aes_string(x = var)) + 
          ggplot2::stat_ecdf(geom = "area", alpha = .7) +
          ggplot2::stat_ecdf(geom = "step", alpha = .7) +
          ggplot2::labs(title = paste("ECDF of", var),
                        x = if (!is.null(trans)) paste0(var, " (", trans, ")") else var ) +  
          fishset_theme +
          ggplot2::theme(legend.position = "bottom")
       
      } else {
      
        plot <- ggplot2::ggplot(dataset, ggplot2::aes_string(x = var, color = group)) +
          ggplot2::stat_ecdf(geom = "step") +
          ggplot2::labs(title = paste("ECDF of", var),
                        x = if (!is.null(trans)) paste0(var, " (", trans, ")") else var ) + 
          fishset_theme +
          ggplot2::theme(legend.position = "bottom")
      
      } 
    
    } else {
    
      dataset$cdf <- stats::pnorm(dataset[[var]], 
                                  mean = mean(dataset[[var]], na.rm = TRUE),
                                  sd = sd(dataset[[var]], na.rm = TRUE))
    
      plot <- ggplot2::ggplot(dataset, ggplot2::aes_string(var)) +
        ggplot2::geom_area(ggplot2::aes(y = cdf), position = "identity", alpha = .7) +
        ggplot2::labs(title = paste("CDF of", var)) + 
        fishset_theme + 
        ggplot2::theme(legend.position = "bottom")
    
    }
    # add date facets 
    if (facet_date == TRUE) {
    
      if (filter_date == "year-month") {
      
        plot <- plot + ggplot2::facet_grid(Year ~ month)
      
      } else if (filter_date == "year") {
      
        plot <- plot + ggplot2::facet_wrap(~Year)
      
      } else {
      
        plot <- plot + ggplot2::facet_wrap(~month)
      }
    }
  
    # add date to title
    if (!is.null(date) & !is.null(filter_date) & !is.null(filter_value) & facet_date == FALSE) {
    
      plot <- date_title(plot, filter_date, filter_value)
    }
    #Log the function 
  
    density_plot_function <- list()
    density_plot_function$functionID <- "density_plot"
    density_plot_function$args <- c(dat, project, var, type, group, date, facet_date, 
                                    filter_date, filter_value, trans, bw, position)
    log_call(density_plot_function)
  
    # Save output
    save_plot(project, "density_plot")
  
    plot
  }
}

