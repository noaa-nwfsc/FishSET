# density_plot filter-facet system

# Density Plot
#'
#' Create density plot
#'
#' Creates a kernel density estimate, empirical cumulative distribution function,
#' or cumulative distribution function plot of selected variable.
#'
#' @param dat Primary data containing information on hauls or trips. Table in the FishSET
#' database contains the string 'MainDataTable'.
#' @param project String, name of project.
#' @param var String, name of variable to plot.
#' @param type String, type of density plot. Options include "kde" (kernel density estimate), 
#'   "ecdf" (empirical cdf), or "cdf" (cumilitive dstribution function).
#' @param group String, name of grouping variable(s). Multiple group variables will be combined. 
#' @param date Date variable from \code{dat} used to subset and/or facet the plot by.
#' @param filter_date The type of filter to apply to the table. The "date_range" option will subset 
#'   the data by two date values entered in \code{filter_val}. Other options include "year-day", 
#'   "year-week", "year-month", "year", "month", "week", or "day". The argument filter_value must 
#'   be provided.
#' @param date_value String containing a start and end date if using filter_date = "date_range", 
#'   e.g. c("2011-01-01", "2011-03-15"). If filter_date = "period" or "year-period", use integers 
#'   (4 digits if year, 1-2 if day, month, or week). Use a list if using a two-part filter, e.g. "year-week",
#'   with the format \code{list(year, period)} or a vector if using a single period, \code{c(period)}. 
#'   For example, \code{list(2011:2013, 5:7)} will filter the data table from weeks 5 through 7 for 
#'   years 2011-2013 if filter_date = "year-week".\code{c(2:5)} will filter the data
#'   February through May when filter_date = "month". 
#' @param filter_by String, variable name to filter by.
#' @param filter_value A vector of values to filter `MainDataTable` by using the variable 
#'   in \code{filter_by}. 
#' @param filter_expr String, a valid R expression to filter `MainDataTable` by using the variable 
#'   in \code{filter_by}. 
#' @param facet_by Variable name to facet by. This can be a variable that exists in
#'   the dataset, or a variable created by \code{density_plot()} such as "year", 
#'   "month", or "week".
#' @param tran String; name of function to transform variable, for example "log" or
#'   "sqrt".
#' @param scale Scale argument passed to \code{\link{facet_grid}}. Defaults to "fixed". 
#'   Other options include "free_y", "free_x", and "free".
#' @param bw Adjusts KDE bandwidth. Defaults to 1.
#' @param position The position of the grouped variable for KDE plot. Options include
#'   "identity", "stack", and "fill".
#' @return Returns a KDE, empirical CDF, or CDF of a selected variable.
#'   The data can be filtered by date using two arguments: \code{filter_date} and
#'   \code{filter_value}. \code{filter_date} specifies how the data should be filtered--
#'   by year, period (i.e. "month" or "week"), or year-period.
#'   \code{date_value} should contain the values (as integers) to filter
#'   the data by. Only one grouping variable will be displayed. If more than one is 
#'   entered they are automatically combined. Any variable in the dataset can be used for faceting,
#'   but "year", "month", and "week" are also available. The \code{group} and \code{tran} 
#'   arguments are not applied to the CDF plot.
#' @export density_plot
#' @examples
#' \dontrun{
#' density_plot(pollockMainDataTable, "pollock", var = "OFFICIAL_TOTAL_CATCH_MT", "kde", 
#'   date = "FISHING_START_DATE", filter_date = "year-month", filter_value = list(2011, 9:11),
#'   trans = "log", facet_date = TRUE, group = "GEAR_TYPE"
#' )
#' }
#' @import ggplot2
#' @importFrom stats pnorm reformulate
#' @importFrom rlang sym
#' @importFrom shiny isRunning

density_plot <- function(dat, project, var, type = "kde", group = NULL, date = NULL,
                         filter_date = NULL, date_value = NULL, filter_by = NULL, 
                         filter_value = NULL, filter_expr = NULL, facet_by = NULL,
                         tran = "identity", scale = "fixed", bw = 1, position = "identity") {
  out <- data_pull(dat)
  dataset <- out$dataset

  if (shiny::isRunning()) {
    if (deparse(substitute(dat)) == "values$dataset") dat <- get("dat_name")
  } else { 
    if (!is.character(dat)) dat <- deparse(substitute(dat)) }
  
  end <- FALSE

  if (!is.null(date)) dataset[[date]] <- date_parser(dataset[[date]])
  
  # convert date var if not date/date-time class
  if (!is.null(date)) {
    if (any(!(class(dataset[[date]]) %in% c("Date", "POSIXct", "POSIXt")))) {
      dataset[[date]] <- date_parser(dataset[[date]])
    }
  } 
  
  # facet setup ----
  if (!is.null(facet_by)) {
    facet_spec <- any(!(facet_by %in% names(dataset)))

    if (facet_spec == TRUE) {
      facet_s_id <- facet_by[!(facet_by %in% names(dataset))]

      if (all(facet_s_id %in% c("year", "month", "week")) == FALSE) {
        warning("Invalid facet variable.")
      } else {
        facet_by <- facet_by

        if (length(facet_by) == 0) {
          facet_by <- NULL
        }
      }
    }
  }

  group_date <- group[group %in% c("year", "month", "week")]
  facet_date <- facet_by[facet_by %in% c("year", "month", "week")]

  if (length(facet_date) > 0) {
    for (i in facet_date) {
      x <- switch(i, "year" = "%Y", "month" = "%b", "week" = "%U")

      dataset[[i]] <- format(dataset[[date]], x)

      if (i == "month") {
        dataset[[i]] <- factor(dataset[[i]], levels = month.abb, ordered = TRUE)
      } else {
        dataset[[i]] <- as.integer(dataset[[i]])
      }
    }
  }
  # group setup ----
  if (!is.null(group)) {
 
    if (length(group_date[!(group_date %in% facet_date)]) > 0) {
      
      for (i in group_date) {
        x <- switch(i, "year" = "%Y", "month" = "%b", "week" = "%U")

        dataset[[i]] <- format(dataset[[date]], x)

        if (i == "month") {
          dataset[[i]] <- factor(dataset[[i]], levels = month.abb, ordered = TRUE)
        }
      }
    }
    # combine group vars
    if (length(group) > 1) {
      dataset <- ID_var(dataset, vars = group, type = "string")
      group <- paste(group, collapse = "_")
    } else {
      group_nd <- group[!(group %in% group_date)]

      if (length(group_nd) > 0) {
        dataset[group_nd] <- lapply(group_nd, function(x) as.factor(dataset[[x]]))
      }
    }
  }

  # filter by variable ----
  if (!is.null(filter_by) | !is.null(filter_expr)) {
    
    dataset <- subset_var(dataset, filter_by, filter_value, filter_expr)
    
    if (nrow(dataset) == 0) {
      
      warning("Filtered data table has zero rows. Check filter parameters.")
      end <- TRUE
    }
  }
  # filter by date ----
  if (!is.null(filter_date)) {
    
    dataset <- subset_date(dataset, date, filter_date, date_value)
    
    if (nrow(dataset) == 0) {
          warning("Filtered data table has zero rows. Check filter parameters.")
          end <- TRUE
    }
  }

  if (end == FALSE) {

    # Plots ----
    var_sym <- rlang::sym(var)
    color_exp <- function() if (!is.null(group)) rlang::sym(group) else NULL
    
    x_lab_exp <- function() if (tran != "identity") paste0(var, " (", tran, ")") else var
    
    if (type == "kde") {
      
      plot <- 
        ggplot2::ggplot(dataset, ggplot2::aes(x = !!var_sym, fill = !!color_exp())) +
        ggplot2::stat_density(position = position, color = "black", 
                              alpha = .7, adjust = bw) +
        ggplot2::labs(x = x_lab_exp(),
                      caption = paste("kernel bandwidth:", bw)) +
        fishset_theme() +
        ggplot2::theme(legend.position = "bottom")
    
      } else if (type == "ecdf") {
        
      if (is.null(group)) {
        
        plot <- 
          ggplot2::ggplot(dataset, ggplot2::aes(x = !!var_sym)) +
          ggplot2::stat_ecdf(geom = "area", alpha = .7) +
          ggplot2::stat_ecdf(geom = "step", alpha = .7) +
          ggplot2::labs(x = x_lab_exp()) +
          fishset_theme() +
          ggplot2::theme(legend.position = "bottom")
      
      } else {
        
        plot <- 
          ggplot2::ggplot(dataset, ggplot2::aes(x = !!var_sym, color = !!color_exp())) +
          ggplot2::stat_ecdf(geom = "step") +
          ggplot2::labs(x = x_lab_exp()) +
          fishset_theme() +
          ggplot2::theme(legend.position = "bottom")
      }
        
    } else if (type == "cdf") {
      
      dataset$cdf <- 
        stats::pnorm(dataset[[var]], 
                     mean = mean(dataset[[var]], na.rm = TRUE), 
                     sd = sd(dataset[[var]], na.rm = TRUE))
      
      plot <- 
        ggplot2::ggplot(dataset, ggplot2::aes(!!var_sym)) +
        ggplot2::geom_area(ggplot2::aes(y = cdf), position = "identity", alpha = .7) +
        fishset_theme() +
        ggplot2::theme(legend.position = "bottom")
    }

    if (!is.null(facet_by)) {
      if (length(facet_by) == 1) {
        fm <- stats::reformulate(".", facet_by)
        plot <- plot + ggplot2::facet_wrap(fm, scales = scale)
      } else if (length(facet_by) == 2) {
        fm <- paste(facet_by, sep = " ~ ")
        plot <- plot + ggplot2::facet_grid(fm, scales = scale)
      }
    }

    plot <- plot +  ggplot2::scale_x_continuous(trans = tran)


    # add date to title
    if (!is.null(date) & !is.null(filter_date) & 
        !is.null(date_value) & length(facet_date) == 0) {
      plot <- date_title(plot, filter_date, date_value)
    }
    # Log the function

    density_plot_function <- list()
    density_plot_function$functionID <- "density_plot"
    density_plot_function$args <- list(
      dat, project, var, type, group, date, filter_date, date_value, filter_by, 
      filter_value, filter_expr, facet_by, tran, scale, bw, position)

    log_call(density_plot_function)

    # Save output
    save_plot(project, "density_plot")

    plot
  }
}
