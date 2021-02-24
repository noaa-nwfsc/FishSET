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
#'   "ecdf" (empirical cdf), "cdf" (cumulative dstribution function), or "all" (containing all plot types).
#'   Two or more plot types can be chosen.
#' @param group Optional, string names of variables to group by. By default, grouping variables 
#'   are combined unless \code{combine = FALSE} and \code{type} is "cdf" and/or "ecdf".
#'   If \code{type} is "kde" or "all" grouping variables are automatically combined. 
#'   "cdf" and "ecdf" plots can use up to two grouping variables if \code{combine = FALSE}:
#'   the first variable is assigned to the "color" aesthetic and second to the "linetype" aesthetic. 
#' @param combine Logical, whether to combine the variables listed in \code{group} for plot. 
#' @param date Date variable from \code{dat} used to subset and/or facet the plot by.
#' @param filter_date The type of filter to apply to `MainDataTable`. To filter by a 
#'   range of dates, use \code{filter_date = "date_range"}. To filter by a given period, use
#'   "year-day", "year-week", "year-month", "year", "month", "week", or "day". 
#'   The argument \code{date_value} must be provided. 
#' @param date_value This argument is paired with \code{filter_date}. If \code{filter_date = "date_range"}, 
#'   enter a string containing a start- and end-date, e.g. \code{date_value = c("2011-01-01", "2011-03-15")}. 
#'   If filtering by period (e.g. "year", "year-month"), use integers 
#'   (4 digits if year, 1-2 digits if referencing a day, month, or week). Use a list if using a year-period type filter, e.g. "year-week",
#'   with the format: \code{list(year, period)}. Use a vector if using a single period (e.g. "month"): \code{c(period)}. 
#'   For example, \code{date_value = list(2011:2013, 5:7)} will filter the data table from May through July for 
#'   years 2011-2013 if \code{filter_date = "year-month"}.\code{date_value = c(2:5)} will filter the data
#'   from February through May when \code{filter_date = "month"}.
#' @param filter_by String, variable name to filter `MainDataTable` by. the argument \code{filter_value} must be provided.
#' @param filter_value A vector of values to filter `MainDataTable` by using the variable 
#'   in \code{filter_by}. For example, if \code{filter_by = "GEAR_TYPE"}, \code{filter_value = 1} 
#'   will include only observations with a gear type of 1. 
#' @param filter_expr String, a valid R expression to filter `MainDataTable` by. 
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
#' @param pages Whether to output plots on a single page (\code{"single"}, the default) 
#'    or multiple pages (\code{"multi"}). 
#' @return Returns a KDE, empirical CDF, or CDF of a selected variable.
#'   The data can be filtered by date or by variable (see \code{filter_date} and
#'   \code{filter_by}). If \code{type} contains "kde" or "all" then grouping variables 
#'   are automatically combined. Any variable in the dataset can be used for faceting,
#'   but "year", "month", and "week" are also available.
#' @export density_plot
#' @examples
#' \dontrun{
#' density_plot(pollockMainDataTable, "pollock", var = "OFFICIAL_TOTAL_CATCH_MT", "kde", 
#'   date = "FISHING_START_DATE", filter_date = "year-month", filter_value = list(2011, 9:11),
#'   trans = "log", facet_date = TRUE, group = "GEAR_TYPE"
#' )
#' }
#' @import ggplot2
#' @importFrom dplyr across group_by mutate
#' @importFrom gridExtra arrangeGrob marrangeGrob grid.arrange
#' @importFrom stats pnorm reformulate
#' @importFrom rlang sym
#' @importFrom scales breaks_extended
#' @importFrom shiny isRunning

density_plot <- function(dat, project, var, type = "kde", group = NULL, combine = TRUE, date = NULL,
                         filter_date = NULL, date_value = NULL, filter_by = NULL, 
                         filter_value = NULL, filter_expr = NULL, facet_by = NULL,
                         tran = "identity", scale = "fixed", bw = 1, position = "identity",
                         pages = "single") {
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
    
    dataset[date] <- 
      lapply(dataset[date], date_parser)
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
  
  # convert non-date facet vars to factors 
  facet_nd <- facet_by[!(facet_by %in% facet_date)]
  if (length(facet_nd) > 0) {
    
    dataset[facet_nd] <- lapply(dataset[facet_nd], as.factor)
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
      
      if (any(type %in% c("ecdf", "cdf")) & combine == FALSE) {
        
        group1 <- group[1] 
        group2 <- group[2]
        
        dataset[group] <- lapply(dataset[group], as.factor)
      } else {
       
        dataset <- ID_var(dataset, vars = group, type = "string")
        group <- paste(group, collapse = "_")
      } 
      
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
    
    color_exp <- function() {
      
      if (is.null(group)) {
        NULL
      } else if (length(group) == 1) {
        rlang::sym(group)
      } else if (length(group) > 1) {
        rlang::sym(group1)
      }
    }
   
    linetype_exp <- function() {
      if (length(group) > 1) {
        rlang::sym(group2)
      } else {
        NULL
      }
    }
    
    x_lab_exp <- function() if (tran != "identity") paste0(var, " (", tran, ")") else var
    
    if ("all" %in% type) {
      
      plot_type <- c("kde", "ecdf", "cdf")
    
    } else {
      
      plot_type <- type
    }
    
    plot_list <-
      lapply(plot_type, function(x) {
      
      if (x == "kde") {
        
        plot <- 
          ggplot2::ggplot(dataset, ggplot2::aes(x = !!var_sym, fill = !!color_exp())) +
          ggplot2::stat_density(position = position, color = "black", 
                                alpha = .7, adjust = bw) +
          ggplot2::labs(title = "Kernel Density",
                        x = x_lab_exp(),
                        caption = paste("kernel bandwidth:", bw)) +
          fishset_theme() +
          ggplot2::theme(legend.position = "bottom")
        
      } else if (x == "ecdf") {
        
        if (is.null(group)) {
          
          plot <- 
            ggplot2::ggplot(dataset, ggplot2::aes(x = !!var_sym)) +
            ggplot2::stat_ecdf(geom = "area", alpha = .7) +
            ggplot2::stat_ecdf(geom = "step", alpha = .7) +
            ggplot2::labs(title = "Empirical Cumulative Distribution Function",
                          x = x_lab_exp(), y = "probability") +
            fishset_theme() +
            ggplot2::theme(legend.position = "bottom")
          
        } else {
          
          plot <- 
            ggplot2::ggplot(dataset, ggplot2::aes(x = !!var_sym)) +
            ggplot2::stat_ecdf(aes(color = !!color_exp(), linetype = !!linetype_exp()), 
                               geom = "step") +
            ggplot2::labs(title = "Empirical Cumulative Distribution Function",
                          x = x_lab_exp(), y = "probability") +
            fishset_theme() +
            ggplot2::theme(legend.position = "bottom")
        }
        
      } else if (x == "cdf") {
        
        if (!is.null(group)) {
          
          dataset <- dataset %>% 
            dplyr::group_by(dplyr::across(group)) %>% 
            dplyr::mutate(cdf = pnorm(!!var_sym, 
                                      mean(!!var_sym, na.rm = TRUE), 
                                      sd(!!var_sym, na.rm = TRUE)))
          
          plot <- 
            ggplot2::ggplot(dataset, ggplot2::aes(!!var_sym)) +
            ggplot2::geom_line(ggplot2::aes(y = cdf, color = !!color_exp(), 
                                            linetype = !!linetype_exp()))
        } else {
          
          dataset$cdf <- 
            stats::pnorm(dataset[[var]], 
                         mean = mean(dataset[[var]], na.rm = TRUE), 
                         sd = sd(dataset[[var]], na.rm = TRUE))
          
          plot <- 
            ggplot2::ggplot(dataset, ggplot2::aes(!!var_sym)) +
            ggplot2::geom_area(ggplot2::aes(y = cdf), position = "identity", alpha = .7)
           
        }
        
        plot <-  plot +
          ggplot2::labs(title = "Cumulative Distribution Function",
                        y = "probability") +
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
      
      x_breaks <- function() {
        if (tran != "identity") {
          scales::breaks_extended(n = 7) 
        } else {
          ggplot2::waiver()
        }
      }
        
      plot <- plot + ggplot2::scale_x_continuous(trans = tran, breaks = x_breaks())
      
      
      # add date to title
      if (!is.null(date) & !is.null(filter_date) & 
          !is.null(date_value) & length(facet_date) == 0) {
        plot <- date_title(plot, filter_date, date_value)
      }
      
      # remove legend if single paged
      if (pages == "single") {
        
        if (length(type) > 1 | "all" %in% type) {
          
          plot <- plot + ggplot2::theme(legend.position = "none")
        }
      }
      
      plot
    })

    # arranging plot
    if (pages == "single") {
      if (length(type) > 1 | "all" %in% type) {
        if (!is.null(group)) {
          # add grouping var
          cdf_linetype <- function() {
            if (all(type %in% c("cdf", "ecdf"))) {
              linetype_exp()
            } else {
              NULL
            }
          }
          grp <- ggplot2::ggplot(dataset, ggplot2::aes(0, 0, color = !!color_exp(),
                                                       linetype = !!cdf_linetype())) +
            ggplot2::geom_point() + ggplot2::geom_line() +
            ggplot2::theme(legend.position = "bottom", legend.box = "vertical",
                           legend.direction = "vertical")
          
          # extract legend
          tmp <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(grp))
          leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
          legend <- tmp$grobs[[leg]]
          
          plot_list[["legend"]] <- legend
        }
        
        d_plot <- do.call(gridExtra::arrangeGrob, c(plot_list, nrow = 2, ncol = 2))
        
      } else {
        
        d_plot <- plot_list[[1]]
      }
    } else if (pages == "multi" & !shiny::isRunning()) {
      
      d_plot <- gridExtra::marrangeGrob(plot_list, nrow = 1, ncol = 1)
    }
    
    if (length(type) == 1 & !("all" %in% type)) {
      
      d_plot <- plot_list[[1]]
    }
    
    print_plot <- function() {
      if (length(type) == 1 & !("all" %in% type)) {
        
        d_plot
      
      } else if (pages == "single") {
        
        if (length(type) > 1 | "all" %in% type) {
          
          gridExtra::grid.arrange(d_plot)
        }
        
      } else if (pages == "multi") {
        if (shiny::isRunning()) {
          plot_list
        } else {
          d_plot
        }
      }
    }
    
    # Log the function

    density_plot_function <- list()
    density_plot_function$functionID <- "density_plot"
    density_plot_function$args <- list(
      dat, project, var, type, group, combine, date, filter_date, date_value, filter_by, 
      filter_value, filter_expr, facet_by, tran, scale, bw, position, pages)

    log_call(density_plot_function)

    # Save output
    if (pages == "multi") {
      lapply(seq_along(plot_list), function(x) {
        save_plot(project, paste0("density_plot_", x), plot_list[[x]])
        })
    } else {
      save_plot(project, "density_plot", d_plot)
    }
    
    print_plot()
  }
}
