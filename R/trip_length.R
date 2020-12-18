# Trip length
#'
#'  Calculate trip duration, hauls per trip, and CPUE
#'
#' @param dat Primary data containing information on hauls or trips. Table in FishSET
#'   database should contain the string `MainDataTable`.
#' @param project String, name of project.
#' @param start Date variable containing the start of vessel trip.
#' @param end Date variable containing the end of vessel trip.
#' @param units Time unit, defaults to \code{"days"}. Options include \code{"secs"},
#'   \code{"mins"}, \code{"hours"}, \code{"days"}, or \code{"weeks"}.
#' @param catch Optional, catch variable in \code{dat} for calculating CPUE.
#' @param hauls Optional, hauls variable in \code{dat} for calculating hauls per trip.
#' @param group Grouping variables. If more than one variable is given they are automatically
#'   combined. 
#' @param filter_date The type of filter to apply to table. The "date_range" option will subset 
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
#'   the dataset, or a variable created by \code{trip_length()} such as \code{"year"},
#'   \code{"month"}, or \code{"week"}.
#' @param type The type of plot. Options include histogram (\code{"hist"}, the default) and
#'   frequency polygon (\code{"freq_poly"}).
#' @param bins The number of bins used in histogram/freqency polygon. 
#' @param density Logical, whether densities or frequencies are used for histogram. Defaults 
#'   to \code{TRUE}. 
#' @param scale Scale argument passed to \code{\link{facet_grid}}. Defaults to \code{"fixed"}.
#'   Other options include \code{"free_y"}, \code{"free_x"}, and \code{"free_xy"}.
#' @param tran Transformation to be applied to the x-axis. A few options include \code{"log"},
#'   \code{"log10"}, and \code{"sqrt"}. See \code{\link[ggplot2]{scale_continuous}} for a complete list.
#' @param pages Whether to output plots on a single page (\code{"single"}, the default) or multiple
#'   pages (\code{"multi"}).
#' @param remove_neg Logical, whether to remove negative trip durations from the plot and table. 
#' @param output Options include 'table', 'plot', or 'tab_plot' (both table and plot,
#'   the default).
#' @param haul_to_trip Logical, whether to convert \code{dat} from haul level data to
#'   trip level. See \code{\link{haul_to_trip}} for details.
#' @param ... Additional arguments passed to \code{\link{haul_to_trip}}. These should
#'   be the column(s) that uniquely identify trips.
#' @return \code{trip_length()} calculates vessel trip duration given a start and end date,
#'   converts trip length to the desired unit of time (e.g. weeks, days, or hours),
#'   and returns a table and/or plot. There is an option for calculating CPUE and
#'   hauls per unit of time as well. The data can be filtered using
#'   two arguments: \code{filter_date} and \code{date_value}. \code{filter_date}
#'   specifies how the data should be filtered--by year, period (i.e. "month" or "week"), or year-period.
#'   \code{date_value} should contain the values (as integers) to filter
#'   the data by. If multiple grouping variables are given then they are combined
#'   into one variable. no more than three is recommended. Any variable in the dataset 
#'   can be used for faceting, but "year", "month", and "week" are also available. 
#'   Distribution plots can be combined on a single page or printed individually with \code{pages}.
#' @export trip_length
#' @examples
#' \dontrun{
#' trip_length(pollockMainDataTable,
#'   start = "FISHING_START_DATE", end = "HAUL_DATE",
#'   units = "days", catch = "OFFICIAL_TOTAL_CATCH", hauls = "HAUL", output = "plot",
#'   haul_to_trip = TRUE, fun.numeric = sum, fun.time = min, "VESSEL", "FISHING_START_DATE"
#' )
#' #'
#' }
#' @importFrom lubridate is.Date is.POSIXt
#' @importFrom stats reformulate
#' @importFrom gridExtra arrangeGrob grid.arrange marrangeGrob
#' @importFrom rlang sym 
#' @importFrom dplyr anti_join
#' @importFrom tidyr pivot_wider
#' @importFrom shiny isRunning
#' @import ggplot2

trip_length <- function(dat, project, start, end, units = "days", catch = NULL,
                        hauls = NULL, group = NULL, filter_date = NULL, date_value = NULL,
                        filter_by = NULL, filter_value = NULL, filter_expr = NULL,
                        facet_by = NULL, type = "hist", bins = 30, density = TRUE, 
                        scale = "fixed", tran = "identity", pages = "single", remove_neg = FALSE,
                        output = "tab_plot", haul_to_trip = FALSE, ...) {
  out <- data_pull(dat)
  dataset <- out$dataset
  
  if (shiny::isRunning()) {
    if (deparse(substitute(dat)) == "values$dataset") dat <- get("dat_name")
  } else { 
    if (!is.character(dat)) dat <- deparse(substitute(dat)) }
  
  # cleaning dates ----
  dataset <- date_check(dataset, start)
  dataset <- date_check(dataset, end)
  
  if (any(class(dataset[[start]]) != class(dataset[[end]]))) {
    warning("Start and end date variables have different classes. Recommend converting date variables to same class.")
  }
  
  if (anyNA(dataset[[start]]) | anyNA(dataset[[end]])) {
    warning("NAs detected in dates.")
  }
  
  # convert hauls to trips
  if (haul_to_trip == TRUE) {
    dataset <- haul_to_trip(dataset, project = project, ...)
  }
  
  # filter by variable/expression 
  if (!is.null(filter_value) | !is.null(filter_expr)) {
    
    dataset <- subset_var(dataset, filter_by, filter_value, filter_expr)
    
    if (nrow(dataset) == 0) {
      
      warning("Filtered data table has zero rows. Check filter parameters.")
      end <- TRUE
    }
  }
  
  # filter by date
  if (!is.null(filter_date)) {
    
    dataset <- subset_date(dataset, start, filter_date, date_value)
    
    if (nrow(dataset) == 0) {
      
      warning("Filtered data table has zero rows. Check filter parameters.")
      end <- TRUE
    }
  }
  # calculate trip duration ----
  if (lubridate::is.POSIXt(dataset[[start]]) | lubridate::is.POSIXt(dataset[[end]])) {
    trip <- round(as.numeric(difftime(dataset[[end]], dataset[[start]]), units = units), 3)
  } else {
    trip <- as.numeric(dataset[[end]] - dataset[[start]], units = units)
  }
  
  trip_tab <- data.frame(start = dataset[[start]], end = dataset[[end]])
  t_nm <- paste0("trip_length_", units)
  trip_tab[[t_nm]] <- trip
  
  
  if (any(trip_tab[[t_nm]] < 0)) {
    warning(paste(sum(trip_tab[[t_nm]] < 0), "negative values produced."))
    
    if (sum((trip_tab[[t_nm]] < 0)) / length(trip_tab[[t_nm]]) > 0.05) {
      warning("Negative values exceed 5% of observations.")
    }
  }
  # calculate hauls per unit (haul ratio) ----
  if (!is.null(hauls)) {
    
    trip_tab$haul_ratio <- round((dataset[[hauls]] / trip_tab[[t_nm]]), 3)
    h_nm <- "haul_ratio"
    
    if (any(is.infinite(trip_tab$haul_ratio))) {
      warning(paste(sum(is.infinite(trip_tab$haul_ratio)), "Inf values produced for haul_ratio."))
    }
    
    if (any(is.nan(trip_tab$haul_ratio))) {
      warning(paste(sum(is.nan(trip_tab$haul_ratio)), "NaN values produced for haul_ratio."))
    }
  } else {
    h_nm <- NULL
  }
  # calculate cpue ----
  if (!is.null(catch)) {
    
    cpue_nm <- vapply(catch, FUN = function(x) paste(x, "cpue", sep = "_"), FUN.VALUE = "character")
    trip_tab[cpue_nm] <- lapply(catch, function(x) round((dataset[[x]] / trip_tab[[t_nm]]), 3))
    # names(cpue) <- cpue_nm
    
    
    if (any(vapply(trip[cpue_nm], FUN = is.infinite, FUN.VALUE = logical(1)))) {
      warning("Inf values produced for cpue.")
    }
    
    if (any(vapply(trip[cpue_nm], FUN = is.nan, FUN.VALUE = logical(1)))) {
      warning("NaN values produced for cpue.")
    }
  } else {
    cpue_nm <- NULL
  }
  
  # facet setup ----
  facet_date <- facet_by[facet_by %in% c("year", "month", "week")]
  
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
  # group setup ----
  group_date <- group[group %in% c("year", "month", "week")]
  trip_tab[group[!(group %in% group_date)]]  <- dataset[group[!(group %in% group_date)]]
  trip_tab[facet_by] <- dataset[facet_by]
  
  if (length(facet_date) > 0) {
    for (i in facet_date) {
      x <- switch(i, "year" = "%Y", "month" = "%m", "week" = "%U")
      
      trip_tab[[i]] <- format(trip_tab$start, x)
      
      if (i == "month") {
        trip_tab[[i]] <- factor(trip_tab[[i]], levels = month.abb, ordered = TRUE)
      } else {
        trip_tab[[i]] <- as.integer(trip_tab[[i]])
      }
    }
  }
  
  if (!is.null(group)) {
    group_date <- NULL
    
    if (any(group %in% c("year", "month", "week"))) {
      group_date <- group[group %in% c("year", "month", "week")]
      
      if (length(group_date[!(group_date %in% facet_date)]) > 0) {
        for (i in group_date) {
          x <- switch(i, "year" = "%Y", "month" = "%b", "week" = "%U")
          
          trip_tab[[i]] <- format(trip_tab$start, x)
          
          if (i == "month") {
            trip_tab[[i]] <- factor(trip_tab[[i]], levels = month.abb, ordered = TRUE)
          }
        }
      }
    }
    
    if (length(group) > 1) {
      trip_tab <- ID_var(trip_tab, vars = group, drop = TRUE)
      group <- paste(group, collapse = "_")
    } else {
      group_nd <- group[!(group %in% group_date)]
      
      if (length(group_nd) > 0) {
        trip_tab[[group_nd]] <- as.factor(dataset[[group_nd]])
      }
    }
  }
  # remove negative trip durations
  if (remove_neg) {
    
    trip_tab <- trip_tab[trip_tab[[t_nm]] >= 0, ]
  }
  
  # columns names for trip length, cpue, and hauls
  p_nm <- unname(c(t_nm, cpue_nm, h_nm))
  
  freq_dens <- function() if (density) "density" else "frequency"
  
  # table output ----
  if (output %in% c("tab_plot", "table")) {
    
    # if only displaying trip durations
    if (is.null(catch) & is.null(hauls)) {
      
      trip_freq <- hist(trip_tab[[t_nm]], breaks = bins, include.lowest = TRUE, plot = FALSE)
      
      h_labs <- paste(trip_freq$breaks, "-", dplyr::lead(trip_freq$breaks))
      h_labs <- h_labs[-length(h_labs)]
      
      if (is.null(group) & is.null(facet_by)) {
        
        if (density == FALSE) {
          table_out <- data.frame(h_labs, trip_freq$counts)
          table_out <- stats::setNames(table_out, c(t_nm, "frequency"))
          
        } else {
          table_out <- data.frame(h_labs, trip_freq$density)
          table_out <- stats::setNames(table_out, c(t_nm, "density"))
        }
        
      } else {
        
        trip_tab$breaks <- cut(trip_tab[[t_nm]], trip_freq$breaks, 
                               include.lowest = TRUE, labels = h_labs)
        
        table_out <- agg_helper(trip_tab, value = "breaks", 
                                group = c("breaks", group, facet_by), fun = length)
        table_out <- stats::setNames(table_out, c("breaks", group, facet_by, freq_dens()))
        
        if (density == TRUE) {
          
          n <- length(trip_tab[[t_nm]][is.finite(trip_tab[[t_nm]])])
          h <- as.double(diff(trip_freq$breaks))[1]
          dens <- table_out$density / (n * h)
          table_out$density <- dens
        }
        
        missing <- lapply(table_out[c(group, facet_by)], unique)
        missing$breaks <- levels(trip_tab$breaks)
        missing <- do.call(expand.grid, missing)
        missing <- dplyr::anti_join(missing, table_out[c("breaks", group, facet_by)])
        
        if (nrow(missing) > 0) {
          
          missing[[freq_dens()]] <- 0
          table_out <- rbind(table_out, missing)
          table_out <- table_out[order(table_out$breaks), ]
          table_out <- tidyr::pivot_wider(table_out, id_cols = breaks, 
                                          names_from = c(!!group, !!facet_by), 
                                          values_from = !!freq_dens())
          table_out <- as.data.frame(table_out)
        }
      }
      
      # if CPUE and/or haul ratio entered
    } else {
      
      table_out <- lapply(p_nm, function(x) {
        
        trip_freq <- hist(trip_tab[[x]], breaks = bins, include.lowest = TRUE, plot = FALSE)
        
        h_labs <- paste(trip_freq$breaks, "-", dplyr::lead(trip_freq$breaks))
        h_labs <- h_labs[-length(h_labs)]
        
        if (is.null(group) & is.null(facet_by)) {
          
          if (density == FALSE) {
            sum_tab <- data.frame(h_labs, trip_freq$counts)
            sum_tab <- stats::setNames(sum_tab, c(x, "frequency"))
            
          } else {
            sum_tab <- data.frame(h_labs, trip_freq$density)
            sum_tab <- stats::setNames(sum_tab, c(x, "density"))
          }
          
        } else {
          
          trip_tab$breaks <- cut(trip_tab[[x]], trip_freq$breaks, 
                                 include.lowest = TRUE, labels = h_labs)
          
          sum_tab <- agg_helper(trip_tab, value = "breaks", 
                                group = c("breaks", group, facet_by), fun = length)
          sum_tab <- stats::setNames(sum_tab, c("breaks", group, facet_by, freq_dens()))
          
          if (density == TRUE) {
            
            n <- length(trip_tab[[x]][is.finite(trip_tab[[x]])])
            h <- as.double(diff(trip_freq$breaks))[1]
            dens <- sum_tab$density / (n * h) # frequency/(n*h) 
            sum_tab$density <- dens
          }
          
          missing <- lapply(sum_tab[c(group, facet_by)], unique)
          missing$breaks <- levels(trip_tab$breaks)
          missing <- do.call(expand.grid, missing)
          missing <- dplyr::anti_join(missing, sum_tab[c("breaks", group, facet_by)])
          
          if (nrow(missing) > 0) {
            
            missing[[freq_dens()]] <- 0
            sum_tab <- rbind(sum_tab, missing)
            sum_tab <- sum_tab[order(sum_tab$breaks), ]
            sum_tab <- tidyr::pivot_wider(sum_tab, id_cols = breaks, 
                                          names_from = c(!!group, !!facet_by),
                                          values_from = !!freq_dens(),
                                          values_fill = 0)
            sum_tab <- as.data.frame(sum_tab)
          }
        }
        
        sum_tab
      })
      
    }
    # save output table
    if (is.null(catch) & is.null(hauls)) {
      save_table(table_out, project, "trip_length")  
    } else {
      lapply(seq_along(table_out), function(x) {
        save_table(table_out[[x]], project, paste0("trip_length_", x))
      })
    }
  }
  
  # plot section ----
  if (output %in% c("tab_plot", "plot")) {
    
    # plot functions ----
    
    group_exp <- function() if (!is.null(group)) rlang::sym(group) else NULL
    
    # single plot (just durations)
    if (is.null(catch) & is.null(hauls)) { 
      
      if (density == FALSE) {
        t_plot <- ggplot2::ggplot(trip_tab, ggplot2::aes(x = !!rlang::sym(t_nm)))
      } else {
        t_plot <- ggplot2::ggplot(trip_tab, ggplot2::aes(x = !!rlang::sym(t_nm), 
                                                         y = ggplot2::after_stat(density)))
      }
      
      t_plot <- t_plot + ggplot2::labs(x = paste0("trip length (", units, ")")) +
        fishset_theme() + ggplot2::scale_x_continuous(trans = tran)
      
      if (type == "hist") {
        t_plot <- t_plot + ggplot2::geom_histogram(ggplot2::aes(fill = !!group_exp()), bins = bins)
      } else if (type == "freq_poly") {
        t_plot <- t_plot + ggplot2::geom_freqpoly(ggplot2::aes(color = !!group_exp()), 
                                                  bins = bins, size = 1)
      }
      
      if (!is.null(facet_by)) {
        if (length(facet_by) == 1) {
          fm <- stats::reformulate(".", facet_by)
        } else if (length(facet_by) == 2) {
          fm <- paste(facet_by, sep = " ~ ")
        }
        
        t_plot <- t_plot + ggplot2::facet_grid(fm, scales = scale)
      }
      # if CPUE and haul ratio entered (multiple plots)
    } else {
      
      plot_list <- lapply(p_nm, function(p) {
        
        if (density == FALSE) {
          h_plot <- ggplot2::ggplot(trip_tab, ggplot2::aes(x = !!rlang::sym(p)))
        } else {
          h_plot <- ggplot2::ggplot(trip_tab, ggplot2::aes(x = !!rlang::sym(p), 
                                                           y = ggplot2::after_stat(density)))
        }
        
        h_plot <- h_plot + ggplot2::labs(title = p, x = paste0(p, " (", units, ")")) +
          fishset_theme() + ggplot2::scale_x_continuous(trans = tran)
        
        if (type == "hist") {
          h_plot <- h_plot + ggplot2::geom_histogram(ggplot2::aes(fill = !!group_exp()), bins = bins)
        } else if (type == "freq_poly") {
          h_plot <- h_plot + ggplot2::geom_freqpoly(ggplot2::aes(color = !!group_exp()), 
                                                    bins = bins, size = 1)
        }
        
        if (pages == "single") {
          h_plot <- h_plot + ggplot2::theme(legend.position = "none")
        }
        
        if (!is.null(facet_by)) {
          if (length(facet_by) == 1) {
            fm <- stats::reformulate(".", facet_by)
          } else if (length(facet_by) == 2) {
            fm <- paste(facet_by, sep = " ~ ")
          }
          
          h_plot <- h_plot + ggplot2::facet_grid(fm, scales = scale)
        }
        
        h_plot
      })
      
      if (pages == "single") {
        if (!is.null(group)) {
          # add grouping var
          grp <- ggplot2::ggplot(trip_tab, ggplot2::aes_string(0, 0, color = group)) +
            ggplot2::geom_point() +
            ggplot2::theme(legend.position = "bottom")
          
          # extract legend
          tmp <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(grp))
          leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
          legend <- tmp$grobs[[leg]]
        }
        
        if (length(catch) + length(hauls) < 3) {
          
          if (!is.null(group)) {
            plot_list[["legend"]] <- legend
          }
          
          t_plot <- do.call(gridExtra::arrangeGrob, c(plot_list, nrow = 2, ncol = 2))
          
        } else if (length(catch) + length(hauls) >= 3) {
          
          t_plot <- do.call(gridExtra::arrangeGrob, c(plot_list))
          
          if (!is.null(group)) {
            t_plot <- gridExtra::arrangeGrob(t_plot, legend, ncol = 1, heights = c(.9, .1))
          }
        }
        
      } else if (pages == "multi" & !shiny::isRunning()) {
        
        t_plot <- gridExtra::marrangeGrob(plot_list, nrow = 1, ncol = 1)
      }
      
    }
    
    f_plot <- function() {
      if (is.null(catch) & is.null(hauls)) {
        t_plot
      } else {
        if (pages == "multi") {
          if (shiny::isRunning()) {
            plot_list
          } else {
            t_plot
          }
        } else {
          if (shiny::isRunning()) {
            t_plot
          } else {
            gridExtra::grid.arrange(t_plot)
          }
        }
      }
    }
    
    # save plot(s) 
    if (pages == "multi" & (!is.null(catch) | !is.null(hauls))) {
      
      lapply(seq_along(plot_list), function(x) save_plot(project, paste0("trip_length_", x), plot_list[[x]]))
    } else {
      save_plot(project, "trip_length", t_plot)
    }
  }
  
  # Log function
  trip_length_function <- list()
  trip_length_function$functionID <- "trip_length"
  trip_length_function$args <- list(dat, project, start, end, units, catch, hauls,
                                    group, filter_date, date_value, filter_by, 
                                    filter_value, filter_expr, facet_by, type,
                                    bins, density, scale, tran, pages, remove_neg,
                                    output, haul_to_trip)
  trip_length_function$kwargs <- list(...)
  log_call(trip_length_function)
  
  if (output == "table") {
    table_out
  } else if (output == "plot") {
    f_plot()
  } else if (output == "tab_plot") {
    out_list <- list(table = table_out, plot = f_plot())
    out_list
  }
}
