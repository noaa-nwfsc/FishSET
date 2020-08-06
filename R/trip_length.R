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
#' @param filter_value String containing a start and end date if using filter_date = "date_range", 
#'   e.g. c("2011-01-01", "2011-03-15"). If filter_date = "period" or "year-period", use integers 
#'   (4 digits if year, 1-2 if day, month, or week). Use a list if using a two-part filter, e.g. "year-week",
#'   with the format \code{list(year, period)} or a vector if using a single period, \code{c(period)}. 
#'   For example, \code{list(2011:2013, 5:7)} will filter the data table from weeks 5 through 7 for 
#'   years 2011-2013 if filter_date = "year-week".\code{c(2:5)} will filter the data
#'   February through May when filter_date = "month".
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
#' @param output Options include 'table', 'plot', or 'tab_plot' (both table and plot,
#'   the default).
#' @param format_tab How table output should be formatted. Options include 'wide' 
#'   (the default) and 'long'.
#' @param haul_to_trip Logical, whether to convert \code{dat} from haul level data to
#'   trip level. See \code{\link{haul_to_trip}} for details.
#' @param ... Additional arguments passed to \code{\link{haul_to_trip}}. These should
#'   be the column(s) that uniquely identify trips.
#' @return \code{trip_length()} calculates vessel trip duration given a start and end date,
#'   converts trip length to the desired unit of time (e.g. weeks, days, or hours),
#'   and returns a table and/or plot. There is an option for calculating CPUE and
#'   hauls per unit of time as well. The data can be filtered using
#'   two arguments: \code{filter_date} and \code{filter_value}. \code{filter_date}
#'   specifies how the data should be filtered--by year, period (i.e. "month" or "week"), or year-period.
#'   \code{filter_value} should contain the values (as integers) to filter
#'   the data by. If multiple grouping variables are given then they are combined
#'   into one variable. no more than three is recommended. Any variable in the dataset 
#'   can be used for faceting, but "year", "month", and "week" are also available. 
#'   Distribution plots can be combined on a single page or printed individually with \code{pages}
#'   (the "multi" feature is only available in the console version of the function).
#' @export trip_length
#' @examples
#' \dontrun{
#' trip_length(pollockMainDataTable,
#'   start = "FISHING_START_DATE", end = "HAUL_DATE",
#'   units = "days", catch = "OFFICIAL_TOTAL_CATCH", hauls = "HAUL", output = "plot",
#'   haul_to_trip = T, fun.numeric = sum, fun.time = min, "VESSEL", "FISHING_START_DATE"
#' )
#' #'
#' }
#' @importFrom lubridate is.Date is.POSIXt
#' @importFrom stats reformulate
#' @importFrom gridExtra arrangeGrob grid.arrange marrangeGrob
#' @importFrom reshape2 melt
#' @import ggplot2

trip_length <- function(dat, project, start, end, units = "days", catch = NULL,
                        hauls = NULL, group = NULL, filter_date = NULL, filter_value = NULL,
                        facet_by = NULL, type = "hist", bins = 30, density = TRUE, 
                        scale = "fixed", tran = "identity", pages = "single", 
                        output = "tab_plot", format_tab = "wide", haul_to_trip = FALSE, ...) {
  out <- data_pull(dat)
  dat <- out$dat
  dataset <- out$dataset
  
  # cleaning ====
  dataset <- date_check(dataset, start)
  dataset <- date_check(dataset, end)
  
  if (any(class(dataset[[start]]) != class(dataset[[end]]))) {
    warning("Start and end dates have different classes. Suggest converting date variables to same class.")
  }
  
  if (anyNA(dataset[[start]]) | anyNA(dataset[[end]])) {
    warning("NAs detected in dates.")
  }
  
  if (haul_to_trip == TRUE) {
    dataset <- haul_to_trip(dataset, ...)
  }
  
  if (!is.null(filter_date)) {
    
    if (filter_date != "none") {
      
      if (filter_date == "date_range") {
        
        dataset <- dataset[dataset[[start]] >= filter_value[1] & dataset[[start]] <= filter_value[2], ]
        
      } else {
        
        pf <- switch(filter_date, "year-month" = c("%Y", "%m"), "year-week" = c("%Y", "%U"),
                     "year-day" = c("%Y", "%j"), "year" = "%Y", "month" = "%m", "week" = "%U",
                     "day" = "%j")
        
        if (grepl("-", filter_date)) {
          
          dataset <- dataset[(as.integer(format(dataset[[start]], pf[1])) %in% filter_value[[1]]) & 
                               (as.integer(format(dataset[[start]], pf[2])) %in% filter_value[[2]]), ]
          
        } else {
          
          dataset <- dataset[as.integer(format(dataset[[start]], pf)) %in% filter_value, ]
        }
        
        if (nrow(dataset) == 0) {
          
          warning("Filtered data table has zero rows. Check filter parameters.")
          end <- TRUE
        }
      }
    }
  }
  
  if (lubridate::is.POSIXt(dataset[[start]]) | lubridate::is.POSIXt(dataset[[end]])) {
    trip_length <- round(as.numeric(difftime(dataset[[end]], dataset[[start]]), units = units), 3)
  } else {
    trip_length <- as.numeric(dataset[[end]] - dataset[[start]], units = units)
  }
  
  trip <- data.frame(start = dataset[[start]], end = dataset[[end]], trip_length)
  t_nm <- "trip_length"
  
  if (any(trip_length[!is.na(trip_length)] < 0)) {
    warning(paste(sum(trip_length < 0), "negative values produced."))
    
    if (sum((trip_length < 0)) / length(trip_length) > 0.05) {
      warning("Negative values exceed 5% of observations.")
    }
  }
  
  if (!is.null(hauls)) {
    haul_ratio <- (dataset[[hauls]] / trip$trip_length)
    
    trip$haul_ratio <- round(haul_ratio, 3)
    h_nm <- "haul_ratio"
    
    if (any(is.infinite(haul_ratio))) {
      warning(paste(sum(is.infinite(haul_ratio)), "Inf values produced for haul_ratio."))
    }
    
    if (any(is.nan(haul_ratio))) {
      warning(paste(sum(is.nan(haul_ratio)), "NaN values produced for haul_ratio."))
    }
  } else {
    h_nm <- NULL
  }
  
  if (!is.null(catch)) {
    cpue <- round(dataset[catch] / trip$trip_length, 3)
    cpue_nm <- vapply(catch, FUN = function(x) paste(x, "cpue", sep = "_"), FUN.VALUE = "character")
    names(cpue) <- cpue_nm
    
    trip <- cbind(trip, cpue)
    
    if (any(apply(trip[cpue_nm], FUN = is.infinite, MARGIN = 2))) {
      warning("Inf values produced for cpue.")
    }
    
    if (any(apply(trip[cpue_nm], FUN = is.nan, MARGIN = 2))) {
      warning("NaN values produced for cpue.")
    }
  } else {
    cpue_nm <- NULL
  }
  
  facet_date_l <- FALSE
  facet_date <- facet_by[facet_by %in% c("year", "month", "week")]
  
  if (!is.null(facet_by)) {
    facet_spec <- any(!(facet_by %in% names(dataset)))
    facet_date_l <- any(facet_by %in% c("year", "month", "week"))
    
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
  trip[group[!(group %in% group_date)]]  <- dataset[group[!(group %in% group_date)]]
  trip[facet_by] <- dataset[facet_by]
  
  if (facet_date_l == TRUE) {
    for (i in facet_date) {
      x <- switch(i, "year" = "%Y", "month" = "%m", "week" = "%U")
      
      trip[[i]] <- format(trip$start, x)
      
      if (i == "month") {
        trip[[i]] <- factor(trip[[i]], levels = month.abb, ordered = TRUE)
      } else {
        trip[[i]] <- as.integer(trip[[i]])
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
          
          trip[[i]] <- format(trip$start, x)
          
          if (i == "month") {
            trip[[i]] <- factor(trip[[i]], levels = month.abb, ordered = TRUE)
          }
        }
      }
    }
    
    if (length(group) > 1) {
      trip[[paste(group, collapse = "_")]] <- apply(dataset[group], 1, paste, collapse = " ")
      group <- paste(group, collapse = "_")
    } else {
      group_nd <- group[!(group %in% group_date)]
      
      if (length(group_nd) > 0) {
        trip[[group_nd]] <- as.factor(dataset[[group_nd]])
      }
    }
  }
  # plots ====
  
  p_nm <- c(t_nm, cpue_nm, h_nm)
  
  plot_list <- lapply(p_nm, function(p) {
    
    if (density == FALSE) {
      h_plot <- ggplot2::ggplot(trip, ggplot2::aes_string(x = p))
    } else {
      h_plot <- ggplot2::ggplot(trip, ggplot2::aes(x = eval(parse(text = p)), 
                                                   y = ggplot2::after_stat(density)))
    }
    
    h_plot <- h_plot + ggplot2::labs(title = p, x = paste0(p, " (", units, ")")) +
      fishset_theme + ggplot2::scale_x_continuous(trans = tran)
    
    if (type == "hist") {
      h_plot <- h_plot + ggplot2::geom_histogram(ggplot2::aes_string(fill = group), bins = bins)
    } else if (type == "freq_poly") {
      h_plot <- h_plot + ggplot2::geom_freqpoly(ggplot2::aes_string(color = group), bins = bins)
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
      grp <- ggplot2::ggplot(trip, ggplot2::aes_string(0, 0, color = group)) +
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
    
  } else if (pages == "multi") {
    
    t_plot <- gridExtra::marrangeGrob(plot_list, nrow = 1, ncol = 1)
  }
  
  names(trip)[names(trip) == "trip_length"] <- paste0("trip_length_", units)
  
  f_plot <- function() if (pages == "multi") t_plot else gridExtra::grid.arrange(t_plot)
  
  if (format_tab == "long") {
    
    if (length(catch) > 1) {
      trip <- reshape2::melt(trip, measure.vars = cpue_nm, variable.name = "species",
                             value.name = "cpue")
      trip$species <- gsub("_cpue$", "", trip$species)
    }
  }
  # Log function
  trip_length_function <- list()
  trip_length_function$functionID <- "trip_length"
  trip_length_function$args <- list(dat, project, start, end, units, catch, hauls,
                                    group, filter_date, filter_value, facet_by, scale,
                                    tran, pages, output, haul_to_trip)
  trip_length_function$kwargs <- list(...)
  log_call(trip_length_function)
  
  # Save output
  save_table(trip, project, "trip_length")
  save_plot(project, "trip_length", t_plot)
  
  if (output == "table") {
    trip
  } else if (output == "plot") {
    f_plot()
  } else if (output == "tab_plot") {
    out_list <- list(table = trip, plot = f_plot())
    out_list
  }
}
