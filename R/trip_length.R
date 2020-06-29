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
#' @param group A variable to group by.
#' @param filter_date The type of filter to apply to table. Options include \code{"year-day"},
#'   \code{"year-week"}, \code{"year-month"}, \code{"year"}, \code{"month"}, \code{"week"},
#'   or \code{"day"}. The argument \code{filter_value} must be provided.
#' @param filter_value Integer (4 digits if year, 1-2 if day, month, or week). A vector or list
#'   of values to filter data table by. Use a list if using a two-part filter, e.g."year-week",
#'   with the format: \code{list(year, period)}. For example, \code{list(2011:2013, 5:7)}
#'   will filter the data table from weeks 5 through 7 for years 2011-2013.
#' @param facet_by Variable name to facet by. This can be a variable that exists in
#'   the dataset, or a variable created by \code{trip_length()} such as \code{"year"},
#'   \code{"month"}, or \code{"week"}.
#' @param combine Logical, whether to combine variables listed in \code{group}.
#' @param type The type of plot. Options include histogram (\code{"hist"}, the default) and
#'   frequency polygon (\code{"freq_poly"}).
#' @param scale Scale argument passed to \code{\link{facet_grid}}. Defaults to \code{"fixed"}.
#'   Other options include \code{"free_y"}, \code{"free_x"}, and \code{"free_xy"}.
#' @param trans Transformation to be applied to the x-axis. A few options include "log",
#'   "log10", and "sqrt". See \code{\link{scale_x_continuous}} for a complete list.
#' @param pages Whether to output plots on a single page (\code{"single"}, the default) or multiple
#'   pages (\code{"multi"}).
#' @param output Options include 'table', 'plot', or 'tab_plot' (both table and plot,
#'   the default).
#' @param haul_to_trip Logical, whether to convert \code{dat} from haul level data to
#'   trip level. See \code{\link{haul_to_trip}} for details.
#' @param ... Additional arguments passed to \code{\link{haul_to_trip}}. These should
#'   be the column(s) that uniquely identify trips.
#' @return \code{trip_length()} calculates vessel trip duration given a start and end date,
#'   converts trip length to the desired unit of time (e.g. weeks, days, or hours),
#'   and returns a table and/or plot. There is an option for calculating CPUE and
#'   hauls per unit of time as well. The data can be filter using
#'   two arguments: \code{filter_date} amd \code{filter_value}. \code{filter_date}
#'   specifies how the data should be filtered--by year, period (i.e. "month" or "week"), or year-period.
#'   \code{filter_value} should contain the values (as integers) to filter
#'   the data by. Only one groupig variable will be displayed; however, Any number of
#'   variables can be combined by using \code{combine = TRUE}, but no more than
#'   three is reccomended. Any variable in the dataset can be used for facetting,
#'   but "year", "month", and "week" are also available. Distriubtion plots can be
#'   combined on a single page or printed individually with \code{pages}.
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
#' @import ggplot2

trip_length <- function(dat, project, start, end, units = "days", catch = NULL,
                        hauls = NULL, group = NULL, filter_date = NULL, filter_value = NULL,
                        facet_by = NULL, combine = FALSE, type = "hist", scale = "fixed",
                        trans = NULL, pages = "single", output = "tab_plot",
                        haul_to_trip = FALSE, ...) {
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

  if (lubridate::is.POSIXt(dataset[[start]]) | lubridate::is.POSIXt(dataset[[end]])) {
    trip_length <- as.numeric(difftime(dataset[[end]], dataset[[start]]), units = units)
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

    trip$haul_ratio <- haul_ratio
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
    cpue <- dataset[catch] / trip$trip_length
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

  facet_date <- FALSE

  if (!is.null(facet_by)) {
    facet_spec <- any(!(facet_by %in% names(dataset)))
    facet_date <- any(facet_by %in% c("year", "month", "week"))

    if (facet_spec == TRUE) {
      facet_s_id <- facet_by[!(facet_by %in% names(dataset))]

      if (all(facet_s_id %in% c("year", "month", "week")) == FALSE) {
        warning("Invalid facet variable.")
      } else {
        facet <- facet_by
        facet_by <- facet_by[facet_by %in% names(dataset)]

        if (length(facet_by) == 0) {
          facet_by <- NULL
        }
      }
    } else {
      facet <- facet_by
    }
  } else {
    facet <- NULL
  }

  if (facet_date == TRUE) {
    for (i in facet) {
      x <- switch(i, "year" = "%Y", "month" = "%m", "week" = "%U")

      trip[[i]] <- format(trip[[start]], x)

      if (x == "%b") {
        trip$month <- factor(trip$month, levels = month.abb, ordered = TRUE)
      } else {
        trip[[i]] <- as.integer(trip[[i]])
      }
    }
  }

  if (!is.null(group)) {
    group_date <- NULL

    if (any(group %in% c("year", "month", "week"))) {
      group_date <- group[group %in% c("year", "month", "week")]

      if (length(group_date[!(group_date %in% facet)]) > 0) {
        for (i in group_date) {
          x <- switch(i, "year" = "%Y", "month" = "%m", "week" = "%U")

          trip[[i]] <- format(trip[[start]], x)

          if (x == "%b") {
            trip[[i]] <- factor(trip[[i]], levels = month.abb, ordered = TRUE)
          }
        }
      }
    }

    if (combine == TRUE) {
      trip[[paste(group, collapse = "_")]] <- apply(dataset[group], 1, paste, collapse = " ")
      group <- paste(group, collapse = "_")
    } else {
      group_nd <- group[!(group %in% group_date)]

      if (length(group_nd) > 0) {
        trip[[group_nd]] <- as.factor(dataset[[group_nd]])
      }
    }

    if (length(group) > 1) {
      warning("Too many grouping variables included, selecting first two.")
    }
  }

  if (!is.null(filter_date)) {
    p <- switch(filter_date, "year-month" = c("%Y", "%m"), "year-week" = c("%Y", "%U"),
      "year-day" = c("%Y", "%j"), "year" = "%Y", "month" = "%m", "week" = "%U",
      "day" = "%j"
    )

    if (grepl("-", filter_date)) {
      trip <- trip[(as.integer(format(trip[[start]], p[1])) %in% filter_value[[1]]) &
        (as.integer(format(trip[[start]], p[2])) %in% filter_value[[2]]), ]
    } else {
      trip <- trip[as.integer(format(trip[[start]], p)) %in% filter_value, ]
    }

    if (nrow(trip) == 0) {
      warning("Filtered data table has zero rows. Check filter parameters.")
      x <- 1
    }
  }

  # plots ====

  p_nm <- c(t_nm, cpue_nm, h_nm)

  plot_list <- lapply(p_nm, function(p) {
    h_plot <- ggplot2::ggplot(trip, ggplot2::aes_string(x = p)) +
      ggplot2::labs(title = p, x = paste0(p, " (", units, ")")) +
      fishset_theme +
      ggplot2::scale_x_continuous(trans = if (!is.null(trans)) trans else "identity")

    if (type == "hist") {
      h_plot <- h_plot + ggplot2::geom_histogram(ggplot2::aes_string(fill = group))
    } else if (type == "freq_poly") {
      h_plot <- h_plot + ggplot2::geom_freqpoly(ggplot2::aes_string(color = group))
    }

    if (pages == "single") {
      h_plot <- h_plot + ggplot2::theme(legend.position = "none")
    }

    if (!is.null(facet)) {
      if (length(facet) == 1) {
        fm <- stats::reformulate(".", facet)
      } else if (length(facet) == 2) {
        fm <- paste(facet, sep = " ~ ")
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

      plot_list[["legend"]] <- legend
    }

    t_plot <- do.call(gridExtra::arrangeGrob, c(plot_list, nrow = 2, ncol = 2))
  } else if (pages == "multi") {
    t_plot <- gridExtra::marrangeGrob(plot_list, nrow = 1, ncol = 1)
  }

  names(trip)[names(trip) == "trip_length"] <- paste0("trip_length_", units)

  f_plot <- function() if (pages == "multi") t_plot else gridExtra::grid.arrange(t_plot)

  # Log function
  trip_length_function <- list()
  trip_length_function$functionID <- "trip_length"
  trip_length_function$args <- list(
    dat, project, start, end, units, catch, hauls,
    group, filter_date, filter_value, facet_by, scale,
    trans, pages, output, haul_to_trip
  )
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
