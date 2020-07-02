#' Apply rolling function to catch
#'
#' @param dat Main data frame over which to apply function. Table in FishSET database
#'   should contain the string `MainDataTable`.
#' @param project Name of project.
#' @param catch Variable name or names containing catch data. Multiple variables can
#'   entered as a vector.
#' @param date Date variable to aggregate by.
#' @param group Variable name or names to group by. Plot will display up to two grouping
#'   variables.
#' @param k The width of the window.
#' @param fun The function to be applied to window. Defaults to \code{mean}.
#' @param filter_date Whether to filter data table by \code{"year"}, \code{"month"}, or
#'   \code{"year-month"}. \code{filter_value} must be provided.
#' @param filter_value Integer (4 digits if year, 1-2 if month). The year, month,
#'   or year-month to filter data table by. Use a list if using "year-month"
#'   with the format: \code{list(year(s), month(s))}. For example, \code{list(2011:2013, 5:7)}
#'   will filter the data table from May to July for years 2011-2013.
#' @param facet_by Variable name to facet by. This can be a variable that exists in
#'   the dataset, or a variable created by \code{roll_catch()} such as \code{"year"},
#'   \code{"month"}, or \code{"species"} if more than one variable is entered in \code{catch}.
#' @param scale Scale argument passed to \code{\link{facet_grid}}.
#'   Options include \code{"free"}, \code{"free_x"}, \code{"free_y"}. Defaults to
#'   \code{"fixed"}.
#' @param align Indicates whether results of window should be left-aligned (\code{"left"}),
#'   right-aligned (\code{"right"}), or centered (\code{"center"}). Defaults to
#'   \code{"center"}.
#' @param convr Convert catch variable to \code{"tons"}, \code{"metric_tons"}, or
#'   by using a function. Defaults to \code{FALSE}.
#' @param output Whether to display \code{"plot"}, \code{"table"}, or both. Defaults
#'   to both (\code{"tab_plot"}).
#' @param ... Additional arguments passed to \code{\link{rollapply}}
#' @export roll_catch
#' @import ggplot2
#' @importFrom stats aggregate reformulate
#' @importFrom zoo zoo merge.zoo rollapply fortify.zoo
#' @importFrom tidyr pivot_longer pivot_wider
#' @examples
#' \dontrun{
#' roll_catch(pollockMainDataTable, project = "pollock", catch = "LBS_270_POLLOCK_LBS",
#'   date = "FISHING_START_DATE", group = "GEAR_TYPE", k = 15
#' )
#'
#' roll_catch(pollockMainDataTable, project = "pollock", catch = c("LBS_270_POLLOCK_LBS", 
#'  "LBS_110_PACIFIC_COD_LBS"), date = "FISHING_START_DATE", group = "GEAR_TYPE", k = 5, 
#'  filter_date = "month", filter_value = 4:6, facet_by = "month", convr = "tons"
#' )
#' }
#'
roll_catch <- function(dat, project, catch, date, group = NULL, k = 10,
                       fun = mean, filter_date = NULL, filter_value = NULL,
                       facet_by = NULL, scale = "fixed", align = "center",
                       convr = FALSE, output = "tab_plot", ...) {
  out <- data_pull(dat)
  dat <- out$dat
  dataset <- out$dataset

  if (!is.null(facet_by)) {
    if (!(facet_by %in% names(dataset))) {
      if (!(facet_by %in% c("year", "month", "species"))) {
        warning("Invalid facet variable.")
      } else {
        facet <- facet_by

        facet_by <- NULL
      }
    } else {
      facet <- facet_by
    }
  } else {
    facet <- NULL
  }

  full_dates <- seq.Date(
    from = min(dataset[[date]], na.rm = TRUE),
    to = max(dataset[[date]], na.rm = TRUE),
    by = "day"
  )

  full_dates <- zoo::zoo(0, full_dates)

  if (!is.null(group)) {
    group1 <- group[1]

    dataset[[group1]] <- as.factor(dataset[[group1]])

    if (length(group) == 2) {
      group2 <- group[2]

      dataset[[group2]] <- as.factor(dataset[[group2]])
    }
  }

  nm <- unique(c(date, group, facet_by))

  l <- lapply(nm, function(x) dataset[[x]])

  names(l) <- nm

  agg <- stats::aggregate(dataset[catch], by = l, FUN = sum)

  if (convr != FALSE) {
    if (convr == "tons") {
      agg[catch] <- agg[catch] / 2000
    } else if (convr == "metric_tons") {
      agg[catch] <- agg[catch] / 2204.62
    } else {
      agg[catch] <- do.call(convr, list(agg[catch]))
    }
  }

  if (!is.null(group) | !is.null(facet_by)) {
    nm2 <- unique(c(group, facet_by))

    agg[nm2] <- apply(agg[nm2], 2, trimws)

    if (length(catch) == 1) {
      agg <- tidyr::pivot_wider(agg, names_from = nm2, values_from = catch, names_sep = "__")
      agg[is.na(agg)] <- 0
    } else {
      agg <- tidyr::pivot_longer(agg, cols = catch, values_to = "catch", names_to = "species")

      nm4 <- c(nm2, "species")
      agg <- tidyr::pivot_wider(agg,
        names_from = nm4, names_sep = "__",
        values_from = "catch", values_fill = list(catch = 0)
      )
    }
  }

  z <- zoo::zoo(agg[names(agg)[!(names(agg) %in% date)]], agg[[date]])

  z <- zoo::merge.zoo(z, full_dates, all = TRUE, fill = 0)

  z$full_dates <- NULL

  rz <- zoo::rollapply(z,
    width = k, FUN = fun, align = align, by.column = TRUE,
    ...
  )

  if (!is.null(group)) {
    rz <- zoo::fortify.zoo(rz)


    if (length(nm2) > 1 & length(catch) == 1) {
      rz <- tidyr::pivot_longer(rz,
        cols = -Index, names_to = nm2, values_to = "catch",
        names_sep = "__"
      )
    } else if (length(catch) > 1) {
      rz <- tidyr::pivot_longer(rz,
        cols = -Index, names_to = nm4, values_to = "catch",
        names_sep = "__"
      )
    }
  } else if (length(catch) > 1) {
    rz <- zoo::fortify.zoo(rz, melt = TRUE)
  } else {
    rz <- zoo::fortify.zoo(rz)
  }

  if (length(catch) == 1) {
    nm3 <- c(nm, catch)
    names(rz) <- nm3
  } else {
    nm5 <- c(nm, "species", "catch")
    names(rz) <- nm5
  }

  if (!is.null(filter_date)) {
    if (filter_date == "year-month") {
      rz <- subset(rz, (as.integer(format(rz[[date]], "%Y")) %in% filter_value[[1]]) &
        (as.integer(format(rz[[date]], "%m")) %in% filter_value[[2]]))
    } else if (filter_date == "year") {
      rz <- subset(rz, as.integer(format(rz[[date]], "%Y")) %in% filter_value)
    } else if (filter_date == "month") {
      rz <- subset(rz, as.integer(format(rz[[date]], "%m")) %in% filter_value)
    } else {
      warning("Invalid filter type. Available options are 'year-month', 'year', and 'month'.")
      x <- 1
    }

    if (nrow(rz) == 0) {
      warning("Filtered data table has zero rows. Check filter parameters.")
      x <- 1
    }
  }

  f_catch <- function() if (length(catch) == 1) catch else "catch"

  if (!is.null(facet)) {
    if (facet %in% c("year", "month")) {
      rz$year <- as.integer(format(rz[[date]], "%Y"))
      rz$month <- as.integer(format(rz[[date]], "%m"))
      rz$day <- as.integer(format(rz[[date]], if (facet == "year") "%j" else "%d"))

      date_lab <- function(x) {
        if (facet == "year") {
          format(as.Date(as.character(x), "%j"), "%b-%d")
        } else if (facet == "month") {
          format(as.Date(as.character(x), "%j"), "%d")
        }
      }

      plot <- ggplot2::ggplot(rz, ggplot2::aes_string("day", f_catch())) +
        fishset_theme +
        ggplot2::theme(legend.position = "bottom") +
        ggplot2::scale_x_continuous(labels = function(x) date_lab(x)) +
        ggplot2::facet_grid(stats::reformulate(".", facet), scales = scale)
    } else {
      plot <- ggplot2::ggplot(rz, ggplot2::aes_string(date, f_catch())) +
        fishset_theme +
        ggplot2::theme(legend.position = "bottom") +
        ggplot2::facet_grid(reformulate(".", facet), scales = scale)
    }
  } else {
    plot <- ggplot2::ggplot(rz, ggplot2::aes_string(date, f_catch())) +
      fishset_theme +
      ggplot2::theme(legend.position = "bottom")
  }

  if (!is.null(group)) {
    if (length(group) == 1) {
      if (length(catch) == 1) {
        plot <- plot + ggplot2::geom_line(ggplot2::aes_string(color = group1))
      } else {
        plot <- plot + ggplot2::geom_line(ggplot2::aes_string(
          color = "species",
          linetype = group1
        ))
      }
    } else if (length(group) == 2) {
      if (length(catch) == 1) {
        plot <- plot + ggplot2::geom_line(ggplot2::aes_string(
          color = group1,
          linetype = group2
        ))
      } else {
        plot <- plot + ggplot2::geom_line(ggplot2::aes_string(
          color = "species",
          linetype = group1,
          size = group2
        ))
      }
    }
  } else if (length(catch) > 1) {
    plot <- plot + ggplot2::geom_line(ggplot2::aes_string(color = "species"))
  } else {
    plot <- plot + ggplot2::geom_line()
  }

  save_plot(project, "roll_catch", plot)
  save_table(rz, project, "roll_catch")

  roll_catch_function <- list()
  roll_catch_function$functionID <- "roll_catch"
  roll_catch_function$args <- list(
    dat, project, catch, date, group, date, k, fun,
    filter_date, filter_value, facet_by, scale,
    align, convr, output
  )
  roll_catch_function$kwargs <- list(...)
  log_call(roll_catch_function)

  if (output == "table") {
    rz
  } else if (output == "plot") {
    plot
  } else if (output == "tab_plot") {
    tab_plot <- list(table = rz, plot = plot)

    tab_plot
  }
}
