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
#' @param tran A function to transform the y-axis. Options include log, log2, log10, sqrt.
#' @param output Whether to display \code{"plot"}, \code{"table"}, or both. Defaults
#'   to both (\code{"tab_plot"}).
#' @param ... Additional arguments passed to \code{\link[zoo]{rollapply}}
#' @export roll_catch
#' @import ggplot2
#' @importFrom stats reformulate setNames
#' @importFrom rlang sym
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
#'  filter_date = "month", date_value = 4:6, facet_by = "month", convr = "tons"
#' )
#' }
#'
roll_catch <- function(dat, project, catch, date, group = NULL, k = 10, fun = "mean", 
                       filter_date = NULL, date_value = NULL, filter_by = NULL,
                       filter_value = NULL, filter_expr = NULL, facet_by = NULL, 
                       scale = "fixed", align = "center",convr = FALSE, tran = "identity", 
                       output = "tab_plot", ...) {
  out <- data_pull(dat)
  dataset <- out$dataset
  
  if (shiny::isRunning()) {
    if (deparse(substitute(dat)) == "values$dataset") dat <- get("dat_name")
  } else { 
    if (!is.character(dat)) dat <- deparse(substitute(dat)) }
  
  #Empty variable
  Index <- NULL
  
  # facet ----
  if (!is.null(facet_by)) {
    # if facet_by contains "species", "year", "month", or "week"
    special_facet <- ifelse(any(!(facet_by %in% names(dataset))), TRUE, FALSE) 
    
    if (special_facet == TRUE) {
      
      facet <- facet_by
      facet_by <- facet_by[facet_by %in% names(dataset)]
      
      if (length(facet_by) == 0) {
        
        facet_by <- NULL
      }
      
    } else {
      
      facet <- facet_by
    }
    
  } else {
    
    facet <- NULL
  }
  
  facet_date <- facet[facet %in% c("year", "month", "week")]
  
  # add missing dates ----
  dataset[[date]] <- date_parser(dataset[[date]])
  
  full_dates <- seq.Date(from = min(dataset[[date]], na.rm = TRUE),
                         to = max(dataset[[date]], na.rm = TRUE),
                         by = "day")
  
  full_dates <- zoo::zoo(0, full_dates)
  
  if (!is.null(group)) {
    
    dataset[group] <- lapply(dataset[group], as.factor)
    group1 <- group[1]
    
    if (length(group) == 1) group2 <- NULL else group2 <- group[2]
    
    if (length(group) > 2) {
      
      warning("Only the first two grouping variables will be displayed in plot.")
    }
    
  } else {
    
    group1 <- NULL
    group2 <- NULL
  }
  
  # filter by variable ----
  if (!is.null(filter_by) | !is.null(filter_expr)) {
    
    dataset <- subset_var(dataset, filter_by, filter_value, filter_expr)
    
    if (nrow(dataset) == 0) {
      
      warning("Filtered data table has zero rows. Check filter parameters.")
      end <- TRUE
    }
  }
  
  # summary table ----
  agg_grp <- unique(c(group, facet_by))
  
  sum_tab <- agg_helper(dataset, value = catch, period = date, group = agg_grp, fun = sum)
  
  # catch conversion ----
  if (convr != FALSE) {
    if (convr == "tons") {
      sum_tab[catch] <- sum_tab[catch] / 2000
    } else if (convr == "metric_tons") {
      sum_tab[catch] <- sum_tab[catch] / 2204.62
    } else {
      sum_tab[catch] <- do.call(convr, list(sum_tab[catch]))
    }
  }
  
  # data prep (pivot table) before converting to zoo object
  if (!is.null(group) | !is.null(facet_by)) {
    
    sum_tab[agg_grp] <- lapply(sum_tab[agg_grp], trimws)
    
    if (length(catch) == 1) {
      
      sum_tab <- tidyr::pivot_wider(sum_tab, names_from = agg_grp, 
                                    values_from = catch, names_sep = "__")
      sum_tab[is.na(sum_tab)] <- 0
      
    } else {
      
      sum_tab <- tidyr::pivot_longer(sum_tab, cols = !!catch, values_to = "catch", 
                                     names_to = "species")
      
      species_grp <- c(agg_grp, "species")
      sum_tab <- tidyr::pivot_wider(sum_tab, names_from = !!species_grp, names_sep = "__",
                                    values_from = "catch", values_fill = list(catch = 0))
    }
  }
  
  # convert summary table to zoo object, merge missing dates
  sum_tab <- zoo::zoo(sum_tab[-which(names(sum_tab) == date)], sum_tab[[date]])
  sum_tab <- zoo::merge.zoo(sum_tab, full_dates, all = TRUE, fill = 0)
  
  sum_tab$full_dates <- NULL
  
  roll_tab <- zoo::rollapply(sum_tab, width = k, FUN = fun, align = align, by.column = TRUE,
                             ...)
  
  # fortify table for plotting ----
  if (length(agg_grp) > 0) {
    
    if (length(catch) == 1) {
      
      if (length(agg_grp) > 1) {
        
        roll_tab <- zoo::fortify.zoo(roll_tab)
        roll_tab <- tidyr::pivot_longer(roll_tab, cols = -Index, names_to = agg_grp, 
                                        values_to = "catch", names_sep = "__")
      } else {
        
        roll_tab <- zoo::fortify.zoo(roll_tab, melt = TRUE)
      }
      
    } else if (length(catch) > 1) {
      
      roll_tab <- zoo::fortify.zoo(roll_tab)
      roll_tab <- tidyr::pivot_longer(roll_tab,cols = -Index, names_to = species_grp, 
                                      values_to = "catch", names_sep = "__")
    }
    
  } else if (length(catch) > 1) {
    roll_tab <- zoo::fortify.zoo(roll_tab, melt = TRUE)
    
  } else {
    roll_tab <- zoo::fortify.zoo(roll_tab)
  }
  
  # set original names ----
  if (length(catch) == 1) {
    roll_tab <- stats::setNames(roll_tab, c(date, agg_grp, catch))
    
  } else {
    
    roll_tab <- stats::setNames(roll_tab, c(date, agg_grp, "species", "catch"))
  }
  
  # filter by date ----
  if (!is.null(filter_date)) {
    
    roll_tab <- subset_date(roll_tab, date, filter_date, date_value)
    
    if (nrow(roll_tab) == 0) {
      
      warning("Filtered data table has zero rows. Check filter parameters.")
      end <- TRUE
    }
  }
  
  # plot ----
  if (output %in% c("tab_plot", "plot")) {
    
    # plot functions ----
    catch_exp <- function() if (length(catch) == 1) rlang::sym(catch) else rlang::sym("catch")
    
    date_sym <- rlang::sym(date)
    
    x_axis_exp <- function() {
      if (!is.null(facet)) {
        if (facet %in% c("year", "month")) {
          rlang::sym("day")
        } else {
          date_sym
        }
      } else {
        date_sym
      }
    }
    
    species_exp <- function() if (length(catch) > 1) rlang::sym("species") else NULL
    
    color_exp <- function() {
      if (length(catch) == 1) {
        if (is.null(group)) {
          NULL
        } else {
          rlang::sym(group1)
        }
      } else if (length(catch) > 1) {
        species_exp()
      }
    }
    
    linetype_exp <- function() {
      if (length(catch) == 1) {
        if (!is.null(group)) {
          if (length(group) == 1) {
            NULL
          } else {
            rlang::sym(group2)
          }
        } else {
          NULL
        }
      } else if (length(catch) > 1) {
        if (!is.null(group)) {
          rlang::sym(group1)
        } else {
          NULL
        }
      }
    }
    
    size_exp <- function() {
      if (length(group) > 1 & length(catch) > 1) {
        rlang::sym(group2) 
      } else { 
        NULL
      }
    }
    
    title_lab <- function() paste("Rolling", fun, k, "day", catch)
    
    y_lab <- function() paste(catch, ifelse(tran == "identity", "", paste0("(", tran, ")")))
    
    
    date_lab <- function(x) {
      if (facet == "year") {
        format(as.Date(as.character(x), "%j"), "%b-%d")
        
      } else if (facet == "month") {
        format(as.Date(as.character(x), "%j"), "%d")
      }
    }
    
    if (!is.null(facet)) {
      if (facet %in% c("year", "month")) {
        roll_tab$year <- as.integer(format(roll_tab[[date]], "%Y"))
        roll_tab$month <- as.integer(format(roll_tab[[date]], "%m"))
        roll_tab$day <- as.integer(format(roll_tab[[date]], if (facet == "year") "%j" else "%d"))
      }
    }
    
    rc_plot <- 
      ggplot2::ggplot(roll_tab, ggplot2::aes(!!x_axis_exp(), !!catch_exp())) +
      ggplot2::geom_line(ggplot2::aes(color = !!color_exp(), linetype = !!linetype_exp(),
                                      size = !!size_exp())) +
      ggplot2::scale_y_continuous(trans = tran) +
      ggplot2::labs(title = title_lab(), y = y_lab()) + 
      fishset_theme() +
      ggplot2::theme(legend.position = "bottom")
    
    
    if (!is.null(facet)) {
      if (facet %in% c("year", "month")) {
        rc_plot <- 
          rc_plot + 
          ggplot2::scale_x_continuous(labels = function(x) date_lab(x))
      }
      
      rc_plot <- 
        rc_plot +
        ggplot2::facet_grid(stats::reformulate(".", facet), scales = scale)
    }
    
    save_plot(project, "roll_catch", rc_plot)
  }
  
  save_table(roll_tab, project, "roll_catch")
  
  roll_catch_function <- list()
  roll_catch_function$functionID <- "roll_catch"
  roll_catch_function$args <- list(dat, project, catch, date, group, k, fun, filter_date, 
                                   date_value, filter_by, filter_value, filter_expr, facet_by, 
                                   scale, align, convr, tran, output)
  roll_catch_function$kwargs <- list(...)
  log_call(roll_catch_function)
  
  if (output == "table") {
    roll_tab
    
  } else if (output == "plot") {
    rc_plot
    
  } else if (output == "tab_plot") {
    tab_plot <- list(table = roll_tab, plot = rc_plot)
    
    tab_plot
  }
}
