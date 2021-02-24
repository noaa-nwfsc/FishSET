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
#' @param combine Whether to combine variables listed in \code{group}. This is passed
#'   to the "fill" or "color" aesthetic for plots. 
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
#' @importFrom dplyr any_of
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
roll_catch <- function(dat, project, catch, date, group = NULL, combine = FALSE, 
                       k = 10, fun = "mean", filter_date = NULL, 
                       date_value = NULL, filter_by = NULL, filter_value = NULL, 
                       filter_expr = NULL, facet_by = NULL, scale = "fixed", 
                       align = "center", convr = FALSE, tran = "identity", 
                       output = "tab_plot", ...) {
  out <- data_pull(dat)
  dataset <- out$dataset
  
  if (shiny::isRunning()) {
    if (deparse(substitute(dat)) == "values$dataset") dat <- get("dat_name")
  } else { 
    if (!is.character(dat)) dat <- deparse(substitute(dat)) }
  
  #Empty variable
  Index <- NULL
  
  group_date <- group[group %in% c("year", "month", "week")]
  facet_date <- facet_by[facet_by %in% c("year", "month", "week")]
  facet_no_date <- facet_by[!(facet_by %in% c("year", "month", "week"))]
  group_no_date <- group[!(group %in% c("year", "month", "week"))]
  
  # date ----
  # convert date and/or sub_date to date class
  if (!is.null(date) | !is.null(sub_date)) {
    
    dataset[unique(c(date, sub_date))] <- 
      lapply(dataset[unique(c(date, sub_date))], date_parser)
  } 
  
  # sub_date ----
  # check if sub_date is needed
  if (!is.null(filter_date)) {
    if (is.null(sub_date)) {
      if (!is.null(date)) {
        sub_date <- date
      } else {
        warning("Argument 'sub_date' required when subsetting by date.")
        end <- TRUE
      }
    }
  }
  
  if (!is.null(facet_by)) {
    if (any(facet_by %in% c("year", "month", "week"))) {
      if (is.null(sub_date)) {
        if (!is.null(date)) {
          sub_date <- date
        } else {
          warning("Spliting by a function-created date variable ('year', ",
                  "'month', or 'week') requires a date variable.")
          end <- TRUE
        }
      }
    } 
  }
  
  if (!is.null(group)) {
    if (any(group %in% c("year", "month", "week"))) {
      if (is.null(sub_date)) {
        if (!is.null(date)) {
          sub_date <- date
        } else {
          warning("Grouping by a function-created date variable ('year', ",
                  "'month', or 'week') requires a date variable.")
          end <- TRUE
        }
      }
    } 
  }
  
  # filter by date (pre roll apply) ----
  
  if (!is.null(filter_date)) {
    
    if (!is.null(sub_date)) {
      
      if (sub_date != date) {
        
        dataset <- subset_date(dataset, sub_date, filter_date, date_value)
        
        if (nrow(dataset) == 0) {
          
          warning("Filtered data table has zero rows. Check filter parameters.")
          end <- TRUE
        }
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
  
  # add missing dates ----
  full_dates <- seq.Date(from = min(dataset[[date]], na.rm = TRUE),
                         to = max(dataset[[date]], na.rm = TRUE),
                         by = "day")
  
  full_dates <- zoo::zoo(0, full_dates)
  
  # summary table ----
  if ("species" %in% facet_by) {
    
    facet_no_date <- facet_no_date[facet_no_date != "species"]
    
    if (length(facet_no_date) == 0) {
      facet_no_date <- NULL
    }
  } 
  
  agg_no_date <- unique(c(facet_no_date, group_no_date))
  
  sum_tab <- agg_helper(dataset, value = catch, period = date, 
                        group = agg_no_date, fun = sum)
  
  # catch conversion ----
  if (convr != FALSE) {
    if (convr == "tons") {
      sum_tab[catch] <- sum_tab[catch] / 2000
    } else if (convr == "metric_tons") {
      sum_tab[catch] <- sum_tab[catch] / 2204.62
    } else if (is.function(convr)) {
      sum_tab[catch] <- do.call(convr, list(sum_tab[catch]))
    }
  }
  
  # data prep (pivot table) before converting to zoo object
  if (length(agg_no_date) > 0) {
    
    sum_tab[agg_no_date] <- lapply(sum_tab[agg_no_date], trimws)
    
    if (length(catch) == 1) {
      
      sum_tab <- tidyr::pivot_wider(sum_tab, names_from = agg_no_date, 
                                    values_from = catch, names_sep = "__", 
                                    values_fill = 0)
      
    } else {
      
      sum_tab <- tidyr::pivot_longer(sum_tab, cols = !!catch, values_to = "catch", 
                                     names_to = "species")
      
      species_grp <- c(agg_no_date, "species")
      sum_tab <- tidyr::pivot_wider(sum_tab, names_from = !!species_grp, 
                                    names_sep = "__", values_from = "catch", 
                                    values_fill = list(catch = 0))
    }
  }
  
  # zoo ----
  # convert summary table to zoo object, merge missing dates
  sum_tab <- zoo::zoo(sum_tab[-which(names(sum_tab) == date)], sum_tab[[date]])
  sum_tab <- zoo::merge.zoo(sum_tab, full_dates, all = TRUE, fill = 0)
  
  sum_tab$full_dates <- NULL
  
  roll_tab <- zoo::rollapply(sum_tab, width = k, FUN = fun, align = align, 
                             by.column = TRUE, ...)
  
  # fortify table for plotting ----
  
  roll_tab <- zoo::fortify.zoo(roll_tab)
  
  sep <- function() {
    if (length(agg_no_date) > 0) {
      if (length(catch) == 1) {
        NULL
      } else if (length(catch) > 1) {
        "__"
      }
      
    } else {
      NULL
    }
  }
  
  names_to <- function() {

    if (length(catch) == 1 & length(agg_no_date) > 0) {
      agg_no_date
    } else if (length(catch) > 1 & length(agg_no_date) == 0) {
      "species"
    } else if (length(catch) > 1 & length(agg_no_date) > 0) {
      species_grp
    }
  }
  
  # create facet/group date variables
  # facet date ----
  if (length(facet_date) > 0) {
    
    roll_tab[facet_date] <- lapply(facet_date, function(x) {
      fp <- switch(x, "year" = "%Y", "month" = "%b", "week" = "%U")
      if (fp == "%b") {
        factor(format(roll_tab$Index, fp), levels = month.abb, ordered = TRUE) 
      } else {
        as.integer(format(roll_tab$Index, fp))
      }
    })
  }
  
  # group date ----
  if (length(group_date) > 0) {
    
    group_date2 <- group_date[!(group_date %in% facet_date)]
    
    if (length(group_date2) > 0) {
      
      for (i in group_date2) {
        x <- switch(i, "year" = "%Y", "month" = "%b", "week" = "%U")
        
        roll_tab[[i]] <- format(roll_tab$Index, x)
        
        if (i == "month") {
          roll_tab[[i]] <- factor(roll_tab[[i]], levels = month.abb, ordered = TRUE)
        }
      }
    }
  }
  
  if (length(catch) > 1 | length(agg_no_date) > 0) {
    
    roll_tab <- tidyr::pivot_longer(roll_tab, cols = !dplyr::any_of(c("Index", "year", 
                                                               "month", "week")), 
                                    names_to = names_to(), values_to = "catch", 
                                    names_sep = sep())
  }
  
  # combine groups ----
  if (length(group) > 0) {

    if (combine == TRUE & length(group) > 1) {

      roll_tab <- ID_var(roll_tab, vars = group, type = "string", drop = TRUE)
      group <- gsub(" ", "", paste(group, collapse = "_"))
      group1 <- group
      group2 <- NULL

    } else {
    
      roll_tab[group_no_date] <- lapply(roll_tab[group_no_date], as.factor)
      group1 <- group[1]
      
      if (length(group) == 1) group2 <- NULL else group2 <- group[2]
      
      if (length(group) > 2) {
        
        warning("Only the first two grouping variables will be displayed in plot.")
      }
    }
  }
  
  names(roll_tab)[names(roll_tab) == "Index"] <- date
  
  if (length(catch) == 1) {
    names(roll_tab)[names(roll_tab) == "catch"] <- catch
  }

  # filter date (post roll apply) ----
  if (!is.null(filter_date)) {
    
    if (!is.null(sub_date)) {
      
      if(sub_date == date) {
        
        roll_tab <- subset_date(roll_tab, sub_date, filter_date, date_value)
        
        if (nrow(roll_tab) == 0) {
          
          warning("Filtered data table has zero rows. Check filter parameters.")
        }
      }
    }
  }
  
  # plot ----
  if (output %in% c("tab_plot", "plot")) {
    
    # plot functions ----
    catch_exp <- function() if (length(catch) == 1) rlang::sym(catch) else rlang::sym("catch")
    
    date_sym <- rlang::sym(date)
    
    x_axis_exp <- function() {
      if (!is.null(facet_by)) {
        if (facet_by %in% c("year", "month")) {
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
    
    title_lab <- function() paste("Rolling", fun, k, "day", catch)
    
    y_lab <- function() paste(catch, ifelse(tran == "identity", "", paste0("(", tran, ")")))
    
    date_lab <- function(x) {
      if (facet_date == "year") {
        format(as.Date(as.character(x), "%j"), "%b-%d")
        
      } else if (facet_date == "month") {
        format(as.Date(as.character(x), "%j"), "%d")
      }
    }
    
    if (length(facet_date) > 0) {
      if (facet_date %in% c("year", "month")) {
        roll_tab$year <- as.integer(format(roll_tab[[date]], "%Y"))
        roll_tab$month <- as.integer(format(roll_tab[[date]], "%m"))
        roll_tab$day <- as.integer(format(roll_tab[[date]], if (facet_date == "year") "%j" else "%d"))
      }
    }
    
    rc_plot <- 
      ggplot2::ggplot(roll_tab, ggplot2::aes(!!x_axis_exp(), !!catch_exp())) +
      ggplot2::geom_line(ggplot2::aes(color = !!color_exp(), 
                                      linetype = !!linetype_exp())) + 
      ggplot2::scale_y_continuous(trans = tran) +
      ggplot2::labs(title = title_lab(), y = y_lab()) + 
      fishset_theme() +
      ggplot2::theme(legend.position = "bottom")
    
    
    if (!is.null(facet_by)) {
      if (facet_by %in% c("year", "month")) {
        rc_plot <- 
          rc_plot + 
          ggplot2::scale_x_continuous(labels = function(x) date_lab(x))
      }
      
      rc_plot <- 
        rc_plot +
        ggplot2::facet_grid(stats::reformulate(".", facet_by), scales = scale)
    }
    
    save_plot(project, "roll_catch", rc_plot)
  }
  
  save_table(roll_tab, project, "roll_catch")
  
  roll_catch_function <- list()
  roll_catch_function$functionID <- "roll_catch"
  roll_catch_function$args <- list(dat, project, catch, date, group, combine, k, 
                                   fun, sub_date, filter_date, date_value, filter_by, 
                                   filter_value, filter_expr, facet_by, scale, align, 
                                   convr, tran, output)
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
