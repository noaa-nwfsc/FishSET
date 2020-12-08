# Vessel count
#' 
#'  Active vessels by time period
#' 
#' @param dat Primary data containing information on hauls or trips. 
#'   Table in FishSET database contains the string 'MainDataTable'.
#' @param project String, name of project.
#' @param v_id Variable in \code{dat} containing vessel identifier to count.
#' @param date Date variable to aggregate by.
#' @param period Time period to aggregate by. Options include \code{"year"}, \code{"month"}, \code{"weeks"} 
#'   (weeks in the year), \code{"weekday"}, \code{"weekday_abv"}, \code{"day"} (day of the month), 
#'   and \code{"day_of_year"}.
#' @param group Names of grouping variables. For line plots two grouping variables 
#'   can be entered, the first is passed to "color" and second to "linetype". Only one 
#'   grouping variable can be used for barplots, which is passed to "fill". When \code{combine = TRUE} 
#'   all variables in \code{group} will be joined. 
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
#' @param facet_by Variable name to facet by. Accepts up to two variables. These can be 
#'   variables that exist in the dataset, or a variable created by \code{vessel_count()} such 
#'   as \code{"year"} or \code{"month"}. The first variable is facetted by row and the 
#'   second by column. 
#' @param combine Whether to combine variables listed in \code{group}. This is passed
#'   to the "fill" or "color" aesthetic for plots. 
#' @param position Positioning of bar plot. Options include 'stack', 'dodge', 
#'   and 'fill'.
#' @param value Whether to return \code{"count"} or \code{"percent"} of active vessels. Defaults
#'   to \code{"count"}. 
#' @param tran transformation function applied to vessel count. Defaults to \code{FALSE}. 
#' @param type Plot type, options include \code{"bar"} (the default) and \code{"line"}. 
#' @param scale Scale argument passed to \code{\link{facet_grid}}. 
#'   Options include \code{"free"}, \code{"free_x"}, \code{"free_y"}. Defaults to 
#'   \code{"fixed"}. 
#' @param output Whether to display \code{"plot"}, \code{"table"}. Defaults 
#'   to both (\code{"tab_plot"}). 
#' @return \code{vessel_count} aggregates the number (or percent) of active vessels
#'   by time period using a column of unique vessel IDs. The data can be filter using 
#'   two arguments: \code{filter_date} and \code{filter_value}. \code{filter_date}
#'   specifies how the data should be filtered--by year, month, or year-month. 
#'   \code{filter_value} should contain the years or months (as integers) to filter
#'   the data by. Up to two grouping variables can be entered. Grouping variables can 
#'   be merged into one variable using \code{combine}; in this case any number of 
#'   variables can be joined, but no more than three is recommended. For faceting,
#'   any variable (including ones listed in \code{group}) can be used, but "year" and
#'   "month" are also available. Currently, combined variables cannot be faceted.
#'   A list containing a table and plot are printed to the console and viewer by default. 
#' @examples 
#' \dontrun{
#' vessel_count(pollockMainDataTable, 'VESSEL_ID', 'DATE_FISHING_BEGAN', period = 'month', 
#'              group = 'DISEMBARKED_PORT', position = 'dodge', output = 'plot')
#' }
#' @export vessel_count
#' @import ggplot2
#' @importFrom stats reformulate
#' @importFrom rlang sym expr
#' @importFrom scales percent
#' @importFrom shiny isRunning

vessel_count <- function(dat, project, v_id, date = NULL, period = NULL, group = NULL, 
                         filter_date = NULL, date_value = NULL, filter_by = NULL, 
                         filter_value = NULL, filter_expr = NULL, facet_by = NULL,
                         combine = FALSE, position = "stack", tran = "identity", 
                         value = "count", type = "bar", scale = "fixed", output = "tab_plot") {
  
  # Call in datasets
  out <- data_pull(dat)
  dataset <- out$dataset
  
  if (shiny::isRunning()) {
    if (deparse(substitute(dat)) == "values$dataset") dat <- get("dat_name")
  } else { 
    if (!is.character(dat)) dat <- deparse(substitute(dat)) }
  
  # Convert to string if v_id is a factor
  if (is.factor(dataset[[v_id]])) dataset[[v_id]] <- as.character(dataset[[v_id]])
  
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
  
  # group----
  if (!is.null(group)) {
    
    if (combine == TRUE) { 
      
      dataset <- ID_var(dataset, vars = group, type = "string")
      group <- gsub(" ", "", paste(group, collapse = "_"))
      group2 <- NULL
    }
    
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
  
  # date ----
  facet_date <- facet[facet %in% c("year", "month", "week")]
  
  if (!is.null(date)) {
    
    periods <- c("year_month", "month_year", "year", "month", "weeks", "weekday", "day", "day_of_year")
    
    if (period %in% periods == FALSE) {
      
      stop("Invalid period. Please select a valid period name (see documentation for details).")
      
    } else {
      
      p <- switch(period, year_month = "%Y-%m", month_year = "%Y-%m", year = "%Y",
                  month = "%b", weeks = "%U", weekday = "%a", day = "%d", day_of_year = "%j")
    }
    
    dataset <- add_missing_dates(dataset, date, v_id, group = group, facet_by = facet_by)
    
    dataset[[period]] <- format(dataset[[date]], p)
    
    if (!is.null(facet_date)) {
      
      if (period != "month" & any("month" %in% facet_date)) {
        
        dataset$month <- factor(format(dataset[[date]], "%b"), levels = month.abb, ordered = TRUE)
        
      } else if (period != "week" & any("week" %in% facet_date)) {
        
        dataset$week <- as.integer(format(dataset[[date]], "%U"))
      }
    }
  }
  
  # filter date ----
  if (!is.null(filter_date)) {
    
    dataset <- subset_date(dataset, date, filter_date, date_value)
    
    if (nrow(dataset) == 0) {
      
      warning("Filtered data table has zero rows. Check filter parameters.")
      end <- TRUE
    }
  }
  
  # summary table ----
  agg_grp <- c(group, facet_by, facet_date)
  
  table_out <- agg_helper(dataset, value = v_id, period = period, 
                          group = agg_grp, fun = function(x) length(unique(x)))
  
  if (value == "percent") {
    
    v_id_perc <- paste0(v_id, "perc")
    table_out[[v_id_perc]] <- (table_out[[v_id]]/sum(table_out[[v_id]])) * 100
  }
  
  # convert period to factor
  if (!is.null(date)) {
    
    if (p %in% c("%Y-%m", "%a", "%b")) {
      
      table_out <- date_factorize(table_out, period, p)
      
    } else {
      
      table_out[[period]] <- as.integer(table_out[[period]])
    }
    # reorder table by period
    table_out <- table_out[order(table_out[[period]]), ]
  }
  
  # order group1 by value
  if (!is.null(group)) {
    
    rev <- ifelse(position == "dodge", TRUE, FALSE)
    or_nm <- ifelse(value == "percent", v_id_perc, v_id)
    table_out <- order_factor(table_out, group1, or_nm, rev = rev)
  }
  
  row.names(table_out) <- 1:nrow(table_out)
  
  if (output %in% c("plot", "tab_plot")) {
    
    # plot functions ----
    vessel_exp <- function() if (value == "percent") rlang::sym(v_id_perc) else rlang::sym(v_id)
    
    xaxis_exp <- function() {
      if (!is.null(date)) { 
        rlang::sym(period)
        
      } else { 
        
        if (!is.null(group)) {
          rlang::sym(group1) 
        } else {
          v_id
        }
      }
    }
    
    group1_exp <- function() if (!is.null(group)) rlang::sym(group1) else NULL
    group2_exp <- function() if (length(group) > 1) rlang::sym(group2) else NULL
    
    if (!is.null(date)) {
      
      interaction_exp <- function() {
        if (is.null(group)) 1
        else if (!is.null(group) & length(group) == 1) group1_exp()
        else if (length(group) > 1) rlang::expr(interaction(!!group1_exp(), !!group2_exp())) }
      
      color_exp <- function() {
        if (is.null(group)) NULL
        else if (!is.null(group)) group1_exp()
      }
      
      linetype_exp <- function() {
        if (is.null(group) | !is.null(group) & length(group) == 1) NULL
        else if (!is.null(group) & length(group) > 1) group2_exp()
      }
      
    } else {
      
      interaction_exp <- function() {
        if (is.null(group)) {
          NULL
        } else if (!is.null(group) & length(group) == 1) {
          1
        } else if (length(group) > 1) {
          group2_exp()
        }
      }
      
      color_exp <- function() {
        if (is.null(group) | !is.null(group) & length(group) == 1) NULL
        else if (!is.null(group) & length(group) > 1) group2_exp()
      }
      
      linetype_exp <- function() NULL
    }
    
    v_plot <- ggplot2::ggplot(data = table_out, ggplot2::aes(x = !!xaxis_exp(), y = !!vessel_exp())) +
      FishSET:::fishset_theme() +
      ggplot2::theme(legend.position = "bottom") +
      ggplot2::scale_y_continuous(labels = if (value == "percent") scales::percent else ggplot2::waiver(),
                                  trans = tran)
    
    if (type == "bar") {
      
      if (position == "dodge") position <- ggplot2::position_dodge2(preserve = "single")
      
      v_plot <- v_plot + ggplot2::geom_col(ggplot2::aes(fill = !!color_exp()), position = position)
      
      position <- ifelse(class(position)[1] == "PositionDodge2", "dodge", position)
      
    } else if (type == "line") {
      
      v_plot <- v_plot + ggplot2::geom_line(ggplot2::aes(group = !!interaction_exp(), color = !!color_exp(), 
                                                         linetype = !!linetype_exp())) +
        ggplot2::geom_point(ggplot2::aes(group = !!interaction_exp(), color = !!color_exp()), size = 2)
    }
    
    if (!is.null(facet)) {
      
      if (length(facet) == 1) {
        
        fm <- stats::reformulate(".", facet)
        
      } else if (length(facet) == 2) {
        
        fm <- paste(facet, sep = " ~ ")
      }
      
      v_plot <- v_plot + ggplot2::facet_grid(fm, scales = scale)
    }
    
    v_plot <- v_plot + ggplot2::labs(y = "active vessels")
    
    if (!is.null(date)) {
      # adjust x-axis breaks 
      if (!(p %in% c("%a", "%b", "%Y-%m"))) {
        
        v_plot <- v_plot + ggplot2::scale_x_continuous(breaks = num_breaks(table_out[[period]]))
        
      } else if (period == "month_year") {
        
        d_labs <- levels(table_out$month_year)
        d_labs <- format(as.Date(paste0(d_labs, "-01")), "%b %y")
        v_plot <- v_plot + ggplot2::scale_x_discrete(labels = d_labs)
      }
    }
    
    save_plot(project, "vessel_count", v_plot)
  }
  
  if (!is.null(date)) {
    if (period == "month_year") {
      table_out$month_year <- format(as.Date(paste0(table_out$month_year, "-01")), "%b %y")
    }
  }
  # Log function
  vessel_count_function <- list()
  vessel_count_function$functionID <- "vessel_count"
  vessel_count_function$args <- list(dat, project, v_id, date, period, group, filter_date,
                                     date_value, filter_by, filter_value, filter_expr, facet_by, 
                                     combine, position, tran,  value, type, scale, output)
  log_call(vessel_count_function)
  
  # Output folder
  save_table(table_out, project, "vessel_count")
  
  if (output == "table") {
    
    table_out
    
  } else if (output == "plot") {
    
    v_plot
    
  } else {
    
    out_list <- list(table = table_out,
                     plot = v_plot)
    out_list
  }
}


