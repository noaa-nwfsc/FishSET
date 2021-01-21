# Weekly catch
#' 
#' Summarize weekly catch 
#'
#' @param dat Primary data containing information on hauls or trips. 
#'   Table in FishSET database contains the string 'MainDataTable'.
#' @param project String, name of project.
#' @param species A variable in \code{dat} containing the species catch or a vector
#'   of species variables.
#' @param date Variable in \code{dat} containing dates to aggregate by.
#' @param fun Name of function to aggregate by. Defaults to \code{\link[base]{sum}}. 
#' @param group Grouping variable names(s). Up to two grouping variables are available for
#'   line plots and one for bar plots. For bar plots, if only one species is entered the first
#'   group variable is passed to 'fill'. If multiple species are entered, species is passed to
#'   "fill" and the grouping variable is dropped. An exception occurs when faceting by species,
#'   then the grouping variable is passed to "fill". For line plots, the first grouping variable
#'   is passed to "fill" and the second to "linetype" if a single species column is entered or if
#'   faceting by species. Otherwise, species is passed to "fill", the first group variable to
#'   "linetype", and second is dropped.
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
#'   the dataset, or a variable created by \code{species_catch()} such as \code{"year"}, 
#'   \code{"month"}, or \code{"species"}.
#' @param facet_by Variable name to facet by. This can be a variable that exists in 
#'   the dataset, or a variable created by \code{weekly_catch()} such as \code{"year"}, 
#'   \code{"month"}, or \code{"species"}.  
#' @param type Plot type, options include \code{"bar"} (the default) and \code{"line"}. 
#' @param conv Convert catch variable to \code{"tons"}, \code{"metric_tons"}, or 
#'   by using a function entered as a string. Defaults to \code{"none"} for no conversion.
#' @param tran A function to transform the y-axis. Options include log, log2, log10, sqrt.
#' @param value Whether to calculate raw \code{"count"} or \code{"percent"} of total catch. 
#' @param position Positioning of bar plot. Options include 'stack', 'dodge', 
#'   and 'fill'. 
#' @param combine Whether to combine variables listed in \code{group}. This is passed
#'   to the "fill" or "color" aesthetic for plots. 
#' @param scale Scale argument passed to \code{\link{facet_grid}}. Defaults to \code{"fixed"}.
#' @param output Return output as \code{"plot"}, \code{"table"}, or both \code{"tab_plot"}. Defaults 
#'   to both (\code{"tab_plot"}).
#' @param format_tab How table output should be formatted. Options include 'wide' 
#'   (the default) and 'long'.
#' @return \code{weekly_catch()} aggregates catch (or percent)
#'   by week using one or more columns of catch data. The data can be filter using
#'   two arguments: \code{filter_date} and \code{filter_value}. \code{filter_date}
#'   specifies how the data should be filtered--by year, period (i.e. "month" or "week"), or year-period.
#'   \code{filter_value} should contain the values (as integers) to filter
#'   the data by. It is often useful to facet by year when using \code{filter_date}.
#'   Up to two grouping variables can be entered. Grouping variables can
#'   be merged into one variable using \code{combine = TRUE}. Any number of
#'   variables can be combined, but no more than three is recommended. For faceting,
#'   any variable (including ones listed in \code{group}) can be used, but "year" and
#'   "month" are also available. Currently, combined variables cannot be faceted.
#'   A list containing a table and plot are printed to the console and viewer by default. 
#' @examples 
#' \dontrun{
#' weekly_catch(pollockMainDataTable,
#'   species = c(
#'     "HAUL_LBS_270_POLLOCK_LBS",
#'     "HAUL_LBS_110_PACIFIC_COD_LBS",  "HAUL_LBS_OTHER_LBS"
#'   ), date = "DATE_FISHING_BEGAN",
#'   convert_to_tons = T, year = 2011, output = "plot"
#' )
#' }
#' @export weekly_catch
#' @importFrom stats reformulate
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom scales label_percent breaks_extended
#' @importFrom rlang sym expr
#' @importFrom shiny isRunning
#' @import ggplot2

weekly_catch <- function(dat, project, species, date, fun = "sum", group = NULL, 
                         filter_date = NULL, date_value = NULL, filter_by = NULL, 
                         filter_value = NULL, filter_expr = NULL, facet_by = NULL, 
                         type = "bar", conv = "none", tran = "identity", value = "count", 
                         position = "stack", combine = FALSE, scale = "fixed",
                         output = "tab_plot", format_tab = "wide") {
  
  # Call in datasets
  out <- data_pull(dat)
  dataset <- out$dataset
  
  if (shiny::isRunning()) {
    if (deparse(substitute(dat)) == "values$dataset") dat <- get("dat_name")
  } else { 
    if (!is.character(dat)) dat <- deparse(substitute(dat)) }
  
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
    
    dataset <- add_missing_dates(dataset, date, species, group = group, facet_by = facet_by)
    
    if (!is.null(facet_date)) {
      
      if ("month" %in% facet_date) {
        
        dataset$month <- factor(format(dataset[[date]], "%b"), levels = month.abb, ordered = TRUE)
        
        
      } else if ("week" %in% facet_date) {
        
        dataset$week <- as.integer(format(dataset[[date]], "%U"))
      }
    }
  }
  
  # date filter ----
  if (!is.null(filter_date)) {
    
    dataset <- subset_date(dataset, date, filter_date, date_value)
    
    if (nrow(dataset) == 0) {
      
      warning("Filtered data table has zero rows. Check filter parameters.")
      end <- TRUE
    }
  }
  
  # add year and week columns
  dataset$year <- format(dataset[[date]], "%Y")
  dataset$week <- format(dataset[[date]], "%U")
  
  if (!is.null(facet_date)) {
    
    if ("month" %in% facet_date){
      
      dataset$month <- factor(format(dataset[[date]], "%b"), 
                              levels = month.abb, ordered = TRUE)
    }
  }
  
  # summary table ----
  agg_grp <- c(group, facet_by, facet_date)
  
  table_out <- agg_helper(dataset, value = species, period = c("year", "week"), 
                          group = agg_grp, fun = fun)
  
  table_out[c("year", "week")] <- lapply(table_out[c("year", "week")], as.integer)
  
  if (length(species) > 1) {
    
    table_out <- tidyr::pivot_longer(table_out, cols = species, names_to = "species", 
                                     values_to = "catch")
    
    rev <- ifelse(position == "dodge", TRUE, FALSE)
    table_out <- order_factor(table_out, "species", "catch", rev = rev)
    
  } else {
    
    if (!is.null(group)) {
      rev <- ifelse(position == "dodge", TRUE, FALSE)
      table_out <- order_factor(table_out, group1, species, rev = rev)
    }
  }
  
  # weight conversion
  f_catch <- function() if (length(species) == 1) species else "catch"
  
  if (conv != "none") {
    
    if (conv == "tons") {
      
      table_out[f_catch()] <- table_out[f_catch()]/2000
      
    } else if (conv == "metric_tons") {
      
      table_out[f_catch()] <- table_out[f_catch()]/2204.62
      
    } else {
      
      table_out[f_catch()] <- do.call(conv, list(table_out[f_catch()]))
    }
  }
  
  if (value == "percent") {
    
    if (fun == "sum") {
      
      table_out[f_catch()] <- (table_out[f_catch()]/sum(table_out[f_catch()])) * 100
      
    } else {
      
      warning("Cannot convert to percentage. Change 'fun' argument to 'sum'.")
    } 
  }
  
  # plot section ----
  if (output %in% c("plot", "tab_plot")) {
    
    # plot functions ----
    
    if (length(species) == 1) single_species_sym <- rlang::sym(species)
    
    if (length(species) > 1) multi_species_sym <- rlang::sym("species")
    
    if (!is.null(group)) {
      
      group1_sym <- rlang::sym(group1)
      if (length(group) > 1) group2_sym <- rlang::sym(group2)
    }
    
    y_axis_exp <- function() if (length(species) == 1) single_species_sym else rlang::sym("catch")
    
    interaction_exp <- function() {
      if (length(species) == 1) {
        if (is.null(group)) {
          1
          
        } else {
          if (length(group) == 1) {
            group1_sym
            
          } else if (length(group) > 1) {
            rlang::expr(interaction(!!group1_sym, !!group2_sym))
          }
        }
        
      } else if (length(species) > 1) {
        if (is.null(group)) {
          multi_species_sym
          
        } else if (length(group) == 1) {
          rlang::expr(interaction(!!multi_species_sym, !!group1_sym))
          
        } else if (length(group) > 1) {
          rlang::expr(interaction(!!multi_species_sym, !!group1_sym, !!group2_sym))
        }
      }
    }
    
    color_exp <- function() {
      
      if (length(species) == 1) {
        if (!is.null(group)) {
          group1_sym
        } else {
          NULL
        }
      } else if (length(species) > 1) {
        multi_species_sym
      }
    }
    
    linetype_exp <- function() {
      
      if (length(species) == 1) {
        if (length(group) > 1) {
          group2_sym
        } else {
          NULL
        }
      } else if (length(species) > 1) {
        group1_sym
      }
    }

    x_lab <- function() paste("week", date)
    y_lab <- function() paste(fun, f_catch(), ifelse(tran == "identity", "", paste0("(", tran, ")")))
    scale_lab <- function() if (value == "percent") scales::label_percent(scale = 1) else ggplot2::waiver()
    y_breaks <- function() {
      if (tran != "identity") {
        scales::breaks_extended(n = 7) 
      } else {
        ggplot2::waiver()
      }
    }
    
    w_plot <- ggplot2::ggplot(data = table_out, ggplot2::aes(x = week, y = !!y_axis_exp())) +
      fishset_theme() +
      ggplot2::theme(legend.position = "bottom") +
      ggplot2::scale_y_continuous(labels = scale_lab(), trans = tran, breaks = y_breaks()) +
      ggplot2::scale_x_continuous(breaks = num_breaks(table_out$week), 
                                  labels = week_labeller(num_breaks(table_out$week), 
                                                         year = table_out$year)) +
      ggplot2::labs(x = x_lab(), y = y_lab())
    
    if (type == "bar") {
      
      if (position == "dodge") position <- ggplot2::position_dodge2(preserve = "single")
      
      w_plot <- w_plot + ggplot2::geom_col(ggplot2::aes(fill = !!color_exp()), 
                                           position = position)
      
      position <- ifelse(class(position)[1] == "PositionDodge2", "dodge", position)
      
    } else if (type == "line") {
      
      w_plot <- w_plot + ggplot2::geom_line(ggplot2::aes(group = !!interaction_exp(),
                                                         color = !!color_exp(), 
                                                         linetype = !!linetype_exp())) +
        ggplot2::geom_point(ggplot2::aes(group = !!interaction_exp(), color = !!color_exp()), 
                            size = 1)
    }
    
    if (!is.null(facet)) {
      
      if (length(facet) == 1) {
        
        fm <- stats::reformulate(".", facet)
        
      } else if (length(facet) == 2) {
        
        fm <- paste(facet, sep = " ~ ")
      }
      
      w_plot <- w_plot + ggplot2::facet_grid(fm, scales = scale)
    }
    
    if (!is.null(filter_date)) {
      w_plot <- date_title(w_plot, filter_date, filter_value)
    }
  }
  
  if (length(species) > 1 & format_tab == "wide") {
    
    table_out <- tidyr::pivot_wider(table_out, names_from = species, values_from = catch)
  }
  # log function
  weekly_catch_function <- list()
  weekly_catch_function$functionID <- "weekly_catch"
  weekly_catch_function$args <- list(dat, project, species, date, fun, group, filter_date, 
                                     date_value, filter_by, filter_value, filter_expr, 
                                     facet_by, type, conv, tran, value, position, 
                                     combine, scale,  output, format_tab)
  log_call(weekly_catch_function)
  
  save_plot(project, "weekly_catch", w_plot)
  save_table(table_out, project, "weekly_catch")
  
  if (output == "table") {
    
    table_out
    
  } else if (output == "plot") {
    
    w_plot
    
  } else {
    
    out_list <- list(table = table_out,
                     plot = w_plot)
    out_list
  }
}

