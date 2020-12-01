#' Catch by Species
#'
#' Creates a table or plot of total species catch by period
#' 
#' @param dat Primary data containing information on hauls or trips. 
#' Table in FishSET database contains the string 'MainDataTable'.
#' @param project String, name of project.
#' @param species Variable in \code{dat} containing the species catch 
#' or a vector of species variables (in pounds).
#' @param date Variable in \code{dat} containing dates to aggregate by.
#' @param period Time period to count by. Options include 'year', 'month', 'weeks' 
#' (weeks in the year),'weekday', 'day' (day of the month), and 'day_of_year'.
#' @param fun String, name of function to aggregate by. Defaults to \code{\link[base]{sum}}. 
#' @param group Grouping variable names(s). Up to two grouping variables are available for 
#'   line plots and one for bar plots. For bar plots, if only one species is entered the first 
#'   group variable is passed to 'fill'. If multiple species are entered, species is passed to 
#'   "fill" and the grouping variable is dropped. An exception occurs when facetting by species, 
#'   then the grouping variable is passed to "fill". For line plots, the first grouping variable 
#'   is passed to "fill" and the second to "linetype" if a single species column is entered or if 
#'   facetting by species. Otherwise, species is passed to "fill", the first group variable to 
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
#' @param output Output a \code{"plot"} or \code{"table"}. Defaults 
#'   to both (\code{"tab_plot"}).
#' @param format_tab How table output should be formatted. Options include 'wide' 
#'   (the default) and 'long'.
#' @return  \code{species_catch()} aggregates catch (or percent)
#'   by time period using one or more columns of catch data. The data can be filtered using
#'   two arguments: \code{filter_date} and \code{filter_value}. \code{filter_date}
#'   specifies how the data should be filtered--by year, period (see \code{period}), or year-period.
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
#' species_catch('pollockMainDataTable', species = c('HAUL_LBS_270_POLLOCK_LBS', 
#' 'HAUL_LBS_110_PACIFIC_COD_LBS', 'HAUL_LBS_OTHER_LBS'), date = 'HAUL_DATE', 
#' value = 'count', period = 'month_num', output = 'plot', year = 2011, convert_to_tons = TRUE)
#' }
#' @export species_catch
#' @import ggplot2
#' @importFrom stats aggregate reformulate
#' @importFrom reshape2 dcast melt
#' @importFrom dplyr anti_join
#' @importFrom rlang expr sym
#' @importFrom scales percent 
#' @importFrom shiny isRunning

species_catch <- function(dat, project, species, date = NULL, period = NULL, fun = "sum", 
                          group = NULL, filter_date = NULL, date_value = NULL, 
                          filter_by = NULL, filter_value = NULL, filter_expr = NULL,
                          facet_by = NULL, type = "bar", conv = "none", tran = "identity", 
                          value = "count", position = "stack", combine = FALSE, 
                          scale = "fixed", output = "tab_plot", format_tab = "wide") {  
  
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
    
    if (combine == TRUE) { # update to new ID_var (my vesion)
      
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
  
  # date ----
  facet_date <- facet[facet %in% c("year", "month", "week")]
  
  if (!is.null(date)) {
    
    periods <- c("year", "month", "weeks", "weekday", "day", "day_of_year")
    
    if (period %in% periods == FALSE) {
      
      stop("Invalid period. Please select a valid period name (see documentation for details).")
      
    } else {
      
      p <- switch(period, year = "%Y", month = "%b", weeks = "%U", weekday = "%a", day = "%d", 
                  day_of_year = "%j")
    }
    
    dataset <- add_missing_dates(dataset, date, species, group = group, facet_by = facet_by)
    
    
    if (period != "year") { # makes year col required; consider changing
      
      dataset$year <- as.integer(format(dataset[[date]], "%Y"))
    }
    
    dataset[[period]] <- format(dataset[[date]], p)
    
    if (!is.null(facet_date)) {
      
      if (period != "month" & any("month" %in% facet_date)) {
        
        dataset$month <- factor(format(dataset[[date]], "%b"), levels = month.abb, ordered = TRUE)
        
        
      } else if (period != "week" & any("week" %in% facet_date)) {
        
        dataset$week <- as.integer(format(dataset[[date]], "%U"))
      }
    }
  }
  
  # filter ----
  if (!is.null(filter_date)) {
    
    dataset <- subset_date(dataset, date, filter_date, date_value)
    
    if (nrow(dataset) == 0) {
      
      warning("Filtered data table has zero rows. Check filter parameters.")
      end <- TRUE
    }
  }
  
  if (!is.null(filter_by)) {
    
    dataset <- subset_var(dataset, filter_by, filter_value, filter_expr)
    
    if (nrow(dataset) == 0) {
      
      warning("Filtered data table has zero rows. Check filter parameters.")
      end <- TRUE
    }
  }
  
  # summary table ----
  agg_grp <- c(group, facet_by, facet_date)
  
  table_out <- agg_helper(dataset, value = species, period = period, group = agg_grp, fun = fun)
  
  
  if (length(species) > 1) { # melt table if multiple species columns entered (for ggplot)
    
    table_out <- reshape2::melt(table_out, measure.vars = species, variable.name = "species", 
                                value.name = "catch")
    
    rev <- ifelse(position == "dodge", TRUE, FALSE)
    table_out <- order_factor(table_out, "species", "catch", rev = rev)
    
  } else {
    
    if (!is.null(group)) {
      
      rev <- ifelse(position == "dodge", TRUE, FALSE)
      table_out <- order_factor(table_out, group1, species, rev = rev)
    }
  }
  
  if (!is.null(date)) {
    # convert period to ordered factor/integer
    if (p %in% c("%a", "%b")) {
      
      table_out <- date_factorize(table_out, period, p)
      
    } else {
      
      table_out[[period]] <- as.integer(table_out[[period]])
    }
    
    table_out <- table_out[order(table_out[[period]]), ]
  }
  
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
    
    table_out[f_catch()] <- table_out[f_catch()]/sum(table_out[f_catch()])
  } 
  
  
  
  if (output %in% c("plot", "tab_plot")) {
    
    # plot functions ----
    
    catch_exp <- function() if (length(species) == 1) rlang::sym(species) else rlang::sym("catch")
    
    xaxis_exp <- function() {
      if (!is.null(date)) { 
        
        rlang::sym(period)
        
      } else { 
        
        if (!is.null(group)) {
          rlang::sym(group1) 
          
        } else {
          
          if (length(species) == 1) {
            
            species
          } else if (length(species) > 1) {
            
            rlang::sym("species")
          }
        }
      }
    }
    
    if (!is.null(date)) {
      
      interaction_exp <- function() {
        
        if (length(species) == 1) {
          if (is.null(group)) {
            1
            
          } else {
            if (length(group) == 1) {
              rlang::sym(group1)
              
            } else if (length(group) > 1) {
              rlang::expr(interaction(!!rlang::sym(group1), !!rlang::sym(group2)))
            }
          }
          
        } else if (length(species) > 1) {
          if (is.null(group)) {
            rlang::expr(species)
            
          } else if (length(group) == 1) {
            rlang::expr(interaction(species, !!rlang::sym(group1)))
            
          } else if (length(group) > 1) {
            rlang::expr(interaction(species, !!rlang::sym(group1), !!rlang::sym(group2)))
          }
        }
      }
      
      color_exp <- function() {
        
        if (length(species) == 1) {
          
          if (is.null(group)) {
            NULL
          } else {
            rlang::sym(group1)
          }
          
        } else if (length(species) > 1) {
          rlang::sym("species")
        }
      }
      
      
      linetype_exp <- function() {
        
        if (length(species) == 1) {
          if (is.null(group) | length(group) == 1) { 
            NULL
          } else {
            rlang::sym(group2)
          }
        } else if (length(species) > 1) {
          if (is.null(group)) {
            NULL 
          } else {
            rlang::sym(group1) 
          }
        }
      }
      
    } else {
      # aggregating by group only (no dates)
      
      interaction_exp <- function() {
        
        if (length(species) == 1) {
          
          if (is.null(group)) {
            
            NULL
          } else if (length(group) == 1) {
            
            1
          } else if (length(group) > 1) {
            
            rlang::sym(group2)
          }
        } else if (length(species) > 1) {
          
          if (is.null(group) | length(group) == 1) {
            
            rlang::sym("species")
          } else if (length(group) > 1) {
            
            rlang::expr(interaction(species, !!rlang::sym(group2)))
          }
        }
      }
      
      color_exp <- function() {
        
        if (length(species) == 1) {
          
          if (is.null(group) | length(group) == 1) {
            NULL
          } else if (length(group) > 1) {
            
            rlang::sym(group2)
          }
        } else if (length(species) > 1) {
          
          rlang::sym("species")
        }
      }
      
      linetype_exp <- function() {
        
        if (length(species) > 1 & length(group) > 1) rlang::sym(group2) else  NULL
      }
      
    }  
    
    
    # plot ----
    s_plot <- ggplot2::ggplot(data = table_out, ggplot2::aes(x = !!xaxis_exp(), y = !!catch_exp())) +
      fishset_theme() +
      ggplot2::theme(legend.position = "bottom") +
      ggplot2::scale_y_continuous(labels = if (value == "percent") scales::percent else ggplot2::waiver(),
                                  trans = tran)
    
    if (type == "bar") {
      
      if (position == "dodge") position <- ggplot2::position_dodge2(preserve = "single")
      
      s_plot <- s_plot + ggplot2::geom_col(ggplot2::aes(fill = !!color_exp()), 
                                           position = position)
      
      position <- ifelse(class(position)[1] == "PositionDodge2", "dodge", position)
      
    } else if (type == "line") {
      
      s_plot <- s_plot + ggplot2::geom_line(ggplot2::aes(group = !!interaction_exp(), 
                                                         color = !!color_exp(), 
                                                         linetype = !!linetype_exp())) +
        ggplot2::geom_point(ggplot2::aes(group = !!interaction_exp(), 
                                         color = !!color_exp()), 
                            size = 1)
    }
    
    if (!is.null(facet)) {
      
      if (length(facet) == 1) {
        
        fm <- stats::reformulate(".", facet)
        
      } else if (length(facet) == 2) {
        
        fm <- paste(facet, sep = " ~ ")
      }
      
      s_plot <- s_plot + ggplot2::facet_grid(fm, scales = scale)
    }
    
    if (!is.null(date)) {
      if (!(p %in% c("%a", "%b"))) {
        
        s_plot <- s_plot + ggplot2::scale_x_continuous(breaks = num_breaks(table_out[[period]]))
      }
    }
    
    save_plot(project, "species_catch")
  }
  
  # Log function ----
  species_catch_function <- list()
  species_catch_function$functionID <- "species_catch"
  species_catch_function$args <- list(dat, project, species, date, period, fun, group, 
                                      filter_date, date_value, filter_by, filter_value,
                                      filter_expr, facet_by, type, conv, tran, value, 
                                      position, combine, scale, output, format_tab)
  log_call(species_catch_function)
  
  if (format_tab == "wide") {
    
    if (length(species) > 1) {
      
      table_out <- reshape2::dcast(table_out, ... ~ species, value.var = "catch", fill = 0)
    }
  }
  
  
  save_table(table_out, project, "species_catch")
  
  # output ----
  if (output == "table") {
    
    table_out
    
  } else if (output == "plot") {
    
    s_plot
    
  } else if (output == "tab_plot") {
    
    out_list <- list(table = table_out,
                     plot = s_plot)
    out_list
  } 
}
