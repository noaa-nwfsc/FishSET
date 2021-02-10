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
#' @param sub_date Date variable used for subsetting, grouping, or splitting by date.
#' @param filter_date The type of filter to apply to `MainDataTable`. To filter by a 
#'   range of dates, use \code{filter_date = "date_range"}. To filter by a given period, use
#'   "year-day", "year-week", "year-month", "year", "month", "week", or "day". 
#'   The argument \code{date_value} must be provided. 
#' @param date_value This argument is paired with \code{filter_date}. If \code{filter_date = "date_range"}, 
#'   enter a string containing a start- and end-date, e.g. \code{date_value = c("2011-01-01", "2011-03-15")}. 
#'   If filtering by period (e.g. "year", "year-month"), use integers 
#'   (4 digits if year, 1-2 digits if referencing a day, month, or week). Use a 
#'   list if using a year-period type filter, e.g. "year-week", with the format: 
#'   \code{list(year, period)}. Use a vector if using a single period (e.g. "month"): 
#'   \code{c(period)}. For example, \code{date_value = list(2011:2013, 5:7)} will 
#'   filter the data table from May through July for years 2011-2013 if \code{filter_date = "year-month"}.
#'   \code{date_value = c(2:5)} will filter the data from February through May when 
#'   \code{filter_date = "month"}.
#' @param filter_by String, variable name to filter `MainDataTable` by. the argument 
#'   \code{filter_value} must be provided.
#' @param filter_value A vector of values to filter `MainDataTable` by using the variable 
#'   in \code{filter_by}. For example, if \code{filter_by = "GEAR_TYPE"}, \code{filter_value = 1} 
#'   will include only observations with a gear type of 1. 
#' @param filter_expr String, a valid R expression to filter `MainDataTable` by using the variable 
#'   in \code{filter_by}. 
#' @param facet_by Variable name to facet by. Accepts up to two variables. These can be 
#'   variables that exist in the dataset, or a variable created by \code{species_catch()} such 
#'   as \code{"year"}, \code{"month"}, or \code{"week"} if a date variable is added 
#'   to \code{sub_date}. Facetting by \code{"species"} is available if multiple catch columns 
#'   are included in \code{"species"}. The first variable is facetted by row and the second by column.   
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
#' @return  \code{species_catch()} aggregates catch using one or more columns of catch 
#'   data. Users can aggregate by time period, by group, or by both. When multiple catch variables
#'   are entered, a new column "species" is created and used to group values in plots. 
#'   The "species" column can also be used to split (or facet) the plot. For table output,
#'   the "species" column will be kept if \code{format_tab = "long"}, i.e. a column of
#'   species names ("species") and a column containing catch ("catch"). When 
#'   \code{format_tab = "wide"}, each species is given its own column of catch.  
#'   The data can be filtered by date and/or by a variable. \code{filter_date} 
#'   specifies the type of date filter to apply--by date-range or by period. 
#'   \code{date_value} should contain the values to filter the data by. To filter 
#'   by a variable, enter its name as a string in \code{filter_by} and include the 
#'   values to filter by in \code{filter_value}. Up to two grouping variables can 
#'   be entered. Grouping variables can be merged into one variable using \code{combine}; 
#'   in this case any number of variables can be joined, but no more than three is 
#'   recommended. For faceting, any variable (including ones listed in \code{group}) 
#'   can be used, but "year", "month", "week" are also available provided a date 
#'   variable is added to \code{sub_date}. Currently, combined variables cannot be 
#'   faceted. A list containing a table and plot are printed to the console and 
#'   viewer by default.  
#' @examples 
#' \dontrun{
#' species_catch('pollockMainDataTable', species = c('HAUL_LBS_270_POLLOCK_LBS', 
#' 'HAUL_LBS_110_PACIFIC_COD_LBS', 'HAUL_LBS_OTHER_LBS'), date = 'HAUL_DATE', 
#' value = 'count', period = 'month_num', output = 'plot', year = 2011, convert_to_tons = TRUE)
#' }
#' @export species_catch
#' @import ggplot2
#' @importFrom stats reformulate
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom rlang expr sym
#' @importFrom scales label_percent breaks_extended
#' @importFrom shiny isRunning

species_catch <- function(dat, project, species, date = NULL, period = NULL, fun = "sum", 
                          group = NULL, sub_date = NULL, filter_date = NULL, date_value = NULL, 
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
  
  end <- FALSE 
  
  if (!is.null(period)) {
    if(period == "no_period") {
      period <- NULL
    }
  }
  
  group_date <- group[group %in% c("year", "month", "week")]
  facet_date <- facet_by[facet_by %in% c("year", "month", "week")]
  
  # date ----
  # convert date and/or sub_date to date class
  if (!is.null(date) | !is.null(sub_date)) {
    
    dataset[unique(c(date, sub_date))] <- 
      lapply(dataset[unique(c(date, sub_date))], function(x) {
        
        if (any(!(class(x) %in% c("Date", "POSIXct", "POSIXt")))) {
          date_parser(x)
        } else {
          x
        }
      })
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
  
  # filter date ----
  if (!is.null(filter_date)) {
    
    dataset <- subset_date(dataset, sub_date, filter_date, date_value)
    
    if (nrow(dataset) == 0) {
      
      warning("Filtered data table has zero rows. Check filter parameters.")
      end <- TRUE
    }
  }
  
  # facet date ----
  if (!is.null(facet_by)) {
    if (length(facet_date) > 0) {
      # if summarizing over period
      if (!is.null(period)) {
        
        if (period != "month" & any("month" %in% facet_date)) {
          
          dataset$month <- factor(format(dataset[[sub_date]], "%b"), 
                                  levels = month.abb, ordered = TRUE)
          
        } else if (period != "week" & any("week" %in% facet_date)) {
          
          dataset$week <- as.integer(format(dataset[[sub_date]], "%U"))
        }
        
      } else {
        # if not summarizing over period
        dataset[facet_date] <- lapply(facet_date, function(x) {
          fp <- switch(x, "year" = "%Y", "month" = "%b", "week" = "%U")
          if (fp == "%b") {
            factor(format(dataset[[sub_date]], fp), levels = month.abb, ordered = TRUE) 
          } else {
            as.integer(format(dataset[[sub_date]], fp))
          }
        })
      }
    }
  }
  
  # group date ----
  if (!is.null(group)) {
    
    if (length(group_date) > 0) {
      
      if (length(group_date[!(group_date %in% facet_date)]) > 0) {
        
        for (i in group_date) {
          x <- switch(i, "year" = "%Y", "month" = "%b", "week" = "%U")
          
          dataset[[i]] <- format(dataset[[sub_date]], x)
          
          if (i == "month") {
            dataset[[i]] <- factor(dataset[[i]], levels = month.abb, ordered = TRUE)
          }
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
  # period ----
  if (!is.null(period)) {
    
    if (is.null(date)) warning("Please enter a date variable.")
    
    periods <- c("year_month", "month_year", "year", "month", "weeks", 
                 "weekday", "day_of_month", "day_of_year", "cal_date")
    
    if (period %in% periods == FALSE) {
      
      warning("Invalid period. Please select a valid period name (see documentation for details).")
      end <- TRUE
      
    } else {
      
      p <- switch(period, year_month = "%Y-%m", month_year = "%Y-%m", year = "%Y",
                  month = "%b", weeks = "%U", weekday = "%a", day_of_month = "%d", 
                  day_of_year = "%j", cal_date = NULL)
    }
  }

  # add missing ----
  if ("species" %in% facet_by) {
    
    facet <- facet_by[facet_by != "species"]
    
    if (length(facet) == 0) {
      facet <- NULL
    }
  } else {
    
    facet <- facet_by
  }

  dataset <- add_missing_dates(dataset, date = date, sub_date = sub_date, 
                               value = species, group = group, facet_by = facet)
  
  if (!is.null(period)) {
    if (period != "cal_date") {
      dataset[[period]] <- format(dataset[[date]], p)
    }
  }
  
  # group ----
  if (!is.null(group)) {
    
    if (combine == TRUE & length(group) > 1) { 
      
      dataset <- ID_var(dataset, vars = group, type = "string")
      group <- gsub(" ", "", paste(group, collapse = "_"))
      group1 <- group
      group2 <- NULL
      
    } else {
      
      dataset[group] <- lapply(dataset[group], as.factor)
      group1 <- group[1]
    }
    
    if (length(group) == 1) group2 <- NULL else group2 <- group[2]
    
    if (length(group) > 2) {
      
      warning("Only the first two grouping variables will be displayed in plot.")
    }
  }
  
  if (end == FALSE) {
    
    # summary table ----
    agg_grp <- c(group, facet, facet_date)
    
    if (!is.null(period)) {
      
      if (period == "cal_date") period <- date
    }
    
    table_out <- agg_helper(dataset, value = species, period = period, group = agg_grp, fun = fun)
    
    
    if (length(species) > 1) { # melt table if multiple species columns entered (for ggplot)
      
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
    
    if (!is.null(period)) {
      if (!is.null(p)) {# if period != "cal_date"
        # convert period to ordered factor/integer
        if (p %in% c("%Y-%m", "%a", "%b")) {
          
          table_out <- date_factorize(table_out, period, p)
          
        } else {
          
          table_out[[period]] <- as.integer(table_out[[period]])
        }
      }
      
      table_out <- table_out[order(table_out[[period]]), ]
    }
    
    f_catch <- function() if (length(species) == 1) species else "catch"
    
    if (conv != "none") {
      
      if (conv == "tons") {
        
        table_out[f_catch()] <- table_out[f_catch()]/2000
        
      } else if (conv == "metric_tons") {
        
        table_out[f_catch()] <- table_out[f_catch()]/2204.62
        
      } else if (is.function(conv)) {
        
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
    # table_out <- as.data.frame(table_out)
    # row.names(table_out) <- 1:nrow(table_out)
    
    if (output %in% c("plot", "tab_plot")) {
      
      # plot functions ----
      
      catch_exp <- function() if (length(species) == 1) rlang::sym(species) else rlang::sym("catch")
      
      xaxis_exp <- function() {
        if (!is.null(period)) { 
          
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
      
      if (!is.null(period)) {
        
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
      
      x_lab <- function() {
        
        if (!is.null(period)) {
          
          p_lab <- switch(period, "year_month" = "year-month", "month_year" = "month-year",
                          "year" = "year", "month" = "month", "weeks" = "weeks", "day_of_month" = "day of the month",
                          "day_of_year" = "day of the year")
          
          if (period != date) {
            paste0(date, " (", p_lab, ")")
          } else {
            date
          }
        } else if (is.null(group) & length(species) == 1) {
          NULL
        } else {
          rlang::as_string(xaxis_exp())
        }
      }  
        
      y_lab <- function() paste(fun, f_catch(), ifelse(tran == "identity", "", paste0("(", tran, ")")))
      
      scale_lab <- function() if (value == "percent") scales::label_percent(scale = 1) else ggplot2::waiver()
      
      y_breaks <- function() {
        if (tran != "identity") {
          scales::breaks_extended(n = 7) 
        } else {
          ggplot2::waiver()
        }
      }
      
      # plot ----
      s_plot <- ggplot2::ggplot(data = table_out, ggplot2::aes(x = !!xaxis_exp(), y = !!catch_exp())) +
        fishset_theme() +
        ggplot2::theme(legend.position = "bottom") +
        ggplot2::scale_y_continuous(labels = scale_lab(), trans = tran, breaks = y_breaks())
      
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
      
      if (!is.null(facet_by)) {
        
        if (length(facet_by) == 1) {
          
          fm <- stats::reformulate(".", facet_by)
          
        } else if (length(facet_by) == 2) {
          
          fm <- paste(facet_by, sep = " ~ ")
        }
        if (is.null(period)) {
          s_plot <- s_plot + ggplot2::facet_wrap(fm, scales = scale)
        } else {
          s_plot <- s_plot + ggplot2::facet_grid(fm, scales = scale)
        }
      }
      
      if (!is.null(period)) {
        if (!is.null(p)) {# if period != "cal_date"
          if (!(p %in% c("%a", "%b", "%Y-%m"))) {
            
            s_plot <- s_plot + ggplot2::scale_x_continuous(breaks = num_breaks(table_out[[period]]))
          
            } else if (period == "month_year") {
            
            d_labs <- levels(table_out$month_year)
            d_labs <- format(as.Date(paste0(d_labs, "-01")), "%b %y")
            s_plot <- s_plot + ggplot2::scale_x_discrete(labels = d_labs)
          }
        }
      }
      
      # remove legend if
      if (is.null(period) & is.null(group) & length(species) > 1) {
        
        s_plot <- s_plot + theme(legend.position = "none")
      }
      
      # add labels
      s_plot <- s_plot  + ggplot2::labs(x = x_lab(), y = y_lab())
      
      save_plot(project, "species_catch")
    }
    
    if (!is.null(period)) {
      if (period == "month_year") {
        table_out$month_year <- format(as.Date(paste0(table_out$month_year, "-01")), "%b %y")
      }
    }
    
    # Log function ----
    species_catch_function <- list()
    species_catch_function$functionID <- "species_catch"
    species_catch_function$args <- list(dat, project, species, date, period, fun, group, 
                                        sub_date, filter_date, date_value, filter_by, 
                                        filter_value, filter_expr, facet_by, type, 
                                        conv, tran, value, position, combine, scale, 
                                        output, format_tab)
    log_call(species_catch_function)
    
    if (format_tab == "wide") {
      
      if (length(species) > 1) {
        
        table_out <- tidyr::pivot_wider(table_out, names_from = species, values_from = catch)
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
}
