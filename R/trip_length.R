# Trip length
#'
#'  Display trip duration and value per unit effort
#'
#' @param dat Primary data containing information on hauls or trips. Table in FishSET
#'   database should contain the string `MainDataTable`.
#' @param project String, name of project.
#' @param start Date variable containing the start of vessel trip.
#' @param end Date variable containing the end of vessel trip.
#' @param units Time unit, defaults to \code{"days"}. Options include \code{"secs"},
#'   \code{"mins"}, \code{"hours"}, \code{"days"}, or \code{"weeks"}.
#' @param vpue Optional, numeric variable in \code{dat} for calculating value per 
#'   unit effort (VPUE).
#' @param group Optional, string names of variables to group by. By default, 
#'   grouping variables are combined unless \code{combine = FALSE} and 
#'   \code{type = "freq_poly"} (frequency polygon). \code{combine = TRUE} will not 
#'   work when \code{type = "hist"} (histogram). Frequency polygon plots can use up 
#'   to two grouping variables if \code{combine = FALSE}: the first variable is 
#'   assigned to the "color" aesthetic and second to the "linetype" aesthetic. 
#' @param combine Logical, whether to combine the variables listed in \code{group} 
#'   for plot. 
#' @param haul_count Logical, whether to include hauls per trip in table and/or plot 
#'   (this can only be used if collapsing data to trip level using \code{tripID}. If 
#'   data is already at trip level, add your haul frequency variable to \code{vpue}).
#' @param sub_date Date variable used for subsetting, grouping, or splitting by date.
#' @param filter_date The type of filter to apply to `MainDataTable`. To filter by a 
#'   range of dates, use \code{filter_date = "date_range"}. To filter by a given
#'   period, use "year-day", "year-week", "year-month", "year", "month", "week", 
#'   or "day". The argument \code{date_value} must be provided. 
#' @param date_value This argument is paired with \code{filter_date}. If 
#'   \code{filter_date = "date_range"}, enter a string containing a start- and 
#'   end-date, e.g. \code{date_value = c("2011-01-01", "2011-03-15")}. If filtering 
#'   by period (e.g. "year", "year-month"), use integers (4 digits if year, 1-2 
#'   digits if referencing a day, month, or week). Use a list if using a year-period 
#'   type filter, e.g. "year-week", with the format: \code{list(year, period)}. 
#'   Use a vector if using a single period (e.g. "month"): \code{c(period)}. For 
#'   example, \code{date_value = list(2011:2013, 5:7)} will filter the data table 
#'   from May through July for years 2011-2013 if \code{filter_date = "year-month"}.
#'   \code{date_value = c(2:5)} will filter the data from February through May when 
#'   \code{filter_date = "month"}.
#' @param filter_by String, variable name to filter `MainDataTable` by. the argument 
#'   \code{filter_value} must be provided.
#' @param filter_value A vector of values to filter `MainDataTable` by using the 
#'   variable in \code{filter_by}. For example, if \code{filter_by = "GEAR_TYPE"}, 
#'   \code{filter_value = 1} will include only observations with a gear type of 1. 
#' @param filter_expr String, a valid R expression to filter `MainDataTable` by. 
#' @param facet_by Variable name to facet by. Facetting by \code{"year"}, \code{"month"}, 
#' or \code{"week"} provided a date variable is added to \code{sub_date}.
#' @param type The type of plot. Options include histogram (\code{"hist"}, the 
#'   default) and frequency polygon (\code{"freq_poly"}).
#' @param bins The number of bins used in histogram/freqency polygon. 
#' @param density Logical, whether densities or frequencies are used for histogram. 
#'   Defaults  to \code{TRUE}. 
#' @param scale Scale argument passed to \code{\link{facet_grid}}. Defaults to 
#'   \code{"fixed"}. Other options include \code{"free_y"}, \code{"free_x"}, and 
#'   \code{"free_xy"}.
#' @param tran Transformation to be applied to the x-axis. A few options include 
#'   \code{"log"}, \code{"log10"}, and \code{"sqrt"}. See 
#'   \code{\link[ggplot2]{scale_continuous}} for a complete list.
#' @param pages Whether to output plots on a single page (\code{"single"}, the 
#'   default) or multiple pages (\code{"multi"}).
#' @param remove_neg Logical, whether to remove negative trip durations from the 
#'   plot and table. 
#' @param output Options include 'table', 'plot', or 'tab_plot' (both table and 
#'   plot, the default).
#' @param tripID Column(s) that identify the individual trip.
#' @param fun.time How to collapse temporal data. For example, \code{min}, 
#'   \code{mean}, \code{max}. Cannot be \code{sum} for temporal variables.
#' @param fun.numeric How to collapse numeric or temporal data. For example, 
#'   \code{min}, \code{mean}, \code{max}, \code{sum}. Defaults to \code{mean}.
#' @return \code{trip_length()} calculates vessel trip duration given a start and end date,
#'   converts trip duration to the desired unit of time (e.g. weeks, days, or hours),
#'   and returns a table and/or plot. There is an option for calculating vpue (value 
#'   per unit of effort) as well. The data can be filtered by date and/or by a variable.
#'   \code{filter_date} specifies the type of date filter to apply--by date-range or by
#'   period. \code{date_value} should contain the values to filter the data by. To filter 
#'   by a variable, enter its name as a string in \code{filter_by} and include the values 
#'   to filter by in \code{filter_value}. If multiple grouping variables are given then 
#'   they are combined into one variable  unless \code{combine = FALSE} and 
#'   \code{type = "freq_poly"}. No more than three grouping variables is recommended 
#'   if \code{pages = "single"}. Any variable in the dataset can be used for faceting, 
#'   but "year", "month", and "week" are also available. Distribution plots can be 
#'   combined on a single page or printed individually with \code{pages}.
#' @export trip_length
#' @seealso \code{\link{haul_to_trip}}
#' @examples
#' \dontrun{
#' trip_length(pollockMainDataTable,
#'   start = "FISHING_START_DATE", end = "HAUL_DATE",
#'   units = "days", vpue = "OFFICIAL_TOTAL_CATCH", output = "plot",
#'   tripID = c("PERMIT", "TRIP_SEQ"), fun.numeric = sum, fun.time = min
#' )
#' 
#' }
#' @importFrom lubridate is.Date is.POSIXt
#' @importFrom stats reformulate
#' @importFrom gridExtra arrangeGrob grid.arrange marrangeGrob
#' @importFrom rlang sym 
#' @importFrom dplyr anti_join
#' @importFrom tidyr pivot_wider
#' @importFrom shiny isRunning
#' @import ggplot2

trip_length <- function(dat, project, start, end, units = "days", vpue = NULL,
                        group = NULL, combine = TRUE, haul_count = TRUE, sub_date = NULL,
                        filter_date = NULL, date_value = NULL, filter_by = NULL, 
                        filter_value = NULL, filter_expr = NULL, facet_by = NULL, 
                        type = "hist", bins = 30, density = TRUE, scale = "fixed", 
                        tran = "identity", pages = "single", remove_neg = FALSE,
                        output = "tab_plot", tripID = NULL, fun.time = NULL, 
                        fun.numeric = NULL) {
  out <- data_pull(dat)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main")
  
  end <- FALSE
  
  not_num <- vapply(dataset[vpue], function(x) !is.numeric(x), logical(1))
  
  if (any(not_num)) {
    
    warning("'vpue' must be numeric.")
    end <- TRUE
  }
  
  if (any(units %in% c("secs", "minutes", "hours", "days", "weeks")) == FALSE) {
    
    warning('Invalid unit. Choices include "secs", "minutes", "hours", "days",',
    'and "weeks".')
  }
  
  # convert hauls to trips
  if (!is.null(tripID)) {
    dataset <- haul_to_trip(dataset, project = project, tripID = tripID, 
                            fun.numeric = fun.numeric, fun.time = fun.time)
  }
  
  # cleaning dates ----
  dataset <- date_check(dataset, start)
  dataset <- date_check(dataset, end)
  
  if (any(class(dataset[[start]]) != class(dataset[[end]]))) {
    warning("Start and end date variables have different classes. Recommend", 
            "converting date variables to same class.")
  }
  
  if (anyNA(dataset[[start]]) | anyNA(dataset[[end]])) {
    warning("NAs detected in dates.")
  }
  
  # sub_date ----
  # convert date and/or sub_date to date class
  if (!is.null(sub_date)) {
    
    dataset[sub_date] <- lapply(dataset[sub_date], date_parser)
  } 
  
  # check if sub_date is needed
  if (!is.null(filter_date)) {
    if (is.null(sub_date)) {
      if (!is.null(start)) sub_date <- start
      else {
        warning("Argument 'sub_date' required when subsetting by date.")
        end <- TRUE
      }
    }
  }
  
  if (!is.null(facet_by)) {
    if (any(facet_by %in% c("year", "month", "week"))) {
      if (is.null(sub_date)) {
        if (!is.null(start)) sub_date <- start
        else {
          warning("Spliting by year requires 'sub_date' argument.")
          end <- TRUE
        }
      }
    } 
  }
  
  if (!is.null(group)) {
    if (any(group %in% c("year", "month", "week"))) {
      if (is.null(sub_date)) {
        if (!is.null(date)) sub_date <- date
        else {
          warning("Grouping by year requires 'sub_date' argument.")
          end <- TRUE
        }
      }
    } 
  }
  
  # filter by date ----
  if (!is.null(filter_date)) {
    
    dataset <- subset_date(dataset, start, filter_date, date_value)
    
    if (nrow(dataset) == 0) {
      
      warning("Filtered data table has zero rows. Check filter parameters.")
      end <- TRUE
    }
  }
  
  # filter by variable/expression ----
  if (!is.null(filter_value) | !is.null(filter_expr)) {
    
    dataset <- subset_var(dataset, filter_by, filter_value, filter_expr)
    
    if (nrow(dataset) == 0) {
      
      warning("Filtered data table has zero rows. Check filter parameters.")
      end <- TRUE
    }
  }
  
  if (end == FALSE) {
    
    # calculate trip duration ----
    if (lubridate::is.POSIXt(dataset[[start]]) | lubridate::is.POSIXt(dataset[[end]])) {
      trip <- round(as.numeric(difftime(dataset[[end]], dataset[[start]]), 
                               units = units), 3)
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
    
    # add hauls per trip 
    if (haul_count & !is.null(tripID)) vpue <- c(vpue, "HAUL_COUNT")
    
    # calculate vpue ----
    if (!is.null(vpue)) {
      
      vpue_nm <- vapply(vpue, FUN = function(x) paste(x, "vpue", sep = "_"), 
                        FUN.VALUE = "character")
      
      trip_tab[vpue_nm] <- lapply(vpue, function(x) {
        round((dataset[[x]] / trip_tab[[t_nm]]), 3)
      })
      
      if (any(vapply(trip[vpue_nm], FUN = is.infinite, FUN.VALUE = logical(1)))) {
        warning("Inf values produced for vpue.")
      }
      
      if (any(vapply(trip[vpue_nm], FUN = is.nan, FUN.VALUE = logical(1)))) {
        warning("NaN values produced for vpue.")
      }
    } else vpue_nm <- NULL
    
    # add sub_date to trip_tab
    if (!is.null(sub_date)) trip_tab[[sub_date]] <- dataset[[sub_date]]
    
    # facet date ----
    facet_date <- facet_by[facet_by %in% c("year", "month", "week")]
    # add non-function-created facet vars to trip_tab
    trip_tab[facet_by[!(facet_by %in% facet_date)]] <- 
      dataset[facet_by[!(facet_by %in% facet_date)]]
    
   
    
    # group date ----
    group_date <- group[group %in% c("year", "month", "week")]
    
    # add non-function-created group variables to trip_tab
    trip_tab[group[!(group %in% group_date)]]  <- 
      dataset[group[!(group %in% group_date)]]
    
    trip_tab <- facet_period(trip_tab, facet_date = unique(c(facet_date, group_date)), 
                             date = sub_date)
    
    if (!is.null(group)) {
      
      if (length(group) > 1) {
        
        if (type == "freq_poly" & combine == FALSE) {
          
          group1 <- group[1] 
          group2 <- group[2]
          
          trip_tab[group] <- lapply(trip_tab[group], as.factor)
          
        } else {
          
          trip_tab <- ID_var(trip_tab, vars = group, drop = TRUE)
          group <- paste(group, collapse = "_")
        }
        
      } else {
        group_nd <- group[!(group %in% group_date)]
        
        if (length(group_nd) > 0) {
          trip_tab[[group_nd]] <- as.factor(dataset[[group_nd]])
        }
      }
    }
    # remove negative trip durations
    if (remove_neg) trip_tab <- trip_tab[trip_tab[[t_nm]] >= 0, ]
    
    # columns names for trip length and vpue(s)
    axis_name <- c(t_nm, vpue_nm)
    p_nm <- unname(axis_name)
    names(axis_name)[1] <- p_nm[1]
    p_type <- function() if (density) "density" else "frequency"
    
    grp_fct <- c(group, facet_by)
    
    # table output ----
    if (output %in% c("tab_plot", "table")) {
      
      table_out <- nfreq_table(trip_tab, var = p_nm, group = c(group, facet_by),
                               bins = bins, type = if (density) "dens" else "freq",
                               format_tab = "long")
      
      # check_confidentiality ----
      if (run_confid_check()) {
        
        cc_par <- get_confid_check()
        
        if (cc_par$rule == "n") {
          
          trip_tab[[cc_par$v_id]] <- dataset[[cc_par$v_id]]
          
          check_out <- nfreq_table(trip_tab, var = p_nm, group = c(grp_fct, cc_par$v_id),
                                   bins = bins, type = "freq", format_tab = "long")
          # iterate over list of tables 
          
          if (is.null(vpue)) { # just trip length
            
            check_out <-
              check_confidentiality(check_out, cc_par$v_id, value_var = "frequency", 
                                    group = c(t_nm, grp_fct), rule = "n", 
                                    value = cc_par$value)
            
            if (check_out$suppress) {
              check_out <- suppress_table(check_out$table, table_out, 
                                          value_var = p_type(),
                                          group = c(t_nm, grp_fct), 
                                          rule = "n")
            }
            
          } else { # vpue
            
            check_out <- 
              lapply(check_out, function(x) {
                
                check_confidentiality(x, cc_par$v_id, value_var = "frequency", 
                                      group = c(names(x)[1], grp_fct), rule = "n", 
                                      value = cc_par$value)
              })
            
            check_out <-
              lapply(names(check_out), function(x) {
                
                supr <- check_out[[x]]$suppress
                
                if (supr) {
                  
                  suppress_table(check_out[[x]]$table, table_out[[x]], value_var = p_type(),
                                 group = c(x, grp_fct), rule = "n")
                }
              })
            
            names(check_out) <- p_nm
            c_ind <- vapply(check_out, function(x) !is.null(x), logical(1))
            
            if (any(c_ind)) {
              
              if (!is.null(grp_fct)) {
                
                check_out <-
                  lapply(names(check_out[c_ind]), function(x) {
                    
                    tidyr::pivot_wider(check_out[[x]], id_cols = !!x,
                                       names_from = !!grp_fct, values_from = !!p_type(), 
                                       names_repair = "unique")
                  })
                
              }
              # save to output folder
              lapply(seq_along(check_out), function(x) {
                save_table(check_out[[x]], project, paste0("trip_length_confid_", x))
              })
            }
          }
        }
      }
      
      # save output table
      if (is.null(vpue) & is.null(tripID)) {
        
        if (!is.null(grp_fct)) {
          
          table_out <- tidyr::pivot_wider(table_out, id_cols = !!t_nm,
                                          names_from = !!grp_fct,
                                          values_from = !!p_type(), 
                                          names_repair = "unique")
        }
        
        save_table(table_out, project, "trip_length")  
        
      } else {
        
        if (!is.null(grp_fct)) {
          
          table_out <-
            lapply(names(table_out), function(x) {
              
              tidyr::pivot_wider(table_out[[x]], id_cols = !!x, names_from = !!grp_fct,
                                 values_from = !!p_type(), names_repair = "unique")
            })
        }
        
        lapply(seq_along(table_out), function(x) {
          save_table(table_out[[x]], project, paste0("trip_length_", x))
        })
      }
      
    }
    
    # plot section ----
    if (output %in% c("tab_plot", "plot")) {
      
      group_list <- list(group = group,
                         group1 = get0("group1"),
                         group2 = get0("group2"))
      
      t_plot <- 
        trip_length_plot(trip_tab, trp_nms = axis_name, vpue, group = group_list, 
                         facet_by, units, type, dens = density, bins, tran, scale,
                         combine, pages)
      
      if (run_confid_check()) {
        
        if (cc_par$rule == "n") {
          
          if (is.null(vpue)) {
            
            check_out <- tibble::as_tibble(check_out)
            
            p_brks <- pretty(range(trip_tab[[t_nm]]), 
                             n = bins, min.n = 1)
            # add breaks to trip_tab
            trip_tab$breaks <- cut(trip_tab[[t_nm]], breaks = p_brks, 
                                   include.lowest = TRUE, right = TRUE)
            
            check_out$breaks <- levels(trip_tab$breaks)
            # find which bins to suppress
            check_out <- check_out[check_out[[p_type()]] == -999, c("breaks", grp_fct)]
            
            trip_tab <- 
              suppress_table(check_out, trip_tab, t_nm, 
                             group = c("breaks", grp_fct), rule = "n", type = "plot")
            
            # trip_tab <- trip_tab[!is.na(trip_tab[[t_nm]]), ]
            
            check_plot <-
              trip_length_plot(trip_tab, trp_nms = t_nm, vpue, group = group_list, 
                               facet_by, units, type, dens = density, bins, tran, scale,
                               combine, pages)
            
          } else { # with vpue
            
            brk_cols <- paste0(p_nm, "_breaks")
            trip_tab[brk_cols] <- lapply(p_nm, function(x) {
              
              var <- trip_tab[[x]][is.finite(trip_tab[[x]])]
              p_brks <- pretty(range(var), n = bins, min.n = 1)
              # add breaks to trip_tab
              cut(trip_tab[[x]], breaks = p_brks, 
                  include.lowest = TRUE, right = TRUE)
            })
            
            check_out <- 
              lapply(seq_along(check_out), function(i) {
                check_out[[i]] <- tibble::as_tibble(check_out[[i]])
                check_out[[i]][[brk_cols[i]]] <- levels(trip_tab[[brk_cols[i]]])
                check_out[[i]][check_out[[i]][p_type()] == -999, c(brk_cols[i], grp_fct)]
              })
            names(check_out) <- p_nm
            
            trip_tab[p_nm] <- 
              lapply(seq_along(check_out), function(i) {
                
                suppress_table(check_out[[i]], trip_tab, p_nm[[i]], 
                               group = c(brk_cols[i], grp_fct), type = "plot",
                               rule = "n", as_vector = TRUE)
              })
            
            check_plot <-
              trip_length_plot(trip_tab, trp_nms = p_nm, vpue, group = group_list, 
                               facet_by, units, type, dens = density, bins, tran, scale,
                               combine, pages)
          }
        }
      }
      
      f_plot <- function() {
        
        if (is.null(vpue)) t_plot$single
        
        else {
          if (pages == "multi") {
            if (shiny::isRunning()) t_plot$multi
            else t_plot$single
            
          } else {
            if (shiny::isRunning()) t_plot$single
            else gridExtra::grid.arrange(t_plot$single)
          }
        }
      }
      
      # save plot(s) 
      if (pages == "multi" & !is.null(vpue)) {
        
        lapply(seq_along(t_plot$multi), function(x) {
          save_plot(project, paste0("trip_length_", x), t_plot$multi[[x]])
        })
        
      } else save_plot(project, "trip_length", t_plot$single)
      
      if (run_confid_check()) {
        
        if (cc_par$rule == "n") {
          
          if (exists("check_plot")) {
            
            if (pages == "multi" & !is.null(vpue)) {
              
              lapply(seq_along(check_plot$multi), function(x) {
                save_plot(project, paste0("trip_length_confid_", x), check_plot$multi[[x]])
              })
              
            } else save_plot(project, "trip_length_confid", check_plot$single)
            
          }
        }
      }
    }
    
    # Log function
    trip_length_function <- list()
    trip_length_function$functionID <- "trip_length"
    trip_length_function$args <- list(dat, project, start, end, units, vpue,
                                      group, combine, haul_count, sub_date, filter_date, 
                                      date_value, filter_by, filter_value, filter_expr,
                                      facet_by, type, bins, density, scale, tran, pages, 
                                      remove_neg, output, tripID, fun.time, fun.numeric)
    log_call(trip_length_function)
    
    if (output == "table") table_out
    else if (output == "plot") f_plot()
    else if (output == "tab_plot") {
      
      out_list <- list(table = table_out, plot = f_plot())
      out_list
    }
  }
}
  
 


trip_length_plot <- function(trip_tab, trp_nms, vpue, group, facet_by, units, type, 
                             dens, bins, tran, scale, combine, pages) {
  #' Trip length plot helper
  #' 
  #' Creates and formats plots for \code{trip_length}.
  #' 
  #' @param trip_tab Dataframe passed on from \code{trip_length}.
  #' @param trp_nms Column names of trip length and vpue variables. 
  #' @param vpue Column names of vpue variable(s).
  #' @param group Column names of grouping variable(s).
  #' @param facet_by Column names of facet variable(s).
  #' @param units Units of trip length.
  #' @param type Whether histogram or frequency polygon is plotted.
  #' @param dens Logical, whether to calculate density \code{TRUE} or frequency
  #'   \code{FALSE}.
  #' @param bins Numeric, the number of bins to sort variables by.
  #' @param tran Transformation to be applied to the x-axis. 
  #' @param scale Scale argument passed to \code{\link{facet_grid}}.
  #' @param combine Logical, whether grouping variables should be combined. 
  #' @param pages Whether to output plots on a single page (\code{"single"}, the 
  #'   default) or multiple pages (\code{"multi"}).
  #' @import ggplot2
  #' @importFrom rlang sym
  #' @importFrom stats reformulate
  #' @importFrom gridExtra arrangeGrob marrangeGrob
  #' @importFrom shiny isRunning
  #' @keywords internal
  #' @export
 
  group1 <- group$group1
  group2 <- group$group2
  group <- group$group
  
  unit_print <- switch(units, "secs" = "sec", "minutes" = "minute", 
                       "hours" = "hour", "days" = "day", "weeks" = "week")
  # plot functions ----
  group_exp <- function() {
    if (!is.null(group)) rlang::sym(group) 
    else NULL
  }
  
  # color aes
  color_exp <- function() {
    
    if (length(group) > 1 & type == "freq_poly" & combine == FALSE) {
      rlang::sym(group1)
    } else group_exp()
  }
  
  #line type aes 
  line_type_exp <- function() {
    
    if (length(group) > 1 & type == "freq_poly" & combine == FALSE) {
      rlang::sym(group2)
    } else NULL
  }
  
  trip_lab <- function() paste0("trip length ", "(", units, ")")
  # single plot (just durations)
  if (is.null(vpue)) { 
    
    # convert bins to breaks
    p_brks <- pretty(range(trip_tab[[trp_nms[1]]]), n = bins, min.n = 1)
  
    if (dens == FALSE) {
      t_plot <- ggplot2::ggplot(trip_tab, ggplot2::aes(x = !!rlang::sym(trp_nms[1])))
      
    } else {
      t_plot <- 
        ggplot2::ggplot(trip_tab, ggplot2::aes(x = !!rlang::sym(trp_nms[1]), 
                                               y = ggplot2::after_stat(density)))
    }
    
    t_plot <- t_plot + ggplot2::labs(x = trip_lab()) +
      fishset_theme() + ggplot2::scale_x_continuous(trans = tran)
    
    if (type == "hist") {
      t_plot <- t_plot + ggplot2::geom_histogram(ggplot2::aes(fill = !!group_exp()),
                                                 color = "black", breaks = p_brks,
                                                 na.rm = TRUE)
    } else if (type == "freq_poly") {
      t_plot <- 
        t_plot + ggplot2::geom_freqpoly(ggplot2::aes(color = !!color_exp(),
                                                     linetype = !!line_type_exp()),
                                        breaks = p_brks, size = 1, na.rm = TRUE)
    }
    
    if (!is.null(facet_by)) {
      if (length(facet_by) == 1) fm <- stats::reformulate(".", facet_by)
      else if (length(facet_by) == 2) fm <- paste(facet_by, sep = " ~ ")
      
      t_plot <- t_plot + ggplot2::facet_grid(fm, scales = scale)
    }
    # if vpue entered (multiple plots)
  } else {
    
    axis_lab <- function(x) {
      if (x == trp_nms[1]) trip_lab()
      else {
        paste0(names(trp_nms[trp_nms == x]), " per ", unit_print)
      }
    }
    
    plot_list <- lapply(trp_nms, function(p) {
      
      p_brks <- pretty(range(trip_tab[[p]]), n = bins, min.n = 1)
      
      if (dens == FALSE) {
        h_plot <- ggplot2::ggplot(trip_tab, ggplot2::aes(x = !!rlang::sym(p)))
      } else {
        h_plot <- 
          ggplot2::ggplot(trip_tab, ggplot2::aes(x = !!rlang::sym(p), 
                                                 y = ggplot2::after_stat(density)))
      }
      
      h_plot <- h_plot + ggplot2::labs(title = p, x = axis_lab(p), 
                                       caption = paste("bins:", bins)) +
        fishset_theme() + ggplot2::scale_x_continuous(trans = tran)
      
      if (type == "hist") {
        h_plot <- h_plot + ggplot2::geom_histogram(ggplot2::aes(fill = !!group_exp()), 
                                                   color = "black", breaks = p_brks,
                                                   na.rm = TRUE)
      } else if (type == "freq_poly") {
        h_plot <- 
          h_plot + ggplot2::geom_freqpoly(ggplot2::aes(color = !!color_exp(),
                                                       linetype = !!line_type_exp()),
                                          breaks = p_brks, size = 1, na.rm = TRUE)
      }
      
      if (pages == "single") {
        h_plot <- h_plot + ggplot2::theme(legend.position = "none")
      }
      
      if (!is.null(facet_by)) {
        if (length(facet_by) == 1) fm <- stats::reformulate(".", facet_by)
        else if (length(facet_by) == 2) fm <- paste(facet_by, sep = " ~ ")
        
        h_plot <- h_plot + ggplot2::facet_grid(fm, scales = scale)
      }
      
      h_plot
    })
    
    if (pages == "single") {
      if (!is.null(group)) {
        
        leg.box <- ifelse(length(vpue) == 2, "vertical", "horizontal")
        # add grouping var
        grp <- ggplot2::ggplot(trip_tab, ggplot2::aes(0, 0, color = !!color_exp(),
                                                      linetype = !!line_type_exp())) +
          ggplot2::geom_point() + ggplot2::geom_line() +
          ggplot2::theme(legend.position = "bottom", legend.box = leg.box,
                         legend.direction = leg.box)
        
        # extract legend
        tmp <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(grp))
        leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
        legend <- tmp$grobs[[leg]]
      }
      
      if (length(vpue) == 2) {
        
        if (!is.null(group)) plot_list[["legend"]] <- legend
        
        t_plot <- gridExtra::arrangeGrob(grobs = plot_list, nrow = 2, ncol = 2)
        
      } else if (length(vpue) == 1 | length(vpue) >= 3) {
        
        t_plot <- gridExtra::arrangeGrob(grobs = plot_list)
        
        if (!is.null(group)) {
          t_plot <- gridExtra::arrangeGrob(t_plot, legend, ncol = 1, 
                                           heights = c(.9, .1))
        }
      }
      
    } else if (pages == "multi" & !shiny::isRunning()) {
      
      t_plot <- gridExtra::marrangeGrob(plot_list, nrow = 1, ncol = 1, top = NULL)
    }
  }
  
  list(multi = get0("plot_list"), single = get0("t_plot"))
}