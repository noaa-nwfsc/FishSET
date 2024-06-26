#' Trip duration table and plot
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
#' @param date_value This argument is paired with \code{filter_date}. To filter
#'   by date range, set \code{filter_date = "date_range"} and enter a  start- and 
#'   end-date into \code{date_value} as a string: 
#'   \code{date_value = c("2011-01-01", "2011-03-15")}. 
#'   
#'   To filter by period (e.g. "year", "year-month"), use integers (4 digits if year, 1-2 
#'   digits if referencing a day, month, or week). Use a vector if filtering by 
#'   a single period: \code{date_filter = "month"} and \code{date_value = c(1, 3, 5)}. 
#'   This would filter the data to January, March, and May. 
#'   
#'   Use a list if using a year-period type filter, e.g. "year-week", with the 
#'   format: \code{list(year, period)}. For example, \code{filter_date = "year-month"}
#'   and \code{date_value = list(2011:2013, 5:7)} will filter the data table from 
#'   May through July for years 2011-2013. 
#'   
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
#' @param format_lab Formatting option for x-axis labels. Options include 
#'   \code{"decimal"} or \code{"scientific"}.
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
#' @return \code{trip_dur_out()} calculates vessel trip duration given a start and end date,
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
#' @export 
#' @seealso \code{\link{haul_to_trip}}
#' @examples
#' \dontrun{
#' trip_dur_out(pollockMainDataTable,
#'   start = "FISHING_START_DATE", end = "HAUL_DATE",
#'   units = "days", vpue = "OFFICIAL_TOTAL_CATCH", output = "plot",
#'   tripID = c("PERMIT", "TRIP_SEQ"), fun.numeric = sum, fun.time = min
#' )
#' 
#' }
#' @importFrom lubridate is.Date is.POSIXt
#' @importFrom tidyr pivot_wider
#' @importFrom tibble as_tibble
#' @importFrom shiny isRunning
#' @importFrom gridExtra arrangeGrob grid.arrange marrangeGrob

trip_dur_out <- function(dat,
                         project,
                         start,
                         end,
                         units = "days",
                         vpue = NULL,
                         group = NULL,
                         combine = TRUE,
                         haul_count = TRUE,
                         sub_date = NULL,
                         filter_date = NULL,
                         date_value = NULL,
                         filter_by = NULL,
                         filter_value = NULL,
                         filter_expr = NULL,
                         facet_by = NULL,
                         type = "hist",
                         bins = 30,
                         density = TRUE,
                         scale = "fixed",
                         tran = "identity",
                         format_lab = "decimal",
                         pages = "single",
                         remove_neg = FALSE,
                         output = "tab_plot",
                         tripID = NULL,
                         fun.time = NULL,
                         fun.numeric = NULL) {
  
  
  out <- data_pull(dat, project)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main", project)
  
  pers <- c("year", "month", "week")
  group_date <- group[group %in% pers]
  facet_date <- facet_by[facet_by %in% pers]
  facet_no_date <- facet_by[!facet_by %in% pers]
  group_no_date <- group[!group %in% pers]
  
  column_check(dataset, cols = c(vpue, start, end, group_no_date, sub_date, 
                                 filter_by, facet_no_date))
  
  num <- qaqc_helper(dataset[vpue], is.numeric)
  
  if (!all(num)) stop("Argument 'vpue' must be numeric.", call. = FALSE)
  
  
  if (any(units %in% c("secs", "minutes", "hours", "days", "weeks")) == FALSE) {
    
    warning('Invalid unit. Choices include "secs", "minutes", "hours", "days",',
    'and "weeks".')
  }
  
  # convert hauls to trips
  if (!is.null(tripID)) {
    dataset <- haul_to_trip(dataset, project = project, tripID = tripID, 
                            fun.numeric = fun.numeric, fun.time = fun.time, 
                            log_fun = FALSE)
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
  
  # sub_date ----
  # check if sub_date is needed
  sub_date <- sub_date_check(sub_date, date, filter_date, group, facet_by)
  
  # filter date ----
  if (!is.null(filter_date)) {
    
    if (is.null(date_value)) {
      
      stop("'date_value' must be provided.", call. = FALSE)
    }
    
    dataset <- subset_date(dataset, sub_date, filter_date, date_value)
  }
  
  # filter by variable ----
  if (!is.null(filter_by) | !is.null(filter_expr)) {
    
    dataset <- subset_var(dataset, filter_by, filter_value, filter_expr)
  }
   
  # calculate trip duration ----
  if (lubridate::is.POSIXt(dataset[[start]]) | lubridate::is.POSIXt(dataset[[end]])) {
    trip <- round(as.numeric(difftime(dataset[[end]], dataset[[start]]), 
                             units = units), 3)
  } else {
    trip <- as.numeric(dataset[[end]] - dataset[[start]], units = units)
  }
  
  trip_tab <- data.frame(start = dataset[[start]], end = dataset[[end]])
  t_nm <- paste0("trip_duration_", units)
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
    
    if (any(qaqc_helper(trip[vpue_nm], "Inf"))) {
      warning("Inf values produced for vpue.")
    }
    
    if (any(qaqc_helper(trip[vpue_nm], "NaN"))) {
      warning("NaN values produced for vpue.")
    }
  } else vpue_nm <- NULL
  
  # add sub_date to trip_tab
  if (!is.null(sub_date)) trip_tab[[sub_date]] <- dataset[[sub_date]]
  
  # facet date ----
  # add non-function-created facet vars to trip_tab
  trip_tab[facet_no_date] <- dataset[facet_no_date]

  # group date ----
  # add non-function-created group variables to trip_tab
  trip_tab[group_no_date]  <- dataset[group_no_date]
  
  trip_tab <- facet_period(trip_tab, facet_date = unique(c(facet_date, group_date)), 
                           date = sub_date)
  
  if (!is.null(group)) {
    
    if (length(group) > 1) {
      
      if (type == "freq_poly" & combine == FALSE) {
        
        group1 <- group[1] 
        group2 <- group[2]
        
        trip_tab[group] <- lapply(trip_tab[group], as.factor)
        
      } else {
        
        trip_tab <- ID_var(trip_tab, project = project, vars = group, drop = TRUE,
                           log_fun = FALSE)
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
  
  # columns names for trip duration and vpue(s)
  axis_name <- c(t_nm, vpue_nm)
  p_nm <- unname(axis_name)
  names(axis_name)[1] <- p_nm[1]
  p_type <- function() if (density) "density" else "frequency"
  
  grp_fct <- c(group, facet_by)
  
  # table output ----
  if (output %in% c("tab_plot", "table")) {
    
    table_out <- nfreq_table(trip_tab, var = p_nm, group = grp_fct, bins = bins, 
                             type = if (density) "dens" else "freq",
                             format_lab = format_lab, format_tab = "long")
    
    # check_confidentiality ----
    if (run_confid_check(project)) {
      
      cc_par <- get_confid_check(project)
      
      if (cc_par$rule == "n") {
        # add v_id to trip_tab
        trip_tab[[cc_par$v_id]] <- dataset[[cc_par$v_id]]
        #count unique vessels by group and bins
        check_out <- nfreq_table(trip_tab, var = p_nm, group = grp_fct,
                                 bins = bins, type = "freq", v_id = cc_par$v_id,
                                 format_lab = format_lab, format_tab = "long")
        
        if (is.null(vpue)) { # just trip duration
    
          # find rows to suppress
          ind <- check_out[[p_type()]] > 0 & check_out[[p_type()]] < cc_par$value
          
          if (any(ind)) {
  
            # cache check table
            check_table <- check_out[ind, c(p_nm, grp_fct)]
            cache_check_table(check_table, project)
            
            # suppress table
            table_out_c <- suppress_table(check_table, table_out,
                                        value_var = p_type(),
                                        group = c(t_nm, grp_fct),
                                        rule = "n")
            save_table(table_out_c, project, "trip_dur_out_confid")
          }
          
        } else { # vpue
          
          # list of suppression indices
          sup_list <- lapply(check_out, function(x) {
            
            ind <- x[[p_type()]] > 0 & x[[p_type()]] < cc_par$value
          })
          
          check_table <- 
            lapply(seq_along(check_out), function(x) {
              
              check_out[[x]][sup_list[[x]], c(p_nm[[x]], grp_fct)]
            })
          
          table_out_c <- 
            lapply(seq_along(table_out), function(x) {
              
              if (any(sup_list[[x]])) {
                
                suppress_table(check_table[[x]], table_out[[x]], value_var = p_type(),
                               group = c(p_nm[[x]], grp_fct), rule = "n")
              }
            })
          
          names(table_out_c) <- p_nm
          c_ind <- vapply(table_out_c, function(x) !is.null(x), logical(1))
          
          if (any(c_ind)) {
            
            if (!is.null(grp_fct)) {
              
              table_out_c <-
                lapply(names(table_out_c[c_ind]), function(x) {
                  
                  tidyr::pivot_wider(table_out_c[[x]], id_cols = !!x,
                                     names_from = !!grp_fct, values_from = !!p_type(), 
                                     names_repair = "unique")
                })
            }
            # save to output folder
            save_ntable(table_out_c, project, "trip_dur_out_confid_", "num")
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
      
      save_table(table_out, project, "trip_dur_out")  
      
    } else {
      
      if (!is.null(grp_fct)) {
        
        table_out <-
          lapply(names(table_out), function(x) {
            
            tidyr::pivot_wider(table_out[[x]], id_cols = !!x, names_from = !!grp_fct,
                               values_from = !!p_type(), names_repair = "unique")
          })
      }
      
      save_ntable(table_out, project, "trip_dur_out", id = "num")
    }
    
  }
  
  # plot section ----
  if (output %in% c("tab_plot", "plot")) {
    
    group_list <- list(group = group,
                       group1 = get0("group1"),
                       group2 = get0("group2"))
    
    t_plot <- 
      trip_duration_plot(trip_tab, trp_nms = axis_name, vpue, group = group_list, 
                       facet_by, units, type, dens = density, bins, tran, 
                       format_lab, scale, combine, pages)
    
    if (run_confid_check(project)) {
      
      if (cc_par$rule == "n") {
        
        if (is.null(vpue)) {
          # recreate breaks from nfreq_table
          p_brks <- pretty(range(trip_tab[[t_nm]], finite = TRUE), 
                           n = bins, min.n = 1)

          # add breaks to trip_tab
          trip_tab$breaks <- as.character(cut(trip_tab[[t_nm]], 
                                              breaks = p_brks, 
                                              include.lowest = TRUE, right = TRUE))
          # check for confid rows
          check_out_plot <- 
            check_confidentiality(trip_tab, project, v_id = cc_par$v_id, 
                                  value_var = p_nm, group = c("breaks", grp_fct), 
                                  rule = "n", value = cc_par$value)
          # check table for plot
          check_out_plot <- check_out_plot$table[c("breaks", grp_fct)]
          # suppress confid rows for plot
          trip_tab <- 
            suppress_table(check_out_plot, trip_tab, t_nm, group = c("breaks", grp_fct), 
                           rule = "n", type = "NA")
          
          t_plot_c <-
            trip_duration_plot(trip_tab, trp_nms = t_nm, vpue, group = group_list, 
                             facet_by, units, type, dens = density, bins, tran, 
                             format_lab, scale, combine, pages)
          
        } else { # with vpue
          
          brk_cols <- paste0(p_nm, "_breaks")
          trip_tab[brk_cols] <- lapply(p_nm, function(x) {
            
            var <- trip_tab[[x]][is.finite(trip_tab[[x]])]
            p_brks <- pretty(range(var, finite = TRUE), n = bins, min.n = 1)
            # add breaks to trip_tab
            cut(trip_tab[[x]], breaks = p_brks, 
                include.lowest = TRUE, right = TRUE)
          })
          
          check_out_plot <-
            lapply(seq_along(brk_cols), function(x) {
              
              check_out <-
              check_confidentiality(trip_tab, project, v_id = cc_par$v_id, 
                                    value_var = p_nm[x], 
                                    group = c(brk_cols[x], grp_fct), 
                                    rule = "n", value = cc_par$value)
              
              check_out$table[c(brk_cols[x], grp_fct)]
            })
          
          trip_tab[p_nm] <- 
            lapply(seq_along(check_out_plot), function(i) {
              
              suppress_table(check_out_plot[[i]], trip_tab, p_nm[[i]], 
                             group = c(brk_cols[i], grp_fct), type = "NA",
                             rule = "n", as_vector = TRUE)
            })
          
          t_plot_c <-
            trip_duration_plot(trip_tab, trp_nms = p_nm, vpue, group = group_list, 
                             facet_by, units, type, dens = density, bins, tran, 
                             format_lab, scale, combine, pages)
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
      
        save_nplot(project, "trip_dur_out", t_plot$multi, id = "num")
      
    } else save_plot(project, "trip_dur_out", t_plot$single)
    
    if (run_confid_check(project)) {
      
      if (cc_par$rule == "n") {
        
        if (exists("check_out_plot")) {
          
          if (pages == "multi" & !is.null(vpue)) {
            
            save_nplot(project, "trip_dur_out_confid", t_plot_c$multi, id = "num")
            
          } else save_plot(project, "trip_dur_out_confid", t_plot_c$single)
          
        }
      }
    }
  }
  
  # Log function
  trip_dur_out_function <- list()
  trip_dur_out_function$functionID <- "trip_dur_out"
  trip_dur_out_function$args <- list(dat, project, start, end, units, vpue,
                                    group, combine, haul_count, sub_date, filter_date, 
                                    date_value, filter_by, filter_value, filter_expr,
                                    facet_by, type, bins, density, scale, tran, 
                                    format_lab, pages, remove_neg, output, tripID, 
                                    fun.time, fun.numeric)
  log_call(project, trip_dur_out_function)
  
  if (output == "table") table_out
  else if (output == "plot") f_plot()
  else if (output == "tab_plot") {
    
    out_list <- list(table = table_out, plot = f_plot())
    out_list
  }
}
  

trip_duration_plot <- function(trip_tab, trp_nms, vpue, group, facet_by, units, type, 
                             dens, bins, tran, format_lab, scale, combine, pages) {
  #' Trip duration plot helper
  #' 
  #' Creates and formats plots for \code{trip_dur_out}.
  #' 
  #' @param trip_tab Dataframe passed on from \code{trip_dur_out}.
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
  #' @param format_lab Formatting option for x-axis labels. Options include 
  #'   \code{"decimal"} or \code{"scientific"}.
  #' @param scale Scale argument passed to \code{\link{facet_grid}}.
  #' @param combine Logical, whether grouping variables should be combined. 
  #' @param pages Whether to output plots on a single page (\code{"single"}, the 
  #'   default) or multiple pages (\code{"multi"}).
  #' @import ggplot2
  #' @importFrom rlang sym
  #' @importFrom scales label_number label_scientific
  #' @importFrom stats reformulate
  #' @importFrom gridExtra arrangeGrob marrangeGrob
  #' @importFrom shiny isRunning
  #' @keywords internal
  #' @export
 
  group1 <- group$group1
  group2 <- group$group2
  group <- group$group
  density <- NA
  
  unit_print <- switch(units, "secs" = "sec", "minutes" = "minute", 
                       "hours" = "hour", "days" = "day", "weeks" = "week")
  
  # plot functions ----
  
  trp_exp <- function(x) {
    
    out <- rlang::sym(x)
    
    if (tran == "sqrt") rlang::expr(sqrt(!!out)) 
    else out
  }
  
  y_exp <- function() {
    
    if (dens) rlang::expr(ggplot2::after_stat(density))
    else  rlang::expr(ggplot2::after_stat(count))
  }
  
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
  
  trip_lab <- function() {

    tran_f <- if (tran == "identity") NULL else paste0(" ", tran, " scale")

    paste0("trip length ", "(", units, ")", tran_f)
  }

  f_label <- function() {
    if (format_lab == "decimal") scales::label_number(big.mark = ",")
    else scales::label_scientific()
  }
  
  x_labeller <- function() {
    
    if (tran == "sqrt") {
      
      function(x) format(x^2, scientific = format_lab == "scientific")
      
    } else f_label()
  }
  
  f_tran <- function() {
    
    if (tran == "sqrt") "identity"
    else tran
  }
  
  if (is.null(vpue)) { 
    
    # convert bins to breaks
    trp_rng <- range(trip_tab[[trp_nms[1]]], finite = TRUE)
    p_brks <- pretty(trp_rng, n = bins, min.n = 1)
    
    t_plot <- 
      ggplot2::ggplot(trip_tab, 
                      ggplot2::aes(x = !!trp_exp(trp_nms[1]),
                                   y = !!y_exp())) + 
      ggplot2::labs(x = trip_lab()) +
      ggplot2::scale_x_continuous(trans = f_tran(),
                                  labels = x_labeller()) +
      fishset_theme()
    
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
        
        tran_f <- if (tran == "identity") NULL else paste0(" ", tran, " scale")
        
        paste0(names(trp_nms[trp_nms == x]), " (per ", unit_print, ")", tran_f)
      }
    }
    
    plot_list <- lapply(trp_nms, function(p) {
      
      p_brks <- pretty(range(trip_tab[[p]], finite = TRUE), n = bins, min.n = 1)
      
      h_plot <- 
        ggplot2::ggplot(trip_tab, 
                        ggplot2::aes(x = !!trp_exp(p), 
                                     y = !!y_exp())) +
        ggplot2::labs(title = p, x = axis_lab(p), 
                      caption = paste("bins:", bins)) +
        fishset_theme() + 
        ggplot2::scale_x_continuous(trans = f_tran(),
                                    labels = x_labeller())
      
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