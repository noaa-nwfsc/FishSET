#' Summarize active vessels
#'
#' \code{vessel_count} counts the number of active vessels in the main table. It
#' can summarize by period if \code{date} is provided, group by any number of 
#' grouping variables, and filter by period or value. There are several options 
#' for customizing table/plot output. 
#' 
#' @param dat Primary data containing information on hauls or trips. 
#'   Table in FishSET database contains the string 'MainDataTable'.
#' @param project String, name of project.
#' @param v_id Variable in \code{dat} containing vessel identifier to count.
#' @param date Date variable to aggregate by.
#' @param period Time period to aggregate by. Options include \code{"year"}, 
#'   \code{"month"}, \code{"week"} (weeks in the year), \code{"weekday"}, 
#'   \code{"weekday_abv"}, \code{"day_of_month"}, \code{"day_of_year"}, and 
#'   \code{"cal_date"} (calender date).
#' @param group Names of grouping variables. For line plots (\code{type = "line"})
#'   two grouping variables can be entered, the first is passed to "color" and second 
#'   to "linetype". Only one grouping variable can be used for barplots 
#'   (\code{type = "bar"}), which is passed to "fill". When \code{combine = TRUE} 
#'   all variables in \code{group} will be joined. Grouping by \code{"year"}, 
#'   \code{"month"}, and \code{"week"} are available if a date variable is added 
#'   to \code{sub_date}.
#' @param sub_date Date variable used for subsetting, grouping, or splitting by date. 
#' @param filter_date The type of filter to apply to `MainDataTable`. To filter 
#'   by a range of dates, use \code{filter_date = "date_range"}. To filter by a 
#'   given period, use "year-day", "year-week", "year-month", "year", "month", 
#'   "week", or "day". The argument \code{date_value} must be provided. 
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
#' @param filter_expr String, a valid R expression to filter `MainDataTable` by 
#'   using the variable in \code{filter_by}. 
#' @param facet_by Variable name to facet by. Accepts up to two variables. These 
#'   can be variables that exist in \code{dat}, or a variable created by 
#'   \code{vessel_count()} such as \code{"year"}, \code{"month"}, or \code{"week"} 
#'   if a date variable is added to \code{sub_date}. The first variable is facetted 
#'   by row and the second by column. 
#' @param combine Whether to combine variables listed in \code{group}. This is passed
#'   to the "fill" or "color" aesthetic for plots. 
#' @param position Positioning of bar plot. Options include 'stack', 'dodge', 
#'   and 'fill'.
#' @param value Whether to return \code{"count"} or \code{"percent"} of active 
#'   vessels. Defaults to \code{"count"}. 
#' @param tran A function to transform the y-axis. Options include log, log2, log10, 
#'   and sqrt.
#' @param format_lab decimal or scientific
#' @param type Plot type, options include \code{"bar"} (the default) and \code{"line"}. 
#' @param scale Scale argument passed to \code{\link{facet_grid}}. 
#'   Options include \code{"free"}, \code{"free_x"}, \code{"free_y"}. Defaults to 
#'   \code{"fixed"}. 
#' @param output Whether to display \code{"plot"}, \code{"table"}. Defaults 
#'   to both (\code{"tab_plot"}). 
#' @details \code{vessel_count} gives the number (or percent) of active vessels
#'   using a column of unique vessel IDs. The data can be filtered by date and/or 
#'   by a variable. (console users may want to use a separate filtering function, 
#'   like \code{dplyr::filter}, before running \code{vessel_count}: note that this 
#'   is okay but will lead to different output if using \code{\link{log_rerun}}).
#'   \code{filter_date} specifies the type of date filter to apply--by date-range or by
#'   period. \code{date_value} should contain the values to filter the data by. To 
#'   filter by a variable, enter its name as a string in \code{filter_by} and
#'   include the values to filter by in \code{filter_value}. 
#'   
#'   Up to two grouping variables can be entered. Grouping variables can be merged 
#'   into one variable using \code{combine = TRUE}; in this case any number of variables 
#'   can be joined, but no more than three is recommended. 
#'   
#'   For faceting, any variable (including ones listed in \code{group}) can be used, 
#'   but "year", "month", "week" are also available provided a date variable is 
#'   added to \code{sub_date}. Currently, combined variables cannot be faceted. 
#'   
#' @return  When \code{output = "tab_plot"} a list containing a table and plot are  
#' returned. If \code{output = "table"} only the summary table is returned, if 
#' \code{output = "plot"} only the plot. 
#' @examples 
#' \dontrun{
#' # grouping by two variables
#' vessel_count(pollockMainDataTable, v_id = "VESSEL_ID", 
#'              group = c("GEAR_TYPE", "IFQ"))
#'              
#' # filter by variable
#' vessel_count(pollockMainDataTable, v_id = "VESSEL_ID", group = "GEAR_TYPE",
#'              filter_by = "IFQ", filter_value = "Y")
#'              
#' # filter by month
#' vessel_count(pollockMainDataTable, v_id = "VESSEL_ID", group = "GEAR_TYPE",
#'              sub_date = "HAUL_DATE", date_filter = "month", date_value = 1:5)
#'              
#' #' # filter by date
#' vessel_count(pollockMainDataTable, v_id = "VESSEL_ID", group = "GEAR_TYPE",
#'              sub_date = "HAUL_DATE", date_filter = "date_range", 
#'              date_value = c("2011-01-01", "2011-02-05"))
#' 
#' # summarize by month
#' vessel_count(pollockMainDataTable, v_id = 'VESSEL_ID', date = 'DATE_FISHING_BEGAN', 
#'              period = 'month', group = 'DISEMBARKED_PORT', position = 'dodge', 
#'              output = 'plot')
#' }
#' @export vessel_count
#' @import ggplot2
#' @importFrom dplyr n_distinct mutate across all_of
#' @importFrom stats reformulate
#' @importFrom rlang sym expr as_string
#' @importFrom scales label_percent breaks_extended log_breaks

vessel_count <- function(dat,
                         project,
                         v_id,
                         date = NULL,
                         period = NULL,
                         group = NULL,
                         sub_date = NULL,
                         filter_date = NULL,
                         date_value = NULL,
                         filter_by = NULL,
                         filter_value = NULL,
                         filter_expr = NULL,
                         facet_by = NULL,
                         combine = FALSE,
                         position = "stack",
                         tran = "identity",
                         format_lab = "decimal",
                         value = "count",
                         type = "bar",
                         scale = "fixed",
                         output = "tab_plot") {
  
  
  # Call in datasets
  out <- data_pull(dat, project)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main", project)
 
  if (!is.null(period)) {
    if(period == "no_period") period <- NULL # for shinyapp
  }
  
  # Convert to string if v_id is a factor
  if (is.factor(dataset[[v_id]])) dataset[[v_id]] <- as.character(dataset[[v_id]])
  
  pers <- c("year", "month", "week")
  group_date <- group[group %in% pers]
  facet_date <- facet_by[facet_by %in% pers]
  facet_no_date <- facet_by[!facet_by %in% pers]
  group_no_date <- group[!group %in% pers]
  
  column_check(dataset, cols = c(v_id, date, group_no_date, sub_date, filter_by,
                                 facet_no_date))
  
  # date ----
  # convert date and/or sub_date to date class
  if (!is.null(date) | !is.null(sub_date)) {
    
    dataset[unique(c(date, sub_date))] <- 
      lapply(dataset[unique(c(date, sub_date))], date_parser)
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
  
  # add missing values----
  dataset <- expand_data(dataset, project, date = date, value = v_id, 
                         period = period, group = group_no_date, 
                         facet_by = facet_no_date, fun = "count")
  
  # period ----
  p <- period_check(period, date)
  
  # create period column
  if (!is.null(period) && period != "cal_date") {
    
    dataset[[period]] <- format(dataset[[date]], p)
  }
  
  # facet/group date ----
  # change name to create_period?
  dataset <- facet_period(dataset, facet_date = unique(c(facet_date, group_date)),
                          date = sub_date, period = period)
  
  # group ----
  if (!is.null(group)) {
    
    if (combine == TRUE & length(group) > 1) { 
      
      dataset <- ID_var(dataset, project = project, vars = group, type = "string",
                        log_fun = FALSE)
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
  
  # summary table ----
  agg_grp <- unique(c(group, facet_by, facet_date))
  
  if (!is.null(period)) {
    
    if (period == "cal_date") period <- date
  }
  
  if (value == "percent") fun <- "percent"
  else fun <- NULL
  
  table_out <- agg_helper(dataset, value = v_id, period = period, group = agg_grp,
                           fun = function(x) dplyr::n_distinct(x, na.rm = TRUE), 
                           count = FALSE)
  
  if (value == "percent") {
    
    v_id_perc <- paste0(v_id, "_perc")
    
    table_out <- table_out %>% 
      dplyr::mutate(dplyr::across(dplyr::all_of(v_id), .fns = ~.x/sum(.x) * 100, 
                                  .names = ".{col}_perc"))
  }
  
  # convert period to factor
  if (!is.null(period)) {
    
    if (!is.null(p)) {# if period != "cal_date"
      if (p %in% c("%Y-%m", "%a", "%b")) {
        
        table_out <- date_factorize(table_out, period, p)
        
      } else {
        
        table_out[[period]] <- as.integer(table_out[[period]])
      }
    }
    # reorder table by period
    table_out <- table_out[order(table_out[[period]]), ]
  }
  
  # order group1 by value
  if (!is.null(group)) {
    
    rev <- position == "dodge"
    or_nm <- ifelse(value == "percent", v_id_perc, v_id)
    table_out <- order_factor(table_out, group1, or_nm, rev = rev)
  }
  
  row.names(table_out) <- 1:nrow(table_out)
  
  # confidentiality check ----
  if (run_confid_check(project)) {
    
    cc_par <- get_confid_check(project)
    
    if (cc_par$rule == "n") {
      # suppression index
      sup_ind <- table_out[[v_id]] > 0 & table_out[[v_id]] < cc_par$value
      
      if (sum(sup_ind) > 0) {
        
        cache_check_table(table_out[sup_ind, c(period, agg_grp)], project)
        table_out_c <- table_out
        table_out_c[sup_ind, v_id] <- -999
        save_table(table_out_c, project, "vessel_count_confid")
      }
    }
  }
  
  # plot ----
  if (output %in% c("plot", "tab_plot")) {
    
    f_vessel <- function() {
      if (value == "percent") rlang::sym(v_id_perc)
      else rlang::sym(v_id)
    }
    
    # plot functions ----
    vessel_exp <- function() {
      if (tran == "sqrt") rlang::expr(sqrt(!!f_vessel()))
      else f_vessel()
    }
    
    xaxis_exp <- function() {
      if (!is.null(period)) rlang::sym(period)
      
      else { 
        if (!is.null(group)) rlang::sym(group1) 
        else v_id
      }
    }
    
    group1_exp <- function() if (!is.null(group)) rlang::sym(group1) else NULL
    group2_exp <- function() if (length(group) > 1) rlang::sym(group2) else NULL
    
    if (!is.null(period)) {
      
      interaction_exp <- function() {
        if (is.null(group)) 1
        else if (!is.null(group) & length(group) == 1) group1_exp()
        else if (length(group) > 1) {
          rlang::expr(interaction(!!group1_exp(), !!group2_exp())) 
        }
      }
      
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
        if (is.null(group))  NULL
        else if (!is.null(group) & length(group) == 1) 1
        else if (length(group) > 1) group2_exp()
      }
      
      color_exp <- function() {
        if (is.null(group) | !is.null(group) & length(group) == 1) NULL
        else if (!is.null(group) & length(group) > 1) group2_exp()
      }
      
      linetype_exp <- function() NULL
    }
    
    x_lab <- function() {
      
      if (!is.null(period)) {
        
        p_lab <- switch(period, "year_month" = "year-month", "month_year" = "month-year",
                        "year" = "year", "month" = "month", "week" = "week", "day_of_month" = "day of the month",
                        "day_of_year" = "day of the year")
        
        if (period != date) paste0(date, " (", p_lab, ")")
        else date
        
      } else if (is.null(group)) NULL
      else rlang::as_string(xaxis_exp())
    }
    
    y_lab <- function() {
      
      paste("active vessels", ifelse(tran != "identity", paste0("(", tran, " scale)"), ""))
    }
    
    point_size <- function() {
      
      if (!is.null(period)) {
        if (is.null(p)) .5
        else if (period == "week") 1
        else 2
        
      } else 2
    }
    
    f_label <- function() {
      if (format_lab == "decimal") scales::label_number(big.mark = ",")
      else scales::label_scientific()
    }
    
    y_labeller <- function() { 
      
      if (value == "percent") {
        
        if (tran == "sqrt") function(x) paste0(x^2, "%")
        else scales::label_percent(scale = 1) 
        
      } else {
        
        if (tran == "sqrt") {
          
          function(x) format(x^2, scientific = format_lab == "scientific")
          
        } else f_label()
      }
    }
    
    y_breaks <- function() {
      if (tran != "identity") {
        
        brk_num <- nchar(trunc(max(dataset[[v_id]], na.rm = TRUE)))
        brk_num <- ifelse(length(brk_num) < 5, 5, brk_num)
        
        if (tran %in% c("log", "log2", "log10")) {
          
          y_base <- switch(tran, "log" = exp(1), "log2" = 2, "log10" = 10)
          
          scales::log_breaks(n = brk_num, base = y_base)
          
        } else {
          
          scales::breaks_extended(n = brk_num + 2)
        }
        
      } else ggplot2::waiver()
    }
    
    f_tran <- function() {
      
      if (tran == "sqrt") "identity"
      else tran
    } 
    
    v_plot <- ggplot2::ggplot(data = table_out, ggplot2::aes(x = !!xaxis_exp(), 
                                                             y = !!vessel_exp())) +
      fishset_theme() +
      ggplot2::theme(legend.position = "bottom") +
      ggplot2::scale_y_continuous(labels = y_labeller(), 
                                  trans = f_tran(), 
                                  breaks = y_breaks())
    
    if (type == "bar") {
      
      if (position == "dodge") position <- ggplot2::position_dodge2(preserve = "single")
      
      v_plot <- v_plot + ggplot2::geom_col(ggplot2::aes(fill = !!color_exp()), 
                                           position = position)
      
      position <- ifelse(class(position)[1] == "PositionDodge2", "dodge", position)
      
    } else if (type == "line") {
      
      v_plot <- v_plot + ggplot2::geom_line(ggplot2::aes(group = !!interaction_exp(), 
                                                         color = !!color_exp(), 
                                                         linetype = !!linetype_exp())) +
        ggplot2::geom_point(ggplot2::aes(group = !!interaction_exp(), 
                                         color = !!color_exp()), size = point_size())
    }
    
    if (!is.null(facet_by)) {
      
      if (length(facet_by) == 1) fm <- stats::reformulate(".", facet_by)
      else if (length(facet_by) == 2) fm <- paste(facet_by, sep = " ~ ")
      
      if (is.null(period)) {
        v_plot <- v_plot + ggplot2::facet_wrap(fm, scales = scale)
      } else {
        v_plot <- v_plot + ggplot2::facet_grid(fm, scales = scale)
      }
    }
    # add labels 
    v_plot <- v_plot + ggplot2::labs(x = x_lab(), y = y_lab())
    
    if (!is.null(period)) {
      # adjust x-axis breaks 
      if (!is.null(p)) {
        if (!(p %in% c("%a", "%b", "%Y-%m"))) {
          
          v_plot <- v_plot + ggplot2::scale_x_continuous(breaks = num_breaks(table_out[[period]]))
          
        } else if (period == "month_year") {
          
          d_labs <- levels(table_out$month_year)
          d_labs <- format(as.Date(paste0(d_labs, "-01")), "%b %y")
          v_plot <- v_plot + ggplot2::scale_x_discrete(labels = d_labs)
        }
      }
    }
    
    save_plot(project, "vessel_count", v_plot)
    
    if (run_confid_check(project)) {
      
      if (exists("table_out_c")) {
        
        v_plot_c <- v_plot
        v_plot_c$data <- replace_sup_code(table_out_c)
        save_plot(project, "vessel_count_confid", plot = v_plot_c)
      }
    }
  }
  
  if (!is.null(period)) {
    if (period == "month_year") {
      table_out$month_year <- format(as.Date(paste0(table_out$month_year, "-01")), "%b %y")
    }
  }
  # Log function
  vessel_count_function <- list()
  vessel_count_function$functionID <- "vessel_count"
  vessel_count_function$args <- 
    list(dat, project, v_id, date, period, group, sub_date, filter_date,
         date_value, filter_by, filter_value, filter_expr, facet_by, 
         combine, position, tran, format_lab, value, type, scale, output)
  log_call(project, vessel_count_function)
  
  # Output folder
  save_table(table_out, project, "vessel_count")
  
  if (output == "table") table_out
  else if (output == "plot") v_plot
  else {
    
    out_list <- list(table = table_out, plot = v_plot)
    out_list
  }
}


