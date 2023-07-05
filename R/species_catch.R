#' Summarize species catch
#' 
#' \code{species_catch} summarizes catch (or other numeric variables) in the
#' main table. It can summarize by period if \code{date} is provided, grouping 
#' variables, and filter by period or value. There are several options for 
#' customizing the table and plot output. 
#' 
#' @param dat Primary data containing information on hauls or trips. 
#' Table in FishSET database contains the string 'MainDataTable'.
#' @param project String, name of project.
#' @param species Variable in \code{dat} containing the species catch or a vector 
#'   of species variables (in pounds).
#' @param date Variable in \code{dat} containing dates to aggregate by.
#' @param period Time period to count by. Options include 'year', 'month', 'week' 
#' (week of the year), 'weekday', 'day' (day of the month), and 'day_of_year'. 
#' \code{date} is required.
#' @param fun String, name of function to aggregate by. Defaults to 
#'   \code{\link[base]{sum}}. 
#' @param group Grouping variable name(s). Up to two grouping variables are available 
#'   for line plots and one for bar plots. For bar plots, if only one species is 
#'   entered the first group variable is passed to 'fill'. If multiple species are 
#'   entered, species is passed to "fill" and the grouping variable is dropped.
#'   An exception occurs when facetting by species, then the grouping variable is 
#'   passed to "fill". For line plots, the first grouping variable is passed to 
#'   "fill" and the second to "linetype" if a single species column is entered or 
#'   if facetting by species. Otherwise species is passed to "fill", the first 
#'   group variable to "linetype", and second is dropped. 
#' @param sub_date Date variable used for subsetting, grouping, or splitting by date.
#' @param filter_date The type of filter to apply to `MainDataTable`. To filter by a 
#'   range of dates, use \code{filter_date = "date_range"}. To filter by a given 
#'   period, use "year-day", "year-week", "year-month", "year", "month", "week", 
#'   or "day".  The argument \code{date_value} must be provided. 
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
#'   can be variables that exist in the \code{dat}, or a variable created by 
#'   \code{species_catch()} such as \code{"year"}, \code{"month"}, or \code{"week"} 
#'   if a date variable is added to \code{sub_date}. Facetting by \code{"species"} 
#'   is available if multiple catch columns are included in \code{"species"}. The 
#'   first variable is facetted by row and the second by column.   
#' @param type Plot type, options include \code{"bar"} (the default) and \code{"line"}. 
#' @param conv Convert catch variable to \code{"tons"}, \code{"metric_tons"}, or 
#'   by using a function entered as a string. Defaults to \code{"none"} for no 
#'   conversion.
#' @param tran A function to transform the y-axis. Options include log, log2, 
#'   log10, sqrt.
#' @param format_lab Formatting option for y-axis labels. Options include 
#'   \code{"decimal"} or \code{"scientific"}.
#' @param value Whether to calculate raw \code{"count"} or \code{"percent"} of 
#'   total catch. 
#' @param position Positioning of bar plot. Options include 'stack', 'dodge', 
#'   and 'fill'. 
#' @param combine Whether to combine variables listed in \code{group}. This is 
#'   passed to the "fill" or "color" aesthetic for plots. 
#' @param scale Scale argument passed to \code{\link{facet_grid}}. Defaults to 
#'   \code{"fixed"}.
#' @param output Output a \code{"plot"} or \code{"table"}. Defaults 
#'   to both (\code{"tab_plot"}).
#' @param format_tab How table output should be formatted. Options include 'wide' 
#'   (the default) and 'long'.
#' @return  \code{species_catch()} aggregates catch using one or more columns of 
#'   catch data. Users can aggregate by time period, by group, or by both. When 
#'   multiple catch variables are entered, a new column "species" is created and 
#'   used to group values in plots. The "species" column can also be used to split 
#'   (or facet) the plot. For table output,
#'   the "species" column will be kept if \code{format_tab = "long"}, i.e. a column 
#'   of species names ("species") and a column containing catch ("catch"). When 
#'   \code{format_tab = "wide"}, each species is given its own column of catch. 
#'    
#'   The data can be filtered by date and/or by a variable. \code{filter_date} 
#'   specifies the type of date filter to apply--by date-range or by period. 
#'   \code{date_value} should contain the values to filter the data by. To filter 
#'   by a variable, enter its name as a string in \code{filter_by} and include the 
#'   values to filter by in \code{filter_value}. 
#'   
#'   Plots can handle Up to two grouping variables, there is no limit for tables. 
#'   Grouping variables can be merged into one variable using \code{combine}; in 
#'   this case any number of variables  can be joined, but no more than three is
#'   recommended. 
#'   
#'   For faceting, any variable (including ones listed in \code{group}) 
#'   can be used, but "year", "month", "week" are also available provided a date 
#'   variable is added to \code{sub_date}. Currently, combined variables cannot be 
#'   faceted. A list containing a table and plot are printed to the console and 
#'   viewer by default.  
#' @examples 
#' \dontrun{
#' # summarizing one catch column by one group variable
#' species_catch(pollockMainDataTable, species = "OFFICIAL_TOTAL_CATCH_MT",
#'               group = "GEAR_TYPE", ouput = "tab_plot")
#' 
#' # summarizing three catch columns by month
#' species_catch('pollockMainDataTable', 
#'               species = c('HAUL_LBS_270_POLLOCK_LBS', 
#'                           'HAUL_LBS_110_PACIFIC_COD_LBS', 
#'                           'HAUL_LBS_OTHER_LBS'), 
#'               date = 'HAUL_DATE', period = 'month_num', output = 'plot', 
#'               conv = 'tons')
#'
#' # filtering by variable
#' species_catch(pollockMainDataTable, species = "OFFICAL_TOTAL_CATCH_MT",
#'               group = "GEAR_TYPE", filter_by = "PORT_CODE", 
#'               filter_value = "Dutch Harbor")
#'               
#'  # filtering by date
#'  species_catch(pollockMainDataTable, species = "OFFICAL_TOTAL_CATCH_MT",
#'                sub_date = "HAUL_DATE", filter_date = "month", date_value = 7:10)
#' }
#' @export species_catch
#' @import ggplot2
#' @importFrom stats reformulate
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom rlang expr sym
#' @importFrom scales label_percent breaks_extended log_breaks label_number 
#'   label_scientific

species_catch <- function(dat,
                          project,
                          species,
                          date = NULL,
                          period = NULL,
                          fun = "sum",
                          group = NULL,
                          sub_date = NULL,
                          filter_date = NULL,
                          date_value = NULL,
                          filter_by = NULL,
                          filter_value = NULL,
                          filter_expr = NULL,
                          facet_by = NULL,
                          type = "bar",
                          conv = "none",
                          tran = "identity",
                          format_lab = "decimal",
                          value = "count",
                          position = "stack",
                          combine = FALSE,
                          scale = "fixed",
                          output = "tab_plot",
                          format_tab = "wide") {
  
  
  # Call in datasets
  out <- data_pull(dat, project)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main", project)
  
  if (!is.null(period)) if (period == "no_period") period <- NULL # for shiny app

  pers <- c("year", "month", "week")
  group_date <- group[group %in% pers]
  facet_date <- facet_by[facet_by %in% pers]
  facet_no_date <- facet_by[!facet_by %in% pers]
  group_no_date <- group[!group %in% pers]
  facet_check <- facet_no_date[!facet_no_date %in% "species"]
  
  column_check(dataset, cols = c(species, date, group_no_date, sub_date, 
                                 filter_by, facet_check))
  
  num <- qaqc_helper(dataset[species], is.numeric)
  
  if (!all(num)) stop("Argument 'species' must be numeric.", call. = FALSE)
  
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
  
  # add missing ----
  if ("species" %in% facet_by) {
    
    facet <- facet_no_date[facet_no_date != "species"]
    
    if (length(facet) == 0) facet <- NULL
    
  } else facet <- facet_no_date
  
  
  # period ----
  p_code <- period_check(period, date)
  
  # add missing values ----
  dataset <- expand_data(dataset, project, date = date, value = species, 
                               period = period, group = group_no_date, 
                               facet_by = facet, fun = "sum")
  
  if (!is.null(period)) {
    if (period != "cal_date") dataset[[period]] <- format(dataset[[date]], p_code)
  }
  
  # facet/group date ----
  if (!is.null(facet_date) | !is.null(group_date)) {
    
    dataset <- facet_period(dataset, facet_date = unique(c(facet_date, group_date)),
                            date = sub_date, period = period)
  }
  
  # group ----
  if (!is.null(group)) {
    
    if (combine == TRUE & length(group) > 1) { 
      
      dataset <- ID_var(dataset, project = project, vars = group, type = "string",
                        log_fun = FALSE)
      group <- gsub(" ", "", paste(group, collapse = "_"))
      group1 <- group
      group2 <- NULL
      
    } else group1 <- group[1]
    
    if (length(group) == 1) group2 <- NULL else group2 <- group[2]
    
    if (length(group) > 2) {
      
      warning("Only the first two grouping variables will be displayed in plot.")
    }
  }
   
  # summary table ----
  agg_grp <- c(group, facet, facet_date)

  if (!is.null(period)) {
    
    if (period == "cal_date") period <- date
  }
  
  table_out <- agg_helper(dataset, value = species, period = period, 
                           group = agg_grp, fun = fun, count = FALSE)
  
  # melt table if multiple species columns entered (for ggplot)
  if (length(species) > 1) { 
    
    table_out <- tidyr::pivot_longer(table_out, cols = !!species, 
                                     names_to = "species", values_to = "catch")
  }
  
  f_catch <- function() if (length(species) == 1) species else "catch"
  
  # create ordered factor for plots
  if (!is.null(group)) table_out[group] <- lapply(table_out[group], as.factor)
  rev <- position == "dodge"
  
  if (length(species) > 1) {
    
    table_out <- order_factor(table_out, "species", "catch", rev = rev)
  
  } else {
    
    if (!is.null(group)) {
      
      table_out <- order_factor(table_out, group1, species, rev = rev)
    }
  }
  
  if (!is.null(period)) {
    if (!is.null(p_code)) {# if period != "cal_date"
      # convert period to ordered factor/integer
      if (p_code %in% c("%Y-%m", "%a", "%b")) {
        
        table_out <- date_factorize(table_out, period, p_code)
        
      } else table_out[[period]] <- as.integer(table_out[[period]])
    }
    
    table_out <- table_out[order(table_out[[period]]), ]
  }
  
  # convert catch to tons, MT, or custom
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
    
      table_out[f_catch()] <- 
        (table_out[f_catch()]/sum(table_out[f_catch()])) * 100
    
    } else {
      
      warning("Cannot convert to percentage. Change 'fun' argument to 'sum'.")
    }
  } 
  
  # confidentiality checks ----
  if (run_confid_check(project)) {
    
    cc_par <- get_confid_check(project)
    
    check_out <- 
      check_confidentiality(dataset = dataset, project, cc_par$v_id, value_var = species, 
                            rule = cc_par$rule, group = c(period, agg_grp), 
                            value = cc_par$value, names_to = "species", 
                            values_to = "catch")
 
    if (any(check_out$suppress)) {

      table_out_c <- suppress_table(check_out$table, table_out, value_var = f_catch(),
                                  group = c(period, agg_grp), rule = cc_par$rule,
                                  type = "code")
      save_table(table_out_c, project, "species_catch_confid")
    }
  }
  
  if (output %in% c("plot", "tab_plot")) {
    
    # plot functions ----
    
    catch_sym <- rlang::sym(f_catch())
    
    catch_exp <- function() {
     
      if (tran == "sqrt") rlang::expr(sqrt(!!catch_sym))
      else catch_sym
    }
    
    xaxis_exp <- function() {
      
      if (!is.null(period)) rlang::sym(period)
      else { 
        
        if (!is.null(group)) rlang::sym(group1) 
        else {
          
          if (length(species) == 1) species
          else if (length(species) > 1) rlang::sym("species")
        }
      }
    }
    
    if (!is.null(period)) {
      
      interaction_exp <- function() {
        
        if (length(species) == 1) {
          if (is.null(group)) 1
          else {
            
            if (length(group) == 1) rlang::sym(group1)
            else if (length(group) > 1) {
              rlang::expr(interaction(!!rlang::sym(group1), !!rlang::sym(group2)))
            }
          }
          
        } else if (length(species) > 1) {
          if (is.null(group)) rlang::expr(species)
          
          else if (length(group) == 1) {
            rlang::expr(interaction(species, !!rlang::sym(group1)))
            
          } else if (length(group) > 1) {
            rlang::expr(interaction(species, !!rlang::sym(group1), !!rlang::sym(group2)))
          }
        }
      }
      
      color_exp <- function() {
        
        if (length(species) == 1) {
          
          if (is.null(group)) NULL
          else rlang::sym(group1)
          
        } else if (length(species) > 1) rlang::sym("species")
      }
      
      linetype_exp <- function() {
        
        if (length(species) == 1) {
          if (is.null(group) | length(group) == 1) NULL
          else rlang::sym(group2)
        
        } else if (length(species) > 1) {
          if (is.null(group)) NULL 
          else rlang::sym(group1) 
        }
      }
      
    } else {
      # aggregating by group only (no dates)
      
      interaction_exp <- function() {
        
        if (length(species) == 1) {
          
          if (is.null(group)) NULL
          else if (length(group) == 1) 1
          else if (length(group) > 1) rlang::sym(group2)
        
        } else if (length(species) > 1) {
          
          if (is.null(group) | length(group) == 1) rlang::sym("species")
          
          else if (length(group) > 1) {
            
            rlang::expr(interaction(species, !!rlang::sym(group2)))
          }
        }
      }
      
      color_exp <- function() {
        
        if (length(species) == 1) {
          
          if (is.null(group) | length(group) == 1)  NULL
          else if (length(group) > 1) rlang::sym(group2)
    
        } else if (length(species) > 1) rlang::sym("species")
      }
      
      linetype_exp <- function() {
        
        if (length(species) > 1 & length(group) > 1) rlang::sym(group2) 
        else NULL
      }
      
    }  
    
    x_lab <- function() {
      
      if (!is.null(period)) {
        
        p_lab <- switch(period, "year_month" = "year-month", "month_year" = "month-year",
                        "year" = "year", "month" = "month", "week" = "week", 
                        "day_of_month" = "day of the month", 
                        "day_of_year" = "day of the year")
        
        if (period != date) paste0(date, " (", p_lab, ")")
        else date
        
      } else if (is.null(group) & length(species) == 1) NULL
      else rlang::as_string(xaxis_exp())
    }  
    
    y_lab <- function() {
      
      f_conv <- function() {
        
        if (conv != "none") {
          
          c_lab <- switch(conv, "tons" = "T", "metric_tons" = "MT", "")
          
          paste0("(", c_lab, ")")
          
        } else NULL
      } 
      
      f_tran <- if (tran != "identity") paste(tran, "scale") else NULL
      
      paste(fun, f_catch(), f_conv(), f_tran)
    }
    
    y_breaks <- function() {
      if (tran != "identity") {
        
        brk_num <- nchar(trunc(max(dataset[[f_catch()]], na.rm = TRUE)))
        brk_num <- ifelse(length(brk_num) < 5, 5, brk_num)
        
        if (tran %in% c("log", "log2", "log10")) {
          
          y_base <- switch(tran, "log" = exp(1), "log2" = 2, "log10" = 10)
          
          scales::log_breaks(n = brk_num, base = y_base)
          
        } else {
          
          scales::breaks_extended(n = brk_num + 2)
        }
        
      } else ggplot2::waiver()
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
    
    f_tran <- function() {
      
      if (tran == "sqrt") "identity"
      else tran
    }
    
    # plot ----
    s_plot <- ggplot2::ggplot(data = table_out, 
                              ggplot2::aes(x = !!xaxis_exp(),
                                           y = !!catch_exp())) +
      fishset_theme() +
      ggplot2::theme(legend.position = "bottom") +
      ggplot2::scale_y_continuous(labels = y_labeller(),
                                  trans = f_tran(),
                                  breaks = y_breaks())
    
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
      
      if (length(facet_by) == 1) fm <- stats::reformulate(".", facet_by)
        
      else if (length(facet_by) == 2) fm <- paste(facet_by, sep = " ~ ")
    
      if (is.null(period)) {
        s_plot <- s_plot + ggplot2::facet_wrap(fm, scales = scale)
      } else {
        s_plot <- s_plot + ggplot2::facet_grid(fm, scales = scale)
      }
    }
    
    if (!is.null(period)) {
      if (!is.null(p_code)) {# if period != "cal_date"
        if (!(p_code %in% c("%a", "%b", "%Y-%m"))) {
          
          s_plot <- 
            s_plot + ggplot2::scale_x_continuous(breaks = num_breaks(table_out[[period]]))
        
          } else if (period == "month_year") {
          
          d_labs <- levels(table_out$month_year)
          d_labs <- format(as.Date(paste0(d_labs, "-01")), "%b %y")
          s_plot <- s_plot + ggplot2::scale_x_discrete(labels = d_labs)
        }
      }
    }
    
    # remove legend if
    if (is.null(period) & is.null(group) & length(species) > 1) {
      
      s_plot <- s_plot + ggplot2::theme(legend.position = "none")
    }
    
    # add labels
    s_plot <- s_plot  + ggplot2::labs(x = x_lab(), y = y_lab())
    
    # save plots
    save_plot(project, "species_catch")
    
    if (run_confid_check(project)) {
      
      if (any(check_out$suppress)) {
        
        s_plot_c <- s_plot
        s_plot_c$data <- replace_sup_code(table_out_c)
        save_plot(project, "species_catch_confid", plot = s_plot_c)
      }
    }
   
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
                                      conv, tran, format_lab, value, position, 
                                      combine, scale, output, format_tab)
  log_call(project, species_catch_function)
  
  if (format_tab == "wide") {
    
    if (length(species) > 1) {
      
      table_out <- tidyr::pivot_wider(table_out, names_from = "species", 
                                      values_from = "catch")
    }
  }
  
  save_table(table_out, project, "species_catch")
  
  # output ----
  if (output == "table") table_out
  else if (output == "plot") s_plot
  else if (output == "tab_plot") {
    
    out_list <- list(table = table_out, plot = s_plot)
    out_list
  } 
}
