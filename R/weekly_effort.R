# Weekly Effort

#'
#' Summarize average CPUE by week
#'
#' @param dat Primary data containing information on hauls or trips.
#'    Table in FishSET database contains the string 'MainDataTable'.
#' @param project String, name of project.
#' @param cpue Variable(s) in \code{dat} containing catch per unit effort.
#' @param date A variable in \code{dat} containing dates to aggregate by.
#' @param group Grouping variable name(s). Up to two grouping variables are available.
#'   For plotting, if a single CPUE column is entered the first grouping variable is 
#'   passed to the "color" aesthetic and the second to "linetype". If multiple CPUE
#'   columns are entered, a new variable named "species" is created and passed to "fill", 
#'   the first group variable to "linetype", and second is dropped.
#' @param sub_date Date variable used for subsetting, grouping, or splitting by date.
#' @param filter_date The type of filter to apply to `MainDataTable`. To filter by a 
#'   range of dates, use \code{filter_date = "date_range"}. To filter by a given period, use
#'   "year-day", "year-week", "year-month", "year", "month", "week", or "day". 
#'   The argument \code{date_value} must be provided. 
#' @param date_value This argument is paired with \code{filter_date}. If 
#'   \code{filter_date = "date_range"}, enter a string containing a start- and end-date, 
#'   e.g. \code{date_value = c("2011-01-01", "2011-03-15")}. If filtering by period 
#'   (e.g. "year", "year-month"), use integers (4 digits if year, 1-2 digits if 
#'   referencing a day, month, or week). Use a list if using a year-period type filter, 
#'   e.g. "year-week", with the format: \code{list(year, period)}. Use a vector if 
#'   using a single period (e.g. "month"): \code{c(period)}. For example, 
#'   \code{date_value = list(2011:2013, 5:7)} will filter the data table from May 
#'   through July for years 2011-2013 if \code{filter_date = "year-month"}.
#'   \code{date_value = c(2:5)} will filter the data from February through May when 
#'   \code{filter_date = "month"}.
#' @param filter_by String, variable name to filter `MainDataTable` by. the argument 
#'   \code{filter_value} must be provided.
#' @param filter_value A vector of values to filter `MainDataTable` by using the 
#'   variable in \code{filter_by}. For example, if \code{filter_by = "GEAR_TYPE"}, 
#'   \code{filter_value = 1} will include only observations with a gear type of 1. 
#' @param filter_expr String, a valid R expression to filter `MainDataTable` by 
#'   using the variable in \code{filter_by}. 
#' @param facet_by Variable name to facet by. Accepts up to two variables. Facetting 
#'   by \code{"year"} is available if a date variable is added to \code{sub_date}. 
#'   Facetting by \code{"species"} is available if multiple cpue columns are included 
#'   in \code{"cpue"}. The first variable is facetted by row and the second by column.
#' @param conv Convert catch variable to \code{"tons"}, \code{"metric_tons"}, or 
#'   by using a function entered as a string. Defaults to \code{"none"} for no conversion.     
#' @param tran A function to transform the y-axis. Options include log, log2, log10, 
#'   sqrt.
#' @param format_lab Formatting option for y-axis labels. Options include 
#'   \code{"decimal"} or \code{"scientific"}.
#' @param combine Whether to combine variables listed in \code{group}. This is passed
#'   to the "color" aesthetic for plots. 
#' @param scale Scale argument passed to \code{\link[ggplot2]{facet_grid}}. Defaults 
#'   to \code{"fixed"}.
#' @param output Whether to display \code{"plot"}, \code{"table"}. Defaults 
#'   to both (\code{"tab_plot"}).
#' @param format_tab How table output should be formatted. Options include 'wide' 
#'   (the default) and 'long'.
#' @return \code{weekly_effort()} calculates mean CPUE by week. This function doesn't 
#'   calculate CPUE; the CPUE variable must be created in advance (see \code{\link{cpue}}).
#'   When multiple CPUE variables are entered, a new column named "species" 
#'   is created and used to group values in plots. 
#'   The "species" column can also be used to split (or facet) the plot. For table output,
#'   the "species" column will be kept if \code{format_tab = "long"}, i.e. a column of
#'   species names ("species") and a column containing the mean CPUE ("mean_cpue"). When 
#'   \code{format_tab = "wide"}, each CPUE variable is given its own value column.  
#'   The data can be filtered by date and/or by a variable. \code{filter_date} 
#'   specifies the type of date filter to apply--by date-range or by period. 
#'   \code{date_value} should contain the values to filter the data by. To filter 
#'   by a variable, enter its name as a string in \code{filter_by} and include the 
#'   values to filter by in \code{filter_value}. Up to two grouping variables can 
#'   be entered. Grouping variables can be merged into one variable using \code{combine}; 
#'   in this case any number of variables can be joined, but no more than three is 
#'   recommended. For faceting, any variable (including ones listed in \code{group}) 
#'   can be used, but "year" and "species" are also available. Facetting by "year" requires a date 
#'   variable be added to \code{sub_date}. Currently, combined variables cannot be 
#'   faceted. A list containing a table and plot are printed to the console and 
#'   viewer by default.    
#' @examples 
#' \dontrun{
#' weekly_effort(pollockMainDataTable, "CPUE", "DATE_FISHING_BEGAN", filter_date = "year", 
#'               date_value = 2011, output = "table")
#' }
#' @export weekly_effort
#' @import ggplot2
#' @importFrom stats reformulate
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom rlang expr sym
#' @importFrom scales breaks_extended log_breaks label_number label_scientific

weekly_effort <- function(dat, project, cpue, date, group = NULL, sub_date = NULL, 
                          filter_date = NULL, date_value = NULL, filter_by = NULL, 
                          filter_value = NULL, filter_expr = NULL, facet_by = NULL,
                          conv = "none", tran = "identity", format_lab = "decimal", 
                          combine = FALSE, scale = "fixed", output = "tab_plot", 
                          format_tab = "wide") {
  
  # Call in datasets
  out <- data_pull(dat)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main")
  
  week <- sym("week")
  end <- FALSE 
  
  not_num <- vapply(dataset[cpue], function(x) !is.numeric(x), logical(1))
  
  if (any(not_num)) {
    
    warning("'cpue' must be numeric.")
    end <- TRUE
  }
  
  group_date <- group[group %in% c("year", "month", "week")]
  facet_date <- facet_by[facet_by %in% c("year", "month", "week")]
  facet_no_date <- facet_by[!(facet_by %in% c("year", "month", "week"))]
  group_no_date <- group[!(group %in% c("year", "month", "week"))]
  
  # filter by variable ----
  if (!is.null(filter_by) | !is.null(filter_expr)) {
    
    dataset <- subset_var(dataset, filter_by, filter_value, filter_expr)
    
    if (nrow(dataset) == 0) {
      
      warning("Filtered data table has zero rows. Check filter parameters.")
      end <- TRUE
    }
  }
  
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
  
  # date filter ----
  if (!is.null(filter_date)) {
    
    
    if (is.null(date_value)) {
      
      warning("'date_value' must be provided.")
      end <- TRUE
    }
    
    dataset <- subset_date(dataset, sub_date, filter_date, date_value)
    
    if (nrow(dataset) == 0) {
      
      warning("Filtered data table has zero rows. Check filter parameters.")
      end <- TRUE
    }
  }
  
  if (end == FALSE) {
    
    # add missing ---- 
    if ("species" %in% facet_by) {
      
      facet <- facet_no_date[facet_no_date != "species"]
      
      if (length(facet) == 0) {
        facet <- NULL
      }
    } else {
      
      facet <- facet_by
    }
    
    dataset <- add_missing_dates(dataset, date = date, sub_date = sub_date, 
                                 value = cpue, group = group_no_date, 
                                 facet_by = facet)
    
    # add year and week columns
    dataset$year <- as.integer(format(dataset[[date]], "%Y"))
    dataset$week <- as.integer(format(dataset[[date]], "%U"))
    
    # facet date ----
    if (!is.null(facet_date)) {
      
      if ("month" %in% facet_date) {
        
        dataset$month <- factor(format(dataset[[sub_date]], "%b"), levels = month.abb, 
                                ordered = TRUE)
      }
    }
    
    # group date ----
    if (length(group_date) > 0) {
      
      if ("month" %in% group_date & !("month" %in% facet_date)) {
        dataset$month <- factor(format(dataset[[sub_date]], "%b"), levels = month.abb, 
                                ordered = TRUE)
      }
    }
    
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
    agg_grp <- c(group, facet, facet_date)
    
    table_out <- agg_helper(dataset, value = cpue, period = c("year", "week"), 
                            group = agg_grp, fun = mean)
    
    if (length(cpue) > 1) {
      
      table_out <- tidyr::pivot_longer(table_out, cols = !!cpue, names_to = "species", 
                                       values_to = "mean_cpue")
    }
    
    f_cpue <- function() if (length(cpue) == 1) cpue else "mean_cpue"
    
    # weight conversion
    if (conv != "none") {
      
      if (conv == "tons") table_out[f_cpue()] <- table_out[f_cpue()]/2000
      
      else if (conv == "metric_tons") {
        
        table_out[f_cpue()] <- table_out[f_cpue()]/2204.62
        
      } else if (is.function(conv)) {
        
        table_out[f_cpue()] <- do.call(conv, list(table_out[f_cpue()]))
      }
    }
    
    # Confidentiality checks ----
    if (run_confid_check()) {
      
      cc_par <- get_confid_check()
      
      if (cc_par$rule == "n") {
        
        check_table <- 
          check_confidentiality(dataset = dataset, cc_par$v_id, value_var = cpue, 
                                rule = "n", group = c("year", "week", agg_grp), 
                                value = cc_par$value, names_to = "species", 
                                values_to = "mean_cpue")
        
        if (check_table$suppress) {
          
          check_out <- 
            suppress_table(check_table$table, table_out, value_var = f_cpue(),
                           group = c("year", "week", agg_grp), rule = cc_par$rule,
                           type = "code")
          save_table(check_out, project, "weekly_effort_confid")
        }
      }
    }
    
    #if (!is.null(group)) dataset[group] <- lapply(dataset[group], as.factor)
    
    # plot section ----
    if (output %in% c("plot", "tab_plot")) {
      
      # plot functions ----
      
      cpue_sym <- rlang::sym(f_cpue())
      
      if (length(cpue) > 1) species_sym <- rlang::sym("species")
      
      if (!is.null(group)) {
        
        group1_sym <- rlang::sym(group1)
        if (length(group) > 1) group2_sym <- rlang::sym(group2)
      }
      
      cpue_exp <- function() {
          
          if (tran == "sqrt") rlang::expr(sqrt(!!cpue_sym))
          else cpue_sym 
      }
      
      interaction_exp <- function() {
        if (length(cpue) == 1) {
          if (is.null(group)) 1
            
          else {
            if (length(group) == 1) group1_sym
              
            else if (length(group) > 1) {
              rlang::expr(interaction(!!group1_sym, !!group2_sym))
            }
          }
          
        } else if (length(cpue) > 1) {
          if (is.null(group)) species_sym
            
          else if (length(group) == 1) {
            rlang::expr(interaction(!!species_sym, !!group1_sym))
            
          } else if (length(group) > 1) {
            rlang::expr(interaction(!!species_sym, !!group1_sym, !!group2_sym))
          }
        }
      }
      
      color_exp <- function() {
        
        if (length(cpue) == 1) {
          if (!is.null(group)) group1_sym
          else NULL
          
        } else if (length(cpue) > 1) species_sym
      }
      
      linetype_exp <- function() {
        
        if (length(cpue) == 1) {
          if (length(group) > 1) group2_sym
          else NULL
          
        } else if (length(cpue) > 1) {
          if (!is.null(group)) group1_sym
          else NULL
        }
      }
      
      x_lab <- function() paste0(date, " (week)")
      y_lab <- function() {
        paste("mean", ifelse(length(cpue) == 1, cpue, "CPUE"), 
              ifelse(tran == "identity", "", paste0("(", tran, ")")))
      }
      
      y_lab <- function() {
        
        var <- if (length(cpue) > 1) "CPUE" else f_cpue()
        
        f_conv <- function() {
          
          if (conv != "none") {
            
            c_lab <- switch(conv, "tons" = "T", "metric_tons" = "MT", "")
            
            paste0("(", c_lab, ")")
            
          } else NULL
        } 
        
        f_tran <- if (tran != "identity") paste(tran, "scale") else NULL
        
        paste("mean", var, f_conv(), f_tran)
      }
      
      y_breaks <- function() {
        if (tran != "identity") {
          
          brk_num <- nchar(trunc(max(dataset[[f_cpue()]], na.rm = TRUE)))
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
        if (format_lab == "decimal") scales::label_number(big.mark = "")
        else scales::label_scientific()
      }
      
      y_labeller <- function() {
        
        if (tran == "sqrt") {
          
          function(x) format(x^2, scientific = format_lab == "scientific")
          
        } else f_label()
      }
      
      f_tran <- function() {
        
        if (tran == "sqrt") "identity"
        else tran
      }
      
      e_plot <- ggplot2::ggplot(data = table_out, ggplot2::aes(x = week, 
                                                               y = !!cpue_exp())) +
        ggplot2::geom_line(ggplot2::aes(group = !!interaction_exp(), 
                                        color = !!color_exp(), 
                                        linetype = !!linetype_exp())) +
        ggplot2::geom_point(ggplot2::aes(group = !!interaction_exp(), 
                                         color = !!color_exp()), size = 1) +
        ggplot2::scale_y_continuous(trans = f_tran(), breaks = y_breaks(), 
                                    labels = y_labeller()) +
        ggplot2::scale_x_continuous(breaks = num_breaks(table_out$week), 
                                    labels = week_labeller(num_breaks(table_out$week), 
                                                           year = table_out$year)) +
        ggplot2::labs(x = x_lab(), y = y_lab()) +
        ggplot2::theme(legend.position = "bottom") +
        fishset_theme()
      
      if (!is.null(facet_by)) {
        
        if (length(facet_by) == 1) fm <- stats::reformulate(".", facet_by)
          
        else if (length(facet_by) == 2) fm <- paste(facet_by, sep = " ~ ")
        
        e_plot <- e_plot + ggplot2::facet_grid(fm, scales = scale)
      }
      # add year/month filtered to subtitle
      if (!is.null(filter_date)) {
        
        e_plot <- date_title(e_plot, filter_date, date_value)
      }
      
      save_plot(project, "weekly_effort", e_plot)
      
      if (run_confid_check()) {
        
        if (cc_par$rule == "n") {
          
          if (check_table$suppress) {
            
            conf_plot <- e_plot
            conf_plot$data <- replace_sup_code(check_out)
            save_plot(project, "weekly_effort_confid", plot = conf_plot)
          }
        }
      }
      
    }
    
    if (length(cpue) > 1 & format_tab == "wide") {
      
      table_out <- tidyr::pivot_wider(table_out, names_from = "species", 
                                      values_from = "mean_cpue")
    }
    
    # Log function
    weekly_effort_function <- list()
    weekly_effort_function$functionID <- "weekly_effort"
    weekly_effort_function$args <- 
      list(dat, project, cpue, date, group, sub_date, filter_date, date_value, 
           filter_by, filter_value, filter_expr, facet_by, conv, tran, format_lab, 
           combine, scale, output, format_tab)
    log_call(project, weekly_effort_function)
    
    save_table(table_out, project, "weekly_effort")
    
    if (output == "plot") e_plot
      
    else if (output == "table") table_out
      
    else {
      
      out_list <- list(table = table_out,
                       plot = e_plot)
      out_list
    }
  }
}