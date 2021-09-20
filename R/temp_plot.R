temp_plot <- function(dat, project, var.select, len.fun = c("length", "unique", "percent"), 
                      agg.fun = c("mean", "median", "min", "max", "sum"), date.var = NULL) {
  #' Returns three different plots of variable by month/year
  #'
  #' Returns three plots showing the variable of interest against time (as month 
  #' or month/year). Plots are raw points by time, number of observations by time, 
  #' and aggregated variable of interest by time.
  #' @param dat Primary data containing information on hauls or trips.
  #'   Table in FishSET database contains the string 'MainDataTable'.
  #' @param project String, name of project.
  #' @param var.select Variable in \code{dat} to plot against a date variable.
  #' @param len.fun Method,\code{"length"} returns the number of observations, 
  #'   \code{"unique"} returns the number of unique observations, \code{"percent"}
  #'   returns the percentage of total observations.
  #' @param agg.fun Method to aggregate \code{var.select} by date. Choices are 
  #'   \code{"mean"}, \code{"median"}, \code{"min"}, \code{"max"}, or \code{"sum"}.
  #' @param date.var Date variable in \code{dat}. Defaults to first date variable 
  #'   in \code{dat} set if not defined.
  #' @keywords plot, temporal, exploration
  #' @description Returns three plots showing the variable of interest against time 
  #'   (as month or month/year). Plots are raw points by date, number of observations 
  #'   by date, and measures of a representative observation by date.
  #' @return Returns plot to R console and saves output to the Output folder.
  #' @import ggplot2
  #' @export
  #' @examples
  #' \dontrun{
  #' temp_plot(pollockMainDataTable, project='pollock', var.select = 'OFFICIAL_TOTAL_CATCH_MT',
  #'            len.fun = 'percent', agg.fun = 'mean', date.var = 'HAUL_DATE')
  #' temp_plot(pollockMainDataTable, project='pollock', var.select = 'OFFICIAL_TOTAL_CATCH_MT',
  #'            len.fun = 'length', agg.fun = 'max')
  #' }
  #' 
  # Call in datasets
  out <- data_pull(dat, project)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main")
  
  end <- FALSE
  suppress <- FALSE
  
  # Date var - NULL or use the first one
  if (is.null(date.var)) {
    
    date.var <- date_cols(dataset)[1]
    if (length(date.var) == 0) {
      warning("No valid date columns detected in dataset.")
      end <- TRUE
    }
    
  } else {
 
    if (!(date.var %in% date_cols(dataset))) {
      warning(date.var, " column is not a date variable.")
      end <- TRUE
    } 
  }
  
  if (grepl("date", var.select, ignore.case = TRUE)) {
    
    if (var.select %in% date_cols(dataset)) {
      
      dataset[[var.select]] <- date_parser(dataset[[var.select]])
    
    } else {
      
      warning(var.select, " column is not a date variable.")
      end <- TRUE
    }
  }
  
  if (end == FALSE) {
    
    dataset[[date.var]] <- date_parser(dataset[[date.var]])
    
    dataset <- facet_period(dataset, c("year", "month"), date.var)
    
    plot_by_yr <- length(unique(dataset$year)) > 1
    
    agg_per <- ifelse(plot_by_yr, "year", "month")
    
    if (plot_by_yr) dataset$year <- as.factor(dataset$year)
    
    temp_fun <- switch(len.fun, 
                       "length" = length, 
                       "unique" = function(x) length(unique(x)),
                       "percent" = function(x) {
                         round(length(x) / nrow(dataset) * 100, 2)
                         }
                       )
 
    # summary tables ----
    len_df <- agg_helper(dataset, var.select, group = agg_per, fun = temp_fun)
    agg_df <- agg_helper(dataset, var.select, group = agg_per, fun = agg.fun)
    
    # confidentiality checks ----
    if (run_confid_check()) {
      cc_par <- get_confid_check()
      if (cc_par$rule == "n") {
        
        len_conf <- check_and_suppress(dataset, len_df, cc_par$v_id, var.select, 
                                       group = agg_per, rule = "n", value = cc_par$value,
                                       type = "NA")
        agg_conf <- check_and_suppress(dataset, agg_df, cc_par$v_id, var.select, 
                                       group = agg_per, rule = "n", value = cc_par$value,
                                       type = "NA")
        
        suppress <- any(len_conf$suppress, agg_conf$suppress)
      }
    }
    
    # plots ----
  
    
    t_plot <- temp_plot_helper(dataset, len_df, agg_df, var.select, date.var, agg_per, 
                               len.fun, agg.fun, plot_by_yr)
    
    if (suppress) {
      
      conf_plot <- temp_plot_helper(dataset, len_conf$table, agg_conf$table, var.select, 
                                    date.var, agg_per, len.fun, agg.fun, plot_by_yr)
      
      save_plot(project, "temp_plot_confid", plot = conf_plot)
    }
    
    # Log the function
    temp_plot_function <- list()
    temp_plot_function$functionID <- "temp_plot"
    temp_plot_function$args <- list(dat, project, var.select, len.fun, agg.fun, 
                                    date.var)
    log_call(project, temp_plot_function)
    
    # Save output
    save_plot(project, "temp_plot", plot = t_plot)
    
    t_plot
  }
}

temp_plot_helper <- function(dataset, len_df, agg_df, var.select, date.var, agg_per, 
                        len.fun, agg.fun, plot_by_yr) {
  #' plot help function for spatial plots
  #' @param dataset dataset name
  #' @param len_df length bar plot
  #' @param agg_df aggregation plot
  #' @param var.select variable
  #' @param date.var date variable
  #' @param agg_per character
  #' @param len.fun length, unique, percent
  #' @param agg.fun aggregation function
  #' @param plot_by_yr Whether to plot by year. 
  #' @export
  #' @keywords interanl
  #' @details Used in temp_plot
  
  # plot functions
  
  len_title <- switch(len.fun, "length" = "No. observations",
                      "unique" = 'No. unique observations',
                      "percent" = '% of total observations')
  
  t2 <- ifelse(plot_by_yr, "Year", "Month")
  
  date_sym <- rlang::sym(date.var)
  var_sym <- rlang::sym(var.select)
  per_sym <- rlang::sym(agg_per)
  
  scale_lab <- function() {
    if (len.fun == "percent") scales::label_percent(scale = 1) 
    else ggplot2::waiver()
  }
  
  break_fun <- function() {
    if (plot_by_yr) unique(dataset$year)
    else levels(dataset$month)[seq(1, length(levels(dataset$month)), 3)]
  }
  
  # scatter plot
  p1 <- 
    ggplot2::ggplot(dataset, ggplot2::aes(x = !!date_sym, y = !!var_sym)) +
    ggplot2::geom_point() +
    ggplot2::labs(subtitle = paste(var.select, "by Date"), 
                  x = "Date", y = var.select) +
    fishset_theme()
  
  # length bar plot
  p2 <- 
    ggplot2::ggplot(len_df, ggplot2::aes(x = !!per_sym, y = !!var_sym)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::labs(subtitle = paste(len_title, "by", tolower(t2)), 
                  x = t2, y = "") +
    ggplot2::scale_y_continuous(labels = scale_lab()) +
    ggplot2::scale_x_discrete(breaks = break_fun()) +
    fishset_theme()
  
  if (!is.numeric(dataset[[var.select]])) p3 <- NULL
  else {
    # agg bar plot
    p3 <- 
      ggplot2::ggplot(agg_df, ggplot2::aes(x = !!per_sym, y = !!var_sym)) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::labs(subtitle = paste(simpleCap(agg.fun),"of value by", tolower(t2)),
                    x = t2, y = "") +
      ggplot2::scale_x_discrete(breaks = break_fun()) +
      fishset_theme()
  }
  t_plot <- suppressWarnings(ggpubr::ggarrange(p1, p2, p3, ncol = 3, nrow = 1))
  
  t_plot
}