temp_plot <- function(dat, project, var.select, len.fun = "length", agg.fun = "mean", 
                      date.var = NULL, alpha = .5, pages = "single", text.size = 8) {
  #' Plot variable by month/year
  #'
  #' Returns three plots showing the variable of interest against time (as month 
  #' or month/year). Plots are raw points by time, number of observations by time, 
  #' and aggregated variable of interest by time.
  #' @param dat Primary data containing information on hauls or trips.
  #'   Table in FishSET database contains the string 'MainDataTable'.
  #' @param project String, name of project.
  #' @param var.select Variable in \code{dat} to plot against a date variable.
  #' @param len.fun Method, \code{"length"} returns the number of observations, 
  #'   \code{"unique"} returns the number of unique observations, \code{"percent"}
  #'   returns the percentage of total observations.
  #' @param agg.fun Method to aggregate \code{var.select} by date. Choices are 
  #'   \code{"mean"}, \code{"median"}, \code{"min"}, \code{"max"}, or \code{"sum"}.
  #' @param date.var Date variable in \code{dat}. Defaults to first date variable 
  #'   in \code{dat} set if not defined.
  #' @param alpha The opaqueness of each data point in scatterplot. 0 is total 
  #'  transparency and 1 is total opaqueness.  Defaults to .5. 
  #' @param pages Whether to output plots on a single page (\code{"single"}, the 
  #'   default) or multiple pages (\code{"multi"}). 
  #' @param text.size Text size of x-axes. 
  #' @keywords plot, temporal, exploration
  #' @description Returns three plots showing the variable of interest against time 
  #'   (as month or month/year). Plots are raw points by date, number of observations 
  #'   by date, and measures of a representative observation by date.
  #' @return Returns plot to R console and saves output to the Output folder.
  #' @import ggplot2
  #' @importFrom dplyr n_distinct
  #' @export
  #' @examples
  #' \dontrun{
  #' temp_plot(pollockMainDataTable, project='pollock', 
  #'           var.select = 'OFFICIAL_TOTAL_CATCH_MT', len.fun = 'percent', 
  #'           agg.fun = 'mean', date.var = 'HAUL_DATE')
  #'           
  #' temp_plot(pollockMainDataTable, project='pollock', 
  #'           var.select = 'OFFICIAL_TOTAL_CATCH_MT', len.fun = 'length',
  #'           agg.fun = 'max')
  #' }
  #' 
  # Call in datasets
  out <- data_pull(dat, project)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main", project)
  
  suppress <- FALSE
  
  # Date var - NULL or use the first one
  if (is.null(date.var)) {
    
    date.var <- date_cols(dataset)[1]
    if (length(date.var) == 0) {
      stop("No valid date columns detected in dataset.", call. = FALSE)
    }
    
  } else {
    
    if (!(date.var %in% date_cols(dataset))) {
      stop(date.var, " column is not a date variable.", call. = FALSE)
    } 
  }
  
  if (grepl("date", var.select, ignore.case = TRUE)) {
    
    if (var.select %in% date_cols(dataset)) {
      
      dataset[[var.select]] <- date_parser(dataset[[var.select]])
      
    } else {
      
      stop(var.select, " column is not a date variable.", call. = FALSE)
    }
  }
   
  dataset[[date.var]] <- date_parser(dataset[[date.var]])
  
 
  # add year and month columns
  dataset <- facet_period(dataset, c("year", "month"), date.var)
  
  plot_by_yr <- length(unique(dataset$year)) > 1
  
  agg_per <- ifelse(plot_by_yr, "year", "month")
  
  # TODO: expand_data() is dropping year/month cols, need to fix
  # add missing dates, if any
  # dataset <- expand_data(dataset, project, date = date.var, value = var.select, 
  #                        period = agg_per, fun = "count")
  
  if (plot_by_yr) dataset$year <- as.factor(dataset$year)
  
  temp_fun <- switch(len.fun, 
                     "length" = function(x) sum(!is.na(x)), 
                     "unique" = function(x) dplyr::n_distinct(x, na.rm = TRUE),
                     "percent" = function(x) {
                       round(sum(!is.na(x)) / nrow(dataset) * 100, 2)
                     }
  )
  
  # summary tables ----
  len_df <- agg_helper(dataset, var.select, group = agg_per, fun = temp_fun)
  agg_df <- agg_helper(dataset, var.select, group = agg_per, fun = agg.fun)
  
  # confidentiality checks ----
  if (run_confid_check(project)) {
    cc_par <- get_confid_check(project)
    if (cc_par$rule == "n") {
      
      len_df_c <- check_and_suppress(dataset, len_df, project, cc_par$v_id, var.select, 
                                     group = agg_per, rule = "n", value = cc_par$value,
                                     type = "NA")
      agg_df_c <- check_and_suppress(dataset, agg_df, project, cc_par$v_id, var.select, 
                                     group = agg_per, rule = "n", value = cc_par$value,
                                     type = "NA")
      
      suppress <- any(len_df_c$suppress, agg_df_c$suppress)
    }
  }
  
  # plots ----
  
  t_plot <- temp_plot_helper(dataset, len_df, agg_df, var.select, date.var, agg_per, 
                             len.fun, agg.fun, plot_by_yr, alpha, pages, text.size)
  
  if (suppress) {
    
    t_plot_c <- temp_plot_helper(dataset, len_df_c$table, agg_df_c$table, var.select, 
                                 date.var, agg_per, len.fun, agg.fun, plot_by_yr, 
                                 alpha, pages, text.size)
    
    if (pages == "single") save_plot(project, "temp_plot_confid", plot = t_plot_c)
    else save_nplot(project, "temp_plot_confid", plot_list = t_plot_c, id = "name")
    
  }
  
  # Log the function
  temp_plot_function <- list()
  temp_plot_function$functionID <- "temp_plot"
  temp_plot_function$args <- list(dat, project, var.select, len.fun, agg.fun, 
                                  alpha, pages, date.var)
  log_call(project, temp_plot_function)
  
  # Save output
  if (pages == "single") save_plot(project, "temp_plot", plot = t_plot)
  else save_nplot(project, "temp_plot", plot_list = t_plot, id = "name")
  
  t_plot
}

temp_plot_helper <- function(dataset, len_df, agg_df, var.select, date.var, agg_per, 
                             len.fun, agg.fun, plot_by_yr, alpha, pages, text.size) {
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
  #' @param alpha transparency of data points.
  #' @param pages Whether to output plots on a single page (\code{"single"}, the 
  #'    default) or multiple pages (\code{"multi"}).  
  #' @param text.size Text size of x-axes. 
  #' @importFrom rlang sym
  #' @importFrom gridExtra grid.arrange
  #' @import ggplot2
  #' @export
  #' @keywords internal
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
    # else ggplot2::waiver()
    else scales::comma
  }
  
  break_fun <- function() {
    if (plot_by_yr) unique(dataset$year)
    else levels(dataset$month)[seq(1, length(levels(dataset$month)), 3)]
  }
  
  # scatter plot
  p1 <- 
    ggplot2::ggplot(dataset, ggplot2::aes(x = !!date_sym, y = !!var_sym)) +
    ggplot2::geom_point(alpha = alpha) +
    ggplot2::scale_y_continuous(labels = scales::comma) + 
    ggplot2::labs(subtitle = paste(var.select, "by Date"), 
                  x = "Date", y = var.select) +
    fishset_theme() + 
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = text.size, angle = 45, hjust = 1),
                   axis.text.y = ggplot2::element_text(size = text.size, angle = 45))
  
  # length bar plot
  p2 <- 
    ggplot2::ggplot(len_df, ggplot2::aes(x = !!per_sym, y = !!var_sym)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::labs(subtitle = paste(len_title, "by", tolower(t2)), 
                  x = t2, y = "") +
    ggplot2::scale_y_continuous(labels = scale_lab()) +
    ggplot2::scale_x_discrete(breaks = break_fun()) +
    fishset_theme() + 
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = text.size, angle = 45, hjust = 1),
                   axis.text.y = ggplot2::element_text(size = text.size, angle = 45))
  
  if (!is.numeric(dataset[[var.select]])) p3 <- NULL
  else {
    # agg bar plot
    if (pages == "multi") c_val <- var.select
    else c_val <- "value"
    
    p3 <- 
      ggplot2::ggplot(agg_df, ggplot2::aes(x = !!per_sym, y = !!var_sym)) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::scale_y_continuous(labels = scales::comma) + 
      ggplot2::labs(subtitle = paste(simpleCap(agg.fun),"of", c_val, "by", tolower(t2)),
                    x = t2, y = "") +
      ggplot2::scale_x_discrete(breaks = break_fun()) +
      fishset_theme() + 
      ggplot2::theme(axis.text.x = ggplot2::element_text(size = text.size, angle = 45, hjust = 1),
                     axis.text.y = ggplot2::element_text(size = text.size, angle = 45))
  }
  
  if (pages == "single") {
    
    t_plot <- gridExtra::grid.arrange(p1, p2, p3, ncol = 3, nrow = 1)
    
  } else {
    
    t_plot <- list(scatter_plot = p1, unique_plot = p2, agg_plot = p3)
  }
 
  t_plot
}
