# Density Plot
#'
#' Creates the KDE, CDF, or empirical CDF plots of the selected variable.
#'
#' Creates a kernel density estimate, empirical cumulative distribution function,
#' or cumulative distribution function plot of selected variable.
#'
#' @param dat Primary data containing information on hauls or trips. Table in the 
#'   FishSET database contains the string 'MainDataTable'.
#' @param project String, name of project.
#' @param var String, name of variable to plot.
#' @param type String, type of density plot. Options include "kde" (kernel density 
#'   estimate), "ecdf" (empirical cdf), "cdf" (cumulative dstribution function), 
#'   or "all" (containing all plot types). Two or more plot types can be chosen.
#' @param group Optional, string names of variables to group by. By default, grouping 
#'   variables are combined unless \code{combine = FALSE} and \code{type} is "cdf" 
#'   and/or "ecdf". If \code{type} is "kde" or "all" grouping variables are 
#'   automatically combined. "cdf" and "ecdf" plots can use up to two grouping 
#'   variables if \code{combine = FALSE}: the first variable is assigned to the 
#'   "color" aesthetic and second to the "linetype" aesthetic. 
#' @param combine Logical, whether to combine the variables listed in \code{group} 
#'   for plot. 
#' @param date Date variable from \code{dat} used to subset and/or facet the plot by.
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
#' @param facet_by Variable name to facet by. This can be a variable that exists in
#'   the dataset, or a variable created by \code{density_plot()} such as "year", 
#'   "month", or "week".
#' @param conv Convert catch variable to \code{"tons"}, \code{"metric_tons"}, or 
#'   by using a function entered as a string. Defaults to \code{"none"} for no conversion.
#' @param tran String; name of function to transform variable, for example "log" or
#'   "sqrt".
#' @param format_lab Formatting option for x-axis labels. Options include 
#'   \code{"decimal"} or \code{"scientific"}.
#' @param scale Scale argument passed to \code{\link{facet_grid}}. Defaults to "fixed". 
#'   Other options include "free_y", "free_x", and "free".
#' @param bw Adjusts KDE bandwidth. Defaults to 1.
#' @param position The position of the grouped variable for KDE plot. Options include
#'   "identity", "stack", and "fill".
#' @param pages Whether to output plots on a single page (\code{"single"}, the default) 
#'    or multiple pages (\code{"multi"}). 
#' @return Returns a KDE, empirical CDF, or CDF of a selected variable.
#'   The data can be filtered by date or by variable (see \code{filter_date} and
#'   \code{filter_by}). If \code{type} contains "kde" or "all" then grouping variables 
#'   are automatically combined. Any variable in the dataset can be used for faceting,
#'   but "year", "month", and "week" are also available.
#' @export density_plot
#' @examples
#' \dontrun{
#' density_plot(pollockMainDataTable, "pollock", var = "OFFICIAL_TOTAL_CATCH_MT", "kde", 
#'   group = "GEAR_TYPE", date = "FISHING_START_DATE", filter_date = "year-month", 
#'   filter_value = list(2011, 9:11), tran = "log"
#' )
#' }
#' @importFrom gridExtra grid.arrange
#' @importFrom shiny isRunning

density_plot <- function(dat, project, var, type = "kde", group = NULL, combine = TRUE, 
                         date = NULL, filter_date = NULL, date_value = NULL, 
                         filter_by = NULL, filter_value = NULL, filter_expr = NULL, 
                         facet_by = NULL, conv = "none", tran = "identity", 
                         format_lab = "decimal", scale = "fixed", bw = 1, 
                         position = "identity", pages = "single") {
  out <- data_pull(dat, project)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main", project)

  end <- FALSE
  suppress <- FALSE
  
  if (!is.numeric(dataset[[var]])) {
    
    warning("'var' must be numeric") 
    end <- TRUE
  }
  
  # convert date var if not date/date-time class
  if (!is.null(date)) dataset[date] <- lapply(dataset[date], date_parser)
  
  # facet setup ----
  if (!is.null(facet_by)) {
    facet_spec <- any(!(facet_by %in% names(dataset)))

    if (facet_spec == TRUE) {
      facet_s_id <- facet_by[!(facet_by %in% names(dataset))]

      if (all(facet_s_id %in% c("year", "month", "week")) == FALSE) {
        warning("Invalid facet variable.")
      } else {
        facet_by <- facet_by

        if (length(facet_by) == 0) facet_by <- NULL
      }
    }
  }

  group_date <- group[group %in% c("year", "month", "week")]
  facet_date <- facet_by[facet_by %in% c("year", "month", "week")]

  # facet date
  if (length(facet_date) > 0) {

    dataset <- facet_period(dataset, facet_date = facet_date, date = date)
  }
  
  # convert non-date facet vars to factors 
  facet_nd <- facet_by[!(facet_by %in% facet_date)]
  if (length(facet_nd) > 0) {
    
    dataset[facet_nd] <- lapply(dataset[facet_nd], as.factor)
  }
  
  # group ----
  if (!is.null(group)) {
 
    group_date2 <- group_date[!(group_date %in% facet_date)]
    
    if (length(group_date2) > 0) {
    
      dataset[group_date2] <- lapply(group_date2, function(x) {
        
        per <- switch(x, "year" = "%Y", "month" = "%b", "week" = "%U")
        
        if (per == "%b") {
          
          factor(format(dataset[[date]], per), levels = month.abb, ordered = TRUE) 
          
        } else as.integer(format(dataset[[date]], per))
      })
    }
    
    # combine group vars
    if (length(group) > 1) {
      
      if (any(type %in% c("ecdf", "cdf")) & combine == FALSE) {
        
        group1 <- group[1] 
        group2 <- group[2]
        
        dataset[group] <- lapply(dataset[group], as.factor)
      } else {
       
        dataset <- ID_var(dataset, project = project, vars = group, type = "string", 
                          log_fun = FALSE)
        group <- paste(group, collapse = "_")
      } 
      
    } else {
      group_nd <- group[!(group %in% group_date)]
      
      if (length(group_nd) > 0) {
        dataset[group_nd] <- lapply(group_nd, function(x) as.factor(dataset[[x]]))
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
  # filter by date ----
  if (!is.null(filter_date)) {
    
    if (is.null(date_value)) {
      
      warning("'date_value' must be provided.")
      end <- TRUE
    }
    
    dataset <- subset_date(dataset, date, filter_date, date_value)
    
    if (nrow(dataset) == 0) {
          warning("Filtered data table has zero rows. Check filter parameters.")
          end <- TRUE
    }
  }
  
  # convert pounds 
  if (conv != "none") {
    
    if (conv == "tons") {
      
      dataset[[var]] <- dataset[[var]]/2000
      
    } else if (conv == "metric_tons") {
      
      dataset[[var]] <- dataset[[var]]/2204.62
      
    } else if (is.function(conv)) {
      
      dataset[[var]] <- do.call(conv, list(dataset[[var]]))
    }
  }
  
  if (end == FALSE) {
    
    # confidentiality checks ----
    if (run_confid_check(project)) {
      
      cc_par <- get_confid_check(project)
      
      if (cc_par$rule == "n") {
        
        check_out <- 
          check_and_suppress(dataset[c(cc_par$v_id, var, group, facet_by)], 
                             output = NULL, project, cc_par$v_id, value_var = var,
                             group = group, rule = "n", value = cc_par$value, 
                             type = "NA")
        
        suppress <- check_out$suppress
      }
    }
    # remove unnecessary columns
    dataset <- dataset[c(var, group, facet_by)]
    
    # Plots ----
    group_list <- list(group = group,
                       group1 = get0("group1"),
                       group2 = get0("group2"))
    
    d_plot <- 
      dens_plot_helper(dataset, var, group_list, date, facet_by, filter_date, 
                       date_value, type, bw, conv, tran, format_lab, scale, position, 
                       pages)
    
    if (suppress) {
      
      conf_plot <- 
        suppressWarnings(
          dens_plot_helper(check_out$table, var, group_list, date, facet_by, filter_date, 
                           date_value, type, bw, conv, tran, format_lab, scale, position,
                           pages)
        )
        
      if (pages == "multi") {
        lapply(seq_along(conf_plot$multi), function(x) {
          save_plot(project, paste0("density_plot_confid_", x), conf_plot$multi[[x]])
        })
        
      } else save_plot(project, "density_plot_confid", conf_plot$single)
    }
    
    print_plot <- function() {
      if (length(type) == 1 & !("all" %in% type)) d_plot$single
      
      else if (pages == "single") {
        
        if (length(type) > 1 | "all" %in% type) gridExtra::grid.arrange(d_plot$single)
        
      } else if (pages == "multi") {
        
        if (shiny::isRunning()) d_plot$multi
        else d_plot$single
      }
    }
    
    # Log the function
    density_plot_function <- list()
    density_plot_function$functionID <- "density_plot"
    density_plot_function$args <- list(
      dat, project, var, type, group, combine, date, filter_date, date_value, 
      filter_by, filter_value, filter_expr, facet_by, conv, tran, format_lab, 
      scale, bw, position, pages)
    
    log_call(project, density_plot_function)
    
    # Save output
    if (pages == "multi") {
      
      lapply(seq_along(d_plot$multi), function(x) {
        save_plot(project, paste0("density_plot_", x), d_plot$multi[[x]])
      })
      
    } else save_plot(project, "density_plot", d_plot$single)
    
    print_plot()
  }
}
  

dens_plot_helper <- function(dataset, var, group, date, facet_by, filter_date, 
                             date_value, type, bw, conv, tran, format_lab, scale, 
                             position, pages) {
  #' density_plot helper function
  #' 
  #' Creates and formats plots
  #' 
  #' @param dataset Data used to create plot. 
  #' @param var String, variable passed from \code{density_plot}.
  #' @param group String, grouping variable(s) passed from \code{density_plot}.
  #' @param date String, date variable passed from \code{density_plot}.
  #' @param facet_by String, facet variable(s) passed from \code{density_plot}.
  #' @param filter_date String, date filter type passed from \code{density_plot}.
  #' @param date_value Numeric, date filter value passed from \code{density_plot}.
  #' @param type String, plot type(s) passed from \code{density_plot}.
  #' @param bw Numeric, bandwidth passed from \code{density_plot}.
  #' @param conv String, convert pounds to "tons" or "metric_tons".
  #' @param tran String, scale transformation passed from \code{density_plot}.
  #' @param scale Scale argument passed to \code{\link{facet_grid}}. Defaults to "fixed". 
  #'   Other options include "free_y", "free_x", and "free".
  #' @param position String, plot position passed from \code{density_plot}.
  #' @param format_lab String, label formatting option passed from \code{density_plot}.
  #' @param pages String, single or multiple plots passed from \code{density_plot}.
  #'
  #' @keywords internal
  #' @import ggplot2
  #' @importFrom dplyr across group_by mutate
  #' @importFrom gridExtra arrangeGrob marrangeGrob
  #' @importFrom stats pnorm reformulate
  #' @importFrom rlang sym expr
  #' @importFrom scales breaks_extended label_number label_scientific log_breaks
  #' @importFrom shiny isRunning
  
  group1 <- group$group1
  group2 <- group$group2
  group <- group$group
  cdf <- NULL
  
  facet_date <- facet_by[facet_by %in% c("year", "month", "week")]
  
  var_sym <- rlang::sym(var)
  
  var_exp <- function() {
    
    if (tran == "sqrt") rlang::expr(sqrt(!!var_sym))
    else var_sym
  }
  
  color_exp <- function() {
    
    if (is.null(group)) NULL
    else if (length(group) == 1) rlang::sym(group)
    else if (length(group) > 1) rlang::sym(group1)
  }
  
  linetype_exp <- function() {
    if (length(group) > 1) rlang::sym(group2)
    else NULL
  }
  
  x_lab <- function() {
    
    f_conv <- function() {
      
      if (conv != "none") {
        
        c_lab <- switch(conv, "tons" = "T", "metric_tons" = "MT", "")
        
        paste0("(", c_lab, ")")
        
      } else NULL
    } 
    
    f_tran <- if (tran != "identity") paste(tran, "scale") else NULL
    
    paste(var, f_conv(), f_tran)
  }
  
  x_breaks <- function() {
    if (tran != "identity") {
      
      brk_num <- nchar(trunc(max(dataset[[var]], na.rm = TRUE)))
      brk_num <- ifelse(length(brk_num) < 5, 5, brk_num)
      
      if (tran %in% c("log", "log2", "log10")) {
        
        x_base <- switch(tran, "log" = exp(1), "log2" = 2, "log10" = 10)
        
        scales::log_breaks(n = brk_num, base = x_base)
        
      } else {
        
        scales::breaks_extended(n = brk_num + 2)
      }
      
    } else ggplot2::waiver()
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
  
  if ("all" %in% type) plot_type <- c("kde", "ecdf", "cdf")
  else plot_type <- type
  
  plot_list <-
    lapply(plot_type, function(x) {
      
      if (x == "kde") {
        
        plot <- 
          ggplot2::ggplot(dataset, ggplot2::aes(x = !!var_exp(), fill = !!color_exp())) +
          ggplot2::stat_density(position = position, color = "black", 
                                alpha = .7, adjust = bw) +
          ggplot2::labs(title = "Kernel Density",
                        x = x_lab(),
                        caption = paste("kernel bandwidth:", bw)) +
          fishset_theme() +
          ggplot2::theme(legend.position = "bottom")
        
      } else if (x == "ecdf") {
        
        if (is.null(group)) {
          
          plot <- 
            ggplot2::ggplot(dataset, ggplot2::aes(x = !!var_exp())) +
            ggplot2::stat_ecdf(geom = "area", alpha = .7) +
            ggplot2::stat_ecdf(geom = "step", alpha = .7) +
            ggplot2::labs(title = "Empirical Cumulative Distribution Function",
                          x = x_lab(), y = "probability") +
            fishset_theme() +
            ggplot2::theme(legend.position = "bottom")
          
        } else {
          
          plot <- 
            ggplot2::ggplot(dataset, ggplot2::aes(x = !!var_exp())) +
            ggplot2::stat_ecdf(ggplot2::aes(color = !!color_exp(), 
                                            linetype = !!linetype_exp()), 
                               geom = "step") +
            ggplot2::labs(title = "Empirical Cumulative Distribution Function",
                          x = x_lab(), y = "probability") +
            fishset_theme() +
            ggplot2::theme(legend.position = "bottom")
        }
        
      } else if (x == "cdf") {
        
        if (!is.null(group)) {
          
          dataset <- dataset %>% 
            dplyr::group_by(dplyr::across(group)) %>% 
            dplyr::mutate(cdf = pnorm(!!var_sym, 
                                      mean(!!var_sym, na.rm = TRUE), 
                                      sd(!!var_sym, na.rm = TRUE)))
          
          plot <- 
            ggplot2::ggplot(dataset, ggplot2::aes(!!var_exp())) +
            ggplot2::geom_line(ggplot2::aes(y = cdf, color = !!color_exp(), 
                                            linetype = !!linetype_exp()))
        } else {
          
          dataset$cdf <- 
            stats::pnorm(dataset[[var]], 
                         mean = mean(dataset[[var]], na.rm = TRUE), 
                         sd = sd(dataset[[var]], na.rm = TRUE))
          
          plot <- 
            ggplot2::ggplot(dataset, ggplot2::aes(!!var_exp())) +
            ggplot2::geom_area(ggplot2::aes(y = cdf), position = "identity", alpha = .7) 
          
        }
        
        plot <-  plot +
          ggplot2::labs(title = "Cumulative Distribution Function",
                        y = "probability", x = x_lab()) +
          fishset_theme() +
          ggplot2::theme(legend.position = "bottom")
      }
      
      if (!is.null(facet_by)) {
        if (length(facet_by) == 1) {
          fm <- stats::reformulate(".", facet_by)
          plot <- plot + ggplot2::facet_wrap(fm, scales = scale)
        } else if (length(facet_by) == 2) {
          fm <- paste(facet_by, sep = " ~ ")
          plot <- plot + ggplot2::facet_grid(fm, scales = scale)
        }
      }
      
      plot <- plot + ggplot2::scale_x_continuous(trans = f_tran(), 
                                                 breaks = x_breaks(),
                                                 labels = x_labeller())
      
      # add date to title
      if (!is.null(date) & !is.null(filter_date) & 
          !is.null(date_value) & length(facet_date) == 0) {
        plot <- date_title(plot, filter_date, date_value)
      }
      
      # remove legend if single paged
      if (pages == "single") {
        
        if (length(type) > 1 | "all" %in% type) {
          
          plot <- plot + ggplot2::theme(legend.position = "none")
        }
      }
      
      plot
    })
  
  # arranging plot
  if (pages == "single") {
    if (length(type) > 1 | "all" %in% type) {
      if (!is.null(group)) {
        # add grouping var
        cdf_linetype <- function() {
          if (all(type %in% c("cdf", "ecdf"))) linetype_exp()
          else NULL
        }
        
        grp <- ggplot2::ggplot(dataset, ggplot2::aes(0, 0, color = !!color_exp(),
                                                     linetype = !!cdf_linetype())) +
          ggplot2::geom_point() + ggplot2::geom_line() +
          ggplot2::theme(legend.position = "bottom", legend.box = "horizontal",
                         legend.direction = "horizontal")
        
        # extract legend
        tmp <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(grp))
        leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
        legend <- tmp$grobs[[leg]]
      }
      
      if (length(type) == 2)  p_layout <- cbind(c(1, 2), c(1, 2))
      else p_layout <- cbind(c(1, 2), c(1, 3))
      
      d_plot <- gridExtra::arrangeGrob(grobs = plot_list, layout_matrix = p_layout)
      
      if (!is.null(group)) {
        d_plot <- gridExtra::arrangeGrob(d_plot, legend, ncol = 1, 
                                         heights = c(.9, .1))
      }
      
    } else d_plot <- plot_list[[1]]
    
  } else if (pages == "multi" & !shiny::isRunning()) {
    
    d_plot <- gridExtra::marrangeGrob(plot_list, nrow = 1, ncol = 1, top = NULL)
  }
  
  if (length(type) == 1 & !("all" %in% type)) {
    
    d_plot <- plot_list[[1]]
  }
  
  list(multi = plot_list, single = get0("d_plot"))
}