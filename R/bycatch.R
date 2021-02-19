# Bycatch
#'
#' Compare bycatch CPUE and total catch/percent of catch to other species
#'
#' @param dat Main data frame over which to apply function. Table in FishSET 
#'   database should contain the string `MainDataTable`.
#' @param project name of project.
#' @param cpue A string of CPUE variable names. The function outputs 
#'   the mean CPUE by period. The variable names must match the order of variable 
#'   names in \code{catch} and \code{names}.    
#' @param catch A character string of names of catch variables to aggregate. The 
#'   function outputs the total catch or share of total catch by period depending 
#'   on the value argument. The order of the catch variable string must match those 
#'   of the \code{cpue} and \code{names} arguments.  
#' @param date A variable containing dates to aggregate by.
#' @param period Period to aggregate by. Options include 'year', month', and weeks'.
#' @param names An optional string of species names that will be used in the plot. If
#'   \code{NULL}, then species names from \code{catch} will be used. 
#' @param group A categorical variable in \code{dat} to group by.
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
#' @param facet_by Variable name to facet by. Accepts up to two variables. Facetting 
#'   by \code{"year"}, \code{"month"}, or \code{"week"} is available if a date variable 
#'   is added to \code{sub_date}.
#' @param value Whether to return raw catch ("raw") or share of total catch ('stc'). 
#' @param tran A function to transform the y-axis. Options include log, log2, log10, sqrt.
#' @param combine Logical, whether to combine variables listed in \code{group}. 
#' @param scale Scale argument passed to \code{\link{facet_grid}}. Defaults to \code{"fixed"}.
#'   Other options include \code{"free_y"}, \code{"free_x"}, and \code{"free_xy"}. 
#' @param output Output type. Options include 'table' or 'plot'.
#' @param format_tab How table output should be formatted. Options include \code{'wide'}
#'   (the default) and \code{'long'}.
#' @details Returns a plot and/or table of the mean CPUE and share of total catch or 
#'  raw count for each species entered. For optimal plot size in an R Notebook/Markdown 
#'  document, we recommend including no more than four species. The order of variables 
#'  in the \code{cpue} and \code{catch} arguments must be in the same order as in the 
#'  \code{names} argument. The \code{names} argument is used to join the \code{catch} 
#'  and \code{cpue} variables together.
#' @return \code{bycatch()} compares the average CPUE and catch total/share of total
#'   catch between one or more species. The data can be filtered by date and/or by a 
#'   variable. \code{filter_date} specifies the type of date filter to apply--by 
#'   date-range or by period. \code{date_value} should contain the values to filter 
#'   the data by. To filter by a variable, enter its name as a string in \code{filter_by}
#'   and include the values to filter by in \code{filter_value}. Only one grouping 
#'   variable will be displayed; however, any number of variables can be combined 
#'   by using \code{combine = TRUE}, but no more than three is recommended. For faceting, 
#'   any variable in the dataset can be used, but "year" and "month" are also available 
#'   provided a date variable is added to \code{sub_date}. Generally, no more than four species
#'   should be compared, and even fewer when faceting due to limited plot space.
#'   A list containing a table and plot are printed to the console and viewer by default.
#'   For optimal plot size in an R Notebook/Markdown document, use the chunk option
#'   \code{fig.asp = 1}.
#' @examples
#' \dontrun{
#' cpue(pollockMainDataTable, "myproject", xWeight = "f1Weight",
#'   xTime = "Hour", "f1_cpue"
#' )
#'
#' bycatch(pollockMainDataTable, "myproject", cpue = c("f1_cpue", "f2_cpue", "f3_cpue", "f4_cpue"),
#'   catch = c("f1", "f2", "f3", "f4"), date = "FISHING_START_DATE",
#'   names = c("fish_1", "fish_2", "fish_3", "fish_4"), period = "month",
#'   date_filter = "year", date_value = 2011
#'   , value = "stc", output = "table"
#' )
#' }
#' @export bycatch
#' @import ggplot2
#' @importFrom stats aggregate reformulate
#' @importFrom purrr pmap
#' @importFrom dplyr left_join
#' @importFrom gridExtra grid.arrange arrangeGrob
#' @importFrom tidyr pivot_longer
#' @importFrom scales percent
#' @importFrom rlang sym as_string
#' @importFrom shiny isRunning

bycatch <- function(dat, project, cpue, catch, date, period = "year", names = NULL, 
                    group = NULL, sub_date = NULL, filter_date = NULL, date_value = NULL, 
                    filter_by = NULL, filter_value = NULL, filter_expr = NULL, facet_by = NULL, 
                    tran = "identity", value = "stc", combine = FALSE, scale = "fixed",
                    output = "tab_plot", format_tab = "wide") {
  
  # Call in datasets
  out <- data_pull(dat)
  dataset <- out$dataset
  
  if (shiny::isRunning()) {
    if (deparse(substitute(dat)) == "values$dataset") dat <- get("dat_name")
  } else { 
    if (!is.character(dat)) dat <- deparse(substitute(dat)) }

  end <- FALSE
  
  group_date <- group[group %in% c("year", "month", "week")]
  facet_date <- facet_by[facet_by %in% c("year", "month", "week")]
  facet_no_date <- facet_by[!(facet_by %in% c("year", "month", "week"))]
  group_no_date <- group[!(group %in% c("year", "month", "week"))]
  
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
  
  # filter date ----
  if (!is.null(filter_date)) {
    
    dataset <- subset_date(dataset, sub_date, filter_date, date_value)
    
    if (nrow(dataset) == 0) {
      
      warning("Filtered data table has zero rows. Check filter parameters.")
      end <- TRUE
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
  
  # Names joining table ----
  if (is.null(names)) {
    
    name_tab <- data.frame(species = catch, species_catch = catch, species_cpue = cpue)
    
  } else {
    
    name_tab <- data.frame(species = names, species_catch = catch, species_cpue = cpue)
  }
  
  # period ----
  periods <- c("year", "month", "weeks")
  
  if (period %in% periods == FALSE) {
    
    warning("Invalid period. Please select a valid period name (see documentation for details).")
    end <- TRUE
  } else {
    
    p <- switch(period, year = "%Y", month = "%b", weeks = "%U")
  }
  
  # add missing ----
  dataset <- add_missing_dates(dataset, date = date, sub_date = sub_date, 
                               value = c(cpue, catch), group = group_no_date, 
                               facet_by = facet_no_date)
  
  if (period != "year") {
    
    dataset$year <- as.integer(format(dataset[[date]], "%Y"))
  }
  
  dataset[[period]] <- format(dataset[[date]], p)
  
  # facet date ----
  if (!is.null(facet_by)) {
    if (length(facet_date) > 0) {
      
      if (period != "month" & any("month" %in% facet_date)) {
        
        dataset$month <- factor(format(dataset[[sub_date]], "%b"), 
                                levels = month.abb, ordered = TRUE)
        
      } else if (period != "week" & any("week" %in% facet_date)) {
        
        dataset$week <- as.integer(format(dataset[[sub_date]], "%U"))
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
    # Mean CPUE summary table ----
    agg_grp <- c("year", group, facet_by, facet_date)
    
    cpue_tab <- agg_helper(dataset, value = cpue, period = period, group = agg_grp, 
                           fun = mean)
    
    if (length(cpue) > 1) {
      
      cpue_tab <- tidyr::pivot_longer(cpue_tab, cols = !!cpue, names_to = "species_cpue", 
                                      values_to = "mean_cpue")
    }
    
    # Total catch/STC table ----
    catch_tab <- agg_helper(dataset, value = catch, period = period, group = agg_grp, 
                            fun = sum)
    
    if (length(catch) > 1) {
      
      catch_tab <- tidyr::pivot_longer(catch_tab, cols = !!catch, names_to = "species_catch", 
                                       values_to = "catch")
    }
    # STC conversion ---- 
    f_catch <- function() if (length(catch) > 1) "catch" else catch
    catch_grp <- function() if (length(catch) > 1) "species_catch" else NULL
    
    if (value == "stc") {
      
      stc_tab <- agg_helper(catch_tab, value = f_catch(), group = catch_grp(), 
                            fun = function(x) x/sum(x))
      
      if (is.vector(stc_tab$catch)) {
        catch_tab$stc <- stc_tab$catch
      } else {
        if (nrow(stc_tab$catch) == 1) {
          catch_tab$stc <- as.vector(stc_tab$catch) 
        } else {
          catch_tab$stc <- as.vector(t(stc_tab$catch))
        }
      }
    }
    
    # Join catch and effort tables ----
    if (length(catch) == 1) {
      
      bycatch <- dplyr::left_join(cpue_tab, catch_tab, by = unique(c(period, agg_grp)))
      
    } else {
      
      cpue_tab <- dplyr::left_join(cpue_tab, name_tab, by = "species_cpue")
      bycatch <- dplyr::left_join(cpue_tab, catch_tab, 
                                  by = unique(c(period, agg_grp, "species_catch")))
    }
    
    if (p == "%b") {
      
      bycatch <- date_factorize(bycatch, period, p)
      
    } else {
      
      bycatch[[period]] <- as.integer(bycatch[[period]])
    }
    
    bycatch <- bycatch[order(bycatch[[period]]), ]
    
    
    if (output %in% c("tab_plot", "plot")) {
      
      # plot functions ----
      species_exp <- function() if (length(catch) > 1) rlang::sym("species") else NULL
      species_exp2 <- function() if (!is.null(species_exp())) rlang::as_string(species_exp()) else NULL
      group_exp <- function() if (is.null(group)) 1 else rlang::sym(group1)
      color_exp <- function() if (is.null(group)) NULL else rlang::sym(group1)
      names <- as.character(name_tab$species)
      
      # Plots ----
      cpue_plots <- lapply(names, function(x) { # plot for each species 
        
        cpue_exp <- function() if (length(cpue) > 1) rlang::sym("mean_cpue") else rlang::sym(cpue)
        cpue_cols <- unique(c(period, agg_grp, species_exp2(), rlang::as_string(cpue_exp())))
        
        if (length(cpue) == 1) {
          cpue_dat <- bycatch[cpue_cols]
        } else {
          cpue_dat <- bycatch[bycatch$species == x, cpue_cols]
        }
        
        cp_plot <- ggplot2::ggplot(cpue_dat, ggplot2::aes(!!rlang::sym(period), 
                                                          !!cpue_exp(), 
                                                          group = !!group_exp(),
                                                          color = !!color_exp())) + 
          ggplot2::geom_point(size = 1) + 
          ggplot2::geom_line(size = 0.65) + 
          ggplot2::labs(title = "CPUE", y = "average CPUE") + 
          fishset_theme() + 
          ggplot2::scale_y_continuous(trans = tran) +
          ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 9), 
                         axis.title = ggplot2::element_text(size = 7), 
                         axis.text = ggplot2::element_text(size = 7), 
                         legend.position = "none")
        
        if (!is.null(facet_by)) {
          
          if (length(facet_by) == 1) {
            
            fm <- stats::reformulate(".", facet_by)
            
          } else if (length(facet_by) == 2) {
            
            fm <- paste(facet_by, sep = " ~ ")
          }
          
          cp_plot <- cp_plot + ggplot2::facet_grid(fm, scales = scale)
        }
        
        if (period == "year") {
          
          cp_plot <- cp_plot + 
            ggplot2::scale_x_continuous(breaks = num_breaks(cpue_dat[[period]]))
          
        } else if (period == "weeks") {
          
          cp_plot <- cp_plot + 
            ggplot2::scale_x_continuous(breaks = num_breaks(cpue_dat[[period]]), 
                                        labels = week_labeller(num_breaks(cpue_dat[[period]]),
                                                               cpue_dat$year))
        }
        
        cp_plot
      })
      
      catch_plots <- lapply(names, function(x) {
        
        catch_exp <- function() {
          if (value == "stc") {
            rlang::sym("stc") 
          } else if (length(catch) > 1) {
            rlang::sym("catch") 
          } else { 
            rlang::sym(catch)
          }
        }
        catch_cols <- unique(c(period, agg_grp, species_exp2(), rlang::as_string(catch_exp())))
        
        if (length(catch) == 1) {
          catch_dat <- bycatch[catch_cols] 
        } else {
          catch_dat <- bycatch[bycatch$species == x, catch_cols] 
        }
        
        ca_plot <- ggplot2::ggplot(catch_dat, ggplot2::aes(!!rlang::sym(period), 
                                                           !!catch_exp(), 
                                                           group = !!group_exp(), 
                                                           color = !!color_exp())) + 
          ggplot2::geom_point(size = 1) + 
          ggplot2::geom_line(size = 0.65) + 
          ggplot2::labs(title = if (value != "stc") "Total" else "STC", 
                        y = if (value == "total")  "total catch" else "share of catch") + 
          fishset_theme() + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 9), 
                                           axis.title = ggplot2::element_text(size = 7),
                                           axis.text = ggplot2::element_text(size = 7), 
                                           legend.position = "none") +
          ggplot2::scale_y_continuous(labels = if (value == "stc") scales::percent else ggplot2::waiver(),
                                      trans = tran)
        
        if (!is.null(facet_by)) {
          
          if (length(facet_by) == 1) {
            
            fm <- stats::reformulate(".", facet_by)
            
          } else if (length(facet_by) == 2) {
            
            fm <- paste(facet_by, sep = " ~ ")
          }
          
          ca_plot <- ca_plot + ggplot2::facet_grid(fm, scales = scale)
        }
        
        if (period == "year") {
          
          ca_plot <- 
            ca_plot + ggplot2::scale_x_continuous(breaks = num_breaks(catch_dat[[period]]))
          
        } else if (period == "weeks") {
          
          ca_plot <- ca_plot + 
            ggplot2::scale_x_continuous(breaks = num_breaks(catch_dat[[period]]),
                                                           labels = week_labeller(num_breaks(catch_dat[[period]]),
                                                                                  catch_dat$year))
        }
        
        ca_plot
      })
      
      # combine catch and cpue plots by species
      plot_list <- 
        purrr::pmap(list(x = cpue_plots, y = catch_plots, p_nm = names), 
                    function(x, y, p_nm) {
        
        gridExtra::arrangeGrob(x, y, nrow = 1, top = p_nm)
      })
      # combine all species plots into one 
      by_plot <- do.call(gridExtra::arrangeGrob, c(plot_list, ncol = 1))
      
      if (!is.null(group)) {
        # add grouping var
        grp <- ggplot2::ggplot(bycatch, ggplot2::aes(0, 0, color = !!rlang::sym(group1))) + 
          ggplot2::geom_point() + 
          ggplot2::theme(legend.position = "bottom")
        
        # extract legend
        tmp <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(grp))
        leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
        legend <- tmp$grobs[[leg]]
        
        by_plot <- gridExtra::arrangeGrob(by_plot, legend, ncol = 1, heights = c(.9, .1))
      }
      
      f_plot <- function() {
        if (shiny::isRunning()) {
          by_plot
        } else {
          gridExtra::grid.arrange(by_plot, ncol = 1)
        }
      }
      
      save_plot(project, "bycatch", by_plot)
    }
    # remove extra cols used for joining 
    bycatch[c("species_catch", "species_cpue")] <- NULL
    
    if (format_tab == "long") {
     
      stc <- if (value == "stc") "stc" else NULL
      cols <- c("mean_cpue", "catch", stc)
  
      bycatch <- tidyr::pivot_longer(bycatch, cols = !!cols, names_to = "measure", 
                                     values_to = "value")
    }
    
    save_table(bycatch, project, "bycatch")
    
    # Log Function
    bycatch_function <- list()
    bycatch_function$functionID <- "bycatch"
    bycatch_function$args <- list(dat, project, cpue, catch, date, period, names, group, 
                                  sub_date, filter_date, date_value, filter_by, filter_value, 
                                  filter_expr, facet_by, tran, value, combine, scale, 
                                  output, format_tab)
    log_call(bycatch_function)
    
    if (output == "plot") {
      
      f_plot()
      
    } else if (output == "table") {
      
      bycatch
      
    } else {
      
      out_list <- list(table = bycatch,
                       plot = f_plot())
      out_list
    }
  }
}
