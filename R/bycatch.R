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
#' @param filter_date The type of filter to apply to table. The "date_range" option will subset 
#'   the data by two date values entered in \code{filter_val}. Other options include "year-day", 
#'   "year-week", "year-month", "year", "month", "week", or "day". The argument filter_value must 
#'   be provided. 
#' @param date_value String containing a start and end date if using filter_date = "date_range", 
#'   e.g. c("2011-01-01", "2011-03-15"). If filter_date = "period" or "year-period", use integers 
#'   (4 digits if year, 1-2 if day, month, or week). Use a list if using a two-part filter, e.g. "year-week",
#'   with the format \code{list(year, period)} or a vector if using a single period, \code{c(period)}. 
#'   For example, \code{list(2011:2013, 5:7)} will filter the data table from weeks 5 through 7 for 
#'   years 2011-2013 if filter_date = "year-week".\code{c(2:5)} will filter the data
#'   February through May when filter_date = "month".
#' @param filter_by String, variable name to filter by.
#' @param filter_value A vector of values to filter `MainDataTable` by using the variable 
#'   in \code{filter_by}. 
#' @param filter_expr String, a valid R expression to filter `MainDataTable` by using the variable 
#'   in \code{filter_by}. 
#' @param facet_by Variable name to facet by. This can be a variable that exists in 
#'   the dataset, or a variable created by \code{bycatch()} such as \code{"year"}, 
#'   \code{"month"}, or \code{"week"}.  
#' @param value Whether to return raw catch ("raw") or share of total catch ('stc'). 
#' @param tran A function to transform the y-axis. Options include log, log2, log10, sqrt.
#' @param combine Logical, whether to combine variables listed in \code{group}. 
#' @param scale Scale argument passed to \code{\link{facet_grid}}. Defaults to \code{"fixed"}.
#'   Other options include \code{"free_y"}, \code{"free_x"}, and \code{"free_xy"}. 
#' @param output Output type. Options include 'table' or 'plot'.
#' @param format_tab How table output should be formatted. Options include \code{'wide'}
#'   (the default) and \code{'long'}.
#' @details Returns a plot or table of the mean CPUE and share of total catch or raw count for each species entered.
#'  For optimal plot size in an R Notebook/Markdown document, we recommend including no more than four species.
#'  The order of variables in the \code{cpue} and \code{catch} arguments must be in the same order as in the \code{names} argument.
#'  The \code{names} argument is used to join the \code{catch} and \code{cpue} variables together.
#' @return \code{bycatch()} compares the average CPUE and catch total/share of total
#' catch between one or more species. The data can be filtered using two arguments: 
#'   \code{filter_date} and \code{filter_value}. \code{filter_date} specifies how the data  
#'   should be filtered--by year, period (i.e. "month" or "week"), or year-period.
#'   \code{filter_value} should contain the values (as integers) to filter the data 
#'   by. It is often useful to facet by year when using \code{filter_date}.
#'   Only one grouping variable will be displayed; however, any number of
#'   variables can be combined by using \code{combine = TRUE}, but no more than
#'   three is recommended. For faceting, any variable in the dataset can be used,
#'   but "year" and "month" are also available. Generally, no more than four species
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
#'   year = 2011, value = "stc", output = "table"
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
                    group = NULL, filter_date = NULL, date_value = NULL, filter_by = NULL,
                    filter_value = NULL, filter_expr = NULL, facet_by = NULL, 
                    tran = "identity", value = "stc", combine = FALSE, scale = "fixed",
                    output = "tab_plot", format_tab = "wide") {
  
  # Call in datasets
  out <- data_pull(dat)
  dataset <- out$dataset
  
  if (shiny::isRunning()) {
    if (deparse(substitute(dat)) == "values$dataset") dat <- get("dat_name")
  } else { 
    if (!is.character(dat)) dat <- deparse(substitute(dat)) }

  # facet setup ----
  if (!is.null(facet_by)) {
    # if facet_by contains "species", "year", "month", or "week"
    special_facet <- ifelse(any(!(facet_by %in% names(dataset))), TRUE, FALSE) 
    
    if (special_facet == TRUE) {
      
      facet <- facet_by
      facet_by <- facet_by[facet_by %in% names(dataset)]
      
      if (length(facet_by) == 0) {
        
        facet_by <- NULL
      }
      
    } else {
      
      facet <- facet_by
    }
    
  } else {
    
    facet <- NULL
  }
  
  facet_date <- facet[facet %in% c("year", "month", "week")]
  
  # group setup ----
  if (!is.null(group)) {
    
    if (combine == TRUE) {
      
      dataset <- ID_var(dataset, vars = group, type = "string")
      group <- gsub(" ", "", paste(group, collapse = "_"))
      group2 <- NULL
    }
    
    dataset[group] <- lapply(dataset[group], as.factor)
    group1 <- group[1]
    
    if (length(group) == 1) group2 <- NULL else group2 <- group[2]
    
    if (length(group) > 1 & combine == FALSE) {
      
      warning("Only the first grouping variable will be displayed in plot.")
    }
    
  } else {
    
    group1 <- NULL
    group2 <- NULL
  }
  
  # Names joining table ----
  if (is.null(names)) {
    
    name_tab <- data.frame(species = catch, species_catch = catch, species_cpue = cpue)
    
  } else {
    
    name_tab <- data.frame(species = names, species_catch = catch, species_cpue = cpue)
  }
  
  # filter by variable ----
  if (!is.null(filter_by) | !is.null(filter_expr)) {
    
    dataset <- subset_var(dataset, filter_by, filter_value, filter_expr)
    
    if (nrow(dataset) == 0) {
      
      warning("Filtered data table has zero rows. Check filter parameters.")
      end <- TRUE
    }
  }
  
  # date setup ----
  periods <- c("year", "month", "weeks")
  
  if (period %in% periods == FALSE) {
    
    stop("Invalid period. Please select a valid period name (see documentation for details).")
    
  } else {
    
    p <- switch(period, year = "%Y", month = "%b", weeks = "%U")
  }
  
  dataset <- add_missing_dates(dataset, date, value = c(cpue, catch), 
                               group = group, facet_by = facet_by)
  
  if (period != "year") {
    
    dataset$year <- as.integer(format(dataset[[date]], "%Y"))
  }
  
  dataset[[period]] <- format(dataset[[date]], p)
  
  if (!is.null(facet_date)) {
    
    if (period != "month" & any("month" %in% facet_date)) {
      
      dataset$month <- factor(format(dataset[[date]], "%b"), levels = month.abb, ordered = TRUE)
      
      
    } else if (period != "week" & any("week" %in% facet_date)) {
      
      dataset$week <- as.integer(format(dataset[[date]], "%U"))
    }
  }
  
  # filter by date ----
  if (!is.null(filter_date)) {
    
    dataset <- subset_date(dataset, date, filter_date, date_value)
    
    if (nrow(dataset) == 0) {
      
      warning("Filtered data table has zero rows. Check filter parameters.")
      end <- TRUE
    }
  }
  # add facet date vars if applicable
  if (!is.null(facet_date)) {
    
    if (period != "month" & any("month" %in% facet)) {
      
      dataset$month <- factor(format(dataset[[date]], "%b"), levels = month.abb, ordered = TRUE)
      
      
    } else if (period != "week" & any("week" %in% facet)) {
      
      dataset$week <- as.integer(format(dataset[[date]], "%U"))
    }
  }
  
  # Mean CPUE summary table ----
  agg_grp <- c("year", group, facet_by, facet_date)
  
  cpue_tab <- agg_helper(dataset, value = cpue, period = period, group = agg_grp, fun = mean)
  
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
    
    stc_tab <- agg_helper(catch_tab, value = f_catch(), group = c(catch_grp()), 
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
      
      cp_plot <- ggplot2::ggplot(cpue_dat, ggplot2::aes(!!rlang::sym(period), !!cpue_exp(), 
                                                        group = !!group_exp(), color = !!color_exp())) + 
        ggplot2::geom_point(size = 1) + 
        ggplot2::geom_line(size = 0.65) + 
        ggplot2::labs(title = "CPUE", y = "average CPUE") + 
        fishset_theme() + 
        ggplot2::scale_y_continuous(trans = tran) +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 9), 
                       axis.title = ggplot2::element_text(size = 7), 
                       axis.text = ggplot2::element_text(size = 7), 
                       legend.position = "none")
      
      if (!is.null(facet)) {
        
        if (length(facet) == 1) {
          
          fm <- stats::reformulate(".", facet)
          
        } else if (length(facet) == 2) {
          
          fm <- paste(facet, sep = " ~ ")
        }
        
        cp_plot <- cp_plot + ggplot2::facet_grid(fm, scales = scale)
      }
      
      if (period == "year") {
        
        cp_plot <- cp_plot + ggplot2::scale_x_continuous(breaks = num_breaks(cpue_dat[[period]]))
        
      } else if (period == "weeks") {
        
        cp_plot <- cp_plot + ggplot2::scale_x_continuous(breaks = num_breaks(cpue_dat[[period]]),
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
      
      ca_plot <- ggplot2::ggplot(catch_dat, ggplot2::aes(!!rlang::sym(period), !!catch_exp(), 
                                                         group = !!group_exp(), color = !!color_exp())) + 
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
      
      if (!is.null(facet)) {
        
        if (length(facet) == 1) {
          
          fm <- stats::reformulate(".", facet)
          
        } else if (length(facet) == 2) {
          
          fm <- paste(facet, sep = " ~ ")
        }
        
        ca_plot <- ca_plot + ggplot2::facet_grid(fm, scales = scale)
      }
      
      if (period == "year") {
        
        ca_plot <- ca_plot + ggplot2::scale_x_continuous(breaks = num_breaks(catch_dat[[period]]))
        
      } else if (period == "weeks") {
        
        ca_plot <- ca_plot + ggplot2::scale_x_continuous(breaks = num_breaks(catch_dat[[period]]),
                                                         labels = week_labeller(num_breaks(catch_dat[[period]]),
                                                                                catch_dat$year))
      }
      
      ca_plot
    })
    
    # combine catch and cpue plots by species
    plot_list <- purrr::pmap(list(x = cpue_plots, y = catch_plots, p_nm = names), function(x, y, p_nm) {
      
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
                                filter_date, date_value, filter_by, filter_value, filter_expr,
                                facet_by, tran, value, combine, scale, output, format_tab)
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
