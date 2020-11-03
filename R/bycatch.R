# Bycatch
#'
#' Compare bycatch CPUE and total catch/percent of catch to other species
#'
#' @param dat Main data frame over which to apply function. Table in FishSET 
#'   database should contain the string `MainDataTable`.
#' @param project name of project.
#' @param cpue A characteric string of CPUE variable names. The function outputs 
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
#' @param filter_value String containing a start and end date if using filter_date = "date_range", 
#'   e.g. c("2011-01-01", "2011-03-15"). If filter_date = "period" or "year-period", use integers 
#'   (4 digits if year, 1-2 if day, month, or week). Use a list if using a two-part filter, e.g. "year-week",
#'   with the format \code{list(year, period)} or a vector if using a single period, \code{c(period)}. 
#'   For example, \code{list(2011:2013, 5:7)} will filter the data table from weeks 5 through 7 for 
#'   years 2011-2013 if filter_date = "year-week".\code{c(2:5)} will filter the data
#'   February through May when filter_date = "month".
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
#' @importFrom dplyr anti_join left_join
#' @importFrom gridExtra grid.arrange arrangeGrob
#' @importFrom reshape2 melt dcast
#' @importFrom scales percent
#' @importFrom shiny isRunning

bycatch <- function(dat, project, cpue, catch, date, period = "year", names = NULL, 
                    group = NULL, filter_date = NULL, filter_value = NULL, facet_by = NULL, 
                    tran = "identity", value = "stc", combine = FALSE, scale = "fixed",
                    output = "tab_plot", format_tab = "wide") {
  
  # Call in datasets
  out <- data_pull(dat)
  dataset <- out$dataset
  
  if (shiny::isRunning()) {
    if (deparse(substitute(dat)) == "values$dataset") dat <- get("dat_name")
  } else { 
    if (!is.character(dat)) dat <- deparse(substitute(dat)) }
  
  periods <- c("year", "month", "weeks")
  
  if (period %in% periods == FALSE) {
    
    stop("Invalid period. Please select a valid period name (see documentation for details).")
    
  } else {
    
    p <- switch(period, year = "%Y", month = "%b", weeks = "%U")
  }
  
  facet_date_l <- FALSE
  
  if (!is.null(facet_by)) {
    
    facet_spec <- any(!(facet_by %in% names(dataset)))
    facet_date_l <- any(facet_by %in% c("year", "month", "week"))
    
    if (facet_spec == TRUE) {
      
      facet_s_id <- facet_by[!(facet_by %in% names(dataset))]
      
      if (all(facet_s_id %in% c("year", "month")) == FALSE) {
        
        warning("Invalid facet variable.")
        
      } else {
        
        facet <- facet_by
        facet_by <- facet_by[facet_by %in% names(dataset)]
        
        if (length(facet_by) == 0) {
          
          facet_by <- NULL
        }
      }
      
    } else {
      
      facet <- facet_by
    }
    
  } else {
    
    facet <- NULL
  }
  
  if (!is.null(group)) {
    
    if (combine == TRUE) {
      
      dataset[[paste(group, collapse = "_")]] <- apply(dataset[group], 1, paste, collapse = " ")
      group <- paste(group, collapse = "_")
      group2 <- NULL
    }
    
    group1 <- group[1]
    
    dataset[[group1]] <- as.factor(dataset[[group1]])
    
    if (length(group) == 2) {
      
      group2 <- group[2]
      
      dataset[[group2]] <- as.factor(dataset[[group2]])
      
    } else if (length(group) > 2) {
      
      warning("Too many grouping variables included, selecting first two.")
      
    } else {
      
      group2 <- NULL
    }
    
  } else {
    
    group1 <- NULL
    group2 <- NULL
  }
  # Joining table
  if (is.null(names)) {
    
    name_tab <- data.frame(species = catch, species_count = catch, species_cpue = cpue)
    
  } else {
    
    name_tab <- data.frame(species = names, species_count = catch, species_cpue = cpue)
  }
  
  dataset[[date]] <- date_parser(dataset[[date]])
  
  # Missing dates ====
  nm <- c(date, group, facet_by)
  
  full_dates <- seq.Date(from = min(dataset[[date]], na.rm = TRUE), 
                         to = max(dataset[[date]], na.rm = TRUE), 
                         by = "day")
  
  grp_fct <- unique(c(group, facet_by))
  
  missing <- lapply(dataset[grp_fct], function(x) unique(x))
  
  missing[[date]] <- full_dates
  
  missing <- do.call(expand.grid, list(missing))
  
  missing <- dplyr::anti_join(missing, dataset[nm])
  
  if (nrow(missing) > 0) {
    
    missing[c(cpue, catch)] <- 0
    
    dataset <- rbind(dataset[c(nm, cpue, catch)], missing)
  }
  
  if (!is.null(filter_date)) {
    
    if (filter_date != "none") {
      
      if (filter_date == "date_range") {
        
        dataset <- dataset[dataset[[date]] >= filter_value[1] & dataset[[date]] <= filter_value[2], ]
        
      } else {
        
        pf <- switch(filter_date, "year-month" = c("%Y", "%m"), "year-week" = c("%Y", "%U"),
                     "year-day" = c("%Y", "%j"), "year" = "%Y", "month" = "%m", "week" = "%U",
                     "day" = "%j")
        
        if (grepl("-", filter_date)) {
          
          dataset <- dataset[(as.integer(format(dataset[[date]], pf[1])) %in% filter_value[[1]]) & 
                               (as.integer(format(dataset[[date]], pf[2])) %in% filter_value[[2]]), ]
          
        } else {
          
          dataset <- dataset[as.integer(format(dataset[[date]], pf)) %in% filter_value, ]
        }
        
        if (nrow(dataset) == 0) {
          
          warning("Filtered data table has zero rows. Check filter parameters.")
          end <- TRUE
        }
      }
    }
  }
  
  
  
  if (period != "year") {
    
    dataset$year <- as.integer(format(dataset[[date]], "%Y"))
  }
  # filter base table 
  dataset[[period]] <- format(dataset[[date]], p)
  
  if (facet_date_l == TRUE) {
    
    if (period != "month" & any("month" %in% facet)) {
      
      dataset$month <- factor(format(dataset[[date]], "%b"), levels = month.abb, ordered = TRUE)
      
      
    } else if (period != "week" & any("week" %in% facet)) {
      
      dataset$week <- as.integer(format(dataset[[date]], "%U"))
    }
  }
  
  # Mean CPUE table ====
  effort <- dataset[names(dataset)[!(names(dataset) %in% catch)]]
  
  e_per <- names(effort)[!(names(effort) %in% c(nm, cpue))]
  
  e_nm <- unique(c(e_per, group, facet_by))
  
  agg_list <- lapply(e_nm, function(x) effort[[x]])
  names(agg_list) <- e_nm
  
  effort <- stats::aggregate(effort[cpue], by = agg_list, FUN = mean)
  
  if (length(cpue) > 1) {
    
    effort <- reshape2::melt(effort, measure.vars = cpue, variable.name = "species_cpue", 
                             value.name = "mean_cpue")
  }
  
  # Count/STC table ====
  count <- dataset[names(dataset)[!(names(dataset) %in% cpue)]]
  
  c_per <- names(count)[!(names(count) %in% c(nm, catch))]
  
  c_nm <- unique(c(c_per, group, facet_by))
  
  agg_list <- lapply(c_nm, function(x) count[[x]])
  names(agg_list) <- c_nm
  
  count <- stats::aggregate(count[catch], by = agg_list, FUN = sum)
  
  if (length(catch) > 1) {
    
    count <- reshape2::melt(count, measure.vars = catch, variable.name = "species_count", 
                            value.name = "catch")
  }
  # STC ====
  f_catch <- function() if (length(catch) > 1) "catch" else catch
  
  if (value == "stc") {
    
    year_total <- stats::aggregate(stats::reformulate("year", f_catch()), count, FUN = sum)
    
    names(year_total)[names(year_total) == f_catch()] <- "yr_total"
    
    count <- dplyr::left_join(count, year_total, by = "year")
    
    count$stc <- count[[f_catch()]]/count$yr_total
  }
  
  # Join tables ====
  if (length(catch) == 1) {
    
    bycatch <- dplyr::left_join(effort, count, by = c_nm)
    
  } else {
    
    effort <- dplyr::left_join(effort, name_tab, by = "species_cpue")
    bycatch <- dplyr::left_join(effort, count, by = c(c_nm, "species_count"))
  }
  
  if (p == "%b") {
    
    bycatch <- date_factorize(bycatch, period, p)
    
  } else {
    
    bycatch[[period]] <- as.integer(bycatch[[period]])
  }
  
  
  
  bycatch <- bycatch[order(bycatch$year, bycatch[[period]]), ]
  f_species <- function() if (length(catch) > 1) "species" else NULL
  f_group <- function() if (is.null(group)) 1 else group1
  names <- as.character(name_tab$species)
  
  # Plots =====
  cpue_plots <- lapply(names, function(x) {
    
    f_cpue <- function() if (length(cpue) > 1) "mean_cpue" else cpue
    cpue_nm <- unique(c(e_nm, f_species(), f_cpue()))
    
    if (length(cpue) == 1) {
      cpue_dat <- bycatch[cpue_nm]
    } else {
      cpue_dat <- bycatch[bycatch$species == x, cpue_nm]
    }
    
    cp_plot <- ggplot2::ggplot(cpue_dat, ggplot2::aes_string(period, f_cpue(), group = f_group(), color = group1)) + 
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
    
    if (p != "%b") {
      
      cp_plot <- cp_plot + ggplot2::scale_x_continuous(breaks = num_breaks(cpue_dat[[period]]))
    }
    
    cp_plot
  })
  
  catch_plots <- lapply(names, function(x) {
    
    f_catch2 <- function() if (value == "stc") "stc" else if (length(catch) > 1) "catch" else catch
    catch_nm <- unique(c(c_nm, f_species(), f_catch2()))
    
    if (length(catch) == 1) {
      catch_dat <- bycatch[catch_nm] 
    } else {
      catch_dat <- bycatch[bycatch$species == x, catch_nm] 
    }
    
    ca_plot <- ggplot2::ggplot(catch_dat, ggplot2::aes_string(period, f_catch2(), group = f_group(), color = group1)) + 
      ggplot2::geom_point(size = 1) + 
      ggplot2::geom_line(size = 0.65) + 
      ggplot2::labs(title = if (value == "total") "Total" else "STC", 
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
    
    if (p != "%b") {
      
      ca_plot <- ca_plot + ggplot2::scale_x_continuous(breaks = num_breaks(catch_dat[[period]]))
    }
    
    ca_plot
  })
  
  plot_list <- purrr::pmap(list(x = cpue_plots, y = catch_plots, p_nm = names), function(x, y, p_nm) {
    
    gridExtra::arrangeGrob(x, y, nrow = 1, top = p_nm)
  })
  
  by_plot <- do.call(gridExtra::arrangeGrob, c(plot_list, ncol = 1))
  
  if (!is.null(group)) {
    # add grouping var
    grp <- ggplot2::ggplot(bycatch, ggplot2::aes_string(0, 0, color = group1)) + 
      ggplot2::geom_point() + 
      ggplot2::theme(legend.position = "bottom")
    
    # extract legend
    tmp <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(grp))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    
    by_plot <- gridExtra::arrangeGrob(by_plot, legend, ncol = 1, heights = c(.9, .1))
  }
  
  bycatch[c("species_count", "species_cpue")] <- NULL
  
  if (format_tab == "long") {
    
    bycatch <- reshape2::melt(bycatch, id.vars = c(c_nm, "species"), 
                              variable.name = "measure", value.name = "value")
  }
  
  save_table(bycatch, project, "bycatch")
  save_plot(project, "bycatch", by_plot)
  
  # Log Function
  bycatch_function <- list()
  bycatch_function$functionID <- "bycatch"
  bycatch_function$args <- list(dat, project, cpue, catch, date, period, names, group, 
                                filter_date, filter_value, facet_by, tran, value, combine, 
                                scale, output, format_tab)
  log_call(bycatch_function)
  
  if (output == "plot") {
    
    gridExtra::grid.arrange(by_plot, ncol = 1)
    
  } else if (output == "table") {
    
    bycatch
    
  } else {
    
    out_list <- list(table = bycatch,
                     plot = gridExtra::grid.arrange(by_plot, ncol = 1))
    out_list
  }
}
