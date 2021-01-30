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
#' @param filter_date The type of filter to apply to table. The "date_range" option will subset 
#'   the data by two date values entered in \code{date_value}. Other options include "year-day", 
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
#' @param filter_expr String, a valid R expression to filter `MainDataTable` by. 
#' @param facet_by Variable name to facet by. This can be a variable that exists in 
#'   the dataset, or a variable created by \code{weekly_effort()} such as \code{"year"}, 
#'   \code{"month"}, or \code{"species"}.
#' @param facet_by Variable name to facet by. This can be a variable that exists in 
#'   the dataset, or a variable created by \code{weekly_effort()} such as \code{"year"}, 
#'   \code{"month"}, or \code{"species"}.  
#' @param tran A function to transform the y-axis. Options include log, log2, log10, sqrt.
#' @param combine Whether to combine variables listed in \code{group}. This is passed
#'   to the "color" aesthetic for plots. 
#' @param scale Scale argument passed to \code{\link[ggplot2]{facet_grid}}. Defaults to \code{"fixed"}.
#' @param output Whether to display \code{"plot"}, \code{"table"}. Defaults 
#'   to both (\code{"tab_plot"}).
#' @param format_tab How table output should be formated. Options include 'wide' 
#'   (the default) and 'long'.
#' @return \code{weekly_effort()} calculates mean CPUE by week. This function doesn't 
#'   calculate CPUE; the CPUE variable must be created in advance (see \code{\link{cpue}}).
#'   There are two types of filters that can be applied to the data: date filters and variable filters.
#'   \code{filter_date} specifies how the data should be filtered--by year, 
#'  period (i.e. "month" or "week"), or year-period. \code{date_value} should contain the 
#'   values (as integers) to filter the data by. Grouping variables can be merged into one variable using
#'   \code{combine = TRUE}. Any number of variables can be combined, but no more than 
#'   three is recommended. For faceting, any variable (including ones listed in \code{group}) 
#'  can be used, but "year" and "month" are also available. Currently, combined variables cannot be faceted.
#'   A list containing a table and plot are printed to the console and viewer by default, 
#'   this changed by setting \code{output} to "table" or "plot".  
#' @examples 
#' \dontrun{
#' weekly_effort(pollockMainDataTable, "CPUE", "DATE_FISHING_BEGAN", year = 2011, output = "table")
#' }
#' @export weekly_effort
#' @import ggplot2
#' @importFrom stats reformulate
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom rlang expr sym

weekly_effort <- function(dat, project, cpue, date, group = NULL, filter_date = NULL, 
                          date_value = NULL, filter_by = NULL, filter_value = NULL,
                          filter_expr = NULL, facet_by = NULL, tran = "identity",  
                          combine = FALSE, scale = "fixed", output = "tab_plot", format_tab = "wide") {
    
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
        
        if (length(group) > 2) {
            
            warning("Only the first two grouping variables will be displayed in plot.")
        }
        
    } else {
        
        group1 <- NULL
        group2 <- NULL
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
    facet_date <- facet[facet %in% c("year", "month", "week")]
    
    if (!is.null(date)) {
        
        dataset <- add_missing_dates(dataset, date, cpue, group = group, facet_by = facet_by)
        
        if (!is.null(facet_date)) {
            
            if ("month" %in% facet_date) {
                
                dataset$month <- factor(format(dataset[[date]], "%b"), levels = month.abb, ordered = TRUE)
                
                
            } else if ("week" %in% facet_date) {
                
                dataset$week <- as.integer(format(dataset[[date]], "%U"))
            }
        }
    }
    
    # date filter ----
    if (!is.null(filter_date)) {
        
        dataset <- subset_date(dataset, date, filter_date, date_value)
        
        if (nrow(dataset) == 0) {
            
            warning("Filtered data table has zero rows. Check filter parameters.")
            end <- TRUE
        }
    }
    
    # add year and week columns
    dataset$year <- format(dataset[[date]], "%Y")
    dataset$week <- format(dataset[[date]], "%U")
    
    if (!is.null(facet_date)) {
        
        if ("month" %in% facet_date){
            
            dataset$month <- factor(format(dataset[[date]], "%b"), 
                                    levels = month.abb, ordered = TRUE)
        }
    }
    
    # summary table ----
    agg_grp <- c(group, facet_by, facet_date)
    
    table_out <- agg_helper(dataset, value = cpue, period = c("year", "week"), 
                            group = agg_grp, fun = mean)
    
    table_out[c("year", "week")] <- lapply(table_out[c("year", "week")], as.integer)
    
    if (length(cpue) > 1) {
        
        table_out <- tidyr::pivot_longer(table_out, cols = cpue, names_to = "species", 
                                         values_to = "mean_cpue")
    }
    
    # plot section ----
    if (output %in% c("plot", "tab_plot")) {
        
        # plot functions ----
        
        if (length(cpue) == 1) single_cpue_sym <- rlang::sym(cpue)
        
        if (length(cpue) > 1) {
            
            multi_cpue_sym <- rlang::sym("mean_cpue")
            species_sym <- rlang::sym("species")
        }
        
        if (!is.null(group)) {
            
            group1_sym <- rlang::sym(group1)
            if (length(group) > 1) group2_sym <- rlang::sym(group2)
        }
        
        y_axis_exp <- function() if (length(cpue) == 1) single_cpue_sym else multi_cpue_sym
        
        
        interaction_exp <- function() {
            if (length(cpue) == 1) {
                if (is.null(group)) {
                    1
                    
                } else {
                    if (length(group) == 1) {
                        group1_sym
                        
                    } else if (length(group) > 1) {
                        rlang::expr(interaction(!!group1_sym, !!group2_sym))
                    }
                }
                
            } else if (length(cpue) > 1) {
                if (is.null(group)) {
                    species_sym
                    
                } else if (length(group) == 1) {
                    rlang::expr(interaction(!!species_sym, !!group1_sym))
                    
                } else if (length(group) > 1) {
                    rlang::expr(interaction(!!species_sym, !!group1_sym, !!group2_sym))
                }
            }
        }
        
        color_exp <- function() {
            
            if (length(cpue) == 1) {
                if (!is.null(group)) {
                    group1_sym
                } else {
                    NULL
                }
            } else if (length(cpue) > 1) {
                species_sym
            }
        }
        
        linetype_exp <- function() {
            
            if (length(cpue) == 1) {
                if (length(group) > 1) {
                    group2_sym
                } else {
                    NULL
                }
            } else if (length(cpue) > 1) {
                if (!is.null(group)) {
                    group1_sym
                } else {
                    NULL
                }
            }
        }
        
        f_cpue <- function() if (length(cpue) == 1) cpue else "CPUE" 
        
        x_lab <- function() paste("week", date)
        y_lab <- function() paste(f_cpue(), ifelse(tran == "identity", "", paste0("(", tran, ")")))
        
        e_plot <- ggplot2::ggplot(data = table_out, ggplot2::aes(x = week, y = !!y_axis_exp())) +
            ggplot2::geom_line(ggplot2::aes(group = !!interaction_exp(), 
                                            color = !!color_exp(), 
                                            linetype = !!linetype_exp())) +
            ggplot2::geom_point(ggplot2::aes(group = !!interaction_exp(), color = !!color_exp()), size = 1) +
            ggplot2::scale_x_continuous(breaks = num_breaks(table_out$week), 
                                        labels = week_labeller(num_breaks(table_out$week), 
                                                               year = table_out$year)) +
            ggplot2::labs(x = x_lab(), y = y_lab()) +
            ggplot2::theme(legend.position = "bottom") +
            fishset_theme()
        
        if (!is.null(facet)) {
            
            if (length(facet) == 1) {
                
                fm <- stats::reformulate(".", facet)
                
            } else if (length(facet) == 2) {
                
                fm <- paste(facet, sep = " ~ ")
            }
            
            e_plot <- e_plot + ggplot2::facet_grid(fm, scales = scale)
        }
        
        save_plot(project, "weekly_effort", e_plot)
    }
    
    if (length(cpue) > 1 & format_tab == "wide") {
        
        table_out <- tidyr::pivot_wider(table_out, names_from = species, values_from = mean_cpue)
    }
    
    # Log function
    weekly_effort_function <- list()
    weekly_effort_function$functionID <- "weekly_effort"
    weekly_effort_function$args <- list(dat, project, cpue, date, group, filter_date, 
                                        date_value, filter_by, filter_value, filter_expr,
                                        facet_by, tran, combine, scale, output, format_tab)
    log_call(weekly_effort_function)
    
    save_table(table_out, project, "weekly_effort")
    
    if (output == "plot") {
        
        plot
        
    } else if (output == "table") {
        
    } else {
        
        out_list <- list(table = table_out,
                         plot = e_plot)
        out_list
    }
}
