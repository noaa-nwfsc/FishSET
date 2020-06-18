#' Catch by Species
#'
#' Creates a table or plot of total species catch by period
#' 
#' @param dat Primary data containing information on hauls or trips. 
#' Table in FishSET database contains the string 'MainDataTable'..
#' @param project String, name of project.
#' @param species Variable in \code{dat} containing the species catch 
#' or a vector of species variables (in pounds).
#' @param date Variable in \code{dat} containing dates to aggregate by.
#' @param period Time period to count by. Options include 'year', 'month', 'weeks' 
#' (weeks in the year),'weekday', 'weekday_abv', 'weekday_num', 'day' (day of the month), 
#'   and 'day_of_year'.
#' @param fun String, name of function to aggregate by. Defaults to \code{\link[base]{sum}}. 
#' @param group Grouping variable names(s). Up to two grouping variables are available for 
#'   line plots and one for bar plots. For bar plots, if only one species is entered the first 
#'   group variable is passed to 'fill'. If multiple species are entered, species is passed to 
#'   "fill" and the grouping variable is dropped. An exception occurs when facetting by species, 
#'   then the grouping variable is passed to "fill". For line plots, the first grouping variable 
#'   is passed to "fill" and the second to "linetype" if a single species column is entered or if 
#'   facetting by species. Otherwise, species is passed to "fill", the first group variable to 
#'   "linetype", and second is dropped. 
#' @param filter_date The type of filter to apply to table. Options include \code{"year-period"}, 
#'   \code{"year-month"}, \code{"year"}, \code{"month"}, or \code{"period"}. The 
#'   argument \code{filter_value} must be provided. 
#' @param filter_value Integer (4 digits if year, 1-3 if month or period). A vector or list
#'   of values to filter data table by. Use a list if using a two-part filter, e.g."year-month",
#'   with the format: \code{list(year, period}. For example, \code{list(2011:2013, 5:7)} 
#'   will filter the data table from May to July for years 2011-2013.
#' @param facet_by Variable name to facet by. This can be a variable that exists in 
#'   the dataset, or a variable created by \code{species_catch()} such as \code{"year"}, 
#'   \code{"month"}, or \code{"species"}.  
#' @param type Plot type, options include \code{"bar"} (the default) and \code{"line"}. 
#' @param conv Convert catch variable to \code{"tons"}, \code{"metric_tons"}, or 
#'   by using a function entered as a string. Defaults to \code{FALSE}, no conversion.
#' @param value Whether to calculate raw \code{"count"} or \code{"percent"} of total catch. 
#' @param position Positioning of bar plot. Options include 'stack', 'dodge', 
#'   and 'fill'. 
#' @param combine Whether to combine variables listed in \code{group}. This is passed
#'   to the "fill" or "color" aesthetic for plots. 
#' @param scale Scale argument passed to \code{\link{facet_grid}}. Defaults to \code{"fixed"}.
#' @param output Output a \code{"plot"} or \code{"table"}. Defaults 
#'   to both (\code{"tab_plot"}).
#' @param format_tab How table output should be formatted. Options include 'wide' 
#'   (the default) and 'long'.
#' @return  \code{species_catch()} aggregates catch (or percent) 
#'   by time period using one or more columns of catch data. The data can be filter using 
#'   two arguments: \code{filter_date} amd \code{filter_value}. \code{filter_date}
#'   specifies how the data should be filtered--by year, period (see \code{period}), or year-period. 
#'   \code{filter_value} should contain the values (as integers) to filter
#'   the data by. It is often useful to facet by year when using \code{filter_date}.
#'   Up to two grouping variables can be entered. Grouping variables can 
#'   be merged into one variable using \code{combine = TRUE}. Any number of 
#'   variables can be combined, but no more than three is reccomended. For faceting,
#'   any variable (including ones listed in \code{group}) can be used, but "year" and
#'   "month" are also available. Currently, combined variables cannot be faceted.
#'   A list containing a table and plot are printed to the console and viewer by default.  
#' @examples 
#' \dontrun{
#' species_catch('pollockMainDataTable', species = c('HAUL_LBS_270_POLLOCK_LBS', 
#' 'HAUL_LBS_110_PACIFIC_COD_LBS', 'HAUL_LBS_OTHER_LBS'), date = 'HAUL_DATE', 
#' value = 'count', period = 'month_num', output = 'plot', year = 2011, convert_to_tons = TRUE)
#' }
#' @export species_catch
#' @import ggplot2
#' @importFrom stats aggregate reformulate
#' @importFrom reshape2 dcast melt
#' @importFrom dplyr anti_join
#' @importFrom scales percent 

species_catch <- function(dat, project, species, date, period = "month", fun = "sum", 
                          group = NULL, filter_date = NULL, filter_value = NULL, 
                          facet_by = NULL, type = "bar", conv = FALSE, value = "count", 
                          position = "stack", combine = FALSE, scale = "fixed",
                          output = "tab_plot", format_tab = "wide") {  
    
    # Call in datasets
    out <- data_pull(dat)
    dat <- out$dat
    dataset <- out$dataset
    
    periods <- c("year", "month", "weeks", "weekday", "weekday_abv", "weekday_num", 
                 "day", "day_of_year")
    
    if (period %in% periods == FALSE) {
        
        stop("Invalid period. Please select a valid period name (see documentation for details).")
        
    } else {
        
        p <- switch(period, year = "%Y", month = "%b", weeks = "%U", weekday = "%A", 
                    weekday_abv = "%a", weekday_num = "%w", day = "%d", day_of_year = "%j")
    }
    
    facet_ym <- FALSE
    
    if (!is.null(facet_by)) {
        
        facet_spec <- ifelse(any(!(facet_by %in% names(dataset))), TRUE, FALSE)
        facet_ym <- ifelse(any(!(facet_by %in% c("year", "month"))), TRUE, FALSE)
        
        if (facet_spec == TRUE) {
            
            facet_s_id <- facet_by[!(facet_by %in% names(dataset))]
            
            if (all(facet_s_id %in% c("year", "month", "species")) == FALSE) {
                
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
    
    dataset[[date]] <- date_parser(dataset[[date]])
    
    nm1 <- c(date, group, facet_by)
    
    full_dates <- seq.Date(from = min(dataset[[date]], na.rm = TRUE), 
                           to = max(dataset[[date]], na.rm = TRUE), 
                           by = "day")
    
    grp_fct <- c(group, facet_by)
    
    missing <- lapply(dataset[grp_fct], function(x) unique(x))
    
    missing[[date]] <- full_dates
    
    missing <- do.call(expand.grid, list(missing))
    
    missing <- dplyr::anti_join(missing, dataset[nm1])
    
    if (nrow(missing) > 0) {
      
      missing[species] <- 0
      
      count <- rbind(dataset[c(nm1, species)], missing)
    }
    
    if (period != "year") {
        
        count$year <- as.integer(format(count[[date]], "%Y"))
    }
    
    count[[period]] <- format(count[[date]], p)
    
    if (facet_ym == TRUE | 
        (!is.null(filter_date) && any(filter_date %in% c("month", "year-month")))) {
        
        if (period != "month") {
            
            count$month <- factor(format(count[[date]], "%b"), levels = month.abb, ordered = TRUE)
            nm2 <- unique(c("year", "month", period, group, facet_by))
            
        } else {
            
            nm2 <- unique(c("year", period, group, facet_by))
        }
        
    } else {
        
      nm2 <- unique(c("year", period, group, facet_by))
    }
    
    count[[date]] <- NULL
    
    agg_list <- lapply(nm2, function(x) count[[x]])
    names(agg_list) <- nm2
    
    count <- stats::aggregate(count[species], by = agg_list, FUN = fun)
    
    if (length(species) > 1) {
        
        count <- reshape2::melt(count, measure.vars = species, variable.name = "species", 
                                value.name = "catch")
        
        rev <- ifelse(position == "dodge", TRUE, FALSE)
        count <- order_factor(count, "species", "catch", rev = rev)
    }
    
    if (p %in% c("%a", "%A", "%b")) {
        
        count <- date_factorize(count, period, p)
        
    } else {
        
        count[[period]] <- as.integer(count[[period]])
    }
    
    if (!is.null(filter_date)) {
        
        if (filter_date == "year-period") {
            
            count <- subset(count, (year %in% filter_value[[1]]) & 
                                (as.integer(count[[period]]) %in% filter_value[[2]])) 
            
        } else if (filter_date == "year-month") {
            
            count <- subset(count, (year %in% filter_value[[1]]) & 
                                (as.integer(month) %in% filter_value[[2]]))
            
        } else if (filter_date == "year") {
            
            count <- subset(count, year %in% filter_value)
            
        } else if (filter_date == "month") {
            
            count <- subset(count, as.integer(month) %in% filter_value)
        
        } else if (filter_date == "period") {
            
            count <- subset(count, as.integer(count[[period]]) %in% filter_value)
  
        } else {
            
            warning("Invalid filter type. Available options are 'year-month', 'year', and 'month'.")
            x <- 1
        }
        
        if (nrow(count) == 0) {
            
            warning("Filtered data table has zero rows. Check filter parameters.")
            x <- 1
        }
    }
    
    f_catch <- function() if (length(species) == 1) species else "catch"
    
    if (conv != FALSE) {
        
        if (conv == "tons") {
            
            count[f_catch()] <- count[f_catch()]/2000
            
        } else if (conv == "metric_tons") {
            
            count[f_catch()] <- count[f_catch()]/2204.62
            
        } else {
            
            count[f_catch()] <- do.call(conv, list(count[f_catch()]))
        }
    }
    
    if (value == "percent") {
        
        count[f_catch()] <- count[f_catch()]/sum(count[f_catch()])
    } 
    
    count <- count[order(count$year, count[[period]]), ]
    
    f_group1 <- function() {
        if (is.null(facet)) {
            if (length(species) == 1) group1 else if (length(species) > 1) "species"
        } else { 
            if ("species" %in% facet) group1 else if (length(species) > 1) "species" else group1
        }
    }
    
    f_group2 <- function() if (length(species) == 1) group2 else group1
    
    s_plot <- ggplot2::ggplot(data = count, ggplot2::aes_string(x = period, y = f_catch())) +
        fishset_theme +
        ggplot2::theme(legend.position = "bottom") +
        ggplot2::scale_y_continuous(labels = if (value == "percent") scales::percent else waiver())
    
    if (type == "bar") {
        
        if (position == "dodge") position <- ggplot2::position_dodge2(preserve = "single")
        
        s_plot <- s_plot + ggplot2::geom_col(ggplot2::aes_string(fill = f_group1()), 
                                             position = position)
        
        position <- ifelse(class(position)[1] == "PositionDodge2", "dodge", position)
        
    } else if (type == "line") {
        
        s_plot <- s_plot + ggplot2::geom_line(ggplot2::aes_string(group = f_group1(), 
                                                                  color = f_group1(), 
                                                                  linetype = f_group2())) +
            ggplot2::geom_point(ggplot2::aes_string(group = f_group1(), 
                                                    color = f_group1()), 
                                size = 1)
    }
    
    if (!is.null(facet)) {
        
        if (length(facet) == 1) {
            
            fm <- stats::reformulate(".", facet)
            
        } else if (length(facet) == 2) {
            
            fm <- paste(facet, sep = " ~ ")
        }
        
        s_plot <- s_plot + ggplot2::facet_grid(fm, scales = scale)
    }
    
    if (!(p %in% c("%a", "%A", "%b"))) {
        
        s_plot <- s_plot + ggplot2::scale_x_continuous(n.breaks = n_breaks(count[[period]]))
    }
    
    # Log the function
    species_catch_function <- list()
    species_catch_function$functionID <- "species_catch"
    species_catch_function$args <- list(dat, project, species, date, period, fun, group, 
                                        filter_date, filter_value, facet_by, type,
                                        conv, value, position, combine, scale, output, format_tab)
    log_call(species_catch_function)
    
    if (format_tab == "wide") {
        
        if (length(species) > 1) {
            
            count <- reshape2::dcast(count, ... ~ species, value.var = "catch", fill = 0)
        }
    }
    
    save_plot(project, "species_catch")
    save_table(count, project, "species_catch")
    
    if (output == "table") {
        
        count
        
    } else if (output == "plot") {
        
        s_plot
        
    } else {
        
        out_list <- list(table = count,
                         plot = s_plot)
        out_list
    }
}
