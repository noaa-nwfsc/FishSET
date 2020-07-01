# Vessel count
#' 
#'  Active vessels by time period
#' 
#' @param dat Primary data containing information on hauls or trips. 
#'   Table in FishSET database contains the string 'MainDataTable'.
#' @param project String, name of project.
#' @param v_id Variable in \code{dat} containing vessel identifier to count.
#' @param date Date variable to aggregate by.
#' @param period Time period to aggregate by. Options include \code{"year"}, \code{"month"}, \code{"weeks"} 
#'   (weeks in the year), \code{"weekday"}, \code{"weekday_abv"}, \code{"day"} (day of the month), 
#'   and \code{"day_of_year"}.
#' @param group Names of grouping variables. For lineplots two grouping variables 
#'   can be entered, the first is passsed to "color" and second to "linetype". Only one 
#'   grouping variable can be used for barplots, which is passed to "fill". When \code{combine = TRUE} 
#'   all variables in \code{group} will be joined. 
#' @param filter_date The type of filter to apply to table. Options include \code{"year-period"}, 
#'   \code{"year-month"}, \code{"year"}, \code{"month"}, or \code{"period"}. The 
#'   argument \code{filter_value} must be provided. 
#' @param filter_value Integer (4 digits if year, 1-2 if month). The year, month,
#'   or year-month to filter data table by. Use a list if using "year-month"
#'   with the format: \code{list(year(s), month(s))}. For example, \code{list(2011:2013, 5:7)} 
#'   will filter the data table from May to July for years 2011-2013.
#' @param facet_by Variable name to facet by. Accepts up to two variables. These can be 
#'   variables that exist in the dataset, or a variable created by \code{vessel_count()} such 
#'   as \code{"year"} or \code{"month"}. The first variable is facetted by row and the 
#'   second by column. 
#' @param combine Whether to combine variables listed in \code{group}. This is passed
#'   to the "fill" or "color" aesthetic for plots. 
#' @param position Positioning of bar plot. Options include 'stack', 'dodge', 
#'   and 'fill'.
#' @param value Whether to return \code{"count"} or \code{"percent"} of active vessels. Defaults
#'   to \code{"count"}. 
#' @param tran transformation function applied to vessel count. Defaults to \code{FALSE}. 
#' @param type Plot type, options include \code{"bar"} (the default) and \code{"line"}. 
#' @param scale Scale argument passed to \code{\link{facet_grid}}. 
#'   Options include \code{"free"}, \code{"free_x"}, \code{"free_y"}. Defaults to 
#'   \code{"fixed"}. 
#' @param output Whether to display \code{"plot"}, \code{"table"}. Defaults 
#'   to both (\code{"tab_plot"}). 
#' @return \code{vessel_count} aggregates the number (or percent) of active vessels
#'   by time period using a column of unique vessel IDs. The data can be filter using 
#'   two arguments: \code{filter_date} amd \code{filter_value}. \code{filter_date}
#'   specifies how the data should be filtered--by year, month, or year-month. 
#'   \code{filter_value} should contain the years or months (as integers) to filter
#'   the data by. Up to two grouping variables can be entered. Grouping variables can 
#'   be merged into one variable using \code{combine}; in this case any number of 
#'   variables can be joined, but no more than three is reccomended. For faceting,
#'   any variable (including ones listed in \code{group}) can be used, but "year" and
#'   "month" are also available. Currently, combined variables cannot be faceted.
#'   A list containing a table and plot are printed to the console and viewer by default. 
#' @examples 
#' \dontrun{
#' vessel_count(pollockMainDataTable, 'VESSEL_ID', 'DATE_FISHING_BEGAN', period = 'month', 
#'              group = 'DISEMBARKED_PORT', position = 'dodge', year = 2011, output = 'plot')
#' }
#' @export vessel_count
#' @import ggplot2
#' @importFrom stats aggregate reformulate
#' @importFrom dplyr anti_join left_join
#' @importFrom scales percent

vessel_count <- function(dat, project, v_id, date, period = "month", group = NULL, 
                         filter_date = NULL, filter_value = NULL, facet_by = NULL,
                         combine = FALSE, position = "stack", tran = "identity", value = "count",
                         type = "bar", scale = "fixed", output = "tab_plot") {
    
    # Call in datasets
    out <- data_pull(dat)
    dat <- out$dat
    dataset <- out$dataset
    
    periods <- c("year",  "month", "weeks", "weekday", "day", "day_of_year")
    
    if (period %in% periods == FALSE) {
        
        stop("Invalid period. Please select a valid period name (see documentation for details).")
        
    } else {
        
        p <- switch(period, year = "%Y", month = "%b", weeks = "%U", weekday = "%a", 
                    day = "%d", day_of_year = "%j")
    }
    
    facet_date_l <- FALSE
    
    if (!is.null(facet_by)) {
        
        facet_spec <- ifelse(any(!(facet_by %in% names(dataset))), TRUE, FALSE)
        facet_date_l <- ifelse(any(!(facet_by %in% c("year", "month", "week"))), TRUE, FALSE)
        
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
    
    facet_date <- facet_by[facet_by %in% c("year", "month", "week")]
    dataset[[date]] <- date_parser(dataset[[date]])
    
    nm1 <- c(date, group, facet_by)
    
    dataset <- dataset[c(nm1, v_id)]
    
    full_dates <- seq.Date(from = min(dataset[[date]], na.rm = TRUE), 
                           to = max(dataset[[date]], na.rm = TRUE), 
                           by = "day")
    
    grp_fct <- c(group, facet_by)
    
    missing <- lapply(dataset[grp_fct], function(x) unique(x))
    
    missing[[date]] <- full_dates
    
    missing <- do.call(expand.grid, list(missing))
    
    missing <- dplyr::anti_join(missing, dataset[nm1])
    
    if (nrow(missing) > 0) {
        
        missing[[v_id]] <- 0
        
        dataset <- rbind(dataset[c(nm1, v_id)], missing)
    }
    
    if (!is.null(filter_date)) {
        
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
    
    
    if (period != "year") {
        
        dataset$year <- as.integer(format(dataset[[date]], "%Y"))
    }
    
    dataset[[period]] <- format(dataset[[date]], p)
    
    if (facet_date_l == TRUE) {
        
        if (period != "month" & any("month" %in% facet_date)) {
            
            dataset$month <- factor(format(dataset[[date]], "%b"), levels = month.abb, ordered = TRUE)
            
            
        } else if (period != "week" & any("week" %in% facet_date)) {
            
            dataset$week <- as.integer(format(dataset[[date]], "%U"))
        }
    }
    
    nm2 <- unique(c("year", facet_date, period, group, facet_by))
    
    agg_list <- lapply(nm2, function(x) dataset[[x]])
    names(agg_list) <- nm2
    
    count <- stats::aggregate(dataset[v_id], by = agg_list, FUN = function(x) length(unique(x)))
    
    if (value == "percent") {
        
        if (period == "year") {
            
            count$prop <- count[[v_id]]/sum(count[[v_id]])
            
        } else {
            
            total <- stats::aggregate(stats::reformulate("year", v_id), count, FUN = sum)
            
            names(total)[names(total) == v_id] <- "total"
            
            count <- dplyr::left_join(count, total, by = "year")
            
            count$prop <- count[[v_id]]/count$total
        }
    }
    
    if (p %in% c("%a", "%b")) {
        
        count <- date_factorize(count, period, p)
        
    } else {
        
        count[[period]] <- as.integer(count[[period]])
    }
    
    
    
    if (!is.null(group)) {
        
        rev <- ifelse(position == "dodge", TRUE, FALSE)
        or_nm <- ifelse(value == "percent", "prop", v_id)
        count <- order_factor(count, group1, or_nm, rev = rev)
    }
    
    count <- count[order(count$year, count[[period]]), ]
    
    f_value <- function() if (value == "percent") "prop" else v_id
    f_int <- function() {
        if (length(group) == 1) group1
        else if (length(group) > 1) paste0("interaction(", group1, ", ", group2, ")") 
        else 1 }
    
    v_plot <- ggplot2::ggplot(data = count, ggplot2::aes_string(x = period, y = f_value())) +
        fishset_theme +
        ggplot2::theme(legend.position = "bottom") +
        ggplot2::scale_y_continuous(labels = if (value == "percent") scales::percent else ggplot2::waiver(),
                                    trans = tran)
    
    if (type == "bar") {
        
        if (position == "dodge") position <- ggplot2::position_dodge2(preserve = "single")
        
        v_plot <- v_plot + ggplot2::geom_col(ggplot2::aes_string(fill = group1), position = position)
        
        position <- ifelse(class(position)[1] == "PositionDodge2", "dodge", position)
        
    } else if (type == "line") {
        
        v_plot <- v_plot + ggplot2::geom_line(ggplot2::aes_string(group = f_int(), color = group1, 
                                                                  linetype = group2)) +
            ggplot2::geom_point(ggplot2::aes_string(group = f_int(), color = group1), size = 1)
    }
    
    if (!is.null(facet)) {
        
        if (length(facet) == 1) {
            
            fm <- stats::reformulate(".", facet)
            
        } else if (length(facet) == 2) {
            
            fm <- paste(facet, sep = " ~ ")
        }
        
        v_plot <- v_plot + ggplot2::facet_grid(fm, scales = scale)
    }
    
    v_plot <- v_plot + ggplot2::labs(y = "active vessels")
    
    if (!(p %in% c("%a", "%b"))) {
        
        v_plot <- v_plot + ggplot2::scale_x_continuous(breaks = num_breaks(count[[period]]))
    }
    
    # Log function
    vessel_count_function <- list()
    vessel_count_function$functionID <- "vessel_count"
    vessel_count_function$args <- list(dat, project, v_id, date, period, group, filter_date,
                                       filter_value, facet_by, combine, position, tran, 
                                       value, type, scale, output)
    log_call(vessel_count_function)
    
    # Output folder
    save_table(count, project, "vessel_count")
    save_plot(project, "vessel_count", v_plot)
    
    if (output == "table") {
        
        count
        
    } else if (output == "plot") {
        
        v_plot
        
    } else {
        
        out_list <- list(table = count,
                         plot = v_plot)
        out_list
    }
}


