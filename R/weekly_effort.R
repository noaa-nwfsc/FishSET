# Weekly Effort
#' 
#' Average CPUE by week
#' 
#' @param dat Main data frame over which to apply function. Table in fishset_db 
#'   database should contain the string `MainDataTable`.
#' @param project Name of project.
#' @param cpue Variable name(s) containing cpue. 
#' @param date A variable containing dates to aggregate by.
#' @param group Grouping variable name(s). Up to two grouping variables are available.
#'   The first grouping variable is passed to "fill" and the second to "linetype" if 
#'   a single species column is entered or if facetting by species. Otherwise, species 
#'   is passed to "fill", the first group variable to "linetype", and second is dropped. 
#' @param filter_date The type of filter to apply to table. Options include \code{"year-week"}, 
#'   \code{"year-month"}, \code{"year"}, \code{"month"}, or \code{"week"}. The 
#'   argument \code{filter_value} must be provided. 
#' @param filter_value Integer (4 digits if year, 1-2 if month or week). A vector or list
#'   of values to filter data table by. Use a list if using a two-part filter, e.g."year-week",
#'   with the format: \code{list(year, period}. For example, \code{list(2011:2013, 5:7)} 
#'   will filter the data table from weeks 5 through 7 for years 2011-2013.
#' @param facet_by Variable name to facet by. This can be a variable that exists in 
#'   the dataset, or a variable created by \code{weekly_effort()} such as \code{"year"}, 
#'   \code{"month"}, or \code{"species"}.  
#' @param conv Convert cpue variable to \code{"tons"}, \code{"metric_tons"}, or 
#'   by using a function entered as a string. Defaults to \code{FALSE}.
#' @param combine Whether to combine variables listed in \code{group}. This is passed
#'   to the "fill" or "color" aesthetic for plots. 
#' @param scale Scale argument passed to \code{\link{facet_grid}}. Defaults to \code{"fixed"}.
#' @param output Whether to display \code{"plot"}, \code{"table"}. Defaults 
#'   to both (\code{"tab_plot"}).
#' @param format_tab How table output should be formated. Options include 'wide' 
#'   (the default) and 'long'.
#' @return \code{weekly_effort()} calculates mean CPUE by week. The data can be filter using 
#'   two arguments: \code{filter_date} amd \code{filter_value}. \code{filter_date}
#'   specifies how the data should be filtered--by year, period (i.e. "month" or "week"), or year-period. 
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
#' weekly_effort('pollockMainDataTable', 'CPUE', 'DATE_FISHING_BEGAN', year = 2011, output = 'table')
#' }
#' @export weekly_effort
#' @import ggplot2
#' @importFrom stats aggregate reformulate
#' @importFrom reshape2 dcast melt
#' @importFrom dplyr anti_join

weekly_effort <- function(dat, project, cpue, date, group = NULL, filter_date = NULL, 
                          filter_value = NULL, facet_by = NULL, conv = FALSE, combine = FALSE, 
                          scale = "fixed", output = "tab_plot", format_tab = "wide") {
    
    # Call in datasets
    out <- data_pull(dat)
    dat <- out$dat
    dataset <- out$dataset
    
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
        
        if (length(cpue) == 1) {
            
            missing[[cpue]] <- 0
            
        } else {
            
            cl <- lapply(cpue, function(x) 0)
            names(cl) <- cpue 
            cd <- as.data.frame(cl)
            missing <- cbind(cd, missing)
        }
        
        count <- rbind(dataset[c(nm1, cpue)], missing)
    }
    
    if (facet_ym == TRUE | !is.null(filter_date)) {
        
        if (any(grepl("year", facet)) | any(grepl("year", filter_date))) {
            
            count$year <- format(count[[date]], "%Y")
        }
        
        if (any(grepl("month", facet)) | any(grepl("month", filter_date))) {
            
            count$month <- factor(format(count[[date]], "%b"), levels = month.abb, ordered = TRUE)
        }
    }
    
    count$week <- format(count[[date]], "%U")
    
    per <- names(count)[!(names(count) %in% c(nm1, cpue))]
    
    nm2 <- unique(c(per, group, facet_by))
    
    count[[date]] <- NULL
    
    agg_list <- lapply(nm2, function(x) count[[x]])
    names(agg_list) <- nm2
    
    count <- stats::aggregate(count[cpue], by = agg_list, FUN = mean)
    
    count$week <- as.integer(count$week)
    
    if (length(cpue) > 1) {
        
        count <- reshape2::melt(count, measure.vars = cpue, variable.name = "species", 
                                value.name = "mean_cpue")
        
        count <- order_factor(count, "species", "catch")
    }
    
    if (!is.null(filter_date)) {
        
        if (filter_date == "year-month") {
            
            count <- subset(count, (as.integer(year) %in% filter_value[[1]]) & 
                                (as.integer(month) %in% filter_value[[2]]))
            
        } else if (filter_date == "year-week") {
            
            count <- subset(count, (as.integer(year) %in% filter_value[[1]]) & 
                                (week %in% filter_value[[2]]))
            
        } else if (filter_date == "year") {
            
            count <- subset(count, as.integer(year) %in% filter_value)
            
        } else if (filter_date == "month") {
            
            count <- subset(count, as.integer(month) %in% filter_value)
            
        }  else if (filter_date == "week") {
            
            count <- subset(count, week %in% filter_value)
            
        } else {
            
            warning("Invalid filter type. Available options are 'year-month', 'year', and 'month'.")
            x <- 1
        }
        
        if (nrow(count) == 0) {
            
            warning("Filtered data table has zero rows. Check filter parameters.")
            x <- 1
        }
    }
    
    f_cpue <- function() if (length(cpue) == 1) cpue else "mean_cpue"
    
    if (conv != FALSE) {
        
        if (conv == "tons") {
            
            count[f_cpue()] <- count[f_cpue()]/2000
            
        } else if (conv == "metric_tons") {
            
            count[f_cpue()] <- count[f_cpue()]/2204.62
            
        } else {
            
            count[f_cpue()] <- do.call(conv, list(count[f_cpue()]))
        }
    }
    
    f_group1 <- function() {
        if (is.null(facet)) {
            if (length(cpue) == 1) group1 else if (length(cpue) > 1) "species"
        } else { 
            if ("species" %in% facet) group1 else if (length(cpue) > 1) "species" else group1
        }
    }
    
    f_group2 <- function() {
        if (!is.null(f_group1()))
            if (f_group1() == "species") group1 else group2 
        else group2
    }
    
    e_plot <- ggplot2::ggplot(data = count, ggplot2::aes_string(x = "week", y = f_cpue())) +
        ggplot2::geom_line(ggplot2::aes_string(color = f_group1(), linetype = f_group2())) +
        ggplot2::geom_point(ggplot2::aes_string(color = f_group1()), size = 1) +
        ggplot2::theme(legend.position = "bottom") +
        FishSET:::fishset_theme
    
    if (!is.null(filter_date) & filter_date != "year") {
        
        e_plot <- e_plot + ggplot2::scale_x_continuous(n.breaks = n_breaks(count$week))
        
    } else {
        
        e_plot <- e_plot + ggplot2::scale_x_continuous(breaks = seq(0, 52, 4))
    }
    
    if (!is.null(facet)) {
        
        if (length(facet) == 1) {
            
            fm <- stats::reformulate(".", facet)
            
        } else if (length(facet) == 2) {
            
            fm <- paste(facet, sep = " ~ ")
        }
        
        e_plot <- e_plot + ggplot2::facet_grid(fm, scales = scale)
    }
    
    if (length(cpue) > 1 & format_tab == "wide") {
        
        count <- reshape2::dcast(count, ... ~ species, value.var = "mean_cpue", 
                                 fill = 0, fun.aggregate = mean)
    }
    
    # Log function
    weekly_effort_function <- list()
    weekly_effort_function$functionID <- "weekly_effort"
    weekly_effort_function$args <- list(dat, project, cpue, date, group, filter_date, 
                                        filter_value, facet_by, conv, combine, scale, 
                                        output, format_tab)
    log_call(weekly_effort_function)
    
    # Save plot
    save_plot(project, "weekly_effort", e_plot)
    save_table(count, project, "weekly_effort")
    
    if (output == "plot") {
        
        plot
        
    } else if (output == "table") {
        
    } else {
        
        out_list <- list(table = count,
                         plot = e_plot)
        out_list
    }
}
