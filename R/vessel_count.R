#' Vessel count
#' 
#' Generates a table or plot of the number of unique vessels by time period
#' 
#' @param dat Main data frame over which to apply function. Table in FishSET 
#'   database should contain the string `MainDataTable`.
#' @param project name of project.
#' @param v vessel ID variable to count.
#' @param t time variable containing dates to aggregate by.
#' @param period Time period to count by. Options include 'year', 'year_abv', 
#'   'month', 'month_abv', 'month_num', 'weeks' (weeks in the year), 
#'   'weekday', 'weekday_abv', 'weekday_num', 'day' (day of the month), 
#'   and 'day_of_year'.
#' @param group Up to two variables to group by. For plots, 
#'   the first variable is passed to 'fill' and second is passed to 'facet_grid'.
#'   If multiple years are entered, plots are faceted row-wise by year as well.
#' @param year A year or vector of years, in the form of a four digit integer, to 
#'   subset the data by. Plot output is faceted by year if multiple years are given.
#' @param position Positioning of bar plot. Options include 'stack', 'dodge', 
#'   and 'fill'. 
#' @param output table or plot. 
#' @return Table or plot of the number of unique vessels within a time period.
#' @examples 
#' \dontrun{
#' vessel_count('pollockMainDataTable', 'VESSEL_ID', 'DATE_FISHING_BEGAN', period = 'month', 
#'              group = 'DISEMBARKED_PORT', position = 'dodge', year = 2011, output = 'plot')
#' }
#' @export vessel_count
#' @import ggplot2
#' @importFrom stats aggregate



vessel_count <- function(dat, project, v, t, period = "month", group = NULL, year = NULL, position = "stack", output = c("table", "plot")) {
    
    # Call in datasets
    out <- data_pull(dat)
    dat <- out$dat
    dataset <- out$dataset
    
    
    periods <- c("year", "year_abv", "month", "month_abv", "month_num", "weeks", "weekday", "weekday_abv", "weekday_num", "day", "day_of_year")
    
    if (period %in% periods == FALSE) {
        
        stop("Invalid period. Please select a valid period name (see documentation for details).")
        
    } else {
        
        p <- switch(period, year = "%Y", year_abv = "%y", month = "%B", month_abv = "%b", month_num = "%m", weeks = "%U", weekday = "%A", weekday_abv = "%a", 
            weekday_num = "%w", day = "%d", day_of_year = "%j")
    }
    
    dataset$years <- format(dataset[[t]], "%Y")
    
    y_val <- year
    
    if (!is.null(year)) {
        
        dataset <- subset(dataset, years %in% y_val)
        
    }
    
    if (!is.null(group)) {
        
        group1 <- group[1]
        
        dataset[[group1]] <- as.factor(dataset[[group1]])
        
        dataset[[group1]] <- as.factor(dataset[[group1]])
        
        if (length(group) == 2) {
            
            group2 <- group[2]
            
            dataset[[group2]] <- as.factor(dataset[[group2]])
            
        } else if (length(group) > 2) {
            
            warning("Too many grouping variables included, selecting first two.")
            
        }
    }
    
    v_freq <- c("vessel_freq")
    
    if (is.null(group)) {
        
        count <- stats::aggregate(dataset[[v]], by = list(dataset$years, format(date_parser(dataset[[t]]), p)), FUN = function(x) {
            length(unique(x))
        })
        
        colnames(count) <- c("years", period, v_freq)
        
    } else if (!is.null(group)) {
        
        if (length(group) == 1) {
            
            count <- stats::aggregate(dataset[[v]], by = list(dataset$years, format(date_parser(dataset[[t]]), p), dataset[[group1]]), FUN = function(x) {
                length(unique(x))
            })
            
            colnames(count) <- c("years", period, group1, v_freq)
            
        } else {
            
            count <- stats::aggregate(dataset[[v]], by = list(dataset$years, format(date_parser(dataset[[t]]), p), dataset[[group1]], dataset[[group2]]), 
                FUN = function(x) {
                  length(unique(x))
                })
            
            colnames(count) <- c("years", period, group1, group2, v_freq)
            
        }
    }
    
    if (p %in% c("%a", "%A", "%b", "%B")) {
        
        count <- date_factorize(count, period, p)
        
    } else {
        
        count[[period]] <- as.integer(count[[period]])
        
        count$years <- as.integer(count$years)
        
    }
    
    ind <- periods_list[[p]][which(!(periods_list[[p]] %in% unique(count[[period]])))]
    
    if (is.null(group)) {
        
        missing_periods <- expand.grid(period = ind, years = unique(count$years))
        
        names(missing_periods)[names(missing_periods) == "period"] <- period
        
    } else if (!is.null(group) & length(group) == 1) {
        
        missing_periods <- expand.grid(period = ind, years = unique(count$years), group1 = unique(count[[group1]]))
        
        missing_periods <- setNames(missing_periods, c(period, "years", group1))
        
    } else if (!is.null(group) & length(group) > 1) {
        
        missing_periods <- expand.grid(period = ind, years = unique(count$years), group1 = unique(count[[group1]]), group2 = unique(count[[group2]]))
        
        missing_periods <- setNames(missing_periods, c(period, "years", group1, group2))
        
    }
    
    if (nrow(missing_periods) > 0) {
        
        missing_periods$vessel_freq <- 0
        
        count <- rbind(count, missing_periods)
        
    }
    
    count <- count[order(count$years, count[[period]]), ]
    
    
    plot <- ggplot2::ggplot(data = count, ggplot2::aes_string(x = period, y = "vessel_freq", fill = if (!is.null(group)) 
        group1 else NULL)) + ggplot2::geom_col(position = if (position == "dodge") 
        ggplot2::position_dodge2(preserve = "single") else position) + ggplot2::labs(title = if (!is.null(year) & length(year) == 1) 
        paste(year) else NULL, x = paste0(t, " ", "(", period, ")"), y = "active vessels") + ggplot2::theme(plot.title = element_text(hjust = 0.5)) + fishset_theme
    
    if (!is.null(group)) {
        
        plot <- plot + ggplot2::guides(fill = ggplot2::guide_legend(title = group[1], title.theme = ggplot2::element_text(size = 10), label.theme = ggplot2::element_text(size = 8))) + 
            ggplot2::theme(legend.position = "bottom")
        
    }
    
    if (!is.null(group) & length(group) == 2) {
        
        if (is.null(year) | (!is.null(year) & length(year) == 1)) {
            
            plot <- plot + ggplot2::facet_grid(stats::reformulate(".", group2), scales = "free_y")
            
        } else if (!is.null(year) & length(year) > 1) {
            
            plot <- plot + ggplot2::facet_grid(stats::reformulate(".", paste("years +", group2)), scales = "free_y")
            
        }
        
    } else {
        
        if (is.null(year) | (!is.null(year) & length(year) == 1)) {
            
            plot <- plot
            
        } else if (length(year) > 1) {
            
            plot <- plot + ggplot2::facet_grid(years ~ ., scales = "free_y")
            
        }
    }
    
    if (p %in% c("%a", "%A", "%b", "%B")) {
        
        plot <- plot + ggplot2::scale_x_discrete(breaks = levels(count[[period]]))
        
    } else {
        
        plot <- plot + ggplot2::scale_x_continuous(breaks = if (length(unique(count[[period]])) <= 12) {
            seq(1, length(unique(count[[period]])), by = 1)
        } else {
            seq(1, length(unique(count[[period]])), by = round(length(unique(count[[period]])) * 0.08))
        })
        
    }
    
    # Log the function
    
    vessel_count_function <- list()
    vessel_count_function$functionID <- "vessel_count"
    vessel_count_function$args <- c(dat, project, v, t, period, group, year, position, output)
    log_call(vessel_count_function)
    
    # Output folder
    save_table(count, project, "vessel_count")
    
    save_plot(project, "vessel_count", plot)
    
    names(count)[names(count) == "years"] <- "year"
    
    if (output == "table") {
        
        count
        
    } else {
        
        plot
    }
}
