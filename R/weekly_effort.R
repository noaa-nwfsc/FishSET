#' Weekly Effort
#' 
#' Average CPUE by week
#' 
#' @param dat Main data frame over which to apply function. Table in fishset_db 
#'   database should contain the string `MainDataTable`.
#' @param project name of project.
#' @param cpue A variable containing cpue. 
#' @param date A time variable containing dates to aggregate by.
#' @param group Up to two categorical variables to group by. Passed to \code{\link{facet_grid}}. 
#'   The first grouping variable is facetted by row and the second grouping variable by 
#'   column. 
#' @param year A year or vector of years to subset the data by. If NULL, all years
#'   are included. 
#' @param plot_type Plot output type. Options include 'line', 'point', and 'line_point'.
#' @param output Whether a table or plot should be generated.
#' @param format_tab  How table output should be formated. Options include 'wide' 
#'   (the default) and 'long'.  
#' @examples 
#' \dontrun{
#' weekly_effort('pollockMainDataTable', 'CPUE', 'DATE_FISHING_BEGAN', year = 2011, output = 'table')
#' }
#' @export weekly_effort
#' @import ggplot2
#' @importFrom stats aggregate reformulate


weekly_effort <- function(dat, project, cpue, date, group = NULL, year = NULL, plot_type = "line_point", output = c("plot", "table"), format_tab = "wide") {
    
    # Call in datasets
    out <- data_pull(dat)
    dat <- out$dat
    dataset <- out$dataset
    
    dataset$week <- format(dataset[[date]], "%U")
    dataset$years <- format(dataset[[date]], "%Y")
    
    y_val <- year
    
    if (!is.null(year)) {
        
        dataset <- subset(dataset, years %in% y_val)
        
    }
    
    if (length(cpue) == 1) {
        
        if (!is.null(group)) {
            
            group1 <- group[1]
            
            dataset[[group1]] <- as.factor(dataset[[group1]])
            
            if (length(group) == 2) {
                
                group2 <- group[2]
                
                dataset[[group2]] <- as.factor(dataset[[group2]])
                
            } else if (length(group) > 2) {
                
                warning("Too many grouping variables included, selecting first two.")
                
            }
        }
        
        if (is.null(group)) {
            
            effort <- stats::aggregate(dataset[[cpue]], by = list(dataset$years, dataset$week), FUN = mean, drop = TRUE)
            
            effort <- setNames(effort, c("years", "week", "avg_cpue"))
            
        } else if (!is.null(group) & length(group) == 1) {
            
            effort <- stats::aggregate(dataset[[cpue]], by = list(dataset$years, dataset$week, dataset[[group1]]), FUN = mean, drop = TRUE)
            
            effort <- setNames(effort, c("years", "week", group1, "avg_cpue"))
            
        } else if (!is.null(group) & length(group) > 1) {
            
            effort <- stats::aggregate(dataset[[cpue]], by = list(dataset$years, dataset$week, dataset[[group1]], dataset[[group2]]), FUN = mean, drop = TRUE)
            
            effort <- setNames(effort, c("years", "week", group1, group2, "avg_cpue"))
            
        }
        
        effort$week <- as.integer(effort$week)
        effort$years <- as.factor(effort$years)
        
        ind <- periods_list[["%U"]][which(!(periods_list[["%U"]] %in% unique(effort$week)))]
        
        if (is.null(group)) {
            
            missing_periods <- expand.grid(week = ind, years = unique(effort$years))
            
        } else if (!is.null(group) & length(group) == 1) {
            
            missing_periods <- expand.grid(week = ind, years = unique(effort$years), group1 = unique(effort[[group1]]))
            
            missing_periods <- setNames(missing_periods, c("week", "years", group1))
            
        } else if (!is.null(group) & length(group) > 1) {
            
            missing_periods <- expand.grid(week = ind, years = unique(effort$years), group1 = unique(effort[[group1]]), group2 = unique(effort[[group2]]))
            
            missing_periods <- setNames(missing_periods, c("week", "years", group1, group2))
            
        }
        
        if (nrow(missing_periods) > 0) {
            
            missing_periods$avg_cpue <- 0
            
            effort <- rbind(effort, missing_periods)
            
        }
        
        effort <- effort[order(effort$years, effort$week), ]
        
        plot <- ggplot2::ggplot(effort, ggplot2::aes(week, avg_cpue, color = if (length(unique(.data[["years"]])) > 1) 
            years else NULL))
        
        if (plot_type == "line") {
            
            plot <- plot + ggplot2::geom_line(size = 0.65)
            
        } else if (plot_type == "point") {
            
            plot <- plot + ggplot2::geom_point()
            
        } else if (plot_type == "line_point") {
            
            plot <- plot + ggplot2::geom_point() + ggplot2::geom_line(size = 0.65)
            
        }
        
        plot <- plot + ggplot2::labs(title = if (!is.null(year) & length(year) == 1) 
            paste(year, "average CPUE by week") else "average CPUE by week", y = "average cpue") + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) + ggplot2::scale_x_continuous(breaks = seq(0, 
            50, by = 10)) + fishset_theme
        
        if (length(unique(effort$years)) > 1) {
            
            plot <- plot + ggplot2::guides(color = ggplot2::guide_legend(title = "Year", title.theme = ggplot2::element_text(size = 10), label.theme = ggplot2::element_text(size = 8))) + 
                ggplot2::theme(legend.position = "bottom")
            
        }
        
        if (!is.null(group) & length(group) == 1) {
            
            plot <- plot + ggplot2::facet_grid(stats::reformulate(".", group1), scales = "free_y", labeller = ggplot2::labeller(.cols = label_both, .rows = label_both))
            
        } else if (!is.null(group) & length(group) > 1) {
            
            plot <- plot + ggplot2::facet_grid(stats::reformulate(group2, group1), scales = "free_y", labeller = ggplot2::labeller(.cols = label_both, 
                .rows = label_both))
        }
        
    } else if (length(cpue) > 1) {
        
        if (is.null(group)) {
            
            effort <- lapply(cpue, function(x) {
                stats::aggregate(dataset[[x]], by = list(dataset$years, dataset$week), FUN = mean)
            })
            
        } else if (!is.null(group)) {
            
            if (length(group) == 1) {
                
                effort <- lapply(cpue, function(x) {
                  stats::aggregate(dataset[[x]], by = list(dataset$years, dataset$week, dataset[[group1]]), FUN = mean)
                })
                
            } else if (length(group) > 1) {
                
                effort <- lapply(cpue, function(x) {
                  stats::aggregate(dataset[[x]], by = list(dataset$years, dataset$week, dataset[[group1]], dataset[[group2]]), FUN = mean)
                })
                
            }
        }
        
        names(effort) <- cpue
        
        effort <- reshape2::melt(effort, id.vars = names(effort[[1]]))
        
        if (is.null(group)) {
            
            effort <- setNames(effort, c("years", "week", "avg_cpue", "cpue"))
            
        } else if (!is.null(group)) {
            
            if (length(group) == 1) {
                
                effort <- setNames(effort, c("years", "week", group1, "avg_cpue", "cpue"))
                
            } else if (length(group) > 1) {
                
                effort <- setNames(effort, c("years", "week", group1, group2, "avg_cpue", "cpue"))
                
            }
        }
        
        effort$week <- as.integer(effort$week)
        effort$years <- as.factor(effort$years)
        
        ind <- periods_list[["%U"]][which(!(periods_list[["%U"]] %in% unique(effort$week)))]
        
        if (is.null(group)) {
            
            missing_periods <- expand.grid(week = ind, years = unique(effort$years), cpue = unique(effort$cpue))
            
        } else if (!is.null(group) & length(group) == 1) {
            
            missing_periods <- expand.grid(week = ind, years = unique(effort$years), group1 = unique(effort[[group1]]), cpue = unique(effort$cpue))
            
            missing_periods <- setNames(missing_periods, c("week", "years", group1, "cpue"))
            
        } else if (!is.null(group) & length(group) > 1) {
            
            missing_periods <- expand.grid(week = ind, years = unique(effort$years), group1 = unique(effort[[group1]]), group2 = unique(effort[[group2]]), 
                cpue = unique(effort$cpue))
            
            missing_periods <- setNames(missing_periods, c("week", "years", group1, group2, "cpue"))
            
        }
        
        if (nrow(missing_periods) > 0) {
            
            missing_periods$avg_cpue <- 0
            
            effort <- rbind(effort, missing_periods)
            
        }
        
        effort <- effort[order(effort$years, effort$week), ]
        
        plot <- ggplot2::ggplot(effort, ggplot2::aes(week, avg_cpue, color = if (length(unique(.data[["years"]])) > 1) 
            years else NULL))
        
        if (plot_type == "line") {
            
            plot <- plot + ggplot2::geom_line(size = 0.65)
            
        } else if (plot_type == "point") {
            
            plot <- plot + ggplot2::geom_point()
            
        } else if (plot_type == "line_point") {
            
            plot <- plot + ggplot2::geom_point() + ggplot2::geom_line(size = 0.65)
            
        }
        
        plot <- plot + ggplot2::labs(title = if (!is.null(year) & length(year) == 1) 
            paste(year, "average CPUE by week") else "average CPUE by week", y = "average cpue") + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) + ggplot2::scale_x_continuous(breaks = seq(0, 
            50, by = 10)) + fishset_theme
        
        if (length(unique(effort$years)) > 1) {
            
            plot <- plot + ggplot2::guides(color = ggplot2::guide_legend(title = "Year", title.theme = ggplot2::element_text(size = 10), label.theme = ggplot2::element_text(size = 8))) + 
                ggplot2::theme(legend.position = "bottom")
            
        }
        if (is.null(group)) {
            
            plot <- plot + ggplot2::facet_grid(cpue ~ ., scales = "free_y", labeller = ggplot2::labeller(.cols = label_both))
            
        } else if (!is.null(group) & length(group) == 1) {
            
            plot <- plot + ggplot2::facet_grid(stats::reformulate(".", paste("cpue +", group1)), scales = "free_y", labeller = ggplot2::labeller(.cols = label_both, 
                .rows = label_both))
            
        } else if (!is.null(group) & length(group) > 1) {
            
            plot <- plot + ggplot2::facet_grid(stats::reformulate(group2, paste("cpue +", group1)), scales = "free_y", labeller = ggplot2::labeller(.cols = label_both, 
                .rows = label_both))
        }
        
        
    }
    
    # Log function
    weekly_effort_function <- list()
    weekly_effort_function$functionID <- "weekly_effort"
    weekly_effort_function$args <- c(dat, project, cpue, date, group, year, plot_type, output, format_tab)
    log_call(weekly_effort_function)
    
    # Save plot
    save_plot(project, "weekly_effort", plot)
    
    names(effort)[names(effort) == "years"] <- "year"
    
    if (output == "plot") {
        
        plot
        
    } else if (output == "table") {
        
        if (length(cpue) == 1) {
            
            names(effort)[names(effort) == "avg_cpue"] <- cpue
            
        } else if (length(cpue) > 1) {
            
            if (format_tab == "wide") {
                
                effort <- reshape2::dcast(effort, ... ~ cpue, value.var = "avg_cpue", fill = 0, fun.aggregate = mean)
                
            }
        }
        
        effort
    }
}
