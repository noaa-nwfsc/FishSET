#' Bycatch
#'
#' Compare bycatch to other species caught
#'
#' @param dat Main data frame over which to apply function. Table in FishSET 
#'   database should contain the string `MainDataTable`.
#' @param project name of project.
#' @param cpue A characteric string of CPUE variable names. The function outputs 
#'   the mean CPUE by period. The variable names must match the order of variable 
#'   names in \code{catch} and \code{names} arguments.    
#' @param catch A character string of names of catch variables to aggregate. The 
#'   function outputs the total catch or share of total catch by period depending 
#'   on the value argument. The order of the catch variable string must match those 
#'   of the \code{names} and \code{cpue} arguments.  
#' @param date A time variable containing dates to aggregate by.
#' @param names A string of species names that will be used to match the \code{cpue} and \code{catch}
#'   variables.
#' @param group A categorical variable to group by.
#' @param year A year or vector of years to subset the data by. If \code{NULL}, all years
#'   are included.
#' @param period Time period to aggregate by. Options include 'year', 'year_abv', 
#'   'month', 'month_abv', 'month_num', and 'weeks'.
#' @param value Whether to return raw count or share of total catch ('stc'). 
#' @param output Options include 'table' or 'plot'.
#' @param format_tab How table output should be formated. Options include 'wide' 
#'   (the default) and 'long'.
#' @return Returns a plot or table of bycatch to other species caught. For optimal 
#' plot size in a R Notebook/Markdown document, use the chunk option \code{fig.asp = 1}. 
#' @examples 
#' \dontrun{
#' bycatch('MainDataTable', 'myProject', cpue = c('f1_cpue', 'f2_cpue', 'f3_cpue', 'f4_cpue'),
#' catch = c('f1', 'f2', 'f3', 'f4'), date = 'FISHING_START_DATE', 
#' names = c('fish_1', 'fish_2', 'fish_3', 'fish_4'), period = 'month_abv', 
#' year = 2011, value = 'stc', output = 'table')
#' }
#' @export bycatch
#' @import ggplot2
#' @importFrom stats aggregate reformulate setNames
#' @importFrom purrr pmap
#' @importFrom gridExtra grid.arrange arrangeGrob
#' @importFrom reshape2 melt dcast


bycatch <- function(dat, project, cpue, catch = NULL, date, names = NULL, group = NULL, year = NULL, period = "year", value = c("count", "stc"), output = c("table", 
    "plot"), format_tab = "wide") {
    
    # Call in datasets
    out <- data_pull(dat)
    dat <- out$dat
    dataset <- out$dataset
    
    periods <- c("year", "year_abv", "month", "month_abv", "month_num", "weeks")
    
    if (period %in% periods == FALSE) {
        
        stop("Invalid period. Please select a valid period name (see documentation for details).")
        
    } else {
        
        p <- switch(period, year = "%Y", year_abv = "%y", month = "%B", month_abv = "%b", month_num = "%m", weeks = "%U")
    }
    
    dataset$Year <- format(dataset[[date]], "%Y")
    
    y_val <- year
    
    if (!is.null(year)) {
        
        dataset <- subset(dataset, Year %in% y_val)
    }
    
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
    
    name_tab <- data.frame(species = names, species_count = catch, species_cpue = cpue)
    
    # Mean CPUE
    if (is.null(group)) {
        
        effort <- stats::aggregate(dataset[cpue], by = list(dataset$Year, format(date_parser(dataset[[date]]), p)), FUN = mean)
        
        names(effort)[names(effort) == "Group.1"] <- "Year"
        names(effort)[names(effort) == "Group.2"] <- period
        
    } else if (!is.null(group)) {
        
        if (length(group) == 1) {
            
            effort <- stats::aggregate(dataset[cpue], by = list(dataset$Year, format(date_parser(dataset[[date]]), p), dataset[[group1]]), FUN = mean)
            
            names(effort)[names(effort) == "Group.1"] <- "Year"
            names(effort)[names(effort) == "Group.2"] <- period
            names(effort)[names(effort) == "Group.3"] <- group1
            
        } else if (length(group) > 1) {
            
            effort <- stats::aggregate(dataset[cpue], by = list(dataset$Year, format(date_parser(dataset[[date]]), p), dataset[[group1]], dataset[[group2]]), 
                FUN = mean)
            
            names(effort)[names(effort) == "Group.1"] <- "Year"
            names(effort)[names(effort) == "Group.2"] <- period
            names(effort)[names(effort) == "Group.3"] <- group1
            names(effort)[names(effort) == "Group.4"] <- group2
        }
    }
    
    if (is.null(group)) {
        
        effort <- reshape2::melt(effort, id.vars = c("Year", period), variable.name = "species_cpue", value.name = "avg_cpue")
        
    } else if (!is.null(group) & length(group) == 1) {
        
        effort <- reshape2::melt(effort, id.vars = c("Year", period, group1), variable.name = "species_cpue", value.name = "avg_cpue")
        
    } else if (!is.null(group) & length(group) > 1) {
        
        effort <- reshape2::melt(effort, id.vars = c("Year", period, group1, group2), variable.name = "species_cpue", value.name = "avg_cpue")
    }
    
    # Count/STC
    if (is.null(group)) {
        
        count <- stats::aggregate(dataset[catch], by = list(dataset$Year, format(date_parser(dataset[[date]]), p)), FUN = sum)
        
        names(count)[names(count) == "Group.1"] <- "Year"
        names(count)[names(count) == "Group.2"] <- period
        
    } else if (!is.null(group)) {
        
        if (length(group) == 1) {
            
            count <- stats::aggregate(dataset[catch], by = list(dataset$Year, format(date_parser(dataset[[date]]), p), dataset[[group1]]), FUN = sum)
            
            names(count)[names(count) == "Group.1"] <- "Year"
            names(count)[names(count) == "Group.2"] <- period
            names(count)[names(count) == "Group.3"] <- group1
            
        } else if (length(group) > 1) {
            
            count <- stats::aggregate(dataset[catch], by = list(dataset$Year, format(date_parser(dataset[[date]]), p), dataset[[group1]], dataset[[group2]]), 
                FUN = sum)
            
            names(count)[names(count) == "Group.1"] <- "Year"
            names(count)[names(count) == "Group.2"] <- period
            names(count)[names(count) == "Group.3"] <- group1
            names(count)[names(count) == "Group.4"] <- group2
        }
    }
    
    if (is.null(group)) {
        
        count <- reshape2::melt(count, id.vars = c("Year", period), variable.name = "species_count", value.name = "total")
        
    } else if (!is.null(group) & length(group) == 1) {
        
        count <- reshape2::melt(count, id.vars = c("Year", period, group1), variable.name = "species_count", value.name = "total")
        
    } else if (!is.null(group) & length(group) > 1) {
        
        count <- reshape2::melt(count, id.vars = c("Year", period, group1, group2), variable.name = "species_count", value.name = "total")
    }
    
    if (value == "stc") {
        
        if (is.null(group)) {
            
            count <- stats::aggregate(stats::reformulate(paste("Year +", period), "total"), data = count, FUN = function(x) x/sum(x))
            
            count <- cbind(count[, 1:2], data.frame(count$total))
            count <- stats::setNames(count, c("Year", period, catch))
            
        } else if (!is.null(group) & length(group) == 1) {
            
            count <- stats::aggregate(stats::reformulate(paste("Year +", period, "+", group1), "total"), data = count, FUN = function(x) x/sum(x))
            
            count <- cbind(count[, 1:3], data.frame(count$total))
            count <- stats::setNames(count, c("Year", period, group1, catch))
            
        } else if (!is.null(group) & length(group) > 1) {
            
            count <- stats::aggregate(stats::reformulate(paste("Year +", period, "+", group1, "+", group2), "total"), data = count, FUN = function(x) x/sum(x))
            
            count <- cbind(count[, 1:4], data.frame(count$total))
            count <- stats::setNames(count, c("Year", period, group1, group2, catch))
        }
        
        count <- reshape2::melt(count, variable.name = "species_count", value.name = "stc")
    }
    
    # merge name_tab so effort and count can be joined
    effort <- merge(effort, name_tab, by = "species_cpue", all = TRUE)
    
    if (is.null(group)) {
        
        bycatch <- merge(effort, count, by = c("Year", period, "species_count"), all = TRUE)
        
    } else if (!is.null(group) & length(group) == 1) {
        
        bycatch <- merge(effort, count, by = c("Year", period, group1, "species_count"), all = TRUE)
        
    } else if (!is.null(group) & length(group) > 1) {
        
        bycatch <- merge(effort, count, by = c("Year", period, group1, group2, "species_count"), all = TRUE)
    }
    
    if (p %in% c("%b", "%B")) {
        
        bycatch <- date_factorize(bycatch, period, p)
        
    } else {
        
        bycatch[[period]] <- as.integer(bycatch[[period]])
    }
    
    bycatch <- reshape2::melt(bycatch, measure.vars = c("avg_cpue", if (value == "stc") "stc" else "total"), variable.name = "measure", value.name = "value")
    
    ind <- periods_list[[p]][which(!(periods_list[[p]] %in% unique(bycatch[[period]])))]
    
    if (is.null(group)) {
        
        missing_periods <- expand.grid(period = ind, Year = unique(bycatch$Year), species = unique(bycatch$species), species_count = unique(bycatch$species_count), 
            species_cpue = unique(bycatch$species_cpue), measure = unique(bycatch$measure))
        
        missing_periods <- stats::setNames(missing_periods, c(period, "Year", "species", "species_count", "species_cpue", "measure"))
        
    } else if (!is.null(group) & length(group) == 1) {
        
        missing_periods <- expand.grid(period = ind, Year = unique(bycatch$Year), group1 = unique(bycatch[[group1]]), species = unique(bycatch$species), 
            species_count = unique(bycatch$species_count), species_cpue = unique(bycatch$species_cpue), measure = unique(bycatch$measure))
        
        missing_periods <- stats::setNames(missing_periods, c(period, "Year", group1, "species", "species_count", "species_cpue", "measure"))
        
    } else if (!is.null(group) & length(group) > 1) {
        
        missing_periods <- expand.grid(period = ind, Year = unique(bycatch$Year), group1 = unique(bycatch[[group1]]), group2 = unique(bycatch[[group2]]), 
            species = unique(bycatch$species), species_count = unique(bycatch$species_count), species_cpue = unique(bycatch$species_cpue), measure = unique(bycatch$measure))
        
        missing_periods <- stats::setNames(missing_periods, c(period, "Year", group1, group2, "species", "species_count", "species_cpue", "measure"))
    }
    
    if (nrow(missing_periods) > 0) {
        
        missing_periods$value <- 0
        
        bycatch <- rbind(bycatch, missing_periods)
    }
    
    bycatch <- bycatch[order(bycatch$Year, bycatch[[period]]), ]
    
    cpue_plots <- lapply(names, function(x) {
        
        by_dat <- subset(bycatch, species == x & measure == "avg_cpue")
        
        ggplot2::ggplot(by_dat, ggplot2::aes_string(period, "value", group = 1, color = if (!is.null(group)) 
            group1 else NULL)) + ggplot2::geom_point(size = 1.5) + ggplot2::geom_line(size = 0.65) + ggplot2::labs(title = "CPUE", y = "average CPUE (tns/day)") + 
            fishset_theme + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 9), axis.title = ggplot2::element_text(size = 7), axis.text = ggplot2::element_text(size = 7), 
            legend.position = "none")
    })
    
    catch_plots <- lapply(names, function(x) {
        
        if (value == "total") {
            
            by_dat <- subset(bycatch, species == x & measure == "total")
            
        } else if (value == "stc") {
            
            by_dat <- subset(bycatch, species == x & measure == "stc")
        }
        
        ggplot2::ggplot(by_dat, ggplot2::aes_string(period, "value", group = 1, color = if (!is.null(group)) 
            group1 else NULL)) + ggplot2::geom_point(size = 1.5) + ggplot2::geom_line(size = 0.65) + ggplot2::labs(title = if (value == "total") 
            "Total" else "STC", y = if (value == "total") 
            "total catch (tns)" else "share of catch") + fishset_theme + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 9), axis.title = ggplot2::element_text(size = 7), 
            axis.text = ggplot2::element_text(size = 7), legend.position = "none")
    })
    
    plot_list <- purrr::pmap(list(x = cpue_plots, y = catch_plots, n = names), function(x, y, n) {
        
        gridExtra::arrangeGrob(x, y, nrow = 1, top = n)
    })
    
    if (!is.null(group)) {
        # add grouping var
        grp <- ggplot2::ggplot(bycatch, ggplot2::aes_string(0, 0, color = group1)) + ggplot2::geom_point() + ggplot2::theme(legend.position = "bottom")
        
        # extract legend
        tmp <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(grp))
        leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
        legend <- tmp$grobs[[leg]]
        
        plot_list[["legend"]] <- legend
    }
    
    by_plot <- do.call(gridExtra::arrangeGrob, c(plot_list, ncol = 1))
    
    save_plot(project, "bycatch", by_plot)
    
    # Log Function
    bycatch_function <- list()
    bycatch_function$functionID <- "bycatch"
    bycatch_function$args <- c(dat, project, cpue, catch, date, names, group, year, period, value, output, format_tab)
    log_call(bycatch_function)
    
    bycatch[, c("species_count", "species_cpue")] <- NULL
    
    if (output == "plot") {
        
        grid.arrange(by_plot, ncol = 1)
        
    } else if (output == "table") {
        
        if (format_tab == "wide") {
            
            if (any(grepl(" ", bycatch$species) == TRUE)) {
                
                bycatch$species <- gsub(" ", "_", bycatch$species)
            }
            
            bycatch <- reshape2::dcast(bycatch, ... ~ species + measure, fun.aggregate = sum, fill = 0, value.var = "value")
        }
        
        save_table(bycatch, project, "bycatch")
        
        bycatch
    }
}
