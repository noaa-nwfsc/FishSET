temp_plot <- function(dat, project, var.select, len.fun = c("length", "unique", "percent"), agg.fun = c("mean", "median", "min", "max", "sum"), date.var = NULL) {
    #'  View temporal patterns of vector in plot format
    #'
    #' @param dat Main data frame over which to apply function. Table in FishSET database should contain the string `MainDataTable`.
    #' @param project Name of project.
    #' @param var.select Variable in dat to plot against a date variable.
    #' @param len.fun Method. 'length' returns the number of observations, 'unique' returns the number of unique observations',
    #' 'percent' returns the percentage of total observations.
    #' @param agg.fun Method. Aggregate selected variable by date using mean, median, minimum (min), maximum (max), or summation (sum).
    #' @param date.var Variable. Date variable defining x-axis. Defaults to first date variable in data set if not defined.
    #' @keywords plot, temporal, exploration
    #' @description Returns three plots showing the variable of interest against time (as month or month/year).; Plots are raw points by date, 
    #' number of observations by date, and measures of a representative observation by date.
    #' @return ggplot output
    #' @import ggplot2
    #' @export
    #' @examples
    #' \dontrun{
    #' temp_plot('pollockMainDataTable', var.select = 'OFFICIAL_TOTAL_CATCH_MT', 
    #'            len.fun = 'percent', agg.fun = 'mean', date.var = 'HAUL_DATE')
    #' temp_plot('pollockMainDataTable', var.select = 'OFFICIAL_TOTAL_CATCH_MT', 
    #'            len.fun = 'length', agg.fun = 'max')
    #' }
    
    requireNamespace("ggplot2")
    
    
    # Call in datasets
    out <- data_pull(dat)
    dat <- out$dat
    dataset <- out$dataset
    
    # Date var - NULL or use the first one
    if (is.null(date.var)) {
        date.y.var <- colnames(dataset)[grep("date", colnames(dataset), ignore.case = TRUE)[1]]
    } else {
        date.y.var <- date.var
    }
    
    if (len.fun == "length") {
        if (length(unique(lubridate::year(date_parser(dataset[[date.y.var]])))) > 1) {
            df2l <- aggregate(dataset[[var.select]] ~ lubridate::year(date_parser(dataset[[date.y.var]])), FUN = length)
        } else {
            df2l <- aggregate(dataset[[var.select]] ~ lubridate::month(date_parser(dataset[[date.y.var]])), FUN = length)
        }
    } else if (len.fun == "unique") {
        if (length(unique(lubridate::year(date_parser(dataset[[date.y.var]])))) > 1) {
            df2l <- aggregate(dataset[[var.select]] ~ lubridate::year(date_parser(dataset[[date.y.var]])), FUN = function(x) length(unique(x)))
        } else {
            df2l <- aggregate(dataset[[var.select]] ~ lubridate::month(date_parser(dataset[[date.y.var]])), FUN = function(x) length(unique(x)))
        }
    } else {
        if (length(unique(lubridate::year(date_parser(dataset[[date.y.var]])))) > 1) {
            df2l <- aggregate(dataset[[var.select]] ~ lubridate::year(date_parser(dataset[[date.y.var]])), FUN = function(x) round(length(x)/nrow(dataset) * 
                100, 2))
        } else {
            df2l <- aggregate(dataset[[var.select]] ~ lubridate::month(date_parser(dataset[[date.y.var]])), FUN = function(x) round(length(x)/nrow(dataset) * 
                100, 2))
        }
    }
    
    
    if (length(unique(lubridate::year(date_parser(dataset[[date.y.var]])))) > 1) {
        df2m <- aggregate(dataset[[var.select]] ~ lubridate::year(date_parser(dataset[[date.y.var]])), FUN = agg.fun, na.rm = T)
    } else {
        df2m <- aggregate(dataset[[var.select]] ~ lubridate::month(date_parser(dataset[[date.y.var]])), FUN = agg.fun, na.rm = T)
    }
    
    t2 <- if (length(unique(lubridate::year(date_parser(dataset[[date.y.var]])))) > 1) {
        "Year"
    } else {
        "Month"
    }
    
    
    if (grepl("date", var.select, ignore.case = T) == TRUE) {
        p1 <- ggplot2::ggplot(dataset, ggplot2::aes_string(x = as.Date(dataset[[date.y.var]], origin = "01-01-1970"), y = as.Date(dataset[[var.select]], 
            origin = "01-01-1970"))) + ggplot2::geom_point() + ggplot2::labs(subtitle = paste(var.select, "by Date"), x = "Date", y = var.select) + ggplot2::theme(panel.grid.major = ggplot2::element_blank(), 
            panel.grid.minor = ggplot2::element_blank(), panel.background = ggplot2::element_blank(), axis.line = ggplot2::element_line(colour = "black"), 
            axis.text = ggplot2::element_text(size = 11), axis.title = ggplot2::element_text(size = 11))
    } else {
        p1 <- ggplot2::ggplot(dataset, ggplot2::aes_string(x = as.Date(dataset[[date.y.var]], origin = "01-01-1970"), y = var.select)) + ggplot2::geom_point() + 
            ggplot2::labs(subtitle = paste(var.select, "by Date"), x = "Date", y = var.select) + ggplot2::theme(panel.grid.major = ggplot2::element_blank(), 
            panel.grid.minor = ggplot2::element_blank(), panel.background = ggplot2::element_blank(), axis.line = ggplot2::element_line(colour = "black"), 
            axis.text = ggplot2::element_text(size = 11), axis.title = ggplot2::element_text(size = 11))
    }
    p2 <- ggplot2::ggplot(df2l, ggplot2::aes_string(x = df2l[, 1], y = df2l[, 2])) + ggplot2::geom_bar(stat = "identity") + ggplot2::labs(subtitle = paste(simpleCap(len.fun), 
        "by", tolower(t2)), x = t2, y = "") + ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), 
        panel.background = ggplot2::element_blank(), axis.line = ggplot2::element_line(colour = "black"), axis.text = ggplot2::element_text(size = 11), 
        axis.title = ggplot2::element_text(size = 11))
    if (!is.numeric(dataset[[var.select]])) {
        p3 <- NULL
    } else {
        p3 <- ggplot2::ggplot(df2m, ggplot2::aes_string(x = df2m[, 1], y = df2m[, 2])) + ggplot2::geom_bar(stat = "identity") + ggplot2::labs(subtitle = paste(simpleCap(agg.fun), 
            "of value by", tolower(t2)), x = t2, y = "") + ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), 
            panel.background = ggplot2::element_blank(), axis.line = ggplot2::element_line(colour = "black"), axis.text = ggplot2::element_text(size = 11), 
            axis.title = ggplot2::element_text(size = 11))
    }
    
    plot <- suppressWarnings(ggpubr::ggarrange(p1, p2, p3, ncol = 3, nrow = 1))
    
    # Log the function
    
    temp_plot_function <- list()
    temp_plot_function$functionID <- "temp_plot"
    temp_plot_function$args <- list(dat, project, var.select, len.fun, agg.fun, date.var)
    log_call(temp_plot_function)
    
    # Save output
    
    save_plot(project, "temp_plot")
    
    plot
}
