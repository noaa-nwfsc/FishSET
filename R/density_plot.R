#' Density Plot
#' 
#' Creates a density or cdf plot of selected variable
#' 
#' @param dat Main data frame over which to apply function. Table in FishSET 
#'   database should contain the string `MainDataTable`.
#' @param project name of project.
#' @param var variable of interest, used to compute density estimate or cdf.
#' @param type Type of density plot. Options include "kde" (kernel density estimate),
#'   "ecdf" (empirical cdf), or "cdf".
#' @param group A grouping variable. 
#' @param date Date variable to subset or facet_wrap by.
#' @param facet_year Logical value, wheter to facet_wrap the plot by all years in the
#'   data table.
#' @param year Numeric four digit value, the year(s) to subset or facet_wrap by.
#' @param position The position of the grouped variable for kde plot. Options include 
#'   "identity", "stack", and "fill". 
#' @return Plot in viewer and in output folder
#' @export density_plot
#' @examples 
#' \dontrun{
#' density_plot(pollockMainDataTable, var = "HAUL", group = "DISEMBARKED_PORT")
#' }
#' @import ggplot2
#' @importFrom stats pnorm
#' 


density_plot <- function(dat, project, var, type = c("kde", "ecdf", "cdf"), 
                         group = NULL, date = NULL, facet_year = FALSE, year = NULL, 
                         position = c("identity", "stack", "fill")) {
  
  out <- data_pull(dat)
  dat <- out$dat
  dataset <- out$dataset
  
  
  if (!is.null(date)) {
    
    if (lubridate::is.Date(dataset[[date]]) == FALSE) {
      
      warning("Date variable is not in correct format. Converting to date format.")
      
      dataset[[date]] <- date_parser(dataset[[date]])
      
    }
    
    dataset$Year <- format(dataset[[date]], "%Y")
    
  }
  
  if (!is.null(year) & facet_year == FALSE) {
    
    if (length(year) == 1) {
      
      dataset <- subset(dataset, Year == year)
      
    } else {
      
      dataset <- subset(dataset, Year %in% year)
      
    }
    
  }
  
  if (!is.null(group) & !is.factor(group)) {
    
    dataset[[group]] <- factor(dataset[[group]])
    
  }
  
  if (type == "kde") {
    
    plot <- ggplot2::ggplot(dataset, ggplot2::aes_string(x = var, fill = if (is.null(group) == F) {
      group} else {NULL})) + 
      ggplot2::stat_density(position = position, color = "black", alpha = .7) +
      ggplot2::labs(title = paste("KDE of", var, if (!is.null(year) & length(year) == 1) year else NULL)) + 
      fishset_theme + if (facet_year == TRUE | length(year) > 1) {
        ggplot2::facet_wrap(~Year)} else {ggplot2::geom_blank()}
    
    
  } else if (type == "ecdf") {
    
    if (is.null(group)) {
      
      plot <- ggplot2::ggplot(dataset, ggplot2::aes_string(x = var)) + 
        ggplot2::stat_ecdf(geom = "area", alpha = .7) +
        ggplot2::stat_ecdf(geom = "step", alpha = .7) +
        ggplot2::labs(title = paste("ECDF of", var, if (!is.null(year) & length(year) == 1) year else NULL)) +  
        fishset_theme + if (facet_year == TRUE | length(year) > 1) {
          ggplot2::facet_wrap(~Year)} else {ggplot2::geom_blank()}
      
    } else {
      
      plot <- ggplot2::ggplot(dataset, ggplot2::aes_string(x = var, color = group)) +
        ggplot2::stat_ecdf(geom = "step") +
        ggplot2::labs(title = paste("ECDF of", var, if (!is.null(year) & length(year) == 1) year else NULL)) + 
        fishset_theme + if (facet_year == TRUE | length(year) > 1) {
          ggplot2::facet_wrap(~Year)} else {ggplot2::geom_blank()}
      
    } 
    
  } else {
    
    dataset$cdf <- stats::pnorm(dataset[[var]], 
                                mean = mean(dataset[[var]]),
                                sd = sd(dataset[[var]]))
    
    plot <- ggplot2::ggplot(dataset, ggplot2::aes_string(var)) +
      ggplot2::geom_area(ggplot2::aes(y = cdf), position = "identity", alpha = .7) +
      ggplot2::labs(title = paste("KDE of", var, if (!is.null(year) & length(year) == 1) year else NULL)) + 
      fishset_theme + if (facet_year == TRUE | length(year) > 1) {
        ggplot2::facet_wrap(~Year)} else {ggplot2::geom_blank()}
    
  }
  
  #Log the function 
  
  density_plot_function <- list()
  density_plot_function$functionID <- "density_plot"
  density_plot_function$args <- c(dat, project, var, type, group, date, facet_year, year, position)
  log_call(density_plot_function)
  
  # Save output
  
  save_plot(project, "density_plot")
  
  plot
}


