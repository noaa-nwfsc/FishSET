#' Density Plot
#' 
#' Creates a density or cdf plot of selected variable
#' 
#' @param dat Main data frame over which to apply function. Table in fishset_db 
#'   database should contain the string `MainDataTable`.
#' @param project name of project.
#' @param var variable to compute a density estimate or cdf with.
#' @param type Type of density plot. Options include "kde" (kernel density estimate),
#'   "ecdf" (empirical cdf), or "cdf".
#' @param group A grouping variable. 
#' @param position The position of the grouped variable for kde plot. Options include 
#'   "identity", "stacked", and "filled". 
#' @return 
#' @export density_plot
#' @examples 
#' \dontrun{
#' 
#' 
#' }
#' @import ggplot2
#' @importFrom stats pnorm
#' 


density_plot <- function(dat, project = NULL, var, type = c("kde", "ecdf", "cdf"), 
                         group = NULL, position = "identity") {
  
  out <- FishSET:::data_pull(dat)
  dat <- out$dat
  dataset <- out$dataset
  
  
  if (!is.null(group) & !is.factor(group)) {
    
    dataset[[group]] <- factor(dataset[[group]])
    
  }
  
  if (type == "kde") {
    
    plot <- ggplot2::ggplot(dataset, ggplot2::aes_string(x = var, fill = if (is.null(group) == F) {
              group} else {NULL})) + 
              ggplot2::stat_density(position = position, color = "black", alpha = .7) +
              labs(title = paste0("KDE of ", var)) + 
              fishset_theme
    
  } else if (type == "ecdf") {
    
    if (is.null(group)) {
      
      plot <- ggplot2::ggplot(dataset, ggplot2::aes_string(x = var)) + 
                ggplot2::stat_ecdf(geom = "area", alpha = .7) +
                ggplot2::stat_ecdf(geom = "step", alpha = .7) +
                labs(title = paste0("ECDF of ", var)) + 
                fishset_theme
      
    } else {
      
      plot <- ggplot2::ggplot(dataset, ggplot2::aes_string(x = var, color = group)) +
                ggplot2::stat_ecdf(geom = "step") +
                labs(title = paste0("ECDF of ", var)) + 
                fishset_theme
      
    } 
    
  } else {
    
    dataset$cdf <- stats::pnorm(dataset[[var]], 
                                mean = mean(dataset[[var]]),
                                sd = sd(dataset[[var]]))
    
    plot <- ggplot2::ggplot(dataset, ggplot2::aes_string(var)) +
              ggplot2::geom_area(ggplot2::aes(y = cdf), position = "identity", alpha = .7) +
              labs(title = paste0("CDF of ", var)) + 
              fishset_theme
    
  }
  
  #Log the function 
  
  density_plot_function <- list()
  density_plot_function$functionID <- "density_plot"
  density_plot_function$args <- c(dat, project, var, type, group, position)
  log_call(density_plot_function)
  
  # Save output
  
  save_plot(project, "density_plot")
  
  plot
}







density_plot(poldat, var = "HAUL")

density_plot(poldat, var = "HAUL", group = "DISEMBARKED_PORT")

density_plot(poldat, var = "HAUL", type = "ecdf")

density_plot(poldat, var = "HAUL", type = "cdf", group = "DISEMBARKED_PORT")

density_plot(poldat, var = "HAUL", type = "cdf")


density_plot(fs_na, var = "HAUL", type = "kde", group = "VESSEL_TYPE")




# filter option for specifying time period? 

