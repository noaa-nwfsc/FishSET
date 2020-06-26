#' Histogram of latitude and longitude by grouping variable
spatial_hist <- function(dat, project, group) {
  #' @param dat Primary data containing information on hauls or trips. Table in FishSET database contains the string 'MainDataTable'.
  #' @param project String, name of project.
  #' @param group Column in \code{dat} containing grouping categories.
  #' @import ggplot2
  #' @importFrom reshape2 melt
  #' @return Returns histogram of latitude and longitude by grouping variable. Output returned to console and saved to Output folder.
  #' @details Returns a histogram of observed lat/lon split by grouping variable. Output printed to console and saved to Output folder.  Function is useful for assessing spatial variance/clumping of selected grouping variable. 
  #' @export
  #' @examples 
  #' \dontrun{
  #' spatial_hist('pollockMainDataTable', 'pollock', 'GEAR_TYPE')
  #' }
  
  requireNamespace("ggplot2")
  
  dataset <- dat
  dat <- deparse(substitute(dat))
  
  
  dataset <- dataset[, c(dataset[[group]], grep("lon|lat", names(dataset), ignore.case = TRUE))]
  melt.dat <- reshape2::melt(dataset)
  plot_out <- ggplot(melt.dat, aes(value, group = group, fill = group)) + 
    geom_histogram(position = "identity", alpha = 0.5, binwidth = 0.25) + 
    facet_wrap(~variable, scales = "free") + scale_color_grey() + scale_fill_grey() + theme_classic()
  
  spatial_hist_function <- list()
  spatial_hist_function$functionID <- "spatial_hist"
  spatial_hist_function$args <- list(dat, project, group)
  spatial_hist_function$kwargs <- list()
  spatial_hist_function$output <- c()
  log_call(spatial_hist_function)
  
  save_plot(project, "spatial_hist")
  
  plot_out
}


#' Summarize variable over data and time
spatial_summary <- function(dat, project, stat.var = c("length", "no_unique_obs", "perc_total", "mean", "median", "min", "max", "sum"), 
                            variable, gridfile, lon.grid, lat.grid, lon.dat, lat.dat, cat) {
  #' @param dat Primary data containing information on hauls or trips. Table in FishSET database contains the string 'MainDataTable'.
  #' @param project String, name of project. 
  #' @param stat.var Options are 'length', 'no_unique_obs', 'perc_total', 'mean', 'median', 'min', 'max', and 'sum'.
  #' @param gridfile Spatial data containing information on fishery management or regulatory zones. Shape, json, geojson, and csv formats are supported. Leave as NULL if the variable ‘ZoneID’ assigning observations to zones exists in \code{dat}.
  #' @param variable Variable in \code{dat} to summarize over date and zone.
  #' @param lon.dat Longitude variable in \code{dat}. Leave as NULL if the variable ‘ZoneID’ assigning observations to zones exists in \code{dat}.
  #' @param lat.dat Latitude variable in \code{dat}. Leave as NULL if the variable ‘ZoneID’ assigning observations to zones exists in \code{dat}.
  #' @param lon.grid Variable or list from \code{gridfile} containing longitude data. Required for csv files. Leave as NULL if \code{gridfile} is a shape or json file or if the variable ‘ZoneID’ assigning observations to zones exists in \code{dat}.
  #' @param lat.grid Variable or list from \code{gridfile} containing latitude data. Required for csv files. Leave as NULL if \code{gridfile} is a shape or json file, or if the variable ‘ZoneID’ assigning observations to zones exists in \code{dat}.
  #' @param cat  Variable or list in \code{gridfile} that identifies the individual areas or zones. If \code{gridfile} is class sf, \code{cat} should be name of list containing information on zones. Leave as NULL if the variable ‘ZoneID’ assigning observations to zones exists in \code{dat}. 
  #' @importFrom graphics par lines plot
  #' @export
  #' @description View summary and exploratory statistics of selected variable by date and zone.
  #' @details
  #' \code{stat.var} details:
  #' \tabular{rlll}{
  #' length: \tab Number of observations \cr
  #' no_unique_obs: \tab Number of unique observations \cr 
  #' perc_total: \tab Percent of total observations \cr 
  #' mean: \tab Mean \cr
  #' median: \tab  Median \cr 
  #' min: \tab  Minimum\cr
  #' max: \tab Maximum \cr
  #' sum: \tab Sum \cr
  #' }
  #' @return Returns two plots, the variable variable aggregated by \code{stat.var} plotted against date and against zone.
  #' @examples 
  #' \dontrun{
  #' Example where ZoneID exists in dataset
  #' spatial_summary(pcodMainDataTable, project='pcod', stat.var = "no_unique_obs",
  #'                 variable='HAUL')
  #' Example where obs. have not been assigned to zones
  #' spatial_summary(pcodMainDataTable, project='pcod', stat.var = "no_unique_obs",
  #'                 variable='HAUL', gridfile='spatdat', lon.dat = 'MidLat', 
  #'                 lat.dat = 'MidLat', cat = 'NMFS_AREA')
  #' }
  
  dataset <- dat
  dat <- deparse(substitute(dat))
  
  if('ZoneID' %in% names(dataset) == FALSE){
  dataset <- assignment_column(dat = dataset, gridfile = gridfile, hull.polygon = TRUE, lon.grid, lat.grid, 
                               lon.dat, lat.dat, cat, closest.pt = TRUE, epsg = NULL)
  }
  
  date.var <- grep("date", names(dataset), ignore.case = TRUE)[1]
  var <- grep(variable, names(dataset), ignore.case = TRUE)[1]
  
  if (stat.var == "mean") {
    lab <- paste("mean", names(dataset)[var])
  } else if (stat.var == "median") {
    lab <- paste("median", names(dataset)[var])
  } else if (stat.var == "min") {
    lab <- paste("min", names(dataset)[var])
  } else if (stat.var == "max") {
    lab <- paste("max", names(dataset)[var])
  } else if (stat.var == "sum") {
    lab <- paste("sum", names(dataset)[var])
  } else if (stat.var == "length") {
    lab <- paste("No. observations", names(dataset)[var])
  }
  graphics::par(mfrow = c(1, 2))
  
  if (stat.var == "no_unique_obs") {
    plotout <-    graphics::plot(aggregate(dataset[[variable]], by = list(dataset[, date.var]), function(x) length(unique(x))), type = "l", lty = 1, ylab = paste("No. unique obs", 
                                                                                                                                                                  names(dataset)[var], "per day"), xlab = "dataset")
    graphics::lines(aggregate(dataset[[variable]], by = list(dataset[, date.var]), function(x) length(unique(x))))
    graphics::plot(aggregate(dataset[[variable]], by = list(as.factor(dataset$ZoneID)), function(x) length(unique(x))), type = "l", lty = 1, ylab = paste("No. unique obs", 
                                                                                                                                                          names(dataset)[var]), xlab = "Zone")
    lines(aggregate(dataset[[variable]], by = list(as.factor(dataset$ZoneID)), function(x) length(unique(x))))
  } else if (stat.var == "perc_total") {
    plotout <-     plot(aggregate(dataset[[variable]], by = list(dataset[, date.var]), function(x) length((x))/length(dataset[[variable]]) * 100), type = "l", lty = 1, 
                        ylab = paste("Percent total", names(dataset)[var], "per day"), xlab = "dataset")
    lines(aggregate(dataset[[variable]], by = list(dataset[, date.var]), function(x) length((x))/length(dataset[[variable]]) * 100))
    plot(aggregate(dataset[[variable]], by = list(as.factor(dataset$ZoneID)), function(x) length((x))/length(dataset[[variable]]) * 100), type = "l", 
         lty = 1, ylab = paste("Percent total", names(dataset)[var]), xlab = "Zone")
    lines(aggregate(dataset[[variable]], by = list(as.factor(dataset$ZoneID)), function(x) length((x))/length(dataset[[variable]]) * 100))
  } else {
    plotout <-      plot(aggregate(dataset[[variable]], by = list(dataset[, date.var]), stat.var), type = "l", lty = 1, ylab = paste(lab, "per day"), xlab = "dataset")
    lines(aggregate(dataset[[variable]], by = list(dataset[, date.var]), stat.var))
    plot(aggregate(dataset[[variable]], by = list(as.factor(dataset$ZoneID)), stat.var), type = "l", ylab = lab, xlab = "Zone")
    lines(aggregate(dataset[[variable]], by = list(as.factor(dataset$ZoneID)), stat.var))
  }
  
  spatial_summary_function <- list()
  spatial_summary_function$functionID <- "spatial_summary"
  spatial_summary_function$args <- list(dat, project, stat.var, variable, gridfile, lon.dat, lat.dat, cat, lon.grid, lat.grid )
  spatial_summary_function$kwargs <- list()
  spatial_summary_function$output <- c()
  log_call(spatial_summary_function)
  
  save_plot(project, "spatial_summary")
  plotout
  
}


