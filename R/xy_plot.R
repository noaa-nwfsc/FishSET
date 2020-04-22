xy_plot <- function(dat, lat, lon, minmax=NULL, percshown=NULL){
  #' Plot observed locations on map
  #'
  #' @param dat Main data frame over which to apply function. Table in FishSET database should contain the string `MainDataTable`.
  # @param gridfile
  #' @param lat Variable in dat that defines latitude, in decimal degrees
  #' @param lon Variable in dat that defines longitude, in decimal degrees
  #' @param minmax Optional map extent parameter, a vector (num) of length 4 
  #' corresponding to c(minlat, maxlat, minlon, maxlon).
  #' @param percshown whole number. Percent of points to show. Use this option if there are a lot of data points.
  #' @keywords map
  #' @description Plot observed locations on a map. For large datasests, it is best to plot a subset of points. 
  #' Use percshown to randomly subset the number of points. If the predefined map extent needs adjusting, use minmax.
  #' @return mapout: ggplot2 object
  #' @import ggplot2
  #' @importFrom maps map
  #' @export
  #' @examples
  #' \dontrun{
  #' map_plot('pollockMainDataTable', 'LonLat_START_LAT', 'LonLat_START_LON', percshown=10)
  #' }
  
  requireNamespace('ggplot2')
  world <- ggplot2::map_data("world")
  
  
  #Call in datasets
  out <- data_pull(dat)
  dat <- out$dat
  dataset <- out$dataset
  
  x <- 0
  
  
  
  
  
  
  
  
  
  #Log the function 
  
  map_plot_function <- list()
  map_plot_function$functionID <- "map_plot"
  map_plot_function$args <- c(dat, lat, lon, minmax, percshown)
  log_call(map_plot_function)
  
  # Save output
  
  save_plot(project, "map_plot")
  
  plot
  
  
}

    