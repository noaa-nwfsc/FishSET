map_plot <- function(dat, project, lat, lon, minmax=NULL, percshown=NULL){
  #' Plot observed locations on map
  #'
  #' @param dat Main data frame over which to apply function. Table in FishSET database should contain the string `MainDataTable`.
  # @param gridfile
  #' @param project Project name
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
  #' map_plot('pollockMainDataTable', 'pollock', 'LonLat_START_LAT', 'LonLat_START_LON', percshown=10)
  #' }
  
  requireNamespace('ggplot2')
  world <- ggplot2::map_data("world")
  
  
  #Call in datasets
  out <- data_pull(dat)
  dat <- out$dat
  dataset <- out$dataset
  
  x <- 0
  
  if (any(abs(dataset[[lon]]) > 180)) {
    warning("Longitude is not valid (outside -180:180). Function not run.")
    x <- 1
  }
  if (any(abs(dataset[[lat]]) > 90)) {
    warning("Latitude is not valid (outside -90:90. Function not run.") 
    x <-1    
  } 


  
  if(x == 0) {
    datatomap <- as.data.frame(cbind(dataset[[lat]], dataset[[lon]]))
    colnames(datatomap)=c('lat', 'lon')
    
    if(!is.null(percshown)){
     datatomap <- datatomap[sample(nrow(datatomap), nrow(datatomap)/percshown),]
    }
  }
  
  if(x==0){
    if (is.null(minmax)) {
      
      if (min(datatomap$lat) < 0 & max(datatomap$lat) < 0) {
        minlat <- min(datatomap$lat)*1.005
        maxlat <- max(datatomap$lat)*0.995
      } else if (min(datatomap$lat) > 0 & max(datatomap$lat) > 0) {
        minlat <- min(datatomap$lat,datatomap$lat)*0.95
        maxlat <- max(datatomap$lat,datatomap$lat)*1.01
      } else {
        minlat <- min(datatomap$lat,datatomap$lat)*0.995
        maxlat <- max(datatomap$lat,datatomap$lat)*1.005
        warning( "User should specify own minmax lat" )
      }
      
      if (min(datatomap$lon) < 0 & max(datatomap$lon) < 0) {
        minlon <- min(datatomap$lon)*1.01
        maxlon <- max(datatomap$lon)*0.95
      } else if (min(datatomap$lon) > 0 & max(datatomap$lon) > 0) {
        minlon <- min(datatomap$lon,datatomap$lon)*0.995
        maxlon <- max(datatomap$lon,datatomap$lon)*1.005
      } else {
        minlon <- min(datatomap$lon,datatomap$lon)*0.995
        maxlon <- max(datatomap$lon,datatomap$lon)*1.005
        warning("User should specify own minmax lon")
      }
      
    } else {
      if (length(minmax) == 4) {
        minlat <- minmax[1]
        maxlat <- minmax[2]
        minlon <- minmax[3]
        maxlon <- minmax[4]
      } else {
        warning("Variable minmax wrong dimensions")
      }
    }
  }
   
  if(is.null(percshown)){
    gptitle <- "Observed locations"
  } else {
    gptitle <- paste0('Observed locations. ', percshown, '% of points shown.')
  }

  plot <- ggplot2::ggplot() +
    ggplot2::geom_map(data=world, map=world, ggplot2::aes(x=long, y=lat, map_id=region),
                      fill="grey", color="black", size=0.375) +
    ggplot2::geom_point(data=datatomap,ggplot2::aes(x=lon,y=lat), 
                        size=1, alpha = 0.25, color='red') +
    ggplot2::xlim(minlon,maxlon) + ggplot2::ylim(minlat,maxlat) +
    ggplot2::ggtitle(gptitle) + 
    ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size=10))) +
    ggplot2::theme(text = ggplot2::element_text(size=12), axis.title.y = ggplot2::element_text(vjust=1.5),
                   legend.title= ggplot2::element_blank(), panel.grid.major = ggplot2::element_blank(), 
                   panel.grid.minor = ggplot2::element_blank(), panel.background = ggplot2::element_blank(),
                   panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=1)) + 
    ggplot2::xlab("Longitude") + ggplot2::ylab("Latitude")
  
  
  #Log the function 
  
  map_plot_function <- list()
  map_plot_function$functionID <- "map_plot"
  map_plot_function$args <- c(dat, lat, lon, minmax, percshown)
  log_call(map_plot_function)
  
  # Save output
  
  save_plot(project, "map_plot")
  
  plot

  
}
