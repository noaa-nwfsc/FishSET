map_plot <- function(dat, project, lat, lon, minmax = NULL, percshown = NULL) {
  #' Map observed vessel locations
  #'
  #' @param dat Primary data containing information on hauls or trips. Table in FishSET database contains the string 'MainDataTable'.
  # @param gridfile
  #' @param project String, project name.
  #' @param lat Variable in \code{dat} that defines latitude, in decimal degrees.
  #' @param lon Variable in \code{dat} that defines longitude, in decimal degrees.
  #' @param minmax Optional map extent argument, a vector (num) of length four
  #' corresponding to c(minlat, maxlat, minlon, maxlon).
  #' @param percshown Whole number, percent of points to show. Use this option if there are a lot of data points.
  #' @description Plot observed locations on a map. For large datasets, it is best to plot a subset of points.
  #' Use \code{percshown} to randomly subset the number of points. If the predefined map extent needs adjusting,
  #'  use \code{minmax}.
  #' @return mapout: ggplot2 object
  #' @import ggplot2
  #' @importFrom maps map
  #' @export
  #' @examples
  #' \dontrun{
  #' map_plot(pollockMainDataTable, 'pollock', 'LonLat_START_LAT', 'LonLat_START_LON', percshown=10)
  #' }

  requireNamespace("ggplot2")
  world <- ggplot2::map_data("world")

  # Call in datasets
  out <- data_pull(dat)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main")
  
  end <- FALSE

  if (any(abs(dataset[[lon]]) > 180)) {
    warning("Longitude is not valid (outside -180:180). Function not run.")
    end <- TRUE
  }
  if (any(abs(dataset[[lat]]) > 90)) {
    warning("Latitude is not valid (outside -90:90. Function not run.")
    end <- TRUE
  }

  if (end == FALSE) {
    datatomap <- as.data.frame(cbind(dataset[[lat]], dataset[[lon]]))
    colnames(datatomap) <- c("lat", "lon")

    if (!is.null(percshown)) {
      datatomap <- datatomap[sample(nrow(datatomap), nrow(datatomap) / percshown), ]
    }

    if (is.null(minmax)) {
      if (min(datatomap$lat) < 0 & max(datatomap$lat) < 0) {
        minlat <- min(datatomap$lat) * 1.005
        maxlat <- max(datatomap$lat) * 0.995
      } else if (min(datatomap$lat) > 0 & max(datatomap$lat) > 0) {
        minlat <- min(datatomap$lat, datatomap$lat) * 0.95
        maxlat <- max(datatomap$lat, datatomap$lat) * 1.01
      } else {
        minlat <- min(datatomap$lat, datatomap$lat) * 0.995
        maxlat <- max(datatomap$lat, datatomap$lat) * 1.005
        warning("User should specify own minmax lat")
      }

      if (min(datatomap$lon) < 0 & max(datatomap$lon) < 0) {
        minlon <- min(datatomap$lon) * 1.01
        maxlon <- max(datatomap$lon) * 0.95
      } else if (min(datatomap$lon) > 0 & max(datatomap$lon) > 0) {
        minlon <- min(datatomap$lon, datatomap$lon) * 0.995
        maxlon <- max(datatomap$lon, datatomap$lon) * 1.005
      } else {
        minlon <- min(datatomap$lon, datatomap$lon) * 0.995
        maxlon <- max(datatomap$lon, datatomap$lon) * 1.005
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

    if (is.null(percshown)) {
      gptitle <- "Observed locations"
    } else {
      gptitle <- paste0("Observed locations. ", percshown, "% of points shown.")
    }
    
    cf <- ggplot2::coord_fixed()
    cf$default <- TRUE
  
    m_plot <- 
      ggplot2::ggplot() +
      ggplot2::geom_map(data = world, map = world, 
                        ggplot2::aes(map_id = region), 
                        fill = "grey", color = "black", size = 0.375) +
      ggplot2::geom_point(data = datatomap, ggplot2::aes(x = lon, y = lat), 
                          size = 1, alpha = 0.25, color = "red") +
      cf + ggplot2::coord_fixed(xlim = c(minlon, maxlon), ylim = c(minlat, maxlat), 
                                ratio=1.3, expand = TRUE) +
      # ggplot2::xlim(minlon, maxlon) +
      # ggplot2::ylim(minlat, maxlat) +
      ggplot2::labs(title = gptitle, x = "Longitude", y = "Latitude") +
      ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size = 10))) +
      ggplot2::theme(panel.grid.major = ggplot2::element_blank(), 
                     panel.grid.minor = ggplot2::element_blank(), 
                     panel.background = ggplot2::element_blank(),  
                     axis.text=ggplot2::element_text(size=12),
                     axis.title=ggplot2::element_text(size=12),
                     panel.border = ggplot2::element_rect(colour = "black", fill = NA, size = 1))
  
    # Log the function
    map_plot_function <- list()
    map_plot_function$functionID <- "map_plot"
    map_plot_function$args <- list(dat, project, lat, lon, minmax, percshown)
    log_call(map_plot_function)
  
    # Save output
    save_plot(project, "map_plot")
  
    m_plot
  }
}
