map_kernel <- function(dat, project, type, latlon, group = NULL, facet = FALSE, 
                       date = NULL, filter_date = NULL, filter_value = NULL, 
                       minmax = NULL) {
  #'
  #' Map kernel density plots
  #'
  #' @param dat Primary data containing information on hauls or trips.
  #' Table in FishSET database contains the string 'MainDataTable'.
  #' @param project String, name of project.
  #' @param type String, plot type. Choices are \code{"point"}, \code{"contours"}, or
  #' \code{"gradient"}. Note if you have a group, you must facet when choosing \code{"gradient"}
  #' (cannot overlap polygons clearly).
  #' @param latlon Character string, specified as latitude then longitude, in decimal degrees.
  #' @param group Optional group argument. Should be a factor with length of (# of observations),
  #' where each observation corresponds to the latlon coordinate of the same
  #' index. Recall that the legend will output the names of
  #' factor levels as you have named them (see \code{?factor}).
  #' @param facet Optional facet parameter. TRUE if mapping each group as a
  #' separate facet. Defaults to FALSE.
  #' @param date Optional date variable to filter data by.
  #' @param filter_date Whether to filter data table by \code{"year"}, \code{"month"}, or
  #'   \code{"year-month"}. \code{date} and \code{filter_value} must be provided.
  #'   Defaults to \code{NULL}.
  #' @param filter_value Integer (4 digits if year, 1-2 if month). The year, month,
  #'   or year-month to filter data table by. Use a list if using \code{"year-month"},
  #'   with the format: list(year(s), month(s)). For example, \code{list(2011:2013, 5:7)}
  #'   will filter the data table from May to July, 2011-2013.
  #' @param minmax Optional map extent argument, a vector (num) of length 4
  #' corresponding to c(minlat, maxlat, minlon, maxlon).
  #' @return Returns ggplot2 object. Map plot saved to Output folder.
  #' @import ggplot2
  #' @importFrom maps map
  #' @export
  #' @examples
  #' \dontrun{
  #' map_kernel(pollockMainDataTable, project = 'pollock', type = 'contours',
  #' latlon = c('LonLat_START_LAT', 'LonLat_START_LON'), group = 'PORT_CODE',
  #' facet = TRUE, minmax = NULL, date = 'FISHING_START_DATE',
  #' filter_date = 'year-month', filter_value = list(2011, 2:4))
  #' }

  ## currently outputs to FishSET not file (could include dirout as argument)
  requireNamespace("ggplot2")
  world <- ggplot2::map_data("world")

  out <- data_pull(dat)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main")
  

  x <- 0
  level <- NA
  lon <- NA
  lat <- NA
  groupv <- NA
  
  #Empty variables
  year <- NULL
  month <- NULL
  
  datatomap <- as.data.frame(dataset[, c(latlon)])
  colnames(datatomap) <- c("lat", "lon")
  
  if (anyNA(datatomap)) {
    datatomap <- datatomap[-which(is.na(datatomap$lon) == TRUE | is.na(datatomap$lat) == TRUE), ]
  }

  if (!is.null(group)) {
    datatomap$groupv <- dataset[[group]]
  }

  if (!is.null(date) & !is.null(filter_date) & !is.null(filter_value)) {
    if (filter_date == "year-month") {
      datatomap$year <- as.integer(format(as.Date(dataset[[date]]), "%Y"))
      datatomap$month <- as.integer(format(as.Date(dataset[[date]]), "%m"))

      datatomap <- subset(datatomap, (year %in% filter_value[[1]]) & (month %in% filter_value[[2]]))
    } else if (filter_date == "year") {
      datatomap$year <- as.integer(format(dataset[[date]], "%Y"))

      datatomap <- subset(datatomap, year %in% filter_value)
    } else if (filter_date == "month") {
      datatomap$month <- as.integer(format(dataset[[date]], "%m"))

      datatomap <- subset(datatomap, month %in% filter_value)
    } else {
      warning("Invalid filter type. Available options are 'year-month', 'year', and 'month'.")
      x <- 1
    }

    if (nrow(datatomap) == 0) {
      warning("Filtered data table has zero rows. Check filter parameters.")
      x <- 1
    }
  }

  if (any(abs(datatomap$lon) > 180)) {
    warning("Longitude is not valid (outside -180:180). Function not run.")
    # stop('Longitude is not valid (outside -180:180.')
    x <- 1
  }
  if (any(abs(datatomap$lat) > 90)) {
    warning("Latitude is not valid (outside -90:90. Function not run.")
    x <- 1
    # stop('Latitude is not valid (outside -90:90.')
  }
  
  #Need to make adjustments if data is centered in the pacific.
  if(min(datatomap$lon) < 0 & max(datatomap$lon) > 0){
    recenter <- TRUE
    datatomap$lon <- ifelse(datatomap$lon < 0 , datatomap$lon + 360, datatomap$lon)
    worldmap <- map_data("world", wrap = c(0, 360))
    
  } else {
    recenter <- FALSE
    worldmap <- map_data("world")
    
  }

  if (x == 0) {
    if (is.null(minmax) == TRUE) {
      if (min(datatomap$lat) < 0 & max(datatomap$lat) < 0) {
        minlat <- min(datatomap$lat) * 1.001
        maxlat <- max(datatomap$lat) * 0.999
      } else if (min(datatomap$lat) > 0 & max(datatomap$lat) > 0) {
        minlat <- min(datatomap$lat, datatomap$lat) * 0.999
        maxlat <- max(datatomap$lat, datatomap$lat) * 1.001
      } else {
        minlat <- min(datatomap$lat, datatomap$lat) * 0.999
        maxlat <- max(datatomap$lat, datatomap$lat) * 1.001
        warning("User should specify own minmax lat")
      }

      if (min(datatomap$lon) < 0 & max(datatomap$lon) < 0) {
        minlon <- min(datatomap$lon) * 1.001
        maxlon <- max(datatomap$lon) * 0.999
      } else if (min(datatomap$lon) > 0 & max(datatomap$lon) > 0) {
        minlon <- min(datatomap$lon, datatomap$lon) * 0.999
        maxlon <- max(datatomap$lon, datatomap$lon) * 1.001
      } else {
        minlon <- min(datatomap$lon, datatomap$lon) * 0.999
        maxlon <- max(datatomap$lon, datatomap$lon) * 1.001
        warning("User should specify own minmax lon")
      }
    } else {
      if (length(minmax) == 4) {
        minlat <- minmax[1]
        maxlat <- minmax[2]
        minlon <- minmax[3]
        maxlon <- minmax[4]
      } else {
        stop("Variable minmax wrong dimensions")
      }
    }
    
    if(recenter == FALSE){
      recenterlab <- 0
    } else {
      recenterlab <-  360
    }
    
    ################################################################################
    
    
    gmap <-
      ggplot2::ggplot() +
      ggplot2::geom_map(map = world, ggplot2::aes(x = world$long, y = world$lat,
                                                  map_id = world$region),
                        fill = "grey", color = "black", size = 0.375)

    map_theme <-
      ggplot2::theme(text = ggplot2::element_text(size = 12),
                     axis.title.y = ggplot2::element_text(vjust = 1.5),
                     legend.title = ggplot2::element_blank(),
                     panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank(),
                     panel.background = ggplot2::element_blank(),
                     panel.border = ggplot2::element_rect(colour = "black",
                                                          fill = NA, size = 1))

    if (type == "point") {

      if (is.null(group) == FALSE & facet == FALSE) {

        mapout <-
          gmap +
          ggplot2::geom_point(data = datatomap, ggplot2::aes(x = lon, y = lat,
                                            colour = groupv),
                               size = 0.375, alpha = 0.25) +
          scale_x_continuous(labels = function(b) { b-recenterlab}) +
          ggplot2::xlim(minlon, maxlon) +
          ggplot2::ylim(minlat, maxlat) +
          ggplot2::ggtitle("Points") +
          ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size = 10))) +
          
          map_theme +
          ggplot2::xlab("Longitude") +
          ggplot2::ylab("Latitude")

      } else if (is.null(group) == FALSE & facet == TRUE) {

        mapout <-
          gmap +
          ggplot2::geom_point(data = datatomap, ggplot2::aes(x = lon, y = lat,
                                           colour = groupv), size = 0.375) +
          scale_x_continuous(labels = function(b) { b-recenterlab}) +
          ggplot2::xlim(minlon, maxlon) +
          ggplot2::ylim(minlat, maxlat) + 
          ggplot2::ggtitle("Points") +
          ggplot2::facet_grid(. ~ groupv) +
          map_theme + ggplot2::theme(legend.position = "none") +
          ggplot2::xlab("Longitude") +
          ggplot2::ylab("Latitude")

      } else {

        mapout <-
          gmap +
          ggplot2::geom_point(data = datatomap, ggplot2::aes(x = lon, y = lat),
                              color = "black", size = 0.375) +
          scale_x_continuous(labels = function(b) { b-recenterlab}) +
          ggplot2::xlim(minlon, maxlon) +
          ggplot2::ylim(minlat, maxlat) +
          ggplot2::ggtitle("Points") +
          map_theme +
          ggplot2::xlab("Longitude") +
          ggplot2::ylab("Latitude")
      }

      ################################################################################
    } else if (type == "contours") {

      if (is.null(group) == FALSE & facet == FALSE) {

        mapout <-
          gmap +
          ggplot2::geom_point(data = datatomap, ggplot2::aes(x = lon, y = lat,
                                           colour = groupv),
                              color = "black", size = 0.375) +
          ggplot2::geom_density_2d(data = datatomap, ggplot2::aes(x = lon, y = lat,
                                                                  colour = groupv)) +
          scale_x_continuous(labels = function(b) { b-recenterlab}) +
          ggplot2::xlim(minlon, maxlon) +
          ggplot2::ylim(minlat, maxlat) +
          ggplot2::ggtitle("Spatial kernel (contours)") +
          map_theme +
          ggplot2::xlab("Longitude") +
          ggplot2::ylab("Latitude")

      } else if (is.null(group) == FALSE & facet == TRUE) {

        mapout <-
          gmap +
          ggplot2::geom_point(data = datatomap, ggplot2::aes(x = lon, y = lat,
                                           colour = groupv),
                              color = "black", size = 0.375) +
          ggplot2::geom_density_2d(data = datatomap, ggplot2::aes(x = lon, y = lat,
                                                                  colour = groupv)) +
          scale_x_continuous(labels = function(b) { b-recenterlab}) +
          ggplot2::xlim(minlon, maxlon) +
          ggplot2::ylim(minlat, maxlat) +
          ggplot2::ggtitle("Spatial kernel (contours)") +
          ggplot2::facet_grid(. ~ groupv) +
          map_theme + ggplot2::theme(legend.position = "none") +
          ggplot2::xlab("Longitude") +
          ggplot2::ylab("Latitude")

      } else {

        mapout <-
          gmap +
          ggplot2::geom_point(ggplot2::aes(x = datatomap$lon, y = datatomap$lat),
                              color = "black", size = 0.375) +
          ggplot2::geom_density_2d(ggplot2::aes(x = datatomap$lon, y = datatomap$lat)) +
          scale_x_continuous(labels = function(b) { b-recenterlab}) +
          ggplot2::xlim(minlon, maxlon) +
          ggplot2::ylim(minlat, maxlat) +
          ggplot2::ggtitle("Spatial kernel (contours)") +
          map_theme +
          ggplot2::xlab("Longitude") +
          ggplot2::ylab("Latitude")
      }

      ################################################################################
    } else if (type == "gradient") {
      if (is.null(group) == FALSE & facet == FALSE) {

        ## can't overlap gradient (use alpha?)

        stop("Cannot overlap gradients please facet or use contours")

      } else if (is.null(group) == FALSE & facet == TRUE) {

        mapout <-
          gmap +
          ggplot2::geom_point(data = datatomap, ggplot2::aes(x = lon, y = lat,
                                                             colour = groupv),
                              color = "black", size = 0.375) +
          ggplot2::stat_density_2d(data = datatomap, ggplot2::aes(x = lon, y = lat,
                                                fill = stat(level)), geom = "polygon") +
          scale_x_continuous(labels = function(b) { b-recenterlab}) +
          ggplot2::xlim(minlon, maxlon) +
          ggplot2::ylim(minlat, maxlat) +
          ggplot2::facet_grid(. ~ groupv) +
          ggplot2::ggtitle("Spatial kernel (gradient)") +
          map_theme +
          ggplot2::theme(legend.title = ggplot2::element_text()) +
          ggplot2::scale_fill_gradient(name = "Level\n(density)") +
          ggplot2::xlab("Longitude") +
          ggplot2::ylab("Latitude")

      } else {

        mapout <-
          gmap +
          ggplot2::geom_point(data = datatomap, ggplot2::aes(x = lon, y = lat),
                              color = "black", size = 0.375) +
          ggplot2::stat_density_2d(data = datatomap, ggplot2::aes(x = lon, y = lat,
                                                fill = stat(level)), geom = "polygon") +
          scale_x_continuous(labels = function(b) { b-recenterlab}) +
          ggplot2::xlim(minlon, maxlon) +
          ggplot2::ylim(minlat, maxlat) +
          ggplot2::ggtitle("Spatial kernel (gradient)") +
          map_theme +
          ggplot2::theme(legend.title = ggplot2::element_text()) +
          ggplot2::scale_fill_gradient(name = "Level\n(density)") +
          ggplot2::xlab("Longitude") +
          ggplot2::ylab("Latitude")
      }
    }
   
    # add year/month to title
    if (!is.null(date) & !is.null(filter_date) & !is.null(filter_value)) {
      mapout <- date_title(mapout, filter_date, filter_value)
    }

    map_kernel_function <- list()
    map_kernel_function$functionID <- "map_kernel"
    map_kernel_function$args <- list(dat, project, type, latlon, group, facet, 
                                     date, filter_date, filter_value, minmax)
    log_call(project, map_kernel_function)

    save_plot(project, "map_kernel", mapout)

    print(mapout)
  }
}

