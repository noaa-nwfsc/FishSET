getis_ord_stats <- function(dat, project, varofint, zoneid, spat, cat, lon.dat = NULL, 
                            lat.dat = NULL, lon.spat = NULL, lat.spat = NULL) {
  #' Calculate and view Getis-Ord statistic
  #'
  #' Wrapper function to calculate global and local Getis-Ord by discrete area
  #'
  #' @param dat Primary data containing information on hauls or trips.
  #'   Table in the FishSET database contains the string 'MainDataTable'.
  #' @param project String, name of project.
  #' @param varofint Numeric variable in \code{dat} to test for spatial high/low clustering.
  #' @param zoneid Variable in \code{dat} that identifies the individual zones or 
  #'   areas. Define if exists in \code{dat} and is not named `ZoneID`. Defaults to NULL. 
  #' @param spat Spatial data containing information on fishery management or 
  #'   regulatory zones. See \code{\link{load_spatial}}.
  #' @param cat Variable in \code{spat} defining the individual areas or zones.
  #' @param lon.dat Longitude variable in \code{dat}.Require if \code{zoneid} is not defined.
  #' @param lat.dat Latitude variable in \code{dat}. Require if \code{zoneid} is not defined.
  #' @param lon.spat Variable or list from \code{spat} containing longitude data. Required for csv files.
  #'   Leave as NULL if \code{spat} is a shape or json file.
  #' @param lat.spat Variable or list from \code{spat} containing latitude data. Required for csv files.
  #'   Leave as NULL if \code{spat} is a shape or json file.
  #' @details Calculates the degree, within each zone, that high or low values of
  #'   the \code{varofint} cluster in space. Function utilizes the \code{\link[spdep]{localG}} 
  #'   and \code{\link[spdep]{knearneigh}} functions from the spdep package. The 
  #'   spatial input is a row-standardized spatial weights matrix for computed nearest 
  #'   neighbor matrix, which is the null setting for the \code{\link[spdep]{nb2listw}}
  #'   function. Requires a data frame with area as a factor, the lon/lat centroid 
  #'   for each area, the lat/lon outlining each area, and the variable of interest 
  #'   (\code{varofint}) or a map file with lat/lon defining boundaries of area/zones 
  #'   and variable of interest for weighting. Also required is the lat/lon defining 
  #'   the center of a zone/area. If the centroid is not included in the map file, 
  #'   then \code{\link{find_centroid}} can be called to calculate the centroid of 
  #'   each zone. If the variable of interest is not associated with an area/zone 
  #'   then the \code{\link{assignment_column}} function can be used to assign each 
  #'   observation to a zone. Arguments to identify centroid and assign variable of 
  #'   interest to area/zone are optional and default to NULL.
  #' @return Returns a plot and table. Both are saved to the output folder.
  # @aliases  moranmap: ggplot2 object; morantable: table of statistics
  #' @import ggplot2
  #' @importFrom spdep knn2nb knearneigh nb2listw localG globalG.test
  #' @importFrom sf st_coordinates st_centroid st_geometry st_make_valid
  #' @importFrom shiny isRunning
  #' @export
  #' @examples
  #' \dontrun{
  #' getis_ord_stats(pcodMainDataTable, project = 'pcod', varofint = 'OFFICIAL_MT_TONS',
  #'   spat = spatdat, lon.dat = 'LonLat_START_LON', lat.dat = 'LonLat_START_LAT', cat = 'NMFS_AREA')
  #' }
  #'

  out <- data_pull(dat, project)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main", project)
  
  spat_out <- data_pull(spat, project)
  spatdat <- spat_out$dataset
  spat <- parse_data_name(spat, "spat", project)
  
  spatdat <- check_spatdat(spatdat = spatdat, lon = lon.spat, lat = lat.spat, id = cat)
  
  GetisOrd <- NA
 
 #Assignment column
  if ('ZoneID' %in% names(dataset)) {
    
      zoneid <- 'ZoneID'
      
  } else if (!is.null(zoneid) && zoneid %in% names(dataset)) {
      
    colnames(dataset)[colnames(dataset)==zoneid] <- 'ZoneID'
      
  } else {
      
    if (is.null(spat) || is.null(lon.dat)) {
      
      stop('Observations must be assigned to zones. Function not run.')
      
    } else {
      
      if (any(abs(dataset[[lon.dat]]) > 180)) {
        
        stop("Longitude is not valid (outside -180:180). Function not run")
      }
      
      if (any(abs(dataset[lat.dat]) > 90)) {
        
        stop("Latitude is not valid (outside -90:90. Function not run")
      }
      
      dataset <- suppressWarnings(assignment_column(dat=dataset, project=project, spat=spatdat, hull.polygon = TRUE, 
                                                    lon.dat=lon.dat, lat.dat=lat.dat, cat=cat, closest.pt = TRUE, 
                                                    lon.spat=lon.spat, lat.spat=lat.spat, epsg = NULL, log.fun = FALSE))
    }
  }

  ## Centroid table
    if (table_exists(paste0(spat, 'Centroid'), project)) {
      
      int <- table_view(paste0(spat, 'Centroid'), project)
      
    } else if (table_exists('spatCentroid', project)) {
      
      int <- table_view('spatCentroid', project)
      
  } else {
    
    int <- suppressWarnings(
    find_centroid(spat=spatdat, project = project, spatID = cat, 
                  lon.spat=lon.spat, lat.spat=lat.spat, log.fun = FALSE)
    )
  }
   
 # datatomap <- merge(temp, int, by='ZoneID')
  int <- merge(int, dataset[, c(varofint, "ZoneID")], by = "ZoneID")
  names(int)[2] = "centroid_lon"
  names(int)[3] = "centroid_lat"
  names(int)[4] = "varofint"
    

  #  Identify variable of interest 
  int[["varofint"]] <- with(int, ave(int[["varofint"]], ZoneID, FUN = function(x) mean(x, na.rm = TRUE)))
  uniquedatatomap <- int[!duplicated(int$ZoneID), c("ZoneID", "centroid_lon", "centroid_lat", "varofint")]
  
    if(min(uniquedatatomap$centroid_lon) < 0 & max(uniquedatatomap$centroid_lon) > 0) {
    spatdat$geometry = (sf::st_geometry(spatdat) + c(360,90)) %% c(360) - c(0,90)
  }
  
  nb.rk <- spdep::knn2nb(spdep::knearneigh(as.matrix(uniquedatatomap[, c("centroid_lon", "centroid_lat")]), longlat = TRUE))
  locg <- spdep::localG(uniquedatatomap[["varofint"]], listw = spdep::nb2listw(nb.rk))
    
  uniquedatatomap$GetisOrd <- round(locg, 3)
    
  globalgetis <- spdep::globalG.test(uniquedatatomap[["varofint"]], listw = spdep::nb2listw(nb.rk, style = "B"))

  spatdat <-  merge(spatdat, uniquedatatomap[,c('ZoneID','GetisOrd')], by.x=cat, by.y='ZoneID')#spatdat$GetisOrd <- g$GetisOrd
  
  spatdat <- cbind(spatdat, sf::st_coordinates(sf::st_centroid(sf::st_make_valid(spatdat))))
  spatdat <- subset(spatdat, !is.na(spatdat$GetisOrd))
  
  minlon <- min(spatdat$X) * 1.001 # lon negative
  maxlon <- max(spatdat$X) * 0.985
  minlat <- min(spatdat$Y) * 0.992
  maxlat <- max(spatdat$Y) * 1.001
  
  world <- ggplot2::map_data("world")
  annotatesize <- 6

  getismap <- ggplot2::ggplot(data = spatdat) +
    ggplot2::geom_sf(data = spatdat, mapping =  ggplot2::aes(fill = GetisOrd), 
                     show.legend = FALSE)+
    xlim(minlon, maxlon) +
    ylim(minlat, maxlat)+ 
    ggplot2::scale_fill_gradient2(
      low = "skyblue2",
      high = "firebrick1", mid = "white", name = "Local\nGetis-Ord"
    )  +
    ggplot2::geom_map(
      data = world,
      map = world,  ggplot2::aes(map_id = .data$region), fill = "grey",
      color = "black", size = 0.375
    ) +
    ggplot2::ggtitle("Getis-Ord statistics") +
    ggplot2::annotate(
      geom = "text", x = min(spatdat$X) * 0.9915,  y = min(spatdat$Y) * 0.997,
      label = paste0("Global Getis-Ord = ", round(globalgetis$estimate[1], 2)), 
      parse = FALSE, size = annotatesize,
      color = "black", hjust = 0
    ) +
    ggplot2::annotate(geom = "text", x = min(spatdat$X) * 0.9915, y = min(spatdat$Y) * 0.994, 
             label = paste0("p-value = ", round(globalgetis$p.value, 2)  ),
             parse = FALSE, size = annotatesize, color = "black", hjust = 0) +
    ggplot2::theme(
      text = element_text(size = 20), axis.title.y = ggplot2::element_text(vjust = 1.5),
      legend.position = c(0.875, 0.7), legend.text = ggplot2::element_text(size = 15), 
      legend.title = ggplot2::element_text(size = 15)
    ) +
    xlab("Longitude") +
    ylab("Latitude")
    
    getis_ord_stats_function <- list()
    getis_ord_stats_function$functionID <- "getis_ord_stats"
    getis_ord_stats_function$args <- list(dat, project, varofint, zoneid, spat, 
                                          lon.dat, lat.dat, cat, lon.spat, lat.spat)

    log_call(project, getis_ord_stats_function)

    save_plot(project, "getis_ord_stats", getismap)
    save_table(uniquedatatomap, project, "getis_ord_stats")
    
 return(list(getismap = getismap, getistable = uniquedatatomap[, c("ZoneID", "GetisOrd")]))
}
