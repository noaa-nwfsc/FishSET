getis_ord_stats <- function(dat, project, varofint, spat, lon.dat = NULL, lat.dat = NULL, cat = NULL, lon.grid = NULL, lat.grid = NULL) {
  #' Calculate and view Getis-Ord statistic
  #'
  #' Wrapper function to calculate global and local Getis-Ord by discrete area
  #'
  #' @param dat Primary data containing information on hauls or trips.
  #'   Table in the FishSET database contains the string 'MainDataTable'.
  #' @param project String, name of project.
  #' @param varofint Numeric variable in \code{dat} to test for spatial high/low clustering.
  #' @param spat Spatial data containing information on fishery management or regulatory zones.
  #'  Can be shape file, json, geojson, data frame, or list.
  #' @param lon.dat Longitude variable in \code{dat}.
  #' @param lat.dat Latitude variable in \code{dat}.
  #' @param lon.grid Variable or list from \code{spat} containing longitude data. Required for csv files.
  #'   Leave as NULL if \code{spat} is a shape or json file.
  #' @param lat.grid Variable or list from \code{spat} containing latitude data. Required for csv files.
  #'   Leave as NULL if \code{spat} is a shape or json file.
  #' @param cat Variable defining the individual areas or zones.
  #' @details Calculates the degree, within each zone, that high or low values of the \code{varofint} cluster in space.
  #'   Function utilizes the \code{\link[spdep]{localG}} and \code{\link[spdep]{knearneigh}} functions from the spdep package.
  #'   The spatial input is a row-standardized spatial weights matrix for computed nearest neighbor matrix,
  #'   which is the null setting for the \code{\link[spdep]{nb2listw}} function. Requires a data frame with
  #'   area as a factor, the lon/lat centroid for each area, the lat/lon outlining each area, and the variable of
  #'   interest (\code{varofint}) or a map file with lat/lon defining boundaries of area/zones and variable of interest
  #'   for weighting. Also required is the lat/lon defining the center of a zone/area. If the centroid is not included in
  #'   the map file, then \code{\link{find_centroid}} can be called to calculate the centroid of each zone. If the
  #'   variable of interest is not associated with an area/zone then the \code{\link{assignment_column}} function can be used to
  #'   assign each observation to a zone. Arguments to identify centroid and assign variable of interest to area/zone are optional and default to NULL.
  #' @return Returns a plot and table. Both are saved to the output folder.
  # @aliases  moranmap: ggplot2 object; morantable: table of statistics
  #' @import ggplot2
  #' @importFrom maps map
  #' @importFrom spdep knn2nb knearneigh nb2listw localG globalG.test
  #' @importFrom sf st_coordinates st_centroid st_geometry
  #' @importFrom shiny isRunning
  #' @export
  #' @examples
  #' \dontrun{
  #' getis_ord_stats(pcodMainDataTable, project = 'pcod', varofint = 'OFFICIAL_MT_TONS',
  #'   spat = spatdat, lon.dat = 'LonLat_START_LON', lat.dat = 'LonLat_START_LAT', cat = 'NMFS_AREA')
  #' }
  #'

  # requireNamespace("ggplot2")
  world <- ggplot2::map_data("world")

  # Call in datasets
  out <- data_pull(dat, project)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main")
  
  
  spat_out <- data_pull(spat, project)
  spatdat <- spat_out$dataset
  spat <- parse_data_name(dat, "spat")
  
    if ("sf" %in% class(spatdat) == FALSE) {
      spatdat <- sf::st_as_sf(spatdat)
    }

  GetisOrd <- NA
  
  x <- 0
  if (any(abs(dataset[[lon.dat]]) > 180)) {
    warning("Longitude is not valid (outside -180:180). Function not run")
    # stop('Longitude is not valid (outside -180:180.')
    x <- 1
  }
  if (any(abs(dataset[lat.dat]) > 90)) {
    warning("Latitude is not valid (outside -90:90. Function not run")
    x <- 1
    # stop('Latitude is not valid (outside -90:90.')
  }

  if (x == 0) {
    # Assign data to zone
    if (!is.null(cat)) {
      dataset <- assignment_column(dat=dataset, project=project, gridfile=spatdat, hull.polygon = TRUE, 
                                   lon.dat=lon.dat, lat.dat=lat.dat, cat=cat, closest.pt = TRUE, 
                                   lon.grid=lon.grid, lat.grid=lat.grid, epsg = NULL, log.fun = FALSE)
      
      
      # Idenfity centroid of zone
      int <- find_centroid(gridfile=spatdat, cat = cat, lon.grid=lon.grid, lat.grid=lat.grid)
    }
    
     
     # datatomap <- merge(temp, int, by='ZoneID')
      int <- merge(int, dataset[, c(varofint, "ZoneID")], by = "ZoneID")
      names(int)[2] = "centroid_lon"
      names(int)[3] = "centroid_lat"
      names(int)[4] = "varofint"
      

  
    #  Identify variable of interest 
    int[["varofint"]] <- with(int, ave(int[["varofint"]], ZoneID, FUN = function(x) mean(x, na.rm = TRUE)))
    uniquedatatomap <- int[!duplicated(int$ZoneID), c("ZoneID", "centroid_lon", "centroid_lat", "varofint")]
    
      if(min(uniquedatatomap$centroid_lon) <0  & max(uniquedatatomap$centroid_lon) > 0) {
      spatdat$geometry = (sf::st_geometry(spatdat) + c(360,90)) %% c(360) - c(0,90)
    }
    
    nb.rk <- spdep::knn2nb(spdep::knearneigh(as.matrix(uniquedatatomap[, c("centroid_lon", "centroid_lat")]), longlat = TRUE))
    locg <- spdep::localG(uniquedatatomap[["varofint"]], listw = spdep::nb2listw(nb.rk))
      
    uniquedatatomap$GetisOrd <- round(locg, 3)
      
    globalgetis <- spdep::globalG.test(uniquedatatomap[["varofint"]], listw = spdep::nb2listw(nb.rk, style = "B"))
      
 
    g <- as.data.frame(spatdat[[cat]])
    colnames(g) = 'ZoneID'
    g <- merge(uniquedatatomap, g, all.y = TRUE)
    spatdat$GetisOrd <- g$GetisOrd
    spatdat <- cbind(spatdat, sf::st_coordinates(sf::st_centroid(spatdat)))
    spatdat <- subset(spatdat, !is.na(spatdat$GetisOrd))
    
   
    minlon <- min(spatdat$X) * 1.001 # lon negative
    maxlon <- max(spatdat$X) * 0.985
    minlat <- min(spatdat$Y) * 0.992
    maxlat <- max(spatdat$Y) * 1.001
    
    annotatesize <- 6

    getismap <- ggplot(data = spatdat) +
      geom_sf(data = spatdat, mapping = aes(fill = GetisOrd), show.legend = FALSE)+
      xlim(minlon, maxlon) +
      ylim(minlat, maxlat)+ 
      scale_fill_gradient2(
        low = "skyblue2",
        high = "firebrick1", mid = "white", name = "Local\nGetis-Ord"
      )  +
      geom_map(
        data = world,
        map = world, aes(map_id = world$region), fill = "grey", color = "black", size = 0.375
      ) +
      ggtitle("Getis-Ord statistics") +
      annotate(
        geom = "text", x = min(spatdat$X) * 0.9915,  y = min(spatdat$Y) * 0.997,
        label = paste0("Global Getis-Ord = ", round(globalgetis$estimate[1], 2)), parse = FALSE, size = annotatesize,
        color = "black", hjust = 0
      ) +
      annotate(geom = "text", x = min(spatdat$X) * 0.9915, y = min(spatdat$Y) * 0.994, 
               label = paste0("p-value = ", round(globalgetis$p.value, 2)  ),
               parse = FALSE, size = annotatesize, color = "black", hjust = 0) +
      theme(
        text = element_text(size = 20), axis.title.y = element_text(vjust = 1.5),
        legend.position = c(0.875, 0.7), legend.text = element_text(size = 15), legend.title = element_text(size = 15)
      ) +
      xlab("Longitude") +
      ylab("Latitude")
    
    getis_ord_stats_function <- list()
    getis_ord_stats_function$functionID <- "getis_ord_stats"
    getis_ord_stats_function$args <- list(dat, project, varofint, spat, lon.dat, lat.dat, cat, lon.grid, lat.grid)

    log_call(project, getis_ord_stats_function)

    return(list(getismap = getismap, getistable = uniquedatatomap[, c("ZoneID", "GetisOrd")]))


    save_plot(project, "getis_ord_stats", getismap)

    save_table(uniquedatatomap, project, "getis_ord_stats")
  }
}
