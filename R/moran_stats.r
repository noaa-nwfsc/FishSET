moran_stats <- function(dat, project, varofint, spat, lon.dat = NULL, lat.dat = NULL, cat = NULL, lon.grid = NULL, lat.grid = NULL) {
  #' Calculate and view Moran's I
  #'
  #' Wrapper function to calculate global and local Moran's I by discrete area.
  #'
  #' @param dat Primary data containing information on hauls or trips.
  #' Table in FishSET database contains the string 'MaindataTable'.
  #' @param project String, name of project.
  #' @param varofint Numeric variable from \code{dat} to test for spatial autocorrelation.
  #' @param spat Spatial data containing information on fishery management or regulatory zones.
  #' Shape, json, geojson, and csv formats are supported.
  #' @param lon.dat Longitude variable from \code{dat}.
  #' @param lat.dat Latitude variable from \code{dat}.
  #' @param lon.grid Variable or list from \code{spat} containing longitude data. Required for csv files.
  #' Leave as NULL if \code{spat} is a shape or json file.
  #' @param lat.grid Variable or list from \code{spat} containing latitude data. Required for csv files.
  #' Leave as NULL if \code{spat} is a shape or json file.
  #' @param cat Variable or list in \code{spat} that identifies the individual areas or zones.
  #' If \code{spat} is class sf, \code{cat} should be name of list containing information on zones.
  #' @details Measure degree of spatial autocorrelation. Function utilizes the \code{\link[spdep]{localmoran}}
  #' and \code{\link[spdep]{knearneigh}} functions from the spdep package. The spatial input is a row-standardized spatial
  #' weights matrix for computed nearest neighbor matrix, which is the null setting for the \code{\link[spdep]{nb2listw}}
  #' function. The function requires a map file with lat/lon defining boundaries of area/zones and \code{varofint}
  #' for to test for spatial autocorrelation. If zonal centroid is not included in the map file, then the \code{\link{find_centroid}}
  #' function is called to calculate the centroid of each zone. If the variable of interest is not associated with an
  #' area/zone then \code{\link{assignment_column}} is called to assign each observation to a zone. Arguments
  #' to identify centroid and assign variable of interest to area/zone are optional and default to NULL.
  #' @return Returns a plot and map of Moran’s I. Output is saved to the Output folder.
  #' @import ggplot2
  #' @importFrom maps map
  #' @importFrom spdep knn2nb knearneigh nb2listw localmoran moran.test
  #' @importFrom shiny isRunning
  #' @export
  #' @examples
  #' \dontrun{
  #' moran_stats(pcodMainDataTable, project='pcod', varofint='OFFICIAL_MT_TONS',
  #' spat=spatdat, lon.dat='LonLat_START_LON', lat.dat ='LonLat_START_LAT', cat='NMFS_AREA')
  #' }
  #'

  # requireNamespace("ggplot2")
  world <- map_data("world")

  # Call in datasets
  out <- data_pull(dat)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main")
  
  spat_out <- data_pull(spat)
  spatdat <- spat_out$dataset
  spat <- parse_data_name(spat, 'spat')
  
 
  x <- 0
  if (any(abs(dataset[[lon.dat]]) > 180)) {
    warning("Longitude is not valid (outside -180:180). Function not run")
    # stop('Longitude is not valid (outside -180:180.')
    x <- 1
  }
  if (any(abs(dataset[[lat.dat]]) > 90)) {
    warning("Latitude is not valid (outside -90:90. Function not run")
    x <- 1
    # stop('Latitude is not valid (outside -90:90.')
  }

  if (x == 0) {
    # Assign data to zone
    if (!is.null(cat)) {
      dataset <- assignment_column(dataset, project, spatdat, hull.polygon = TRUE, 
                                   lon.dat, lat.dat, cat, closest.pt = TRUE, lon.grid, 
                                   lat.grid, epsg = NULL)

      # Idenfity centroid of zone
      int <- find_centroid(dataset, spatdat, lon.dat, lat.dat, cat, 
                           lon.grid, lat.grid, weight.var = NULL)
    }

    # Create dataset
    if ("sf" %in% class(spatdat)) {
      nc_geom <- sf::st_geometry(spatdat)
      temp <- data.frame(ZoneID = rep(spatdat[[cat]][1], path = dim(as.data.frame(nc_geom[[1]][[1]]))[1]), as.data.frame(nc_geom[[1]][[1]]))
      for (i in 2:length(nc_geom)) {
        temp <- rbind(temp, data.frame(ZoneID = rep(spatdat[[cat]][i], path = dim(as.data.frame(nc_geom[[1]][[i]]))[1]), as.data.frame(nc_geom[[1]][[i]])))
      }
      # datatomap <- merge(temp, int, by='ZoneID')
      int <- merge(int, dataset[, c(varofint, "ZoneID")], by = "ZoneID")
      names(temp)[2] <- "path_lon"
      names(temp)[3] <- "path_lat"
      names(int)[2] <- "centroid_lon"
      names(int)[3] <- "centroid_lat"
      names(int)[4] <- "varofint"
    }
    # 4. Identify variable of interest 5.
    int[["varofint"]] <- with(int, ave(int[["varofint"]], ZoneID, FUN = function(x) mean(x, na.rm = TRUE)))
    uniquedatatomap <- int[!duplicated(int$ZoneID), c("ZoneID", "centroid_lon", "centroid_lat", "varofint")]

    nb.rk <- spdep::knn2nb(spdep::knearneigh(as.matrix(uniquedatatomap[, c("centroid_lon", "centroid_lat")]), longlat = TRUE))
    locm <- spdep::localmoran(uniquedatatomap$varofint, listw = spdep::nb2listw(nb.rk))

    uniquedatatomap$Moran <- round(locm[, 1], 3)

    gmoranspdep <- spdep::moran.test(uniquedatatomap$varofint, listw = spdep::nb2listw(nb.rk))

    datatomap <- unique(merge(temp, uniquedatatomap))

    minlon <- min(datatomap$path_lon) * 1.001 # lon negative
    maxlon <- max(datatomap$path_lon) * 0.985
    minlat <- min(datatomap$path_lat) * 0.992
    maxlat <- max(datatomap$path_lat) * 1.001

    annotatesize <- 6

    moranmap <- ggplot(data = datatomap) +
      geom_path(aes(x = datatomap$path_lon, y = datatomap$path_lat, group = datatomap$ZoneID), 
                color = "black", size = 0.375) +
      geom_map(
        data = world,
        map = world, aes(map_id = world$region), 
        #map = world, aes(x = datatomap$long, y = datatomap$lat, map_id = world$region), 
        fill = "grey", color = "black", size = 0.375
      ) +
      geom_polygon(data = datatomap, aes(
        x = datatomap$path_lon,
        y = datatomap$path_lat, group = datatomap$ZoneID, fill = datatomap$Moran
      ), color = "black", alpha = 1, size = 0.375) +
      scale_fill_gradient2(
        low = "skyblue2", high = "firebrick1",
        mid = "white", name = "Local\nMoran's I"
      ) +
      xlim(minlon, maxlon) +
      ylim(minlat, maxlat) +
      ggtitle("Moran's I statistics") +
      annotate(
        geom = "text",
        x = min(datatomap$path_lon) * 0.9915, y = min(datatomap$path_lat) * 0.997, label = paste0("Global Moran's I = ", round(
          gmoranspdep$estimate[1],
          2
        )), parse = FALSE, size = annotatesize, color = "black", hjust = 0
      ) +
      annotate(geom = "text", x = min(datatomap$path_lon) * 0.9915, y = min(datatomap$path_lat) *
        0.994, label = paste0("p-value = ", round(gmoranspdep$p.value, 2)), parse = FALSE, size = annotatesize, color = "black", hjust = 0) +
      theme(
        text = element_text(size = 20),
        axis.title.y = element_text(vjust = 1.5), legend.position = c(0.875, 0.7), legend.text = element_text(size = 15), legend.title = element_text(size = 15)
      ) +
      xlab("Longitude") +
      ylab("Latitude")

    colnames(uniquedatatomap)[which(colnames(uniquedatatomap) == "Moran")] <- "Morans_I"

    moran_stats_function <- list()
    moran_stats_function$functionID <- "moran_stats"
    moran_stats_function$args <- list(dat, project, varofint, spat, lon.dat, lat.dat, cat, lon.grid, lat.grid)
    log_call(project, moran_stats_function)

    return(list(moranmap = moranmap, morantable = uniquedatatomap[, c("ZoneID", "Morans_I")]))


    save_plot(project, "moran_stats", moranmap)

    save_table(uniquedatatomap, project, "moran_stats")
  }
}
