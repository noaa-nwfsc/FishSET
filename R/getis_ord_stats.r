getis_ord_stats <- function(dat, project, varofint, gridfile, lon.dat = NULL, lat.dat = NULL, cat = NULL, lon.grid = NULL, lat.grid = NULL) {
  #' Calculate and view Getis-Ord statistic
  #'
  #' Wrapper function to calculate global and local Getis-Ord by discrete area
  #'
  #' @param dat Primary data containing information on hauls or trips.
  #'   Table in the FishSET database contains the string 'MainDataTable'.
  #' @param project String, name of project.
  #' @param varofint Numeric variable in \code{dat} to test for spatial high/low clustering.
  #' @param gridfile Spatial data containing information on fishery management or regulatory zones.
  #'  Can be shape file, json, geojson, data frame, or list.
  #' @param lon.dat Longitude variable in \code{dat}.
  #' @param lat.dat Latitude variable in \code{dat}.
  #' @param lon.grid Variable or list from \code{gridfile} containing longitude data. Required for csv files.
  #'   Leave as NULL if \code{gridfile} is a shape or json file.
  #' @param lat.grid Variable or list from \code{gridfile} containing latitude data. Required for csv files.
  #'   Leave as NULL if \code{gridfile} is a shape or json file.
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
  #' @importFrom sf st_geometry
  #' @export
  #' @examples
  #' \dontrun{
  #' getis_ord_stats(pcodMainDataTable, project = 'pcod', varofint = 'OFFICIAL_MT_TONS',
  #'   gridfile = spatdat, lon.dat = 'LonLat_START_LON', lat.dat = 'LonLat_START_LAT', cat = 'NMFS_AREA')
  #' }
  #'

  # requireNamespace("ggplot2")
  world <- map_data("world")

  # Call in datasets
  out <- data_pull(dat)
  dat <- out$dat
  dataset <- out$dataset

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
      dataset <- assignment_column(dataset, gridfile, hull.polygon = TRUE, lon.dat, lat.dat, cat, closest.pt = TRUE, lon.grid, lat.grid, epsg = NULL)

      # Idenfity centroid of zone
      int <- find_centroid(dataset, gridfile, lon.dat, lat.dat, cat, lon.grid, lat.grid, weight.var = NULL)
    }

    # Create dataset
    if (any(class(gridfile) == "sf")) {
      nc_geom <- sf::st_geometry(gridfile)
      temp <- data.frame(ZoneID = rep(gridfile[[cat]][1], path = dim(as.data.frame(nc_geom[[1]][[1]]))[1]), as.data.frame(nc_geom[[1]][[1]]))
      for (i in 2:length(nc_geom)) {
        temp <- rbind(temp, data.frame(ZoneID = rep(gridfile[[cat]][i], path = dim(as.data.frame(nc_geom[[1]][[i]]))[1]), as.data.frame(nc_geom[[1]][[i]])))
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
    locg <- spdep::localG(uniquedatatomap[["varofint"]], listw = spdep::nb2listw(nb.rk))

    uniquedatatomap$GetisOrd <- round(locg, 3)

    globalgetis <- spdep::globalG.test(uniquedatatomap[["varofint"]], listw = spdep::nb2listw(nb.rk, style = "B"))

    datatomap <- unique(merge(temp, uniquedatatomap))

    minlon <- min(datatomap$path_lon) * 1.001 # lon negative
    maxlon <- max(datatomap$path_lon) * 0.985
    minlat <- min(datatomap$path_lat) * 0.992
    maxlat <- max(datatomap$path_lat) * 1.001

    annotatesize <- 6

    getismap <- ggplot(data = datatomap) +
      geom_path(aes(x = path_lon, y = path_lat, group = ZoneID), color = "black", size = 0.375) +
      geom_map(
        data = world,
        map = world, aes(map_id = region), fill = "grey", color = "black", size = 0.375
      ) +
      geom_polygon(data = datatomap, aes(
        x = path_lon, y = path_lat,
        group = ZoneID, fill = GetisOrd
      ), color = "black", alpha = 1, size = 0.375) +
      xlim(minlon, maxlon) +
      ylim(minlat, maxlat) +
      scale_fill_gradient2(
        low = "skyblue2",
        high = "firebrick1", mid = "white", name = "Local\nGetis-Ord"
      ) +
      ggtitle("Getis-Ord statistics") +
      annotate(
        geom = "text", x = min(datatomap$path_lon) *
          0.9915, y = min(datatomap$path_lat) * 0.997, label = paste0("Global Getis-Ord = ", round(globalgetis$estimate[1], 2)), parse = FALSE, size = annotatesize,
        color = "black", hjust = 0
      ) +
      annotate(geom = "text", x = min(datatomap$path_lon) * 0.9915, y = min(datatomap$path_lat) * 0.994, label = paste0(
        "p-value = ",
        round(globalgetis$p.value, 2)
      ), parse = FALSE, size = annotatesize, color = "black", hjust = 0) +
      theme(
        text = element_text(size = 20), axis.title.y = element_text(vjust = 1.5),
        legend.position = c(0.875, 0.7), legend.text = element_text(size = 15), legend.title = element_text(size = 15)
      ) +
      xlab("Longitude") +
      ylab("Latitude")

    getis_ord_stats_function <- list()
    getis_ord_stats_function$functionID <- "getis_ord_stats"
    getis_ord_stats_function$args <- list(dat, project, varofint, gridfile, lon.dat, lat.dat, cat)
    getis_ord_stats_function$kwargs <- list(lon.grid, lat.grid)

    log_call(getis_ord_stats_function)

    return(list(getismap = getismap, getistable = uniquedatatomap[, c("ZoneID", "GetisOrd")]))


    save_plot(project, "getis_ord_stats", getismap)

    save_table(uniquedatatomap, project, "getis_ord_stats")
  }
}
