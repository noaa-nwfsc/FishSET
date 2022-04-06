moran_stats <- function(dat, project, varofint, zoneid, spat, cat, lon.dat = NULL, lat.dat = NULL, lon.spat = NULL, lat.spat = NULL) {
  #' Calculate and view Moran's I statistic
  #'
  #' Wrapper function to calculate global and local Moran's I by discrete area.
  #'
  #' @param dat Primary data containing information on hauls or trips.
  #' Table in FishSET database contains the string 'MaindataTable'.
  #' @param project String, name of project.
  #' @param varofint Numeric variable from \code{dat} to test for spatial autocorrelation.
  #' @param zoneid Variable in \code{dat} that identifies the individual zones or areas. Define if exists in \code{dat} and is not named `ZoneID`.
  #'      Defaults to NULL. 
  #' @param spat Spatial data containing information on fishery management or regulatory zones.
  #' Shape, json, geojson, and csv formats are supported.
  #' @param cat Variable or list in \code{spat} that identifies the individual areas or zones.
  #' If \code{spat} is class sf, \code{cat} should be name of list containing information on zones.
  #' @param lon.dat Longitude variable from \code{dat}.
  #' @param lat.dat Latitude variable from \code{dat}.
  #' @param lon.spat Variable or list from \code{spat} containing longitude data. Required for csv files.
  #' Leave as NULL if \code{spat} is a shape or json file.
  #' @param lat.spat Variable or list from \code{spat} containing latitude data. Required for csv files.
  #' Leave as NULL if \code{spat} is a shape or json file.
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
  #' @importFrom spdep knn2nb knearneigh nb2listw localmoran moran.test
  #' @importFrom sf st_coordinates st_centroid st_geometry st_make_valid
  #' @importFrom shiny isRunning
  #' @export
  #' @examples
  #' \dontrun{
  #' moran_stats(pcodMainDataTable, project='pcod', varofint='OFFICIAL_MT_TONS',
  #' spat=spatdat, lon.dat='LonLat_START_LON', lat.dat ='LonLat_START_LAT', cat='NMFS_AREA')
  #' }
  #'

  # requireNamespace("ggplot2")
  world <- ggplot2::map_data("world")

  # Call in datasets
  out <- data_pull(dat, project)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main", project)
  
  spat_out <- data_pull(spat, project)
  spatdat <- spat_out$dataset
  spat <- parse_data_name(spat, 'spat', project)
  
 
  if(is.null(cat)){
    stop('`cat` argument not defined')
  }
  
  x <- 0
  Moran <- NA
  
  

  if (x == 0) {
    # Assign data to zone
    #Assignment column
    if('ZoneID' %in% names(dataset)){
      zoneid <- 'ZoneID'
    } else if(!is.null(zoneid) && zoneid %in% names(dataset)){
      colnames(dataset)[colnames(dataset)==zoneid] <- 'ZoneID'
    } else {
      if(is.null(spat) || is.null(lon.dat)){
        stop('Observations must be assigned to zones. Function not run.')
      } else {
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
        dataset <- suppressWarnings(assignment_column(dat=dataset, project=project, spat=spatdat, hull.polygon = TRUE, 
                                                      lon.dat=lon.dat, lat.dat=lat.dat, cat=cat, closest.pt = TRUE, 
                                                      lon.spat=lon.spat, lat.spat=lat.spat, epsg = NULL, log.fun = FALSE))
      }}
      
      ## Centoid table
      if(table_exists(paste0(spat, 'Centroid'), project) ==TRUE) {
        int <- table_view(paste0(spat, 'Centroid'), project)
      } else if(table_exists('spatCentroid', project)==TRUE){
        int <- table_view('spatCentroid', project)
      }
         else {
      int <- suppressWarnings(find_centroid(spat=spatdat, project = project, cat = cat, 
                                            lon.spat=lon.spat, lat.spat=lat.spat))
    }
    
    # Create dataset
    if ("sf" %in% class(spatdat) == FALSE) {
      spatdat <- sf::st_as_sf(spatdat)
    }
    
      int <- merge(int, dataset[, c(varofint, "ZoneID")], by = "ZoneID")
      names(int)[2] <- "centroid_lon"
      names(int)[3] <- "centroid_lat"
      names(int)[4] <- "varofint"
    
      
    # 4. Identify variable of interest 5.
    int[["varofint"]] <- with(int, ave(int[["varofint"]], ZoneID, FUN = function(x) mean(x, na.rm = TRUE)))
    uniquedatatomap <- int[!duplicated(int$ZoneID), c("ZoneID", "centroid_lon", "centroid_lat", "varofint")]

    nb.rk <- spdep::knn2nb(spdep::knearneigh(as.matrix(uniquedatatomap[, c("centroid_lon", "centroid_lat")]), longlat = TRUE))
    locm <- spdep::localmoran(uniquedatatomap$varofint, listw = spdep::nb2listw(nb.rk))

    uniquedatatomap$Moran <- round(locm[, 1], 3)

    if(min(uniquedatatomap$centroid_lon) <0  & max(uniquedatatomap$centroid_lon) > 0) {
      spatdat$geometry = (sf::st_geometry(spatdat) + c(360,90)) %% c(360) - c(0,90)
    }
    
    gmoranspdep <- spdep::moran.test(uniquedatatomap$varofint, listw = spdep::nb2listw(nb.rk))

    #g <- as.data.frame(spatdat[[cat]])
    #colnames(g) = 'ZoneID'
    #g <- merge(uniquedatatomap, g, all.y = TRUE)
    #spatdat$Moran <- g$Moran
    #spatdat <- cbind(spatdat, sf::st_coordinates(sf::st_centroid(spatdat)))
    
    spatdat <-  merge(spatdat, uniquedatatomap[,c('ZoneID','Moran')], by.x=cat, by.y='ZoneID')#spatdat$GetisOrd <- g$GetisOrd
    spatdat <- cbind(spatdat, sf::st_coordinates(sf::st_centroid(sf::st_make_valid(spatdat))))
    spatdat <- subset(spatdat, !is.na(spatdat$Moran))
    
    minlon <- min(spatdat$X) * 1.001 # lon negative
    maxlon <- max(spatdat$X) * 0.985
    minlat <- min(spatdat$Y) * 0.992
    maxlat <- max(spatdat$Y) * 1.001
    
    annotatesize <- 6

    moranmap <- ggplot2::ggplot(data = spatdat) +
      ggplot2::geom_sf(data = spatdat, mapping =  ggplot2::aes(fill = Moran)) +
      xlim(minlon, maxlon) +
      ylim(minlat, maxlat) +
      ggplot2::scale_fill_gradient2(
        low = "skyblue2", high = "firebrick1",
        mid = "white", name = "Local\nMoran's I") +
      ggplot2::geom_map(
        data = world,
        map = world,  ggplot2::aes(map_id = .data$region), 
        fill = "grey", color = "black", size = 0.375) +
      ggplot2::ggtitle("Moran's I statistics") +
      ggplot2::annotate(
        geom = "text",
        x = min(spatdat$X) * 0.9915, y = min(spatdat$Y) * 0.997, label = paste0("Global Moran's I = ", 
          round(gmoranspdep$estimate[1],2)), parse = FALSE, size = annotatesize, color = "black", hjust = 0) +
      ggplot2::annotate(geom = "text", x = min(spatdat$X) * 0.9915, y = min(spatdat$Y) * 0.994, 
       label = paste0("p-value = ", round(gmoranspdep$p.value, 2)), parse = FALSE, size = annotatesize, color = "black", hjust = 0) +
      ggplot2::theme(
        text = element_text(size = 20),
        axis.title.y = element_text(vjust = 1.5), legend.position = c(0.875, 0.7), legend.text = element_text(size = 15),
        legend.title = element_text(size = 15)) +
      xlab("Longitude") +
      ylab("Latitude")

    colnames(uniquedatatomap)[which(colnames(uniquedatatomap) == "Moran")] <- "Morans_I"

    moran_stats_function <- list()
    moran_stats_function$functionID <- "moran_stats"
    moran_stats_function$args <- list(dat, project, varofint, zoneid, spat, lon.dat, lat.dat, cat, lon.spat, lat.spat)
    log_call(project, moran_stats_function)

    save_plot(project, "moran_stats", moranmap)

    save_table(uniquedatatomap, project, "moran_stats")
    
    return(list(moranmap = moranmap, morantable = uniquedatatomap[, c("ZoneID", "Morans_I")]))


    
  }
}
