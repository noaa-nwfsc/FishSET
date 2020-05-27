moran_stats <- function(dat, project, varofint, gridfile, lon.dat = NULL, lat.dat = NULL, cat = NULL, lon.grid = NULL, lat.grid = NULL) {
    #' Wrapper function to calculate Moran's I
    #'
    #' Wrapper function to calculate global and local Moran's I by discrete area
    #'
    #' @param dat Main data frame over which to apply function. Table in FishSET database should contain the string `MainDataTable`.
    #' @param project Name of project
    #' @param varofint Numeric variable of interest to test for spatial autocorrelation.
    #' @param gridfile Spatial data set. Can be shape file, data frame, or list.
    #' @param lon.dat Longitude of points from dataset.
    #' @param lat.dat Latitude of points from dataset.
    #' @param lon.grid Longitude of points from gridfile.
    #' @param lat.grid Latitude of points from gridfile.
    #' @param cat Variable defining the individual areas or zones.
    #' @details Measure degree of spatial autocorrelation.
    #' Function utilizes the localmoran and knearneigh functions from the spdep package. All parameters are set to NULL except the varofint and location variables.
    #' The spatial input is a row-standardized spatial weights matrix for computed nearest neighbor matrix, which is the null setting for the nb2listw function.
    #' The function requires A data frame with area as a factor, the lon/lat centroid for each area ('centroid_lon',
    #'     'centroidlat'), the path_lon/path_lat outlining each area
    #'     ('path_lon', 'path_lat'), and the variable of interest ('varofint') or 
    #'  a map file with lat/lon defining boundaries of area/zones and variable of interest for weighting.
    #' Also required is the lat/lon defining the center of a zone/area. If the centroid is not included in the map file, 
    #' then the find_centorid function can be called to calculate the centroid of each zone. If the varible of interest is not
    #' associated with a area/zone than the assignement_column function can be used to assign each observation to a zone.  
    #' Parameters to identify centroid and assign variable of interest to area/zone are optional and default to NULL.
    #' @return moranmap: ggplot2 object; morantable: table of statistics
    #' @import ggplot2
    #' @importFrom maps map
    #' @importFrom spdep knn2nb knearneigh nb2listw localmoran moran.test
    #' @export
    #' @examples
    #' \dontrun{
    #' names(datatomap)[1] <- 'path_lon'
    #' names(datatomap)[2] <- 'path_lat'
    #' names(datatomap)[3] <- 'centroid_lon'
    #' names(datatomap)[4] <- 'centroid_lat'
    #' names(datatomap)[5] <- 'ADFGstat6'
    #' names(datatomap)[5] <- 'varofint'
    #' moran_stats(datatomap)
    #' }
    #'
    
    requireNamespace("ggplot2")
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
    if (any(abs(dataset[[lat.dat]]) > 90)) {
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
        locm <- spdep::localmoran(uniquedatatomap$varofint, listw = spdep::nb2listw(nb.rk))
        
        uniquedatatomap$Moran <- round(locm[, 1], 3)
        
        gmoranspdep <- spdep::moran.test(uniquedatatomap$varofint, listw = spdep::nb2listw(nb.rk))
        
        datatomap <- unique(merge(temp, uniquedatatomap))
        
        minlon = min(datatomap$path_lon) * 1.001  #lon negative
        maxlon = max(datatomap$path_lon) * 0.985
        minlat = min(datatomap$path_lat) * 0.992
        maxlat = max(datatomap$path_lat) * 1.001
        
        annotatesize <- 6
        
        moranmap <- ggplot(data = datatomap) + geom_path(aes(x = path_lon, y = path_lat, group = ZoneID), color = "black", size = 0.375) + geom_map(data = world, 
            map = world, aes(x = long, y = lat, map_id = region), fill = "grey", color = "black", size = 0.375) + geom_polygon(data = datatomap, aes(x = path_lon, 
            y = path_lat, group = ZoneID, fill = Moran), color = "black", alpha = 1, size = 0.375) + scale_fill_gradient2(low = "skyblue2", high = "firebrick1", 
            mid = "white", name = "Local\nMoran's I") + xlim(minlon, maxlon) + ylim(minlat, maxlat) + ggtitle("Moran's I statistics") + annotate(geom = "text", 
            x = min(datatomap$path_lon) * 0.9915, y = min(datatomap$path_lat) * 0.997, label = paste0("Global Moran's I = ", round(gmoranspdep$estimate[1], 
                2)), parse = FALSE, size = annotatesize, color = "black", hjust = 0) + annotate(geom = "text", x = min(datatomap$path_lon) * 0.9915, y = min(datatomap$path_lat) * 
            0.994, label = paste0("p-value = ", round(gmoranspdep$p.value, 2)), parse = FALSE, size = annotatesize, color = "black", hjust = 0) + theme(text = element_text(size = 20), 
            axis.title.y = element_text(vjust = 1.5), legend.position = c(0.875, 0.7), legend.text = element_text(size = 15), legend.title = element_text(size = 15)) + 
            xlab("Longitude") + ylab("Latitude")
        
        colnames(uniquedatatomap)[which(colnames(uniquedatatomap) == "Moran")] <- "Morans_I"
        
        moran_stats_function <- list()
        moran_stats_function$functionID <- 'moran_stats'
        moran_stats_function$args <- list(dat, project, varofint, gridfile, lon.dat, lat.dat, cat)
        moran_stats_function$kwargs <- list(lon.grid, lat.grid)
        log_call(moran_stats_function)
        
        return(list(moranmap = moranmap, morantable = uniquedatatomap[, c("ZoneID", "Morans_I")]))
        
        
        save_plot(project, "moran_stats", moranmap)
        
        save_table(uniquedatatomap, project, "moran_stats")
    }
}
