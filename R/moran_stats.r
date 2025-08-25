#' Calculate Moran's I statistic
#'
#' Wrapper function to calculate global and local Moran's I by discrete area.
#'
#' @param dat Primary data containing information on hauls or trips. Table in FishSET database 
#'   contains the string 'MaindataTable'.
#' @param var Numeric variable from \code{dat} to test for spatial autocorrelation.
#' @param dat_zone Variable in \code{dat} that identifies the individual zones or
#'   areas. Define if exists in \code{dat} and is not named `ZoneID`. Defaults to NULL. 
#' @param spat Spatial data containing information on fishery management or regulatory zones.
#' Shape, json, geojson, and csv formats are supported.
#' @param spat_zone Variable or list in \code{spat} that identifies the individual areas or zones.
#' If \code{spat} is class sf, \code{cat} should be name of list containing information on zones.
#' @details Measure degree of spatial autocorrelation. Function utilizes the 
#' \code{\link[spdep]{localmoran}} and \code{\link[spdep]{knearneigh}} functions 
#' from the spdep package. The spatial input is a row-standardized spatial
#' weights matrix for computed nearest neighbor matrix, which is the null setting 
#' for the \code{\link[spdep]{nb2listw}} function. The function requires a map 
#' file with lat/lon defining boundaries of area/zones and \code{varofint} for 
#' to test for spatial autocorrelation. If zonal centroid is not included in the 
#' map file, then the \code{\link{find_centroid}} function is called to calculate 
#' the centroid of each zone. If the variable of interest is not associated with an
#' area/zone then \code{\link{assignment_column}} is called to assign each observation 
#' to a zone. Arguments to identify centroid and assign variable of interest to 
#' area/zone are optional and default to NULL.
#' @return Returns a plot and map of Moran’s I. Output is saved to the Output folder.
#' @import ggplot2
#' @importFrom spdep knn2nb knearneigh nb2listw localmoran moran.test
#' @importFrom sf st_coordinates st_centroid st_geometry st_make_valid
#' @export
#' @examples
#' \dontrun{
#' moran_stats(pcodMainDataTable, var='OFFICIAL_MT_TONS', dat_zone='zoneID',
#'   spat=spatdat, spat_zone='NMFS_AREA')
#' }
#'
moran_stats <- function(dat, var, dat_zone, spat, spat_zone) {
  
  # Data prep -------------------------------------------------------------------------------------
  
  # Ensure necessary packages are loaded
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required.")
  }
  if (!requireNamespace("spdep", quietly = TRUE)) {
    stop("Package 'spdep' is required for spatial weights calculation.")
  }
  
  # Rename zone IDs to make sure these match between dat and spat
  spat_tmp <- spat
  names(spat_tmp)[names(spat_tmp) == spat_zone] <- dat_zone
  
  # Only include zones that are present in dat
  spat_tmp <- spat_tmp[which(spat_tmp[[dat_zone]] %in% dat[[dat_zone]]),]
  spat_tmp <- spat_tmp %>%
    select(all_of(dat_zone), geometry)
  
  merged_df <- dat %>%
    dplyr::left_join(., spat_tmp, by = dat_zone, relationship = "many-to-many") %>%
    dplyr::distinct()
    
  dataset <- suppressWarnings(
          assignment_column(dat=dataset, project=project, spat=spatdat, hull.polygon = TRUE,
                            lon.dat=lon.dat, lat.dat=lat.dat, cat=cat, closest.pt = TRUE,
                            lon.spat=lon.spat, lat.spat=lat.spat, epsg = NULL, log.fun = FALSE))
   
  
  # Only save lcoe areas included in boem aliquots
  idx_lcoe_aliquot <- which(lcoe_aliquot_sf$OBJECTID %in% boem_aliquots$OBJECTID)
  lcoe_aliquot_sf <- lcoe_aliquot_sf[idx_lcoe_aliquot,]
}


# world <- ggplot2::map_data("world")
# 
# # Call in datasets
# out <- data_pull(dat, project)
# dataset <- out$dataset
# dat <- parse_data_name(dat, "main", project)
# 
# spat_out <- data_pull(spat, project)
# spatdat <- spat_out$dataset
# spat <- parse_data_name(spat, 'spat', project)
# 
# spatdat <- check_spatdat(spatdat = spatdat, lon = lon.spat, lat = lat.spat, id = cat)
# 
# x <- 0
# Moran <- NA
# 
# # Assign data to zone
# #Assignment column
# if('ZoneID' %in% names(dataset)) {
#   
#   zoneid <- 'ZoneID'
#   
# } else if(!is.null(zoneid) && zoneid %in% names(dataset)){
#   
#   colnames(dataset)[colnames(dataset)==zoneid] <- 'ZoneID'
#   
# } else {
#   
#   if(is.null(spat) || is.null(lon.dat)){
#     
#     stop('Observations must be assigned to zones. Function not run.')
#     
#   } else {
#     
#     if (any(abs(dataset[[lon.dat]]) > 180)) {
#       
#       stop("Longitude is not valid (outside -180:180). Function not run")
#     }
#     
#     if (any(abs(dataset[[lat.dat]]) > 90)) {
#       
#       stop("Latitude is not valid (outside -90:90. Function not run")
#       
#     }
#     
#     dataset <- suppressWarnings(
#       assignment_column(dat=dataset, project=project, spat=spatdat, hull.polygon = TRUE, 
#                         lon.dat=lon.dat, lat.dat=lat.dat, cat=cat, closest.pt = TRUE, 
#                         lon.spat=lon.spat, lat.spat=lat.spat, epsg = NULL, log.fun = FALSE)
#     )
#   }
# }
# 
# ## Centroid table
# if (table_exists(paste0(spat, 'Centroid'), project)) {
#   
#   int <- table_view(paste0(spat, 'Centroid'), project)
#   
# } else if (table_exists('spatCentroid', project)) {
#   
#   int <- table_view('spatCentroid', project)
#   
# } else {
#   
#   int <- suppressWarnings(find_centroid(spat=spatdat, project = project, spatID = cat, 
#                                         lon.spat=lon.spat, lat.spat=lat.spat, log.fun = FALSE))
# }
# 
# int <- merge(int, dataset[, c(varofint, "ZoneID")], by = "ZoneID")
# names(int)[2] <- "centroid_lon"
# names(int)[3] <- "centroid_lat"
# names(int)[4] <- "varofint"
# 
# # 4. Identify variable of interest 5.
# int[["varofint"]] <- with(int, ave(int[["varofint"]], ZoneID, FUN = function(x) mean(x, na.rm = TRUE)))
# uniquedatatomap <- int[!duplicated(int$ZoneID), c("ZoneID", "centroid_lon", "centroid_lat", "varofint")]
# 
# nb.rk <- spdep::knn2nb(spdep::knearneigh(as.matrix(uniquedatatomap[, c("centroid_lon", "centroid_lat")]), longlat = TRUE))
# locm <- spdep::localmoran(uniquedatatomap$varofint, listw = spdep::nb2listw(nb.rk))
# 
# uniquedatatomap$Moran <- round(locm[, 1], 3)
# 
# if(min(uniquedatatomap$centroid_lon) <0  & max(uniquedatatomap$centroid_lon) > 0) {
#   spatdat$geometry = (sf::st_geometry(spatdat) + c(360,90)) %% c(360) - c(0,90)
# }
# 
# gmoranspdep <- spdep::moran.test(uniquedatatomap$varofint, listw = spdep::nb2listw(nb.rk))
# 
# spatdat <-  merge(spatdat, uniquedatatomap[,c('ZoneID','Moran')], by.x=cat, by.y='ZoneID')#spatdat$GetisOrd <- g$GetisOrd
# spatdat <- cbind(spatdat, sf::st_coordinates(sf::st_centroid(sf::st_make_valid(spatdat))))
# spatdat <- subset(spatdat, !is.na(spatdat$Moran))
# 
# minlon <- min(spatdat$X) * 1.001 # lon negative
# maxlon <- max(spatdat$X) * 0.985
# minlat <- min(spatdat$Y) * 0.992
# maxlat <- max(spatdat$Y) * 1.001
# 
# annotatesize <- 6
# 
# moranmap <- ggplot2::ggplot(data = spatdat) +
#   ggplot2::geom_sf(data = spatdat, mapping =  ggplot2::aes(fill = Moran)) +
#   xlim(minlon, maxlon) +
#   ylim(minlat, maxlat) +
#   ggplot2::scale_fill_gradient2(
#     low = "skyblue2", high = "firebrick1",
#     mid = "white", name = "Local\nMoran's I") +
#   ggplot2::geom_map(
#     data = world,
#     map = world,  ggplot2::aes(map_id = .data$region), 
#     fill = "grey", color = "black", size = 0.375) +
#   ggplot2::ggtitle("Moran's I statistics") +
#   ggplot2::annotate(
#     geom = "text",
#     x = min(spatdat$X) * 0.9915, y = min(spatdat$Y) * 0.997, label = paste0("Global Moran's I = ", 
#                                                                             round(gmoranspdep$estimate[1],2)), parse = FALSE, size = annotatesize, color = "black", hjust = 0) +
#   ggplot2::annotate(geom = "text", x = min(spatdat$X) * 0.9915, y = min(spatdat$Y) * 0.994, 
#                     label = paste0("p-value = ", round(gmoranspdep$p.value, 2)), parse = FALSE, 
#                     size = annotatesize, color = "black", hjust = 0) +
#   ggplot2::theme(
#     text = ggplot2::element_text(size = 20),
#     axis.title.y = ggplot2::element_text(vjust = 1.5), legend.position = c(0.875, 0.7), 
#     legend.text = ggplot2::element_text(size = 15),
#     legend.title = ggplot2::element_text(size = 15)) +
#   ggplot2::xlab("Longitude") +
#   ggplot2::ylab("Latitude")
# 
# colnames(uniquedatatomap)[which(colnames(uniquedatatomap) == "Moran")] <- "Morans_I"
# 
# moran_stats_function <- list()
# moran_stats_function$functionID <- "moran_stats"
# moran_stats_function$args <- list(dat, project, varofint, zoneid, spat, lon.dat, 
#                                   lat.dat, cat, lon.spat, lat.spat)
# log_call(project, moran_stats_function)
# 
# save_plot(project, "moran_stats", moranmap)
# save_table(uniquedatatomap, project, "moran_stats")
# 
# return(list(moranmap = moranmap, morantable = uniquedatatomap[, c("ZoneID", "Morans_I")]))
# }
