#'  Generate centroid of polygon of zone or area

#' @param dataset dataframe or matrix
#' @param gridfile name of gridded dataset
#' @param lon.dat Longitude of points from dataset
#' @param lat.dat Latitude of points from dataset
#' @param lon.grid Longitude of points from gridfile
#' @param lat.grid Latitude of points from gridfile
#' @param cat Categorical variable defining the individual areas or zones
#' @param weight.var  Variable for weighted average
#' @keywords centroid, zone, polygon
#' @return Returns a dataframe with location of centroid
#' @details Functions returns the center of a zone or area based on set of latitude and longitudes for using in generating distance matrices.
#'  Function can also return a weighted centroid. Code works for dataframes and shape files. Lists can also be used as long as inputs are a list of latitudes, longitudes, and areas or zones.
#'  Includes assignmentColumn function. Assigns a zone from a gridded dataset to the main dataset.
#'  Depends on geosphere, sf, rgeos,  spatialEco,  sp  
 
#' 
#' 

require(geosphere)
require(sf) # for shape file
require(rgeos)  # create polygones from points
require(sp)

#map <- read.csv('C:/Users/Melanie/Documents/FishSET_RPackage/Data/adfgstat6_map.csv', header=F)
#require(rjson)
#map  <- fromJSON('C:/Users/Melanie/Documents/FishSET_RPackage/Data/NMFS_RA.json', flatten=TRUE)
#map <- map$features

assignmentColumn <- function(dataset, gridfile, lon.grid, lat.grid, lon.dat, lat.dat, cat){ #
     #Create spatial polygon dataframe from grid data
     # make a list
     map_list <- split(gridfile[,c(lon.grid,lat.grid,cat)], gridfile[[cat]])
     # only want lon-lats in the list, not the names
     map_list <- lapply(map_list, function(x) { x[cat] <- NULL; x })

     ps <- lapply(map_list, Polygon)

     # add id variable
     p1 <- lapply(seq_along(ps), function(i) Polygons(list(ps[[i]]), ID = names(map_list)[i]))
     my_spatial_polys <- SpatialPolygons(p1, proj4string = CRS("+proj=longlat +datum=WGS84"))                                                 
     
     #Change to spatial polygon dataframe
     srdf <- SpatialPolygonsDataFrame(my_spatial_polys, data.frame(row.names=c(names(map_list)), ID=names(map_list)))
     
     #Assign zone to dataset based on lat and long
     dat_sub <- dataset
     # Assignment modified according
     coordinates(dat_sub) <- c(lon.dat, lat.dat)
     
      # Set the projection of the SpatialPointsDataFrame using the projection of the shapefile
     proj4string(dat_sub) <- CRS("+proj=longlat +datum=WGS84")#proj4string(sodo)
     #identify intersections of points in dataset with polygons in grid file
     pts <- point.in.poly(dat_sub, srdf, duplicate=F)
     pts <- subset(pts, select = -c(X, pid1))
     pts <- as.data.frame(pts)
     colnames(pts)[colnames(pts)=="p"] <- "ZoneID"
     return(pts)
}



findCentroid <- function(use.grid, dataset, gridfile, lon.grid, lat.grid, lat.dat, lon.dat, cat, weight.var) {
     #Centroid based on grid file or dataset
     if(use.grid==T){
          int <- gridfile
          lon <- lon.grid
          lat <- lat.grid
     } else {
          int <- dataset
          lon <- lon.dat
          lat <- lat.dat
     }
     #Test. Lat and long must be within logical bounds
          if(is.data.frame(int)==T) {
               if(any(abs(int[[lon]]) > 180)) {
                    stop('Longitude is not valid (outside -180:180.')
               } 
               if(any(abs(int[[lat]]) > 90)) {
                    stop('Latitude is not valid (outside -90:90.')
               }
          }     
     #simple centroid
      if(is.empty(weight.var)){
           if(is.data.frame(int)==T){
          int$cent.lon <- ave(int[[lon]], int[[cat]])
          int$cent.lat <- ave(int[[lat]], int[[cat]])
         
          
          } else {
               int$cent <- st_centroid(int)
          }
     } else {
     #weighted centroid
          if(is.data.frame(int)==T) {
               if(use.grid==T){
          
               int <- assignmentColumn(dataset=dataset, gridfile=gridfile, lon.grid=lon.grid, lat.grid=lat.grid, lon.dat=lon.dat, lat.dat=lat.dat, cat=cat)#as.data.frame(assignmentColumn())
               int$cent.lon <- ave(int[c(lon.dat, weight.var)], int$ZoneID, FUN= function(x) weighted.mean(x[[lon.dat]], x[[weight.var]]))[[1]]
               int$cent.lat <- ave(int[c(lat.dat, weight.var)], int$ZoneID, FUN= function(x) weighted.mean(x[[lat.dat]], x[[weight.var]]))[[1]]
               } else {
                    int$cent.lon <- ave(int[c(lon, weight.var)], int[[cat]], FUN= function(x) weighted.mean(x[[lon]], x[[weight.var]]))[[1]]
                    int$cent.lat <- ave(int[c(lat, weight.var)], int[[cat]], FUN= function(x) weighted.mean(x[[lat]], x[[weight.var]]))[[1]]
               }
          } else {
                int$cent <- wt.centroid(int, weight.var, sp = TRUE)
          }  
     }  
     colnames(int)[colnames(int)==cat] <- "ZoneID"
     int <- int[,c('ZoneID','cent.lon','cent.lat')]
     int <- unique(int)
     return(int)
}


