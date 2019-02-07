#'  Generate centroid of polygon of zone or area

#' @param dataset dataframe or matrix
#' @param gridfile name of gridded dataset
#' @param lon.dat Longitude of points from dataset
#' @param lat.dat Latitude of points from dataset
#' @param lon.grid Longitude of points from gridfile
#' @param lat.grid Latitude of points from gridfile
#' @param cat Categorical variable defining the individual areas or zones
#' @param weight.var Variable for weighted average
#' @param use.grid TRUE/FALSE
#' @keywords centroid, zone, polygon
#' @importFrom sf st_centroid  
#' @importFrom spatialEco wt.centroid
#' @importFrom rgeos gCentroid
#' @return Returns a dataframe with location of centroid
#' @export find_centroid
#' @details Functions returns the center of a zone or area based on set of latitude and longitudes for using in generating distance matrices.
#'  Function can also return a weighted centroid. Code works for dataframes and shape files. Lists can also be used as long as inputs are a list of latitudes, longitudes, and areas or zones.
#'  Includes assignmentColumn function. Assigns a zone from a gridded dataset to the main dataset.



find_centroid <- function(use.grid, dataset, gridfile, lon.grid, lat.grid, lat.dat, lon.dat, cat, weight.var) {
  
  #For json and shape files
  if(any(class(gridfile)=='sf')) {
    if (is.empty(weight.var)) {
      int <- rgeos::gCentroid(as(gridfile, "Spatial"), byid = TRUE)
      int <- cbind(gridfile[[cat]], as.data.frame(int))
      colnames(int)=c("ZoneID", "cent.lon", "cent.lat")
      if (any(abs(int$cent.lon) > 180)) {
        cat("Longitude is not valid (outside -180:180.", 
            file=paste(getwd(),'/Logs/InforMessage',Sys.Date(),'.txt', sep=''), append=TRUE)
        stop("Longitude is not valid (outside -180:180.")
      }
      if (any(abs(int$cent.lat) > 90)) {
        cat("Latitude is not valid (outside -90:90.", 
            file=paste(getwd(),'/Logs/InforMessage',Sys.Date(),'.txt', sep=''), append=TRUE)
        stop("Latitude is not valid (outside -90:90.")
      } 
      if(mean(int)<0){
        if(length(which(int$x>mean(int$x)/4|int$x<mean(int$x)*4))>0){
          warning('At least one centroid may be inaccurate. Check for consistency in signs.')
                 print(int[which(int$x>mean(int$x)/4|int$x<mean(int$x)*4),])
        }
      }
    } else {
  #Weighted variables      
      int <- assignmentColumn(dataset = dataset, gridfile = gridfile, lon.grid = lon.grid, 
                              lat.grid = lat.grid, lon.dat = lon.dat, lat.dat = lat.dat, cat = cat)  
      int$cent.lon <- ave(int[c(lon.dat, weight.var)], int$ZoneID, 
                          FUN = function(x) weighted.mean(x[[lon.dat]], x[[weight.var]]))[[1]]
      int$cent.lat <- ave(int[c(lat.dat, weight.var)], int$ZoneID, 
                          FUN = function(x) weighted.mean(x[[lat.dat]], x[[weight.var]]))[[1]]
    }
 
  } 
  #begin dataframe
  else {
    # Centroid based on grid file or dataset
    if (use.grid == T) {
      int <- gridfile
      lon <- lon.grid
      lat <- lat.grid
    } else {
      int <- dataset
      lon <- lon.dat
      lat <- lat.dat
    }
    # Lat and long must be within logical bounds
    if (is.data.frame(int) == T) {
      if (any(abs(int[[lon]]) > 180)) {
        cat("Longitude is not valid (outside -180:180.", 
            file=paste(getwd(),'/Logs/InforMessage',Sys.Date(),'.txt', sep=''), append=TRUE)
        stop("Longitude is not valid (outside -180:180.")
      }
      if (any(abs(int[[lat]]) > 90)) {
        cat("Latitude is not valid (outside -90:90.", 
            file=paste(getwd(),'/Logs/InforMessage',Sys.Date(),'.txt', sep=''), append=TRUE)
        stop("Latitude is not valid (outside -90:90.")
      }
    }
    # simple centroid
    if (is.empty(weight.var)) {
      if (is.data.frame(int) == T) {
        int$cent.lon <- ave(int[[lon]], int[[cat]])
        int$cent.lat <- ave(int[[lat]], int[[cat]])
      } else {
        int$cent <- sf::st_centroid(int)
      }
    } else {
      # weighted centroid
      if (is.data.frame(int) == T) {
        if (use.grid == T) {
        
          int <- assignmentColumn(dataset = dataset, gridfile = gridfile, lon.grid = lon.grid, 
                                lat.grid = lat.grid, lon.dat = lon.dat, lat.dat = lat.dat, cat = cat)  
          int$cent.lon <- ave(int[c(lon.dat, weight.var)], int$ZoneID, 
                            FUN = function(x) weighted.mean(x[[lon.dat]], x[[weight.var]]))[[1]]
          int$cent.lat <- ave(int[c(lat.dat, weight.var)], int$ZoneID, 
                            FUN = function(x) weighted.mean(x[[lat.dat]], x[[weight.var]]))[[1]]
        } else {
          int$cent.lon <- ave(int[c(lon, weight.var)], int[[cat]], 
                            FUN = function(x) weighted.mean(x[[lon]], x[[weight.var]]))[[1]]
          int$cent.lat <- ave(int[c(lat, weight.var)], int[[cat]], 
                            FUN = function(x) weighted.mean(x[[lat]], x[[weight.var]]))[[1]]
        }
      } else {
        int$cent <- spatialEco::wt.centroid(int, weight.var, sp = TRUE)
      }
    }
  }
  
    colnames(int)[colnames(int) == cat] <- "ZoneID"
    int <- int[, c("ZoneID", "cent.lon", "cent.lat")]
    int <- unique(int)
    return(int)
}

