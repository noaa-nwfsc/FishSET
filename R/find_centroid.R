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
#' @return Returns a dataframe with location of centroid
#' @details Functions returns the center of a zone or area based on set of latitude and longitudes for using in generating distance matrices.
#'  Function can also return a weighted centroid. Code works for dataframes and shape files. Lists can also be used as long as inputs are a list of latitudes, longitudes, and areas or zones.
#'  Includes assignmentColumn function. Assigns a zone from a gridded dataset to the main dataset.
#'  Depends on geosphere, sf, rgeos,  spatialEco,  sp  


#require(geosphere)
#require(sf)  # for shape file
#require(rgeos)  # create polygones from points
#require(sp)
#c
# require(rjson) map2 <- fromJSON('C:/Users/Melanie/Documents/FishSET_RPackage/Data/NMFS_RA.json', flatten=TRUE) map <- map$features


findCentroid <- function(use.grid, dataset, gridfile, lon.grid, lat.grid, lat.dat, lon.dat, cat, weight.var) {
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
  # Test. Lat and long must be within logical bounds
  if (is.data.frame(int) == T) {
    if (any(abs(int[[lon]]) > 180)) {
      stop("Longitude is not valid (outside -180:180.")
    }
    if (any(abs(int[[lat]]) > 90)) {
      stop("Latitude is not valid (outside -90:90.")
    }
  }
  # simple centroid
  if (is.empty(weight.var)) {
    if (is.data.frame(int) == T) {
      int$cent.lon <- ave(int[[lon]], int[[cat]])
      int$cent.lat <- ave(int[[lat]], int[[cat]])
      
      
    } else {
      int$cent <- st_centroid(int)
    }
  } else {
    # weighted centroid
    if (is.data.frame(int) == T) {
      if (use.grid == T) {
        
        int <- assignmentColumn(dataset = dataset, gridfile = gridfile, lon.grid = lon.grid, lat.grid = lat.grid, lon.dat = lon.dat, lat.dat = lat.dat, 
                                cat = cat)  #as.data.frame(assignmentColumn())
        int$cent.lon <- ave(int[c(lon.dat, weight.var)], int$ZoneID, FUN = function(x) weighted.mean(x[[lon.dat]], x[[weight.var]]))[[1]]
        int$cent.lat <- ave(int[c(lat.dat, weight.var)], int$ZoneID, FUN = function(x) weighted.mean(x[[lat.dat]], x[[weight.var]]))[[1]]
      } else {
        int$cent.lon <- ave(int[c(lon, weight.var)], int[[cat]], FUN = function(x) weighted.mean(x[[lon]], x[[weight.var]]))[[1]]
        int$cent.lat <- ave(int[c(lat, weight.var)], int[[cat]], FUN = function(x) weighted.mean(x[[lat]], x[[weight.var]]))[[1]]
      }
    } else {
      int$cent <- wt.centroid(int, weight.var, sp = TRUE)
    }
  }
  colnames(int)[colnames(int) == cat] <- "ZoneID"
  int <- int[, c("ZoneID", "cent.lon", "cent.lat")]
  int <- unique(int)
  return(int)
}


