#'  Generate centroid of polygon of zone or area

#' @param dat Main data frame over which to apply function. Table in fishet_db database should contain the string `MainDataTable`.
#' @param gridfile Spatial data set. Can be shape file, data frame, or list.
#' @param lon.dat Longitude of points from dataset.
#' @param lat.dat Latitude of points from dataset.
#' @param lon.grid Longitude of points from gridfile.
#' @param lat.grid Latitude of points from gridfile.
#' @param cat Variable defining the individual areas or zones.
#' @param weight.var Variable for weighted average.
#' @keywords centroid, zone, polygon
#' @importFrom sf st_centroid  
#' @importFrom spatialEco wt.centroid
#' @importFrom rgeos gCentroid
#' @importFrom stats ave weighted.mean
#' @importFrom methods as
#' @return Data frame where each row is a unique zone and columns are the latitude and longitude defining the centroid of each zone.
#' @export find_centroid
#' @details Functions returns the center of a zone or area based on set of latitude and longitudes. Zone centroids are used in calculating distance matrices.
#'  Function can also return a weighted centroid. Code works for data frames and shape files. Lists can also be used as long as inputs are a list of 
#'  latitudes, longitudes, and areas or zones. Calls \code{\link{assignment_column}} function. 



find_centroid <- function(dat, gridfile, lon.grid, lat.grid, lon.dat, lat.dat, cat, weight.var) {
  #Call in datasets
  fishset_db <- DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite")
  if(is.character(dat)==TRUE){
    if(is.null(dat)==TRUE | table_exists(dat)==FALSE){
      print(DBI::dbListTables(fishset_db))
      stop(paste(dat, 'not defined or does not exist. Consider using one of the tables listed above that exist in the database.'))
    } else {
      dataset <- table_view(dat)
    }
  } else {
    dataset <- dat 
  }
  DBI::dbDisconnect(fishset_db)
  
  
  
  tmp <- tempfile()
  cat("", file=tmp, append=TRUE)
  x <- 0
  #For json and shape files
  if(any(class(gridfile)=='sf')) {
    if (FishSET:::is_empty(weight.var)) {
      int <- rgeos::gCentroid(methods::as(gridfile, "Spatial"), byid = TRUE)
      int <- cbind(gridfile[[cat]], as.data.frame(int))
      colnames(int)=c("ZoneID", "cent.lon", "cent.lat")
      if (any(abs(int$cent.lon) > 180)) {
        cat("Longitude is not valid (outside -180:180).", file=tmp, append=TRUE)
        #stop("Longitude is not valid (outside -180:180.")
        x <- 1
      }
      if (any(abs(int$cent.lat) > 90)) {
        cat("\nLatitude is not valid (outside -90:90.", file=tmp, append=TRUE) 
        x <-1    
       # stop("Latitude is not valid (outside -90:90.")
      } 
      if(mean(int$cent.lon)<0){
        if(length(which(int$cent.lon>mean(int$cent.lon)/4|int$cent.lon<mean(int$cent.lon)*4))>0){
          print(paste('At least one centroid may be inaccurate. Check for consistency in signs.',
              data.frame(int[which(int$cent.lon>mean(int$cent.lon)/4|int$cent.lon<mean(int$cent.lon)*4),])))
        }
      }
    } else {
  #Weighted variables 
      if(x!=1){
      int <- assignment_column(dat = dataset, gridfile = gridfile, lon.grid = lon.grid, 
                              lat.grid = lat.grid, lon.dat = lon.dat, lat.dat = lat.dat, cat = cat)  
      int$cent.lon <- stats::ave(int[c(lon.dat, weight.var)], int$ZoneID, 
                          FUN = function(x) stats::weighted.mean(x[[lon.dat]], x[[weight.var]]))[[1]]
      int$cent.lat <- stats::ave(int[c(lat.dat, weight.var)], int$ZoneID, 
                          FUN = function(x) stats::weighted.mean(x[[lat.dat]], x[[weight.var]]))[[1]]
      }
    }
 
  } 
  #begin dataframe
  else {
    # Centroid based on spatial data file or data set
    if (!is.null(gridfile)) {
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
        cat("\nLongitude is not valid (outside -180:180).", file=tmp, append=TRUE) 
        x<-1
        #stop("Longitude is not valid (outside -180:180.")
      }
      if (any(abs(int[[lat]]) > 90)) {
        cat("\nLatitude is not valid (outside -90:90).", file=tmp, append=TRUE)
        x<-1
        #stop("Latitude is not valid (outside -90:90.")
      }
    } 
    if(x!=1){
    # simple centroid
    if (FishSET:::is_empty(weight.var)) {
      if (is.data.frame(int) == T) {
        int$cent.lon <- stats::ave(int[[lon]], int[[cat]])
        int$cent.lat <- stats::ave(int[[lat]], int[[cat]])
      } else {
        int$cent <- sf::st_centroid(int)
      }
    } else {
      # weighted centroid
      if (is.data.frame(int) == T) {
        if (!is.null(gridfile)) {
        
          int <- assignment_column(dat=dataset, gridfile = gridfile, lon.grid = lon.grid, 
                                lat.grid = lat.grid, lon.dat = lon.dat, lat.dat = lat.dat, cat = cat)  
          int$cent.lon <- stats::ave(int[c(lon.dat, weight.var)], int$ZoneID, 
                            FUN = function(x) stats::weighted.mean(x[[lon.dat]], x[[weight.var]]))[[1]]
          int$cent.lat <- stats::ave(int[c(lat.dat, weight.var)], int$ZoneID, 
                            FUN = function(x) stats::weighted.mean(x[[lat.dat]], x[[weight.var]]))[[1]]
        } else {
          int$cent.lon <- stats::ave(int[c(lon, weight.var)], int[[cat]], 
                            FUN = function(x) stats::weighted.mean(x[[lon]], x[[weight.var]]))[[1]]
          int$cent.lat <- stats::ave(int[c(lat, weight.var)], int[[cat]], 
                            FUN = function(x) stats::weighted.mean(x[[lat]], x[[weight.var]]))[[1]]
        }
      } else {
        int$cent <- spatialEco::wt.centroid(int, weight.var, sp = TRUE)
      }
    }
    }
  
  if(x!=1){
    colnames(int)[colnames(int) == cat] <- "ZoneID"
    int <- int[, c("ZoneID", "cent.lon", "cent.lat")]
    int <- unique(int)
  }
  }
     

    return(int)
}

