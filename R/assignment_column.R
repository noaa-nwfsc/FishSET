#'  Generate centroid of polygon of zone or area
#'  
#'
#' @param dat Main data frame containing data on hauls or trips. Table in fishset_db database should contain the string `MainDataTable`.
#' @param gridfile Spatial data. Shape, json, and csv formats are supported.
#' @param hull.polygon T/F If TRUE, creates convex hull polygon. Use if spatial data creating polygon are sparse or irregular.
#' @param lon.dat Column containing longitude data in main data frame.
#' @param lat.dat Column containing latitude data in main data frame.
#' @param lon.grid Column containing longitude data in gridfile.
#' @param lat.grid lColumn containing latitude data in gridfile.
#' @param cat  Column in gridfile that identifies the individual areas or zones. If gridfile is class sf, `cat` should be name of list containing information on zones. 
#' @param epsg EPSG number. See \url{http://spatialreference.org/} to help identify optimal epsg number. 
#' @param closest.pt  TRUE/FALSE If true, zone ID identified as the closest polygon to the point.
#' @importFrom sp CRS Polygons Polygon SpatialPolygons SpatialPolygonsDataFrame coordinates
#' @importFrom rgeos gDistance
#' @importFrom grDevices chull
#' @importFrom raster projection
#' @details  Function is called by the \code{\link{create_alternative_choice}} function to assign each observation to zones defined by a spatial data set.
#' Converts point data from gridfile into polygons and then finds which polygon each observation in the main data frame is within. Use hull.polygon=T if data is sparse or irregular.
#' @keywords  zone, polygon
#' @return Main data frame with new assignment column labeled zoneID.
#' @export 

# read.csv('Data/adfgstat6_map.csv', header = F) require(rjson) 


assignment_column <- function(dat, gridfile, hull.polygon = c(TRUE, FALSE), lon.grid, lat.grid, 
                              lon.dat, lat.dat, cat,  closest.pt = c(TRUE, FALSE), epsg=NULL) {
  
  #Call in data sets
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
  
  
  #For json and shape files
  if(any(class(gridfile)=='sf')) {
    #map2 <- sf::st_read('C:/Users/melanie.harsch/Work/OLDFishSET/NMFS_RA.json') 
    dat_sub <- sf::st_as_sf(x = dataset, 
                            coords = c(lon.dat, lat.dat),
                            crs = "+proj=longlat +datum=WGS84")
    
    if(raster::projection(gridfile) != raster::projection(dat_sub)) {
      warning('Projection does not match. Consider transforming data to same epsg.')
    }
    if(!is.null(epsg)){
      dat_sub <- st_transform(dat_sub, epsg)
      gridfile <- st_transform(gridfile, epsg)
    }
    pts <- as.data.frame(as.numeric(sf::st_intersects(dat_sub, gridfile)))
    colnames(pts)='col.id'
    pts$ID <- gridfile[[cat]][pts$col.id]
  } 
    else {
      # sort data
  gridfile <- as.data.frame(gridfile)
  gridfile <- gridfile[order(gridfile[, cat], gridfile[, lon.grid], gridfile[, lat.grid]), ]
  
  # Create spatial polygon dataframe from grid data make a list
  map_list <- split(gridfile[, c(lon.grid, lat.grid, cat)], gridfile[[cat]])
  # only want lon-lats in the list, not the names
  map_list <- lapply(map_list, function(x) {
    x[cat] <- NULL
    x
  })
  
  if (hull.polygon == T) {
    ps <- lapply(map_list, function(x) x[c(grDevices::chull(x), grDevices::chull(x)[1]), ])
    p1 <- lapply(seq_along(ps), function(i) sp::Polygons(list(sp::Polygon(ps[[i]])), ID = names(map_list)[i]))
  } else {
      # add id variable
      ps <- suppressWarnings(lapply(map_list, sp::Polygon))
      p1 <- lapply(seq_along(ps), function(i) sp::Polygons(list(ps[[i]]), ID = names(map_list)[i]))
    }
  
    my_spatial_polys <- sp::SpatialPolygons(p1, proj4string = sp::CRS("+proj=longlat +datum=WGS84"))
  
    # Change to spatial polygon dataframe
    srdf <- sp::SpatialPolygonsDataFrame(my_spatial_polys, data.frame(row.names = c(names(map_list)), 
                                                                    ID = names(map_list)))
  
    # Assign zone to data set based on lat and long
    dat_sub <- dataset
    # Assignment modified according
    sp::coordinates(dat_sub) <- c(lon.dat, lat.dat)
  
    # Set the projection of the SpatialPointsDataFrame using the projection of the shapefile
    sp::proj4string(dat_sub) <- sp::CRS("+proj=longlat +datum=WGS84")  #proj4string(sodo)
    # identify intersections of points in data set with polygons in grid file
    pts <- sp::over(dat_sub, srdf, duplicate = F)

   
  if (closest.pt == TRUE) {
    closest <- data.frame(matrix(NA, nrow = length(which(is.na(pts$ID) == TRUE)), ncol = 1))
    for (i in 1:length(which(is.na(pts$ID) == TRUE))) {
      closest[i, 1] <- names(which(rgeos::gDistance(dat_sub[which(is.na(pts$ID) == TRUE), ][i, ], 
                                                    as(srdf, "SpatialLines"), byid = TRUE)[, 1] == 
                                     min(rgeos::gDistance(dat_sub[which(is.na(pts$ID) == TRUE), ][i, ], as(srdf, "SpatialLines"), byid = TRUE))))
    }
    pts[which(is.na(pts$ID) == TRUE), ] <- closest
  }
  
  if (any(is.na(pts$ID))) {
    drop.points <- dataset[is.na(pts$ID)==TRUE, c(lon.dat, lat.dat)]
    warning("Zone ID not identified for at least one point. Consider plotting points against before dropping points by assigning remove.na to TRUE or 
         assigning these points to closest zone by setting closest to TRUE. Undefined points are recorded in the log file")


  }
  }
  
  pts <- cbind(dataset, ZoneID=pts$ID)
  

  pts <- as.data.frame(pts)
  return(pts)
  }

