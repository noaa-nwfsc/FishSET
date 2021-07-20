#'  Assign each observation in the primary dataset to a fishery management or regulatory zone
#'  
#' @description Assign each observation in the primary dataset to a fishery management or regulatory zone. 
#'    Function is primarily called by other functions that require zone assignment but can also be used on its own.
#' @param dat Primary data containing information on hauls or trips. Table in FishSET database contains the string 'MainDataTable'.
#' @param project name of project.
#' @param gridfile Spatial data containing information on fishery management or regulatory zones. 
#'    Shape, json, geojson, and csv formats are supported.
#' @param hull.polygon Logical, if TRUE, creates convex hull polygon. Use if spatial data creating polygon 
#'   are sparse or irregular.
#' @param lon.dat Longitude variable in \code{dat}.
#' @param lat.dat Latitude variable in \code{dat}.
#' @param lon.grid Variable or list from \code{gridfile} containing longitude data. 
#'    Required for csv files. Leave as NULL if \code{gridfile} is a shape or json file.
#' @param lat.grid Variable or list from \code{gridfile} containing latitude data. 
#'   Required for csv files. Leave as NULL if \code{gridfile} is a shape or json file.
#' @param cat Variable or list in \code{gridfile} that identifies the individual areas or zones. 
#'    If \code{gridfile} is class sf, \code{cat} should be name of list containing information on zones.
#' @param epsg EPSG code. Set the epsg code to ensure that \code{gridfile} and \code{dat} have the same 
#'   projections. If epsg is not specified but is defined for \code{gridfile}, then the 
#'   \code{gridfile} coordinate reference system will be applied to \code{dat}. 
#'   See \url{http://spatialreference.org/} to help identify optimal epsg number.
#' @param closest.pt  Logical, if true, observations that fall outside zones are classed as the closest 
#'    zone polygon to the point.
#' @param bufferval Maximum buffer distance, in meters, for assigning observations to the closest zone polygon. 
#'   If no zone polygons are within the defined bufferval then observation will not be assigned to a 
#'   zone polygon. Required if closest.pt is TRUE. 
#' @param log.fun Logical, whether to log function call (for internal use).
#' @importFrom sp CRS Polygons Polygon SpatialPolygons SpatialPolygonsDataFrame coordinates
#' @importFrom rgeos gDistance
#' @importFrom grDevices chull
#' @importFrom raster projection
#' @details  Function uses the specified latitude and longitude from the primary dataset to assign each row of the 
#' primary dataset to a zone. Zone polygons are defined by the spatial dataset. Set \code{hull.polygon} to TRUE if
#'  spatial data is sparse or irregular. Function is called by other functions if a zone identifier does not exist 
#'  in the primary dataset.
#' @keywords  zone, polygon
#' @return Returns primary dataset with new assignment column labeled zoneID.
#' @export


assignment_column <- function(dat, project, gridfile, lon.dat, lat.dat, cat, closest.pt = FALSE, bufferval = NULL,
                              lon.grid = NULL, lat.grid = NULL, hull.polygon = FALSE, epsg = NULL,
                              log.fun = TRUE) {

  # Call in data sets
  out <- data_pull(dat)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main")

  gridout <- data_pull(gridfile)
  grid <- gridout$dataset
  gridfile <- parse_data_name(dat, "grid")

  dataset[[lat.dat]] <- as.numeric(as.vector(dataset[[lat.dat]]))
  dataset[[lon.dat]] <- as.numeric(as.vector(dataset[[lon.dat]]))

  x <- 0
  
  if(anyNA(dataset[[lat.dat]])|| anyNA(dataset[[lon.dat]])){
    warning('Missing values in coordinates not allowed. Function not run.')
    x <- 1
  }
  
  if (any(abs(dataset[[lon.dat]]) > 180, na.rm=TRUE)) {
    warning("Longitude is not valid (outside -180:180). Function not run")
    # stop('Longitude is not valid (outside -180:180.')
    x <- 1
  }
  if (any(abs(dataset[[lat.dat]]) > 90, na.rm=TRUE)) {
    warning("Latitude is not valid (outside -90:90. Function not run")
    x <- 1
    # stop('Latitude is not valid (outside -90:90.')
  }

  
  if (x == 0) {
    # For json and shape files
    if(any(class(grid) %in% "sf")){
      if(!is.null(epsg)) {
        dat_sub <- sf::st_as_sf(x = dataset, coords = c(lon.dat, lat.dat), crs = "+proj=longlat +datum=WGS84")
        dat_sub <- sf::st_transform(dat_sub, epsg)
      } else if(!is.na(sf::st_crs(grid))){
        dat_sub <- sf::st_as_sf(x = dataset, coords = c(lon.dat, lat.dat), crs = sf::st_crs(grid))
      } else {
        dat_sub <- sf::st_as_sf(x = dataset, coords = c(lon.dat, lat.dat), crs = "+proj=longlat +datum=WGS84")
        warning('No coordinate reference system supplied. Set using epsg.')
        x <- 1
      }
      temp <- sf::st_intersects(dat_sub, grid)
      if(any(lengths(temp)>1)) {
        warning('At least one observation assigned to multiple regulatory zones. Assigning observations to nearest polygon.')
        dub <- which(lengths(temp)>1)
        temp[dub] <- sf::st_nearest_feature(dat_sub[dub,], grid)
      }
      if (closest.pt==TRUE) {
        if(anyNA(temp)){
          dub <- which(is.na(temp))
          nearest <- sf::st_nearest_feature(dat_sub[dub,], grid)  
          dist.rec = sf::st_distance(dat_sub[dub,], grid[nearest,], by_element=TRUE)
          distkeep <- which(as.numeric(dist.rec)< bufferval)
          temp[dub[distkeep]] <- nearest[distkeep]
          
          message(length(distkeep), ' observations assigned to nearest zone polygon within ', bufferval, ' meters. ', 
                  length(which(as.numeric(dist.rec)>0.01)), ' observations were greater than ', bufferval, ' and were not assigned.')
        }
        }
      pts <- as.data.frame(as.numeric(temp))
      colnames(pts) <- "col.id"
      pts$ID <- grid[[cat]][pts$col.id]
    } else if(any(class(grid) %in% c("sp", "SpatialPolygonsDataFrame"))) {
      dat_sub <- dataset
      sp::coordinates(dat_sub) <- c(lon.dat, lat.dat)
      sp::proj4string(dat_sub) <- sp::proj4string(grid)
      
      temp <- sp::over(dat_sub, grid)
      pts <- as.data.frame(as.numeric(temp[[cat]]))
      colnames(pts) <- "ID"
      
    } else {
      # sort data
      grid <- as.data.frame(grid)
      grid <- grid[order(grid[, cat], grid[, lon.grid], grid[, lat.grid]), ]

      # Create spatial polygon dataframe from grid data make a list
      map_list <- split(grid[, c(lon.grid, lat.grid, cat)], grid[[cat]])
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
      srdf <- sp::SpatialPolygonsDataFrame(my_spatial_polys, data.frame(row.names = c(names(map_list)), ID = names(map_list)))

      # Assign zone to data set based on lat and long
      dat_sub <- dataset
      # Assignment modified according
      sp::coordinates(dat_sub) <- c(lon.dat, lat.dat)

      # Set the projection of the SpatialPointsDataFrame using the projection of the shapefile
      sp::proj4string(dat_sub) <- sp::CRS("+proj=longlat +datum=WGS84") # proj4string(sodo)
      # identify intersections of points in data set with polygons in grid file
      pts <- sp::over(dat_sub, srdf, duplicate = F)


      if (closest.pt == TRUE) {
        closest <- data.frame(matrix(NA, nrow = length(which(is.na(pts$ID) == TRUE)), ncol = 1))
        for (i in 1:length(which(is.na(pts$ID) == TRUE))) {
          closest[i, 1] <- names(which(rgeos::gDistance(dat_sub[which(is.na(pts$ID) == TRUE), ][i, ], 
                                                        as(srdf, "SpatialLines"), byid = TRUE)[,1] == 
                                         min(rgeos::gDistance(dat_sub[which(is.na(pts$ID) == TRUE), ][i, ], as(srdf, "SpatialLines"), byid = TRUE))))
        }
        pts[which(is.na(pts$ID) == TRUE), ] <- closest
      }

      # if (anyNA(pts$ID)) { drop.points <- dataset[is.na(pts$ID)==TRUE, c(lon.dat, lat.dat)] warning('Zone ID not identified for at least one point.
      # Consider plotting points against before dropping points by assigning remove.na to TRUE or assigning these points to closest zone by setting closest
      # to TRUE. Undefined points are recorded in the log file') }
    }
  }
  
    if(x == 0){
    pts <- cbind(dataset, ZoneID = pts$ID)

    if (log.fun) {
      
      assignment_column_function <- list()
      assignment_column_function$functionID <- "assignment_column"
      assignment_column_function$args <- list(dat, project, gridfile, lon.dat, lat.dat, 
                                              cat, closest.pt, lon.grid, lat.grid, 
                                              hull.polygon, epsg, log.fun)
      log_call(project, assignment_column_function)
    }

    pts <- as.data.frame(pts)
    return(pts)
    }
  }
}
