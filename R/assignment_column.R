#'  Assign each observation in the primary dataset to a fishery management or regulatory zone. 
#'  
#' @description Assign each observation in the primary dataset to a fishery management or regulatory zone. 
#'    Function is primarily called by other functions that require zone assignment but can also be used on its own.
#' @param dat Primary data containing information on hauls or trips. Table in FishSET database contains the string 'MainDataTable'.
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
#' @param epsg EPSG number. See \url{http://spatialreference.org/} to help identify optimal epsg number.
#' @param closest.pt  Logical, if true, observations that fall outside zones are classed as the closest 
#'    zone polygon to the point.
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


assignment_column <- function(dat, gridfile, lon.dat, lat.dat, cat, closest.pt = FALSE, lon.grid = NULL, lat.grid = NULL, 
                              hull.polygon = FALSE, epsg = NULL) {

  # Call in data sets
  dataset <- dat
  dat <- deparse(substitute(dat))

  dataset[[lat.dat]] <- as.numeric(as.vector(dataset[[lat.dat]]))
  dataset[[lon.dat]] <- as.numeric(as.vector(dataset[[lon.dat]]))

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
    # For json and shape files
    if (any(class(gridfile) %in% "sf") || any(class(gridfile) %in% c("sp", "SpatialPolygonsDataFrame"))) {
      # map2 <- sf::st_read('Z:/OLDFishSET/NMFS_RA.json')
      dat_sub <- sf::st_as_sf(x = dataset, coords = c(lon.dat, lat.dat), crs = "+proj=longlat +datum=WGS84")

      if (raster::projection(gridfile) != raster::projection(dat_sub)) {
        warning("Projection does not match. Consider transforming data to same epsg.")
      }
      if (!is.null(epsg)) {
        dat_sub <- sf::st_transform(dat_sub, epsg)
        gridfile <- sf::st_transform(gridfile, epsg)
      } else {
        gridfile <- sf::st_transform(gridfile, "+proj=longlat +datum=WGS84")
      }
      pts <- as.data.frame(as.numeric(sf::st_intersects(dat_sub, gridfile)))
      colnames(pts) <- "col.id"
      pts$ID <- gridfile[[cat]][pts$col.id]
    } else {
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
          closest[i, 1] <- names(which(rgeos::gDistance(dat_sub[which(is.na(pts$ID) == TRUE), ][i, ], as(srdf, "SpatialLines"), byid = TRUE)[
            ,
            1
          ] == min(rgeos::gDistance(dat_sub[which(is.na(pts$ID) == TRUE), ][i, ], as(srdf, "SpatialLines"), byid = TRUE))))
        }
        pts[which(is.na(pts$ID) == TRUE), ] <- closest
      }

      # if (anyNA(pts$ID)) { drop.points <- dataset[is.na(pts$ID)==TRUE, c(lon.dat, lat.dat)] warning('Zone ID not identified for at least one point.
      # Consider plotting points against before dropping points by assigning remove.na to TRUE or assigning these points to closest zone by setting closest
      # to TRUE. Undefined points are recorded in the log file') }
    }

    pts <- cbind(dataset, ZoneID = pts$ID)


    pts <- as.data.frame(pts)
    return(pts)
  }
}
