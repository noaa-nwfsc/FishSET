#'  Identify geographic centroid of fishery management or regulatory zone

#' @param project Name of project
#' @param gridfile Spatial data containing information on fishery management or regulatory zones. Can be shape file, json, geojson, data frame, or list.
#' @param cat Variable or list in \code{gridfile} that identifies the individual areas or zones. If \code{gridfile} is class sf, \code{cat} should be name of list containing information on zones.
#' @param lon.grid Variable or list from \code{gridfile} containing longitude data. Required for csv files. Leave as NULL if \code{gridfile} is a shape or json file.
#' @param lat.grid Variable or list from \code{gridfile} containing latitude data. Required for csv files. Leave as NULL if \code{gridfile} is a shape or json file.
#' @keywords centroid, zone, polygon
#' @importFrom sf st_centroid  st_as_sf
#' @importFrom rgeos gCentroid
#' @importFrom stats ave weighted.mean
#' @importFrom methods as
#' @return Returns a data frame where each row is a unique zone and columns are the zone ID and the latitude and longitude 
#'   defining the centroid of each zone.
#' @export find_centroid
#' @details Returns the geographic centroid of each area/zone in \code{gridfile}. The centroid table is saved to the FishSET database.
#' Function is called by the \code{create_alternative_choice} and \code{create_dist_between} functions.



find_centroid <- function(project, gridfile, cat, lon.grid = NULL, lat.grid = NULL) {
  
  # Call in datasets
  gridname <- deparse(substitute(gridfile))
    
  tmp <- tempfile()
  cat("", file = tmp, append = TRUE)
  x <- 0
  
    if (any(grepl("Spatial", class(gridfile)))) {
      if(any(class(gridfile) %in% c("sp", "SpatialPolygonsDataFrame"))) {
        gridfile <- sf::st_as_sf(gridfile)
      } else {
    if (is_empty(lon.grid) | is_empty(lat.grid)) {
      warning("lat.grid and lon.grid must be supplied to convert sp object to a sf object.")
      x <- 1
    } else {
      # map2 <- sf::st_read('Z:/OLDFishSET/NMFS_RA.json')
      gridfile <- sf::st_as_sf(
        x = gridfile,
        coords = c(lon.grid, lat.grid),
        crs = "+proj=longlat +datum=WGS84"
      )
    }
      }}
  

  # For json and shape files
  if (any(class(gridfile) == "sf")) {
      int <- rgeos::gCentroid(methods::as(gridfile, "Spatial"), byid = TRUE)
      int <- cbind(gridfile[[cat]], as.data.frame(int))
      colnames(int) <- c("ZoneID", "cent.lon", "cent.lat")
      if (any(abs(int$cent.lon) > 180)) {
        cat("Longitude is not valid (outside -180:180).", file = tmp, append = TRUE)
        # stop("Longitude is not valid (outside -180:180.")
        x <- 1
      }
      if (any(abs(int$cent.lat) > 90)) {
        cat("\nLatitude is not valid (outside -90:90.", file = tmp, append = TRUE)
        x <- 1
        # stop("Latitude is not valid (outside -90:90.")
      }
      if (mean(int$cent.lon) < 0) {
        if (length(which(int$cent.lon > mean(int$cent.lon) / 4 | int$cent.lon < mean(int$cent.lon) * 4)) > 0) {
          print(paste("At least one centroid may be inaccurate. Check for consistency in signs.",
            paste(data.frame(int[which(int$cent.lon > mean(int$cent.lon) / 4 | int$cent.lon < mean(int$cent.lon) * 4), ])),
            collapse = "; "
          ))
      }
    } 
  } else {
    # Centroid based on spatial data file or data set

      int <- gridfile
      lon <- lon.grid
      lat <- lat.grid


    
    if (x != 1) {
      # simple centroid
        if (is.data.frame(int) == T) {
          int$cent.lon <- stats::ave(int[[lon]], int[[cat]])
          int$cent.lat <- stats::ave(int[[lat]], int[[cat]])
        } else {
          int$cent <- sf::st_centroid(int)
        }
    }

    if (x != 1) {
      colnames(int)[colnames(int) == cat] <- "ZoneID"
      int <- int[, c("ZoneID", "cent.lon", "cent.lat")]
      int <- unique(int)
    }
  }


  suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project = project)))
  DBI::dbWriteTable(fishset_db, paste0(noquote(gridname), "Centroid"), int, overwrite = TRUE)
  DBI::dbDisconnect(fishset_db)
  message('Geographic centroid saved to fishset database')
  return(int)
}
