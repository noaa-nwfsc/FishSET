#'  Identify geographic centroid of fishery management or regulatory zone

#' @param project Name of project
#' @param spat Spatial data containing information on fishery management or regulatory 
#'   zones. Can be shape file, json, geojson, data frame, or list.
#' @param cat Variable or list in \code{spat} that identifies the individual areas 
#'   or zones. If \code{spat} is class sf, \code{cat} should be name of list containing 
#'   information on zones.
#' @param lon.spat Variable or list from \code{spat} containing longitude data. 
#'   Required for csv files. Leave as NULL if \code{spat} is a shape or json file.
#' @param lat.spat Variable or list from \code{spat} containing latitude data. 
#'   Required for csv files. Leave as NULL if \code{spat} is a shape or json file.
#' @keywords centroid, zone, polygon
#' @importFrom sf st_centroid st_coordinates st_cast
#' @importFrom DBI dbConnect dbDisconnect dbWriteTable
#' @importFrom RSQLite SQLite
#' @return Returns a data frame where each row is a unique zone and columns are 
#'   the zone ID and the latitude and longitude defining the centroid of each zone.
#' @export find_centroid
#' @details Returns the geographic centroid of each area/zone in \code{spat}. The 
#'   centroid table is saved to the FishSET database. Function is called by the 
#'   \code{create_alternative_choice} and \code{create_dist_between} functions.



find_centroid <- function(project, spat, cat, lon.spat = NULL, lat.spat = NULL) {
 
  # Call in datasets
  spat_out <- data_pull(spat, project)
  spatdat <- spat_out$dataset
  spat <- parse_data_name(spatdat, "spat", project)
  
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  cat("", file = tmp, append = TRUE)
  
  spatdat <- check_spatdat(spatdat, lon = lon.spat, lat = lat.spat, id = cat)
    
  int <-  sf::st_centroid(spatdat)
  int <- as.data.frame(cbind(int[[cat]], 
                             sf::st_coordinates(sf::st_cast(int, "POINT"))))
  
  colnames(int) <- c("ZoneID", "cent.lon", "cent.lat")
  
  int$cent.lon <- as.numeric(int$cent.lon)
  int$cent.lat <- as.numeric(int$cent.lat)
  
  if (any(abs(int$cent.lon) > 180)) {
    
    cat("Longitude is not valid (outside -180:180).", file = tmp, append = TRUE)
    stop("Longitude is not valid (outside -180:180).")
  }
  
  if (any(abs(int$cent.lat) > 90)) {
    
    cat("\nLatitude is not valid (outside -90:90).", file = tmp, append = TRUE)
    stop("Latitude is not valid (outside -90:90).")
  }
  
  if (any(table(int$ZoneID) > 1)) {
    
    int <- int[!duplicated(int$ZoneID),]
    warning('Duplicate centoids found for at least one zone. Using first centroid.')
  }
  
  suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project = project)))
  on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
  
  DBI::dbWriteTable(fishset_db, paste0('spat', "Centroid"), int, overwrite = TRUE)
  message('Geographic centroid saved to fishset database')
  return(int)
}
