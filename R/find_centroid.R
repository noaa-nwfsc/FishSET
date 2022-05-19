#'  Identify geographic centroid of fishery management or regulatory zone
#'
#' @param spat Spatial data containing information on fishery management or regulatory 
#'   zones. Can be shape file, json, geojson, data frame, or list.
#' @param project Name of project
#' @param cat Variable or list in \code{spat} that identifies the individual areas 
#'   or zones. If \code{spat} is class sf, \code{cat} should be name of list containing 
#'   information on zones.
#' @param lon.spat Variable or list from \code{spat} containing longitude data. 
#'   Required for csv files. Leave as NULL if \code{spat} is a shape or json file.
#' @param lat.spat Variable or list from \code{spat} containing latitude data. 
#'   Required for csv files. Leave as NULL if \code{spat} is a shape or json file.
#' @param log.fun Logical, whether to log function call (for internal use).
#' @keywords centroid, zone, polygon
#' @importFrom sf st_centroid st_coordinates st_cast
#' @importFrom tibble tibble
#' @importFrom DBI dbConnect dbDisconnect dbWriteTable
#' @importFrom RSQLite SQLite
#' @return Returns a data frame where each row is a unique zone and columns are 
#'   the zone ID and the latitude and longitude defining the centroid of each zone.
#' @export find_centroid
#' @details Returns the geographic centroid of each area/zone in \code{spat}. The 
#'   centroid table is saved to the FishSET database. Function is called by the 
#'   \code{create_alternative_choice} and \code{create_dist_between} functions.

find_centroid <- function(spat, project, cat, lon.spat = NULL, lat.spat = NULL,
                          log.fun = TRUE) {
 
  # Call in datasets
  spat_out <- data_pull(spat, project)
  spatdat <- spat_out$dataset
  spat <- parse_data_name(spat, "spat", project)
  
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  cat("", file = tmp, append = TRUE)
  
  column_check(spatdat, cols = c(cat, lon.spat, lat.spat))
  
  spatdat <- check_spatdat(spatdat, lon = lon.spat, lat = lat.spat, id = cat)
    
  cent <- sf::st_centroid(spatdat)
  
  # check if any feature is not a point (possible/necessary?)
  if (any(!sf::st_is(cent, "POINT"))) {
    
    cent <- sf::st_cast(cent, "POINT")
  }
  
  # TODO: consider different name for ZoneID, e.g. cent.id (or name in "cat" var). Update other functions.
  cent_coord <- sf::st_coordinates(cent)
  cent <- tibble::tibble(ZoneID = cent[[cat]], 
                         cent.lon = cent_coord[ , 1], 
                         cent.lat = cent_coord[ , 2])
  
  cent$cent.lon <- as.numeric(cent$cent.lon)
  cent$cent.lat <- as.numeric(cent$cent.lat)
  
  if (any(abs(cent$cent.lon) > 180)) {
    
    cat("Longitude is not valid (outside -180:180).", file = tmp, append = TRUE)
    stop("Longitude is not valid (outside -180:180).")
  }
  
  if (any(abs(cent$cent.lat) > 90)) {
    
    cat("\nLatitude is not valid (outside -90:90).", file = tmp, append = TRUE)
    stop("Latitude is not valid (outside -90:90).")
  }
  
  if (any(table(cent$ZoneID) > 1)) {
    
    cent <- cent[!duplicated(cent$ZoneID),]
    warning('Duplicate centoids found for at least one zone. Using first centroid.')
  }
  
  # TODO: add project name to centroid table name, update other funs
  suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project = project)))
  on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
  
  DBI::dbWriteTable(fishset_db, paste0(spat, "Centroid"), cent, overwrite = TRUE)
  
  # this should be removed eventually; create_alternative_choice() tends to break
  # if "spatCentroid" hasn't been saved
  DBI::dbWriteTable(fishset_db, "spatCentroid", cent, overwrite = TRUE)
  message('Geographic centroid saved to fishset database')
  
  if (log.fun) {
    
    find_centroid_function <- list()
    find_centroid_function$functionID <- "find_centroid"
    find_centroid_function$args <- list(spat, project, cat, lon.spat, lat.spat, 
                                        log.fun)
    log_call(project, find_centroid_function)
  }
  
  return(cent)
}
