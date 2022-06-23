#'  Assign each observation in the primary dataset to a fishery management or 
#'  regulatory zone
#'  
#' @description Assign each observation in the primary dataset to a fishery management 
#'   or regulatory zone. Function is primarily called by other functions that require 
#'   zone assignment but can also be used on its own.
#' @param dat Primary data containing information on hauls or trips. Table in FishSET 
#'   database contains the string 'MainDataTable'.
#' @param project name of project.
#' @param spat Spatial data containing information on fishery management or 
#'   regulatory zones. \code{sf} objects are recommended, but \code{sp} objects
#'   can be used as well. If using a spatial table read from a csv file, then
#'   arguments \code{lon.spat} and \code{lat.spat} are required. To upload your
#'   spatial data to the FishSETFolder see \code{\link{load_spatial}}.
#' @param hull.polygon Logical, if TRUE, creates convex hull polygon. Use if spatial 
#'   data creating polygon are sparse or irregular.
#' @param lon.dat Longitude variable in \code{dat}.
#' @param lat.dat Latitude variable in \code{dat}.
#' @param lon.spat Variable or list from \code{spat} containing longitude data. 
#'    Required for spatial tables read from csv files. Leave as \code{NULL} if 
#'    \code{spat} is an \code{sf} or \code{sp} object.
#' @param lat.spat Variable or list from \code{spat} containing latitude data. 
#'   Required for spatial tables read from csv files. Leave as \code{NULL} if 
#'   \code{spat} is an \code{sf} or \code{sp} object.
#' @param cat Variable or list in \code{spat} that identifies the individual areas 
#'   or zones. If \code{spat} is class \code{sf}, \code{cat} should be name of list 
#'   containing information on zones.
#' @param name The name of the new assignment column. Defaults to \code{"ZoneID"}. 
#' @param epsg EPSG code. Set the epsg code to ensure that \code{spat} and \code{dat} 
#'   have the same projections. If epsg is not specified but is defined for \code{spat}, 
#'   then the \code{spat} coordinate reference system will be applied to \code{dat}. 
#'   See \url{http://spatialreference.org/} to help identify optimal epsg number.
#' @param closest.pt  Logical, if \code{TRUE}, observations that fall outside zones are 
#'   classed as the closest zone polygon to the point.
#' @param bufferval Maximum buffer distance, in meters, for assigning observations 
#'   to the closest zone polygon. If no zone polygons are within the defined 
#'   \code{bufferval}, then observation will not be assigned to a zone polygon. 
#'   Required if \code{closest.pt = TRUE}. 
#' @param log.fun Logical, whether to log function call (for internal use).
#' @importFrom sf st_transform st_as_sf
#' @details  Function uses the specified latitude and longitude from the primary 
#'   dataset to assign each row of the primary dataset to a zone. Zone polygons 
#'   are defined by the spatial dataset. Set \code{hull.polygon} to \code{TRUE} 
#'   if spatial data is sparse or irregular. Function is called by other functions 
#'   if a zone identifier does not exist in the primary dataset.
#' @keywords  zone, polygon
#' @return Returns primary dataset with new assignment column.
#' @export
#' @examples 
#' \dontrun{
#' pollockMainDataTable <- 
#'      assignment_column(pollockMainDataTable, "pollock", spat = pollockNMFSSpatTable,
#'                        lon.dat = "LonLat_START_LON", lat.dat = "LonLat_START_LAT")
#' }

assignment_column <- function(dat, project, spat, lon.dat, lat.dat, cat, name = "ZoneID", 
                              closest.pt = FALSE, bufferval = NULL, lon.spat = NULL, 
                              lat.spat = NULL, hull.polygon = FALSE, epsg = NULL,
                              log.fun = TRUE) {

  # Call in data sets
  out <- data_pull(dat, project)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main", project)
  
  spat_out <- data_pull(spat, project)
  spatdat <- spat_out$dataset
  spat <- parse_data_name(dat, "spat", project)
  
  # why use as.vector? 
  dataset[[lat.dat]] <- as.numeric(as.vector(dataset[[lat.dat]]))
  dataset[[lon.dat]] <- as.numeric(as.vector(dataset[[lon.dat]]))
  
  column_check(dataset, cols = c(lon.dat, lat.dat))
  column_check(spatdat, cols = c(cat, lon.spat, lat.spat))
  
  name_check(dataset, names = name)
  
  if(anyNA(dataset[[lat.dat]])|| anyNA(dataset[[lon.dat]])) {
    
    stop('Missing values in coordinates not allowed. Areas not assigned.', call. = FALSE)
  }
  
  if (any(abs(dataset[[lon.dat]]) > 180, na.rm = TRUE)) {
    
    stop("Longitude is not valid (outside -180:180). Areas not assigned.", call. = FALSE)
  }
  
  if (any(abs(dataset[[lat.dat]]) > 90, na.rm = TRUE)) {
    
    stop("Latitude is not valid (outside -90:90). Areas not assigned.", call. = FALSE)
  }
  
  # convert dat to sf object
  dat_sf <- sf::st_as_sf(x = dataset, coords = c(lon.dat, lat.dat), crs = 4326)
  
  spatdat <- check_spatdat(spatdat, lon = lon.spat, lat = lat.spat, id = cat)
  
  if (sf::st_crs(spatdat) != sf::st_crs(dat_sf)) {
    
    warning("Projection does not match. The detected projection in the ",
            "spatial file will be used unless epsg is specified.", call. = FALSE)
  }
  
  if (!is.null(epsg)) {
    
    dat_sf <- sf::st_transform(dat_sf, crs = epsg)
    spatdat <- sf::st_transform(spatdat, crs = epsg)
    
  } else if (!is.na(sf::st_crs(spatdat))) { # if crs isn't missing from spatdat
    
    dat_sf <- sf::st_transform(dat_sf, sf::st_crs(spatdat))
    
  } else { # default to WGS 84
    
    spatdat <- sf::st_transform(spatdat, crs = 4326)
  }
  
  if (any(!(sf::st_is_valid(spatdat)))) {
    
    spatdat <- sf::st_make_valid(spatdat)
  } 
  
  inter <- sf::st_intersects(dat_sf, spatdat)
  
  if (any(lengths(inter) > 1)) {
    
    warning('At least one observation assigned to multiple regulatory zones. ',  
            'Assigning observations to nearest polygon.', call. = FALSE)
    
    dub <- which(lengths(inter) > 1)
    
    inter[dub] <- sf::st_nearest_feature(dat_sf[dub, ], spatdat)
  }
  
  if (closest.pt) {
    
    if (any(lengths(inter) == 0)) { 
      
      # index of obs that didn't intersect area
      ind <- which(lengths(inter) == 0)
      
      dist.rec <- sf::st_is_within_distance(dat_sf[ind, ], spatdat, dist = bufferval)
      
      # index of obs within buffer 
      within_buff <- which(lengths(dist.rec) > 0)
      
      # index of area closest to obs
      nearest <- sf::st_nearest_feature(dat_sf[ind[within_buff], ], spatdat) 
      
      # assign obs within buffer to nearest area
      inter[ind[within_buff]] <- nearest
      
      message(length(within_buff), ' observations assigned to nearest zone polygon within ', 
              bufferval, ' meters. ', length(ind) - length(within_buff), 
              ' observations had distances greater than ', bufferval, ' meters and were not assigned.')
    }
  }
  
  # create new assignment column
  pts <- as.numeric(inter)
  dataset[[name]] <- spatdat[[cat]][pts]

  if (log.fun) {
    
    assignment_column_function <- list()
    assignment_column_function$functionID <- "assignment_column"
    assignment_column_function$args <- list(dat, project, spat, lon.dat, lat.dat, 
                                            cat, name, closest.pt,  bufferval, 
                                            lon.spat, lat.spat, hull.polygon, epsg, 
                                            log.fun)
    log_call(project, assignment_column_function)
  }

  return(dataset)
}
