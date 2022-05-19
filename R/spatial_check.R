shift_long <- function(spat) {
  #' Shift longitude predicate
  #' Detects whether longitude coordinates should be re-centered to Pacific view
  #' @param spat Spatial data to check.
  #' @return \code{TRUE} if longitude should be shifted to pacific view (i.e. 
  #' any longitude values < 0). 
  #' @importFrom sf st_bbox
  #' @keywords internal

  # this is just a working version, needs elaboration
  bbox <- sf::st_bbox(spat)
  
  bbox$xmin < -179 | bbox$xmax > 179
}


is_invalid_spat <- function(spat) {
  #' Detect basic spatial issues
  #' 
  #' Predicate function that returns \code{TRUE} if certain spatial issues are
  #' found.
  #' 
  #' @param spat Spatial data to check.
  #' @importFrom sf st_is st_is_valid st_is_empty
  #' @keywords internal
  #' @return \code{TRUE} if a "GEOMETRYCOLLECTION" is found, if any spatial 
  #' features are not "POLYGON" or "MULTIPOLYGON", if invalid geometries are 
  #' found, if any empty geometries are detected, or if longitude needs to be
  #' shifted to Pacific view. 
  
  any(sf::st_is(spat, "GEOMETRYCOLLECTION")) ||
    any(!sf::st_is(spat, c("MULTIPOLYGON", "POLYGON"))) ||
    any(!sf::st_is_valid(spat)) ||
    any(is.na(sf::st_is_valid(spat))) || 
    any(sf::st_is_empty(spat)) ||
    shift_long(spat)
}


clean_spat <- function(spat) {
  #' Clean spatial data
  #' @param spat Spatial data to check.
  #' @keywords internal
  #' @import sf
  #' @details \code{clean_spat} extracts polygons from "GEOMETRYCOLLECTION"
  #' spatial features, removes non-polygons from the data, attempts to fix 
  #' invalid geometries, and shifts longitude to Pacific view if any points 
  #' are less than 0. 
  
  sf_is_empty <- sf::st_is_empty(spat)
  
  if (any(sf_is_empty)) {
    
    if ("sfc" %in% class(spat)) {
      
      spat <- spat[which(!sf_is_empty)]
      
    } else {
      
      spat <- spat[!sf_is_empty, ]
    }
  }
  
  if (any(sf::st_is(spat, "GEOMETRYCOLLECTION"))) {
    
    spat <- sf::st_collection_extract(spat, "POLYGON") # multipoly? Other types? Lines?
  }
  
  is_polygon <- sf::st_is(spat, c("MULTIPOLYGON", "POLYGON"))
  
  if (any(is_polygon == FALSE)) {
    
    spat <- spat[is_polygon]
  }
  
  if (any(sf::st_is_valid(spat) == FALSE)) {
    
    if (requireNamespace("lwgeom")) {
      
      spat <- sf::st_make_valid(spat)
      
    } else {
      
      invalid <- !sf::st_is_valid(spat)
      spat[invalid, ] <- sf::st_buffer(spat[invalid, ], 0.0)
      warning("Package lwgeom not installed, using buffer method to fix geometry.",
              " This may result in an invalid geometry.")
    }
  }
  
  # shift to Pacific view if needed
  if (shift_long(spat)) {
    
    spat <- sf::st_shift_longitude(spat)
  }
  
  spat
}


dat_to_sf <- function(dat, lon, lat, id, cast = "POLYGON", multi = FALSE,
crs = 4326) {
  #' Convert dataframe to sf
  #' 
  #' Used to convert spatial data with no spatial class to a \code{sf} object. 
  #' This is useful if the spatial data was read from a non-spatial file type, 
  #' e.g. a CSV file. 
  #'
  #'@param dat Spatial data containing information on fishery management or 
  #'   regulatory zones. 
  #' @param lon Longitude variable in \code{spatdat}. 
  #' @param lat Latitude variable in \code{spatdat}. 
  #' @param id Spatial feature ID column. 
  #' @param cast Spatial feature type to create. Commonly used options are \code{"POINT"},
  #'   \code{"LINESTRING"}, and \code{"POLYGON"}. See \code{\link[sf]{st_cast}} 
  #'   for details. 
  #' @param multi Logical, use if needing to convert to a multi-featured (grouped)
  #'   \code{sf} object, e.g. \code{MULTIPOLYGON} or \code{MULTILINESTRING}.
  #' @param crs Coordinate reference system to assign to \code{dat}. Defaults to
  #' WGS 84 (EPSG: 4326).
  #' @export
  #'@importFrom sf st_as_sf st_cast
  #'@importFrom dplyr group_by across summarize  %>%
  
  spatdat <- sf::st_as_sf(x = dat, coords = c(lon, lat), crs = crs)
  
  if (cast != "POINT") { # allow if multi = TRUE
    
    # Convert point geometry to polygon
    spatdat <- 
      spatdat %>%
      dplyr::group_by(dplyr::across(id)) %>% 
      dplyr::summarize(do_union = FALSE) %>% 
      sf::st_cast(cast)
    
    # error cases?
    if (multi) {
      # unexpected type?
      m_cast <- switch(cast, "POINT" = "MULTIPOINT", "LINESTRING" = "MULTILINESTRING",
                       "POLYGON" = "MULTIPOLYGON")
      
      spatdat <- sf::st_cast(spatdat, m_cast)
    }
  }
 
  spatdat
} 


check_spatdat <- function(spatdat, lon = NULL, lat = NULL, id = NULL) {
  #' Check and correct spatial data format
  #' 
  #' Converts spatial data to a \code{sf} object
  #' 
  #' @param spatdat Spatial data containing information on fishery management or 
  #'   regulatory zones. 
  #' @param lon Longitude variable in \code{spatdat}. This is required for csv files 
  #' or if \code{spatdat} is a dataframe (i.e. is not a \code{sf} or \code{sp} object).
  #' @param lat Latitude variable in \code{spatdat}. This is required for csv files 
  #' or if \code{spatdat} is a dataframe (i.e. is not a \code{sf} or \code{sp} object).
  #' @param id Polygon ID column. This is required for csv files or if \code{spatdat} 
  #' is a dataframe (i.e. is not a \code{sf} or \code{sp} object).
  #' @export
  #' @details This function checks whether \code{spatdat} is a \code{sf} object
  #'   and attempts to convert it if not. It also applies \code{\link{clean_spat}}
  #'   which fixes certain spatial issues such as invalid or empty polygons, 
  #'   whether a projected CRS is used (converts to WGS84 if detected), and if 
  #'   longitude should be shifted to Pacific view (0-360 format) to avoid 
  #'   splitting the Alaska region during plotting. 
  #' @importFrom sf st_as_sf st_crs st_transform st_shift_longitude

  if (!inherits(spatdat, "sf")) {
    
    if (inherits(spatdat, c("sp", "SpatialPolygonsDataFrame"))) {
      
      spatdat <- sf::st_as_sf(spatdat)
      
    } else {
      
      if (!is.null(lon) & !is.null(lat) & !is.null(id)) {
        
        # convert dataframe to sf
        spatdat <- dat_to_sf(spatdat, lon, lat, id)
        
      } else {
        
        stop("Arguments \"lon\", \"lat\", and \"id\" are needed to convert ",
             "spatial data to sf.", call. = FALSE)
      }
    }
  }
  
  stopifnot("Spatial data could not be converted to sf" = inherits(spatdat, "sf"))

  # Convert to WGS84 if projected
  if (any(grepl('PROJCRS',  sf::st_crs(spatdat)))) {
    
    spatdat <- sf::st_transform(spatdat, 4326) # WGS EPSG
  }
  
  if (is_invalid_spat(spatdat)) {
    
    spatdat <- clean_spat(spatdat)
  } 
  
  spatdat
}