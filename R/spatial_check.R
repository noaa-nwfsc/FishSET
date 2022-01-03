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
  #' found, and if any empty geometries are detected. 
  
  any(sf::st_is(spat, "GEOMETRYCOLLECTION")) ||
    any(!sf::st_is(spat, c("MULTIPOLYGON", "POLYGON"))) ||
    any(!sf::st_is_valid(spat)) ||
    any(is.na(sf::st_is_valid(spat))) || 
    any(sf::st_is_empty(spat))   
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
  
  if (any(sf::st_is(spat, "GEOMETRYCOLLECTION"))) {
    
    spat <- sf::st_collection_extract(spat, "POLYGON")
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
  
  if (shift_long(spat)) {
    
    spat <- sf::st_shift_longitude(spat)
  }
  
  spat
}


check_spatdat <- function(spatdat, lon = NULL, lat = NULL, id = NULL) {
  #' Check and correct spatial data format
  #' 
  #' Converts spatial data to a \code{sf} object
  #' 
  #' @param spatdat Spatial data containing information on fishery management or 
  #'   regulatory zones. 
  #' @param lon Longitude variable in \code{dat}. This is required for csv files 
  #' or if \code{spatdat} is a dataframe (i.e. is not a \code{sf} or \code{sp} object).
  #' @param lat Latitude variable in \code{dat}. This is required for csv files 
  #' or if \code{spatdat} is a dataframe (i.e. is not a \code{sf} or \code{sp} object).
  #' @param id Polygon ID column. This is required for csv files or if \code{spatdat} 
  #' is a dataframe (i.e. is not a \code{sf} or \code{sp} object).
  #' @export
  #' @details This function checks whether \code{spatdat} is a \code{sf} object
  #'   and attempts to convert it if not. It also applies \code{\link{clean_spat}}
  #'   which fixes certain spatial issues such as invalid or empty polygons. 
  #' @import sf 
  #' @importFrom dplyr group_by across summarize
  #' @importFrom magrittr %>%
  
  pass <- TRUE

  if (!("sf" %in% class(spatdat))) {
    
    if ("sp" %in% class(spatdat)) spatdat <- sf::st_as_sf(spatdat)
    
    else {
      
      if (!is.null(lon) & !is.null(lat) & !is.null(id)) {
        
        # convert dataframe to sf
        spatdat <- sf::st_as_sf(x = spatdat, coords = c(lon, lat), 
                                crs = "+proj=longlat +datum=WGS84")
        
        # Convert point geometry to polygon
        spatdat <- 
          spatdat %>%
          dplyr::group_by(dplyr::across(id)) %>% 
          dplyr::summarize(do_union = FALSE) %>% 
          sf::st_cast("POLYGON")
        
        # convert polygon to multi-polygon
        spatdat <- sf::st_cast(spatdat, "MULTIPOLYGON")
        
      } else {
        
        warning("Arguments \"lon\", \"lat\", and \"id\" are needed to convert ",
        "spatial data to sf.")
        pass <- FALSE
      }
    }
  }
  
  if (pass) {
    
    if (any(grepl('PROJCRS',  sf::st_crs(spatdat)))) {
      
      spatdat <- sf::st_transform(spatdat, "+proj=longlat +ellps=WGS84 +datum=WGS84")
    }
    
    if (shift_long(spatdat)) {
      
      spatdat <- sf::st_shift_longitude(spatdat)
    }
    
    if (is_invalid_spat(spatdat)) {
      
      spatdat <- clean_spat(spatdat)
    } 
  }
 
  spatdat
}