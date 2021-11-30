check_spatdat <- function(spatdat, lon = NULL, lat = NULL, id = NULL) {
  #' Check that spatial data is in correct format
  #' 
  #' Converts spatial data to a \code{sf} object
  #' 
  #' @param spatdat Spatial data containing information on fishery management or 
  #'   regulatory zones. 
  #' @param lon Longitude variable in \code{dat}.
  #' @param lat Latitude variable in \code{dat}.
  #' @param id Polygon ID column. Required for csv files.
  #' @export
  #' @importFrom sf st_as_sf st_cast st_is_valid st_make_valid
  #' @importFrom dplyr group_by across summarize
  #' @importFrom magrittr %>%
  
  pass <- TRUE

  if (!("sf" %in% class(spatdat))) {
    
    if ("sp" %in% class(spatdat)) spatdat <- sf::st_as_sf(spatdat)
    
    else {
      
      if (!is.null(lon) & !is.null(lon) & !is.null(lon)) {
        
        # convert dataframe to sf
        spatdat <- sf::st_as_sf(x = spatdat, coords = c(lon, lat), 
                                crs = "+proj=longlat +datum=WGS84")
        
        # Convert point geometry to polygon
        spatdat <- 
          spatdat %>%
          dplyr::group_by(dplyr::across(id)) %>% 
          dplyr::summarize(do_union = FALSE) %>% 
          sf::st_cast("POLYGON")
        
        # convert polygon to multipolygon
        spatdat <- sf::st_cast(spatdat, "MULTIPOLYGON")
        
      } else {
        
        warning("Arguments \"lon\", \"lat\", and \"id\" are needed to convert ",
        "spatial data to sf.")
        pass <- FALSE
      }
    }
  }
  
  if (pass) {
    
    if (any(!(sf::st_is_valid(spatdat)))) {
      
      spatdat <- sf::st_make_valid(spatdat)
    } 
  }
 
  spatdat
}