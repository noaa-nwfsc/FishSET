make_polygon <- function(coord) {
  #' create a polygon
  #' 
  #' @param coord Longitude and latitude coordinates forming a polygon. Can be a 
  #'    numeric vector of even length or a numeric matrix with two columns.
  #' @keywords internal
  #' @importFrom purrr is_bare_numeric
  #' @import sf
  #' @details If the coordinates create an open polygon the function automatically
  #' closes it and gives a warning. 
  
  # coord should be a vector or matrix 
  # must be even and last set of coords must match 
  # (i.e. close the polygon). 

  if (!purrr::is_bare_numeric(coord)) {
    
    stop("Coordinates must be a numeric vector or matrix.")
  }
  
  if (is.matrix(coord)) c_len <- length(as.numeric(coord))
  else c_len <- length(coord)
  

  even <- c_len %% 2 == 0
  
  if (!even) {
    
    stop("Missing coordinate value. Check for missing lon/lat.")
  }
  # check that polygon is closed 
  
  if (!is.matrix(coord)) coord <- matrix(coord, ncol = 2, byrow = TRUE)
  
  n <- nrow(coord)
  first <- coord[1, ]
  last <- coord[n, ]
  
  if (!identical(first, last)) {
    
    coord <- rbind(coord, first)
    warning("Polygon must be closed. Closing polygon.")
  }
  # output is sfg class (simple feature geometry)
  new_poly <- sf::st_polygon(list(coord))
  
  if (!sf::st_is_valid(new_poly)) {
    
    new_poly <- sf::st_make_valid(new_poly)
  }
  
  new_poly
}


add_polygon <- function(poly, spat, spat.id, new.id = NULL, combine = FALSE) {
  #' Add polygon to spatial data
  #' 
  #' @param poly A valid polygon to add to spatial data.
  #' @param spat Spatial dataset to add polygon too.
  #' @param spat.id The ID column in \code{spat}.
  #' @param new.id The ID for new polygon. 
  #' @param combine Whether to use \code{\link{combine_zone}}. This will turn 
  #'   the intersections between \code{poly} and \code{spat} into new polygons. 
  #'   Note that the new polygon IDs will be derived from \code{spat} and \code{new.id} 
  #'   will not be used. 
  #' @seealso \code{\link{combine_zone}}
  #' @keywords internal
  #' @import sf
  #' @importFrom dplyr bind_rows rename_with
  #' @importFrom tibble tibble
 
  # need to consider whether to allow multiple polygons (not multipolygons)
  # could loop through each.
  
  if (!sf::st_is(poly, c("POLYGON", "MULTIPOLYGON"))) {
    
    stop("Object 'poly' must be a polygon.")
  }
  
  if (is_value_empty(new.id) & !combine) {
    
    stop("'new.id' required.")
  }
  
  if (!is_value_empty(new.id) & !combine) {
    
    if (new.id %in% spat[[spat.id]]) {
      
      stop("'new.id' is not unique. Enter a new ID.")
    }
  }
  
  # check for mis-matched ID classes
  if (class(new.id) != class(spat[[spat.id]])) {
    
    stop("'new.id' does not match 'spat.id' class (", class(spat[[spat.id]]), ").")
  }

  # convert to sfc (simple feature geometry list column)
  poly <- sf::st_sfc(poly, crs = sf::st_crs(spat))
  
  # check if spat has shifted longitude and updated coord if necessary
  if (shift_long(spat)) {
    
    poly <- sf::st_shift_longitude(poly)
    warning("Shifted longitude detected in spatial table, adjusting coordinates.")
  }
  
  if (combine) {
    
    inter <- lengths(sf::st_intersects(poly, spat))
    
    if (sum(inter) == 0) { # new poly doesn't intersect
      
      if (is_value_empty(new.id)) {
        
        stop("'new.id' required: new polygon does not intersect with 'spat'.")
      
      } else {
        
        if (new.id %in% spat[[spat.id]]) {
          
          stop("'new.id' is not unique. Enter a new ID.")
        }
      }
      
    } else { # new poly intersects
      
      if (!is.null(new.id)) {
        
        warning("New polygon intersects 'spat', 'new.id' ignored.")
      }
    }
  }
  
  # covert to sf, add new id 
  # note: geometry column may be named something else, like "geom" and
  # new spat will have two geometry cols
  poly <- sf::st_sf(tibble::tibble("temporary_col_name" = new.id, geometry = poly)) %>%
    dplyr::rename_with(~ spat.id, .cols = "temporary_col_name")
  
  # combine 
  if (combine && sum(inter) > 0) {
    
    spat_out <- combine_zone(spat, poly, spat.id, spat.id)
  
  } else {
    
    spat_out <- dplyr::bind_rows(spat, poly)
  }
  
  spat_out
}


make_spat_area <- function(spat, project, coord, spat.id, new.id, combine) {
#' Add an area/polygon to spatial data
#' 
#' @param spat Spatial dataset to add polygon too.
#' @param project Name of project. 
#' @param coord Longitude and latitude coordinates forming a polygon. Can be a 
#'    numeric vector of even length or a numeric matrix with two columns.
#' @param spat.id The ID column in \code{spat}
#' @param new.id The ID for new polygon. 
#' @param combine Whether to use \code{\link{combine_zone}}. This will turn 
#'   the intersections between \code{poly} and \code{spat} into new polygons. 
#'   Note that the new polygon IDs will be derived from \code{spat} and \code{new.id} 
#'   will not be used. 
#' @export
#' @details Adds an area/polygone to a spatial area
#' @seealso \code{\link{make_polygon}} \code{\link{add_polygon}}
  
  out <- data_pull(spat, project)
  spatdat <- out$dataset
  spat <- parse_data_name(spat, "spat", project)
  
  new_poly <- make_polygon(coord)
  
  spatdat <- add_polygon(new_poly, spatdat, spat.id, new.id, combine)
  
  # log function
  make_spat_area_function <- list()
  make_spat_area_function$functionID <- "make_spat_area"
  make_spat_area_function$args <- list(spat, project, coord, spat.id, new.id, combine)
  log_call
  
  spatdat
}

