recast_multipoly <- function(grid, closure, combined, id) {
  #' Recast multi-polygons
  #' 
  #' Re-cast intersecting multi-polygons to polygons. Used when combining 
  #' regulatory zones with closure areas. 
  #' 
  #' @param grid Grid file containing regulatory zones.
  #' @param closure Closure file containing closure areas. 
  #' @param combined Combined version of grid and closure file.
  #' @param id Character, the name of the zone ID column. 
  #' @keywords internal
  #' @export
  #' @importFrom sf st_intersects st_is st_cast
  #' @importFrom  dplyr filter bind_rows
  #' @importFrom magrittr %>% 
  #' @details This function is primarily used to extract polygons during 
  #'   the ID re-labeling process performed by \code{\link{new_zone_id}}. If
  #'   a polygon from \code{grid} intersected with \code{closure}, it is extracted
  #'   from it's multi-polygon and given a unique ID. 
  #'   
  #' @seealso \code{\link{combine_zone}} \code{\link{new_zone_id}}
  
  grid <- check_spatdat(grid)
  closure <- check_spatdat(closure)
  
  grid_inter <- sf::st_intersects(grid, closure)
  
  ind <- lengths(grid_inter) > 0
  # zones that intersect closures
  inter_nm <- grid[ind, ][[id]]
  
  if (is_invalid_spat(combined)) combined <- clean_spat(combined)
  # check if was originally MP? 
  is_multi <- sf::st_is(combined, type = "MULTIPOLYGON")
  is_inter <- combined[[id]] %in% inter_nm
  cast <- is_multi & is_inter
  
  # convert to single polygons
  inter_cast <- combined %>% 
    dplyr::filter(cast) %>% 
    sf::st_cast("POLYGON")
  
  combined <- combined[!cast, ]
  combined <- dplyr::bind_rows(combined, inter_cast)
  
  combined
}


new_zone_id <- function(combined, id, grid = NULL, closure = NULL, 
                        recast = TRUE) {
  #' Create new zone IDs
  #' 
  #' Creates a new zone ID column for the combined zone file. Used when combining
  #' regulatory zones with closure areas. 
  #' 
  #' @param combined Combined version of grid and closure file.
  #' @param id Character, the name of the zone ID column. 
  #' @param grid Grid file containing regulatory zones.
  #' @param closure Closure file containing closure areas. 
  #' @param recast Logical, if \code{TRUE} \code{combined} is passed to 
  #'   \code{\link{recast_multipoly}}.
  #' @keywords internal
  #' @export
  #' @import dplyr
  #' @importFrom magrittr %>% 
  #' @importFrom purrr pmap
  #' @importFrom  sf st_set_geometry
  #' @details This function assigns a new zone ID if the intersection of \code{grid} 
  #'   and \code{closure} creates new, non-contiguous polygons. The ID naming 
  #'   convection is the original name of the regulatory zone followed by a 
  #'   decimal and a number. For example, if a closure area bi-sects regulatory 
  #'   zone A, the resulting zone IDs would be "A.1" and "A.2".  
  #' @seealso \code{\link{recast_multipoly}} \code{\link{combine_zone}}
  
  n_count <- c()
  
  if (recast) {
    
    combined <- recast_multipoly(grid = grid, closure = closure, 
                                 combined = combined, id = id)
  }
  
  id_count <- 
    combined %>% 
    sf::st_set_geometry(value = NULL) %>% 
    dplyr::mutate(n_count = 1) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(id))) %>% 
    dplyr::mutate(n_count = cumsum(n_count)) %>% 
    dplyr::ungroup()
  
  # find split (duplicate) zone ids  
  dupID <- 
    id_count %>%
    dplyr::count(dplyr::across(dplyr::all_of(id))) %>% 
    dplyr::filter(n > 1) %>% 
    dplyr::pull(dplyr::all_of(id))
  
  id_count$dupID <- id_count[[id]] %in% dupID
  
  new_ids <- 
    purrr::pmap(list(x = id_count[[id]], 
                     y = id_count$n_count, 
                     z = id_count$dupID), 
                
                function(x, y, z) {
                  
                  if (z) paste0(x, ".", y)
                  else x
                })
  
  combined[[id]] <- unlist(new_ids)
  
  combined
}


combine_zone <- function(grid, closure, grid.nm, closure.nm, recast = TRUE) {
  #' Combine zone and closure area
  #' 
  #' Creates a new spatial dataset that merges regulatory zones with closure areas.
  #' 
  #' @param grid Grid file containing regulatory zones.
  #' @param closure Closure file containing closure areas. 
  #' @param grid.nm Character, column name containing grid ID.
  #' @param closure.nm Character, column name containing closure ID.
  #' @param recast Logical, if \code{TRUE} \code{combined} is passed to 
  #'   \code{recast_multipoly}.
  #' @export
  #' @importFrom sf st_crs st_snap st_make_valid st_union st_difference st_intersection
  #' @importFrom dplyr bind_rows
  #' @importFrom magrittr %>% 
  #' @details To combine zones with closure areas, this function performs the 
  #'   following steps:
  #'   \enumerate{
  #'     \item Create the union of the closure area
  #'     \item Take the difference between the closure union and the zone file
  #'     \item Take the intersection of zone and the closure union 
  #'     \item Combine the difference and intersection objects into one spatial 
  #'       dataframe
  #'     \item Assign new zone IDs to intersecting polygons
  #'   } 
  #'   The result is a single spatial dataset containing all polygons from both \code{grid}
  #'   and \code{closure} with overlapping (intersecting) polygons receiving new
  #'   IDs (see \code{\link{new_zone_id}}). This allows users to partially close
  #'   regulatory zones during the model design stage. 
  #' @seealso \code{\link{recast_multipoly}} \code{\link{new_zone_id}}
  
  grid <- check_spatdat(grid)
  closure <- check_spatdat(closure)
  
  if (class(grid[[grid.nm]]) != class(closure[[closure.nm]])) {
    
    grid[[grid.nm]] <- as.character(grid[[grid.nm]])
    closure[[closure.nm]] <- as.character(closure[[closure.nm]])
  }
  
  # check if duplicate names between files
  if (any(closure[[closure.nm]] %in% grid[[grid.nm]])) {
    
    closure[[closure.nm]] <- paste("closure", closure[[closure.nm]])
  }
  
  # rename closure column
  if (grid.nm != closure.nm) {
    
    names(closure)[names(closure) == closure.nm] <- grid.nm
  }
  
  # # snap closure
  close_crs <- sf::st_crs(closure)
  sf::st_crs(closure) <- NA

  closure <-
    sf::st_snap(closure, closure, tolerance = 0.0001) %>%
    sf::st_make_valid()
  sf::st_crs(closure) <- close_crs

  if (is_invalid_spat(closure)) closure <- clean_spat(closure)

  # closure union
  c_un <- 
    sf::st_union(closure) %>% 
    sf::st_make_valid()
  
  if (is_invalid_spat(c_un)) c_un <- clean_spat(c_un)
  
  diff <- sf::st_difference(grid, c_un) %>% sf::st_make_valid()
  
  inter <- sf::st_intersection(grid, c_un) %>% sf::st_make_valid()
  out <- dplyr::bind_rows(diff, inter)
  
  if (is_invalid_spat(out)) out <- clean_spat(out)
  
  out <- new_zone_id(combined = out, id = grid.nm, grid, closure, 
                     recast = recast)
    
  if (is_invalid_spat(out)) out <- clean_spat(out)
  
  out
}