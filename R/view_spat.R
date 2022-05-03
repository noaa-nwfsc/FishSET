plot_spat <- function(spat) {
  #' Plot spatial dataset
  #' 
  #' Simple plotting function for viewing spatial data.
  #' 
  #' @param spat Spatial dataset to view. Must be an object of class \code{sf} 
  #'   or \code{sfc}. 
  #' @importFrom ggplot2 ggplot geom_sf
  #' @export
  #' @examples 
  #' \dontrun{
  #' plot_spat(pollockNMFSSpatTable)
  #' }
  
  if (!inherits(spat, c("sf", "sfc"))) {
    
    stop("'spat' must be a sf object.")
  }
  
  ggplot2::ggplot(data = spat) + ggplot2::geom_sf() + fishset_theme()
}


view_spat <- function(spat, id = NULL) {
  #' View interactive map of spatial data
  #' 
  #' @param spat Spatial dataset to view. Must be an object of class \code{sf} 
  #'   or \code{sfc}. 
  #' @param id Optional, name of spatial ID column to view with spatial data.
  #' @export
  #' @import leaflet
  #' @importFrom sf st_transform
  #' @importFrom dplyr %>% 
  #' @examples 
  #' \dontrun{
  #' view_spat(pollockNMFSSpatTable, id = "NMFS_AREA")
  #' }
  
  if (!inherits(spat, c("sf", "sfc"))) {
    
    stop("'spat' must be a sf object.")
  }
  
  if (!is.null(id)) {
    
    column_check(spat, id)
    
    spat_ids <- spat[[id]]
    
  } else spat_ids <- NULL
  
  spat <- sf::st_transform(spat, crs = 4326)
  
  leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addPolygons(data = spat,
                         fillColor = "white",
                         fillOpacity = 0.5,
                         color = "black",
                         stroke = TRUE,
                         weight = 1,
                         layerId = spat_ids,
                         label = spat_ids)
}