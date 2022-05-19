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


view_spat <- function(spat, id = NULL, type = "polygon") {
  #' View interactive map of spatial data
  #' 
  #' @param spat Spatial dataset to view. Must be an object of class \code{sf} 
  #'   or \code{sfc}. 
  #' @param id Optional, name of spatial ID column to view with spatial data.
  #' @param type Can be \code{"polygon"}, \code{"line"}, or \code{"point"}.
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
  
  leaf_map <- leaflet::leaflet() %>% leaflet::addTiles() 
    
  if (type == "polygon") {
    
    leaf_map %>% 
    leaflet::addPolygons(data = spat,
                         fillColor = "white",
                         fillOpacity = 0.5,
                         color = "black",
                         stroke = TRUE,
                         weight = 1,
                         layerId = spat_ids,
                         label = spat_ids)
    
  } else if (type == "line") {
    
    leaf_map %>% 
    leaflet::addPolylines(data = spat,
                          fillColor = "white",
                          fillOpacity = 0.5,
                          color = "black",
                          stroke = TRUE,
                          layerId = spat_ids,
                          label = spat_ids)
    
  } else if (type == "point") {
    
    leaf_map %>% 
    leaflet::addCircleMarkers(data = spat,
                              fillColor = "white",
                              fillOpacity = 0.5,
                              color = "black",
                              stroke = TRUE,
                              weight = 1,
                              layerId = spat_ids,
                              label = spat_ids)
  }
}


view_lon_lat <- function(dat, lon, lat, id = NULL, crs = 4326) {
  #' View 
  #' 
  #' @param dat Data containing \code{lon} and \code{lat} columns. 
  #' @param lon Name of Longitude column.
  #' @param lat Name of Lattitude column.
  #' @param id Optional, name of an ID variable that is paired with \code{lon} and 
  #'   \code{lat} columns
  #' @param crs Optional, coordinate reference system to use. Defaults to EPSG code
  #'   4326 (WGS 84). 
  #' @export
  #' @importFrom rlang sym
  #' @importFrom dplyr mutate
  #' @importFrom leaflet addCircleMarkers
  #' @importFrom htmltools HTML
  
  
  if (!inherits(dat, c("sf", "sfc"))) {
    
    column_check(dat, cols = c(lon, lat, id))
    
    id_sym <- function() if (!is.null(id)) rlang::sym(id) else NULL
    lon_sym <- rlang::sym(lon)
    lat_sym <- rlang::sym(lat)
    
    dat <- dat %>% 
      dplyr::mutate(lonlat_lab = paste0(!!id_sym(), 
                                        "<br/>lon: ", round(!!lon_sym, 5), 
                                        "<br/>lat: ", round(!!lat_sym, 5)))
    
    dat <- dat_to_sf(dat, lon = lon, lat = lat, 
                     id = id, crs = crs, cast = "POINT")
    
    spat_id <- if (!is.null(id)) dat[[id]] else NULL
    
    view_spat(dat, id = id, type = "point") %>% 
      leaflet::addCircleMarkers(data = dat,
                                fillColor = "white",
                                fillOpacity = 0.5,
                                color = "black",
                                stroke = TRUE,
                                weight = 1,
                                layerId = spat_id,
                                label = ~lapply(lonlat_lab, htmltools::HTML))
  } else {
    
    view_spat(dat, id = id, type = "point")
  }
}