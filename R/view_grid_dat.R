


view_grid_dat <- function(gridfile, project, lon, lat, value, split_by = NULL, agg_by = NULL, gmap = FALSE) {
  #' Visualize gridded data
  #' 
  #' 
  #' @param gridfile Gridded data table to visualize. Use string if visualizing a gridded data
  #'   table in the FishSET Database. 
  #' @param project String, project name. 
  #' @param lon String, variable name containing longitude.
  #' @param lat String, variable name containing latitude.
  #' @param value String, variable name containing gridded values, e.g. sea surface 
  #'   temperature, wind speed, etc. 
  #' @param split_by String, variable in gridded data table to split by. 
  #' @param agg_by String, variable in gridded data table to group "value" by. By default,
  #'   the mean is aggregated. The string "latlon" is a shortcut for \code{agg_by = c("lon", "lat")}
  #'   which aggregates the "value" for each latitude-longitude pair across the entire dataset. 
  #' @param gmap A ggmap object to be passed to \code{\link[ggmap]{ggmap}}. If FALSE, 
  #'   then a ggmap is automatically retrieved. Defaults to FALSE. 
  #' @export
  #' @import ggplot2
  #' @import ggmap
  #' @import dplyr 
  #' @examples 
  #' \dontrun{
  #' view_grid_dat(SST, "pollock", "lon", "lat", value = "analysed_sst")
  #' }
  #' 
  out <- data_pull(gridfile)
  grid <- out$dataset
  
  if (shiny::isRunning())
    if (deparse(substitute(gridfile)) == "grddat$dataset") gridfile <- get("grid_name")
    
   else 
     if (!is.character(gridfile)) gridfile <- deparse(substitute(gridfile))
  
  data(colors) # load in colors.RData in Data/
  
  # make bounding box
  bbox <- ggmap::make_bbox(lon, lat, grid)
  
  xlim <- c(bbox["left"], bbox["right"])
  ylim <- c(bbox["bottom"], bbox["top"])
  
  if (is.logical(gmap)) { # Check if existing ggmap is present
    
    grid_map <- retrieve_map(grid, lon, lat)

  } else {
    
    grid_map <- gmap
    gmap <- deparse(substitute(gmap))
  }
  
  # remove NA values
  grid <- grid[!is.na(grid[[value]]), ]
  
  # Shiny app 
  if (!is.null(split_by)) if (split_by == "none") split_by <- NULL
  
  # aggregate 
  if (!is.null(agg_by)) {
    
    if (agg_by == "latlon") agg_by <- c(lon, lat)
    
    grid <- 
      grid %>% 
        dplyr::group_by(dplyr::across(agg_by)) %>% 
        dplyr::mutate(dplyr::across(value, mean, na.rm = TRUE))
  }
  
  map_color <- colors$temperature
  
  map_out <- 
    ggmap::ggmap(grid_map) +
    ggplot2::geom_raster(data = grid, 
                         ggplot2::aes_string(x = lon, y = lat, fill = value),
                         interpolate = FALSE, 
                         na.rm = TRUE,
                         alpha = .85) + # slight transparency for map feature visibility
    ggplot2::coord_fixed(ratio = 1.5, xlim = xlim, ylim = ylim) +
    FishSET:::fishset_theme + 
    ggplot2::labs(x = "longitude", y = "latitude") +
    ggplot2::scale_fill_gradientn(colors = map_color, na.value = NA) 
  
  # check # of unique values in split_by, if high use facet_wrap
  if (!is.null(split_by)) {
    
    if (length(unique(grid[[split_by]])) > 3) {
      
      map_out <- map_out + ggplot2::facet_wrap(split_by)
    
    } else {
    
      fm <- paste(split_by, "~ .")
      
      map_out <- map_out + ggplot2::facet_grid(fm)
    }
  }
  
  save_plot(project, "view_grid_dat", map_out)
  
  # log function
  view_grid_dat_function <- list()
  view_grid_dat_function$functionID <- "view_grid_dat"
  view_grid_dat_function$args <- list(gridfile, project, lon, lat, value, split_by, agg_by, gmap)
  log_call(view_grid_dat_function)
  
  map_out
}

retrieve_map <- function(grid, lon, lat) {
  #' Get stamen map 
  #' 
  #' @param grid Gridded data table to visualize. Use string if visualizing a gridded data
  #'   table in the FishSET Database. 
  #' @param lon String, variable name containing longitude.
  #' @param lat String, variable name containing latitude.
  #' @import ggmap
  #' @export
  #' @details This is a wrapper for \code{\link[ggmap]{get_stamenmap}}
  
  # make bounding box
  bbox <- ggmap::make_bbox(lon, lat, grid)
  
  # Find ideal zoom level
  zm <- ggmap::calc_zoom(lon, lat, grid)
  
  grid_map <- ggmap::get_stamenmap(bbox = bbox, 
                                   zoom = zm,
                                   maptype = "toner-lite",
                                   crop = FALSE)
  
  grid_map
}