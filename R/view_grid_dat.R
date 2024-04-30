view_grid_dat <- function(grid, project, lon, lat, value, split_by = NULL, 
                          group = NULL, agg_fun = "mean") {
  #' Visualize gridded data on a map
  #' 
  #' 
  #' @param grid Gridded data table to visualize. Use string if visualizing 
  #'   a gridded data table in the FishSET Database. 
  #' @param project String, project name. 
  #' @param lon String, variable name containing longitude.
  #' @param lat String, variable name containing latitude.
  #' @param value String, variable name containing gridded values, e.g. sea surface 
  #'   temperature, wind speed, etc. 
  #' @param split_by String, variable in gridded data table to split by. 
  #' @param group String, variable in gridded data table to group \code{value} by.
  #'   In addition to the variable(s) in \code{group}, \code{value} is also 
  #'   aggregated by each longitude-latitude pair. The string \code{"lonlat"} is 
  #'   a shortcut for \code{group = c("lon", "lat")} which aggregates the \code{value} 
  #'   for each longitude-latitude pair across the entire dataset.
  #' @param agg_fun Aggregating function applied to \code{group}. Defaults to mean.
  #' @export
  #' @import ggplot2
  #' @importFrom rlang sym
  #'

  out <- data_pull(grid, project)
  grid_dat <- out$dataset
  grid <- parse_data_name(grid, "grid", project)
  
  # make bounding box
  bbox <- bbox(grid_dat, lon = lon, lat = lat)
  
  xlim <- c(bbox["left"], bbox["right"])
  ylim <- c(bbox["bottom"], bbox["top"])
  
  base_map <- ggplot2::map_data("world", xlim = xlim, ylim = ylim)
  
  base_map <- dat_to_sf(base_map, lon = "long", lat = "lat", id = "group", 
                        cast = "POLYGON", multi = TRUE)
  
  # remove NA values
  grid_dat <- grid_dat[!is.na(grid_dat[[value]]), ]
  
  # Shiny app 
  if (!is.null(split_by)) if (split_by == "none") split_by <- NULL
  
  # aggregate 
  if (!is.null(group)) {
    
    if (group == "lonlat") group <- c(lon, lat)
    
    if (!is.function(agg_fun)) {
      
      if (is.character(agg_fun)) {
        
        agg_fun <- match.fun(agg_fun)
      }
    }
    
    grp <- unique(c(lon, lat, group, split_by))
    
    grid_dat <- agg_helper(grid_dat, value = value, group = grp, fun = agg_fun)
  }
  
  # plot functions 
  lon_sym <- rlang::sym(lon)
  lat_sym <- rlang::sym(lat)
  val_sym <- rlang::sym(value)
  
  map_out <- 
    ggplot2::ggplot(data = grid_dat) +
    ggplot2::geom_sf(data = base_map) + 
    ggplot2::geom_raster(ggplot2::aes(x = !!lon_sym, y = !!lat_sym, fill = !!val_sym),
                         interpolate = FALSE, 
                         na.rm = TRUE,
                         alpha = .85) + # slight transparency for maps feature visibility
    ggplot2::coord_sf(xlim = xlim, ylim = ylim, expand = TRUE) +
    fishset_theme() + 
    ggplot2::labs(x = "longitude", y = "latitude") +
   ggplot2::scale_fill_gradientn(colors = fishset_viridis(10), na.value = NA)
  
  # check # of unique values in split_by, if high use facet_wrap
  if (!is.null(split_by)) {
    
    if (length(unique(grid_dat[[split_by]])) > 3) {
      
      map_out <- map_out + ggplot2::facet_wrap(split_by)
    
    } else {
    
      fm <- paste(split_by, "~ .")
      
      # map_out <- map_out + ggplot2::facet_grid_dat(fm)
      map_out <- map_out + ggplot2::facet_grid(fm)
    }
  }
  
  save_plot(project, "view_grid_dat", map_out)
  
  # log function
  view_grid_dat_function <- list()
  view_grid_dat_function$functionID <- "view_grid_dat"
  view_grid_dat_function$args <- list(grid, project, lon, lat, value, 
                                      split_by, group, agg_fun)
  log_call(project, view_grid_dat_function)
  
  map_out
}

