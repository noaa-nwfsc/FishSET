#' Visualize gridded data
#' 
#' 
#' @param grid Gridded data table to visualize. Use string if visualizing a gridded data
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
#'   date 
#' @export
#' @import ggplot2
#' @import ggmap
#' @import dplyr 
#' @examples 
#' \dontrun{
#' view_grid_dat(SST, "pollock", "lon", "lat", value = "analysed_sst")
#' }


view_grid_dat <- function(grid, project, lon, lat, value, split_by = NULL, agg_by = NULL) {
  
  out <- data_pull(grid)
  dat <- out$dat
  grid <- out$dataset
  
  # make bounding box
  bbox <- ggmap::make_bbox(lon, lat, grid)
  
  xlim <- c(bbox["left"], bbox["right"])
  ylim <- c(bbox["bottom"], bbox["top"])
  
  # Find ideal zoom level
  zm <- ggmap::calc_zoom(lon, lat, grid)
  
  # remove NA values
  grid <- grid[!is.na(grid[[value]]), ]
  
  # aggregate 
  if (!is.null(agg_by)) {
    
    if (agg_by == "latlon") agg_by <- c(lon, lat)
    
    grid <- 
      grid %>% 
        dplyr::group_by(dplyr::across(agg_by)) %>% 
        dplyr::mutate(dplyr::across(value, mean, na.rm = TRUE))
  }
  
  map_color <- colors$temperature # (from rerddap)
  
  grid_map <- ggmap::get_stamenmap(bbox = bbox, 
                                   zoom = zm,
                                   maptype = "toner-lite",
                                   crop = FALSE)
  
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
  view_grid_dat_function$args <- list(grid, project, lon, lat, split_by, agg_by)
  log_call(view_grid_dat_function)
  
  map_out
}