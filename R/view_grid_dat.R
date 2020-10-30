


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
  #
  
  #color gradient for mapping
  map_color <- c("#0B222E", "#0B2330", "#0B2432", "#0A2533", "#0A2635", "#0A2637", "#092739", "#09283B", "#09293C",
                 "#08293E", "#082A40", "#072B42", "#072C44", "#062C46", "#062D48", "#052E4A", "#052F4D", "#042F4F",
                 "#043051", "#033153", "#033155", "#023258", "#02335A", "#01335C", "#01345F", "#013461", "#013564",
                 "#013566", "#013669", "#02366B", "#03376E", "#043770", "#053773", "#083875", "#0A3878", "#0D387A",
                 "#10387C", "#13397E", "#163980", "#193982", "#1C3983", "#1F3984", "#223A85", "#253A86", "#273A86",
                 "#2A3B87", "#2C3B87", "#2F3B87", "#313C88", "#333C88", "#353D88", "#373D88", "#393E88", "#3B3E87",
                 "#3D3F87", "#3F3F87", "#414087", "#424087", "#444187", "#464186", "#474286", "#494286", "#4B4386",
                 "#4C4486", "#4E4486", "#4F4585", "#514585", "#524685", "#544685", "#554785", "#574784", "#584884",
                 "#5A4984", "#5B4984", "#5D4A84", "#5E4A84", "#5F4B84", "#614B84", "#624C84", "#644C83", "#654D83",
                 "#674D83", "#684E83", "#694E83", "#6B4F83", "#6C4F83", "#6D5083", "#6F5183", "#705183", "#725283",
                 "#735283", "#745383", "#765383", "#775383", "#795483", "#7A5483", "#7C5583", "#7D5583", "#7E5683",
                 "#805683", "#815782", "#835782", "#845882", "#865882", "#875882", "#895982", "#8A5982", "#8C5A82",
                 "#8D5A82", "#8F5B82", "#905B82", "#925B81", "#935C81", "#955C81", "#965D81", "#985D81", "#995D80",
                 "#9B5E80", "#9D5E80", "#9E5E7F", "#A05F7F", "#A15F7F", "#A35F7E", "#A5607E", "#A6607E", "#A8607D",
                 "#A9617D", "#AB617C", "#AD627C", "#AE627B", "#B0627B", "#B2637A", "#B3637A", "#B56379", "#B66478",
                 "#B86478", "#BA6477", "#BB6576", "#BD6576", "#BF6575", "#C06674", "#C26673", "#C36672", "#C56771",
                 "#C76771", "#C86870", "#CA686F", "#CB686E", "#CD696D", "#CF696C", "#D06A6B", "#D26A6A", "#D36B68",
                 "#D56B67", "#D66C66", "#D86C65", "#D96D64", "#DB6D63", "#DC6E62", "#DD6E60", "#DF6F5F", "#E0705E",
                 "#E2705D", "#E3715B", "#E4725A", "#E57259", "#E77357", "#E87456", "#E97555", "#EA7654", "#EB7752",
                 "#EC7851", "#ED7950", "#EE7A4F", "#EF7B4D", "#F07C4C", "#F17D4B", "#F27E4A", "#F27F49", "#F38048",
                 "#F48247", "#F48346", "#F58445", "#F58644", "#F68743", "#F68843", "#F78A42", "#F78B41", "#F88D41",
                 "#F88E40", "#F8903F", "#F9913F", "#F9933F", "#F9943E", "#F9963E", "#FA973E", "#FA993E", "#FA9A3D",
                 "#FA9C3D", "#FA9D3D", "#FA9F3D", "#FAA13D", "#FAA23D", "#FAA43E", "#FAA53E", "#FAA73E", "#FAA93E",
                 "#FAAA3F", "#FAAC3F", "#FAAD40", "#FAAF40", "#FAB140", "#FAB241", "#F9B442", "#F9B642", "#F9B743",
                 "#F9B943", "#F9BB44", "#F8BC45", "#F8BE46", "#F8C046", "#F8C147", "#F7C348", "#F7C449", "#F7C64A",
                 "#F6C84A", "#F6C94B", "#F6CB4C", "#F5CD4D", "#F5CE4E", "#F4D04F", "#F4D250", "#F4D351", "#F3D552",
                 "#F3D753", "#F2D854", "#F2DA55", "#F1DC56", "#F1DD57", "#F0DF58", "#F0E159", "#EFE35A", "#EFE45C",
                 "#EEE65D", "#EDE85E", "#EDE95F", "#ECEB60", "#EBED61", "#EBEE62", "#EAF063", "#E9F264", "#E9F366",
                 "#E8F567", "#E7F768", "#E6F969", "#E5FA6A")
  
  out <- data_pull(gridfile)
  grid <- out$dataset
  
  if (shiny::isRunning())
    if (deparse(substitute(gridfile)) == "grddat$dataset") gridfile <- get("grid_name")
    
   else 
     if (!is.character(gridfile)) gridfile <- deparse(substitute(gridfile))
  
#  data(colors) # load in colors.RData in Data/
  
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
  
 
  
  map_out <- 
    ggmap::ggmap(grid_map) +
    ggplot2::geom_raster(data = grid, 
                         ggplot2::aes_string(x = lon, y = lat, fill = value),
                         interpolate = FALSE, 
                         na.rm = TRUE,
                         alpha = .85) + # slight transparency for maps feature visibility
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