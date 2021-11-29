zone_freq_map <- function(dat, spat, project, lon.dat, lat.dat, zone.dat, 
                          zone.spat, breaks = NULL, n.breaks = 10, na.rm = TRUE) {
  
  #' Plots map of frequency of hauls by fishery management or regulatory zone 
  #' 
  #' \code{zone_freq_map} plots the number of observations in \code{dat} by 
  #' fishery management or regulatory zone.
  #' 
  #'@param dat Primary data containing information on hauls or trips. 
  #'  Table in FishSET database contains the string 'MainDataTable'.
  #'@param spat Spatial data containing information on fishery management or 
  #'   regulatory zones.
  #'@param project Name of project.
  #'@param lon.dat Name of longitude column in \code{dat}.
  #'@param lat.dat Name of latitude column in \code{dat}.
  #'@param zone.dat Name of zone ID column in \code{dat}.
  #'@param zone.spat Name of zone ID column in \code{spat}.
  #'@param breaks A numeric vector of breaks to bin zone frequencies by. Overrides
  #'  \code{n.breaks} if entered. 
  #'@param n.breaks The number of break points to create if breaks are not given 
  #'  directly. Defaults to 10. 
  #'@param na.rm Logical, whether to remove zones with zero counts. 
  #'@details Observations in \code{dat} must be assigned to regulatory zones to 
  #'  use this function. See \code{\link{assignment_column}} for details. 
  #'@export
  #'@import ggplot2
  #'@import dplyr
  #'@import sf
  #'@importFrom viridis viridis
  #'@examples 
  #'\dontrun{
  #'zone_freq_map(pollockMainTable, nmfs_area, "LonLat_START_LON", "LonLat_START_LAT",
  #'              zone.dat = "ZoneID", zone.spat = "NMFS_AREA")
  #'}
  
  # Call in datasets
  out <- data_pull(dat, project)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main")
  
  spatout <- data_pull(spat, project)
  spatdat <- spatout$dataset
  spat <- parse_data_name(spat, "spat")
  
  # Count zones
  zone_freq <- dataset %>% 
    dplyr::count(dplyr::across(zone.dat), sort = TRUE) %>% 
    dplyr::rename(n_obs = "n")
  
  by_vec <- zone.dat
  names(by_vec) <- zone.spat
  
  # merge spatdat w/ zone counts
  spatdat <- dplyr::left_join(spatdat, zone_freq, by = by_vec)
  
  
  # convert dat to sf object
  dat_sf <- sf::st_as_sf(x = dataset, coords = c(lon.dat, lat.dat), 
                         crs = "+proj=longlat +datum=WGS84")
  
  
  if (!is.na(sf::st_crs(spatdat))) {
    
    dat_sf <- sf::st_transform(dat_sf, sf::st_crs(spatdat))
    
  } else {
    
    spatdat <- sf::st_transform(spatdat, "+proj=longlat +datum=WGS84")
  }
  
  # base map ---- 
  bbox <- sf::st_bbox(dat_sf)
  
  base_map <- ggplot2::map_data("world", 
                                xlim = c(bbox["xmin"], bbox["xmax"]), 
                                ylim = c(bbox["ymin"], bbox["ymax"]))
  
  base_map <- sf::st_as_sf(base_map, coords = c("long", "lat"), 
                           crs = sf::st_crs(spatdat))
  
  # convert points to polygon
  base_map <- 
    base_map %>%
    dplyr::group_by(group) %>% 
    dplyr::summarize(do_union = FALSE) %>% 
    sf::st_cast("POLYGON")
  
  # plot functions 
  lon_sym <- rlang::sym(lon.dat)
  lat_sym <- rlang::sym(lat.dat)
  
  
  if (any(!(sf::st_is_valid(spatdat)))) {
    
    spatdat <- sf::st_make_valid(spatdat)
  } 
  
  # filter out zero counts
  spat_filtered <- spatdat[!is.na(spatdat$n_obs), ]
  
  if (!is.null(breaks)) {
    
    brks <- breaks 
    b_colors <- viridis::viridis(length(brks), option = "H")
    
  } else {
    
    brks <- pretty(zone_freq$n_obs, n = n.breaks)
    b_colors <- c("gray", viridis::viridis(length(brks) - 1, option = "H"))
    
    if (min(brks) == 0) {
      
      if (brks[2] > 10) brks[1] <- 10
      else  brks[1] <- round((brks[2]/2))
    }
  }

  
  if (na.rm) {
    
    z_plot <- 
      ggplot2::ggplot() + 
      ggplot2::geom_sf(data = base_map) +  
      ggplot2::geom_sf(data = spat_filtered, aes(fill = n_obs), 
                       color = "black", alpha = .8) +
      ggplot2::coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]),
                        expand = TRUE) +
      ggplot2::scale_fill_stepsn(breaks = brks, 
                                 colors = b_colors, 
                                 show.limits = TRUE,
                                 name = "# obs") +
      fishset_theme() +
      ggplot2::theme(legend.key.size = unit(1, "cm"))
    
  } else {
    
    z_plot <- 
      ggplot2::ggplot() +   
      ggplot2::geom_sf(data = spatdat, 
                       ggplot2::aes(fill = n_obs), color = "black", alpha = .8) +
      ggplot2::coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]),
                        expand = TRUE) +
      ggplot2::scale_fill_stepsn(breaks = brks, 
                                 colors = b_colors, 
                                 show.limits = TRUE,
                                 name = "# obs",
                                 na.value = "white") +
      fishset_theme() +
      ggplot2::theme(legend.key.size = unit(1, "cm"))
  }
  
  # save plot
  save_plot(project, "zone_freq_map", z_plot)
  
  # log function
  zone_freq_map_function <- list()
  zone_freq_map_function$functionID <- "zone_freq_map"
  zone_freq_map_function$args <- list(dat, spat,  project, lon.dat, lat.dat, 
                                      zone.dat, zone.spat, breaks, n.breaks,
                                      na.rm)
  log_call(project, zone_freq_map_function)
  
  
  z_plot
}