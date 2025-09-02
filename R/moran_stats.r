#' Calculate Moran's I statistic
#'
#' This wrapper function calculates global and local Moran's I statistics to measure spatial 
#' autocorrelation by discrete area.
#'
#' @param dat Primary data containing information on hauls or trips. Table in FishSET database 
#'   contains the string 'MaindataTable'.
#' @param var Numeric variable from \code{dat} to test for spatial autocorrelation.
#' @param dat_zone Variable in \code{dat} that identifies the individual zones or
#'   areas. Define if exists in \code{dat} and is not named `ZoneID`. Defaults to NULL. 
#' @param spat Spatial data containing information on fishery management or regulatory zones.
#' Shape, json, geojson, and csv formats are supported.
#' @param spat_zone Variable or list in \code{spat} that identifies the individual areas or zones.
#' If \code{spat} is class sf, \code{cat} should be name of list containing information on zones.
#' @param project Name of the project
#' @details 
#' The function measures the degree of spatial autocorrelation and utilizes functions from the 
#' `spdep` package. The function requires a spatial file with latitude and longitude 
#' coordinates defining the boundaries of areas or zones and a variable of interest (`var`) to 
#' test for spatial autocorrelation.
#' 
#' @return Returns a list with (1) global Moran's I stats, (2) Moran lagged plot, (3) LISA cluster
#'  map summarizing local Moran's values
#' @import ggplot2
#' @importFrom spdep knn2nb knearneigh nb2listw localmoran moran.test
#' @importFrom sf st_coordinates st_centroid st_geometry st_make_valid
#' @export
#' @examples
#' \dontrun{
#' moran_stats(pcodMainDataTable, var='OFFICIAL_MT_TONS', dat_zone='zoneID',
#'   spat=spatdat, spat_zone='NMFS_AREA', project = 'pcod')
#' }
#'
moran_stats <- function(dat, var, dat_zone, spat, spat_zone, project) {
  
  # Ensure necessary packages are loaded
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required.")
  }
  if (!requireNamespace("spdep", quietly = TRUE)) {
    stop("Package 'spdep' is required for spatial weights calculation.")
  }
  
  # Data check and prep ---------------------------------------------------------------------------
  
  # Call in datasets
  out <- data_pull(dat, project)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main", project)
  
  spat_out <- data_pull(spat, project)
  spatdat <- spat_out$dataset
  spat <- parse_data_name(spat, 'spat', project)
  
  # Check if the variable exists in the dataset
  if (!var %in% names(dataset)) {
    stop(paste0("The variable '", var, "' was not found in the dataset '", dat, "'."))
  }
  
  # Check if the variable is numeric
  if (!is.numeric(dataset[[var]])) {
    stop(paste0("The variable '", var, "' is of type '", class(dataset[[var]]), 
                "'. It must be a numeric variable to calculate Moran's I."))
  }
  
  # Check if the data zone variable exists in the dataset
  if (!dat_zone %in% names(dataset)) {
    stop(paste0("The variable '", dat_zone, "' was not found in the dataset '", dat, "'."))
  }
  
  # Check if the spatial zone variable exists in the spatial data
  if (!spat_zone %in% names(spatdat)) {
    stop(paste0("The variable '", spat_zone, "' was not found in the spatial data '", spat, "'."))
  }
  
  if (var == dat_zone) {
    stop(paste0("Cannot use ", var, " for 'var' and 'dat_zone' input arguments."))
  }
  
  # Rename zone IDs to make sure these match between dataset and spat
  spat_tmp <- spatdat
  names(spat_tmp)[names(spat_tmp) == spat_zone] <- dat_zone
  
  # Only include zones that are present in dataset and remove any duplcated zones
  spat_tmp <- spat_tmp[which(spat_tmp[[dat_zone]] %in% dataset[[dat_zone]]),]
  spat_tmp <- spat_tmp %>%
    dplyr::select(all_of(dat_zone), geometry) %>%
    dplyr::distinct(pick(all_of(dat_zone)), .keep_all = TRUE)
  
  # Merge dataset and spat and convert to sf object and group by zone
  # Aggregate dataset by zone (mean for each zone)
  merged_df <- dataset %>%
    dplyr::group_by(across(all_of(dat_zone))) %>%
    dplyr::summarise(
      across(all_of(var), \(x) mean(x, na.rm = TRUE), .names = "{.col}"),
      .groups = "drop") %>%
    dplyr::ungroup() %>%
    dplyr::left_join(., spat_tmp, by = dat_zone)
  
  # Convert the merged data frame to a simple features (sf) object
  merged_sf <- sf::st_as_sf(merged_df)
  
  # Find indices with empty geometries, and remove
  empty_geoms_ind <- which(st_is_empty(merged_sf))
  merged_sf <- st_make_valid(merged_sf)
  merged_sf <- merged_sf[!st_is_empty(merged_sf), ]
  
  # Check if the merged object is empty
  if (nrow(merged_sf) == 0) {
    stop("The provided data frames do not have matching zone IDs. The merge resulted in an 
         empty data set.")
  }
  
  # Spatial weights calculation -------------------------------------------------------------------
  
  # Calculate spatial weights (e.g., Queen contiguity)
  # This step uses the 'sf' and 'spdep' packages
  nb_list <- suppressWarnings(spdep::poly2nb(merged_sf, queen = TRUE)) 
  queen_weights <- spdep::nb2listw(nb_list, style = "W", zero.policy = TRUE)
  
  # Global Moran's I ------------------------------------------------------------------------------
  
  moran_test <- spdep::moran.test(merged_sf[[var]], listw = queen_weights)
  
  # Create data for the Moran plot
  moran_plot_dat <- spdep::moran.plot(merged_sf[[var]], 
                                      listw = queen_weights,
                                      xlab = var,
                                      ylab = paste0("Spatially lagged ", var))
  
  # Generate Moran's plot using ggplot
  moran_plot <- ggplot() +
    geom_point(data = moran_plot_dat, aes(x = x, y = wx), shape = 1) +
    geom_smooth(data = moran_plot_dat, aes(x = x, y = wx), method = "lm") +
    geom_vline(xintercept = mean(moran_plot_dat$x), 
               linetype = "dashed") +
    geom_hline(yintercept = mean(moran_plot_dat$wx), 
               linetype = "dashed") +
    labs(x = var,
         y = paste0("Spatially lagged ", var)) +
    theme_classic()
  
  # Local indicators of spatial association (LISA) Clusters ---------------------------------------
  # Calculate local Moran's I for LISA clusters
  local_moran_results <- spdep::localmoran(merged_sf[[var]],
                                           listw = queen_weights,
                                           zero.policy = TRUE)
  
  # Add lagged variable and scaled variables to the sf object
  merged_sf$lagged <- spdep::lag.listw(queen_weights, 
                                       merged_sf[[var]], 
                                       zero.policy = TRUE)
  merged_sf$scaled_var <- scale(merged_sf[[var]])
  merged_sf$scaled_lagged <- scale(merged_sf[[var]])
  merged_sf$p_value <- local_moran_results[, 5]
  merged_sf$lisa_cluster <- "Not Significant"  
  
  # High-High (Hot Spot)
  merged_sf$lisa_cluster[merged_sf$scaled_var > 0 & 
                           merged_sf$scaled_lagged > 0 & 
                           merged_sf$p_value < 0.05] <- "High-High"
  # Low-Low (Cold Spot)
  merged_sf$lisa_cluster[merged_sf$scaled_var < 0 & 
                           merged_sf$scaled_lagged < 0 & 
                           merged_sf$p_value < 0.05] <- "Low-Low"
  # High-Low (Spatial Outlier)
  merged_sf$lisa_cluster[merged_sf$scaled_var > 0 & 
                           merged_sf$scaled_lagged < 0 & 
                           merged_sf$p_value < 0.05] <- "High-Low"
  # Low-High (Spatial Outlier)
  merged_sf$lisa_cluster[merged_sf$scaled_var < 0 & 
                           merged_sf$scaled_lagged > 0 & 
                           merged_sf$p_value < 0.05] <- "Low-High"
  
  # Define a color palette for the map
  lisa_colors <- c("High-High" = "#D7191C", "Low-Low" = "#2C7BB6",
                   "High-Low" = "#FDAE61", "Low-High" = "#ABD9E9",
                   "Not Significant" = "grey90")
  
  # Base map
  bbox <- sf::st_bbox(merged_sf)
  base_map <- ggplot2::map_data(map = ifelse(shift_long(merged_sf), "world2", "world"),
                                xlim = c(bbox["xmin"], bbox["xmax"]),
                                ylim = c(bbox["ymin"], bbox["ymax"]))
  base_map <- sf::st_as_sf(base_map, coords = c("long", "lat"),
                           crs = sf::st_crs(merged_sf))
  base_map <-
    base_map %>%
    dplyr::group_by(across(all_of("group"))) %>%
    dplyr::summarize(do_union = FALSE) %>%
    sf::st_cast("POLYGON")
  
  # Create the LISA cluster map using ggplot
  lisa_cluster_map <- ggplot() +
    ggplot2::geom_sf(data = base_map) +
    ggplot2::geom_sf(data = merged_sf, 
                     aes(fill = lisa_cluster, color = lisa_cluster), lwd = 0.5, alpha = 0.6) +
    ggplot2::scale_fill_manual(values = lisa_colors, name = "Cluster Type") +
    ggplot2::scale_color_manual(values = lisa_colors, name = "Cluster Type") +
    ggplot2::coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]),
                      expand = TRUE) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "bottom")
  
  # Log function call -----------------------------------------------------------------------------
  moran_stats_function <- list()
  moran_stats_function$functionID <- "moran_stats"
  moran_stats_function$args <- list(dat, var, dat_zone, spat, spat_zone, project)
  log_call(project, moran_stats_function)
  
  return(list(
    moran_test,
    moran_plot,
    lisa_cluster_map
  ))
}