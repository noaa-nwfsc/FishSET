spatial_qaqc <- function(dat, project, spat, lon.dat, lat.dat, lon.spat = NULL,
                         lat.spat = NULL, id.spat = NULL, epsg = NULL, date = NULL, 
                         group = NULL) {
  #' Spatial Data Quality Checks
  #' 
  #' This function performs spatial quality checks and outputs summary tables and 
  #' plots.
  #' 
  #' @param dat Primary data containing information on hauls or trips. Table in 
  #'   FishSET database contains the string 'MainDataTable'.
  #' @param project Name of project.
  #' @param spat Spatial data containing information on fishery management or 
  #'   regulatory zones. See \code{\link{read_dat}} for details on importing 
  #'   spatial data. 
  #' @param lon.dat Longitude variable in \code{dat}.
  #' @param lat.dat Latitude variable in \code{dat}.
  #' @param lon.spat Variable or list from \code{spat} containing longitude data. 
  #'    Required for csv files. Leave as NULL if \code{spat} is a shape or json file.
  #' @param lat.spat Variable or list from \code{spat} containing latitude data. 
  #'   Required for csv files. Leave as NULL if \code{spat} is a shape or json file.
  #' @param id.spat Polygon ID column. Required for csv files. Leave as NULL if 
  #'   \code{spat} is a shape or json file.
  #' @param epsg EPSG number. Set the epsg to ensure that \code{spat} and \code{dat} 
  #'   have the same projections. If epsg is not specified but is defined for 
  #'   \code{spat}, then the \code{spat} epsg will be applied to \code{dat}. 
  #'   See \url{http://spatialreference.org/} to help identify optimal epsg number.
  #' @param date String, name of date variable. Used to summarize over year. If
  #'   \code{NULL} the first date column will be used. Returns an error if no date
  #'   columns can be found. 
  #' @param group String, optional. Name of variable to group spatial summary by. 
  #' @export
  #' @import ggplot2
  #' @import sf
  #' @importFrom rlang sym
  #' @importFrom dplyr group_by summarize
  #' @importFrom magrittr %>% 
  #' @return A list of plots and/or dataframes depending on whether spatial data 
  #' quality issues are detected. The list includes:
  #'   \describe{
  #'     \item{spatial_summary}{Dataframe containing the percentage of observations 
  #'       that occur at sea and within zones, on land, outside zones but at sea, 
  #'       or on zone boundary by year and/or group. The total number of observations by 
  #'       year/group are in the "N" column.}
  #'     \item{outside_plot}{Plot of observations outside regulatory zones.}
  #'     \item{land_plot}{Plot of observations that fall on land.}
  #'     \item{land_out_plot}{Plot of observations that occur on land and are outside
  #'       the regulatory zones (combines outside_plot and land_plot if both occur).}
  #'     \item{boundary_plot}{Plot of observations that fall on zone boundary.}
  #'     \item{expected_plot}{Plot of observations that occur at sea and within zones.}
  #'     \item{distance_plot}{Histogram of distance form nearest zone (meters) by year 
  #'       for observations that are outside regulatory grid.}
  #'     \item{distance_freq}{Binned frequency table of distance values.}
  #'     \item{distance_summary}{Dataframe containing the minimum, 1st quartile, 
  #'       median, mean, 3rd quartile, and maximum distance values by year and/or group.}
  #'   }
  #'   
  
  out <- data_pull(dat)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main")
  
  spatout <- data_pull(spat)
  spatdat <- spatout$dataset
  spat <- parse_data_name(spat, "spat")
  
  tmp <- tempfile()
  
  end <- FALSE
  out_col <- NULL
  land_col <- NULL
  bound_col <- NULL
  expected_col <- NULL
  
  grp_len <- length(c("YEAR", group))
  
  # Year ----
  if (!is.null(date)) {
    
    dataset[[date]] <- date_parser(dataset[[date]])
    dataset$YEAR <- as.integer(format(dataset[[date]], "%Y"))
    
  } else {
    
    date <- date_cols(dataset)[1]
    
    if (length(date) == 0) {
      
      warning("'date' column required.")
      end <- TRUE
      
    } else {
      
      dataset$YEAR <- as.integer(format(dataset[[date]], "%Y"))
    }
  }
  
  # Lat Lon checks ----
  lat_lon <- grep("lat|lon", names(dataset), ignore.case = TRUE)
  lat_cols <- find_lat(dataset)
  lon_cols <- find_lon(dataset)
  
  num_ll <- !qaqc_helper(dataset[lat_lon], is.numeric)
  
  lat_deg <- qaqc_helper(dataset[lat_cols], function(x) {
    if (!is.numeric(x)) TRUE
    else any(nchar(trunc(abs(x))) > 2)}) 
  
  lon_deg <- qaqc_helper(dataset[lon_cols], function(x) {
    if (!is.numeric(x)) TRUE
    else any(nchar(trunc(abs(x))) > 3)}) 
  
  if (any(c(lat_deg, lon_deg, num_ll))) {
    
    lat_deg_ind <- which(lat_deg)
    lon_deg_ind <- which(lon_deg)
    num_ind <- which(num_ll)
    
    warning(paste("The following latitude/longitude variables are not in decimal degrees:", 
                  paste(names(dataset)[unique(c(num_ind, lat_deg_ind, lon_deg_ind))], collapse = ","), 
                  "\nRun 'degree' function to convert to decimal degrees."))
    
    end <- TRUE
  } 
  
  if (any(abs(dataset[[lon.dat]]) > 180)) {
    
    warning("Longitude is not valid (outside -180:180). Function not run")
    end <- TRUE
  }
  
  if (any(abs(dataset[[lat.dat]]) > 90)) {
    
    warning("Latitude is not valid (outside -90:90. Function not run")
    end <- TRUE
  }
  
  if (end == FALSE) {

    # convert dat to sf object
    dat_sub <- sf::st_as_sf(x = dataset, coords = c(lon.dat, lat.dat), 
                            crs = "+proj=longlat +datum=WGS84")
    
    if (!("sf" %in% class(spatdat))) {
      
      if ("sp" %in% class(spatdat)) spatdat <- sf::st_as_sf(spatdat)
      
      else {
        
        spatdat <- sf::st_as_sf(x = spatdat, coords = c(lon.spat, lat.spat), 
                                crs = "+proj=longlat +datum=WGS84")
        # Convert point geometry to polygon
        spatdat <- 
          spatdat %>%
          dplyr::group_by(dplyr::across(id.spat)) %>% 
          dplyr::summarize(do_union = FALSE) %>% 
          sf::st_cast("POLYGON")
      }
    }
    
    if (raster::projection(spatdat) != raster::projection(dat_sub)) {
      
      warning("Projection does not match. The detected projection in the",
              " spatial file will be used unless epsg is specified.")
    }
    
    if (!is.null(epsg)) {
      
      dat_sub <- sf::st_transform(dat_sub, epsg)
      spatdat <- sf::st_transform(spatdat, epsg)
      
    } else if (!is.na(sf::st_crs(spatdat))) {
      
      dat_sub <- sf::st_transform(dat_sub, sf::st_crs(spatdat))
      
    } else {
      
      spatdat <- sf::st_transform(spatdat, "+proj=longlat +datum=WGS84")
    }
    
    # base map ---- 
    
    bbox <- sf::st_bbox(dat_sub)
    
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
    
    # points on land ----
    lon_sym <- rlang::sym(lon.dat)
    lat_sym <- rlang::sym(lat.dat)
    
    land_pts <- sf::st_intersects(dat_sub, base_map)
    
    obs_on_land <- vapply(land_pts, function(x) length(x) > 0, logical(1))
    land_ind <- which(obs_on_land)
    
    if (sum(obs_on_land) > 0) {
      
      n_land <- sum(obs_on_land)
      p_land <- round(n_land/nrow(dataset) * 100, 1)
      land_msg <- paste0(n_land, " observations (", p_land, "%) occur on land.\n")
      
      cat(land_msg, file = tmp, append = TRUE)
      warning(land_msg)
      
      land_col <- "ON_LAND"
      dataset[[land_col]] <- obs_on_land
      
      land_plot <- 
        ggplot2::ggplot() + 
        ggplot2::geom_sf(data = base_map) + 
        ggplot2::geom_point(data = dataset[land_ind, ], 
                            ggplot2::aes(x = !!lon_sym, y = !!lat_sym), 
                            size = 1, alpha = .25, color = "red") + 
        ggplot2::coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]),
                          expand = TRUE) +
        ggplot2::labs(title = "Obs on land", x = "Longitude", y = "Latitude",
                      subtitle = paste0("N:", n_land, " (", p_land, "%)")) + 
        fishset_theme()
    }
    
    # points outside zone ----
    pts_int <- sf::st_intersects(dat_sub, spatdat)
    
    obs_outside <- vapply(pts_int, function(x) length(x) == 0, logical(1))
    obs_out_not_land <- obs_outside != obs_on_land # remove land obs
    out_nl_ind <- which(obs_out_not_land)
    
    if (sum(obs_out_not_land) > 0) {
      
      n_out <- sum(obs_out_not_land)
      p_out <- round(n_out/nrow(dataset) * 100, 1)
      out_msg <- paste0(n_out, " observations (", p_out, "%) are outside the regulatory zones.\n")
      
      cat(out_msg, file = tmp)
      warning(out_msg)
      
      out_col <- "OUTSIDE_ZONE"
      dataset[[out_col]] <- obs_out_not_land
      
      outside_plot <- 
        ggplot2::ggplot() + 
        ggplot2::geom_sf(data = base_map) + 
        ggplot2::geom_point(data = dataset[out_nl_ind, ], 
                            ggplot2::aes(x = !!lon_sym, y = !!lat_sym), 
                            size = 1, alpha = .25, color = "red") + 
        ggplot2::coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]),
                          expand = TRUE) +
        ggplot2::labs(title = "Obs outside zone", x = "Longitude", y = "Latitude",
                      subtitle = paste0("N:", n_out, " (", p_out, "%)")) + 
        fishset_theme()
    }
    
    # obs on zone boundary lines ----
    obs_on_bound <- vapply(pts_int, function(x) length(x) > 1, logical(1))
    bound_ind <- which(obs_on_bound)
    
    if (sum(obs_on_bound) > 0) {
      
      n_bound <- sum(obs_on_bound)
      p_bound <- round(n_bound/nrow(dataset) * 100, 1)
      bound_msg <- paste0(n_bound, " observations (", p_bound, "%) occur on boundary",
                          " line between regulatory zones.\n")
      
      cat(bound_msg, file = tmp, append = TRUE)
      warning(bound_msg)
      
      bound_col <- "ON_BOUNDARY"
      dataset[[bound_col]] <- obs_on_bound
      
      bound_plot <- 
        ggplot2::ggplot() + 
        ggplot2::geom_sf(data = base_map) + 
        ggplot2::geom_point(data = dataset[bound_ind, ], 
                            ggplot2::aes(x = !!lon_sym, y = !!lat_sym), 
                            size = 1, alpha = .25, color = "red") + 
        ggplot2::coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]),
                          expand = TRUE) +
        ggplot2::labs(title = "Obs on zone boundary", x = "Longitude", y = "Latitude",
                      subtitle = paste0("N:", n_bound, " (", p_bound, "%)")) + 
        fishset_theme()# + guides(colour = guide_legend(override.aes = list(alpha = 1)))
    }
    
    # expected location ----
    obs_expected_loc <- !obs_on_bound & !obs_on_land & !obs_outside
    
    expected_col <- "EXPECTED_LOC"
    dataset[[expected_col]] <- obs_expected_loc
    
    n_expected <- sum(obs_expected_loc)
    p_expected <- round(n_expected/nrow(dataset) * 100, 1)
    
    expected_plot <- 
      ggplot2::ggplot() + 
      ggplot2::geom_sf(data = base_map) + 
      ggplot2::geom_point(data = dataset[obs_expected_loc, ], 
                          ggplot2::aes(x = !!lon_sym, y = !!lat_sym), 
                          size = 1, alpha = .15, color = "red") + 
      ggplot2::coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]),
                        expand = TRUE) + 
      ggplot2::labs(title = "Obs in expected location", x = "Longitude", y = "Latitude",
                    subtitle = paste0("N:", n_expected, " (", p_expected, "%)")) + 
      fishset_theme() # + guides(colour = guide_legend(override.aes = list(alpha = 1)))
    
    # Spatial summary table ----
    
    if (any(!is.null(out_col), !is.null(land_col), !is.null(bound_col))) {
      
      spat_tab <- agg_helper(dataset, value = c(expected_col, out_col, land_col, bound_col),
                             group = c("YEAR", group), fun = sum)
    
      year_tab <- agg_helper(dataset, value = "YEAR", group = c("YEAR", group),
                             fun = length)
      
      spat_tab$N <- year_tab[[grp_len + 1]]
      
      spat_nm <- names(spat_tab[c(expected_col, out_col, land_col, bound_col)])
      perc_nm <- paste0(c(expected_col, out_col, land_col, bound_col), "_perc")
      
      spat_tab[perc_nm] <-
        round(spat_tab[ ,c(expected_col, out_col, land_col, bound_col)] / spat_tab[ ,"N"] * 100, 2)
      
      spat_tab[spat_nm] <- NULL
      
      spat_tab <- spat_tab[order(spat_tab$YEAR), 
                           c("YEAR", "N", group, perc_nm)]
      
      row.names(spat_tab) <- 1:nrow(spat_tab)
    }
    
    # distance from nearest zone (meters) ----
    
    if (sum(obs_outside) > 0) {
      
      nearest <- sf::st_nearest_feature(dat_sub[obs_outside, ], spatdat) 
      dist.rec <- sf::st_distance(dat_sub[obs_outside, ], spatdat[nearest, ], 
                                  by_element = TRUE)
      
      dist_df <- dataset[obs_outside, c(lat.dat, lon.dat, "YEAR", group, 
                                        land_col, out_col)]
      dist_df$dist <- as.numeric(dist.rec)
      
      dist_rng <- range(dist_df$dist, finite = TRUE)
      p_brks <- pretty(dist_rng, n = 15, min.n = 1)
      
      group_exp <- function() {
        if (!is.null(group)) rlang::sym(group) 
        else NULL
      }
      
      
      dist_plot <- 
        ggplot2::ggplot(data = dist_df, ggplot2::aes(dist)) + 
        ggplot2::labs(title = "Distance (m) from nearest zone", 
                      x = "Distance (m)",
                      color = "Year",
                      fill = "Year") +
        ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)), 
                                breaks = p_brks, color = "black", fill = "grey80") + 
        ggplot2::stat_density(position = "identity", alpha = .1, adjust = 2, 
                              fill = "red", color = "red") +
        fishset_theme() + 
        ggplot2::theme(legend.position = "bottom")
      
      if (!is.null(group)) {
        
        fm <- stats::reformulate(group)
        dist_plot <- dist_plot + ggplot2::facet_wrap(fm)
      }
      
      # freq table
      dist_freq <- freq_table(dist_df, "dist", group = c("YEAR", group), 
                              bins = 15, type = "freq", format_lab = "decimal")
      
      # dist summary table
      dist_sum <- agg_helper(dist_df, "dist", group = c("YEAR", group), 
                             fun = function(x) summary(x, digits = 2))
      
      dist_sum <- dist_sum[order(dist_sum$YEAR), ]
      row.names(dist_sum) <- 1:nrow(dist_sum)
    }
    
    if (sum(obs_on_bound, obs_outside) == 0) {
      
      cat("No spatial data quality issues detected.", file = tmp)
    }
    
    # arrange plots ----
    ## on land/outside zone ----
    if (sum(obs_on_land) > 0 & sum(obs_out_not_land) > 0) {
      
      land_out_plot <- gridExtra::arrangeGrob(grobs = list(land_plot, outside_plot), 
                                         nrow = 1, ncol = 2)
    }
    
    # save output ----
    
    # Log function
    spatial_qaqc_function <- list()
    spatial_qaqc_function$functionID <- "spatial_qaqc"
    spatial_qaqc_function$args <- list(dat, project, spat, lon.dat, lat.dat, 
                                       lon.spat, lat.spat, epsg, date, group)
    spatial_qaqc_function$msg <- suppressWarnings(readLines(tmp))
    log_call(project, spatial_qaqc_function)
    
    unlink(tmp)
    
    f_plot <- function(x) if (!is.null(x)) gridExtra::grid.arrange(x) else NULL
    
    f_land <- function() {
      
      if (sum(obs_on_land) > 0 & sum(obs_out_not_land) == 0) land_plot
      else NULL
    }
    
    f_outside <- function() {
      
      if (sum(obs_out_not_land) > 0 & sum(obs_on_land) == 0) outside_plot
      else NULL
    }
    
    f_land_out <- function() {
      
      if (sum(obs_out_not_land) > 0 & sum(obs_on_land) > 0) {
        gridExtra::grid.arrange(land_out_plot)
      } else NULL
    }
    
    out <- 
    list(spatial_summary = get0("spat_tab"),
         outside_plot =  f_outside(), 
         land_plot = f_land(), 
         land_outside_plot = f_land_out(),
         boundary_plot = get0("bound_plot"),
         expected_plot = expected_plot,
         distance_plot = get0("dist_plot"),
         distance_freq = get0("dist_freq"),
         distance_summary = get0("dist_sum"))
    
    ind <- vapply(out, function(x) !is.null(x), logical(1))
    
    out[ind]
  }
}