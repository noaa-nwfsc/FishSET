zone_summary <- function(dat, spat, project, lon.dat, lat.dat, zone.dat, 
                         zone.spat, count = TRUE, var = NULL, group = NULL, fun = NULL, 
                         breaks = NULL, n.breaks = 10, bin_colors = NULL, na.rm = TRUE, 
                         dat.center = TRUE, output = "plot") {
  
  #' Summarize zones/closure areas
  #' 
  #' \code{zone_summary} counts observations and aggregates values in \code{dat} 
  #' by regulatory zone or closure area.
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
  #'@param count Logical. if \code{TRUE}, then the number observations per zone 
  #'  will be returned. Can be paired with \code{fun = "percent"} and \code{group}.
  #'  \code{zone_summary} will return an error if \code{var} is include and 
  #'  \code{count = TRUE}.
  #'@param var Optional, name of numeric variable to aggregate by zone/closure
  #'  area. 
  #'@param group Name of grouping variable to aggregate by zone/closure area. Only
  #'  one variable is allowed. 
  #'@param fun Function name (string) to aggregate by. \code{"percent"} the 
  #'  percentage of observations in a given zone. Other options include "sum", 
  #'  "mean", "median", "min", and "max". 
  #'@param breaks A numeric vector of breaks to bin zone frequencies by. Overrides
  #'  \code{n.breaks} if entered. 
  #'@param n.breaks The number of break points to create if breaks are not given 
  #'  directly. Defaults to 10. 
  #'@param bin_colors Optional, a vector of colors to use in plot. Must be same length
  #'  as breaks. Defaults to \code{viridis::viridis(option = "H")}.
  #'@param na.rm Logical, whether to remove zones with zero counts. 
  #'@param dat.center Logical, whether the plot should center on \code{dat} 
  #'  (\code{TRUE}) or \code{spat} (\code{FALSE}). Recommend \code{dat.center = TRUE}
  #'  when aggregating by regulatory zone and \code{dat.center = FALSE} when
  #'  aggregating by closure area. 
  #'@param output  Output a \code{"plot"}, \code{"table"}, or both (\code{"tab_plot"}).
  #'  Defaults to \code{"plot"}.
  #'@details Observations in \code{dat} must be assigned to regulatory zones to 
  #'  use this function. See \code{\link{assignment_column}} for details. 
  #'  \code{zone_summary} can return: the number of observations per zone 
  #'  (\code{count = TRUE}, \code{fun = NULL}, \code{group = NULL}), the percentage
  #'  of observations by zone (\code{count = TRUE}, \code{fun = "percent"}, \code{group = NULL}),
  #'  the percentage of observations by zone and group (\code{count = TRUE}, \code{fun = "percent"}, 
  #'  \code{group = "group"}), summary of a numeric variable by zone (\code{count = FALSE}, 
  #'  \code{var = "var"}, \code{fun = "sum"}, \code{group = NULL}), summary of a numeric variable
  #'  by zone and group (\code{count = FALSE}, \code{var = "var"}, \code{fun = "sum"}, 
  #'  \code{group = "group"}), share (percentage) of a numeric variable by zone
  #'  (\code{count = FALSE}, \code{var = "var"}, \code{fun = "percent"}, \code{group = NULL}), 
  #'  share (percentage) of a numeric variable by zone and group (\code{count = FALSE}, 
  #'  \code{var = "var"}, \code{fun = "percent"}, \code{group = "group"}).
  #'@export
  #'@import ggplot2
  #'@import dplyr
  #'@import sf
  #'@examples 
  #'\dontrun{
  #'zone_summary(pollockMainTable, nmfs_area, "LonLat_START_LON", "LonLat_START_LAT",
  #'              zone.dat = "ZoneID", zone.spat = "NMFS_AREA")
  #'}
  
  
  # Call in datasets
  out <- data_pull(dat, project)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main", project)
  
  spatout <- data_pull(spat, project)
  spatdat <- spatout$dataset
  spat <- parse_data_name(spat, "spat", project)
  
  multi_plot <- FALSE
  
  # secondary column when fun = percent
  val_2 <- NULL
  
  # summary table ----
  
  if (count & !is.null(var)) {
    
    stop("Cannot use count with var. To count by group, add variable to 'group' argument.", 
         call. = FALSE)
  }
  
  if (!is.null(group) && length(group) > 1) {
    
    stop("Cannot summarize zones by more than one grouping variable.", call. = FALSE)
  }
  
  if (!is.null(var) && is.double(dataset[[var]]) && count) {
    
    stop("Cannot count by a numeric variable.", call. = FALSE)
  }
  
  if (!count & is.null(fun)) {
    
    stop("'fun' argument required.", call. = FALSE)
  }
  
  zone_tab <- agg_helper(dataset, value = var, group = c(zone.dat, group), 
                          fun = fun, count = count)
  
  # percent flag
  if (!is.null(fun) && fun == "percent") calc_perc <- TRUE 
  else calc_perc <- FALSE
  
  if (count) {
    
    if (!calc_perc & is.null(group)) {
      # count zones
      val_var <- "n"
      legend_name <- "# of obs"
      
    } else if (calc_perc & is.null(group)) {
      # count zones, then perc
      val_var <- "perc"
      val_2  <- "n"
      legend_name <- "% of total obs"
      
    } else if (calc_perc & !is.null(group)) {
      # count group by zone, then perc (one plot per group)
      val_var <- "perc"
      val_2  <- "n"
      multi_plot <- TRUE
      
    } else if (!calc_perc & !is.null(group)) {
      # count group by zone (one plot per group)
      val_var <- "n"
      multi_plot <- TRUE
    
    } else stop("Invalid arguments.", call. = FALSE)
    
  } else {
    
    if (!calc_perc & is.null(var)) {
      
      stop("Invalid arguments. Set 'count = TRUE' or include a numeric variable",
           " to aggregate by.", call. = FALSE)
      
    } else if (calc_perc & is.null(var)) {
      
      stop("Invalid arguments. Include a numeric variable to aggregate by.", 
           call. = FALSE)
      
    } else if (!calc_perc & !is.null(var)) {
      # agg var by zone
      legend_name <- paste0(var, " (", fun, ")")
      val_var <- var
      
    } else if (calc_perc & !is.null(var)) {
      # agg var by zone, then perc
      val_var <- paste0(var,"_perc")
      val_2 <- var
      legend_name <- paste("% of total", var)
    
    } else stop("Invalid arguments.", call. = FALSE)
    
    if (!is.null(group)) multi_plot <- TRUE
  }
  
  # confid check ----
  # skip check if rule = "k" and count = TRUE
  cc_par <- get_confid_check(project)
  
  check_c <- cc_par$check & (cc_par$rule == "n" | cc_par$rule == "k" & !count)
  
  if (check_c) {
      
    check_out <- 
      check_confidentiality(dataset, project, v_id = cc_par$v_id, value_var = var,
                            group = c(zone.dat, group), rule = cc_par$rule,
                            value = cc_par$value)
    
    if (any(check_out$suppress)) {
      
      zone_tab_c <- 
        suppress_table(check_out$table, zone_tab, value_var = c(val_2, val_var), 
                       group = c(zone.dat, group), rule = cc_par$rule, type = "code")
      
      save_table(zone_tab_c, project, "zone_summary_confid")
    }
  }
  
  if (output %in% c("plot", "tab_plot")) {
    
    # convert dat to sf object
    dat_sf <- sf::st_as_sf(x = dataset, coords = c(lon.dat, lat.dat), 
                           crs = "+proj=longlat +datum=WGS84")
    
    merge_spat <- function(z_tab) {
      
      by_vec <- zone.dat
      names(by_vec) <- zone.spat
      
      # merge spatdat w/ zone summary
      spat_join <- dplyr::left_join(spatdat[zone.spat], z_tab, by = by_vec)
      
      if (!is.na(sf::st_crs(spatdat))) {
        
        dat_sf <- sf::st_transform(dat_sf, sf::st_crs(spatdat))
        
      } else {
        
        spat_join <- sf::st_transform(spat_join, "+proj=longlat +datum=WGS84")
      }

      if (any(!(sf::st_is_valid(spatdat)))) {
        
        spat_join <- sf::st_make_valid(spat_join)
      } 
      
      if (na.rm) {
        
        # filter out zero counts
        spat_join <- spat_join[!is.na(spat_join[[val_var]]), ]
      }
      
      spat_join
    }
    
    spat_join <- merge_spat(zone_tab)
    
    # base map ----
    if (dat.center)  bbox <- sf::st_bbox(dat_sf)
    else  bbox <- sf::st_bbox(spatdat)

    base_map <- ggplot2::map_data("world",
                                  xlim = c(bbox["xmin"], bbox["xmax"]),
                                  ylim = c(bbox["ymin"], bbox["ymax"]))

    base_map <- sf::st_as_sf(base_map, coords = c("long", "lat"),
                             crs = sf::st_crs(spat_join))

    # convert points to polygon
    base_map <-
      base_map %>%
      dplyr::group_by(across(all_of("group"))) %>%
      dplyr::summarize(do_union = FALSE) %>%
      sf::st_cast("POLYGON")
    
    # plot functions 
    lon_sym <- rlang::sym(lon.dat)
    lat_sym <- rlang::sym(lat.dat)

    # breaks ----
    z_brk_fun <- function(dat, breaks, n.breaks, bin_colors, count) {
      
      if (!is.null(breaks)) {
        
        brks <- breaks 
        
        if (!is.null(bin_colors)) {
          
          if (length(bin_colors) != length(brks)) {
            
            warning("bin_colors length is not equal to breaks. Using default colors.")
            # bin_colors <- viridis::viridis(length(brks), option = "H")
            bin_colors <- fishset_viridis(length(brks))
          }
          
        } else {
          
          # bin_colors <- viridis::viridis(length(brks), option = "H")
          bin_colors <- fishset_viridis(length(brks))
        }
        
      } else {
        
        brks <- pretty(dat[[val_var]], n = n.breaks)
        
        if (!is.null(bin_colors)) {
          
          if (length(bin_colors) != length(brks)) {
            
            warning("bin_colors length is not equal to breaks. Using default colors.")
            # bin_colors <- c("gray", viridis::viridis(length(brks) - 1, option = "H"))
            bin_colors <- c("gray", fishset_viridis(length(brks) - 1))
          }
          
        } else {
          
          # bin_colors <- c("gray", viridis::viridis(length(brks) - 1, option = "H"))
          bin_colors <- c("gray", fishset_viridis(length(brks) - 1))
        }
        
        if (count) {
          
          if (min(brks) == 0) {
            
            if (brks[2] > 10) brks[1] <- 10
            else  brks[1] <- round((brks[2]/2))
          }
        }
      }
      
      list(brks = brks, colors = bin_colors)
    }
    
    # plot ----
    var_sym <- function() rlang::sym(val_var)
    
    z_plot_fun <- function(spatdat, brks, bin_colors, legend_name) {
      
      ggplot2::ggplot() +  
        ggplot2::geom_sf(data = base_map) +  
        ggplot2::geom_sf(data = spatdat, 
                         ggplot2::aes(fill = !!var_sym()), color = "black", alpha = .8) +
        ggplot2::coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]),
                          expand = TRUE) +
        ggplot2::scale_fill_stepsn(breaks = brks,
                                   colors = bin_colors, 
                                   labels = scales::comma,
                                   show.limits = TRUE,
                                   values = scales::rescale(brks),
                                   name = legend_name,
                                   na.value = "white") +
        fishset_theme() +
        ggplot2::theme(legend.key.size = unit(1, "cm"))
    }
    
    if (multi_plot) {
      
      group_zone <- function(spat_join) {
        
        p_levels <- unique(spat_join[[group]]) # what if too many levels?
        
        z_plot <- 
          lapply(p_levels, function(x) {
            
            dat <- dplyr::filter(spat_join, .data[[group]] == !!x)
            
            if (count) {
              # update legend to include group name
              if (calc_perc) legend_name <- paste0("% of total obs: \n ", x)
              else legend_name <- paste("# of obs: \n ", x)
              
            } else {
              
              if (calc_perc) legend_name <- paste0("% of ", var, ": \n", x)
              else legend_name <- paste0(fun, " ", var, ": \n", x)
            }
            
            break_list <- z_brk_fun(dat, breaks, n.breaks, bin_colors, count = count)
            
            z_plot_fun(dat, brks = break_list$brks, 
                       bin_colors = break_list$colors, 
                       legend_name = legend_name)
          })
        
        z_plot
      }
      
      z_plot <- group_zone(spat_join)
      # save plot
      save_nplot(project, "zone_summary", z_plot) 
      
    } else {
      
      break_list <- z_brk_fun(spat_join, breaks, n.breaks, bin_colors, count = count)
      
      z_plot <- z_plot_fun(spat_join, brks = break_list$brks, 
                           bin_colors = break_list$colors, 
                           legend_name = legend_name)
      # save plot
      save_plot(project, "zone_summary", z_plot) 
    }
    
    # confid plot ----
      
    if (check_c && any(check_out$suppress)) {
    # filter out suppressed values
    spat_join_c <- merge_spat(zone_tab_c)
    spat_join_c <- spat_join_c %>% dplyr::filter(.data[[val_var]] != -999)
    
      if (multi_plot) {
        
        z_plot_c <- group_zone(spat_join_c)
        # save plot
        save_nplot(project, "zone_summary_confid", z_plot_c) 
      
      } else {
      
        break_list_c <- z_brk_fun(spat_join_c, breaks, n.breaks, bin_colors, count = count)
        
        z_plot_c <- z_plot_fun(spat_join_c, brks = break_list_c$brks, 
                             bin_colors = break_list_c$colors, 
                             legend_name = legend_name)  
        # save plot
        save_plot(project, "zone_summary_confid", z_plot_c) 
      }
    }
  }
  
  # save table
  save_table(zone_tab, project, "zone_summary")
  
  # log function
  zone_summary_function <- list()
  zone_summary_function$functionID <- "zone_summary"
  zone_summary_function$args <- list(dat, spat, project, lon.dat, lat.dat,
                                     zone.dat, zone.spat, count, var, group,
                                     fun, breaks, n.breaks, bin_colors, na.rm,
                                     dat.center, output)
  log_call(project, zone_summary_function)
  
  if (output == "plot") z_plot
  else if (output == "tab_plot") list(table = zone_tab, plot = z_plot)
  else zone_tab
}
