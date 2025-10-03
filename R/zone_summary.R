#' Summarize zones, closure areas
#' 
#' `zone_summary` counts observations and aggregates values in `dat` 
#' by regulatory zone or closure area.
#' 
#'@param dat Primary data containing information on hauls or trips. 
#'  Table in FishSET database contains the string 'MainDataTable'.
#'@param spat A spatial data file containing information on fishery management 
#'  or regulatory zones boundaries. `sf` objects are recommended, but `sp` objects 
#'  can be used as well. See [dat_to_sf()] to convert a spatial table read from 
#'  a csv file to an `sf` object. To upload your spatial data to the FishSETFolder 
#'  see [load_spatial()].
#'@param project Name of project.
#'@param zone.dat Name of zone ID column in `dat`.
#'@param zone.spat Name of zone ID column in `spat`.
#'@param count Logical. if `TRUE`, then the number observations per zone 
#'  will be returned. Can be paired with `fun = "percent"` and `group`.
#'  `zone_summary` will return an error if `var` is include and 
#'  `count = TRUE`.
#'@param var Optional, name of numeric variable to aggregate by zone/closure
#'  area. 
#'@param group Name of grouping variable to aggregate by zone/closure area. Only
#'  one variable is allowed. 
#'@param fun Function name (string) to aggregate by. `"percent"` the 
#'  percentage of observations in a given zone. Other options include "sum", 
#'  "mean", "median", "min", and "max". 
#'@param na.rm Logical, whether to remove zones with zero counts. 
#'@param dat.center Logical, whether the plot should center on `dat` 
#'  (`TRUE`) or `spat` (`FALSE`). Recommend `dat.center = TRUE`
#'  when aggregating by regulatory zone and `dat.center = FALSE` when
#'  aggregating by closure area. 
#'@param output  Output a `"plot"`, `"table"`, or both (`"tab_plot"`).
#'  Defaults to `"plot"`.
#'@details Observations in `dat` must be assigned to regulatory zones to 
#'  use this function. See [assignment_column()] for details. 
#'  `zone_summary` can return: the number of observations per zone 
#'  (`count = TRUE`, `fun = NULL`, `group = NULL`), the percentage
#'  of observations by zone (`count = TRUE`, `fun = "percent"`, 
#'  `group = NULL`), the percentage of observations by zone and group 
#'  (`count = TRUE`, `fun = "percent"`, `group = "group"`), summary 
#'  of a numeric variable by zone (`count = FALSE`, `var = "var"`, 
#'  `fun = "sum"`, `group = NULL`), summary of a numeric variable
#'  by zone and group (`count = FALSE`, `var = "var"`, `fun = "sum"`, 
#'  `group = "group"`), share (percentage) of a numeric variable by zone
#'  (`count = FALSE`, `var = "var"`, `fun = "percent"`, `group = NULL`), 
#'  share (percentage) of a numeric variable by zone and group (`count = FALSE`, 
#'  `var = "var"`, `fun = "percent"`, `group = "group"`).
#'@export
#'@import ggplot2
#'@import dplyr
#'@import sf
#'@importFrom plotly ggplotly style config plotly_build
#'@examples 
#'\dontrun{
#'
#'# count # of obs
#'zone_summary(pollockMainTable, spat = nmfs_area, zone.dat = "ZoneID", 
#'             zone.spat = "NMFS_AREA")
#'             
#'# percent of obs
#'zone_summary(pollockMainTable, spat = nmfs_area, zone.dat = "ZoneID", 
#'             zone.spat = "NMFS_AREA", count = TRUE, fun = "percent")
#'
#'# count by group
#'zone_summary(pollockMainTable, spat = nmfs_area, zone.dat = "ZoneID", 
#'             zone.spat = "NMFS_AREA", group = "GEAR_TYPE")   
#'
#'# total catch by zone           
#'zone_summary(pollockMainTable, spat = nmfs_area, zone.dat = "ZoneID", 
#'             zone.spat = "NMFS_AREA", var = "OFFICIAL_TOTAL_CATCH_MT",
#'             count = FALSE, fun = "sum")  
#'
#'# percent of catch by zone           
#'zone_summary(pollockMainTable, spat = nmfs_area, zone.dat = "ZoneID", 
#'             zone.spat = "NMFS_AREA", var = "OFFICIAL_TOTAL_CATCH_MT",
#'             count = FALSE, fun = "percent")         
#'             
#'}

zone_summary <- function(dat,
  spat,
  project,
  zone.dat,
  zone.spat,
  dat_lon,
  dat_lat, 
  count = TRUE,
  var = NULL,
  group = NULL,
  fun = NULL,
  na.rm = TRUE,
  dat.center = TRUE,
  plot_type = "dynamic",
  output = "plot") {

  
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
  if (!is.null(fun) && fun == "percent"){
    calc_perc <- TRUE
  } else {
    calc_perc <- FALSE
  }
  
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
          group = c(zone.dat, group), rule = cc_par$rule, type = "code") %>% 
        mutate(zone.dat = as.character(zone.dat))
      
      save_table(zone_tab_c, project, "zone_summary_confid")
    }
  }
  
  if (output %in% c("plot", "tab_plot")) {
    
    zone_tab[[zone.dat]] <- as.character(zone_tab[[zone.dat]])
    spatdat[[zone.spat]] <- as.character(spatdat[[zone.spat]])
    
    merge_spat <- function(z_tab) {
      
      by_vec <- zone.dat
      names(by_vec) <- zone.spat
      # merge spatdat w/ zone summary

      spat_join <- dplyr::left_join(spatdat[zone.spat], z_tab,

        relationship = "many-to-many", by = by_vec)
      # use WGS 84 if crs is missing
      if (is.na(sf::st_crs(spatdat))) {
        spat_join <- sf::st_transform(spat_join, crs = 4326)
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
    if (dat.center)  {
      # create a bbox using zones that exist in dat
      z_ind <- spatdat[[zone.spat]] %in% unique(zone_tab[[zone.dat]])
      bbox <- sf::st_bbox(spatdat[z_ind, ]) # keeps shifted long
      
    } else bbox <- sf::st_bbox(spatdat) # use entire spatial data
    
    # world2 uses 0 - 360 lon format
    base_map <- ggplot2::map_data(map = ifelse(shift_long(spatdat), "world2", "world"),
      xlim = c(bbox["xmin"], bbox["xmax"]),
      ylim = c(bbox["ymin"], bbox["ymax"]))
    
    # convert data to sf for plotting purposes
    base_map <- sf::st_as_sf(base_map, coords = c("long", "lat"),
      crs = sf::st_crs(spat_join))
    
    # convert points to polygon
    base_map <-
      base_map %>%
      dplyr::group_by(across(all_of("group"))) %>%
      dplyr::summarize(do_union = FALSE) %>%
      sf::st_cast("POLYGON")
    
    # plot ----
    var_sym <- rlang::sym(val_var)
    
    ## function to plot ---------------------------------------------------------------------------
    z_plot_fun_static <- function(spatdat, legend_name) {
      full_data_range <- range(spatdat[[val_var]], na.rm = TRUE)

      out <- ggplot2::ggplot() +
        ggplot2::geom_sf(data = base_map) +
        ggplot2::geom_sf(data = spatdat,
          ggplot2::aes(fill = !!rlang::sym(val_var)),
          color = "black", alpha = .8) +
        ggplot2::scale_fill_viridis_c(
          name = legend_name,
          limits = full_data_range) +
        ggplot2::coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]),
          expand = TRUE) +
        fishset_theme() +
        ggplot2::theme(legend.key.size = unit(1, "cm"),
          legend.background = ggplot2::element_rect(fill = "grey90"))
      
      out
    }
    
    z_plot_fun_dynamic <- function(spatdat, legend_name) {
    
      spatdat <-  sf::st_transform(spatdat, "+proj=longlat +datum=WGS84")
      
      pal <- colorBin(
        palette = "viridis",
        bins = 10,
        domain = spatdat[[var_sym]] # colors depend on the count variable
      )
      
      fill_colors <- pal(spat_join[[var_sym]])
      
      # Leaflet map
      leaflet::leaflet() %>%
        leaflet::addProviderTiles("OpenStreetMap") %>%
        leaflet::addPolygons(data =  spatdat,
          fillColor = ~fill_colors,
          fillOpacity = 0.75,
          color = "black",
          stroke = TRUE,
          weight = 0.5,
          layerId = ~var_sym,
          group = "Polygons",
          label = ~paste0(val_var,": ", round(spatdat[[var_sym]],2))) %>%
        addCircleMarkers(
          data = dataset,
          lng = ~dataset[[dat_lon]], lat = ~dataset[[dat_lat]],
          popup = ~var_sym,
          color = "red",
          radius= 3,
          stroke = FALSE, fillOpacity = 0.5,
          label = ~paste0(val_var,": ", round(spatdat[[var_sym]],2)),
          group = "Points"
        ) %>%
        # Add the layer control to manage both groups
        addLayersControl(
          overlayGroups = c("Polygons", "Points"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>% 
        leaflet::addLegend(pal = pal,
          values = spatdat[[var_sym]],
          position = "bottomright",
          title = legend_name) %>% 
        hideGroup("Points")
    }
    
    if (multi_plot) {
      
      create_layered_map <- function(multdat, group, legend_name) {
        
        multdat <-  sf::st_transform(multdat, "+proj=longlat +datum=WGS84")
        
         if (count) {
            # update legend to include group name
            if (calc_perc) {legend_name <- paste0("% of total obs")
            }else {legend_name <- paste("# of obs ")}
            
          } else {
            
            if (calc_perc) {legend_name <- paste0("% of ", var)
            } else {legend_name <- paste0(fun, " ", var)}
          }
        
        pal <- colorBin(
          palette = "viridis",
          bins = 10,
          domain = multdat[[var_sym]] # colors depend on the count variable
        )
        
        fill_colors <- pal(multdat[[var_sym]])
        
        my_map <- leaflet(data = multdat) %>%
          addProviderTiles("OpenStreetMap")
        
        p_levels <- unique(multdat[[group]])
        for (level in p_levels) {
          
          # Filter the data for the current group
          dat_subset <- multdat %>%
            filter(.data[[group]] == level)
          
          my_map <- my_map %>%
            leaflet::addPolygons(
              data = dat_subset,
              fillColor = ~fill_colors, # Use the palette to get color
              fillOpacity = 0.75,
              color = "black",
              stroke = TRUE,
              weight = 1,
              label = ~paste0(val_var, ": ", round(multdat[[var_sym]], 2)),
              group = level # Assign the layer to the current group
            )
        }
        
        my_map <- my_map %>%
          leaflet::addLayersControl(
            overlayGroups = p_levels,
            options = layersControlOptions(collapsed = FALSE)
          ) %>%
          leaflet::addLegend(
            pal = pal,
            values =multdat[[var_sym]], # Legend uses values from the whole dataset
            position = "bottomright",
            title = legend_name
          )
        my_map
      } 
      
      if(plot_type == "dynamic"){
        z_plot <- create_layered_map(spat_join, group,legend_name = legend_name)
        
      } else{ 
        #static plot
        p_levels <- unique(spat_join[[group]]) # what if too many levels?
        z_plot <- lapply(p_levels, function(x) {
          dat <- dplyr::filter(spat_join, .data[[group]] == !!x)
  
          if (count) {
            # update legend to include group name
            if (calc_perc) {legend_name <- paste0("% of total obs: \n ", x)
            }else {legend_name <- paste("# of obs: \n ", x)}
            
          } else {
            
            if (calc_perc) {legend_name <- paste0("% of ", var, ": \n", x)
            } else {legend_name <- paste0(fun, " ", var, ": \n", x)}
          }
          
            Zone <- dat[[zone.spat]] 
           z_plot <-  suppressWarnings(z_plot_fun_static(dat,legend_name = legend_name))
     
          
        })
        
        z_plot
      } 
      # save plot
      save_nplot(project, "zone_summary", z_plot)
     
    } else {
      
      if(plot_type == "dynamic"){
        z_plot <- z_plot_fun_dynamic(spat_join, legend_name = legend_name)
        
      } else{ 
        #static plot
        z_plot <- suppressWarnings(z_plot_fun_static(spat_join, legend_name = legend_name))

      }    
      # save plot
      save_plot(project, "zone_summary", z_plot)
    }
    
    # confid plot ----
    if (check_c && any(check_out$suppress)) {

      zone_tab_c[[zone.dat]] <- as.character(zone_tab_c[[zone.dat]])
      spatdat[[zone.spat]] <- as.character(spatdat[[zone.spat]])
      
      spat_join_c <- merge_spat(zone_tab_c)
      spat_join <- spat_join_c %>% dplyr::filter(.data[[val_var]] != -999)
      

      if (multi_plot) {
        
      z_plot <- create_layered_map(spat_join, group,legend_name = legend_name)

        # save plot
        save_nplot(project, "zone_summary_confid", z_plot)
        
      } else {
        if(plot_type == "dynamic"){
          z_plot <- suppressWarnings(z_plot_fun_dynamic(spat_join,
            legend_name = legend_name))
        } else{
          z_plot <- suppressWarnings(z_plot_fun_static(spat_join, 
            legend_name = legend_name))
        }
        # save plot
        save_plot(project, "zone_summary_confid", z_plot)
      }
    }
  }
  
  # save table
  save_table(zone_tab, project, "zone_summary")
  
  # log function
  zone_summary_function <- list()
  zone_summary_function$functionID <- "zone_summary"
  zone_summary_function$args <- list(dat, spat, project, zone.dat, zone.spat,
    count, var, group, fun, na.rm, dat.center, output)
  log_call(project, zone_summary_function)
  
  if (output == "plot") z_plot
  else if (output == "tab_plot") list(table = zone_tab, plot = z_plot)
  else zone_tab
}

