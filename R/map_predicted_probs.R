#' Map FishSET Predicted Probabilities
#' 
#' Maps the predicted probabilities from a fitted FishSET model using either a 
#' static ggplot map or an interactive leaflet map. Integrates directly with 
#' FishSET project databases for data loading and saving.
#'
#' @param fit_name Character string. Name of the specific model fit saved in 
#'   the project's 'ModelFit' table.
#' @param spat A spatial data file name or object containing information on fishery 
#'   management or regulatory zones boundaries.
#' @param project Character string. Name of the project.
#' @param zone_spat Name of zone ID column in `spat`. Must match the zone IDs 
#'   modeled in the probabilities.
#' @param obs_index Integer (Optional). If provided, maps the predicted probabilities 
#'   for a specific observation (row). If `NULL` (default), maps the average 
#'   predicted probability for each zone across all observations.
#' @param dat_center Logical, whether the static plot should center on the zones 
#'   present in the model predictions (`TRUE`) or the entire spatial dataset (`FALSE`).
#' @param plot_type Type of plot output; dynamic (`"dynamic"`) using leaflet 
#'   or static (`"static"`) using ggplot2.
#' @param output Output a `"plot"`, `"table"`, or both (`"tab_plot"`). 
#'   Defaults to `"plot"`.
#' 
#' @export
#' @import ggplot2
#' @import dplyr
#' @import sf
#' @import leaflet
#' @importFrom rlang sym
#' @importFrom htmltools HTML
map_predicted_probs <- function(fit_name,
                                spat,
                                project,
                                zone_spat,
                                obs_index = NULL,
                                dat_center = TRUE,
                                plot_type = "dynamic",
                                output = "plot") {
  
  # 1. Input Validation and Missing Checks --------------------------------------
  if (missing(fit_name)) stop("Argument 'fit_name' is missing. Please provide the name of the
                              model fit.")
  if (missing(spat)) stop("Argument 'spat' is missing. Please provide the spatial data.")
  if (missing(project)) stop("Argument 'project' is missing. Please provide the project name.")
  if (missing(zone_spat)) stop("Argument 'zone_spat' is missing. Please provide the zone ID 
                               column name.")
  
  if (length(fit_name) > 1) {
    stop("Please provide only a single model name for 'fit_name' 
         (e.g., fit_name = 'clogit1_fit').")
  }
  
  # Pull in model fit -----------------------------------------------------------
  full_fit_list <- tryCatch({
    unserialize_table(paste0(project, "ModelFit"), project)
  }, error = function(e) list())
  
  if (!(fit_name %in% names(full_fit_list))) {
    stop(paste0("Model fit '", fit_name, "' not found. Available fits: ", 
                paste(names(full_fit_list), collapse = ", ")))
  }
  
  fit <- full_fit_list[[fit_name]]
  
  if (is.null(fit$prob_matrix)) {
    stop("The fit object does not contain 'prob_matrix'. Please re-run fishset_fit() with 
         return_full_prob_mat = TRUE.")
  }
  
  # Pull in spatial dataset -----------------------------------------------------
  spatout <- data_pull(spat, project)
  spatdat <- spatout$dataset
  spat_name <- parse_data_name(spat, "spat", project)
  
  if (is.null(spatdat)) {
    stop("The spatial dataset failed to load. Check that the file exists in the project.")
  }
  
  if (!(zone_spat %in% colnames(spatdat))) {
    stop(sprintf("The Zone ID column '%s' was NOT found in the spatial dataset. Available 
                 columns are: %s", 
                 zone_spat, paste(colnames(spatdat), collapse = ", ")))
  }
  
  # Calculate probabilities -----------------------------------------------------
  if (is.null(obs_index)) {
    probs <- colMeans(fit$prob_matrix, na.rm = TRUE)
    val_var <- "mean_prob"
    legend_name <- "Avg Predicted\nProbability"
  } else {
    if (obs_index < 1 || obs_index > nrow(fit$prob_matrix)) {
      stop("obs_index is out of bounds for the probability matrix.")
    }
    probs <- fit$prob_matrix[obs_index, ]
    val_var <- paste0("prob_obs_", obs_index)
    legend_name <- paste("Predicted Prob\n(Obs", obs_index, ")")
  }
  
  # Build summary table 
  zone_names <- colnames(fit$prob_matrix)
  if (is.null(zone_names)) {
    warning("prob_matrix lacks column names. Assuming column index matches zone ID order.")
    zone_names <- as.character(1:ncol(fit$prob_matrix))
  }
  
  prob_tab <- data.frame(
    zone_id = as.character(zone_names),
    prob_val = as.numeric(probs),
    stringsAsFactors = FALSE
  )
  
  names(prob_tab)[names(prob_tab) == "zone_id"] <- zone_spat
  names(prob_tab)[names(prob_tab) == "prob_val"] <- val_var
  
  # Merge with Spatial Data -----------------------------------------------------
  if (output %in% c("plot", "tab_plot")) {
    
    prob_tab[[zone_spat]] <- as.character(prob_tab[[zone_spat]])
    spatdat[[zone_spat]] <- as.character(spatdat[[zone_spat]])
    
    spat_join <- dplyr::left_join(spatdat[zone_spat], prob_tab, by = zone_spat)
    
    # Check for empty join (Bad Zone IDs)
    if (all(is.na(spat_join[[val_var]]))) {
      stop("All joined probability values are NA. This means the Zone IDs in the model did 
           not match any Zone IDs in the spatial dataset.")
    }
    
    # ---> FILTER OUT NA ZONES HERE <---
    spat_join <- spat_join[!is.na(spat_join[[val_var]]), ]
    
    # use WGS 84 if crs is missing
    if (is.na(sf::st_crs(spatdat))) {
      spat_join <- sf::st_transform(spat_join, crs = 4326)
    }
    if (any(!(sf::st_is_valid(spatdat)))) {
      spat_join <- sf::st_make_valid(spat_join)
    }
    
    # Base map generation limits -----------------------------------
    if (dat_center) {
      z_ind <- spatdat[[zone_spat]] %in% unique(prob_tab[[zone_spat]])
      bbox <- sf::st_bbox(spatdat[z_ind, ])
    } else {
      bbox <- sf::st_bbox(spatdat) 
    }
    
    use_world2 <- shift_long(spatdat)
    map_name <- ifelse(use_world2, "world2", "world")
    
    x_limits <- c(bbox["xmin"], bbox["xmax"])
    if (use_world2) {
      x_limits <- ifelse(x_limits < 0, x_limits + 360, x_limits)
      x_limits <- sort(x_limits)
    }
    
    # Sub-functions for Plotting ------------------------------------------------
    var_sym <- rlang::sym(val_var)
    
   z_plot_fun_static <- function(spatdat, legend_name) {
      
      # Safely attempt to generate the base coastline map
      base_map <- tryCatch({
        ggplot2::map_data(map = map_name,
                          xlim = x_limits,
                          ylim = c(bbox["ymin"], bbox["ymax"]))
      }, error = function(e) {
        # If map_data fails (e.g., purely oceanic zones with no land), return an empty dataframe
        data.frame()
      })
      
      # Safety check to prevent the st_cast 'length zero' crash
      if (nrow(base_map) > 0) {
        base_map <- sf::st_as_sf(base_map, 
                                 coords = c("long", "lat"),
                                 crs = sf::st_crs(spatdat)) %>%
          dplyr::group_by(group) %>%
          dplyr::summarize(do_union = FALSE, .groups = "drop") %>%
          sf::st_cast("POLYGON")
        
        p <- ggplot2::ggplot() + ggplot2::geom_sf(data = base_map)
      } else {
        p <- ggplot2::ggplot() # Return empty base plot if in deep ocean
      }

      full_data_range <- range(spatdat[[val_var]], na.rm = TRUE)
      
      p <- p +
        ggplot2::geom_sf(data = spatdat,
                         ggplot2::aes(fill = !!var_sym),
                         color = "black", alpha = 0.8) +
        ggplot2::scale_fill_viridis_c(
          name = legend_name,
          limits = full_data_range,
          option = "plasma",
          na.value = "grey80"
        ) +
        ggplot2::coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]),
                          expand = TRUE) +
        fishset_theme() +
        ggplot2::theme(legend.key.size = unit(1, "cm"),
                       legend.background = ggplot2::element_rect(fill = "grey90"))
      
      return(p)
    }
    
    z_plot_fun_dynamic <- function(spatdat, legend_name) {
      
      spatdat <- sf::st_transform(spatdat, "+proj=longlat +datum=WGS84")
      
      pal <- colorBin(
        palette = "viridis",
        bins = 10,
        domain = spatdat[[var_sym]] 
      )
      
      fill_colors <- pal(spat_join[[var_sym]])
      
      hover_labels <- lapply(
        sprintf(
          "<strong>Zone ID:</strong> %s<br/><strong>Probability:</strong> %s",
          spatdat[[zone_spat]], 
          round(spatdat[[val_var]], 4)
        ),
        htmltools::HTML
      )
      
      leaflet::leaflet() %>%
        leaflet::addProviderTiles("OpenStreetMap") %>%
        leaflet::addPolygons(
          data = spatdat,
          fillColor = ~fill_colors,
          fillOpacity = 0.75,
          color = "black",
          stroke = TRUE,
          weight = 0.5,
          label = hover_labels,
          labelOptions = leaflet::labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "13px",
            direction = "auto"
          ),
          highlightOptions = leaflet::highlightOptions(
            weight = 2,
            color = "#666",
            bringToFront = TRUE
          )
        ) %>%
        leaflet::addLegend(
          pal = pal,
          values = spatdat[[val_var]],
          position = "bottomright",
          title = legend_name,
          opacity = 0.75
        )
    }
    
    # Generate and save Plot ----------------------------------------------------
    if (plot_type == "dynamic") {
      z_plot <- z_plot_fun_dynamic(spat_join, legend_name = legend_name)
    } else {
      z_plot <- suppressWarnings(z_plot_fun_static(spat_join, legend_name = legend_name))
    }
    
    save_plot(project, "map_predicted_probs", z_plot)
  }
  
  # Save table and log call -----------------------------------------------------
  save_table(prob_tab, project, "map_predicted_probs")
  
  map_probs_function <- list()
  map_probs_function$functionID <- "map_predicted_probs"
  map_probs_function$args <- list(fit_name, spat_name, project, zone_spat, 
                                  obs_index, dat_center, plot_type, output)
  log_call(project, map_probs_function)
  
  # Output return logic ---------------------------------------------------------
  if (output == "plot") return(z_plot)
  else if (output == "tab_plot") return(list(table = prob_tab, plot = z_plot))
  else return(prob_tab)
}