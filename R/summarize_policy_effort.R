#' Summarize and Plot Policy Effort Redistribution
#'
#' Extracts and visualizes the spatial redistribution of fishing effort.
#' 
#' @details 
#' \strong{What does "Effort" represent?}
#' 
#' In this simulation framework, "effort" represents the expected number of fishing 
#' choice occasions (e.g., trips or hauls) allocated to each spatial zone. 
#'
#' @param project Character. Name of the project.
#' @param spat Character. Name of the spatial dataset containing the fishing zones.
#' @param zone_spat Character. The ID column in the spatial data matching simulation zone IDs.
#'
#' @return A list containing \code{summary_data} and three nested lists of \code{ggplot} objects:
#'   \code{plots_absolute_map}, \code{plots_percent_map}, and \code{plots_scatter}.
#' @export
#' @import ggplot2
#' @importFrom sf st_as_sf st_bbox

summarize_policy_effort <- function(project, spat, zone_spat) {
  
  # Load policy simulations and spatial data ------------------------------------------------------
  table_name <- paste0(project, "PolicySimulations")
  sim_list <- tryCatch({ unserialize_table(table_name, project) }, error = function(e) list())
  if (length(sim_list) == 0) stop("No policy simulations found.")
  
  spat_out <- data_pull(spat, project)
  spat_sf <- check_spatdat(spat_out$dataset, id = zone_spat)
  
  # Extract and compile effort vectors ------------------------------------------------------------
  effort_df_list <- lapply(names(sim_list), function(sim_name) {
    res <- sim_list[[sim_name]]$results
    base_effort <- res$effort_base
    new_effort <- res$effort_new
    
    # Calculate Percentage Change cleanly (avoiding division by zero)
    pct_change <- ifelse(base_effort == 0, NA, ((new_effort - base_effort) / base_effort) * 100)
    
    data.frame(
      Simulation = sim_name,
      Scenario = sim_list[[sim_name]]$scenario,
      Zone = names(base_effort),
      Baseline_Effort = unname(base_effort),
      Counterfactual_Effort = unname(new_effort),
      Effort_Change = unname(new_effort - base_effort),
      Pct_Effort_Change = unname(pct_change),
      row.names = NULL
    )
  })
  
  effort_df <- do.call(rbind, effort_df_list)
  
  # Determine plot bounding box -------------------------------------------------------------------
  modeled_sf <- spat_sf[spat_sf[[zone_spat]] %in% effort_df$Zone, ]
  bbox <- sf::st_bbox(modeled_sf)
  buffer_pct <- 0.2
  x_range <- bbox["xmax"] - bbox["xmin"]
  y_range <- bbox["ymax"] - bbox["ymin"]
  x_limits <- c(bbox["xmin"] - (x_range * buffer_pct), bbox["xmax"] + (x_range * buffer_pct))
  y_limits <- c(bbox["ymin"] - (y_range * buffer_pct), bbox["ymax"] + (y_range * buffer_pct))
  
  # Generate spatial and scatter plots ------------------------------------------------------------
  plot_abs <- list()
  plot_pct <- list()
  plot_scatter <- list()
  
  sim_names <- unique(effort_df$Simulation)
  
  for (s_name in sim_names) {
    sub_data <- effort_df[effort_df$Simulation == s_name, ]
    merged_spat <- merge(spat_sf, sub_data, by.x = zone_spat, by.y = "Zone", all.x = TRUE)
    closed_idx <- which(merged_spat$Counterfactual_Effort == 0 & merged_spat$Baseline_Effort > 0)
    
    # Map theme
    map_theme <- list(
      ggplot2::coord_sf(xlim = x_limits, ylim = y_limits, expand = FALSE),
      ggplot2::theme_classic(),
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold"),
        axis.text = ggplot2::element_blank(), 
        axis.ticks = ggplot2::element_blank(),
        axis.line = ggplot2::element_blank(),
        panel.border = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 1)
      )
    )
    
    ## Absolute change map ----
    p_a <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = merged_spat, ggplot2::aes(fill = Effort_Change), 
                       color = "black", linewidth = 0.2) +
      ggplot2::scale_fill_viridis_c(option = "viridis", na.value = "white", 
                                    name = "Change in\nEffort") +
      ggplot2::labs(title = paste("Absolute Effort Redistribution:", s_name), 
                    subtitle = "Net change in expected trips/hauls by zone")
    
    ## Percentage change map ----
    p_p <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = merged_spat, ggplot2::aes(fill = Pct_Effort_Change), 
                       color = "black", linewidth = 0.2) +
      ggplot2::scale_fill_viridis_c(na.value = "white", name = "% Change\nin Effort") +
      ggplot2::labs(title = paste("Relative Effort Redistribution:", s_name), 
                    subtitle = "Percentage change in expected trips/hauls relative to baseline")
    
    # Apply closures and themes to maps
    if (length(closed_idx) > 0) {
      closed_spat <- merged_spat[closed_idx, ]
      closure_layer <- list(
        ggplot2::geom_sf(data = closed_spat, ggplot2::aes(color = "Closed Area"), 
                         fill = "tomato", linewidth = 0.2),
        ggplot2::scale_color_manual(name = NULL, values = c("Closed Area" = "black")),
        ggplot2::guides(color = ggplot2::guide_legend(override.aes = 
                                                        list(fill = "tomato", color = "black")))
      )
      p_a <- p_a + closure_layer
      p_p <- p_p + closure_layer
    }
    
    plot_abs[[s_name]] <- p_a + map_theme
    plot_pct[[s_name]] <- p_p + map_theme
    
    ## Spillover scatter plot ----
    p_s <- ggplot2::ggplot(sub_data, ggplot2::aes(x = Baseline_Effort, 
                                                  y = Counterfactual_Effort)) +
      ggplot2::geom_abline(intercept = 0, slope = 1, 
                           linetype = "dashed", color = "grey50", linewidth = 1) +
      ggplot2::geom_point(ggplot2::aes(fill = Effort_Change), 
                          shape = 21, size = 3, color = "black", alpha = 0.8) +
      ggplot2::scale_fill_viridis_c(option = "viridis", name = "Net Change") +
      ggplot2::labs(
        title = paste("Effort Spillover Dynamics:", s_name),
        subtitle = "Points above the line gained effort; points below lost effort.",
        x = "Baseline Expected Effort (Trips/Hauls)",
        y = "Counterfactual Expected Effort (Trips/Hauls)"
      ) +
      ggplot2::theme_classic() +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
    
    plot_scatter[[s_name]] <- p_s
  }
  
  # Return data and plots -------------------------------------------------------------------------
  return(list(
    summary_data = effort_df,
    plots_absolute_map = plot_abs,
    plots_percent_map = plot_pct,
    plots_scatter = plot_scatter
  ))
}