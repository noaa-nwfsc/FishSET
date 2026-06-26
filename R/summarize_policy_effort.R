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
#' @param output_type Character. Either "static" (ggplot2) or "dynamic" (leaflet/plotly). 
#'   Default is "static".
#' @param plot_scenarios Character vector (optional). Specific Scenario names to include.
#' @param plot_models Character vector (optional). Specific Model names to include.
#' @param plotly_source Character (optional). Unique ID for plotly cross-talk mapping.
#'
#' @return A list containing \code{summary_data} and three nested lists of plot objects.
#' @export
summarize_policy_effort <- function(project, spat, zone_spat, output_type = "static", 
                                    plot_scenarios = NULL, plot_models = NULL,
                                    plotly_source = "effort_scatter") {
  
  # Validate output type
  output_type <- tolower(output_type)
  if(!(output_type %in% c("static", "dynamic"))) {
    stop("The 'output_type' should be 'static' or 'dynamic'. Check input spelling.")
  }
  
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
      Model = sim_list[[sim_name]]$model_name,
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
  
  # Filter data to determine which plots to generate ----------------------------------------------
  plot_data <- effort_df
  
  # Filter for specific models if requested
  if (!is.null(plot_models)) {
    plot_data <- plot_data[plot_data$Model %in% plot_models, ]
    if (nrow(plot_data) == 0) {
      stop("None of the provided 'plot_models' were found in the data.")
    }
  }
  
  # Filter for specific scenarios if requested
  if (!is.null(plot_scenarios)) {
    scen_string <- paste(plot_scenarios, collapse = "|")
    filtered_scenarios <- grep(scen_string, plot_data$Scenario)
    plot_data <- plot_data[filtered_scenarios, ]
    if (nrow(plot_data) == 0) {
      stop("None of the provided 'plot_scenarios' were found in the data.")
    }
  }
  
  sim_names_to_plot <- unique(plot_data$Simulation)
  
  # Determine plot bounding box (for static map only) ---------------------------------------------
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
  
  for (s_name in sim_names_to_plot) {
    sub_data <- effort_df[effort_df$Simulation == s_name, ]
    merged_spat <- merge(spat_sf, sub_data, by.x = zone_spat, by.y = "Zone", all.x = TRUE)
    
    # Partition Data: Identify closed vs open areas for mapping
    closed_idx_map <- which(merged_spat$Counterfactual_Effort == 0 & 
                              merged_spat$Baseline_Effort > 0)
    if (length(closed_idx_map) > 0) {
      open_spat <- merged_spat[-closed_idx_map, ]
      closed_spat <- merged_spat[closed_idx_map, ]
    } else {
      open_spat <- merged_spat
      closed_spat <- NULL
    }
    
    # Partition Data: Identify closed vs open areas for the scatter plot
    closed_idx_scatter <- which(sub_data$Counterfactual_Effort == 0 & 
                                  sub_data$Baseline_Effort > 0)
    if (length(closed_idx_scatter) > 0) {
      open_sub <- sub_data[-closed_idx_scatter, ]
      closed_sub <- sub_data[closed_idx_scatter, ]
    } else {
      open_sub <- sub_data
      closed_sub <- NULL
    }
    
    ## static figures (ggplot2) -------------------------------------------------------------------
    if (output_type == "static") {
      
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
      
      ## Absolute change map
      p_a <- ggplot2::ggplot() +
        ggplot2::geom_sf(data = open_spat, ggplot2::aes(fill = Effort_Change), 
                         color = "black", linewidth = 0.2) +
        ggplot2::scale_fill_viridis_c(option = "viridis", na.value = "white", 
                                      name = "Change in\nEffort") +
        ggplot2::labs(title = paste("Absolute Effort Redistribution:", s_name), 
                      subtitle = "Net change in expected trips/hauls by zone")
      
      ## Percentage change map
      p_p <- ggplot2::ggplot() +
        ggplot2::geom_sf(data = open_spat, ggplot2::aes(fill = Pct_Effort_Change), 
                         color = "black", linewidth = 0.2) +
        ggplot2::scale_fill_viridis_c(na.value = "white", name = "% Change\nin Effort") +
        ggplot2::labs(title = paste("Relative Effort Redistribution:", s_name), 
                      subtitle = "Percentage change in expected trips/hauls relative to baseline")
      
      if (!is.null(closed_spat)) {
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
      
      ## Scatter plot
      p_s <- ggplot2::ggplot() +
        ggplot2::geom_abline(intercept = 0, slope = 1, 
                             linetype = "dashed", color = "grey50", linewidth = 1) +
        ggplot2::geom_point(data = open_sub, ggplot2::aes(x = Baseline_Effort, 
                                                          y = Counterfactual_Effort,
                                                          fill = Effort_Change), 
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
      
      if (!is.null(closed_sub)) {
        p_s <- p_s + 
          ggplot2::geom_point(data = closed_sub, ggplot2::aes(x = Baseline_Effort, 
                                                              y = Counterfactual_Effort,
                                                              color = "Closed Area"), 
                              fill = "tomato", shape = 21, size = 3, alpha = 0.8) +
          ggplot2::scale_color_manual(name = NULL, values = c("Closed Area" = "black")) +
          ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(fill = "tomato", 
                                                                            color = "black")))
      }
      
      plot_scatter[[s_name]] <- p_s
      
      ## dynamic figures (leaflet & plotly) -------------------------------------------------------
    } else if (output_type == "dynamic") {
      
      # Transform to WGS84 for leaflet
      open_spat_wgs <- sf::st_transform(open_spat, 4326)
      if (!is.null(closed_spat)) closed_spat_wgs <- sf::st_transform(closed_spat, 4326)
      
      ## HTML Title block for Absolute Map
      html_title_abs <- paste0(
        "<div style='background-color: rgba(255, 255, 255, 0.9); padding: 5px 10px;",
        " border-radius: 4px; border: 1px solid #ccc;'>",
        " <strong style='font-size: 14px;'>Absolute Effort Redistribution: ", s_name, 
        " </strong><br>", "<span style='font-size: 12px; color: #555;'>Net change in",
        " expected trips/hauls by zone</span>",
        "</div>"
      )
      
      ## Dynamic Absolute change map (Leaflet)
      abs_clean <- na.omit(open_spat_wgs$Effort_Change)
      if (length(abs_clean) > 0 && max(abs_clean) == min(abs_clean)) {
        domain_abs <- c(abs_clean[1] - 1, abs_clean[1] + 1)
      } else {
        domain_abs <- open_spat_wgs$Effort_Change
      }
      
      pal_abs <- leaflet::colorNumeric(palette = "viridis", domain = domain_abs,
                                       na.color = "#e5e5e5")
      
      l_a <- leaflet::leaflet()
      l_a <- leaflet::addProviderTiles(l_a, leaflet::providers$CartoDB.Positron)
      l_a <- leaflet::addPolygons(l_a, data = open_spat_wgs,
                                  layerId = open_spat_wgs[[zone_spat]], # Added Layer ID for targeting clicks
                                  fillColor = ~pal_abs(Effort_Change),
                                  fillOpacity = 0.7, color = "black", weight = 1,
                                  popup = ~paste("<b>Zone:</b>", 
                                                 open_spat_wgs[[zone_spat]], 
                                                 "<br><b>Abs Change:</b>", 
                                                 round(Effort_Change, 2)))
      
      l_a <- leaflet::addLegend(l_a, pal = pal_abs, values = na.omit(domain_abs),
                                title = "Net Effort Change", position = "bottomright")
      
      l_a <- leaflet::addControl(l_a, html = html_title_abs, position = "topright")
      
      if (!is.null(closed_spat)) {
        l_a <- leaflet::addPolygons(l_a, data = closed_spat_wgs,
                                    layerId = closed_spat_wgs[[zone_spat]],
                                    fillColor = "tomato",
                                    fillOpacity = 0.7, color = "black", weight = 1,
                                    popup = ~paste("<b>Zone:</b>", closed_spat_wgs[[zone_spat]], 
                                                   "<br><b>Status:</b> Closed"))
        
        l_a <- leaflet::addLegend(l_a, colors = "tomato", labels = "Closed Area", 
                                  position = "bottomright")
      }
      
      plot_abs[[s_name]] <- l_a
      
      ## HTML Title block for Percentage Map
      html_title_pct <- paste0(
        "<div style='background-color: rgba(255, 255, 255, 0.9); padding: 5px 10px; ",
        "border-radius: 4px; border: 1px solid #ccc;'>",
        "<strong style='font-size: 14px;'>Relative Effort Redistribution: ", s_name, 
        "</strong><br>", "<span style='font-size: 12px; color: #555;'>Percentage change ",
        "in expected trips/hauls relative to baseline</span>", "</div>"
      )
      
      ## Dynamic Percentage change map (Leaflet)
      pct_clean <- na.omit(open_spat_wgs$Pct_Effort_Change)
      if (length(pct_clean) > 0 && max(pct_clean) == min(pct_clean)) {
        domain_pct <- c(pct_clean[1] - 1, pct_clean[1] + 1)
      } else {
        domain_pct <- open_spat_wgs$Pct_Effort_Change
      }
      
      pal_pct <- leaflet::colorNumeric(palette = "viridis", domain = domain_pct, 
                                       na.color = "#e5e5e5")
      
      l_p <- leaflet::leaflet()
      l_p <- leaflet::addProviderTiles(l_p, leaflet::providers$CartoDB.Positron)
      l_p <- leaflet::addPolygons(l_p, data = open_spat_wgs,
                                  layerId = open_spat_wgs[[zone_spat]], # Added Layer ID for targeting clicks
                                  fillColor = ~pal_pct(Pct_Effort_Change),
                                  fillOpacity = 0.7, color = "black", weight = 1,
                                  popup = ~paste("<b>Zone:</b>", open_spat_wgs[[zone_spat]], 
                                                 "<br><b>% Change:</b>", 
                                                 round(Pct_Effort_Change, 2), "%"))
      
      l_p <- leaflet::addLegend(l_p, pal = pal_pct, values = na.omit(domain_pct),
                                title = "% Effort Change", position = "bottomright")
      
      l_p <- leaflet::addControl(l_p, html = html_title_pct, position = "topright")
      
      if (!is.null(closed_spat)) {
        l_p <- leaflet::addPolygons(l_p, data = closed_spat_wgs,
                                    layerId = closed_spat_wgs[[zone_spat]],
                                    fillColor = "tomato",
                                    fillOpacity = 0.7, color = "black", weight = 1,
                                    popup = ~paste("<b>Zone:</b>", closed_spat_wgs[[zone_spat]], 
                                                   "<br><b>Status:</b> Closed"))
        
        l_p <- leaflet::addLegend(l_p, colors = "tomato", labels = "Closed Area", 
                                  position = "bottomright")
      }
      
      plot_pct[[s_name]] <- l_p
      
      ## Dynamic Scatter plot (Plotly)
      max_val <- max(c(sub_data$Baseline_Effort, sub_data$Counterfactual_Effort), na.rm = TRUE)
      
      # Tied specific unique source ID to this plot to capture standard crosstalk
      p_s_dyn <- plotly::plot_ly(source = plotly_source)
      p_s_dyn <- plotly::add_lines(p_s_dyn, x = c(0, max_val), y = c(0, max_val), 
                                   line = list(dash = "dash", color = "grey"), 
                                   name = "1:1 Reference", hoverinfo = "none")
      
      p_s_dyn <- plotly::add_markers(p_s_dyn, data = open_sub, 
                                     x = ~Baseline_Effort, y = ~Counterfactual_Effort,
                                     color = ~Effort_Change, colors = "viridis",
                                     customdata = ~Zone, # Embedded zone variable into the marker point
                                     marker = list(size = 8, line = list(color = 'black', 
                                                                         width = 1)),
                                     text = ~paste("Zone:", Zone,
                                                   "<br>Baseline:", 
                                                   round(Baseline_Effort, 2),
                                                   "<br>Counterfactual:", 
                                                   round(Counterfactual_Effort, 2),
                                                   "<br>Net Change:", 
                                                   round(Effort_Change, 2)),
                                     hoverinfo = "text",
                                     name = "Open Zones")
      
      if (!is.null(closed_sub)) {
        p_s_dyn <- plotly::add_markers(p_s_dyn, data = closed_sub, 
                                       x = ~Baseline_Effort, y = ~Counterfactual_Effort,
                                       customdata = ~Zone, # Embedded zone variable here too
                                       marker = list(color = "tomato", size = 8, 
                                                     line = list(color = 'black', width = 1)),
                                       text = ~paste("Zone:", Zone,
                                                     "<br>Baseline:", round(Baseline_Effort, 2),
                                                     "<br>Status: Closed"),
                                       hoverinfo = "text",
                                       name = "Closed Areas")
      }
      
      p_s_dyn <- plotly::layout(p_s_dyn, 
                                title = list(
                                  text = paste0("<b>Effort Spillover Dynamics: ", s_name, 
                                                "</b><br>", "<sup style='color:#555;'>Points ",
                                                "above the line gained effort; points below lost ",
                                                "effort.</sup>")
                                ),
                                xaxis = list(title = "Baseline Expected Effort"),
                                yaxis = list(title = "Counterfactual Expected Effort"),
                                margin = list(t = 60))
      
      p_s_dyn <- plotly::event_register(p_s_dyn, 'plotly_click')
      p_s_dyn <- plotly::event_register(p_s_dyn, 'plotly_doubleclick')

      plot_scatter[[s_name]] <- p_s_dyn
    }
  }
  
  # Return data and plots 
  return(list(
    summary_data = plot_data,
    plots_absolute_map = plot_abs,
    plots_percent_map = plot_pct,
    plots_scatter = plot_scatter
  ))
}