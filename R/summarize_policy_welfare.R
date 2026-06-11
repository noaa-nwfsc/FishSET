#' Summarize and Plot Policy Welfare Impacts
#'
#' Extracts and visualizes the expected economic welfare changes across 
#' all simulated policy scenarios, including statistical uncertainty.
#' 
#' @details 
#' \strong{What does "Welfare Change" represent?}
#' 
#' In this simulation framework, "welfare change" represents the Compensating Variation (CV) 
#' resulting from a policy shock, expressed in real-world monetary units (e.g., dollars).
#' 
#' It is calculated using the log-sum difference formula from random utility theory. 
#' This metric captures not just the direct penalty of a closure or a drop in 
#' catch, but also the mitigating effect of spatial substitution—how fishers adapt by 
#' reallocating their effort to the next-best available fishing grounds.
#'
#' @param project Character. Name of the project.
#' @param plot_scenarios Character vector. Optional. Specific Scenario or Simulation names 
#'   to include in the plots. If \code{NULL} (the default), scenarios are not filtered.
#' @param plot_models Character vector. Optional. Specific Model names to include in 
#'   the plots (e.g., "zonal_logit", "clogit"). If \code{NULL} (the default), models are not
#'   filtered.
#'
#' @return A list containing three elements: \code{summary_data} (a data frame of welfare impacts), 
#'   \code{plot_bar} (mean per-trip changes), and \code{plot_density} (distribution of uncertainty).
#' @export
#' @import ggplot2

summarize_policy_welfare <- function(project, plot_scenarios = NULL, plot_models = NULL) {
  
  # Load policy simulations -----------------------------------------------------------------------
  table_name <- paste0(project, "PolicySimulations")
  
  sim_list <- tryCatch({
    unserialize_table(table_name, project)
  }, error = function(e) {
    list()
  })
  
  if (length(sim_list) == 0) {
    stop("No policy simulations found in the database. Run run_simulation() first.")
  }
  
  # Extract and compile welfare metrics and draws -------------------------------------------------
  welfare_df_list <- list()
  draws_df_list <- list()
  
  for (sim_name in names(sim_list)) {
    res <- sim_list[[sim_name]]$results
    meta <- sim_list[[sim_name]]$metadata
    
    # Standard Summary
    welfare_df_list[[sim_name]] <- data.frame(
      Simulation = sim_name,
      Model = sim_list[[sim_name]]$model_name,
      Scenario = sim_list[[sim_name]]$scenario,
      Mean_Welfare_Per_Trip = res$mean_welfare_loss,
      Lower_95 = res$quantiles["2.5%"],
      Median = res$quantiles["50%"],
      Upper_95 = res$quantiles["97.5%"],
      row.names = NULL
    )
    
    # Raw Draws for Density Plot
    if (sim_list[[sim_name]]$scenario != "baseline") {
      draws_df_list[[sim_name]] <- data.frame(
        Simulation = sim_name,
        Model = sim_list[[sim_name]]$model_name,
        Scenario = sim_list[[sim_name]]$scenario, 
        Welfare_Change = res$welfare_draws,
        row.names = NULL
      )
    }
  }
  
  welfare_df <- do.call(rbind, welfare_df_list)
  draws_df <- do.call(rbind, draws_df_list)
  
  # Format data for plotting ----------------------------------------------------------------------
  plot_data <- welfare_df[welfare_df$Scenario != "baseline", ]
  
  # Filter for specific models if requested
  if (!is.null(plot_models)) {
    plot_data <- plot_data[plot_data$Model %in% plot_models, ]
    draws_df <- draws_df[draws_df$Model %in% plot_models, ]
    
    if (nrow(plot_data) == 0) {
      warning("None of the provided 'plot_models' were found in the data. Plots will be empty.")
    }
  }
  
  # Filter for specific scenarios if requested
  if (!is.null(plot_scenarios)) {
    plot_data <- plot_data[plot_data$Scenario %in% plot_scenarios | 
                             plot_data$Simulation %in% plot_scenarios, ]
    draws_df <- draws_df[draws_df$Scenario %in% plot_scenarios | 
                           draws_df$Simulation %in% plot_scenarios, ]
    
    if (nrow(plot_data) == 0) {
      warning("None of the provided 'plot_scenarios' were found in the remaining data. 
              Plots will be empty.")
    }
  }
  
  # Order simulations by severity of mean impact
  sim_levels <- plot_data$Simulation[order(plot_data$Mean_Welfare_Per_Trip)]
  plot_data$Simulation <- factor(plot_data$Simulation, levels = sim_levels)
  
  # Ensure draws_df factor levels match plot_data exactly to avoid plotting issues
  if (nrow(draws_df) > 0) {
    draws_df$Simulation <- factor(draws_df$Simulation, levels = sim_levels)
  }
  
  # Generate the bar plot -------------------------------------------------------------------------
  p_bar <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Simulation, y = Mean_Welfare_Per_Trip, 
                                                   fill = Model)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.8) +
    ggplot2::geom_col(color = "black", alpha = 0.8, width = 0.7) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = Lower_95, ymax = Upper_95), width = 0.25, 
                           linewidth = 0.6) +
    ggplot2::scale_fill_viridis_d(option = "cividis", begin = 0.3, end = 0.8) +
    ggplot2::labs(
      title = "Expected Welfare Impacts of Policy Scenarios",
      subtitle = "Mean compensating variation per trip/haul (with 95% intervals)",
      x = "Simulation Scenario",
      y = "Mean Welfare Change Per Occasion (Monetary Units)"
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      legend.position = "top",
      axis.text = ggplot2::element_text(color = "black")
    )
  
  # Generate the density (violin) plot ------------------------------------------------------------
  p_density <- ggplot2::ggplot(draws_df, ggplot2::aes(x = Simulation, y = Welfare_Change, 
                                                      fill = Model)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.8) +
    ggplot2::geom_violin(color = "black", alpha = 0.6, trim = FALSE) +
    ggplot2::geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
    ggplot2::scale_fill_viridis_d(option = "cividis", begin = 0.3, end = 0.8) +
    ggplot2::labs(
      title = "Welfare Uncertainty Distribution",
      subtitle = "Full distribution of simulated welfare draws showing skew and risk",
      x = "Simulation Scenario",
      y = "Welfare Change Per Occasion (Monetary Units)"
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      legend.position = "none",
      axis.text = ggplot2::element_text(color = "black")
    )
  
  # Return data and plots -------------------------------------------------------------------------
  return(list(
    summary_data = welfare_df, 
    plot_bar = p_bar,
    plot_density = p_density
  ))
}