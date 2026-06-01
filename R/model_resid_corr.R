#' Check Spatial Autocorrelation of Model Residuals
#'
#' Calculates zone-level residuals from a fitted FishSET model, tests for spatial 
#' autocorrelation using Moran's I, and generates a spatial plot of the residuals.
#'
#' @param project Character string. Name of the project.
#' @param model_name Character string. Name of the specific model design used.
#' @param spat Character string of spatial table name in project database OR \code{sf} polygon 
#'   object containing the spatial boundaries of the fishing zones.
#' @param spat_id Character string. The name of the column in \code{spat} that 
#'   matches the zone identifiers used in the model design.
#' @param fit_name Character string (Optional). Name of the model fit object. 
#'   Defaults to \code{paste0(model_name, "_fit")}.
#' @param distribution Character string (Optional). Distribution for the continuous 
#'   catch component in EPMs. Required if evaluating an EPM.
#'
#' @return A list of class \code{"fishset_spatial_resid"} containing:
#' \describe{
#'   \item{moran_test}{The results of the Moran's I test from \code{spdep}.}
#'   \item{residual_map}{A \code{ggplot2} object mapping the spatial residuals.}
#'   \item{zonal_residuals}{A dataframe of the calculated mean residuals per zone.}
#'   \item{spatial_data}{The merged \code{sf} object containing geometries and residuals.}
#' }
#' 
#' @export
#' @importFrom Matrix colSums

model_resid_corr <- function(project,
                             model_name,
                             spat,
                             spat_id,
                             fit_name = NULL,
                             distribution = NULL) {
  
  # Load data and check dependencies --------------------------------------------------------------
  # Check dependencies
  req_pkgs <- c("sf", "spdep", "ggplot2")
  missing_pkgs <- req_pkgs[!vapply(req_pkgs, requireNamespace, 
                                   quietly = TRUE, 
                                   FUN.VALUE = logical(1))]
  if (length(missing_pkgs) > 0) {
    stop(paste("The following packages are required for spatial analysis:", 
               paste(missing_pkgs, collapse = ", "), 
               ". Please install them."))
  }
  
  # Load Model Fit
  if (is.null(fit_name)) fit_name <- paste0(model_name, "_fit")
  
  tryCatch({
    full_fit_list <- unserialize_table(paste0(project, "ModelFit"), project)
  }, error = function(cond) {
    stop("Could not load ModelFit table. Run fishset_fit() first.")
  })
  
  if (!(fit_name %in% names(full_fit_list))) {
    stop(paste0("Fit object '", fit_name, "' not found in project database."))
  }
  fit <- full_fit_list[[fit_name]]
  
  # Load Model Design
  designs_dir <- file.path(locproject(), project, "Models", "ModelDesigns")
  base_path_qs2 <- file.path(designs_dir, paste0(model_name, ".qs2"))
  base_path_rds <- file.path(designs_dir, paste0(model_name, ".rds"))
  
  if (file.exists(base_path_qs2) && requireNamespace("qs2", quietly = TRUE)) {
    design <- qs2::qs_read(base_path_qs2)
  } else if (file.exists(base_path_rds)) {
    design <- readRDS(base_path_rds)
  } else {
    stop("Model design not found in project folders.")
  }
  
  # Pull spatial data if needed
  spat_out <- data_pull(spat, project)
  spatdat <- spat_out$dataset
   
  # Check/build the sf object, then transform to Leaflet CRS
  spat <- check_spatdat(spatdat, id = spat_id)
  
  # Predict probabilities -------------------------------------------------------------------------
  is_epm <- isTRUE(design$epm$is_epm)
  J <- design$settings$J_alts
  N <- design$settings$N_obs
  
  if (!is_epm && !is.null(distribution)) {
    message("Note: the distribution input will be ignored for standard logit models.")
  }
  
  if (is_epm && is.null(distribution)) {
    stop("EPMs require the 'distribution' argument to calculate residuals.")
  }
  
  if (!is_epm) {
    final_v <- as.vector(design$X %*% fit$opt$par)
    dim(final_v) <- c(J, N)
    v_max <- apply(final_v, 2, max)
    exp_v <- exp(t(t(final_v) - v_max))
    prob_mat_t <- t(exp_v) / colSums(exp_v)
    
  } else {
    final_par <- fit$opt$par
    n_c <- ncol(design$epm$X_catch)
    b_c <- final_par[1:n_c]
    
    util_vars <- setdiff(colnames(design$X), colnames(design$epm$X_catch))
    if (length(util_vars) > 0) {
      X_util <- design$X[, util_vars, drop = FALSE]
    } else {
      X_util <- matrix(0, nrow = length(design$epm$Y_catch), ncol = 0)
    }
    b_u <- if (length(util_vars) > 0) final_par[(n_c + 1):(n_c + ncol(X_util))] else numeric(0)
    
    l_sig_e <- final_par[grep("log_sigma_e", names(final_par))]
    l_sig_c <- final_par[grep("log_sigma_c", names(final_par))]
    
    lin_pred <- design$epm$X_catch %*% b_c
    zone_seq <- ((0:(length(lin_pred) - 1)) %% J) + 1
    sig_c_full <- exp(l_sig_c)[zone_seq]
    
    if (distribution == "normal") { 
      mu_catch <- lin_pred
    } else if (distribution == "lognormal") { 
      mu_catch <- exp(lin_pred + 0.5 * sig_c_full^2)
    } else if (distribution == "weibull") { 
      mu_catch <- exp(lin_pred) * exp(lgamma(1 + 1/sig_c_full)) 
    }
    
    rev_u <- design$epm$price_vec * mu_catch
    cost_u <- if (length(b_u) > 0) X_util %*% b_u else 0
    
    v <- as.matrix((1 / exp(l_sig_e)) * (rev_u + cost_u))
    dim(v) <- c(J, N)
    
    v_max <- apply(v, 2, max)
    exp_v <- exp(t(t(v) - v_max))
    prob_mat_t <- t(exp_v) / colSums(exp_v)
  }
  
  # Calculate residuals for each zone -------------------------------------------------------------
  # Y matrix (N x J) of actual choices
  y_mat <- matrix(0, nrow = N, ncol = J)
  chosen_lin_idx <- which(design$y == 1)
  choice_idx_report <- (chosen_lin_idx - 1) %% J + 1
  y_mat[cbind(1:N, choice_idx_report)] <- 1
  
  # Residuals = Actual - Predicted
  resid_mat <- y_mat - prob_mat_t
  
  # Average residual per zone
  mean_zonal_resid <- colMeans(resid_mat)
  
  # Extract zone names exactly as they were formatted in the design
  zone_names <- levels(as.factor(design$ids$zone))
  resid_df <- data.frame(
    zone_id = zone_names,
    mean_residual = mean_zonal_resid,
    stringsAsFactors = FALSE
  )
  
  
  # Merge with spatial data -----------------------------------------------------------------------
  if (!(spat_id %in% names(spat))) {
    stop(paste("Column", spat_id, "not found in the provided spat object."))
  }
  
  # Standardize merge column types
  spat[[spat_id]] <- as.character(spat[[spat_id]])
  # Group by your zone ID and merge the geometries together
  spat_clean <- suppressWarnings(suppressMessages({
    spat %>%
      # Fix any slightly broken geometries first (helps prevent st_union crashes)
      st_make_valid() %>%
      group_by(!!sym(spat_id)) %>%
      # is_coverage = TRUE heavily speeds up the unioning of adjacent borders
      summarize(geometry = st_union(geometry, is_coverage = TRUE), .groups = "drop")
  }))
  
  merged_sf <- merge(spat_clean, resid_df, by.x = spat_id, by.y = "zone_id", all.x = FALSE)
  
  if (nrow(merged_sf) != J) {
    warning("Mismatch between the number of zones in the model and the sf object. ",
            "Check for missing geometries or unmatched zone names.")
  }

  
  # Calculate Moran's I ---------------------------------------------------------------------------
  # Create neighborhood and weights (Queen contiguity by default)
  nb <- suppressMessages(suppressWarnings(spdep::poly2nb(merged_sf))) 
  
  # Check for islands (zones with no neighbors)
  total_edges <- sum(spdep::card(nb))
  has_islands <- any(spdep::card(nb) == 0)
  n_connected <- sum(spdep::card(nb) > 0)
  
  if (has_islands && total_edges > 0) {
    warning(paste0("Some zones have no spatial neighbors (islands).",
                   "They will be evaluated with zero.policy = TRUE."))
  }
  
  # Check for zero variance (e.g., Models with ASCs)
  resid_var <- var(merged_sf$mean_residual, na.rm = TRUE)
  
  # Evaluate guardrails before passing to moran.test
  if (total_edges == 0) {
    message("Notice: No spatial neighbors detected across the entire map (all zones are islands). ",
            "Moran's I requires at least some connected borders. Skipping test.")
    moran_res <- list(estimate = c("Moran I statistic" = NA), p.value = NA)
    
  } else if (n_connected <= 3) {  
    message("Notice: Too few connected zones (", n_connected, ") to calculate a statistically ",
            "meaningful Moran's I. Skipping test.")
    moran_res <- list(estimate = c("Moran I statistic" = NA), p.value = NA)
    
  } else if (n_connected < 20) {
    warning("Moran's I is an asymptotic test. With fewer than 20 connected zones (", 
            n_connected, "), the p-value may be unreliable.")
    lw <- spdep::nb2listw(nb, style = "W", zero.policy = TRUE)
    moran_res <- spdep::moran.test(merged_sf$mean_residual, lw, zero.policy = TRUE)
    
  } else if (is.na(resid_var) || resid_var < 1e-10) {
    message("Notice: Zonal residual variance is near zero. This can happen when Area-Specific ",
            "Constants (ASCs) perfectly absorb aggregate zonal variation. ",
            "Skipping Moran's I test.")
    moran_res <- list(estimate = c("Moran I statistic" = NA), p.value = NA)
    
  } else {
    # If all checks pass, run the test
    lw <- spdep::nb2listw(nb, style = "W", zero.policy = TRUE)
    moran_res <- spdep::moran.test(merged_sf$mean_residual, lw, zero.policy = TRUE)
  }
  
  # Generate Plot ---------------------------------------------------------------------------------
  # Calculate max absolute residual to center the color scale around 0
  max_abs <- suppressWarnings(max(abs(merged_sf$mean_residual), na.rm = TRUE))
  
  # Edge case handling: if all residuals are NA
  if (is.infinite(max_abs) || is.na(max_abs) || max_abs == 0) {
    max_abs <- 1  # Provide a safe default limit so the plot renders without crashing
  }
  
  p <- ggplot2::ggplot(data = merged_sf) +
    ggplot2::geom_sf(ggplot2::aes(fill = mean_residual), color = "black", linewidth = 0.2) +
    ggplot2::scale_fill_distiller(
      palette = "RdBu", 
      direction = -1, 
      limits = c(-max_abs, max_abs),
      name = "Mean\nResidual"
    ) +
    ggplot2::theme_classic() +
    ggplot2::labs(
      title = "Spatial Distribution of Model Residuals",
      subtitle = paste0("Model: ", model_name, " | Moran's I: ", 
                        round(moran_res$estimate[1], 3), 
                        " (p = ", format.pval(moran_res$p.value, eps = 0.001), ")")
    )
  
  # Return Results --------------------------------------------------------------------------------
  out <- list(
    moran_test = moran_res,
    residual_map = p,
    zonal_residuals = resid_df,
    spatial_data = merged_sf
  )
  
  class(out) <- "fishset_spatial_resid"
  return(out)
}

#' Print FishSET Spatial Residuals
#'
#' @param x A \code{fishset_spatial_resid} object.
#' @param ... Additional arguments.
#' @export
print.fishset_spatial_resid <- function(x, ...) {
  cat("\nFishSET Spatial Residual Analysis\n")
  cat("========================================================\n")
  cat("Moran's I Statistic: ", round(x$moran_test$estimate[1], 4), "\n")
  cat("P-value:             ", format.pval(x$moran_test$p.value, eps = 0.001), "\n\n")
  cat("========================================================\n")
  cat("Note: You can view the residual map by plotting this object: plot(result)\n")
  invisible(x)
}

#' Plot FishSET Spatial Residuals
#'
#' @param x A \code{fishset_spatial_resid} object.
#' @param ... Additional arguments.
#' @export
plot.fishset_spatial_resid <- function(x, ...) {
  print(x$residual_map)
}