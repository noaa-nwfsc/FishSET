#' Hausman-McFadden Test for IIA
#'
#' Performs the Hausman-McFadden specification test to check the Independence of 
#' Irrelevant Alternatives (IIA) assumption for logit models.
#'
#' The test compares the estimates from a full model (fitted on all alternatives) 
#' against a restricted model (fitted on a subset of alternatives). If the IIA assumption 
#' holds, the coefficients common to both models should not be systematically different.
#'
#' @param project Character string. Name of the project.
#' @param model_name Character string. Name of the specific model design used.
#' @param fit_name Character string. Name of the full model fit object previously 
#'   saved in the project database (created by \code{\link{fishset_fit}}).
#' @param omitted_zones Character vector (Optional). The names of the zones (alternatives) to 
#'   exclude from the restricted model. These must match the zone labels found in 
#'   the original data. If NULL (default), a random zone is selected.
#' @param robust Logical. Default FALSE. If TRUE, uses numerically stable utility values,
#'   mirroring the fit in \code{\link{fishset_fit}}.
#' @param ... Additional arguments passed to the optimization of the restricted model 
#'   (e.g., \code{control}, \code{start_values}).
#'
#' @return A list object of class \code{"fishset_iia"} containing:
#' \describe{
#'   \item{statistic}{The Hausman chi-squared test statistic.}
#'   \item{p_value}{The p-value of the test.}
#'   \item{df}{Degrees of freedom (number of coefficients compared).}
#'   \item{full_coefs}{Coefficients from the full model (subsetted for comparison).}
#'   \item{restricted_coefs}{Coefficients from the restricted model.}
#'   \item{description}{Text interpretation of the result.}
#' }
#'
#' @details 
#' A significant p-value (typically < 0.05) indicates that the IIA assumption has been 
#' violated, suggesting that the logit model may be misspecified (e.g., 
#' unobserved correlation between alternatives). Note: Alternative-Specific Constants 
#' (ASCs) are excluded from the statistical comparison to prevent reference-level shift bias.
#'
#' @export
#' @importFrom RTMB MakeADFun sdreport
#' @importFrom stats pchisq nlminb
#' @importFrom DBI dbConnect dbDisconnect dbExecute
#' @importFrom RSQLite SQLite

fishset_iia_test <- function(project,
                             model_name,
                             fit_name = NULL,
                             omitted_zones = NULL,
                             robust = FALSE,
                             ...) {
  
  # Helper functions ------------------------------------------------------------------------------
  # Helper: Clean collinear/constant columns from logit design matrix
  clean_logit_matrix <- function(mat, group_ids) {
    if (ncol(mat) == 0) return(mat)
    mat_dense <- as.matrix(mat)
    
    # Demean by choice task to find true variation
    g_means <- rowsum(mat_dense, group_ids) / as.vector(table(group_ids))
    mat_demean <- mat_dense - g_means[as.character(group_ids), , drop = FALSE]
    
    # Check for zero variance
    col_vars <- apply(mat_demean, 2, stats::var)
    keep_idx <- which(col_vars > 1e-10)
    
    if (length(keep_idx) == 0) return(mat[, integer(0), drop = FALSE])
    
    # Check for linear independence
    mat_sub <- mat_demean[, keep_idx, drop = FALSE]
    qr_dec <- qr(mat_sub)
    indep_idx <- qr_dec$pivot[seq_len(qr_dec$rank)]
    
    final_cols <- colnames(mat_sub)[indep_idx]
    return(mat[, final_cols, drop = FALSE])
  }
  
  # Helper: Clean Linear design matrix (EPM Catch)
  clean_linear_matrix <- function(mat) {
    if (ncol(mat) == 0) return(mat)
    mat_dense <- as.matrix(mat)
    
    # Drop pure zero columns (e.g. omitted dummies)
    col_max <- apply(abs(mat_dense), 2, max)
    keep_idx <- which(col_max > 1e-10)
    
    if (length(keep_idx) == 0) return(mat[, integer(0), drop = FALSE])
    
    # Check for linear independence
    mat_sub <- mat_dense[, keep_idx, drop = FALSE]
    qr_dec <- qr(mat_sub)
    indep_idx <- qr_dec$pivot[seq_len(qr_dec$rank)]
    
    final_cols <- colnames(mat_sub)[indep_idx]
    return(mat[, final_cols, drop = FALSE])
  }
  
  # Load the full model fit -----------------------------------------------------------------------
  if (is.null(fit_name)) fit_name <- paste0(model_name, "_fit")
  
  tryCatch({
    full_fit_list <- unserialize_table(paste0(project, "ModelFit"), project)
  }, error = function(cond) {
    stop("Could not load ModelFit table. Run fishset_fit() first.")
  })
  
  if (!(fit_name %in% names(full_fit_list))) {
    stop(paste0("Fit object '", fit_name, "' not found in project database."))
  }
  
  full_fit <- full_fit_list[[fit_name]]
  
  # Retrieve the original design ------------------------------------------------------------------
  paths <- file.path(locproject(), 
                     project, 
                     "Models", 
                     "ModelDesigns", 
                     c(paste0(model_name, ".qs2"), paste0(model_name, ".rds")))
  found_path <- paths[file.exists(paths)][1]
  
  if (is.na(found_path)) {
    stop(paste0("Design file '", model_name, "' not found. Make sure fishset_design() was run."))
  }
  
  # Load based on extension
  if (grepl("\\.qs2$", found_path)) {
    if (!requireNamespace("qs2", quietly = TRUE)) stop("Install 'qs2' to read this design.")
    full_design <- qs2::qs_read(found_path)
  } else {
    full_design <- readRDS(found_path)
  }
  
  is_epm <- isTRUE(full_design$epm$is_epm)
  
  # Validation: check omitted zone(s) -------------------------------------------------------------
  all_zones <- unique(as.character(full_design$ids$zone))
  
  if (is.null(omitted_zones)) {
    # Randomly select one zone to omit
    omitted_zones <- sample(all_zones, 1)
  }
  
  if (!all(omitted_zones %in% all_zones)) {
    stop("One or more 'omitted_zones' not found in the original dataset.")
  }
  
  if (length(omitted_zones) >= (length(all_zones) - 1)) {
    stop("You must keep at least two alternatives in the restricted set.")
  }
  
  # Construct restricted data ---------------------------------------------------------------------
  # Logical mask for rows to KEEP (zones NOT in omitted_zones)
  keep_mask <- !(as.character(full_design$ids$zone) %in% omitted_zones)
  
  # Subset Data
  y_restricted_raw <- full_design$y[keep_mask]
  obs_id_restricted <- full_design$ids$obs[keep_mask]

  # Remove observations where the chosen zone was an omitted zone
  # Sum across each obs_id_restricted - if 0 then omitted_zone(s) were selected for that occasion.
  valid_obs_check <- tapply(y_restricted_raw, obs_id_restricted, sum)
  valid_obs_ids <- names(valid_obs_check)[valid_obs_check == 1]
  
  # Filter data again to keep only valid observations
  final_mask <- obs_id_restricted %in% valid_obs_ids
  obs_id_final <- obs_id_restricted[final_mask]
  
  y_restr <- as.numeric(y_restricted_raw[final_mask])
  chosen_lin_idx_restr <- which(y_restr == 1)
  
  # Update Dimensions
  N_restr <- length(unique(obs_id_final))
  J_restr <- length(all_zones) - length(omitted_zones)
  
  # Base param arrays from full fit
  b_full <- full_fit$opt$par
  
  # Model routing setup (standard vs EPM) ---------------------------------------------------------
  if (!is_epm) {
    # Standard conditional logit
    X_restr_raw <- full_design$X[keep_mask, , drop = FALSE][final_mask, , drop = FALSE]
    
    # Safely clean out dropped ASCs and collinear reference levels
    X_restr <- clean_logit_matrix(X_restr_raw, obs_id_final)
    
    if (ncol(X_restr) == 0) stop("No identifiable parameters remain in the restricted model.")
    
    data_list <- list(X = X_restr,
                      chosen_lin_idx = chosen_lin_idx_restr,
                      N_obs = as.integer(N_restr),
                      J_alts = as.integer(J_restr))
    
    # Naming the parameters for the full model so they map perfectly
    names(b_full) <- colnames(full_design$X)
    
    dots <- list(...)
    if ("start_values" %in% names(dots)) {
      init_beta <- dots$start_values
    } else {
      common_start_pars <- intersect(names(b_full), colnames(X_restr))
      init_beta <- rep(0.0001, ncol(X_restr))
      names(init_beta) <- colnames(X_restr)
      init_beta[common_start_pars] <- b_full[common_start_pars]
    }
    
    pars_list <- list(betas = as.numeric(init_beta))
    
    # Define NLL - standard logit
    nll_func_restr <- function(pars) {
      RTMB::getAll(data_list, pars)
      v <- X %*% betas
      v_chosen <- v[chosen_lin_idx]
      dim(v) <- c(J_alts, N_obs) # Reshape J x N
      
      if (robust) {
        v_max <- v[1, ]
        for(j in 2:J_alts) {
          v_max <- (v_max + v[j, ] + abs(v_max - v[j, ])) * 0.5
        }
        log_sum_exp <- log(RTMB::rowSums(exp(t(v) - v_max))) + v_max
      } else {
        log_sum_exp <- log(RTMB::colSums(exp(v)))
      }
      return(-sum(v_chosen - log_sum_exp))
    }
    
  } else {
    # Expected profit model
    
    # Infer distribution if EPM
    if (is_epm) {
      if (any(grepl("Sigma_Catch", names(full_fit$coefficients)))) distribution <- "normal"
      else if (any(grepl("Sdlog_Catch", names(full_fit$coefficients)))) distribution <- "lognormal"
      else if (any(grepl("Shape_Catch", names(full_fit$coefficients)))) distribution <- "weibull"
      else stop("EPM requires a distribution. Please specify it.")
    }
    dist_code <- switch(distribution, "normal" = 1, "lognormal" = 2, "weibull" = 3)
    
    # Subsetting components
    X_catch_raw <- full_design$epm$X_catch[keep_mask, , drop = FALSE][final_mask, , drop = FALSE]
    X_catch_restr <- clean_linear_matrix(X_catch_raw)
    
    Y_catch_restr <- full_design$epm$Y_catch[keep_mask][final_mask]
    prices_restr <- full_design$epm$price_vec[keep_mask][final_mask]
    
    util_vars <- setdiff(colnames(full_design$X), colnames(full_design$epm$X_catch))
    if (length(util_vars) > 0) {
      X_util_raw <- full_design$X[keep_mask, util_vars, drop = FALSE][final_mask, , drop = FALSE]
      X_util_restr <- clean_logit_matrix(X_util_raw, obs_id_final)
    } else {
      X_util_restr <- matrix(0, nrow = length(Y_catch_restr), ncol = 0)
    }
    
    data_list <- list(
      Y_catch_chosen = as.double(as.vector(Y_catch_restr[chosen_lin_idx_restr])),
      X_util = X_util_restr, 
      X_catch = X_catch_restr, 
      prices = prices_restr,
      chosen_lin_idx = chosen_lin_idx_restr, 
      N_obs = as.integer(N_restr),
      J_alts = as.integer(J_restr),
      zone_id = ((chosen_lin_idx_restr - 1) %% J_restr) + 1,
      zone_seq = ((0:(nrow(X_catch_restr) - 1)) %% J_restr) + 1,
      dist_code = dist_code
    )
    
    # Parameter naming maps for EPM
    n_c_full <- ncol(full_design$epm$X_catch)
    n_u_full <- length(util_vars)
    names(b_full)[1:(n_c_full + n_u_full)] <- c(colnames(full_design$epm$X_catch), util_vars)
    
    # Smart initializations for restricted optimization
    n_c <- ncol(X_catch_restr)
    n_u <- ncol(X_util_restr)
    
    # Smart initializations
    beta_c_init <- rep(0.0001, n_c)
    names(beta_c_init) <- colnames(X_catch_restr)
    c_pars <- intersect(names(b_full), colnames(X_catch_restr))
    beta_c_init[c_pars] <- b_full[c_pars]
    
    beta_u_init <- rep(0.0001, n_u)
    if (n_u > 0) {
      names(beta_u_init) <- colnames(X_util_restr)
      u_pars <- intersect(names(b_full), colnames(X_util_restr))
      beta_u_init[u_pars] <- b_full[u_pars]
    }
    
    kept_zones_idx <- which(all_zones %in% all_zones[!(all_zones %in% omitted_zones)])
    log_sig_c_full <- full_fit$opt$par[grep("log_sigma_c", names(full_fit$opt$par))]
    
    pars_list <- list(
      beta_catch = as.numeric(beta_c_init),
      beta_util = as.numeric(beta_u_init),
      log_sigma_c = as.numeric(log_sig_c_full[kept_zones_idx]),
      log_sigma_e = as.numeric(full_fit$opt$par[grep("log_sigma_e", names(full_fit$opt$par))])
    )
    
    # Define NLL - EPM
    nll_func_restr <- function(pars) {
      RTMB::getAll(data_list, pars)
      sigma_c <- exp(log_sigma_c)
      sigma_e <- exp(log_sigma_e)
      lin_pred <- X_catch %*% beta_catch
      lin_pred_chosen <- lin_pred[chosen_lin_idx]
      sigma_c_chosen <- sigma_c[zone_id]
      sigma_c_full <- sigma_c[zone_seq]
      
      if (dist_code == 1) {
        E_catch <- lin_pred
        nll_cont <- -sum(RTMB::dnorm(Y_catch_chosen, lin_pred_chosen, sigma_c_chosen, log = TRUE))
      } else if (dist_code == 2) {
        E_catch <- exp(lin_pred + 0.5 * sigma_c_full^2)
        nll_cont <- -sum(RTMB::dlnorm(Y_catch_chosen, lin_pred_chosen, sigma_c_chosen, log = TRUE))
      } else {
        scale_chosen <- exp(lin_pred[chosen_lin_idx])
        E_catch <- exp(lin_pred) * exp(lgamma(1 + 1/sigma_c_full))
        nll_cont <- -sum(RTMB::dweibull(
          Y_catch_chosen, shape = sigma_c_chosen, scale = scale_chosen, log = TRUE))
      }
      
      revenue_util <- prices * E_catch
      cost_util <- if (ncol(X_util) > 0) X_util %*% beta_util else 0
      
      v <- (1 / sigma_e) * (revenue_util + cost_util)
      v_chosen <- v[chosen_lin_idx]
      dim(v) <- c(J_alts, N_obs)
      
      if (robust) {
        v_max <- v[1, ]
        for(j in 2:J_alts) {
          v_max <- (v_max + v[j, ] + abs(v_max - v[j, ])) * 0.5
        }
        log_sum_exp <- log(RTMB::rowSums(exp(t(v) - v_max))) + v_max
      } else {
        log_sum_exp <- log(RTMB::colSums(exp(v)))
      }
      return(nll_cont - sum(v_chosen - log_sum_exp))
    }
  }
  
  # Build the full model covariance matrix --------------------------------------------------------
  if (is.null(full_fit$diagnostics$hessian) || any(is.na(full_fit$diagnostics$hessian))) {
    stop(paste0("The full model fit does not contain a valid Hessian matrix.",
                " Run fishset_fit() wit se_calc = TRUE."))
  }
  
  V_full <- tryCatch({
    solve(full_fit$diagnostics$hessian)
  }, error = function(e) {
    stop("Could not invert full model Hessian.")
  })
  rownames(V_full) <- names(b_full)
  colnames(V_full) <- names(b_full)
  
  # Optimize restricted model ---------------------------------------------------------------------
  param_count <- if (!is_epm) ncol(X_restr) else (ncol(X_catch_restr) + ncol(X_util_restr))
  use_sparse_hess <- (param_count >= 50)
  TMB::config(tmbad.sparse_hessian_compress = use_sparse_hess, DLL="RTMB")
  
  obj <- RTMB::MakeADFun(func = nll_func_restr, 
                         parameters = pars_list, 
                         data = data_list, 
                         silent = TRUE)
  
  default_ctrl <- list(eval.max = 1000, iter.max = 1000)
  dots <- list(...)
  if("control" %in% names(dots)) {
    usr_ctrl <- modifyList(default_ctrl, dots$control)
  } else {
    usr_ctrl <- default_ctrl
  }
  
  opt <- stats::nlminb(obj$par, 
                       obj$fn, 
                       obj$gr, 
                       control = usr_ctrl)
  b_restr <- opt$par
  
  # Name params strictly
  if (!is_epm) {
    names(b_restr) <- colnames(X_restr)
  } else {
    names(b_restr)[1:(n_c + n_u)] <- c(colnames(X_catch_restr), colnames(X_util_restr))
  }
  
  # Hausman-McFadden test -------------------------------------------------------------------------
  # Calculate restricted covariance (Inverse Hessian)
  tryCatch({
    k_len <- length(opt$par)
    hessian_mat <- if (k_len < 50) stats::optimHess(opt$par, obj$fn, obj$gr) else obj$he(opt$par)
    V_restr <- solve(hessian_mat)
    rownames(V_restr) <- names(b_restr)
    colnames(V_restr) <- names(b_restr)
  }, error = function(e) { V_restr <<- matrix(NA, k_len, k_len) })
  
  zone_var <- full_design$settings$zone_id
  
  # Only compare the structurally similar vars
  common_pars <- intersect(names(b_full), names(b_restr))
  common_pars <- common_pars[!grepl("log_sigma", common_pars)]
  
  # Exclude ASCs to prevent reference level shift bias
  if (!is.null(zone_var) && zone_var != "") {
    common_pars <- common_pars[!grepl(zone_var, common_pars)]
  }
  
  if (length(common_pars) == 0) {
    stop("No common parameters found between full and restricted models.")
  }
  
  # Subset vectors and matrices
  b_f_sub <- b_full[common_pars]
  b_r_sub <- b_restr[common_pars]
  V_f_sub <- V_full[common_pars, common_pars]
  V_r_sub <- V_restr[common_pars, common_pars]
  
  # Statistic: H = (b_r - b_f)' * (V_r - V_f)^-1 * (b_r - b_f)
  b_diff <- b_r_sub - b_f_sub
  V_diff <- V_r_sub - V_f_sub
  
  # Use generalized inverse if available, else standard solve
  if (requireNamespace("MASS", quietly = TRUE)) {
    V_diff_inv <- MASS::ginv(V_diff)
  } else {
    V_diff_inv <- solve(V_diff)
  }
  
  H_stat <- as.numeric(t(b_diff) %*% V_diff_inv %*% b_diff)
  df <- length(b_diff)
  p_val <- pchisq(H_stat, df, lower.tail = FALSE)
  
  # Log function call -----------------------------------------------------------------------------
  fishset_iia_function <- list()
  fishset_iia_function$functionID <- "fishset_iia_test"
  fishset_iia_function$args <- as.list(match.call())[-1]
  fishset_iia_function$kwargs <- list()
  
  log_call(project, fishset_iia_function)
  
  # Return result ---------------------------------------------------------------------------------
  res <- list(
    statistic = H_stat,
    p_value = p_val,
    df = df,
    omitted = omitted_zones,
    common_parameters = common_pars,
    full_coefs = b_f_sub,
    restricted_coefs = b_r_sub,
    description = ifelse(p_val < 0.05, 
                         "Significant difference (IIA Likely Violated)", 
                         "No significant difference (IIA Tentatively Supported)"),
    is_epm = is_epm
  )
  
  class(res) <- "fishset_iia"
  return(res)
}

#' Print Hausman-McFadden IIA Test Results
#'
#' Formats and prints the output of a Hausman-McFadden test for the Independence 
#' of Irrelevant Alternatives (IIA) assumption within the FishSET framework. 
#' Displays the omitted alternatives, the Chi-squared test statistic, degrees of 
#' freedom, the calculated p-value, and a clear text interpretation of the result
#' (including custom diagnostic interpretations for Expected Profit Models).
#'
#' @param x A \code{fishset_iia} object returned by \code{\link{fishset_iia_test}}.
#' @param ... Additional arguments passed to other methods (currently ignored).
#'
#' @method print fishset_iia
#' @export
print.fishset_iia <- function(x, ...) {
  cat("\nHausman-McFadden IIA Test\n")
  cat("----------------------------------------------------------------\n")
  cat("Omitted Alternative Name(s): ", paste(x$omitted, collapse=", "), "\n")
  cat("Chi-squared Statistic:  ", round(x$statistic, 3), "\n")
  cat("Degrees of Freedom:     ", x$df, "\n")
  cat("P-value:                ", format.pval(x$p_value, eps=0.001), "\n")
  
  # Check if the model is an EPM
  if (isTRUE(x$is_epm)) {
    cat("Model Type:             Expected Profit Model (Joint)\n")
    cat("----------------------------------------------------------------\n")
    
    if (x$p_value < 0.05) {
      cat("Result: Significant difference detected.\n\n")
      cat("EPM Interpretation:\n")
      cat("Because the EPM evaluates continuous catch and discrete choice\n")
      cat("simultaneously, a failure here indicates AT LEAST ONE of the\n")
      cat("following issues:\n")
      cat("  1. A true IIA violation (unobserved spatial correlation \n")
      cat("     between fishing zones in the utility equation).\n")
      cat("  2. Structural misspecification or sample selection bias in \n")
      cat("     the continuous catch equation.\n")
    } else {
      cat("Result: No significant difference detected.\n\n")
      cat("EPM Interpretation:\n")
      cat("The IIA assumption is tentatively supported (run multiple times to confirm.\n")
      cat("Furthermore, this implies that BOTH your continuous catch equation and\n")
      cat("discrete choice utility equation are robust and well-specified.\n")
    }
    
  } else {
    # Standard conditional logit printout
    cat("Result:                 ", x$description, "\n")
  }
  
  cat("----------------------------------------------------------------\n")
  invisible(x)
}