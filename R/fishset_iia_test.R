#' Hausman-McFadden Test for IIA
#'
#' Performs the Hausman-McFadden specification test to check the Independence of 
#' Irrelevant Alternatives (IIA) assumption for a conditional logit model.
#'
#' The test compares the estimates from a full model (fitted on all alternatives) 
#' against a restricted model (fitted on a subset of alternatives). If the IIA assumption 
#' holds, the coefficients common to both models should not be systematically different.
#'
#' @param project Character string. Name of the project.
#' @param fit_name Character string. Name of the full model fit object previously 
#'   saved in the project database (created by \code{\link{fishset_fit}}).
#' @param omitted_zones Character vector. The names of the zones (alternatives) to 
#'   exclude from the restricted model. These must match the zone labels found in 
#'   the original data.
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
#' violated, suggesting that the conditional logit model may be misspecified (e.g., 
#' unobserved correlation between alternatives).
#'
#' @export
#' @importFrom RTMB MakeADFun sdreport
#' @importFrom stats pchisq nlminb
#' @importFrom DBI dbConnect dbDisconnect dbExecute
#' @importFrom RSQLite SQLite

fishset_iia_test <- function(project,
                             fit_name,
                             omitted_zones,
                             ...) {
  
  # Load the full model fit -----------------------------------------------------------------------
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
  design_name <- full_fit$design_name
  
  if (is.null(design_name)) {
    stop("The fit object does not contain 'design_name'. Please re-run fishset_fit() with 
         the latest version.")
  }
  
  design_list <- unserialize_table(paste0(project, "ModelDesigns"), project)
  
  if (!(design_name %in% names(design_list))) {
    stop(paste0("Design object '", design_name, "' not found in ModelDesigns."))
  }
  
  full_design <- design_list[[design_name]]
  
  # Validation: check omitted zone(s) -------------------------------------------------------------
  # Get unique zone IDs from the design metadata
  all_zones <- unique(as.character(full_design$ids$zone))
  
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
  X_restricted_raw <- full_design$X[keep_mask, , drop = FALSE]
  obs_id_restricted <- full_design$ids$obs[keep_mask]
  
  # Clean choice sets
  # Remove observations where the fisher chose an OMITTED zone.
  # Sum across each obs_id_restricted - if 0 then omitted_zone(s) were selected for that occasion.
  valid_obs_check <- tapply(y_restricted_raw, obs_id_restricted, sum)
  valid_obs_ids <- names(valid_obs_check)[valid_obs_check == 1]
  
  # Filter data again to keep only valid observations
  final_mask <- obs_id_restricted %in% valid_obs_ids
  
  y_restr <- y_restricted_raw[final_mask]
  X_restr <- X_restricted_raw[final_mask, , drop = FALSE]
  
  # Update Dimensions
  N_restr <- length(unique(obs_id_restricted[final_mask]))
  J_restr <- length(all_zones) - length(omitted_zones)
  
  # Fit restricted model --------------------------------------------------------------------------
  # Prep data for RTMB
  y_vec <- as.numeric(y_restr)
  y_mat <- matrix(y_vec, nrow = N_restr, ncol = J_restr, byrow = TRUE)
  choice_idx_restr <- max.col(y_mat, ties.method = "first")
  
  X_clean <- as.matrix(X_restr)
  storage.mode(X_clean) <- "double"
  attr(X_clean, "dimnames") <- NULL
  
  data_list <- list(
    X = X_clean,
    choice_idx = choice_idx_restr, # Pass indices
    N_obs = as.integer(N_restr),
    J_alts = as.integer(J_restr)
  )
  
  # Handle start values (Use full model estimates for efficiency)
  dots <- list(...)
  if ("start_values" %in% names(dots)) {
    init_beta <- dots$start_values
  } else {
    init_beta <- rep(0.0001, ncol(X_clean))
  }
  pars_list <- list(betas = init_beta)
  
  # Define NLL (Standard RTMB logic from fishset_fit)
  nll_func_restr <- function(pars) {
    RTMB::getAll(data_list, pars)
    
    v <- X %*% betas
    dim(v) <- c(J_alts, N_obs) # Reshape J x N
    
    # Log-sum-exp (denominator)
    log_sum_exp <- log(RTMB::colSums(exp(v))) 
    
    # Numerator (Utility of chosen alternative)
    # Use the integer index to pick the specific value from the AD matrix
    chosen_utilities <- v[cbind(choice_idx, 1:N_obs)]
    
    nll_restr <- -sum(chosen_utilities - log_sum_exp)
    
    return(nll_restr)
  }
  
  # Optimize
  obj <- RTMB::MakeADFun(func = nll_func_restr, 
                         parameters = pars_list, 
                         data = data_list, 
                         silent = TRUE)
  
  default_ctrl <- list(eval.max = 1000, 
                       iter.max = 1000)
  
  if("control" %in% names(dots)) {
    usr_ctrl <- modifyList(default_ctrl, dots$control)
  } else {
    usr_ctrl <- default_ctrl
  }
  
  opt <- stats::nlminb(obj$par, 
                       obj$fn, 
                       obj$gr, 
                       control = usr_ctrl)
  
  # Hausman-McFadden test -------------------------------------------------------------------------
  # Add param names to the restricted model coefficients
  names(opt$par) <- colnames(X_restr)
  
  # Extract Beta vectors and Covariance Matrices
  b_full <- full_fit$opt$par
  b_restr <- opt$par
  
  V_full <- full_fit$vcov
  # Calculate restricted covariance (Inverse Hessian)
  tryCatch({
    V_restr <- solve(obj$he(opt$par))
    rownames(V_restr) <- colnames(X_restr)
    colnames(V_restr) <- colnames(X_restr)
  }, error = function(e) {
    matrix(NA, ncol(X_clean), ncol(X_clean))
  })
  
  # Identify common parameters by name
  names_full <- names(b_full)
  names_restr <- colnames(X_restr)
  common_pars <- intersect(names_full, names_restr)
  
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
                         "No significant difference (IIA Supported)")
  )
  
  class(res) <- "fishset_iia"
  return(res)
}

#' @export
print.fishset_iia <- function(x, ...) {
  cat("\nHausman-McFadden IIA Test\n")
  cat("------------------------------------------------\n")
  cat("Omitted Alternative(s): ", paste(x$omitted, collapse=", "), "\n")
  cat("Chi-squared Statistic:  ", round(x$statistic, 3), "\n")
  cat("Degrees of Freedom:     ", x$df, "\n")
  cat("P-value:                ", format.pval(x$p_value, eps=0.001), "\n")
  cat("Result:                 ", x$description, "\n")
  cat("------------------------------------------------\n")
}