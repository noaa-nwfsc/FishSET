#' Fit FishSET Discrete Choice Model
#'
#' Estimates parameters for logit models using the RTMB (R Template Model Builder) 
#' framework. This function takes a design object created by \code{\link{fishset_design}}, 
#' optimizes the negative log-likelihood, and returns a comprehensive list of model results, 
#' fit statistics, and diagnostics.
#'
#' @param project Character string. Name of the project.
#' @param model_name Character string. Name of the specific model design to fit. 
#'   Must match a name saved in the project's 'ModelDesigns' table.
#' @param fit_name Character string (Optional). Name to assign to the resulting fit object 
#'   in the database. Defaults to \code{paste0(model_name, "_fit")}.
#' @param distribution Character string. Distribution for the continuous catch component in EPMs.
#'   Options: \code{"normal"}, \code{"lognormal"}, \code{"weibull"}, default = NULL.
#' @param robust Logical. Default FALSE. If TRUE, uses numerically stable utility values.
#' @param return_full_prob_mat Logical. If TRUE, returns the full N_obs x J_alts matrix of
#'   probabilities for every alternative. Default is FALSE (returns only chosen probs)
#'   to save memory on large datasets.
#' @param se_calc Logical. Set \code{"se_calc" = TRUE} (default) to calculate standard errors.
#'   Set to FALSE for faster runtime during model selection.
#' @param ... Additional arguments passed to the optimization control.
#'   \itemize{
#'     \item \code{control}: A list of control parameters passed to \code{\link[stats]{nlminb}} 
#'     (e.g., \code{list(eval.max = 2000, iter.max = 2000)}).
#'     \item \code{start_values}: A numeric vector of initial parameter values. Must match 
#'     the number of predictors in the design matrix.
#'   }
#'
#' @return A list object of class \code{"fishset_fit"} containing, this list is also saved in 
#'   the project database:
#' \describe{
#'   \item{coefficients}{Named vector of estimated parameters.}
#'   \item{coef_table}{Data frame with Estimates, Std. Errors, Z-values, and P-values.}
#'   \item{vcov}{Variance-covariance matrix of the parameters.}
#'   \item{opt}{Raw optimization output from \code{nlminb}.}
#'   \item{logLik}{The maximum log-likelihood value of the fitted model.}
#'   \item{null_logLik}{The log-likelihood of a null model (random guessing).}
#'   \item{pseudo_R2}{McFadden's Pseudo-R-squared.}
#'   \item{AIC, AICc, BIC}{Information criteria for model comparison.}
#'   \item{accuracy}{The proportion of observations where the model assigned the highest 
#'     probability to the actual choice.}
#'   \item{fitted_values}{Vector of predicted probabilities for the chosen alternatives.}
#'   \item{prob_matrix}{Matrix of predicted probabilities for all alternatives (N_obs x J_alts).}
#'   \item{diagnostics}{A list containing the Hessian, gradients, eigenvalues, and 
#'     condition number.}
#' }
#' 
#' @examples
#' \dontrun{
#'   # 1. Standard fit using default settings
#'   # This uses the design object named "clogit_design" saved in "MyProject"
#'   fit_result <- fishset_fit(
#'     project = "MyProject",
#'     model_name = "clogit_design"
#'   )
#'   
#'   # 2. Advanced fit with custom optimization settings and start values
#'   # 'control' and 'start_values' are passed via the '...' argument
#'   fit_custom <- fishset_fit(
#'     project = "MyProject",
#'     model_name = "clogit_design",
#'     fit_name = "clogit_custom_fit",
#'     
#'     # Pass control list to nlminb (e.g., increase max iterations, turn on tracing)
#'     control = list(eval.max = 5000, iter.max = 5000, trace = 1),
#'     
#'     # Pass initial start values for the parameters (e.g., for 2 predictors)
#'     start_values = c(0.5, -0.2)
#'   )
#' }
#'
#' @seealso \code{\link{fishset_design}} for creating the input design object.
#'
#' @export
#' @importFrom RTMB MakeADFun sdreport getAll REPORT
#' @importFrom stats nlminb cov2cor pnorm pchisq
#' @importFrom Matrix colSums 

fishset_fit <- function(project,
                        model_name,
                        fit_name = NULL,
                        distribution = NULL,
                        robust = FALSE,
                        return_full_prob_mat = FALSE,
                        se_calc = TRUE,
                        ...) {
  
  # Setup and validate ----------------------------------------------------------------------------
  
  # Load project designs
  tryCatch({
    full_design_list <- model_design_list(project)
  }, error = function(cond) {
    message("Not able to load model designs. Run fishset_design() first.")
    return(NULL)
  })
  
  if (!(model_name %in% full_design_list)) {
    stop(paste0("Model design '", model_name, "' not found in project database."))
  }
  
  # Load design file (qs2 or rds)
  db_path <- locdatabase(project)
  designs_dir <- file.path(dirname(db_path), "ModelDesigns")
  base_path <- file.path(designs_dir, model_name)
  paths <- c(paste0(base_path, ".qs2"), paste0(base_path, ".rds"))
  found_path <- paths[file.exists(paths)][1]
  
  if (is.na(found_path)) stop("Design file not found on disk.")
  
  # Load based on extension
  if (grepl("\\.qs2$", found_path)) {
    if (!requireNamespace("qs2", quietly = TRUE)) stop("Install 'qs2' to read this design.")
    design <- qs2::qs_read(found_path)
  } else {
    design <- readRDS(found_path)
  }
  
  # Load model fit list and check fit_name input
  if (is_empty(fit_name)) fit_name <- paste0(model_name, "_fit")
  
  if (table_exists(paste0(project, "ModelFit"), project)) {
    full_fit_list <- unserialize_table(paste0(project, "ModelFit"), project)  
    if (fit_name %in% names(full_fit_list)) {
      stop(paste0("Model fit '", fit_name, "' already exists. Enter a new fit_name."))
    }
  }
  
  # Check if this is an EPM
  is_epm <- isTRUE(design$epm$is_epm)
  if (is_epm) {
    if (is.null(distribution)) stop("EPMs require a distribution for the catch function.")
    valid_dists <- c("normal", "lognormal", "weibull")
    distribution <- match.arg(distribution, valid_dists)    
  }
  
  # Control args
  dots <- list(...)
  default_control <- list(eval.max = 1000, iter.max = 1000)
  if ("control" %in% names(dots)) {
    control_list <- utils::modifyList(default_control, dots$control)
  } else {
    control_list <- default_control
  }
  
  # Data prep -------------------------------------------------------------------------------------
  N_obs <- as.integer(design$settings$N_obs)
  J_alts <- as.integer(design$settings$J_alts)
  y_vec <- as.numeric(design$y) # Choice index
  chosen_lin_idx <- which(y_vec == 1) # Index of chosen zones in flattened matrix
  
  # Validation to ensure data wasn't corrupted
  if (length(chosen_lin_idx) != N_obs) {
    stop(paste("Error in choice index: number of choices does not match N_obs.",
               "Ensure data is sorted by Obs/Zone."))
  }
  
  # STANDARD LOGIT SETUP --------------------------------------------------------------------------
  if (!is_epm) {
    K_vars <- design$settings$K_vars
    # Initial betas
    if ("start_values" %in% names(dots)) {
      init_beta <- dots$start_values
      if(length(init_beta) != K_vars) stop(paste0("Start values length (", length(init_beta), 
                                                  ") does not match parameters (", K_vars, ")."))
    } else {
      init_beta <- rep(0.0001, K_vars)
    }
    
    data_list <- list(
      X = design$X,
      chosen_lin_idx = chosen_lin_idx,
      N_obs = N_obs,
      J_alts = J_alts
    )
    
    start_pars <- list(betas = init_beta)
    
    # Objective function
    nll_func <- function(pars) {
      RTMB::getAll(data_list, pars)
      
      # Sparse Matrix Multiply (Zonal)
      v <- X %*% betas
      
      v_chosen <- v[chosen_lin_idx]
      
      dim(v) <- c(J_alts, N_obs)
      
      if (robust) {
        v_max <- v[1, ]
        for(j in 2:J_alts) {
          v_next <- v[j, ]
          v_max <- (v_max + v_next + abs(v_max - v_next)) * 0.5
        }
        t_v_shifted <- t(v) - v_max
        log_sum_exp <- log(RTMB::rowSums(exp(t_v_shifted))) + v_max
        
      } else {
        log_sum_exp <- log(RTMB::colSums(exp(v)))
      }
      
      nll <- -sum(v_chosen - log_sum_exp)
      return(nll)
    }
    
    
    # EPM LOGIT SETUP -----------------------------------------------------------------------------
  } else {
    ### Function 1 ###
    # Extract and type cast data
    Y_catch <- as.double(as.vector(design$epm$Y_catch)) 
    X_catch <- as.matrix(design$epm$X_catch)
    storage.mode(X_catch) <- "double"
    attr(X_catch, "dimnames") <- NULL
    
    prices <- as.double(as.vector(design$epm$price_vec))
    
    # Extract utility variables (remove catch preds from X)
    util_vars <- setdiff(colnames(design$X), colnames(design$epm$X_catch))
    
    if(length(util_vars) > 0) {
      X_util <- as.matrix(design$X[, util_vars, drop = FALSE])
      storage.mode(X_util) <- "double"
      attr(X_util, "dimnames") <- NULL
    } else {
      X_util <- matrix(0, nrow = length(Y_catch), ncol = 0)
    }
    
    # Pre-calculate indices for each zone
    idx_list <- lapply(1:J_alts, function(j) seq(j, N_obs * J_alts, by = J_alts))
    
    # Automated pooling detection
    # Check if user manually overrode via dots, otherwise detect from data.
    if ("pooled_catch_beta" %in% names(dots)) {
      pooled <- as.integer(dots$pooled_catch_beta)
    } else {
      # Auto-detection logic:
      # Check the first observation's alternatives (Rows 1 to J_alts).
      # If ANY predictor column has variance > 0 across zones, we assume spatial variation 
      # and default to Global (Pooled) coefficients.
      # If ALL predictors are constant (var approx 0), we default to Zone-Specific (Unpooled).
      
      first_obs_X <- X_catch[1:J_alts, , drop = FALSE]
      
      # Calculate variance for each column (predictor)
      col_vars <- apply(first_obs_X, 2, var)
      
      # If any column has non-zero variance (approx), set pooled = 1
      if (any(col_vars > 1e-9)) {
        message("EPM: Spatial variation detected in catch predictors. Using GLOBAL coefficients.")
        pooled <- 1
      } else {
        message("EPM: No spatial variation in catch predictors. Using ZONE-SPECIFIC coefficients.")
        pooled <- 0
      }
    }
    
    ### Function 2 ###
    n_catch_preds <- ncol(X_catch)
    n_util_preds <- ncol(X_util)
    
    # Catch Parameters Setup based on Pooling Flag
    if (pooled == 1) {
      # POOLED: One beta per predictor (Global)
      init_beta_catch <- matrix(0.1, nrow = n_catch_preds, ncol = 1)
    } else {
      # UNPOOLED: One beta per predictor PER ZONE
      init_beta_catch <- matrix(0.1, nrow = n_catch_preds, ncol = J_alts)
    }
    
    # Utility Parameters
    init_beta_util <- if (n_util_preds > 0) rep(0.001, n_util_preds) else numeric(0)
    
    # Variance Parameters
    init_log_sigma_c <- rep(log(0.1), J_alts)
    init_log_sigma_e <- log(1.0)
    
    list(
      beta_catch = init_beta_catch,
      beta_util = init_beta_util,
      log_sigma_c = init_log_sigma_c,
      log_sigma_e = init_log_sigma_e
    )
    # End of FUNCTION 2####
    
    # Continuous catch data
    Y_catch <- as.double(as.vector(design$epm$Y_catch)) # Actual catch
    X_catch <- as.matrix(design$epm$X_catch) # Predictors for catch
    n_catch_preds <- ncol(X_catch)
    
    # Strip attributes for RTMB efficiency
    storage.mode(X_catch) <- "double"
    attr(X_catch, "dimnames") <- NULL
    
    prices <- as.double(as.vector(design$epm$price_vec))
    
    # Discrete choice data
    util_vars <- setdiff(colnames(design$X), colnames(design$epm$X_catch))
    
    if(length(util_vars) > 0) {
      X_util <- as.matrix(design$X[, util_vars, drop = FALSE])
      storage.mode(X_util) <- "double"
      attr(X_util, "dimnames") <- NULL
      n_util_preds <- ncol(X_util)
    } else {
      # Handle edge case where Utility is ONLY driven by expected revenue (no distance/cost)
      X_util <- matrix(0, nrow = nrow(design$X), ncol = 0)
      n_util_preds <- 0
    }
    
    # Create zone index for vectorization
    zone_idx <- rep(1:J_alts, N_obs)
    
    idx_list <- lapply(1:J_alts, function(j) seq(j, N_obs * J_alts, by = J_alts))
    
    # Set up parameters 
    # Catch params (zone specific)
    init_beta_catch <- matrix(0.1, nrow = n_catch_preds, ncol = J_alts)
    
    # Utility params
    init_beta_util <- if (n_util_preds > 0) rep(0.001, n_util_preds) else numeric(0)
    
    # Variance params
    init_log_sigma_c <- rep(log(0.1), J_alts)
    init_log_sigma_e <- log(1.0)
    
    start_pars <- list(beta_catch = init_beta_catch,
                       beta_util = init_beta_util,
                       log_sigma_c = init_log_sigma_c,
                       log_sigma_e = init_log_sigma_e)
    
    data_list <- list(Y_catch = Y_catch,
                      X_catch = X_catch,
                      X_util = X_util,
                      prices = prices,
                      chosen_lin_idx = chosen_lin_idx,
                      zone_idx = zone_idx,
                      N_obs = N_obs,
                      J_alts = J_alts,
                      model_type = "EPM") # Flag for RTMB
    
    nll_func <- function(pars) {
      RTMB::getAll(data_list, pars)
      
      # Transform variances
      sigma_c <- exp(log_sigma_c) # for each zone
      sigma_e <- exp(log_sigma_e)
      
      total_rows <- N_obs * J_alts
      E_catch_vec <- numeric(total_rows)
      
      # Loop through zones to fill the expected catch vector
      for(j in 1:J_alts){
        # Subset X for Zone j
        X_sub <- X_catch[idx_list[[j]], , drop = FALSE]
        # Get beta for Zone j
        b_sub <- beta_catch[, j]
        # Calculate predictions (AD Vector)
        # Note: result of %*% is matrix, cast to vector for assignment
        preds <- as.vector(X_sub %*% b_sub)
        # Assign to correct indices (Promotes E_catch_vec to AD automatically)
        E_catch_vec[idx_list[[j]]] <- preds
      }
      
      # Subset to chosen zones
      row_idx <- (0:(N_obs-1)) * J_alts + chosen_lin_idx
      E_catch_chosen <- E_catch_vec[row_idx]
      Y_catch_chosen <- Y_catch[row_idx]
      sigma_c_chosen <- sigma_c[chosen_lin_idx]
      
      nll_cont <- -sum(RTMB::dnorm(Y_catch_chosen, E_catch_chosen, sigma_c_chosen, log = TRUE))
      
      # Discrete likelihood
      # Expected revenue
      revenue_util <- prices * E_catch_vec
      
      # Expected cost
      if (ncol(X_util) > 0) {
        cost_util <- X_util %*% beta_util
      } else {
        cost_util <- 0
      }
      
      # Total utility
      V_vec <- (1 / sigma_e) * (revenue_util + cost_util)
      V_mat <- matrix(V_vec, nrow = N_obs, ncol = J_alts, byrow = TRUE)
      log_sum_exp <- log(rowSums(exp(V_mat)))
      
      V_chosen <- V_mat[cbind(1:N_obs, chosen_lin_idx)]
      
      nll_disc <- -sum(V_chosen - log_sum_exp)
      
      return(nll_cont + nll_disc)
    }
  }
  
  # Optimization ----------------------------------------------------------------------------------
  # Enable sparse Hessian compression (crucial for zonal logit)
  use_sparse_hess <- (design$settings$K_vars >= 50)
  TMB::config(tmbad.sparse_hessian_compress = use_sparse_hess, DLL="RTMB")
  
  obj <- RTMB::MakeADFun(func = nll_func,
                         data = data_list,
                         parameters = start_pars,
                         silent = TRUE)
  
  # Minimize NLL
  opt <- nlminb(obj$par, 
                obj$fn, 
                obj$gr, 
                control = control_list)
  
  # Standard errors and diagnostics ---------------------------------------------------------------
  hessian_mat <- NULL
  eigen_vals <- NULL
  cond_num <- NULL
  report_se <- rep(NA, length(opt$par))
  final_gradient <- NULL
  
  if (se_calc) {
    k_len <- length(opt$par)
    if (k_len < 50) {
      # FAST PATH (Small K): Finite Difference
      hessian_mat <- stats::optimHess(opt$par, obj$fn, obj$gr)
    } else {
      # SCALABLE PATH (Large K / Zonal): AD
      hessian_mat <- obj$he(opt$par)
    }
    
    # Invert Hessian for SEs
    cov_mat <- tryCatch(solve(hessian_mat), error = function(e) matrix(NA, k_len, k_len))
    d_vals <- diag(cov_mat)
    report_se <- sqrt(ifelse(d_vals < 0, NA, d_vals))
    final_gradient <- obj$gr(opt$par) # Get gradient
    
    # Diagnostics (Only compute eigenvalues if needed)
    if (!is.null(hessian_mat) && k_len < 2000) {
      eigen_vals <- eigen(hessian_mat, symmetric = TRUE, only.values = TRUE)$values
      cond_num <- max(abs(eigen_vals)) / min(abs(eigen_vals))
    }
  }
  
  # Reporting and unscaling -----------------------------------------------------------------------
  estimated_coefs <- opt$par
  coef_names <- colnames(design$X)
  if(length(estimated_coefs) == ncol(design$X)) names(estimated_coefs) <- coef_names

  report_coefs <- estimated_coefs

  # Unscaling (if necessary)
  if (!is.null(design$scalers) && length(design$scalers) > 0) {
    scale_factors <- rep(1, length(report_coefs))
    names(scale_factors) <- names(report_coefs)

    # Part 1 (Direct)
    if (!is.null(design$scalers$X1)) {
      s <- design$scalers$X1$sd
      # Patch for unnamed vectors in legacy designs
      if (is.null(names(s)) && !is.null(coef_names)) {
        try({names(s) <- coef_names[1:length(s)]}, silent=TRUE)
      }
      common <- intersect(names(s), names(scale_factors))
      scale_factors[common] <- s[common]
    }

    # Part 2 (Interaction)
    if (!is.null(design$scalers$X2)) {
      s2 <- design$scalers$X2$sd
      if (!is.null(names(s2))) {
        for (var in names(s2)) {
          idx <- grep(paste0("^", var, ":"), names(scale_factors))
          scale_factors[idx] <- s2[[var]]
        }
      }
    }
    
    report_coefs <- report_coefs / scale_factors
    if (se_calc) report_se <- report_se / scale_factors
  }

  coef_table <- data.frame(
    Estimate = report_coefs,
    Std_Error = report_se,
    z_value = if(se_calc) report_coefs / report_se else NA,
    Pr_z = if(se_calc) 2 * (1 - pnorm(abs(report_coefs / report_se))) else NA
  )
  
  # Fit stats and predictions ---------------------------------------------------------------------
  nll <- opt$objective
  k_param <- length(opt$par)
  null_logLik <- -1 * N_obs * log(J_alts)
  aic <- 2 * nll + 2 * k_param
  bic <- 2 * nll + k_param * log(N_obs)
  rho2 <- 1 - ((-nll) / null_logLik)

  # Predictions (Recalculate with original X outside AD tape)
  final_v <- as.vector(design$X %*% opt$par)
  dim(final_v) <- c(J_alts, N_obs)

  v_max <- apply(final_v, 2, max)
  exp_v <- exp(t(t(final_v) - v_max))
  sum_exp <- colSums(exp_v)
  prob_mat_t <- t(exp_v) / sum_exp

  choice_idx_report <- (chosen_lin_idx - 1) %% J_alts + 1
  chosen_probs <- prob_mat_t[cbind(1:N_obs, choice_idx_report)]
  pred_choice <- max.col(prob_mat_t, ties.method = "first")
  accuracy <- mean(pred_choice == choice_idx_report)

  # Output and save -------------------------------------------------------------------------------
  result <- list(
    opt = opt,
    coefficients = report_coefs,
    coef_table = coef_table,
    logLik = -nll,
    null_logLik = null_logLik,
    AIC = aic,
    BIC = bic,
    pseudo_R2 = rho2,
    accuracy = accuracy,
    fitted_values = chosen_probs,

    diagnostics = list(
      converged = (opt$convergence == 0),
      message = opt$message,
      final_gradient = final_gradient, # Now possibly NULL to save time
      hessian = hessian_mat,           # Now possibly NULL to save time
      eigenvalues = eigen_vals,        # Now possibly NULL to save time
      condition_number = cond_num
    )
  )

  # OPTIONAL: Include full probability matrix (Heavy!)
  if (return_full_prob_mat) {
    if (!is.null(design$ids$zone)) {
      unz <- unique(design$ids$zone)
      if(length(unz) == J_alts) colnames(prob_mat_t) <- as.character(unz)
    }
    result$prob_matrix <- prob_mat_t
    result$residuals <- 1 - chosen_probs
  }

  class(result) <- "fishset_fit"
  
  # Save to database
  # Check if fit_name exists
  fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project = project))
  on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)

  table_name <- paste0(project, "ModelFit")
  fit_wrapper <- list()
  fit_wrapper[[fit_name]] <- result

  # Append or create a new table
  if (table_exists(table_name, project)) {
    existing_fits <- unserialize_table(table_name, project)
    # Remove if exists to overwrite
    if (fit_name %in% names(existing_fits)) {
      existing_fits[[fit_name]] <- NULL
    }
    table_remove(table_name, project)
    fit_wrapper <- c(existing_fits, fit_wrapper)
  }

  DBI::dbExecute(fishset_db, 
                 paste("CREATE TABLE IF NOT EXISTS",
                       table_name,
                       "(data fit_wrapper)"))
  DBI::dbExecute(fishset_db,
                 paste("INSERT INTO",
                       table_name,
                       "VALUES (:data)"),
                 params = list(data = list(serialize(fit_wrapper, NULL))))
  
  
  # Log the function call
  fishset_fit_function <- list()
  fishset_fit_function$functionID <- "fishset_fit"
  fishset_fit_function$args <- as.list(match.call())[-1]
  fishset_fit_function$kwargs <- list()
  
  log_call(project, fishset_fit_function)

  return(result)
}


#' Print FishSET Model Fit Results
#'
#' Formats and prints the output of a FishSET discrete choice model fit.
#' Displays the model formula (if available), coefficients table with significance stars,
#' and key goodness-of-fit statistics (Log-Likelihood, AIC, BIC, Pseudo-R2, Accuracy).
#'
#' @param x A \code{fishset_fit} object returned by \code{\link{fishset_fit}}.
#' @param digits Integer. The number of significant digits to use when printing
#'   numeric values. Default is 4.
#' @param ... Additional arguments passed to \code{\link[stats]{printCoefmat}}.
#'
#' @method print fishset_fit
#' @export
print.fishset_fit <- function(x, digits = 4, ...) {

  # Helper for formatting stats
  fmt <- function(n, d=2) format(round(n, d), nsmall=d)

  # Header
  cat("\nFishSET Model Fit\n")
  cat("========================================================\n")

  # Metadata (if available in settings, otherwise check formula)
  if (!is.null(x$formula)) {
    cat("Formula:      ", deparse(x$formula), "\n")
  }

  # Coefficients table
  cat("\nCoefficients:\n")
  cat("--------------------------------------------------------\n")
  if (!is.null(x$coef_table)) {
    stats::printCoefmat(x$coef_table,
                        digits = digits,
                        signif.stars = TRUE,
                        P.values = TRUE,
                        has.Pvalue = TRUE)
  } else {
    print(x$coefficients)
  }
  cat("--------------------------------------------------------\n")
  cat("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")

  # Fit statistics table
  cat("\nModel Statistics:\n")
  cat("--------------------------------------------------------\n")
  cat("Log-Likelihood: ", fmt(x$logLik, 2), "\n", sep="")
  cat("AIC:            ", fmt(x$AIC, 2),    "  (BIC:  ", fmt(x$BIC, 2), ")\n", sep="")
  cat("Pseudo R2:      ", fmt(x$pseudo_R2, 3), "\n", sep="")
  cat("Accuracy:       ", fmt(x$accuracy * 100, 1), "%\n", sep="")

  # LR Test (if available)
  if (!is.null(x$LR_p_value)) {
    sig_star <- ""
    if (x$LR_p_value < 0.001) sig_star <- "***"
    else if (x$LR_p_value < 0.01) sig_star <- "**"
    else if (x$LR_p_value < 0.05) sig_star <- "*"
    else if (x$LR_p_value < 0.1) sig_star <- "."

    p_val_str <- format.pval(x$LR_p_value, eps = 0.001)
    cat("LR Test:        Chi2 =", fmt(x$LR_stat, 2), ", p =", p_val_str, sig_star, "\n")
  }

  invisible(x)
}