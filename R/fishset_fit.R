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
#' @param overwrite Logical. Default FALSE. If TRUE, overwrites an existing model fit 
#'   if \code{fit_name} already exists in the project database.
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
#' # 1. Standard fit using default settings
#' # This uses the design object named "clogit_design" saved in "MyProject"
#' fit_result <- fishset_fit(
#'   project = "MyProject",
#'   model_name = "clogit_design"
#' )
#'   
#' # 2. Advanced fit with custom optimization settings and start values
#' # 'control' and 'start_values' are passed via the '...' argument
#' fit_custom <- fishset_fit(
#'   project = "MyProject",
#'   model_name = "clogit_design",
#'   fit_name = "clogit_custom_fit",
#'     
#'   # Pass control list to nlminb (e.g., increase max iterations, turn on tracing)
#'   control = list(eval.max = 5000, iter.max = 5000, trace = 1),
#'     
#'   # Pass initial start values for the parameters (e.g., for 2 predictors)
#'   start_values = c(0.5, -0.2)
#' )
#'   
#' # 3. EPM - normal catch function
#' epm_fit <- fishset_fit(project = project,
#'   model_name = "epm1",
#'   fit_name = "epm_fit1",
#'   distribution = "normal"
#' )
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
                        overwrite = FALSE,
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
  
  if (is.na(found_path)) stop("Design file not found.")
  
  # Load based on extension
  if (grepl("\\.qs2$", found_path)) {
    if (!requireNamespace("qs2", quietly = TRUE)) stop("Install 'qs2' to read this design.")
    design <- qs2::qs_read(found_path)
  } else {
    design <- readRDS(found_path)
  }
  
  # Load model fit list and check fit_name input
  full_fit_list <- tryCatch({
    unserialize_table(paste0(project, "ModelFit"), project)
  }, error = function(e) {
    list()
  })
  
  if (is_empty(fit_name)) fit_name <- paste0(model_name, "_fit")
  
  if (fit_name %in% names(full_fit_list)) {
    if (!overwrite) {
      stop(paste0("Model fit '", fit_name, 
                  "' already exists. Set overwrite = TRUE to replace it."))  
    }
  }
  
  # Check if this is EPM or Poisson
  is_poisson <- isTRUE(design$settings$model_type == "poisson")
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
  
  if (design$settings$model_type == "logit") {
    chosen_lin_idx <- which(y_vec == 1) # Index of chosen zones in flattened matrix
    # Validation to ensure data wasn't corrupted
    if (length(chosen_lin_idx) != N_obs) {
      stop(paste("Error in choice index: number of choices does not match N_obs.",
                 "Ensure data is sorted by Obs/Zone."))
    }  
  }
  
  # EPM LOGIT SETUP -------------------------------------------------------------------------------
  if (is_epm) {
    # Extract utility variables (remove catch preds from X)
    util_vars <- setdiff(colnames(design$X), colnames(design$epm$X_catch))
    
    if(length(util_vars) > 0) {
      X_util <- design$X[, util_vars, drop = FALSE]
    } else {
      X_util <- matrix(0, nrow = length(Y_catch), ncol = 0)
    }
    
    Y_catch_chosen = as.double(as.vector(design$epm$Y_catch[chosen_lin_idx]))
    zone_id <- ((chosen_lin_idx - 1) %% J_alts) + 1
    
    data_list <- list(
      Y_catch_chosen = Y_catch_chosen,
      X_util = X_util,
      X_catch = design$epm$X_catch,
      prices = design$epm$price_vec,
      chosen_lin_idx = chosen_lin_idx,
      N_obs = N_obs,
      J_alts = J_alts,
      zone_id = zone_id
    )
    
    # Initialize parameters
    init_beta_catch <- rep(0.1, ncol(design$epm$X_catch))
    init_beta_util <- rep(0.1, ncol(X_util))
    init_log_sigma_c <- rep(log(1.0), J_alts)
    init_log_sigma_e <- log(1.0)
    
    start_pars <- list(beta_catch = init_beta_catch,
                       beta_util = init_beta_util,
                       log_sigma_c = init_log_sigma_c,
                       log_sigma_e = init_log_sigma_e)
    
    nll_func <- function(pars) {
      RTMB::getAll(data_list, pars)
      
      # Parameters
      sigma_c <- exp(log_sigma_c) # for each zone
      sigma_e <- exp(log_sigma_e)
      
      # Continuous likelihood
      E_catch <- X_catch %*% beta_catch
      E_catch_chosen <- E_catch[chosen_lin_idx]
      sigma_c_chosen <- sigma_c[zone_id]
      nll_cont <- -sum(RTMB::dnorm(Y_catch_chosen, E_catch_chosen, sigma_c_chosen, log = TRUE))
      
      # Discrete likelihood
      # Expected revenue
      revenue_util <- prices * E_catch
      # Expected cost
      if (ncol(X_util) > 0) {
        cost_util <- X_util %*% beta_util
      } else {
        cost_util <- 0
      }
      
      # Utility
      v <- (1 / sigma_e) * (revenue_util + cost_util)
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
      
      nll_disc <- -sum(v_chosen - log_sum_exp)
      
      return(nll_cont + nll_disc)
    }
    
    # POISSON EQUIVALENCE SETUP -------------------------------------------------------------------
  } else if (is_poisson) {
    K_vars <- design$settings$K_vars
    
    if ("start_values" %in% names(dots)) {
      init_beta <- dots$start_values
    } else {
      init_beta <- rep(0.01, K_vars)
    }
    
    data_list <- list(
      count = design$y,
      X = design$X,
      occ_id = design$ids$occ_id
    )
    
    start_pars <- list(
      betas = init_beta,
      alpha_occ = rep(0, N_obs) # intercept for each occasion
    )
    
    nll_func <- function(pars) {
      RTMB::getAll(data_list, pars)
      
      # Linear predictor: occasion inercepts + utilities
      eta <- alpha_occ[occ_id + 1] + X %*% betas
      
      # Expected count
      lambda <- exp(eta)
      
      # Poisson negative LL
      nll <- -sum(RTMB::dpois(count, lambda, log = TRUE))
    }
    
    # STANDARD LOGIT SETUP ------------------------------------------------------------------------
  } else {
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
  }
  
  # Optimization ----------------------------------------------------------------------------------
  # Enable sparse Hessian compression (crucial for zonal logit)
  use_sparse_hess <- (design$settings$K_vars >= 50)
  TMB::config(tmbad.sparse_hessian_compress = use_sparse_hess, DLL="RTMB")
  
  # If Poisson, profile out alpha_occ
  random_args <- if (is_poisson) "alpha_occ" else NULL
  
  obj <- RTMB::MakeADFun(func = nll_func,
                         data = data_list,
                         parameters = start_pars,
                         random = random_args,
                         silent = TRUE)
  
  # Minimize NLL
  opt <- nlminb(obj$par, 
                obj$fn, 
                obj$gr, 
                control = control_list)
  
  # Standard errors and diagnostics ---------------------------------------------------------------
  hessian_mat <- NULL
  final_gradient <- NULL
  eigen_vals <- NULL
  cond_num <- NULL
  report_se <- rep(NA, length(opt$par))
  
  if (se_calc) {
    k_len <- length(opt$par)
    final_gradient <- obj$gr(opt$par)
    
    if (is_poisson) {
      # Use sdreport for models with random effects
      sdr <- tryCatch(RTMB::sdreport(obj), error = function(e) NULL)
      
      if (!is.null(sdr)) {
        cov_mat <- sdr$cov.fixed
        d_vals <- diag(cov_mat)
        report_se <- sqrt(ifelse(d_vals < 0, NA, d_vals))
        
        # Approximate Hessian from inverse of cov matrix
        hessian_mat <- tryCatch(solve(cov_mat), error = function(e) matrix(NA, k_len, k_len))
      }
      
    } else {
      if (k_len < 50) {
        hessian_mat <- stats::optimHess(opt$par, obj$fn, obj$gr)
      } else {
        hessian_mat <- obj$he(opt$par)
      }
      
      cov_mat <- tryCatch(solve(hessian_mat), error = function(e) matrix(NA, k_len, k_len))
      d_vals <- diag(cov_mat)
      report_se <- sqrt(ifelse(d_vals < 0, NA, d_vals))
    }
    
    if (!any(is.na(hessian_mat))) {
      e_decomp <- eigen(hessian_mat, symmetric = TRUE, only.values = TRUE)
      eigen_vals <- e_decomp$values
      cond_num <- tryCatch(max(abs(eigen_vals)) / min(abs(eigen_vals)), error = function(e) NA)
    }
  }
  
  # Reporting and unscaling -----------------------------------------------------------------------
  estimated_coefs <- opt$par
  
  if (!is_epm) {
    ### standard logit reporting ###
    coef_names <- colnames(design$X)
    if(length(estimated_coefs) == ncol(design$X)) names(estimated_coefs) <- coef_names
    report_coefs <- estimated_coefs
    
    if (!is.null(design$scalers) && length(design$scalers) > 0) {
      scale_factors <- rep(1, length(report_coefs))
      names(scale_factors) <- names(report_coefs)
      
      if (!is.null(design$scalers$X1)) {
        s <- design$scalers$X1$sd
        if (is.null(names(s))) names(s) <- coef_names[1:length(s)]
        common <- intersect(names(s), names(scale_factors))
        scale_factors[common] <- s[common]
      }
      if (!is.null(design$scalers$X2)) {
        s2 <- design$scalers$X2$sd
        for (var in names(s2)) {
          idx <- grep(paste0("^", var, ":"), names(scale_factors))
          scale_factors[idx] <- s2[[var]]
        }
      }
      report_coefs <- report_coefs / scale_factors
      if (se_calc) report_se <- report_se / scale_factors
    }
    
  } else {
    ### epm reporting (normal only) ###
    n_catch <- ncol(design$epm$X_catch)
    n_util  <- ncol(data_list$X_util)
    
    beta_c_est <- estimated_coefs[1:n_catch]
    beta_u_est <- estimated_coefs[(n_catch + 1):(n_catch + n_util)]
    
    # Unscale Catch Betas
    if (!is_empty(design$scalers$X_catch)) {
      sc <- design$scalers$X_catch
      beta_c_est <- beta_c_est / sc$sd
      if(se_calc) report_se[1:n_catch] <- report_se[1:n_catch] / sc$sd
    }
    
    # Unscale Utility Betas
    if (!is_empty(design$scalers)) {
      util_names <- colnames(data_list$X_util)
      # Combine all utility scalers
      all_sds <- c(design$scalers$X1$sd, unlist(design$scalers$X2$sd))
      
      for(i in seq_along(util_names)) {
        vn <- util_names[i]
        if (vn %in% names(all_sds)) {
          beta_u_est[i] <- beta_u_est[i] / all_sds[vn]
          report_se[n_catch + i] <- report_se[n_catch + i] / all_sds[vn]
        }
      }
    }
    
    names(beta_c_est) <- colnames(design$epm$X_catch)
    names(beta_u_est) <- colnames(data_list$X_util)
    
    # Append Sigmas to report
    sig_c_est <- exp(estimated_coefs[grep("log_sigma_c", names(estimated_coefs))])
    sig_e_est <- exp(estimated_coefs[grep("log_sigma_e", names(estimated_coefs))])
    names(sig_c_est) <- paste0("Sigma_Catch_", levels(as.factor(design$ids$zone)))
    names(sig_e_est) <- "Sigma_Error"
    
    report_coefs <- c(beta_c_est, beta_u_est, sig_c_est, sig_e_est)
    
    idx_log_sig_c <- grep("log_sigma_c", names(estimated_coefs))
    idx_log_sig_e <- grep("log_sigma_e", names(estimated_coefs))
    
    se_log_sig_c <- report_se[idx_log_sig_c]
    se_log_sig_e <- report_se[idx_log_sig_e]
    
    se_sig_c_natural <- se_log_sig_c * sig_c_est
    se_sig_e_natural <- se_log_sig_e * sig_e_est
    
    # Adjust SE vector length to match report_coefs (Sigmas need Delta method)
    report_se <- c(report_se[1:(n_catch + n_util)], se_sig_c_natural, se_sig_e_natural)
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
  
  # Calculate appropriate NULL LLs
  if (is_poisson) {
    # Null Poisson: assumes counts are distributed perfectly evenly across all cells
    actual_mat <- t(matrix(y_vec, nrow = J_alts, ncol = N_obs))
    lambda_null <- rowSums(actual_mat) / J_alts
    null_logLik <- sum(stats::dpois(y_vec, rep(lambda_null, each = J_alts), log = TRUE))
  } else {
    null_logLik <- -1 * N_obs * log(J_alts)  
  }
  
  aic <- 2 * nll + 2 * k_param
  bic <- 2 * nll + k_param * log(N_obs)
  rho2 <- 1 - ((-nll) / null_logLik)
  
  # Predictions (Recalculate with original X outside AD tape)
  if (!is_epm) {
    final_v <- as.vector(design$X %*% opt$par)
    dim(final_v) <- c(J_alts, N_obs)
    v_max <- apply(final_v, 2, max)
    exp_v <- exp(t(t(final_v) - v_max))
    sum_exp <- colSums(exp_v)
    prob_mat_t <- t(exp_v) / sum_exp
    
  } else {
    # EPM Normal predictions
    final_par <- opt$par
    n_c <- ncol(design$epm$X_catch)
    b_c <- final_par[1:n_c]
    b_u <- final_par[(n_c+1):(n_c + ncol(data_list$X_util))]
    l_sig_e <- final_par[grep("log_sigma_e", names(final_par))]
    
    # Expected catch
    mu_catch <- design$epm$X_catch %*% b_c
    
    # Utility
    rev_u <- design$epm$price_vec * mu_catch
    if(length(b_u) > 0) cost_u <- data_list$X_util %*% b_u else cost_u <- 0
    
    v <- as.matrix((1/exp(l_sig_e)) * (rev_u + cost_u))
    dim(v) <- c(J_alts, N_obs)
    
    # Probabilities
    v_max <- apply(v, 2, max)
    exp_v <- exp(t(t(v) - v_max))
    sum_exp <- colSums(exp_v)
    prob_mat_t <- t(exp_v) / sum_exp
  }
  
  # Calculate accuracy and outputs
  if (is_poisson) {
    pred_choice <- max.col(prob_mat_t, ties.method = "first")
    actual_max <- max.col(actual_mat, ties.method = "first")
    accuracy <- mean(pred_choice == actual_max)
    
    # Instead of 'chosen_probs', we output the 'Expected Counts' (Shares * Total Traps)
    total_counts_per_occ <- rowSums(actual_mat)
    chosen_probs <- as.vector(prob_mat_t * total_counts_per_occ)
    
  } else {
    choice_idx_report <- (chosen_lin_idx - 1) %% J_alts + 1
    chosen_probs <- prob_mat_t[cbind(1:N_obs, choice_idx_report)]
    pred_choice <- max.col(prob_mat_t, ties.method = "first")
    accuracy <- mean(pred_choice == choice_idx_report)
  }
  
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