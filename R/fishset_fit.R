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

fishset_fit <- function(project,
                        model_name,
                        fit_name = NULL,
                        distribution = NULL,
                        ...) {
  
  # Load and validate -----------------------------------------------------------------------------
  tryCatch({
    full_design_list <- model_design_list(project)
  }, error = function(cond) {
    message("Not able to load model designs. Run fishset_design() first.")
    return(NULL)
  })
  
  if (!(model_name %in% full_design_list)) {
    stop(paste0("Model design '", model_name, "' not found in project database."))
  }
  
  # Read the design .rds file
  db_path <- locdatabase(project)
  project_dir <- dirname(db_path)
  designs_dir <- file.path(project_dir, "ModelDesigns")
  file_name <- paste0(model_name, ".rds")
  file_path <- file.path(designs_dir, file_name)
  design <- readRDS(file_path)
  
  # Check if this is an EPM
  is_epm <- isTRUE(design$epm$is_epm)
  
  # Load model fit list and check fit_name input
  if (is_empty(fit_name)) {
    fit_name <- paste0(model_name, "_fit")
  }
  
  if (table_exists(paste0(project, "ModelFit"), project)) {
    full_fit_list <- unserialize_table(paste0(project, "ModelFit"), project)  
    if (fit_name %in% names(full_fit_list)) {
      stop(paste0("Model fit '", fit_name, "' already exists. Enter a new fit_name."))
    }
  }
  
  if (!is.null(distribution)) {
    valid_dists <- c("normal", "lognormal", "weibull")
    distribution <- match.arg(distribution, valid_dists)    
  }
  
  # Extract "..." arguments -----------------------------------------------------------------------
  dots <- list(...)
  
  default_control <- list(eval.max = 1000, iter.max = 1000)
  if ("control" %in% names(dots)) {
    control_list <- utils::modifyList(default_control, dots$control)
  } else {
    control_list <- default_control
  }
  
  # General data prep -----------------------------------------------------------------------------
  # Dimensions
  N_obs <- as.integer(design$settings$N_obs)
  J_alts <- as.integer(design$settings$J_alts)
  
  # Choice index
  # Save the choice index
  y_clean <- as.vector(as.numeric(design$y)) # Strip names/attributes
  y_mat_temp <- matrix(y_clean, nrow = N_obs, ncol = J_alts, byrow = TRUE)
  
  # Create an integer vector (1 to J) indicating which alternative was chosen per row
  choice_idx <- max.col(y_mat_temp, ties.method = "first")
  
  # Expected profit model (EPM) -------------------------------------------------------------------
  if (is_epm) {
    
    # FUNCTION 1 ####
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
    # End of FUNCTION 1 ####
    
    # FUNCTION 2 ####
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
                      choice_idx = choice_idx,
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
      row_idx <- (0:(N_obs-1)) * J_alts + choice_idx
      E_catch_chosen <- E_catch_vec[row_idx]
      Y_catch_chosen <- Y_catch[row_idx]
      sigma_c_chosen <- sigma_c[choice_idx]
      
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
      
      V_chosen <- V_mat[cbind(1:N_obs, choice_idx)]
      
      nll_disc <- -sum(V_chosen - log_sum_exp)
      
      return(nll_cont + nll_disc)
    }
  
  # Standard logit model --------------------------------------------------------------------------
  } else {
    # Force X to a clean matrix (strip all attributes/dimnames)
    X <- as.matrix(design$X)
    storage.mode(X) <- "double"
    attr(X, "dimnames") <- NULL  
    K_vars <- design$settings$K_vars
    
    if (nrow(X) != (N_obs * J_alts)) {
      stop("Design matrix dimensions do not match N_obs * J_alts.")
    }
    
    # Handle start values
    if ("start_values" %in% names(dots)) {
      init_beta <- dots$start_values
      if(length(init_beta) != K_vars) stop(paste0("Start values length (", length(init_beta), 
                                                  ") does not match parameters (", K_vars, ")."))
    } else {
      init_beta <- rep(0.0001, K_vars)
    }
    
    data_list <- list(
      X = X,
      choice_idx = choice_idx, 
      N_obs = N_obs,
      J_alts = J_alts
    )  
    
    start_pars <- list(betas = init_beta)
    
    nll_func <- function(pars) {
      RTMB::getAll(data_list, pars)
      
      # Calculate Utility (Vector length N*J)
      v <- X %*% betas
      
      # Reshape to Matrix (N_obs x J_alts)
      dim(v) <- c(J_alts, N_obs)
      
      # Log-Sum-Exp (Denominator) - calculate log(sum(exp(v))) for every column
      log_sum_exp <- log(RTMB::colSums(exp(v)))
      
      # Numerator (Utility of chosen alternative)
      # Use the integer index to pick the specific value from the AD matrix
      chosen_utilities <- v[cbind(choice_idx, 1:N_obs)]
      
      # Negative Log Likelihood
      nll <- -sum(chosen_utilities - log_sum_exp)
      
      return(nll)
    }  
  }
  
  # Optimization ----------------------------------------------------------------------------------
  obj <- RTMB::MakeADFun(func = nll_func,
                         data = data_list,
                         parameters = start_pars,
                         silent = TRUE)
  
  # Minimize NLL
  opt <- nlminb(obj$par, 
                obj$fn, 
                obj$gr, 
                control = control_list)
  
  # Package Results -------------------------------------------------------------------------------
  sdr <- RTMB::sdreport(obj)
  
  #### Name coefficients ####
  estimated_coefs <- opt$par
  coef_names <- colnames(design$X)
  
  if(length(estimated_coefs) == ncol(design$X)){
    names(estimated_coefs) <- coef_names
    names(opt$par) <- coef_names # Ensure opt object is also named
    names(sdr$par.fixed) <- coef_names
  }
  
  #### Fit statistics (AIC, BIC, LogLik) ####
  nll <- opt$objective
  k <- length(opt$par)
  n <- N_obs 
  
  aic <- 2 * nll + 2 * k
  bic <- 2 * nll + k * log(n)
  
  # AICc (small sample correction)
  denom <- n - k - 1
  if (denom > 0) {
    aicc <- aic + (2 * k * (k + 1)) / denom
  } else {
    aicc <- Inf
    warning("Sample size too small for valid AICc calculation.")
  }
  
  #### Pseudo-R2 & Global Test ####
  # Null Likelihood: Model with no parameters (Equiprobable choice = 1/J)
  null_logLik <- -1 * N_obs * log(J_alts)
  model_logLik <- -nll
  
  # McFadden's Pseudo-R2
  rho2 <- 1 - (model_logLik / null_logLik)
  
  # Global likelihood ratio test (Model vs Null)
  lr_stat <- -2 * (null_logLik - model_logLik)
  lr_p_value <- 1 - pchisq(lr_stat, df = k)
  
  #### Diagnostics (Hessian & Gradient) ####
  hessian_mat <- obj$he(opt$par)
  final_gradient <- obj$gr(opt$par)
  
  if(!is.null(coef_names)){
    rownames(hessian_mat) <- coef_names
    colnames(hessian_mat) <- coef_names
    names(final_gradient) <- coef_names
  }
  
  # Eigenvalues (Check for Positive Definiteness/Singularity)
  eigen_vals <- eigen(hessian_mat)$values
  
  # Condition Number (Check for Scaling Issues)
  cond_num <- max(abs(eigen_vals)) / min(abs(eigen_vals))
  
  # Correlation Matrix (Check for Collinearity)
  cov_mat <- tryCatch(solve(hessian_mat), error = function(e) matrix(NA, k, k))
  cor_mat <- tryCatch(cov2cor(cov_mat), error = function(e) matrix(NA, k, k))
  
  if(!is.null(coef_names)){
    rownames(cor_mat) <- coef_names
    colnames(cor_mat) <- coef_names
  }
  
  #### Coefficient Table ####
  sdr_summary <- summary(sdr, "fixed")
  coef_table <- data.frame(
    Estimate = sdr_summary[, "Estimate"],
    Std_Error = sdr_summary[, "Std. Error"]
  )
  
  # Z-score and P-value (Two-tailed)
  coef_table$z_value <- coef_table$Estimate / coef_table$Std_Error
  coef_table$Pr_z <- 2 * (1 - pnorm(abs(coef_table$z_value)))
  if(!is.null(coef_names)) rownames(coef_table) <- coef_names
  
  #### Fitted Values & Predictions ####
  # Re-calculate probabilities using the final betas
  final_v <- X %*% opt$par
  
  final_v_mat <- matrix(final_v, nrow = N_obs, ncol = J_alts, byrow = TRUE)
  
  # Softmax
  exp_v <- exp(final_v_mat)
  sum_exp_v <- rowSums(exp_v) # Sum across alternatives
  prob_matrix <- exp_v / sum_exp_v
  
  # Apply labels if available in design
  if (!is.null(design$ids$zone)) {
    # Extract unique zone labels (assuming order matches J_alts)
    unique_zones <- unique(design$ids$zone)
    if(length(unique_zones) == J_alts) colnames(prob_matrix) <- as.character(unique_zones)
  }
  
  # Prob of the chosen alternative
  chosen_probs <- prob_matrix[cbind(1:N_obs, choice_idx)]
  
  # Hit rate (accuracy)
  predicted_choice_idx <- max.col(prob_matrix, ties.method = "first")
  accuracy <- mean(predicted_choice_idx == choice_idx)
  
  # Compile Result List ---------------------------------------------------------------------------
  result <- list(
    # Core Objects
    opt = opt,
    obj = obj,
    sdr = sdr,
    
    # Metadata
    formula = design$formula,
    converged = (opt$convergence == 0),
    message = opt$message,
    
    # Tables and Stats
    coefficients = estimated_coefs,
    coef_table = coef_table,
    vcov = cov_mat,
    
    # Fit Statistics
    logLik = model_logLik,
    null_logLik = null_logLik,
    pseudo_R2 = rho2,
    LR_stat = lr_stat,
    LR_p_value = lr_p_value,
    AIC = aic,
    AICc = aicc,
    BIC = bic,
    accuracy = accuracy,
    
    # Predictions
    fitted_values = chosen_probs,
    prob_matrix = prob_matrix,
    residuals = 1 - chosen_probs,
    
    # Diagnostics
    diagnostics = list(
      hessian = hessian_mat,
      final_gradient = final_gradient,
      eigenvalues = eigen_vals,
      condition_number = cond_num,
      correlation_matrix = cor_mat,
      max_gradient = max(abs(final_gradient))
    )
  )
  
  class(result) <- "fishset_fit"
  
  # Save to database ------------------------------------------------------------------------------
  # Check if fit_name exists
  fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project = project))
  on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
  
  # Save this into a new table or append to a designs list
  table_name <- paste0(project, "ModelFit")
  
  # Create a named list wrapper
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
  
  # Log the function call -------------------------------------------------------------------------
  fishset_fit_function <- list()
  fishset_fit_function$functionID <- "fishset_fit"
  fishset_fit_function$args <- as.list(match.call())[-1]
  fishset_fit_function$kwargs <- list()
  
  log_call(project, fishset_fit_function)
  
  return(result)
}

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
  cat("Log-Likelihood: ", fmt(x$logLik, 2), "  (Null: ", fmt(x$null_logLik, 2), ")\n", sep="")
  cat("AIC:            ", fmt(x$AIC, 2),    "  (BIC:  ", fmt(x$BIC, 2), ")\n", sep="")
  cat("Pseudo R2:      ", fmt(x$pseudo_R2, 3), "\n", sep="")
  cat("Accuracy:       ", fmt(x$accuracy * 100, 1), "%\n", sep="")
  
  # LR Test
  sig_star <- ""
  if (!is.null(x$LR_p_value)) {
    if (x$LR_p_value < 0.001) sig_star <- "***"
    else if (x$LR_p_value < 0.01) sig_star <- "**"
    else if (x$LR_p_value < 0.05) sig_star <- "*"
    else if (x$LR_p_value < 0.1) sig_star <- "."
    
    p_val_str <- format.pval(x$LR_p_value, eps = 0.001)
    cat("LR Test:        Chi2 =", fmt(x$LR_stat, 2), ", p =", p_val_str, sig_star, "\n")
  }
  
  invisible(x)
}