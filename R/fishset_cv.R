#' K-Fold Cross Validation for FishSET Models
#'
#' Performs k-fold cross-validation on a fitted FishSET model to evaluate out-of-sample 
#' predictive performance. Bypasses formula parsing by directly subsetting the design matrix.
#'
#' @param project Character string. Name of the project.
#' @param base_model_name Character string. The name of the original model design to 
#'   cross-validate.
#' @param k Integer. The number of folds to create. Default is 5.
#' @param seed Integer. Random seed for reproducible fold generation. Default is 42.
#' @param distribution Character string. Distribution for the continuous catch component in EPMs.
#' @param ... Additional control arguments passed to \code{fishset_fit()}.
#'
#' @return A list containing the average out-of-sample accuracy, log-likelihood, and fold details.
#'
#' @export
#' @importFrom DBI dbConnect dbDisconnect dbExecute
#' @importFrom RSQLite SQLite

fishset_cv <- function(project, 
                       base_model_name, 
                       k = 5, 
                       seed = 42, 
                       distribution = NULL, 
                       ...) {
  
  # Load the base design
  designs_dir <- file.path(locproject(), project, "Models", "ModelDesigns")
  base_path_qs2 <- file.path(designs_dir, paste0(base_model_name, ".qs2"))
  base_path_rds <- file.path(designs_dir, paste0(base_model_name, ".rds"))
  
  if (file.exists(base_path_qs2) && requireNamespace("qs2", quietly = TRUE)) {
    base_design <- qs2::qs_read(base_path_qs2)
  } else if (file.exists(base_path_rds)) {
    base_design <- readRDS(base_path_rds)
  } else {
    stop("Base model design not found.")
  }
  
  is_epm <- isTRUE(base_design$epm$is_epm)
  if (is_epm && is.null(distribution)) stop("EPMs require a 'distribution' argument for CV.")
  
  # Create folds based on unique choice occasions (Trips/Hauls)
  set.seed(seed)
  obs_vec <- base_design$ids$obs
  unique_obs <- unique(obs_vec)
  
  fold_assignments <- sample(rep(1:k, length.out = length(unique_obs)))
  fold_dict <- data.frame(obs = unique_obs, fold = fold_assignments)
  
  # Helper to subset the design object
  subset_design <- function(design, target_obs) {
    idx <- which(obs_vec %in% target_obs)
    new_des <- design
    new_des$y <- design$y[idx]
    new_des$X <- design$X[idx, , drop = FALSE]
    
    if (is_epm) {
      new_des$epm$Y_catch <- design$epm$Y_catch[idx]
      new_des$epm$X_catch <- design$epm$X_catch[idx, , drop = FALSE]
      new_des$epm$price_vec <- design$epm$price_vec[idx]
    }
    
    new_des$settings$N_obs <- length(target_obs)
    new_des$ids$obs <- design$ids$obs[idx]
    new_des$ids$zone <- design$ids$zone[idx]
    return(new_des)
  }
  
  cv_results <- data.frame(
    Fold = 1:k, Train_N = integer(k), Test_N = integer(k),
    In_Sample_LL = numeric(k), Out_Sample_LL = numeric(k),
    In_Sample_Acc = numeric(k), Out_Sample_Acc = numeric(k)
  )
  
  # The CV Loop
  for (i in 1:k) {
    cat(sprintf("  Running Fold %d of %d...\n", i, k))
    
    train_obs <- fold_dict$obs[fold_dict$fold != i]
    test_obs  <- fold_dict$obs[fold_dict$fold == i]
    
    cv_results$Train_N[i] <- length(train_obs)
    cv_results$Test_N[i]  <- length(test_obs)
    
    train_des <- subset_design(base_design, train_obs)
    test_des  <- subset_design(base_design, test_obs)
    
    # Save temp training design to design folder so fishset_fit can find it
    tmp_model_name <- paste0("cv_tmp_des_", i)
    saveRDS(train_des, file.path(designs_dir, paste0(tmp_model_name, ".rds")), compress = FALSE)
    
    # Fit training model and skip SE for speed
    suppressMessages({
      fit_train <- fishset_fit(
        project = project, 
        model_name = tmp_model_name, 
        fit_name = paste0("cv_tmp_fit_", i),
        distribution = distribution, 
        se_calc = FALSE, 
        overwrite = TRUE
        # ...
      )
    })
    
    # Out-of-sample prediction using exactly the math from fishset_fit 
    J <- test_des$settings$J_alts
    N <- test_des$settings$N_obs
    chosen_lin_idx <- which(test_des$y == 1)
    choice_idx_report <- (chosen_lin_idx - 1) %% J + 1
    
    if (!is_epm) {
      final_v <- as.vector(test_des$X %*% fit_train$opt$par)
      dim(final_v) <- c(J, N)
      v_max <- apply(final_v, 2, max)
      exp_v <- exp(t(t(final_v) - v_max))
      prob_mat_t <- t(exp_v) / colSums(exp_v)
      
    } else {
      # EPM predictions
      final_par <- fit_train$opt$par
      n_c <- ncol(test_des$epm$X_catch)
      b_c <- final_par[1:n_c]
      
      util_vars <- setdiff(colnames(test_des$X), colnames(test_des$epm$X_catch))
      if (length(util_vars) > 0) {
        X_util <- test_des$X[, util_vars, drop = FALSE]
      } else {
        X_util <- matrix(0, nrow = length(test_des$epm$Y_catch), ncol = 0)
      }
      b_u <- if (length(util_vars) > 0) final_par[(n_c + 1):(n_c + ncol(X_util))] else numeric(0)
      
      l_sig_e <- final_par[grep("log_sigma_e", names(final_par))]
      l_sig_c <- final_par[grep("log_sigma_c", names(final_par))]
      
      lin_pred <- test_des$epm$X_catch %*% b_c
      zone_seq <- ((0:(length(lin_pred) - 1)) %% J) + 1
      sig_c_full <- exp(l_sig_c)[zone_seq]
      
      if (distribution == "normal") { 
        mu_catch <- lin_pred
      } else if (distribution == "lognormal") { 
        mu_catch <- exp(lin_pred + 0.5 * sig_c_full^2)
      } else if (distribution == "weibull") { 
        mu_catch <- exp(lin_pred) * exp(lgamma(1 + 1/sig_c_full)) 
      }
      
      rev_u <- test_des$epm$price_vec * mu_catch
      cost_u <- if (length(b_u) > 0) X_util %*% b_u else 0
      
      v <- as.matrix((1 / exp(l_sig_e)) * (rev_u + cost_u))
      dim(v) <- c(J, N)
      
      v_max <- apply(v, 2, max)
      exp_v <- exp(t(t(v) - v_max))
      prob_mat_t <- t(exp_v) / colSums(exp_v)
    }
    
    chosen_probs <- prob_mat_t[cbind(1:N, choice_idx_report)]
    pred_choice <- max.col(prob_mat_t, ties.method = "first")
    
    cv_results$In_Sample_LL[i] <- fit_train$logLik
    cv_results$In_Sample_Acc[i] <- fit_train$accuracy
    cv_results$Out_Sample_LL[i] <- sum(log(chosen_probs))
    cv_results$Out_Sample_Acc[i] <- mean(pred_choice == choice_idx_report)
    
    # Cleanup temp files
    unlink(file.path(designs_dir, paste0(tmp_model_name, ".rds")))
  }
  
  # Remove temporary fits from the SQLite database
  table_name <- paste0(project, "ModelFit")
  existing_fits <- unserialize_table(table_name, project)
  existing_fits <- existing_fits[!grepl("^cv_tmp_fit_", names(existing_fits))]
  
  fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project = project))
  DBI::dbExecute(fishset_db, paste("DELETE FROM", table_name))
  DBI::dbExecute(fishset_db, 
                 paste("INSERT INTO", table_name, "VALUES (:data)"), 
                 params = list(data = list(serialize(existing_fits, NULL))))
  DBI::dbDisconnect(fishset_db)
  
  out <- list(
    k_folds = k,
    avg_out_sample_accuracy = mean(cv_results$Out_Sample_Acc),
    avg_out_sample_logLik = mean(cv_results$Out_Sample_LL),
    fold_details = cv_results
  )
  class(out) <- "fishset_cv"
  
  return(out)
}


#' Print FishSET Cross Validation Results
#'
#' Formats and prints the output of a FishSET cross-validation run.
#' Displays the average out-of-sample performance and a detailed table of metrics
#' for each individual fold.
#'
#' @param x A \code{fishset_cv} object returned by \code{\link{fishset_cv}}.
#' @param digits Integer. The number of significant digits to use when printing numeric values. 
#'   Default is 4.
#' @param ... Additional arguments passed to the print method.
#'
#' @method print fishset_cv
#' @export
print.fishset_cv <- function(x, digits = 4, ...) {
  
  # Helper for formatting stats consistently
  fmt <- function(n, d = 2) format(round(n, d), nsmall = d)
  
  # Header
  cat("\nFishSET Cross-Validation Results\n")
  cat("========================================================\n")
  
  # Overall Summary
  cat("Total Folds:             ", x$k_folds, "\n")
  cat("Avg Out-of-Sample LL:    ", fmt(x$avg_out_sample_logLik, 2), "\n")
  cat("Avg Out-of-Sample Acc:   ", fmt(x$avg_out_sample_accuracy * 100, 1), "%\n")
  
  # Detailed Folds Table
  cat("\nFold Details:\n")
  cat("--------------------------------------------------------\n")
  
  # Create a copy of the dataframe to format purely for printing
  df_print <- x$fold_details
  
  # Format numeric columns for clean console output
  df_print$In_Sample_LL   <- fmt(df_print$In_Sample_LL, 2)
  df_print$Out_Sample_LL  <- fmt(df_print$Out_Sample_LL, 2)
  df_print$In_Sample_Acc  <- paste0(fmt(df_print$In_Sample_Acc * 100, 1), "%")
  df_print$Out_Sample_Acc <- paste0(fmt(df_print$Out_Sample_Acc * 100, 1), "%")
  
  # Print the formatted dataframe without row numbers
  print(df_print, row.names = FALSE, right = TRUE)
  
  cat("========================================================\n")
  
  # Invisibly return the original object so assignment works if they call print() directly
  invisible(x)
}