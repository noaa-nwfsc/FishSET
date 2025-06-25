#' Welfare analysis
#' 
#' Simulate the welfare loss/gain from changes in policy or changes in other factors that influence fisher 
#' location choice.
#' 
#' @param project Name of project
#' @param mod.name Name of selected model (mchoice)
#' @param closures Closure scenarios
#' @param betadraws Integer indicating the numer of times to run the welfare simulation. Default value is
#'   \code{betadraws = 1000}
#' @param marg_util_income For conditional and zonal logit models. Name of the coefficient to use as 
#'    marginal utility of income
#' @param income_cost For conditional and zonal logit models. Logical indicating whether the coefficient 
#'    for the marginal utility of income relates to cost (\code{TRUE}) or revenue (\code{FALSE})
#' @param expected.catch Name of expectedchatch table to use
#' @param enteredPrice Price for welfare
#' @importFrom DBI dbConnect dbDisconnect dbGetQuery
#' @importFrom RSQLite SQLite
#' @importFrom data.table fwrite
#' @export
#' @details To simulate welfare loss/gain, the model coefficients are sampled 1000 times using a multivariate random
#'    number generator (\code{\link{mvgrnd}}) and the welfare loss/gain for each observation is calculated (see section 9.3 
#'    in the user manual) for each of the sampled coefficients, and all of the estimated welfare values are saved to a file
#'    in the project outputs folder. \cr\cr
#'    Note that this function is called by \code{\link{run_policy}}.

welfare_predict <- function(project, mod.name, closures, betadraws = 1000, marg_util_income = NULL, income_cost = NULL, expected.catch = NULL, enteredPrice = NULL){
  
  #TODO make stdard version, only works for zone that all 100% closed and
  #compares closure at entered price versus non closure at that price (not a comparison to other years
  #inputs:
  #   filename: filename of model file to be used
  #   zoneCloseMap: which zones have been closed
  #   tac: percent of closure of a zone
  #   enteredPrice: the price entered in the gui by user for welfare
  #   determination
  #output:
  # prW
  # welfare
  # numTrips
  
  #---
  # Create connection to fishset database ----
  #---
  fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project=project))
  on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
  
  #---
  # Pull output data from model optimization ----
  #---
  # Get list with all model outputs
  policy.name <- unlist(lapply(closures, function(x){x$scenario}))  
  
  theta_list <- vector("list", length(mod.name)) 
  names(theta_list) <- mod.name # Assign policy names to the inner list
  
  welfare_output <- vector("list", length(mod.name)) # Stores final welfare results
  prcwelfare_output <- vector("list", length(mod.name)) 
  
  names(welfare_output) <- mod.name # Assign model names to the outer list
  names(prcwelfare_output) <- mod.name
  
  scenario_names <- unlist(lapply(closures, function(x){x$scenario}))  
  
  for (k in seq_along(mod.name)) { 
    
    # Get model likelihood
    mod.ll <- model_design_list(project=project)[[which(lapply(model_design_list(project=project), "[[", "mod.name") == mod.name[[k]])]]$likelihood
    
    selected_mod <- model_out_view(project=project)[[which(lapply(model_design_list(project=project), "[[", "mod.name") == mod.name[[k]])]]
    
    # Get parameter estimates
    Eq <- selected_mod$OutLogit[,1]
    
    
    #---
    # Positive definite check for inverse Hessian ----
    #---
    # If the inverse Hessian is positive definite then the function is a minimum
    # Get inverse hessian for selected model
    invHess <- selected_mod$H1 
    
    flag <- 0
    
    # Try Cholesky factorization, if successful the matrix should be positive definite
    tryCatch(
      {test.pd <- chol(invHess)},
      error = function(err){flag <<- 1}
    )
    if(flag == 1){
      stop(paste0('Inverse Hessian matrix not positive definite and Cholesky factorization failed'), call. = FALSE)
    }
    
    # Number of closure scenarios
    n_scenarios <- length(closures)
    # Create empty matrices
    welfare_output_k <- vector("list", n_scenarios)          # Temporary storage for this model
    prcwelfare_output_k <- vector("list", n_scenarios) 
    
    
    names(welfare_output_k) <- policy.name                   # Assign policy names to the inner list
    names(prcwelfare_output_k) <- policy.name
    
    
    #---
    # Pull data from predicted output table generated in model_prediction() ----
    #---
    # Get predicted output table
    predict_temp <- unserialize_table(paste0(project, "predictOutput"), project)
    predict_temp <- predict_temp[which(unlist(lapply(predict_temp, function(x) grepl(mod.name[[k]], x$scenario.name))))]
    
    
    #---
    # Settings for welfare predictions ----
    #---
    # Draw simulation parameter values from multivariate random number generator
    mu_rand <- t(mvgrnd(Eq, invHess, betadraws))  # flipped so that rows are output and columns are draws
    
    
    #---
    # Get model data ----
    #---
    # Need data compile, distance, gridvarying, interaction terms. Only need first one because modDat is the same for all predict_temp items
    x <- predict_temp[[1]]$modelDat
    griddat <- as.matrix(do.call(cbind, x$otherdat$griddat))    
    intdat <- as.matrix(do.call(cbind, x$otherdat$intdat))
    distance <- x$distance
    alts <- length(predict_temp[[1]]$zoneID)
    obsnum <- dim(griddat)[1]
    zoneIDs <- predict_temp[[1]]$zoneID
    
    # Price (EPMs) or marginal utility of income (non-EPMs)
    # If EPM, then get price data
    if(grepl("epm", mod.ll)){
      # If entered price is null, then use price from data
      if(is.null(enteredPrice)){
        pricedat <- matrix(x$otherdat$pricedat)
        
        # Else if entered price is the length of obsnum, or just a single value, then set to pricedat
      } else {
        pricedat <- matrix(enteredPrice, nrow = obsnum)
      } 
    } 
    
    #---
    # Save coefficients ----
    #---
    # Get number of variables
    if(grepl("epm", mod.ll)){
      gridnum <- dim(griddat)[2]  
      
    } else if(mod.ll == "logit_c"){
      gridnum <- dim(griddat)[2] / alts
      
    } else if(mod.ll == "logit_zonal"){
      gridnum <- dim(griddat)[2] * (alts - 1) # (grid variables * num of coefficients) calculation here makes following code cleaner
    }
    intnum <- dim(intdat)[2]
    z <- length(Eq) # number of parameters
    
    
    #---
    # Loop through welfare scenarios ----
    #---
    for(j in 1:length(scenario_names)){
      tmp_close <- predict_temp[[j]]$zoneIdIn
      closeID <- which(zoneIDs %in% tmp_close)
      zones <- which(!(zoneIDs %in% tmp_close))
      
      # Create empty betadraw matrices
      welfare_betadraws <- prc_welfare_betadraws <- matrix(data = NA, nrow = obsnum, ncol = betadraws)
      
      # Loop through betadraws
      for(l in 1:betadraws){
        mu_rand_new <- mu_rand[,l]
        
        ## EPM WELFARE ----
        if(grepl('epm', predict_temp[[1]]$type, ignore.case=TRUE)){
          # Get coefficients
          gridcoef <- mu_rand_new[1:(alts * gridnum)]
          intcoef <- mu_rand_new[((alts * gridnum) + 1):((alts * gridnum) + intnum)]
          
          ### Weibull ----
          if(predict_temp[[1]]$type == "epm_weibull"){
            if ((z - ((gridnum * alts) + intnum + 1)) == alts) {
              k <- mu_rand_new[((gridnum * alts) + intnum + 1):((gridnum * alts) + intnum + alts)]
              knum <- alts
              sig <- mu_rand_new[((gridnum * alts) + intnum + alts + 1):z]
            } else {
              k <- mu_rand_new[((gridnum * alts) + intnum + 1)]
              knum <- 1
              sig <- mu_rand_new[((gridnum * alts) + intnum + 2):z]
            }
            k_exp <- exp(k)
            gamma_kexp <- gamma((k_exp + 1)/k_exp)
            
            ## Calculate welfare
            # (beta_jm * G_im)
            gridbetas <- (matrix(gridcoef, obsnum, alts * gridnum, byrow = TRUE) * matrix(griddat, obsnum, alts * gridnum, byrow = TRUE))
            dim(gridbetas) <- c(nrow(gridbetas), alts, gridnum)
            # SUM(beta_jm * G_im), which is the scale portion of the Weibull function
            gridbetas <- rowSums(gridbetas, dims = 2)
            # Scale portion of the weibull function is constrained to positive values with exponential function in epm_weibull.R
            gridbetas_exp <- exp(gridbetas)
            # Expected mean catch function
            gridmu <- gridbetas_exp * matrix(gamma((k_exp + 1) / k_exp), obsnum, alts)
            # Revenue
            revbetas <- gridmu * matrix(pricedat, obsnum, alts)
            # Cost portion of the likelihood
            intbetas <- .rowSums(intdat * matrix(intcoef, obsnum, intnum, byrow = TRUE), obsnum, intnum)
            costbetas <- matrix(intbetas, obsnum, alts) * as.matrix(distance)
            # numerator from logit component of epm model
            numer <- exp((revbetas + costbetas) / matrix(sig, obsnum, alts))
            # Welfare before closure
            Wb <- log(rowSums(numer))
            # Calculate welfare after closure
            Wa <- log(rowSums(numer[,-(closeID)]))
            # Welfare loss/gain
            tmp_welfare <- sig * (Wa - Wb)
            tmp_prc_welfare <- ((sig*Wa) - (sig*Wb))/((sig*Wb) + (0.57721*sig))
            
            ## lognormal ----
          } else if(predict_temp[[1]]$type == "epm_lognormal"){
            # Get coefficients
            if ((z - ((gridnum * alts) + intnum + 1)) == alts) {
              stdev <- mu_rand_new[((gridnum * alts) + intnum + 1):((gridnum * alts) + intnum + alts)]
              stdevnum <- alts
              sig <- mu_rand_new[((gridnum * alts) + intnum + alts + 1):z]
            } else {
              stdev <- mu_rand_new[((gridnum * alts) + intnum + 1)]
              stdevnum <- 1
              sig <- mu_rand_new[((gridnum * alts) + intnum + 2):z]
            }
            stdev_exp <- exp(stdev)
            
            ## Calculate welfare
            # (beta_jm * G_im)
            gridbetas <- (matrix(gridcoef, obsnum, alts * gridnum, byrow = TRUE) * matrix(griddat, obsnum, alts * gridnum, byrow = TRUE))
            dim(gridbetas) <- c(nrow(gridbetas), alts, gridnum)
            # SUM(beta_jm * G_im), which is mu in the lognormal function
            gridbetas <- rowSums(gridbetas, dims = 2)
            # Expected mean catch function (mean of lognormal distribution)
            gridmu <- exp(gridbetas + (0.5 * (matrix(stdev_exp, obsnum, alts, byrow = TRUE)^2)))
            # Revenue
            revbetas <- gridmu * matrix(pricedat, obsnum, alts)
            # Cost portion of the likelihood
            intbetas <- .rowSums(intdat * matrix(intcoef, obsnum, intnum, byrow = TRUE), obsnum, intnum)
            costbetas <- matrix(intbetas, obsnum, alts) * as.matrix(distance)
            # numerator from logit component of epm model
            numer <- exp((revbetas + costbetas) / matrix(sig, obsnum, alts))
            # Welfare before closure
            Wb <- log(rowSums(numer))
            # Calculate welfare after closure
            Wa <- log(rowSums(numer[,-(closeID)]))
            # Welfare loss/gain
            tmp_welfare <- sig * (Wa - Wb)
            tmp_prc_welfare <- ((sig*Wa) - (sig*Wb))/((sig*Wb) + (0.57721*sig))
            
            ## normal ----
          } else if(predict_temp[[1]]$type == "epm_normal"){
            # Get coefficients
            if ((z - ((gridnum * alts) + intnum + 1)) == alts) {
              stdev <- mu_rand_new[((gridnum * alts) + intnum + 1):((gridnum * alts) + intnum + alts)]
              stdevnum <- alts
              sig <- mu_rand_new[((gridnum * alts) + intnum + alts + 1):z]
            } else {
              stdev <- mu_rand_new[((gridnum * alts) + intnum + 1)]
              stdevnum <- 1
              sig <- mu_rand_new[((gridnum * alts) + intnum + 2):z]
            }
            stdev_exp <- exp(stdev)
            
            ## Calculate welfare
            # (beta_jm * G_im)
            gridbetas <- (matrix(gridcoef, obsnum, alts * gridnum, byrow = TRUE) * matrix(griddat, obsnum, alts * gridnum, byrow = TRUE))
            dim(gridbetas) <- c(nrow(gridbetas), alts, gridnum)
            # SUM(beta_jm * G_im), which is mu and expected catch for normal function
            gridbetas <- rowSums(gridbetas, dims = 2)
            gridmu <- gridbetas
            # Revenue
            revbetas <- gridmu * matrix(pricedat, obsnum, alts)
            # Cost portion of the likelihood
            intbetas <- .rowSums(intdat * matrix(intcoef, obsnum, intnum, byrow = TRUE), obsnum, intnum)
            costbetas <- matrix(intbetas, obsnum, alts) * as.matrix(distance)
            # numerator from logit component of epm model
            numer <- exp((revbetas + costbetas) / matrix(sig, obsnum, alts))
            # Welfare before closure
            Wb <- log(rowSums(numer))
            # Calculate welfare after closure
            Wa <- log(rowSums(numer[,-(closeID)]))
            # Welfare loss/gain
            tmp_welfare <- sig * (Wa - Wb)
            tmp_prc_welfare <- ((sig*Wa) - (sig*Wb))/((sig*Wb) + (0.57721*sig))
          }
          
          
        } else if (grepl('logit', predict_temp[[1]]$type, ignore.case=TRUE)) {
          ## LOGIT WELFARE ----
          # Get marginal utility of income
          theta <- mu_rand_new[which(rownames(selected_mod$OutLogit) == marg_util_income[[k]])]
          
          if(income_cost[[k]]){ ## if income cost variable == TRUE
            theta <- -theta
          } 
          if(theta < 0){
            if(!shiny::isRunning()){
              stop(paste0("Marginal utility of income is negative , ", mod.name[[k]], ". Check model coefficient (estimate and standard error) and select appropriate marginal utility of income."))
            }
            if(shiny::isRunning()){
              theta <- theta
            }
          }
          
          
          # set up distance matrix
          distance <- as.matrix(distance, nrow = dim(distance)[1], ncol = dim(distance)[2])
          
          # Get coefficients
          gridcoef <- mu_rand_new[1:gridnum]
          
          intcoef <- mu_rand_new[(gridnum + 1):(gridnum + intnum)]
          
          ## logit_c ----
          if(mod.ll == "logit_c"){
            # calculate the sum of the products of the grid coefficients and the gridvarying variables
            gridbetas <- matrix(rep(gridcoef, each = alts), nrow = obsnum, ncol = alts*gridnum, byrow = TRUE) * griddat
            dim(gridbetas) <- c(nrow(gridbetas), alts, gridnum)
            # Following three lines replaced rowSums() to improve runtime
            dims <- dim(gridbetas)
            gridbetas <- array(gridbetas, c(dims[1] * dims[2], dims[3])) %*% rep(1, dims[3])
            gridbetas <- array(gridbetas, c(dims[1], dims[2]))
            
            ## logit_zonal ----
          } else if(mod.ll == "logit_zonal"){
            # insert 0 for the first alternative (interpretation is relative to the first alt and this beta_1 = 0)
            gridcoef <- matrix(gridcoef, ncol = (alts - 1), byrow = TRUE) # get grid coefficients for each alternative
            gridcoef <- cbind(matrix(0, nrow = dim(griddat)[2], ncol = 1), gridcoef) # insert 0 for the first alternative (other coefs are relative to the first alt)
            gridcoef <- as.vector(t(gridcoef))
            
            # calculate the sum of the products of the alternative-specific coefficients and the alternative-invariant variables
            gridbetas <- matrix(rep(gridcoef), nrow = obsnum, ncol = alts * dim(griddat)[2], byrow = TRUE)
            gridbetas <- gridbetas * matrix(rep(t(griddat), each = alts), nrow = obsnum, byrow = TRUE)
            dim(gridbetas) <- c(nrow(gridbetas), alts, dim(griddat)[2])
            gridbetas <- rowSums(gridbetas, dims = 2)
          }
          
          # calculate the sum of the products of the distance coefficient, alternative invariant variables that interacts with distance, and the distance between locations
          intbetas <- matrix(intcoef, nrow = obsnum, ncol = intnum, byrow = TRUE) * intdat
          intbetas <- matrix(apply(intbetas, 2, function(x) rep(x, alts)), ncol = intnum * alts) * matrix(rep(distance, intnum), nrow = obsnum, ncol = alts * intnum)
          dim(intbetas) <- c(nrow(intbetas), alts, intnum)
          
          
          # intbetas <- rowSums(intbetas, dims = 2)
          
          dims <- dim(intbetas)
          intbetas <- array(intbetas, c(dims[1] * dims[2], dims[3])) %*% rep(1, dims[3])
          intbetas <- array(intbetas, c(dims[1], dims[2]))
          
          # Welfare before closure
          Wb <- log(as.matrix(rowSums(exp(gridbetas + intbetas))))
          # Welfare after closure
          Wa <- log(as.matrix(rowSums(exp(gridbetas[,-(closeID)] + intbetas[,-(closeID)]))))
          
          
          # Welfare loss/gain
          tmp_welfare <- (1/theta) * (Wa - Wb)
          tmp_prc_welfare <- ((1/theta) * (Wa - Wb)) / ((1/theta) * Wb)
          
        }
        
        #income_list[[b]] <- theta
        # Save output for each betadraw of parameters
        
        welfare_betadraws[,l] <- tmp_welfare
        prc_welfare_betadraws[,l] <- tmp_prc_welfare
        
      } # close betadraws
      
      print(paste("Model:", k, "Scenario:", j))
      # Save all data from each simulation and each closure scenario
      welfare_output_k[[j]] <- welfare_betadraws
      prcwelfare_output_k[[j]] <- prc_welfare_betadraws
      
      
    } # close n_scenario
    
    welfare_output[[k]] <- welfare_output_k
    prcwelfare_output[[k]] <- prcwelfare_output_k
    
    theta_list[[k]] <- theta
  }
  
  flatten_list <- function(df, parent_names = NULL) {
    data.table::rbindlist(
      lapply(names(df), function(name) {
        element <- df[[name]]
        if (is.list(element) && !is.data.frame(element)) {
          flatten_list(element, c(parent_names, name))
        } else {
          data.table::data.table(
            setNames(as.list(element), names(element)),
            model_name = parent_names[1],
            # level2 = parent_names[2],
            policy_name = name
          )
        }
      }),
      fill = TRUE
    )
  }
  
  # Flatten the nested list
  welfare_output_df <- flatten_list(welfare_output)
  prcwelfare_output_df <- flatten_list(prcwelfare_output)
  
  # Write data to csv files - files are overwritten each time welfare_predict() is called
  data.table::fwrite(welfare_output_df, file = paste0(locoutput(project), "welfare_output_df.csv"), col.names = TRUE, row.names = FALSE)
  data.table::fwrite(prcwelfare_output_df, file = paste0(locoutput(project), "prcwelfare_output_df.csv"), col.names = TRUE, row.names = FALSE)
  
  return(theta_list)
}


