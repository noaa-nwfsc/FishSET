epm_weibull <- function(starts3, dat, otherdat, alts, project, expname, mod.name) {
  #' Expected profit model with Weibull catch function
  #'
  #' Calculate the negative log-likelihood of the expected profit model (EPM) with Weibull catch 
  #' function. For more information on the EPM Weibull model see section 8.4.4 in the FishSET 
  #' user manual.
  #' \url{https://docs.google.com/document/d/1dzXsVt5iWcAQooDDXRJ3XyMoqnSmpZOqirU_f_PnQUM/edit#heading=h.gh3zw8f9nsdi}
  #'
  #' @param starts3 Starting parameter values as a numeric vector. The order of parameters in the 
  #'     vector is: \cr 
  #'     c([\emph{catch-function params}], [\emph{travel-dist params}], 
  #'     [\emph{shape params}], [\emph{common scale param}]), \cr
  #'     where the length of catch-function parameters is the # of alternatives * # of 
  #'     catch-function variables, length of travel-distance parameters is the # of 
  #'     travel-distance variables, length of shape parameters defaults to 1 but alternative-
  #'     specific shape parameters can be specified (length = # of alternatives), and the
  #'     common scale parameter is a single value.
  #' @param dat Data matrix, see output from \code{\link{shift_sort_x}}, alternatives with
  #'     distance.
  #' @param otherdat List that contains other data used in the model, see section 8.4.4 in the 
  #'     FishSET user manual for more details (link in the description above):
  #'     (1) \strong{'griddat'}: catch-function variables that interact with alternative-specific
  #'     catch-function parameters and do not vary across alternatives (e.g., vessel gross 
  #'     tonnage). (2) \strong{'intdat'}: travel-distance variables that interact with 
  #'     travel-distance parameters and the distance matrix and do not vary across alternatives.
  #'     (3) \strong{'prices'}: price in terms of $/landings units. This is typically a vector with
  #'     prices for each observation, but can be a single value representing price for the
  #'     entire dataset.
  #' @param alts Number of alternative choices in the model
  #' @param project Name of project
  #' @param expname Expected catch table (optional)
  #' @param mod.name Name of model run for model result output table
  #' @return ld: negative log likelihood
  #' @details This function is called in \code{\link{discretefish_subroutine}} when running an EPM model with
  #'     a Weibull catch function.
  #' @export
  
  ##
  ## Set up variables ----
  ##
  # Get number of observations
  obsnum <- dim(as.data.frame(otherdat$griddat))[1]
  
  # Format catch-function variables (G_im in R user manual equation) that interact with 
  #   alternative-specific parameters (beta_jm in R user manual equation)
  griddat <- as.matrix(do.call(cbind, otherdat$griddat))
  gridnum <- dim(griddat)[2] # number of catch-function variables
  griddat <- matrix(apply(griddat, 2, function(x) rep(x, times = alts)), obsnum, gridnum * alts) # replicate the vector for each variable by the number of alternatives and cbind
  
  # Format travel-distance variables (T_in in R user manual) that interact with 
  #   distance matrix (D_ij) and alternative-invariant parameters (gamma_n)
  intdat <- as.matrix(do.call(cbind, otherdat$intdat))
  intnum <- dim(intdat)[2] # number of travel-distance variables

  # Format price data
  pricedat <- as.matrix(unlist(otherdat$pricedat))

  
  ##
  ## Parse initial values and coefficients ----
  ##
  # Format initial values and coefficients
  starts3 <- as.matrix(starts3)
  gridcoef <- as.matrix(starts3[1:(gridnum * alts), ]) # beta_jm params
  intcoef <- as.matrix(starts3[((gridnum * alts) + 1):((gridnum * alts) + intnum), ]) # gamma_n params

  # Format shape parameter (k in R user manual) and common scale parameter (sigma_epsilon in R user manual)
  # if number of parameters before scale parameter is not equal to number of alts, use first parameter for shape (k)
  if ((dim(starts3)[1] - ((gridnum * alts) + intnum + 1)) == alts) {
    k <- as.matrix(starts3[((gridnum * alts) + intnum + 1):((gridnum * alts) + intnum + alts), ])
    knum <- alts # different shape param for each alternative
    sig <- as.matrix(starts3[((gridnum * alts) + intnum + alts + 1):length(starts3), ])
  } else {
    k <- as.matrix(starts3[((gridnum * alts) + intnum + 1), ])
    knum <- 1
    sig <- as.matrix(starts3[((gridnum * alts) + intnum + 2):length(starts3), ])
  }

  # Force k to be a positive value
  k_exp <- exp(k)
  
    
  ##
  ## Calculate likelihood for choice component ----
  ##
  # beta_jm * G_im
  gridbetas <- (matrix(gridcoef, obsnum, alts * gridnum, byrow = TRUE) * griddat)
  dim(gridbetas) <- c(nrow(gridbetas), alts, gridnum)
  
  # SUM(beta_jm * G_im), which is the scale portion of the Weibull function
  gridbetas <- rowSums(gridbetas, dims = 2)
  
  # constrain Weibull scale to positive value with exponential function
  gridbetas_exp <- exp(gridbetas)

  # expected mean catch function (mean of weibull distribution)
  gridmu <- gridbetas_exp * matrix(gamma((k_exp + 1) / k_exp), obsnum, alts, byrow = TRUE)

  # Cost portion of the likelihood
  intbetas <- .rowSums(intdat * matrix(intcoef, obsnum, intnum, byrow = TRUE), obsnum, intnum)

  # Revenue portion of the likelihood (i.e., price * expected mean catch) followed by cost portion, saved in the same matrix
  rev_cost_betas <- matrix(c((gridmu * matrix(pricedat, obsnum, alts)), intbetas), obsnum, (alts + 1))

  # (Revenue * dummy variable indicating zones fished) and (cost * distance) in a single, large matrix
  # Note: distances in dat are shifted/sorted, same as choice possibilities, even though the zone column names retain the original order of zones
  djztemp <- rev_cost_betas[1:obsnum, rep(1:ncol(rev_cost_betas), each = alts)] * dat[, 3:(dim(dat)[2])]
  
  # Reformat matrix [d1 = num of obs, d2 = num of alts, d3 = num of alts + 1 (zonal coefficients in revenue portion and cost portion)]
  dim(djztemp) <- c(nrow(djztemp), ncol(djztemp) / (alts + 1), alts + 1)

  # Revenue + cost for each zone
  prof <- rowSums(djztemp, dims = 2)
  
  # Relative to the first column so set = 0
  profx <- prof - prof[, 1]

  # Exp(Beta*G + Gamma*T*D) for each zone
  exb <- exp(profx / matrix(sig, dim(prof)[1], dim(prof)[2]))

  # Log-likelihood (for each observation) for choice component of the model
  ldchoice <- (-log(rowSums(exb)))
  
  
  ##
  ## Calculate likelihood for catch component ----
  ##
  yj <- matrix(dat[, 1]) # actual catch
  cj <- matrix(dat[, 2]) # actual choice
  
  # Get the shape parameter for each observation
  if (knum == 1) {
    empk <- k_exp
  } else {
    empk <- k_exp[cj]
  }

  # Format beta parameters
  empgridbetas <- t(gridcoef)
  dim(empgridbetas) <- c(nrow(empgridbetas), alts, gridnum)

  # Format catch-function variables that interact with beta parameters
  empgriddat <- griddat
  dim(empgriddat) <- c(nrow(empgriddat), alts, gridnum)

  # Calculate scale portion of the Weibull catch function
  empgridmu <- .rowSums(empgridbetas[, cj, ] * empgriddat[, 1, ], obsnum, gridnum) # note grid data same across all alternatives
  
  # Force values to be positive
  empgridmu <- exp(empgridmu)
  
  # Log-likelihood Weibull function
  # log(k) + (-k * log(lambda)) + ((k - 1) * log(Y)) + (-((Y / lambda)^(k)))
  ldcatch <- (matrix((log(empk)), obsnum)) + (matrix((-(empk)), obsnum) * log(empgridmu)) + (matrix((empk - 1), obsnum) * log(yj)) + 
    (-((yj / empgridmu)^(matrix(empk, obsnum))))
  
  # Sum log-likelihoods (for each observation) for catch and choice components of EPM model
  ld1 <- ldcatch + ldchoice

  
  ##
  ## Negative log-likelihood ----
  ##
  ld <- -(sum(ld1))

  if (is.nan(ld) == TRUE) {
    ld <- .Machine$double.xmax
  }

  ldsumglobalcheck <- ld
  paramsglobalcheck <- starts3
  LDGlobalCheck <- unlist(as.matrix(ld1))

  LDGlobalCheck <- list(model = paste0(project, expname, mod.name), ldsumglobalcheck = ldsumglobalcheck, paramsglobalcheck = paramsglobalcheck, LDGlobalCheck = LDGlobalCheck)

  pos <- 1
  envir = as.environment(pos)
  assign("LDGlobalCheck", value = LDGlobalCheck, envir = envir)
  # assign("LDGlobalCheck", value = LDGlobalCheck, pos = 1, envir = rlang::caller_env())
  return(ld)
}
