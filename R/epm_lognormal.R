epm_lognormal <- function(starts3, dat, otherdat, alts, project, expname, mod.name) {
  #' Expected profit model with log-normal catch function
  #'
  #' Calculate the negative log-likelihood of the expected profit model (EPM) with log-normal catch 
  #' function. For more information on the EPM lognormal model see section 8.4.5 in the FishSET 
  #' user manual.
  #' \url{https://docs.google.com/document/d/1dzXsVt5iWcAQooDDXRJ3XyMoqnSmpZOqirU_f_PnQUM/edit#heading=h.ps7td88zo4ge}
  #'
  #' @param starts3 Starting parameter values as a numeric vector. The order of parameters in the 
  #'     vector is: \cr 
  #'     c([\emph{catch-function params}], [\emph{travel-dist params}], 
  #'     [\emph{stdev}], [\emph{common scale param}]), \cr
  #'     where the length of catch-function parameters is the # of alternatives * # of 
  #'     catch-function variables, length of travel-distance parameters is the # of 
  #'     travel-distance variables, length of standard deviation defaults to 1 but alternative-
  #'     specific standard deviation values can be specified (length = # of alternatives), and the
  #'     common scale parameter is a single value.
  #' @param dat Data matrix, see output from \code{\link{shift_sort_x}}, alternatives with
  #'     distance.
  #' @param otherdat List that contains other data used in the model, see section 8.4.5 in the 
  #'     FishSET user manual for more details (link in the description above):
  #'     (1) \strong{'griddat'}: catch-function variables that interact with alternative-specific
  #'     catch-function parameters and do not vary across alternatives (e.g., vessel gross 
  #'     tonnage). (2) \strong{'intdat'}: travel-distance variables that interact with 
  #'     travel-distance parameters and the distance matrix and do not vary across alternatives.
  #'     (3) \strong{'prices'}: price in terms of $/landings units. This is typically a vector with
  #'     prices for each observation, but can be a single value representing price for the
  #'     entire dataset.
  #' @param alts Number of alternative choices in model
  #' @param project Name of project
  #' @param expname Expected catch table (optional)
  #' @param mod.name Name of model run for model result output table
  #' @return ld: negative log likelihood
  #' @details This function is called in \code{\link{discretefish_subroutine}} when running an EPM model with
  #'     a log-normal catch function.
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
  griddat <- matrix(apply(griddat, 2, function(x) rep(x, times = alts)), obsnum, gridnum * alts) # replicate the vector for each variable by the number of alternatives
  
  # Format travel-distance variables (T_in in R user manual equation) that interact with
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
  
  # Format standard deviation parameter (sigma_j in R user manual equation) and common scale parameter
  #   (sigma_epsilon in user manual). If length of values before scale parameter is not equal to
  #   the number of alts, use first param for standard deviation
  if ((dim(starts3)[1] - ((gridnum * alts) + intnum + 1)) == alts) {
    stdev <- as.matrix(starts3[((gridnum * alts) + intnum + 1):((gridnum * alts) + intnum + alts), ])
    stdevnum <- alts # different standard deviation for each alternative
    sig <- as.matrix(starts3[((gridnum * alts) + intnum + alts + 1):length(starts3), ])
  } else {
    stdev <- as.matrix(starts3[((gridnum * alts) + intnum + 1), ])
    stdevnum <- 1
    sig <- as.matrix(starts3[((gridnum * alts) + intnum + 2):length(starts3), ])
  }

  # Force stdev to be a positive value
  stdev_exp <- exp(stdev)
  
  
  ##
  ## Calculate likelihood for choice component ----
  ##
  # beta_jm * G_im
  gridbetas <- (matrix(gridcoef, obsnum, alts * gridnum, byrow = TRUE) * griddat)
  dim(gridbetas) <- c(nrow(gridbetas), alts, gridnum)

  # SUM(beta_jm * G_im)
  gridbetas <- rowSums(gridbetas, dims = 2)
  
  # expected mean catch function (mean of lognormal distribution)
  gridmu <- exp(gridbetas + (0.5 * (matrix(stdev_exp, obsnum, alts, byrow = TRUE)^2)))

  # cost portion of the likelihood
  intbetas <- .rowSums(intdat * matrix(intcoef, obsnum, intnum, byrow = TRUE), obsnum, intnum)

  # revenue portion of the likelihood (i.e., price * expected mean catch) followed by cost portion, saved in the same matrix
  rev_cost_betas <- matrix(c((gridmu * matrix(pricedat, obsnum, alts)), intbetas), obsnum, (alts + 1))
  
  # (Revenue * dummy variable indicating zones fished) and (cost * distance) in a single, large matrix
  # Note: distances in dat are shifted/sorted, same as choice possibilities, even though the zone column names retain the original order of zones
  djztemp <- rev_cost_betas[1:obsnum, rep(1:ncol(rev_cost_betas), each = alts)] * dat[, 3:(dim(dat)[2])]
  
  # reformat matrix [d1 = num of obs, d2 = num of alts, d3 = num of alts + 1 (zonal coefs in revenue and cost)]
  dim(djztemp) <- c(nrow(djztemp), ncol(djztemp) / (alts + 1), alts + 1)
  
  # revenue + cost for each zone
  prof <- rowSums(djztemp, dims = 2)
  
  # relative to the first column so set = 0 
  profx <- prof - prof[, 1]
  
  # exp(beta*G + gamma*T*D) for each zone
  exb <- exp(profx / matrix(sig, dim(prof)[1], dim(prof)[2]))

  # log-likelihood (for each observation) for choice component of the model
  ldchoice <- (-log(rowSums(exb)))
  

  ##
  ## Calculate likelihood for catch component ----
  ##
  yj <- matrix(dat[, 1]) # actual catch
  cj <- matrix(dat[, 2]) # actual choice

  # get the standard deviation parameter for each observation
  if (stdevnum == 1) {
    empstdev <- stdev_exp
  } else {
    empstdev <- stdev_exp[cj]
  }

  # format beta parameters
  empgridbetas <- t(gridcoef)
  dim(empgridbetas) <- c(nrow(empgridbetas), alts, gridnum)
  
  # format catch-function variables that interact with beta parameters
  empgriddat <- griddat
  dim(empgriddat) <- c(nrow(empgriddat), alts, gridnum)
  
  # calculate mean of the lognormal catch function
  empgridmu <- .rowSums(empgridbetas[, cj, ] * empgriddat[, 1, ], obsnum, gridnum) # note grid data same across all alternatives

  # log-likelihood log-normal function
  ldcatch <- (matrix((-(log(yj))), obsnum)) + (matrix((-(log(empstdev))), obsnum)) + (matrix((-(0.5) * log(2 * pi)), obsnum)) +
    (-(((matrix(log(yj), obsnum) - empgridmu)^2) / (2 * (matrix(empstdev, obsnum)^2))))
    
  # sum log-likelihood (for each observation) for catch and choice components of the EPM model
  ld1 <- ldcatch + ldchoice
  
  
  ##
  ## Negative log-likelihood ----
  ld <- -sum(ld1)

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

