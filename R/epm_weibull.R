epm_weibull <- function(starts3, dat, otherdat, alts, project, expname, mod.name) {
  #' Expected profit model Weibull catch function
  #'
  #' @param starts3 Starting values as a vector (num). For this likelihood,
  #'     the order takes: c([catch-function parameters], [travel-distance
  #'     parameters], [shape parameter], [catch sigma(s)]). \cr \cr
  #'     The catch-function and travel-distance parameters are of length (# of
  #'     catch-function variables)*(k) and (# of travel-distance variables)
  #'     respectively, where (k) equals the number of alternatives. The catch
  #'     sigma(s) are either of length equal to unity or length (k) if the
  #'     analyst is estimating location-specific catch sigma parameters. The
  #'     scale parameter is of length equal to unity.
  #' @param dat Data matrix, see output from shift_sort_x, alternatives with
  #'     distance.
  #' @param otherdat Other data used in model (as a list containing objects
  #'     `intdat`, `griddat`, and `prices`). \cr \cr
  #'     For this likelihood, `intdat` are 'travel-distance variables', which
  #'     are alternative-invariant variables that are interacted with travel
  #'     distance to form the cost portion of the likelihood. Each variable
  #'     name therefore corresponds to data with dimensions (number of
  #'     observations) by (unity), and returns a single parameter. \cr \cr
  #'     In `griddat` are 'catch-function variables' that are
  #'     alternative-invariant variables that are interacted with zonal
  #'     constants to form the catch portion of the likelihood. Each variable
  #'     name therefore corresponds to data with dimensions (number of
  #'     observations) by (unity), and returns (k) parameters where (k) equals
  #'     the number of alternatives. \cr \cr
  #'     For 'catch-function variables' `griddat` and 'travel-distance
  #'     variables' `intdat`, any number of variables are allowed, as a list
  #'     of matrices. Note the variables (each as a matrix) within `griddat`
  #'     `intdat` have no naming restrictions. 'Catch-function variables' may
  #'     correspond to variables that impact catches by location, or
  #'     interaction variables may be vessel characteristics that affect how
  #'     much disutility is suffered by traveling a greater distance. Note in
  #'     this likelihood the 'catch-function variables' vary across
  #'     observations but not for each location: they are allowed to impact
  #'     catches differently across alternatives due to the location-specific
  #'     coefficients. If there are no other data, the user can set `griddat`
  #'     as ones with dimension (number of observations) x (number of
  #'     alternatives) and `intdat` variables as ones with dimension (number
  #'     of observations) by (unity). \cr \cr
  #'     The variable `prices` is a matrix of dimension (number of
  #'     observations) by (unity), corresponding to prices.
  #' @param alts Number of alternative choices in model as length equal to
  #'     unity (as a numeric vector).
  #' @param project Name of project
  #' @param expname Expected catch table
  #' @param mod.name Name of model run for model result output table
  #' @return ld: negative log likelihood
  #' @export
  #' @examples
  #' \dontrun{
  #' data(zi)
  #' data(catch)
  #' data(choice)
  #' data(distance)
  #' data(si)
  #' data(prices)
  #'
  #' catch[catch<0] <- 0.00001
  #' #Note weibull catch distribution.
  #'
  #' optimOpt <- c(1000,1.00000000000000e-08,1,0)
  #'
  #' methodname <- 'BFGS'
  #'
  #' si2 <- sample(1:5,dim(si)[1],replace=TRUE)
  #' zi2 <- sample(1:10,dim(zi)[1],replace=TRUE)
  #'
  #' otherdat <- list(griddat=list(si=as.matrix(si),si2=as.matrix(si2)),
  #'     intdat=list(zi=as.matrix(zi),zi2=as.matrix(zi2)),
  #'     pricedat=list(prices=as.matrix(prices)))
  #'
  #' initparams <- c(2.5, 2.0, 1.5, 1.0, 1.1, 1.05, 0.9, 0.8, -0.8, -0.4, 3,
  #'     2, 3.5, 2.5, 1)
  #'
  #' func <- epm_weibull
  #'
  #' results <- discretefish_subroutine(catch,choice,distance,otherdat,
  #'     initparams,optimOpt,func,methodname)
  #' }
  #' @section Graphical examples:
  #' \if{html}{
  #' \figure{epm_weibull_grid.png}{options: width='40\%'
  #' alt='Figure: epm_weibull_grid.png'}
  #' \cr
  #' \figure{epm_weibull_travel.png}{options: width='40\%'
  #' alt='Figure: epm_weibull_travel.png'}
  #' \cr
  #' \figure{epm_weibull_sigma.png}{options: width='40\%'
  #' alt='Figure: epm_weibull_sigma.png'}
  #' }
  #'

  
  ##
  ## Set up variables ----
  ##
  # Get number of observations
  obsnum <- dim(as.data.frame(otherdat$griddat))[1]
  
  # Format choice occasion-specific variables (G_im in R user manual equation) that interact with alternative-specific parameters (beta_jm in R user manual equation)
  griddat <- as.matrix(do.call(cbind, otherdat$griddat))
  gridnum <- dim(griddat)[2] # number of occasion-specific variables
  griddat <- matrix(apply(griddat, 2, function(x) rep(x, times = alts)), obsnum, gridnum * alts) # replicate the vector for each variable by the number of alternatives and cbind
  
  # Format alternative-invariant variables (T_in in R user manual) that interact with distance (D_ij) and alternative-invariant parameters (gamma_n)
  intdat <- as.matrix(do.call(cbind, otherdat$intdat))
  intnum <- dim(intdat)[2]

  # Format price data
  pricedat <- as.matrix(unlist(otherdat$pricedat))

  
  ##
  ## Parse initial values and coefficients ----
  ##
  # Format initial values and coefficients
  starts3 <- as.matrix(starts3)
  gridcoef <- as.matrix(starts3[1:(gridnum * alts), ])
  intcoef <- as.matrix(starts3[((gridnum * alts) + 1):((gridnum * alts) + intnum), ])

  # Format shape parameter (k in R user manual) and catch scale parameter (sigma_epsilon in R user manual)
  # if number of parameters before scale parameter is not equal to number of alts, use first parameter for shape (k)
  if ((dim(starts3)[1] - ((gridnum * alts) + intnum + 1)) == alts) {
    k <- as.matrix(starts3[((gridnum * alts) + intnum + 1):((gridnum * alts) + intnum + alts), ])
    knum <- alts
    sig <- as.matrix(starts3[((gridnum * alts) + intnum + alts + 1):length(starts3), ])
  } else {
    k <- as.matrix(starts3[((gridnum * alts) + intnum + 1), ])
    knum <- 1
    sig <- as.matrix(starts3[((gridnum * alts) + intnum + 2):length(starts3), ])
  }

  # Force k to be a positive value
  k_exp <- exp(k)

  # sigmac <- as.matrix(starts3[((gridnum * alts) + intnum + 1 + signum), ])
  # end of coefficient vector
    
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

  # Expected mean catch function
  gridmu <- gridbetas_exp * matrix(gamma((k_exp + 1) / k_exp), obsnum, alts)

  # Cost portion of the likelihood
  intbetas <- .rowSums(intdat * matrix(intcoef, obsnum, intnum, byrow = TRUE), obsnum, intnum)

  # Revenue portion of the likelihood (i.e., price * expected mean catch) followed by cost portion, saved in the same matrix
  rev_cost_betas <- matrix(c((gridmu * matrix(pricedat, obsnum, alts)), intbetas), obsnum, (alts + 1))

  # (Revenue * dummy variable indicating zones fished) and (cost * distance) in a single, large matrix
  # Note: distances in dat are shifted/sorted, same as choice possibilities, even though the zone columns retain the original order of zones
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

  # Format choice occasion-specific parameters that interact with beta parameters
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
