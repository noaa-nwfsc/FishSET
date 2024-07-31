#' EPM predict
#' 
#' Prediction component for expected profit models (EPMs). This function is called in 
#' \code{\link{model_prediction}}, which is called within \code{\link{run_policy}}.
#'
#' @param project Name of project
#' @param mod.name Name of saved model to use
#' @param mod.type String. Options are weibull, lognormal, normal
#' @param use.scalers Input for \code{\link{create_model_input}}. Logical, should data be normalized? Defaults to \code{FALSE}. Rescaling factors are the mean of the 
#' numeric vector unless specified with \code{scaler.func}.
#' @param scaler.func Input for \code{\link{create_model_input}}. Function to calculate rescaling factors.
#' @param outsample Logical, \code{FALSE} if predicting probabilities for primary data, and \code{TRUE} if predicting for out-of-sample data. \code{outsample = FALSE} 
#'   is the default setting.
#' @return Returns probability of epm model by choice
#' @export
#' @keywords internal


epm_predict <- function(project, mod.name, mod.type, use.scalers = FALSE, scaler.func = NULL, outsample = FALSE, outsample.mod.name = NULL){
  
  # Obtain parameter estimates ----
  if(!outsample){ # IN-SAMPLE
    # Get parameter estimates
    tmpEq <- get_latest_projectfile(project, mod.name)
    epmEq <- tmpEq[[1]]
    
    # Display file used in the gui
    if(isRunning()){
      showNotification(paste0("Pulling from model output file '", tmpEq[[2]]), "'", type = 'message', duration = 10)
    }
  
  } else { # OUT-OF-SAMPLE
    # Get parameter estimates
    tmpEq <- get_latest_projectfile(project, mod.name)
    epmEq <- tmpEq[[1]]
    
    # Display file used in the gui
    if(isRunning()){
      showNotification(paste0("Pulling from model output file '", tmpEq[[2]]), "'", type = 'message', duration = 10)
    }
    
    # Need to save original model in case the number of alternatives are different
    mdf <- model_design_list(project)
    mdf_n <- model_names(project)
    mdf_om <- mdf[[which(mdf_n == mod.name)]]  # save original model
    in_zones <- sort(unique(mdf_om$choice$choice)) # get zones in the original
    
    # Overwrite mod.name as the out-of-sample model design name
    mod.name <- outsample.mod.name
  } 
  
  
  # Get model data ----
  # Need data compile, distance, gridvarying, interaction terms
  mdf <- model_design_list(project)
  mdf_n <- model_names(project)
  mdf_new <- mdf[[which(mdf_n == mod.name)]]
  exp.names <- unlist(mdf_new$expectcatchmodels)
  
  mod.dat <- create_model_input(project, x = mdf_new, mod.name = mdf_new$mod.name, 
                                use.scalers = use.scalers, scaler.func = scaler.func, expected.catch = exp.names)
  
  distance <- mdf_new$distance
  griddat <- as.matrix(do.call(cbind, mod.dat$otherdat$griddat))
  intdat <- as.matrix(do.call(cbind, mod.dat$otherdat$intdat))
  price <- matrix(mod.dat$otherdat$pricedat)
  zoneID <- sort(unique(mod.dat$choice.table$choice))
  
  # Get the number of variables
  alts <- dim(unique(mod.dat$choice))[1] # alternatives
  obsnum <- dim(griddat)[1] # number of observations
  
  gridnum <- dim(griddat)[2] # number of gridvarying variables  
  intnum <- dim(intdat)[2] # number of gridvarying variables  
  
  # Get coefficients 
  if(!outsample){
    epmEq <- epmEq$estimate
    
  } else if(outsample){
    # Format out-of-sample coefficients if alts not equal to in-sample alts  
    if(mdf_om$alts != mdf_new$alts){
      tmp <- format_outsample_coefs(in_zones = in_zones, out_zones = zoneID, Eq = epmEq, likelihood = mdf_new$likelihood)
      epmEq <- tmp[[1]]
      z_flag <- tmp[[2]]
      if(z_flag == 1) gridnum <- gridnum + 1 # need to add one because the first alt was remove from gridbum above
    } else {
      # Else when alts are equal just get the original coefficients
      epmEq <- epmEq$estimate
    }
  }
  
  gridcoef <- epmEq[1:(alts * gridnum)] # catch-function coefficients
  intcoef <- epmEq[((alts * gridnum) + 1):((alts * gridnum) + intnum)] #travel-distance coefficients
  
  ##
  # Predict fishing probabilities ----
  ##
  
  ## EPM Weibull ----
  if(mod.type == "epm_weibull"){
    # get remaining epm weibull specific parameters
    if ((length(epmEq) - ((gridnum * alts) + intnum + 1)) == alts) {
      k <- as.matrix(epmEq[((gridnum * alts) + intnum + 1):((gridnum * alts) + intnum + alts)])
      knum <- alts
      sig <- as.matrix(epmEq[((gridnum * alts) + intnum + alts + 1):length(epmEq)])
    } else {
      k <- as.matrix(epmEq[((gridnum * alts) + intnum + 1)])
      knum <- 1
      sig <- as.matrix(epmEq[((gridnum * alts) + intnum + 2):length(epmEq)])
    }
    
    # k is constrained to positive values using exponential function in epm_weibull.R
    k_exp <- exp(k)
    
    # Choice component of epm weibull
    # beta_jm * G_im
    gridbetas <- (matrix(gridcoef, obsnum, alts * gridnum, byrow = TRUE) * matrix(griddat, obsnum, alts * gridnum, byrow = TRUE))
    dim(gridbetas) <- c(nrow(gridbetas), alts, gridnum)
    
    # SUM(beta_jm * G_im), which is the scale portion of the Weibull function
    gridbetas <- rowSums(gridbetas, dims = 2)
    
    # Scale portion of the weibull function is constrained to positive values with exponential function in epm_weibull.R
    gridbetas_exp <- exp(gridbetas)
    
    # Expected mean catch function
    gridmu <- gridbetas_exp * matrix(gamma((k_exp + 1) / k_exp), obsnum, alts)
    
    # Revenue
    revbetas <- gridmu * matrix(price, obsnum, alts)
    
    # Cost portion of the likelihood
    intbetas <- .rowSums(intdat * matrix(intcoef, obsnum, intnum, byrow = TRUE), obsnum, intnum)
    costbetas <- matrix(matrix(intbetas, obsnum, alts) * distance, obsnum, alts)
    
    # Numer of choice component
    numer <- exp((revbetas + costbetas) / matrix(sig, obsnum, alts))
    
    # Denom
    denom <- matrix(rep(as.matrix(rowSums(numer)), alts), obsnum, alts)
    
    # Fishing probabilities 
    pLogit <- numer/denom # probs for each zone, for each observation
    probLogit <- colMeans(pLogit) # mean probs for each zone
    probLogit <- data.frame(zoneID = zoneID, prob = probLogit)
    
    return(list(probLogit, mod.dat, pLogit))
    
  
  ## EPM log-normal ----
  } else if (mod.type == "epm_lognormal"){
    # get remaining epm lognormal specific parameters
    if ((length(epmEq) - ((gridnum * alts) + intnum + 1)) == alts) {
      stdev <- as.matrix(epmEq[((gridnum * alts) + intnum + 1):((gridnum * alts) + intnum + alts)])
      stdevnum <- alts
      sig <- as.matrix(epmEq[((gridnum * alts) + intnum + alts + 1):length(epmEq)])
    } else {
      stdev <- as.matrix(epmEq[((gridnum * alts) + intnum + 1)])
      stdevnum <- 1
      sig <- as.matrix(epmEq[((gridnum * alts) + intnum + 2):length(epmEq)])
    }
    
    # Force stdev to be a positive value
    stdev_exp <- exp(stdev)
    
    # Choice component of epm lognormal
    # beta_jm * G_im
    gridbetas <- (matrix(gridcoef, obsnum, alts * gridnum, byrow = TRUE) * matrix(griddat, obsnum, alts * gridnum, byrow = TRUE))
    dim(gridbetas) <- c(nrow(gridbetas), alts, gridnum)
    
    # SUM(beta_jm * G_im), which represents the mu in the lognormal function
    gridbetas <- rowSums(gridbetas, dims = 2)
    
    # Expected mean catch function (mean of lognormal distribution)
    gridmu <- exp(gridbetas + (0.5 * (matrix(stdev_exp, obsnum, alts, byrow = TRUE)^2)))
    
    # Revenue
    revbetas <- gridmu * matrix(price, obsnum, alts)
    
    # Cost portion of the likelihood
    intbetas <- .rowSums(intdat * matrix(intcoef, obsnum, intnum, byrow = TRUE), obsnum, intnum)
    costbetas <- matrix(matrix(intbetas, obsnum, alts) * distance, obsnum, alts)
    
    # Numer of choice component
    numer <- exp((revbetas + costbetas) / matrix(sig, obsnum, alts))
    
    # Denom
    denom <- matrix(rep(as.matrix(rowSums(numer)), alts), obsnum, alts)
    
    # Fishing probabilities 
    pLogit <- numer/denom # probs for each zone, for each observation
    probLogit <- colMeans(pLogit) # mean probs for each zone
    probLogit <- data.frame(zoneID = zoneID, prob = probLogit)
    
    return(list(probLogit, mod.dat, pLogit))
  
  ## EPM normal ----
  } else if (mod.type == "epm_normal"){
    # get remaining epm normal specific parameters
    if ((length(epmEq) - ((gridnum * alts) + intnum + 1)) == alts) {
      stdev <- as.matrix(epmEq[((gridnum * alts) + intnum + 1):((gridnum * alts) + intnum + alts)])
      stdevnum <- alts
      sig <- as.matrix(epmEq[((gridnum * alts) + intnum + alts + 1):length(epmEq)])
    } else {
      stdev <- as.matrix(epmEq[((gridnum * alts) + intnum + 1)])
      stdevnum <- 1
      sig <- as.matrix(epmEq[((gridnum * alts) + intnum + 2):length(epmEq)])
    }
    
    # Force stdev to be a positive value
    stdev_exp <- exp(stdev)
    
    # Choice component of epm normal
    # beta_jm * G_im
    gridbetas <- (matrix(gridcoef, obsnum, alts * gridnum, byrow = TRUE) * matrix(griddat, obsnum, alts * gridnum, byrow = TRUE))
    dim(gridbetas) <- c(nrow(gridbetas), alts, gridnum)
    
    # SUM(beta_jm * G_im), which represents mu in the normal function, and expected mean catch
    gridbetas <- rowSums(gridbetas, dims = 2)
    gridmu <- gridbetas 
    
    # Revenue
    revbetas <- gridmu * matrix(price, obsnum, alts)
    
    # Cost portion of the likelihood
    intbetas <- .rowSums(intdat * matrix(intcoef, obsnum, intnum, byrow = TRUE), obsnum, intnum)
    costbetas <- matrix(matrix(intbetas, obsnum, alts) * distance, obsnum, alts)
    
    # Numer of choice component
    numer <- exp((revbetas + costbetas) / matrix(sig, obsnum, alts))
    
    # Denom
    denom <- matrix(rep(as.matrix(rowSums(numer)), alts), obsnum, alts)
    
    # Fishing probabilities 
    pLogit <- numer/denom # probs for each zone, for each observation
    probLogit <- colMeans(pLogit) # mean probs for each zone
    probLogit <- data.frame(zoneID = zoneID, prob = probLogit)
    
    return(list(probLogit, mod.dat, pLogit)) 
  }
  
  
  
  # bchar <- length(mod.dat$otherdat) - 1 #size(modelInputData.bCHeader,2)-1; bchar is the number of gridvarying variables and interaction terms
  # Beta <- epmEq[(alts+1):(alts+1+bchar)] 
  
  # # Might need to include more parameters here
  # mod.dat <- as.matrix(cbind((mod.dat$dataCompile*price), mod.dat$distance, mod.dat$otherdat))
  
  # if(is_empty(Beta)){
  #   Beta <- 1
  # }
  
  # A <- alts*alts+1
  # bTerms <-  mod.dat[,-c(1:A-1)] #choice matrix
  # numerEPM <- matrix(NA, nrow(mod.dat), alts) #array(NA, c(nrow(mod.dat), nrow(mod.dat), alts)) # nan(nrow(x), alts)
  
  
  ###check this - sigmachoice should be sigma parameter estimate
  # sigmachoice <- epmEq[(alts+bchar+2)]
  
  #   #Calculate these parameters for weibull
  #   if(grepl('weibull', mod.type, ignore.case=TRUE)){
  #     k <- epmEq[-c(1:(length(epmEq)-(alts-1)-1))]
  #     k_exp <- exp(k)
  #     exp_alphaEPM <- exp(alphaEPM)
  #   
  #     gammakexp <- gamma((k_exp+1)/k_exp)
  #   }
  #   
  # if (grepl('weibull', mod.type, ignore.case=TRUE)){
  #   
  #   if(length(p)==1){
  #     for (i in 1:nrow(mod.dat)){
  #       bTermsMatrix <- array(bTerms[i,],c(alts, bchar+1))
  #       numerEPM[i,] = t(exp((p*exp_alphaEPM*gammakexp + bTermsMatrix*Beta)/sigmachoice))
  #     }
  #   } else{
  #     
  #     for (i in 1:nrow(mod.dat)){
  #       bTermsMatrix=array(bTerms[i,],c(alts, bchar+1))
  #       numerEPM[i,] <- t(exp((p(i)*exp_alphaEPM*gammakexp + bTermsMatrix*Beta)/sigmachoice))
  #     }
  #   }
  # } else {
  #   if(length(p)==1){
  # 
  #     for (i in 1:nrow(mod.dat)){ # for each individual
#       bTermsMatrix <- array(bTerms[i,],c(alts, bchar+1)) #reshape(bTerms(i,:),alts,bchar+1);
#       numerEPM[i,] = t(exp((alphaEPM*p + bTermsMatrix*Beta)/sigmachoice))  #exp(betaLogit+miles*BmilesLogit+ milesSQ*bmilesSQ+Betaf*bf+....ect....
#     
#     }
#   } else {
#                                                                            
#     for (i in 1:nrow(mod.dat)){ # for each individual
#       bTermsMatrix <- array(bTerms[i,],c(alts, bchar+1)) #reshape(bTerms(i,:),alts,bchar+1)
#       numerEPM[i,] <- t(exp((alphaEPM*p(i) + bTermsMatrix*Beta)/sigmachoice))  #exp(betaLogit+miles*BmilesLogit+ milesSQ*bmilesSQ+Betaf*bf+....ect....
#     }
#   }
# }
#   
# denomEPM = rowSums(numerEPM)  # sum by row%FIXME.. how to deal with nans
# probEPMzone= numerEPM/(matrix(1,1,alts) %x% denomEPM) #numerEPM./repmat(denomEPM,1,alts)
# #BML(count,model)=BmilesLogit;
# probEPM <- colMeans(probEPMzone, na.rm = TRUE)
# 
# out <- list(probEPM=probEPM, modelDat=mod.dat)
# return(out)    
                                                                                                                                                     
}                                                                                                                                                     
                                                                                                                                                     