
#' EPM predict
#' 
#' Prediction component from epm models called in Policy3, under predict_model_tempNew.m
#'
#' @param project Name of project
#' @param alts  Number of alternatives choices in model
#' @param modname Name of saved model to use
#' @param mod.type String. Options are weibull, lognormal, normal
#' @param price Numeric. Price.
#' @return Returns probability of epm model by choice
#' @export
#' @keywords internal


epm_predict <- function(project, alts, modname, mod.type, price){
  
  #Obtain parameter estimates
  epmEq <- read_dat(paste0(locoutput(project), pull_output(project, type='table', fun='params')))
  
  #Get model data
  # Need data compile, distance, gridvarying, interaction terms
  mod.dat <- create_model_input(project, x=NULL, mod.name=modname, use.scalers= TRUE, scaler.func=NULL, expected.catch.name=NULL)
  alts <- dim(unique(mod.dat$choice))[1]
  #Might need to include more parameters here
  mod.dat <- as.matrix(cbind((mod.dat$dataCompile*price), mod.dat$distance, mod.dat$otherdat))
  
  epmEq <- epmEq[,2]
#  z <- length(epmEq)
  alphaEPM <- epmEq[1:alts]
  bchar <- length(mod.dat$otherdat) -1 #size(modelInputData.bCHeader,2)-1; bchar is the number of gridvarying variables and interaction terms
  
  Beta <- epmEq[(alts+1):(alts+1+bchar)] 
 
  
  if(is_empty(Beta)){
    Beta <- 1
  }
  
  A <- alts*alts+1
  bTerms <-  mod.dat[,-c(1:A-1)] #choice matrix
  numerEPM <- matrix(NA, nrow(mod.dat), alts) #array(NA, c(nrow(mod.dat), nrow(mod.dat), alts)) # nan(nrow(x), alts)
  

  ###check this - sigmachoice should be sigma parameter estimate
  sigmachoice <- epmEq[(alts+bchar+2)]
  
  #Calculate these parameters for weibull
  if(grepl('weibull', mod.type, ignore.case=TRUE)){
    k <- epmEq[-c(1:(length(epmEq)-(alts-1)-1))]
    k_exp <- exp(k)
    exp_alphaEPM <- exp(alphaEPM)
  
    gammakexp <- gamma((k_exp+1)/k_exp)
  }
  
if (grepl('weibull', mod.type, ignore.case=TRUE)){
  
  if(length(p)==1){
    for (i in 1:nrow(mod.dat)){
      bTermsMatrix <- array(bTerms[i,],c(alts, bchar+1))
      numerEPM[i,] = t(exp((p*exp_alphaEPM*gammakexp + bTermsMatrix*Beta)/sigmachoice))
    }
  } else{
    
    for (i in 1:nrow(mod.dat)){
      bTermsMatrix=array(bTerms[i,],c(alts, bchar+1))
      numerEPM[i,] <- t(exp((p(i)*exp_alphaEPM*gammakexp + bTermsMatrix*Beta)/sigmachoice))
    }
  }
} else {
  if(length(p)==1){

    for (i in 1:nrow(mod.dat)){ # for each individual
      bTermsMatrix <- array(bTerms[i,],c(alts, bchar+1)) #reshape(bTerms(i,:),alts,bchar+1);
      numerEPM[i,] = t(exp((alphaEPM*p + bTermsMatrix*Beta)/sigmachoice))  #exp(betaLogit+miles*BmilesLogit+ milesSQ*bmilesSQ+Betaf*bf+....ect....
    
    }
  } else {
                                                                           
    for (i in 1:nrow(mod.dat)){ # for each individual
      bTermsMatrix <- array(bTerms[i,],c(alts, bchar+1)) #reshape(bTerms(i,:),alts,bchar+1)
      numerEPM[i,] <- t(exp((alphaEPM*p(i) + bTermsMatrix*Beta)/sigmachoice))  #exp(betaLogit+miles*BmilesLogit+ milesSQ*bmilesSQ+Betaf*bf+....ect....
    }
  }
}
  
denomEPM = rowSums(numerEPM)  # sum by row%FIXME.. how to deal with nans
probEPMzone= numerEPM/(matrix(1,1,alts) %x% denomEPM) #numerEPM./repmat(denomEPM,1,alts)
#BML(count,model)=BmilesLogit;
probEPM <- colMeans(probEPMzone, na.rm = TRUE)

out <- list(probEPM=probEPM, modelDat=mod.dat)
return(out)    
                                                                                                                                                     
}                                                                                                                                                     
                                                                                                                                                     