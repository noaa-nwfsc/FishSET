#' Logit predict
#' 
#' Prediction component from logit models (non mixed) called in Policy3, under predict_model_tempNew.m
#'
#' @param project Name of project
#' @param mod.name Name of saved model to use
#' @param expected.catch.name Required for conditonal logit (\code{logit_c}) model. 
#'   Name of expected catch table to use. 
#'    Can be the expected catch from the short-term scenario (\code{short}), the medium-term scenario (\code{med}), the 
#'    long-term scenario (\code{long}), or the user-defined temporal parameters (\code{user}).
#' @return Returns probability of logit model by choice
#' @export
#' @keywords internal


logit_predict <- function(project, mod.name, expected.catch.name=NULL){

#Obtain parameter estimates
  logitEq <- read_dat(paste0(locoutput(project), pull_output(project, type='table', fun='params')))
  zoneID <- logitEq$X
  
#Get model data
  # Need data compile, distance, gridvarying, interaction terms
  mod.dat <- create_model_input(project, x=NULL, mod.name=mod.name, use.scalers= TRUE, scaler.func=NULL, expected.catch.name=expected.catch.name)
  alts <- dim(unique(mod.dat$choice))[1]
  mod.dat <- as.matrix(cbind(mod.dat$dataCompile, mod.dat$distance, mod.dat$otherdat))

logitEq <- logitEq[,2]
p <- length(logitEq)
betaLogit <- logitEq[1:alts]
##What do we do if we have not interaction terms?
Beta <- logitEq[-c(1:alts)] 
if(is_empty(Beta)){
  Beta <- 1
}
A <- alts*alts+1
bTerms <-  mod.dat[,-c(1:A-1)] #choice matrix
numerLogit <- matrix(NA, nrow(mod.dat), alts) #array(NA, c(nrow(mod.dat), nrow(mod.dat), alts)) # nan(nrow(x), alts)




for(i in 1:nrow(bTerms)){ # for each individual
  if(p-alts == 0){
    bTermsMatrix <- 0
  } else {
    bTermsMatrix <- array(bTerms[i,],c(alts, p-alts)) #reshape(bTerms[i,],alts,p-alts)
  }
  numerLogit[i,] = t(exp(betaLogit + bTermsMatrix*Beta))  #exp(betaLogit+miles*BmilesLogit+ milesSQ*bmilesSQ+Betaf*bf+....ect....
}
                                                            
denomLogit <- rowSums(numerLogit) # sum(numerLogit,2)  # sum by row
probLogitzone <-  numerLogit/(matrix(1,1,alts) %x% denomLogit)  #numerLogit./repmat(denomLogit,1,alts)
#BML(count,model)=BmilesLogit
probLogit <- colMeans(probLogitzone)
probLogit <- cbind(zoneID, probLogit)

out <- list(probLogit=probLogit, modelDat=mod.dat)
return(out)                                                           
}
