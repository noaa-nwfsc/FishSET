#  Mixed logit
# Prediction compoenet form logit models ( mixed) called in Policy3, under predict_model_tempNew.m
  #	probMixedLogit: probablity of mixed logit model by choice

#' Mixed Logit predict
#' 
#' Prediction component from mixed logit models alled in Policy3, under predict_model_tempNew.m
#'
#' @param project Name of project
#' @param logitEq Parameter estimates
#' @param alts  Number of alternatives choices in model
#' @param mod.name Name of saved model to use
#' @param expected.catch Required for conditional logit (\code{logit_c}) model. 
#'   Name of expected catch table to use. 
#'    Can be the expected catch from the short-term scenario (\code{short}), the medium-term scenario (\code{med}), the 
#'    long-term scenario (\code{long}), or the user-defined temporal parameters (\code{user}).
#' @return Returns probability of mixed logit model by choice
#' @export
#' @keywords internal

mixed_logit_predict <- function(project, logitEq, alts,  mod.name, expected.catch=NULL){
  
  #Obtain parameter estimates
  logitEq <- read_dat(paste0(locoutput(project), pull_output(project, type='table', fun='params')))
  
  #Get model data
  # Need data compile, distance, gridvarying, interaction terms
  mod.dat <- create_model_input(project, x=NULL, mod.name=mod.name, use.scalers= TRUE, scaler.func=NULL, expected.catch=expected.catch)
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
  ###TO DEAL WITH ###
#!!! bTerms(:,1:22)=-bTerms(:,1:22);  # need to make miles=mile*-1
  numerLogit <- matrix(NA, nrow(mod.dat), alts) #array(NA, c(nrow(mod.dat), nrow(mod.dat), alts)) # nan(nrow(x), alts)
  
bchar <- p-alts-2
c <- logitEq[alts+bchar+1]
s = logitEq[alts+bchar+2]

nreps <- 100
probLogitRand <- matrix(NA, nreps, alts)

for (count in 1:nreps){  #mixed logit loops
  BetaR <- rbind(exp(c+s*rnorm(1)), Beta)  #[exp(c+s*rnorm(1)); Beta]
  for (i in 1:nrow(bTerms)) {# for each individual
     bTermsMatrix <- array(bTerms[i,],c(alts, bchar+1)) 
     numerLogit[i,] = t(exp(betaLogit + bTermsMatrix*BetaR))  #exp(betaLogit+miles*BmilesLogit+ milesSQ*bmilesSQ+Betaf*bf+....ect....
  }
                                                             
 denomLogit <- rowSums(numerLogit) # sum(numerLogit,2)  # sum by row
 probLogitzone <-  numerLogit/(matrix(1,1,alts) %x% denomLogit)  #numerLogit./repmat(denomLogit,1,alts)
 
#BML(count,model)=BmilesLogit;
 probLogitRand[count,] <- mean(probLogitzone)
                                                             
}

 probMixedLogit <- colMeans(probLogitRand)
                                                             
} 
 
