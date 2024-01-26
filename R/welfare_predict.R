#' Welfare analysis code
#' 
#' @param project Name of project
#' @param mod.name Name of selected model (mchoice)
#' @param closures Closure scenarios
#' @param expected.catch Name of expectedchatch table to use
#' @param enteredPrice Price for welfare
#' @importFrom DBI dbConnect dbDisconnect dbGetQuery
#' @importFrom RSQLite SQLite
#' @export
#' @details Called by \code{\link{run_policy}} function. The 
#'   \code{\link{model_prediction}} function should be run before this function.

welfare_predict <- function(project, mod.name, closures, expected.catch = NULL, enteredPrice = NULL){
  
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
  
  
  # Create connection to fishset database ----
  fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project=project))
  on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
  
  
  # Pull output data from model optimization ----
  # Get list with all model outputs
  mod.out <- model_out_view(project)
  
  # Check if mod.name from input exists in the model output list
  # If the model does not exist, stop function and return error message
  flag <- 0
  tryCatch(
    {mod.out <- mod.out[[which(lapply(mod.out , "[[" , "name") == mod.name)]]},
    error = function(err){flag <<- 1}
  )
  if(flag == 1){
    stop(paste0('Model output for "', mod.name,'" does not exist.'), call. = FALSE)
  }
  
  # Get parameter estimates
  Eq <- mod.out$OutLogit[,1]
  
  # Positive definite check for inverse Hessian ----
  # If the inverse Hessian is positive definite then the function is a minimum
  # Get inverse hessian for selected model
  invHess <- mod.out$H1 
  
  # Try Cholesky factorization, if successful the matrix should be positive definite
  tryCatch(
    {test.pd <- chol(invHess)},
    error = function(err){flag <<- 1}
  )
  if(flag == 1){
    stop(paste0('Inverse Hessian matrix not positive definite and Cholesky factorization failed'), call. = FALSE)
  }
  
  
  # Pull data from predicted output table generated in model_prediction() ----
  # Get predicted output table
  predict_temp <- unserialize_table(paste0(project, "predictOutput"), project)
  predict_temp <- predict_temp[which(unlist(lapply(predict_temp, function(x) grepl(mod.name, x$scenario.name))))]
  
  
  # Settings for welfare predictions ----
  # Number of closure scenarios
  n_scenarios <- length(closures)
  
  # Set the number of draws from the multivariate normal random number generator (mvgrnd)
  betadraws <- 1000
  
  # Draw simulation parameter values from multivariate random number generator
  mu_rand <- t(mvgrnd(Eq, invHess, betadraws))  # flipped so that rows are output and columns are draws
  
  # Get model data ----
  # Need data compile, distance, gridvarying, interaction terms. Only need first one because modDat is the same for all predict_temp items
  x <- predict_temp[[1]]$modelDat
  griddat <- as.matrix(do.call(cbind, x$otherdat$griddat))
  intdat <- as.matrix(do.call(cbind, x$otherdat$intdat))
  distance <- x$distance
  alts <- length(predict_temp[[1]]$zoneID)
  obsnum <- dim(griddat)[1]
  zoneIDs <- predict_temp[[1]]$zoneID
  
  ## Save coefficients ----
  # Get number of variables
  gridnum <- griddat[2]
  intnum <- intdat[2]
  z <- length(Eq)
  
  # Loop through welfare scenarios ----
  # Setup price loop
  i_price <- length(enteredPrice)
  
  # If entered price is NULL we still want a single run of the price for loop
  # Also if the length is the number of observations we want that to be the price dat and not loop through all prices
  if(i_price == 0 || i_price == obsnum) i_price <- 1 
  
  # Create empty matrices
  welfare <- matrix(rep(NA, 5*n_scenarios*i_price)) # 5 represents quantiles
  dim(welfare) <- c(5, n_scenarios, i_price)
  prc_welfare <- matrix(rep(NA, n_scenarios*i_price), nrow = n_scenarios, ncol = i_price)
  
  
  for(i in 1:i_price){
    # If entered price is null, then use price from data
    if(is.null(enteredPrice)){
      pricedat <- matrix(x$otherdat$pricedat)
      
      # Else if entered price is the length of obsnum then set to pricedat
    } else if (length(enteredPrice) == obsnum){
      pricedat <- matrix(enteredPrice, nrow = obsnum)
      
      # Loop through entered prices
    } else {
      pricedat <- matrix(enteredPrice[i], nrow = obsnum)
      
    }
    
    # TODO Why was price scaled in the matlab version of this function?
    # pscale <- mean(pricedat, na.rm=T) #scaler
    # p <- pricedat/pscale
    
    # Loop through closure scenarios
    for(j in 1:n_scenarios){
      tmp_close <- predict_temp[[j]]$zoneIdIn
      closeID <- which(zoneIDs %in% tmp_close)
      
      # Create empty betadraw matrices
      welfare_betadraws <- prc_welfare_betadraws <- NULL
      
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
            costbetas <- as.matrix(matrix(intbetas, obsnum, alts) * distance)
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
            costbetas <- as.matrix(matrix(intbetas, obsnum, alts) * distance)
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
            costbetas <- as.matrix(matrix(intbetas, obsnum, alts) * distance)
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
          
          # LOGIT WELFARE ----
        } else if (grepl('logit', predict_temp[[1]]$type, ignore.case=TRUE)) {
          
          
        }
        
        # Save output for each betadraw of parameters
        welfare_betadraws <- c(welfare_betadraws, tmp_welfare)
        prc_welfare_betadraws <- c(prc_welfare_betadraws, mean(tmp_prc_welfare))
        
      } # close betadraws
      
      # Summarize data ----
      welfare[, j, i] <- quantile(welfare_betadraws, probs = c(0.025, 0.05, 0.5, 0.95, 0.975), na.rm = TRUE)
      prc_welfare[j,i] <- mean(prc_welfare_betadraws, na.rm = TRUE)
      
    } # close n_scenario
  } # close price loop
  
  
  # log welfare_predict function call ----
  # Note that this is not logged when called within the run_policy() function
  welfare_predict_function <- list()
  welfare_predict_function$functionID <- "welfare_predict"
  welfare_predict_function$args <- list(project, mod.name, closures, expected.catch, enteredPrice)
  log_call(project, welfare_predict_function)
  
  return(list(welfare, prc_welfare))
}
  
  
#### OLD CODE FROM MATLAB VERSION ####
#'   # TODO: Figure out how welfare prediction works for logit models
#'   if(predict_temp[[1]]$type == 'Logit'){
#'     for(i in length(scenario)){
#'       zoneID <- predict_temp[[i]]$zoneID
#'       scenario[[i]]$open <- matrix(1, 1, alts) #ones(1,alts)
#'       colnames(scenario[[i]]$open) <- zoneID
#'       if(!is_empty(zi)){
#'         scenario[[i]]$open[,which(colnames(scenario[[i]]$open)==zoneIdIn)] <- 0 #Replace columns with 0 for zoneIDIn
#'       }
#'       scenario[[i]]$quality <- matrix(1,1, alts) 
#'       colnames(scenario[[i]]$quality) <- zoneID
#'       #ones(alts,1) #FIXME to generalize   #TODO these become percent changes to variables or  on off to variables
#'       #?  scenario.a.quality(:,end+1)=ones(alts,1) #FIXME to generalize
#'     }
#'     
#'     numTrips=nrow(x)
#'     # number of sims for generating std errs (note, each sim can be run
#'     # independently so this is easy to parralelize using parfor):
#'     
#'     # draw sims beta vectors consistent with estimated model above:
#'     # This method follows Krinsky Robb.  It would also be possible to
#'     # bootstrap the std err's of the welfare measure but would be
#'     # computationally more expensive.
#'     
#'     
#'     #check for positive definate
#'     #FIXME see main_welfare_like_rob_cjb.m
#'     #[r,p]=chol(invHess)
#'     #if (p==0){
#'     mu_rand <- t(mvgrnd(ModelOutput,invHess,betadraws))  # flipped so that rows are output and columns are draws
#'     welfare = sim_welfare(project=project, X=x,alts=alts,beta_j=mu_rand, scenario)
#'     
#'     #  warning('Welfare prediction cannot be performed as the inverse hessian is not positive definite')
#'     
#'     # prW <- NA
#'     #  welfare <- NA
#'     #}
#'     
#'     prW <- NA
#'     
#'   } else if(grepl('epm', predict_temp[[1]]$type, ignore.case=TRUE)){
#'     # numTrips <- nrow(x$choice)
#'     obsnum <- dim(x$otherdat$griddat)[1] # number of observations
#'     
#'     priceData = predict_temp[[1]]$modelDat$otherdat$pricedat # model.priceCol{mChoice}
#'     pscale = mean(priceData, na.rm=T) #scaler
#'     
#'     # TODO: Figure out why this code is in here
#'     if(is.na(enteredPrice) || is.null(enteredPrice)) {
#'       p <- priceData/pscale
#'       allPrice <- c(p,p) #{p,p} # use internal price
#'     } else {
#'       newPrice <- enteredPrice/pscale
#'       p <- priceData/pscale
#'       allPrice <- c(p, newPrice) #{p,newPrice}   # size = size(nameAllTime) first cell has base in it
#'     }
#'     
#'     A <- alts * alts + 1 # TODO what is the purpose of this??
#'     bTerms <- x[,-c(1:(A-1))] # TODO what is the purpose of this??
#'     
#'     WelfareScale <- 100000 # TODO what is the purpose of this??
#'     
#'     # Check that invhess is positive definite
#'     # So mu_rand is a parametric bootstrap?
#'     mu_rand <- t(mvgrnd(epmEq$estimate,invHess,betadraws)) # flipped so that rows are coefficients and columns are draws
#'     
#'     z <- length(epmEq$estimate) # TODO what is the purpose of this??
#'     
#'     for (quant in 1:length(allPrice)){ # not right for base case ... should skip base case? need to add something to take care of partial closures?
#'       if(quant==1){
#'         prW[quant] <- NA
#'         welfare[quant] <- NA
#'       } else { # skip base case for speed right now t comply with only using later case in plotting...
#'         p <- allPrice[quant]
#'         welfareDiff <- matrix(NA, obsnum, betadraws) #nan(numTrips,betadraws)
#'         numerEPM= matrix(NA, obsnum, alts) #nan(numTrips,alts)
#'         prctWelfare <- matrix(NA, obsnum, betadraws)
#'       }
#'       
#'       for(V in 1:betadraws){
#'         #switch modelNameChoice
#'         #case {'EPM gamma'}
#'         #bcData= model.bcDataCol{mChoice}  #this is the column of bc data used with gammas in the modeling
#'         #alphaEPM=mu_rand(1:alts,V) #NOTE: this comes from mu_rand now instead of modelOutput
#'         #gammasEPM=mu_rand(alts+1:2*alts,V)
#'         
#'         #bchar=size(modelInputData.bCHeader,2)-1 
#'         #Beta=mu_rand(2*alts+1:2*alts+1+bchar,V) #NOTE: this comes from mu_rand now instead of modelOutput
#'         #sigmachoice= mu_rand(2*alts+bchar+2,V) #NOTE: this comes from mu_rand now instead of modelOutput
#'         #adjustS=sigmachoice*0.57721
#'         
#'         #otherwise
#'         
#'         # Get number of variables
#'         gridnum <- dim(x$otherdat$griddat)[2]
#'         intnum <- dim(x$otherdat$intdat)[2]
#'         
#'         # Get coefficients
#'         gridcoef <- mu_rand[1:(alts * gridnum), V] # NOTE: this comes from mu_rand now instead of modelOutput
#'         intcoef <- mu_rand[((alts * gridnum) + 1):((alts * gridnum) + intnum), V]
#'         # alphaEPM <- mu_rand[1:alts,V] #NOTE: this comes from mu_rand now instead of modelOutput
#'         # Beta <- mu_rand[alts+1:alts+1+bchar,V] #NOTE: this comes from mu_rand now instead of modelOutput
#'         # sigmachoice <- mu_rand[alts+bchar+2,V] #NOTE: this comes from mu_rand now instead of modelOutput
#'         # adjustS <- sigmachoice*0.57721
#'         
#'         if(grepl('normal', predict_temp[[1]]$type, ignore.case=TRUE)){
#'           # added for log-normal
#'           sigmaa <- mu_rand[-c(1:(alts+bchar+2)),V]  #NOTE: this comes from mu_rand now instead of modelOutput
#'           half_sigmaa_sq <- 0.5*(sigmaa^2)
#'           alphaEPM=exp(alphaEPM+half_sigmaa_sq)  # this is substituted below
#'           
#'         } else {
#'           #'EPM weibull'
#'           if ((z - ((gridnum * alts) + intnum + 1)) == alts) {
#'             k <- mu_rand[((gridnum * alts) + intnum + 1):((gridnum * alts) + intnum + alts), V]
#'             knum <- alts
#'             sig <- mu_rand[((gridnum * alts) + intnum + alts + 1):z, V]
#'           } else {
#'             k <- mu_rand[((gridnum * alts) + intnum + 1), V]
#'             knum <- 1
#'             sig <- mu_rand[((gridnum * alts) + intnum + 2):z, V]
#'           }
#'           # k=mu_rand[-c(1:(z-(alts-1)-1)),V] #FIXME check this is correct
#'           
#'           k_exp <- exp(k)
#'           gamma_kexp <- gamma((k_exp + 1)/k_exp)
#'           
#'           # (beta_jm * G_im)
#'           gridbetas <- (matrix(gridcoef, obsnum, alts * gridnum, byrow = TRUE) * matrix(griddat, obsnum, alts * gridnum, byrow = TRUE))
#'           dim(gridbetas) <- c(nrow(gridbetas), alts, gridnum)
#'           # SUM(beta_jm * G_im), which is the scale portion of the Weibull function
#'           gridbetas <- rowSums(gridbetas, dims = 2)
#'           # Scale portion of the weibull function is constrained to positive values with exponential function in epm_weibull.R
#'           gridbetas_exp <- exp(gridbetas)
#'           # alphaEPM=exp(alphaEPM)
#'         }
#'         
#'         if(length(p)==1){
#'           for(rep in 1:obsnum){
#'             
#'             # TODO: what is bTermsMatrix supposed to be
#'             bTermsMatrix= matrix(bTerms[rep,], nrow=alts) #reshape(bTerms[rep,],alts,bchar+1)
#'             
#'             if(grepl('weibull', predict_temp[[1]]$type, ignore.case=TRUE)){
#'               numerEPM[rep,] = exp((alphaEPM * p * gammakexp  + bTermsMatrix * Beta)/sigmachoice)
#'               
#'             } else {
#'               numerEPM[rep,] = exp((alphaEPM * p + bTermsMatrix * Beta)/sigmachoice)   #exp(betaLogit+miles*BmilesLogit+ milesSQ*bmilesSQ+Betaf*bf+....ect....
#'             }
#'           }
#'           
#'         } else {
#'           for(rep in 1:obsnum){
#'             
#'             # TODO: what is bTermsMatrix supposed to be
#'             bTermsMatrix <- matrix(bTerms[rep,], nrow=alts) #reshape(bTerms(rep,:),alts,bchar+1)
#'             
#'             if(grepl('weibull', predict_temp[[1]]$type, ignore.case=TRUE)){
#'               numerEPM[rep,] = exp((gridbetas_exp * p[rep] * gammakexp + bTermsMatrix * Beta)/sigmachoice)
#'               
#'             } else {
#'               numerEPM[rep,] = exp((alphaEPM*p[rep] + bTermsMatrix*Beta)/sigmachoice)   #exp(betaLogit+miles*BmilesLogit+ milesSQ*bmilesSQ+Betaf*bf+....ect....
#'             }
#'           }
#'         }
#'         
#'         
#'         # WHY IS THIS SUBTRACTING BY adjustS?? These values for EmaxM and EmaxMm should cancel eachother out and could be omitted from the calculation according to Haynie and Layton 2010?
#'         EmaxM = WelfareScale*(sigmachoice*(log(rowSums(numerEPM)))-adjustS) # sum by row
#'         
#'         EmaxMm = WelfareScale*(sigmachoice*(log(rowSums(numerEPM[,zoneIdOut])))-adjustS) # sum by row
#'         
#'         
#'         welfareDiff[,V]=EmaxMm-EmaxM
#'         
#'         prctWelfare[,V]=(EmaxMm-EmaxM)/EmaxM
#'         
#'       } # End betadraws loop
#'       
#'       sortWelfareDiff= matrix(welfareDiff, nrow=betadraws*numTrips) #reshape(welfareDiff,betadraws*numTrips,1)
#'       prW[quant]= mean(prctWelfare, na.rm=TRUE) #nanmean(nanmean(prctWelfare))
#'       welfare[quant] = quantile(sortWelfareDiff, probs=c(0.025,.05,.50,.95,.975)) # prctile(sortWelfareDiff,[2.5,5,50,95,97.5])  # FIXME check how this deals with nans
#'       
#'     } #end quant loop
#'   }# End EPM calculations
#'   return(list(prW=prW, welfare=welfare))
#' }

##### Old (and some superfluous) code

######### This chunk of code is superfluous as information is already in list form in predict_temp
# scenario <- list()
# 
# for (i in 1:length(predict_temp)){
#   zoneIdOut <- predict_temp[[i]]$zoneIdOut
#   zoneIdIn <- predict_temp[[i]]$zoneIdIn
#   tac <- predict_temp[[i]]$tac
#   alts <- length(predict_temp[[i]]$zoneID) #length(model.gridID)
#   
#   # Currently only works for tac=0, set zondeidin to zone where tac=0 and zoneIDout to all others
#   zi <- which(tac == 0)
#   if(!is_empty(zi)){
#     zoneIdOut <- c(zoneIdOut, zoneIdIn[-zi])
#     zoneIdIn <- zoneIdIn[zi]
#   } else {
#     zoneIdOut <- zoneIdOut
#   }
#   
#   scenario[[length(scenario)+1]] <- list(scenario.name=predict_temp[[i]]$scenario.name, zoneIdIn=zoneIdIn, zoneIdOut=zoneIdOut)
# }
#########


#numData=~[data.isXY] & ~[data.isID] &~[data.isMatTime]
#combineData=[data(numData).dataColumn] 
#dataPerZone=combineData(modelInputData.dataZoneTrue,:) 
#x=[]

# zoneIdIn=find(ismember(zoneid,zoneCloseMap)) #in closed zones, returns
# [] if none  #not the same since we only need 100% closures
#zoneIdOut=find(~ismember(zoneid,zoneCloseMap)) #outside of closures, returns [] 
#The predict_model function needs to have each scenario with 

##                                                                     
#x=create_zonal_input(modelInputData, dataPerZone)

#FIND ZONES SPECIFIED WITH TAC = 0
#zi=find(tac==0)
#if(!isempty(zi){  ##-> replace isempty
#zoneIdOut=find(~ismember(zoneid,zoneCloseMap(zi)))
#} else {
#zoneIdOut=zoneidNUM
#}

#zoneid=model.gridID