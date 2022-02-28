  
  #' Welfare analysis code
  #' @param project Name of project
  #' @param mod.name Name of selected model (mchoice)
  #' @param expected.catch.name Name of expectedchatch table to use
  #' @param enteredPrice Price for welfare
  #' @importFrom DBI dbConnect dbDisconnect dbGetQuery
  #' @importFrom RSQLite SQLite
  #' @export
  #' @details Called by \code{\link{run_policy}} function. The \code{\link{model_prediction}} function should be run before this function.
welfare_predict <- function(project, mod.name, expected.catch.name, enteredPrice){

 #TODO make stdard version, only works for zone that all 100% closed and
 #compares closure at entered price versus non closure at that price (not a comparison to other years
 #inputs:
 #   filename: filename of model file to be used
 #   zoneCloseMap: which zones have been closed
 #   tac: percent of closure of a zone
 #   enteredPrice:  the price entered in the gui by user for welfare
 #   determination
 #output:
 # prW
 # welfare
 # numTrips
 
   betadraws <- 1000

  #Call model prediction output
  fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project=project))
  on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
  
  predict_temp <- unserialize(DBI::dbGetQuery(fishset_db, paste0("SELECT PredictOutput FROM ", project, "predictoutput LIMIT 1"))$PredictOutput[[1]])
  

   #Get inverse hessian for selected model
  #Get model output
  tlength <- length(tables_database(project=project)[grep('modelOut', tables_database(project=project))])
     for(i in 1:tlength){
        tempname <- tables_database(project=project)[grep('modelOut', tables_database(project=project))][tlength]
        out <- model_out_view(table = tempname, project=project)
    if(any(lapply( out , "[[" , "name" ) == mod.name)==TRUE){
      break   #model.H1{mChoice} #Note added inv hessian
    } 
    }

  stopifnot(any(lapply( out , "[[" , "name" ) == mod.name)==TRUE)
    warning('Specified model not found. Check name.')
  
  
  invHess <- out[[which(lapply( out , "[[" , "name" ) == mod.name)]]$H1 
  
#numData=~[data.isXY] & ~[data.isID] &~[data.isMatTime]
#combineData=[data(numData).dataColumn] 
#dataPerZone=combineData(modelInputData.dataZoneTrue,:) 
#x=[]


 # zoneIdIn=find(ismember(zoneid,zoneCloseMap)) #in closed zones, returns
 # [] if none  #not the same since we only need 100% closures
 #zoneIdOut=find(~ismember(zoneid,zoneCloseMap)) #outside of closures, returns [] 
#The predict_model function needs to have each scenario with 
 #Put this somewhere
  if(exists(paste0(locoutput(project), pull_output(project, type='zone', fun='closures')))) {
    closures <- yaml::read_yaml(paste0(locoutput(project), pull_output(project, type='zone', fun='closures')))
  } 

  
#modelOutput=cell2mat(model.Out{mChoice}(:,1:end))
#modeloutput is  the parameter estimates
  if(file.exists(read_dat(paste0(locoutput(project), pull_output(project, type='table', fun=paste0('params_',  mod.name)))))){
      modelOutput <- read_dat(paste0(locoutput(project), pull_output(project, type='table', fun=paste0('params_',  mod.name))))
  } else {
    "Parameter estimate table could not be found."
  }
#zoneID <- logitEq$X
modelOutput <- modelOutput[,2]


 ##                                                                     
  #x=create_zonal_input(modelInputData, dataPerZone)
  #Get model data
  # Need data compile, distance, gridvarying, interaction terms
  x <- predict_temp[[1]]$modelDat

    
scenario <- list()
for (i in 1:length(predict_temp)){
    
zoneIdOut <- predict_temp[[i]]$zoneIdOut
zoneIdIn <- predict_temp[[i]]$zoneIdIn
tac <- predict_temp[[i]]$tac
alts <- length(predict_temp[[i]]$zoneID) #length(model.gridID)

#Currently only works for tac=0, set zondeidin to zone where tac=0 and zoneIDout to all others
zi <- which(tac == 0)
  if(!is_empty(zi)){
    zoneIdOut <- c(zoneIdOut, zoneIdIn[-zi])
    zoneIdIn <- zoneIdIn[zi]
  } else {
    zoneIdOut <- zoneIdOut
  }

scenario[[length(scenario)+1]] <- list(scenario.name=predict_temp[[i]]$scenario.name, zoneIdIn=zoneIdIn, zoneIdOut=zoneIdOut)
}

  #FIND ZONES SPECIFIED WITH TAC = 0
#zi=find(tac==0)
#if(!isempty(zi){  ##-> replace isempty
#zoneIdOut=find(~ismember(zoneid,zoneCloseMap(zi)))
#} else {
#zoneIdOut=zoneidNUM
#}


#zoneid=model.gridID
zoneidNUM <- t(1:alts) #[1:length(zoneid)]"'"


if(predict_temp[[1]]$type == 'Logit'){
  for(i in length(scenario)){
    zoneID <- predict_temp[[i]]$zoneID
    scenario[[i]]$open <- matrix(1, 1, alts) #ones(1,alts)
    colnames(scenario[[i]]$open) <- zoneID
    if(!is_empty(zi)){
      scenario[[i]]$open[,which(colnames(scenario[[i]]$open)==zoneIdIn)] <- 0 #Replace columns with 0 for zoneIDIn
    }
  scenario[[i]]$quality <- matrix(1,1, alts) 
  colnames(scenario[[i]]$quality) <- zoneID
           #ones(alts,1) #FIXME to generalize   #TODO these become percent changes to variables or  on off to variables
  #?  scenario.a.quality(:,end+1)=ones(alts,1) #FIXME to generalize
  }

  numTrips=nrow(x)
  # number of sims for generating std errs (note, each sim can be run
  # independently so this is easy to parralelize using parfor):

  # draw sims beta vectors consistent with estimated model above:
  # This method follows Krinsky Robb.  It would also be possible to
  # bootstrap the std err's of the welfare measure but would be
  # computationally more expensive.


  #check for positive definate
  #FIXME see main_welfare_like_rob_cjb.m
  #[r,p]=chol(invHess)
  #if (p==0){
  mu_rand <- t(mvgrnd(modelOutput,invHess,betadraws))  # flipped so that rows are output and columns are draws
  welfare = sim_welfare(project=project, X=x,alts=alts,beta_j=mu_rand, scenario)

  #  warning('Welfare prediction cannot be performed as the inverse hessian is not positive definite')

  # prW <- NA
  #  welfare <- NA
  #}

  prW <- NA

} else if(grepl('epm', predict_temp[[1]]$type, ignore.case=TRUE)){
  x <- predict_temp[[1]]$modelDat

numTrips <- nrow(x)

priceData= predict_temp[[1]]$price # model.priceCol{mChoice}
pscale=mean(priceData, na.rm=T) #scaler

if(is.na(enteredPrice)) {
  p= priceData/pscale
  allPrice= c(p,p) #{p,p}   # use internal price
} else {
  newPrice=enteredPrice/pscale
  p= priceData/pscale
  allPrice= c(p, newPrice) #{p,newPrice}   # size = size(nameAllTime) first cell has base in it
}


A=alts*alts+1
bTerms <- x[,-c(1:(A-1))] #A:end]
 #bTerms=x

WelfareScale=100000

 #check that invhess is positive definite


mu_rand= t(mvgrnd(modelOutput,invHess,betadraws))  # fliped so that rows are output and columns are draws

z=length(modelOutput[,1])




for (quant in 1:length(allPrice)){   # not right for base case .. should skip base case? need to add something to take care of partial closres?
  if(quant==1){
    prW[quant] <- NA
    welfare[quant] <- NA
  } else { #skip base case for speed right now t comply with only using later case in plotting...
    p=allPrice[quant]
    welfareDiff <- matrix(NA, numTrips, betadraws) #nan(numTrips,betadraws)
    numerEPM= matrix(NA, numTrips, alts) #nan(numTrips,alts)
    prctWelfare <- matrix(NA, numTrips, betadraws)
  }
  
for(V in 1:betadraws){
#switch modelNameChoice
#case {'EPM gamma'}
#bcData= model.bcDataCol{mChoice}  #this is the column of bc data used with gammas in the modeling
#alphaEPM=mu_rand(1:alts,V) #NOTE: this comes from mu_rand now instead of modelOutput
#gammasEPM=mu_rand(alts+1:2*alts,V)

#bchar=size(modelInputData.bCHeader,2)-1 
#Beta=mu_rand(2*alts+1:2*alts+1+bchar,V) #NOTE: this comes from mu_rand now instead of modelOutput
#sigmachoice= mu_rand(2*alts+bchar+2,V) #NOTE: this comes from mu_rand now instead of modelOutput
#adjustS=sigmachoice*0.57721

#otherwise
  alphaEPM <- mu_rand[1:alts,V] #NOTE: this comes from mu_rand now instead of modelOutput
  bchar= 2 #!<<<<<< HERE >>>>>>>> size(modelInputData.bCHeader,2)-1 
  Beta <- mu_rand[alts+1:alts+1+bchar,V] #NOTE: this comes from mu_rand now instead of modelOutput
  sigmachoice <- mu_rand[alts+bchar+2,V] #NOTE: this comes from mu_rand now instead of modelOutput
  adjustS <- sigmachoice*0.57721


  if(grepl('normal', predict_temp[[1]]$type, ignore.case=TRUE)){

  # added for log-normal
    sigmaa <- mu_rand[-c(1:(alts+bchar+2)),V]  #NOTE: this comes from mu_rand now instead of modelOutput
    half_sigmaa_sq <- 0.5*(sigmaa^2)
    alphaEPM=exp(alphaEPM+half_sigmaa_sq)  # this is substituted below
  } else {
  #'EPM weibull'
    k=mu_rand[-c(1:(z-(alts-1)-1)),V] #FIXME check this is correct
    k_exp=exp(k)
    alphaEPM=exp(alphaEPM)
    gammakexp=gamma((k_exp+1)/k_exp)
  }


if(length(p)==1){

  for(rep in 1:numTrips){

    bTermsMatrix= matrix(bTerms[rep,], nrow=alts) #reshape(bTerms[rep,],alts,bchar+1)
    if(grepl('weibull', predict_temp[[1]]$type, ignore.case=TRUE)){
      numerEPM[rep,] = exp((alphaEPM*p*gammakexp  + bTermsMatrix*Beta)/sigmachoice)
    } else {
      numerEPM[rep,] = exp((alphaEPM*p + bTermsMatrix*Beta)/sigmachoice)   #exp(betaLogit+miles*BmilesLogit+ milesSQ*bmilesSQ+Betaf*bf+....ect....
    }
  }
} else {

  for(rep in 1:numTrips){

  bTermsMatrix=matrix(bTerms[rep,], nrow=alts) #reshape(bTerms(rep,:),alts,bchar+1)
  if(grepl('weibull', predict_temp[[1]]$type, ignore.case=TRUE)){
    numerEPM[rep,] = exp((alphaEPM*p[rep]*gammakexp + bTermsMatrix*Beta)/sigmachoice)
  } else {
    numerEPM[rep,] = exp((alphaEPM*p[rep] + bTermsMatrix*Beta)/sigmachoice)   #exp(betaLogit+miles*BmilesLogit+ milesSQ*bmilesSQ+Betaf*bf+....ect....
  }
  }
}



EmaxM = WelfareScale*(sigmachoice*(log(rowSums(numerEPM)))-adjustS) # sum by row

EmaxMm = WelfareScale*(sigmachoice*(log(rowSums(numerEPM[,zoneIdOut])))-adjustS) # sum by row


welfareDiff[,V]=EmaxMm-EmaxM

prctWelfare[,V]=(EmaxMm-EmaxM)/EmaxM

} # End betadraws loop

sortWelfareDiff= matrix(welfareDiff, nrow=betadraws*numTrips) #reshape(welfareDiff,betadraws*numTrips,1)
prW[quant]= mean(prctWelfare, na.rm=TRUE) #nanmean(nanmean(prctWelfare))
welfare[quant] =quantile(sortWelfareDiff, probs=c(0.025,.05,.50,.95,.975)) # prctile(sortWelfareDiff,[2.5,5,50,95,97.5])  # FIXME check how this deals with nans

} #end quant loop
}# End EPM calculations
return(list(prW=prW, welfare=welfare))
}
