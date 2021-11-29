#Policy projections

# Need to call saved data table
#1. Save favorite model by model name in separate file
#2. Pull that data, not the genereric data

##Needs
  #data matrix
  #Zone closure
  #Tac
  #Price
  #Prediction probablity 

#' Model prediction function
#' @param project Name of project
#' @param mod.name Name of saved model to use
#' @param expected.catch.name Required for conditonal logit (\code{logit_c}) model. 
#'   Name of expected catch table to use. 
#'    Can be the expected catch from the short-term scenario (\code{short}), the medium-term scenario (\code{med}), the 
#'    long-term scenario (\code{long}), or the user-defined temporal parameters (\code{user}).
#' @param enteredPrice NEED TO FIGURE OUT WHAT EXACTLY THIS IS
#' @details Calls \code{\link{logit_predict}}, \code{\link{epm_predict}}, and \code{\link{predict_probability}}.
#'    Closure scenarios and TAC must be define using \code{\link{zone_closure}} function before function can be run. 
#' @export
#' @keywords internal 
#' 


model_prediction <- function(project, mod.name, expected.catch.name=NULL, enteredPrice){
  

#Functions to create
  #model prediction function (logit, epm, etc)
  #Predict probability

#Required values
nreps <- 100

#. First - create the data

# Call in datasets
fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project = project))
on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)

x_temp <- unserialize(DBI::dbGetQuery(fishset_db, paste0("SELECT ModelInputData FROM ", project, "modelinputdata LIMIT 1"))$ModelInputData[[1]])
x_new <- x_temp[[which(lapply( x_temp , "[[" , "mod.name" ) == mod.name)]]

#Create data matrix
choice_raw <- x_new[["choice"]]
zoneID <- sort(unique(choice_raw))
alts <- length(zoneID)
choice <- data.frame(as.matrix(as.numeric(factor(choice_raw))))
dataCompile <- create_logit_input(choice)

price <- x_new$epmDefaultPrice
dataCompileEPM <- dataCompile*price
predict <- list()

#Create model output
#modelOutput <- cell2mat(model.Out{mChoice}(:,1:end));

#Get number of trip and alternative choices
#[trips, alts]=size(modelInputData.zonalChoices); # this is rows of data
trips <- nrow(choice)
ab <- max(choice)



  #Read in zone closure information
  if(utils::file_test("-f", paste0(locoutput(project), pull_output(project, type='zone', fun='closures')))) {
    closures <- yaml::read_yaml(paste0(locoutput(project), pull_output(project, type='zone', fun='closures')))
  } else {
  # Open zone closure function in file not found  
    warning('Closure scenarios must be specified using zone_closure function.')
  }


for(i in 1:length(closures)){
#Closed zones 
  scenarioname <- closures[[i]]$scenario
  zone.closure <- closures[[i]]$zone
  tac <- as.numeric(closures[[i]]$tac)
  zone.closure <- stringr::str_remove(zone.closure, "Zone_")
  
  if(any(zone.closure %in% zoneID)){
        z <- zone.closure
  } else {
      z <- 0
      warning('Selected closed zone was not found in the model')
  }
  
  
 
zoneClosedFish <- c(0,z) # id for zonesz closed to fishing in given time, size same as nameALlTime

  
 #Get TAC
 #percent/100 of TAC that was allowed during specific times when there was a closure per season
  
  tacAllowedAllTime <- c(1, tac/100)    # first cell has base in it
#  nameAllTime={'base', nameTime}  # first cell has base in it
    # nameTime is used to loop through scenarios.
  
# zone id's for in and out of regions
if(all(!is_empty(which(z %in% as.character(zoneID))))){
  zoneIdIn <-  zoneID[which(z %in% as.character(zoneID))]
  zoneIdOut <-  zoneID[-which(z %in% as.character(zoneID))]
}
# all boat characteristic data for all model comes from X in file.. - need to program

###
#Run Model predictions
###

if (grepl('logit', x_new$likelihood)) {  #need to correct this
##'Zonal (ASC) Logit'
  ProbL <-  matrix(1, alts, length(tacAllowedAllTime))   #zeros(alts,length(tacAllowedAllTime));
  InOutLogit <- matrix(0,length(tacAllowedAllTime), 2)  #=zeros(length(tacAllowedAllTime),2);


  temp <- logit_predict(project=project, mod.name=mod.name, expected.catch.name=expected.catch.name) #modelOutput{mChoice},alts,x);
  probLogit <- temp$probLogit
  modelDat <- temp$modelDat
  # Predictions for zonal logit
        probDataModelIn <- as.data.frame(probLogit[match(zoneIdIn,probLogit[,1]),])
        
          if(dim(probDataModelIn)[2] == 1){
            probDataModelIn <- t(probDataModelIn)
          }
        
        probDataModelOut <- as.data.frame(probLogit[match(zoneIdOut,probLogit[,1]),])
        
          if(dim(probDataModelOut)[2] == 1){
            probDataModelOut <- t(probDataModelOut)
          }

        for(t in 1:length(tacAllowedAllTime)){           
              if(tacAllowedAllTime[t]==1 &&  zoneClosedFish[t] == 0){  # full TAC and no zone closure (baseline)
                  sumProbDataModelIn <- sum(probDataModelIn[,2])
                  sumProbDataModelOut <- sum(probDataModelOut[,2])
                  InOutLogit[t,1] <- sumProbDataModelIn
                  InOutLogit[t,2] <- sumProbDataModelOut
                  ProbL[,t] <- probLogit[,2]*100
              } else {
                  zoneClose <- sum(probLogit[match(zoneClosedFish, probLogit[,1]),2], na.rm=TRUE) # add up prob for all closed zones
                  predout <- predict_probability(probLogit, probDataModelIn, probDataModelOut, zoneClose, tacAllowedAllTime[t],zoneClosedFish)
                  InOutLogit[t,1] <- predout$sumPredictIn
                  InOutLogit[t,2] <- predout$sumPredictOut
                  ProbL[,t] <- predout$probPredict[,2]*100
              }
        }
    
        predict[[length(predict)+1]] <- list(scenario.name=scenarioname, InOut=InOutLogit, prob=ProbL, 
                                             time='nameAllTime', type='Logit', tac=tac, 
                                             zoneID=zoneID, zoneIdIn=zoneIdIn, zoneIdOut=zoneIdOut,
                                             modelDat=modelDat)
        
} else if (grepl('epm', x_new$likelihood)){
  
  
  #prices for EPM forecasting   
  pscale <- x_new$scales$price
  priceData <- x_new$epmDefaultPrice

  if(is.na(enteredPrice)){
    p= priceData/pscale
    allPrice= c(p, p) #{p,p}  #use internal price 
  } else {
    newPrice = enteredPrice/pscale
     p = priceData/pscale
    allPrice <- c(p,newPrice)  # size = size(nameAllTime); first cell has base in it
  }
  
  # EPM % Now the same process for the EPM
  InOutEPM = matrix(0,length(tacAllowedAllTime), 2)#zeros(length(tacAllowedAllTime),2);
  InOutEPMbase= matrix(0,length(tacAllowedAllTime), 2)# zeros(length(tacAllowedAllTime),2);
  ProbE = matrix(1, alts, length(tacAllowedAllTime)) #zeros(alts,length(tacAllowedAllTime));

  for (t in 1:length(allPrice)) {
  #switch t
  #case 1 % full TAC and no zone closure
  # this code is different then logit according to original code,
  # predictions are based on given  year, not on base year
  # full TAC and no zone closure
    #1 epm normal
    
    temp <- epm_predict(project=project, modname=mod.name, alts=alts, mod.type=mod.type, price=price) #modelOutput{mChoice},alts,x);
    probEPM <- temp$probEPM
    modelDat <- temp$modelDat

  
  ProbE[,t] <- t(probEPM)*100 #probEPM'*100;
  InOutEPM[t,1] = sum(probEPM[zoneIdIn])
  InOutEPM[t,2] = sum(probEPM[zoneIdOut])
  
    if (zoneClosedFish[t]==0) {
      zoneClose=0
    } else {
      zoneClose = sum(probEPM(zoneClosedFish[t])) # add up prob for all closed zones
    }
  
  #?probEPM=[zoneidNUM t(probEPM)] #[zoneidNUM probEPM']
  probDataModelIn=  as.data.frame(probEPM[match(zoneIdIn,probEPM[,1]),]) #probEPM[zoneIdIn,]
  probDataModelOut=  as.data.frame(probEPM[match(zoneIdOut,probEPM[,1]),])# probEPM[zoneIdOut,]
                    
  sumProbDataModelIn=sum(probDataModelIn[,2])
  sumProbDataModelOut=sum(probDataModelOut[,2])
  InOutEPMbase[t,1]=sumProbDataModelIn
  InOutEPMbase[t,2]=sumProbDataModelOut
                    
  # Predictions for EPM
                    
  predout <- predict_probability(probEPM, probDataModelIn, probDataModelOut, zoneClose, tacAllowedAllTime[t],zoneClosedFish)
  #[probPredict, sumPredictIn,sumPredictOut]=predict_probaibility(probDataModelIn, probDataModelOut, zoneClose, tacAllowedAllTime(t),zoneClosedFish{t})
                  
  ProbE[,t] = predout$probPredict[,2]*100
  InOutEPM[t,1] = predout$sumPredictIn
  InOutEPM[t,2] = predout$sumPredictOut
  
} #end t loop
  
  predict[[length(predict)+1]] <- list(scenario.name=scenarioname, InOut=InOutLogit, prob=ProbL, 
                                       time='nameAllTime', type=x_new$likelihood, tac=tac, 
                                       zoneID=zoneID, zoneIdIn=zoneIdIn, zoneIdOut=zoneIdOut,
                                       modelDat=modelDat, priceData = priceData)
  
} #end epm loop
}



 #Write to FishSET database
single_sql <- paste0(project, "predictOutput")
DBI::dbExecute(fishset_db, paste("CREATE TABLE IF NOT EXISTS", single_sql, "(PredictOut predict)"))
DBI::dbExecute(fishset_db, paste("INSERT INTO", single_sql, "VALUES (:PredictOutput)"),
               params = list(PredictOutput = list(serialize(predict, NULL))))


single_sql <- paste0(project, "predictOutput")
date_sql <- paste0(project, "predictOutput", format(Sys.Date(), format = "%Y%m%d"))
if (table_exists(single_sql, project)){ #& replace == FALSE) {
  # predictOutput <- table_view()
  pOutput <- unserialize(DBI::dbGetQuery(fishset_db, paste0("SELECT PredictOutput FROM ", project, "predictoutput LIMIT 1"))$PredictOutput[[1]])
  pOutput[[length(pOutput) + 1]] <- predict
} else {
  pOutput <- list()
  pOutput[[length(pOutput) + 1]] <- predict
}

if (table_exists(single_sql, project)) {
  table_remove(single_sql, project)
}
if (table_exists(date_sql, project)) {
  table_remove(date_sql, project)
}

DBI::dbExecute(fishset_db, paste("CREATE TABLE IF NOT EXISTS", single_sql, "(PredictOutput predict)"))
DBI::dbExecute(fishset_db, paste("INSERT INTO", single_sql, "VALUES (:PredictOutput)"),
               params = list(PredictOutput = list(serialize(pOutput, NULL)))
)
DBI::dbExecute(fishset_db, paste("CREATE TABLE IF NOT EXISTS", date_sql, "(PredictOutput predict)"))
DBI::dbExecute(fishset_db, paste("INSERT INTO", date_sql, "VALUES (:PredictOutput)"),
               params = list(PredictOutput = list(serialize(pOutput, NULL)))
)

}
