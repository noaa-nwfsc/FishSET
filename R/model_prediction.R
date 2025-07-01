#' Model prediction
#' 
#' Estimate redistributed fishing effort following policy change or change in other factors that 
#' influence fisher location choice.
#' 
#' @param project Name of project
#' @param mod.name Model name. Argument can be the name of the model or the name can be pulled the 
#'    modelChosen table. Leave \code{mod.name} empty to use the name of the saved "best" model. If 
#'    more than one model is saved, \code{mod.name} should be the numeric indicator of which model 
#'    to use. Use \code{table_view("modelChosen", project)} to view a table of saved models.
#' @param closures closure scenarios
#' @param enteredPrice NEED TO FIGURE OUT WHAT EXACTLY THIS IS
#' @param use.scalers Input for \code{create_model_input()}. Logical, should data be normalized? 
#'    Defaults to \code{FALSE}. Rescaling factors are the mean of the numeric vector unless 
#'    specified with \code{scaler.func}.
#' @param scaler.func Input for \code{create_model_input()}. Function to calculate 
#'    rescaling factors.
#' @importFrom yaml read_yaml
#' @importFrom DBI dbConnect dbDisconnect dbGetQuery
#' @importFrom RSQLite SQLite
#' @details Calls \code{\link{logit_predict}}, \code{\link{epm_predict}}, and 
#'    \code{\link{predict_probability}}. Closure scenarios and TAC must be define using 
#'    \code{\link{zone_closure}} function before function can be run. 
#' @export
#' @keywords internal 
#' 

model_prediction <- function(project, mod.name, closures, enteredPrice = NULL, 
                             use.scalers = FALSE, scaler.func = NULL){
  
  # 1. Retrieve the model data ----
  # Call in datasets
  fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project = project))
  on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
  
  model_input_query <- paste0("SELECT ModelInputData FROM ", project, "ModelInputData LIMIT 1")
  x_temp <- unserialize(DBI::dbGetQuery(fishset_db, model_input_query)$ModelInputData[[1]])
  
  if(!any(mod.name %in% sapply(x_temp , "[[" , "mod.name"))){
    stop("Model design file was not found for", mod.name)
  }
  
  for (ii in seq_along(mod.name)) { 
    x_new <- x_temp[[which(sapply(x_temp, function(x) x[["mod.name"]]) == mod.name[ii])]]
    
    # 2. Create data matrix ----
    choice_raw <- x_new[["choice"]]
    zoneID <- sort(unique(choice_raw)[,1])
    alts <- length(zoneID)
    
    # Extra code for EPM models later on...
    # dataCompile <- mod.dat$dataCompile
    # if(!is_empty(x_new$epmDefaultPrice)){
    #   price <- x_new$epmDefaultPrice
    #   dataCompileEPM <- dataCompile*price
    # } else {
    #   price <- NULL
    #   dataCompileEPM <- NULL
    # }
    n_scenarios <- length(closures)
    
    # Create an empty list to save predictions from policy scenarios
    predict <- list()
    
    # 3. Set up closure scenarios ----
    for(i in seq_len(n_scenarios)){
      # Closed zones 
      scenarioname <- closures[[i]]$scenario
      zone.closure <- gsub("Zone_", "", closures[[i]]$zone)
      tac <- as.numeric(closures[[i]]$tac)
      
      # Make sure the closure zone selected are included in the model
      if(any(zone.closure %in% zoneID)){
        z <- zone.closure
      } else {
        z <- 0
        warning('Selected closed zone was not found in the model')
      }
      
      stopifnot(z != 0)
      
      # ID for closed zones
      zoneClosedFish <- c(0,z) # id for zones closed to fishing in given time
      
      # Get TAC
      # percent of TAC allowed during specific times when there was a closure per season
      tacAllowedAllTime <- c(1, tac/100) # first cell has base in it
      
      # zone id's for in and out of regions
      if(all(!is_empty(which(z %in% as.character(zoneID))))){
        zoneIdIn <- zoneID[which(as.character(zoneID) %in% z)]
        zoneIdOut <- zoneID[-which(as.character(zoneID) %in% z)]
      }
      
      ###
      # 4. Run Model predictions ----
      ###
      
      ## Logit models ----
      if (grepl('logit', x_new$likelihood)) {  
        ## Set up conditional and zonal logit
        # predicted probabilities of selecting zones for each TAC
        ProbL <-  matrix(1, alts, length(tacAllowedAllTime)) 
        # sum of probabilities in and out of closures
        InOutLogit <- matrix(0,length(tacAllowedAllTime), 2) 
        
        # Run logit model prediction
        temp <- logit_predict(project = project, mod.name = mod.name[ii], 
                              use.scalers = use.scalers, scaler.func = scaler.func) 
        probLogit <- temp[[1]] # Predicted probabilities of selecting each zone
        modelDat <- temp[[2]] # Model data
        
        # Predictions for zones in closures
        probDataModelIn <- as.data.frame(probLogit[match(zoneIdIn,probLogit[,1]),])
        # if(dim(probDataModelIn)[1] == 1){
        #   probDataModelIn <- t(probDataModelIn)
        # }
        
        # Predictions for zones not in closures
        probDataModelOut <- as.data.frame(probLogit[match(zoneIdOut,probLogit[,1]),])
        # if(dim(probDataModelOut)[1] == 1){
        #   probDataModelOut <- t(probDataModelOut)
        # }
        
        # Calculate redistributed predicted probabilities
        for(t in 1:length(tacAllowedAllTime)){           
          # full TAC and no zone closure (baseline)
          if(tacAllowedAllTime[t]==1 &&  zoneClosedFish[t] == 0){  
            sumProbDataModelIn <- sum(probDataModelIn[,2])
            sumProbDataModelOut <- sum(probDataModelOut[,2])
            InOutLogit[t,1] <- sumProbDataModelIn
            InOutLogit[t,2] <- sumProbDataModelOut
            ProbL[,t] <- probLogit[,2]*100
            
          } else {
            # add up prob for all closed zones
            zoneClose <- sum(probLogit[match(zoneClosedFish, probLogit[,1]),2], na.rm=TRUE) 
            predout <- predict_probability(probLogit, probDataModelIn, probDataModelOut, 
                                           zoneClose, tacAllowedAllTime[t],zoneClosedFish)
            InOutLogit[t,1] <- predout$sumPredictIn
            InOutLogit[t,2] <- predout$sumPredictOut
            ProbL[,t] <- predout$probPredict[,2]*100
          }
        }
        
        predict[[length(predict)+1]] <- list(
          scenario.name = paste(mod.name[ii], closures[[i]]$scenario), 
          InOut = InOutLogit, prob = ProbL, 
          time = 'nameAllTime', type = 'Logit', tac = tac, 
          zoneID = zoneID, zoneIdIn = zoneIdIn, zoneIdOut = zoneIdOut,
          modelDat = modelDat)
        
        ## EPM models ----  
      } else if (grepl('epm', x_new$likelihood)){
        
        # TODO: remove the following line when epm models are functioning
        # x_new$scales$price <- NULL
        
        #prices for EPM forecasting 
        # pscale <- x_new$scales$price
        # priceData <- x_new$epmDefaultPrice
        # 
        # if(is.na(enteredPrice)){
        #   p= priceData/pscale
        #   allPrice= c(p, p) #{p,p}  #use internal price 
        # } else {
        #   newPrice = enteredPrice/pscale
        #   p = priceData/pscale
        #   allPrice <- c(p,newPrice)  # size = size(nameAllTime); first cell has base in it
        # }
        
        ## Set up EPM probability matrices
        # predicted probabilities of selecting zones for each TAC
        ProbE <- matrix(1, alts, length(tacAllowedAllTime))
        # sum of probabilities in and out of closures
        InOutepm <- matrix(0,length(tacAllowedAllTime), 2) 
        # sum of probabilities in and out of closures
        InOutepm_base <- matrix(0,length(tacAllowedAllTime), 2) 
        
        # Run logit model prediction
        temp <- epm_predict(project = project, mod.name = mod.name[ii], 
                            mod.type = x_new$likelihood, 
                            use.scalers = use.scalers, scaler.func = scaler.func) 
        probepm <- temp[[1]] # Predicted probabilities of selecting each zone
        modelDat <- temp[[2]] # Model data
        
        # Predictions for zones in closures
        probDataModelIn <- as.data.frame(probepm[match(zoneIdIn, probepm[,1]),])
        
        # Predictions for zones not in closures
        probDataModelOut <- as.data.frame(probepm[match(zoneIdOut, probepm[,1]),])
        
        # Calculate redistributed predicted probabilities
        for(t in 1:length(tacAllowedAllTime)){           
          # full TAC and no zone closure (baseline)
          if(tacAllowedAllTime[t]==1 &&  zoneClosedFish[t] == 0){  
            sumProbDataModelIn <- sum(probDataModelIn[,2])
            sumProbDataModelOut <- sum(probDataModelOut[,2])
            InOutepm[t,1] <- sumProbDataModelIn
            InOutepm[t,2] <- sumProbDataModelOut
            ProbE[,t] <- probepm[,2]*100
            
          } else {
            # add up prob for all closed zones
            zoneClose <- sum(probepm[match(zoneClosedFish, probepm[,1]),2], na.rm=TRUE) 
            predout <- predict_probability(probepm, probDataModelIn, probDataModelOut, 
                                           zoneClose, tacAllowedAllTime[t], zoneClosedFish)
            InOutepm[t,1] <- predout$sumPredictIn
            InOutepm[t,2] <- predout$sumPredictOut
            ProbE[,t] <- predout$probPredict[,2]*100
          }
        }
        
        
        # for (t in 1:length(allPrice)) {
        #   #switch t
        #   #case 1 % full TAC and no zone closure
        #   # this code is different then logit according to original code,
        #   # predictions are based on given year, not on base year
        #   # full TAC and no zone closure
        #   #1 epm normal
        #   
        #   # temp <- epm_predict(project=project, modname=mod.name, alts=alts, mod.type=sub(".*_", "", x_new$likelihood), price=price) #modelOutput{mChoice},alts,x);
        #   probEPM <- temp$probEPM
        #   modelDat <- temp$modelDat
        #   
        #   
        #   ProbE[,t] <- t(probEPM)*100 #probEPM'*100;
        #   InOutEPM[t,1] = sum(probEPM[zoneIdIn])
        #   InOutEPM[t,2] = sum(probEPM[zoneIdOut])
        #   
        #   if (zoneClosedFish[t]==0) {
        #     zoneClose=0
        #   } else {
        #     zoneClose = sum(probEPM(zoneClosedFish[t])) # add up prob for all closed zones
        #   }
        #   
        #   #?probEPM=[zoneidNUM t(probEPM)] #[zoneidNUM probEPM']
        #   probDataModelIn=  as.data.frame(probEPM[match(zoneIdIn,probEPM[,1]),]) #probEPM[zoneIdIn,]
        #   probDataModelOut=  as.data.frame(probEPM[match(zoneIdOut,probEPM[,1]),])# probEPM[zoneIdOut,]
        #   
        #   sumProbDataModelIn=sum(probDataModelIn[,2])
        #   sumProbDataModelOut=sum(probDataModelOut[,2])
        #   InOutEPMbase[t,1]=sumProbDataModelIn
        #   InOutEPMbase[t,2]=sumProbDataModelOut
        #   
        #   # Predictions for EPM
        #   
        #   predout <- predict_probability(probEPM, probDataModelIn, probDataModelOut, zoneClose, tacAllowedAllTime[t],zoneClosedFish)
        #   #[probPredict, sumPredictIn,sumPredictOut]=predict_probaibility(probDataModelIn, probDataModelOut, zoneClose, tacAllowedAllTime(t),zoneClosedFish{t})
        #   
        #   ProbE[,t] = predout$probPredict[,2]*100
        #   InOutEPM[t,1] = predout$sumPredictIn
        #   InOutEPM[t,2] = predout$sumPredictOut
        #   
        # } #end t loop
        
        predict[[length(predict)+1]] <- list(
          scenario.name = paste(mod.name[ii], closures[[i]]$scenario), 
          InOut=InOutepm, prob = ProbE, 
          time='nameAllTime', type=x_new$likelihood, tac=tac, 
          zoneID=zoneID, zoneIdIn=zoneIdIn, zoneIdOut=zoneIdOut,
          modelDat=modelDat)
        
      } #end epm loop
    } #end closure loop
    
    # Write to FishSET database
    predOut_nm <- paste0(project, "predictOutput")
    predOut_exists <- table_exists(predOut_nm, project)
    date_sql <- paste0(project, "predictOutput", format(Sys.Date(), format = "%Y%m%d"))
    date_tab_exists <- table_exists(date_sql, project)
    
    if (predOut_exists) {
      # if the table already exists, then save the data and remove the existing table
      predict_out_query <- paste0("SELECT PredictOutput FROM ", project, "predictOutput LIMIT 1")
      pOutput <- DBI::dbGetQuery(fishset_db, predict_out_query)
      if (nrow(pOutput) == 0 | length(pOutput$PredictOutput) == 0) {
        # Table is empty, remove and assign pOutput
        table_remove(predOut_nm, project)
        pOutput <- predict
        
      } else {
        pOutput <- unserialize(pOutput$PredictOutput[[1]])  
        table_remove(predOut_nm, project)  
        # Get names of output scenarios
        pOutput_db <- unlist(lapply(pOutput, function(x) x$scenario.name))
        pOutput_n <- unlist(lapply(predict, function(x) x$scenario.name))
        
        # If the scenario name was already in the database, then overwrite the existing model, 
        # else add the new model Used a for loop because apply functions were now working...
        for(tmp_i in 1:length(pOutput_n)){
          if(pOutput_n[tmp_i] %in% pOutput_db){
            pOutput[[which(pOutput_db %in% pOutput_n[tmp_i])]] <- predict[[tmp_i]]
          } else {
            pOutput[[length(pOutput) + 1]] <- predict[[tmp_i]]
          }
        }
      }
      
    } else {
      # If the table doesn't exist create new variable for saving data
      pOutput <- predict
      
    }
    
    DBI::dbExecute(fishset_db, 
                   paste("CREATE TABLE IF NOT EXISTS", predOut_nm, "(PredictOutput predict)"))
    DBI::dbExecute(fishset_db, 
                   paste("INSERT INTO", predOut_nm, "VALUES (:PredictOutput)"),
                   params = list(PredictOutput = list(serialize(pOutput, NULL))))
    
  }
}
