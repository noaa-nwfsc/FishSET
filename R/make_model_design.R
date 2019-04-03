#'  Make model design
#'
#' @param dataset dataframe or matrix
#' @param catchID  Name of variable that containts catch data such as 'HAUL'
#' @param alternativeMatrix Whether the alternative choice matrix should come from 'loaded data' or 'gridded data'
#' @param lon.dat Variable containing longitude data
#' @param lat.dat Variable containing latitude data
#' @param indeVarsForModel Independent variables to include in the model
#' @param gridVariablesInclude Variables from gridded dataset to include in the model. 
#' @param priceCol NULL If required, specify which variable contains price data
#' @param vesselID NULL If required, specify which varible defines vessel
#' @param project Project name. For naming output saved in sql database
#' @importFrom geosphere distm
#' @importFrom DBI dbGetQuery dbExecute
#' @return  ModelInputData a list containing information on alternative choice
#' @export make_model_design
#' @details Functions returns model design matrix. Calls the create_centroid function to generate Lat/Lon centroid of zones or areas.
#' Also calls the Alternative Choice functions which defines alternative fishing options.
#' #outputs:                                                                                                                                                                                                                   
#'     modelInputData <- list(
#'     catch=catch, #vector of data defined my uses as containing catch data 
#'     scales= c(catch=yscale, zonal=mscale, data=dscale),   # scale vectors to put catch data, zonal data, and other data to same scale   
#'     zonalChoices <- X, # area choice from data
#'     instances, dim(zonalChoices)[1],  #number of observations
#'     alts <- dim(zonalChoices)[2], #number of alternative zones
#'     epmDefaultPrice <- epmDefaultPrice,    #Price data is user-defined                                                                                                                                                                               
#'     dataZoneTrue <- dataZoneTrue,  #Vector of 0/1 indicating whether the data from taht zone is to be included.
#'     numOfNecessary <- Alt$numOfNecessary,  # # of hauls/trips per zone                                                                                                                                                   
#'     typeOfNecessary <- Alt$zoneType,  #haul or trip                                                                                                                                                                         
#'     altChoiceType <- altChoiceType, # function choice                                                                                                                                                                      
#'     altChoiceUnits <- altChoiceUnits, # units of X                                                                                                                                                                         
#'     altToLocal1 <- altToLocal1,   #Defines how starting points (Lat/Lon) are defined. Can be zonal centroid, port, etc                                                                                                                                                                                     
#'     altToLocal2 <- altToLocal2,   #Defines how end points (Lat/Lon) are defined
#'     bCHeader <-  bCHeader,  #Variables to include in the model, includes independent variables and interactions                                                                                                                                                                                             
#'     bColumnsWant <- bColumnsWant ,                                                                                                                                                                                        
#'     bInterAct <- bInterAct,                                                                                                                                                                                               
#'     msq <-  msq,      
#'     gridVaryingVariables <- gridVaryingVariables,
#'     created <- Sys.time()                                                                                                                                                                        
#'     )
# @examples


make_model_design <- function(dataset, catchID, alternativeMatrix = c("loaded data", "gridded data"), lon.dat, lat.dat, 
                               indeVarsForModel = "", gridVariablesInclude = "", priceCol = NULL, vesselID = NULL, project) {
  
  if (!exists("Alt")) {
    if (!exists('AltMatrixName')) {
      Alt <- unserialize(DBI::dbGetQuery(fishset_db, "SELECT AlternativeMatrix FROM altmatrix LIMIT 1")$AlternativeMatrix[[1]])
      if (!exists("Alt")) {
        stop("Alternative Choice Matrix does not exist. Please run the createAlternativeChoice() function.")
      }
    }
  }
  
  if (!exists("ExpectedCatch")) {
    stop("Expected Catch Matrix does not exist. Please run the create_expectations() function.")
  }  else {
    ExpectedCatch <- ExpectedCatch
  }
  
  alt_var <- Alt[["alt_var"]]
  occasion <- Alt[["occasion"]]
  dataZoneTrue <- Alt[["dataZoneTrue"]]  
  int <- Alt[["int"]]
  choice <- Alt[["choice"]]
  bCHeader <- Alt[["altChoiceUnits"]]
  
   if (!is.empty(gridVariablesInclude)) {
    bCHeader <- list(bCHeader, gridVariablesInclude)
  }
  if (!exists("newDumV")) {
    bcHeader <- bCHeader
  } else {
    newDumV <- newDumV
    bCHeader <- list(bCHeader, newDumV)
  }
  # 
  if (is.empty(indeVarsForModel)) {
    bCHeader <- list(bCHeader, indeVarsForModel = as.data.frame(rep(1, nrow(choice))))
    bColumnsWant <- ""
    bInterAct <- ""
  } else {
    if (any(indeVarsForModel %in% c("Miles * Miles", "Miles*Miles, Miles x Miles"), 
            ignore.case = TRUE)) {
      bCHeader = list(bCHeader, lapply(indeVarsForModel[-1], function(x) dataset[[x]][which(dataZoneTrue == 1)]))
    } else {
      bCHeader = list(bCHeader, lapply(indeVarsForModel, function(x) dataset[[x]][which(dataZoneTrue == 1)]))
    }
  }
  
  
  ################### Steps if alternative matrix come from grid file use loaded alternatives matrix
  if (alternativeMatrix == "grid matrix") {
    
    X <- Alt[['matrix']]
    
    altChoiceUnits <- Alt[["altChoiceUnits"]]
    allZP <- dataset[, grepl("AREA", colnames(dataset))]  # get zonal type variables                                                                                                                                                                     
    if (all(is.null(allZP)) || Alt[["alt_var"]] > length(allZP)) {
      v2 <- 0  # zonal centroid                                                                                                                                                                                                     
    } else {
      v2 <- allZP(Alt[["alt_var"]])  # <<- What is this doing?  Getting an index to find the name?                                                                                                                                                                                         
    }
    if (v2 == 0) {
      altToLocal1 <- ""
      altToLocal2 <- "Centroid of Zonal Assignment"
      
    } else {
      altToLocal1 <- ""
      altToLocal2 <- alt_var                                                                                                                                                                                                
      
    }
    altChoiceType <- "loaded grid data matrix"
    B <- Alt[["zoneRow"]]
    choiceZ <- ""
    # End Grid Matrix
  } else {
    
    # steps if alternatie matrix comes from loaded data (expectations)
    
    #####---Begin Alt Var--###
    if (any(grepl("zon", alt_var, ignore.case = T))) {
      # (alt_var==c('Zonal centroid')){ #(v1==0){ #Zonal centroid toXY1 <-
      # assignmentColumn()
      # #M.CentroidArcView(grindInfo['assignmentColumn',,][which(dataZoneTrue==1)])
      # #gridInfo.assignmentColumn(dataZoneTrue),:) # toXY1 <-
      # toXY1[,c('cent.long','cen.lat','ID')][which(is.data.frame(dataZoneTrue)==1)]
      toXY1 <- int[which(dataZoneTrue == 1), 2:3]
      
      altToLocal1 <- "Centroid of Zonal Assignment"
    } else {
      # Port (isfield(data,'isPort') && data(v1).isPort){ Data from dataframe
      if (any(grepl("Port", alt_var, ignore.case = TRUE) == T)) {
        if (is.data.frame(dataset)) {
          if (any(is.empty(dataset[[alt_var]]))) {
            stop("alt_var does not exist in datset")
          }
          
          toXYa <- data.frame(dataset[[alt_var]][which(dataZoneTrue == 1)])  #  data[[altToLocal1]]data(v1).dataColumn(dataZoneTrue,:)      #subset data to when dataZoneTrue==1                                                                                                                                                              
          
          colnames(toXYa) <- c(alt_var)
          # portLL <- data[[alt_var]].codeID[,2] # Extract lat long for selected port variable,
          # cell2mat(data(v1).codeID(:,2)) # convert cell array to an ordinary array
          temp <- data.frame(unique(dataset[[alt_var]]), tapply(dataset[[lon.dat]], dataset[[alt_var]], mean), 
                                                         tapply(dataset[[lat.dat]], dataset[[alt_var]], mean))
          colnames(temp) = c(alt_var, "LON", "LAT")
          toXY1 <- merge(toXYa, temp)  #portLL(toXYa,:) 
          toXY1 <- unique(toXY1)
          # Data from list
        } else {
          toXYa <- data.frame(dataset[["data"]][, , which(unlist(dataset[["data"]][, 1, ][3, ]) == alt_var)]$dataColumn[which(dataZoneTrue == 1)])  #data.frame(dataset[[alt_var]][which(dataZoneTrue==1)])#  data[[altToLocal1]]data(v1).dataColumn(dataZoneTrue,:)                                                                                                                                                              
          colnames(toXYa) <- c(alt_var)
          # portLL <- data[[alt_var]].codeID[,2] # Extract lat long for selected port variable,
          # cell2mat(data(v1).codeID(:,2)) # convert cell array to an ordinary array
          temp <- data.frame(unique(data.frame(dataset[["data"]][, , which(unlist(dataset[["data"]][, 1, ][3, ]) == alt_var)]$dataColumn)), 
                             tapply(data.frame(dataset[["data"]][, , which(unlist(dataset[["data"]][, 1, ][3, ]) == "LonLat_START")]$dataColumn)[, 1], 
                                    data.frame(dataset[["data"]][, , which(unlist(dataset[["data"]][, 1, ][3, ]) == alt_var)]$dataColumn), mean), 
                             tapply(data.frame(dataset[["data"]][, , which(unlist(dataset[["data"]][, 1, ][3, ]) == "LonLat_START")]$dataColumn)[, 2], 
                                    data.frame(dataset[["data"]][, , which(unlist(dataset[["data"]][, 1, ][3, ]) == alt_var)]$dataColumn), mean))
          colnames(temp) = c(alt_var, "LON", "LAT")
          toXY1 <- merge(toXYa, temp)  #portLL(toXYa,:)  
        }
        # Lat/Lon
      } else {
        # Data is from a dataframe or matrix
        if (is.data.frame(dataset)) {
          if (length(alt_var) > 1) {
            toXY1 <- data.frame(dataset[[alt_var[1]]][which(dataZoneTrue == 1)], 
                                dataset[[alt_var[2]]][which(dataZoneTrue == 1)])
          } else {
            toXY1 <- data.frame(dataset[[lon.dat]][which(dataZoneTrue == 1)], 
                                dataset[[lat.dat]][which(dataZoneTrue == 1)])
          }
          # Data from a list
        } else {
          toXY1 <- data.frame(dataset[["data"]][, , which(unlist(dataset[["data"]][, 1, ][3, ]) == alt_var)]$dataColumn)[which(dataZoneTrue == 1), ]  #data.frame(dataset[[alt_var]][which(dataZoneTrue==1)])#  data[[altToLocal1]]data(v1).dataColumn(dataZoneTrue,:)      #subset data to when dataZoneTrue==1                                                                                                                                                              
        }
      }
      altToLocal1 <- alt_var
    }
    ###--End Alt Var---###  
    ####---Begin Occasion Var--##
    if (any(grepl("zon|cent", occasion, ignore.case = T))) {
      # (v2==0){ #Zonal centroid [B,I,choiceZ] <-
      # unique(gridInfo.assignmentColumn(dataZoneTrue))#
      B <- int[int$ZoneID %in% unique(choice[which(dataZoneTrue == 1), ]), 1]
      choiceZ <- match(int$ZoneID[which(dataZoneTrue == 1)], unique(int$ZoneID[which(dataZoneTrue == 1)]))
      
      centersZone <- int[int$ZoneID %in% unique(choice[which(dataZoneTrue == 1), ]), 2:3]  #M.CentroidArcView[B,] #Lat and Long                                                                                                                                                                              
      altToLocal2 <- "Centroid of Zonal Assignment"
    } else {
      if (is.data.frame(dataset)) {
        if (length(occasion) < 2) {
          stop("Define both lat and long in occasion variable.")
        }
        
        if (any(is.empty(dataset[[occasion[1]]]))) {
          stop("occasion does not exist in datset")
        }
        if (any(is.empty(dataset[[occasion[2]]]))) {
          stop("occasion does not exist in datset")
        }
        toXY2 <- data.frame(dataset[[occasion[1]]][which(dataZoneTrue == 1)], 
                            dataset[[occasion[2]]][which(dataZoneTrue == 1)])
      } else {
        toXY2 <- dataset[[occasion]][which(dataZoneTrue == 1)]  # MUST be a LAT/LONG data(v2).dataColumn(dataZoneTrue,:)                                                                                                                                                                        
      }
      # [Bb,I,choiceZ] <-
      # unique(cbind(gridInfo['assignmentColumn',,][which(dataZoneTrue==1)],toXY),'rows')
      # #unique([gridInfo.assignmentColumn(dataZoneTrue),toXY],'rows')#
      Bb <- unique(cbind(data.frame(choice[which(dataZoneTrue == 1), ]), data.frame(toXY2)))
      B <- Bb[, 1]  #Assignment column                                                                                                                                                                                                     
      centersZone <- Bb[, 2:3]  #Latitude aned Longitude                                                                                                                                                                                        
      altToLocal2 <- occasion
      
    }
  }
  ##-End Occasion Var--##
  # End From loaded data
  
  ##------ Generate Distance Matrix ----##       
  # Test for potential issues with data
  if (any(do.call(cbind, lapply(toXY1, is.nan)))) {
    stop(paste("NaN found in ", altToLocal1, ". Design file aborted."))
  }
  if (any(do.call(cbind, lapply(centersZone, is.nan)))) {
    stop(paste("NaN found in ", altToLocal2, ". Design file aborted."))
  }
  # Generate distances using distm function [distAll,!,!] <-
  # #m_idist(toXY1[q,1],toXY1[q,2], centersZone[,1], centersZone[,2])
  if (dim(toXY1)[2] > 2) {
    distMatrix <- geosphere::distm(toXY1[, 2:3], centersZone[, 1:2])
  } else {
    distMatrix <- geosphere::distm(toXY1[, 1:2], centersZone[, 1:2])
  }
  
  altChoiceType <- "distance"
  
  if (Alt[["altChoiceUnits"]] %in% c("meters", "M")) {
    X <- distMatrix
    altChoiceUnits <- "meters"
  } else if (Alt[["altChoiceUnits"]] %in% c("kilometers", "KM")) {
    X <- distMatrix/1000
    altChoiceUnits <- "kilometers"
  } else if (Alt[["altChoiceUnits"]] == "miles") {
    X <- distMatrix * 0.000621371192237334  #meter* miles/meter                                                                                                                                                        
    altChoiceUnits <- "miles"
  }
  
  
  
  
  ### ---- add special terms: ----### add only for EPM model
  catch <- dataset[which(dataZoneTrue == 1), catchID]
  r <- nchar(sub("\\.[0-9]+", "", max(catch, na.rm = T)))
  yscale <- 10^(r - 1)
  
  
  # some models need vessel ID } !isfield(data,'vesselID')){
  if (is.null(vesselID) & any(grepl("vesselID", names(dataset), ignore.case = T)) == F) {
    warning("ModelInputData matrix generated but Vessel ID not found. Rerun if Vessel ID is needed for models.")
  } else {
    if (!is.null(vesselID)) {
      vesselID <- dataset[which(dataZoneTrue == 1), vesselID]
    } else {
      vesselID <- dataset[which(dataZoneTrue == 1), "vesselID"]  #data([data.vesselID]).dataColumn(dataZoneTrue)   
    }
  }
  
  # Some models need price data
  if (!is.null(priceCol)) {
    epmDefaultPrice <- dataset[which(dataZoneTrue == 1), priceCol]
  } else {
    epmDefaultPrice <- ""
  }
  
  # scales zonal
  r <- nchar(sub("\\.[0-9]+", "", max(max(X, na.rm = T), na.rm = T)))                                                                                                                                                            
  mscale <- 10^(r - 1)
  
  # scales data r in
  # regexp(arrayfun(@num2str,nanmax(dataPerZone),'UniformOutput',false),'\\.','split')){){
  # dscale <- cellfun(@(x) 10^length(x{1}-1),r)
  dscale = 1
  
  ### -- Create output list --- ###
  modelInputData <- list(catch = catch, 
                          choice = choice[which(dataZoneTrue == 1), ], 
                          scales = c(catch = yscale, zonal = mscale, data = dscale), 
                          zonalChoices = X, 
                          instances = dim(X)[1], 
                          alts = dim(X)[2], 
                          epmDefaultPrice = epmDefaultPrice, 
                          dataZoneTrue <- dataZoneTrue, 
                          typeOfNecessary = Alt[["zoneType"]], 
                          altChoiceType = altChoiceType, 
                          altChoiceUnits = altChoiceUnits, 
                          altToLocal1 = altToLocal1, 
                          altToLocal2 = altToLocal2, 
                          bCHeader = bCHeader, 
                          bColumnsWant = bColumnsWant, 
                          bInterAct = bInterAct, 
                          gridVaryingVariables = ExpectedCatch)
  
  fishset_db <- DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite")
  single_sql <- paste0(project, 'modelinputdata', format(Sys.Date(), format="%Y%m%d"))
  DBI::dbExecute(fishset_db, paste("CREATE TABLE IF NOT EXISTS", single_sql, "(ModelInputData MODELINPUTDATA)"))
  DBI::dbExecute(fishset_db, paste("INSERT INTO", single_sql, "VALUES (:ModelInputData)"), 
                 params = list(ModelInputData = list(serialize(modelInputData, NULL))))
  DBI::dbDisconnect(fishset_db)
  
  #write(layout.json.ed(trace, "make_model_design", dataset = deparse(substitute(dataset)), x = "",
  #                      msg = paste("catchID:", catchID, ", indeVarsForModel:", indeVarsForModel,  ", gridVariablesInclude:",
  #                                         gridVariablesInclude, ", alternativeMatrix:", alternativeMatrix, ", lon.dat:", 
  #                                        lon.dat, ", lat.dat:", lat.dat, ", priceCol:", priceCol, ", vesselID:", vesselID)), 
  #      paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""), append = T)
  
  
  if(!exists('logbody')) { 
    logging_code()
  } 
  make_model_design_function <- list()
  make_model_design_function$functionID <- 'make_model_design'
  make_model_design_function$args <- c(deparse(substitute(dataset)), deparse(substitute(dataindex)), catchID, alternativeMatrix, lon.dat, lat.dat)
  make_model_design_function$kwargs <- list('indeVarsForModel'=indeVarsForModel, 'gridVariablesInclude'=gridVariablesInclude, 'priceCol'=priceCol, 'vesselID'=vesselID)
  functionBodyout$function_calls[[length(functionBodyout$function_calls)+1]] <- (make_model_design_function)
  logbody$fishset_run <- list(infoBodyout, functionBodyout)
  write(jsonlite::toJSON(logbody, pretty = TRUE, auto_unbox = TRUE),paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""))
  assign("functionBodyout", value = functionBodyout, pos = 1)
 
   
   assign('modelInputData', modelInputData, pos=1)
}
