#'  Make model design
#'
#' @param dat Main data frame over which to apply function. Table in fishset_db database should contain the string `MainDataTable`.
#' @param catchID  Name of variable that contains catch data such as 'HAUL'
#' @param alternativeMatrix Whether the alternative choice matrix should come from 'loaded data' or 'gridded data'
#' @param lon.dat longitude Column containing longitude data
#' @param lat.dat latitude Column containing latitude data
#' @param likelihood Name of likelihood function. Current choices are logit_c, logit_avgcat, epm_normal, epm_weibull, epm_ognormal.
#' @param vars1 List varialbes using `c()`. See Detail section for how to specify for each likelihood function. 
#' @param vars2 List varialbes using `c()`. See Detail section for how to specify for each likelihood function. 
#' @param priceCol NULL If required, specify which variable contains price data.
# @param vesselID NULL If required, specify which variable defines individual vessels.
#' @param project name. name of project. For name of output table saved in sql database
#' @importFrom geosphere distm
#' @importFrom DBI dbGetQuery dbExecute dbListTables
#' @export make_model_design
#' @details Functions returns model design matrix. Calls the Alternative Choice matrix from `create_alternative_choice` function which defines alternative fishing options
#' and the expected catch from the `create_expectations` function. The distance from the starting point to alternative choices is calculated.
#' @return 
#' vars1 details \cr
#' \tabular{rlll}{
#' logit_c: independent variables that get interacted widh distance.
#' logit_avgcat:
#' epm_normal
#' epm_lognormal
#' epm_weibull
#' }
#' 
#' ' vars2 details \cr
#' \tabular{rlll}{
#' logit_c: 
#' logit_avgcat:
#' epm_normal
#' epm_lognormal
#' epm_weibull
#' }

#'   Model design matrix containing \cr
#'   \tabular{rlll}{
#'     likelihood: \tab Name of likelihood function\cr
#'     choice: \tab Data corresponding to actual zonal choice\cr 
#'     catch: \tab Data corresponding to actual zonal catch\cr 
#'     scales: \tab Scale vectors to put catch data, zonal data, and other data on same scale\cr 
#'     distance: \tab Data corresponding to distance\cr
#'     instances: \tab Number of observations\cr 
#'     alt: \tab Number of alternative zones\cr
#'     epmDefaultPrice: \tab Price data\cr 
#'     dataZoneTrue: \tab Vector of 0/1 indicating whether the data from that zone is to be included.\cr 
#'     numOfNecessary: \tab Minimum number of hauls/trips per zone for data from that zone to be included\cr
#'     typeOfNecessary: \tab Haul or trip\cr
#'     altChoiceType: \tab Function choice. Set to distance\cr
#'     altChoiceUnits: \tab Units of distance\cr
#'     altToLocal: \tab Identifies how to find lat/lon for starting point. Can be zonal centroid, port, etc\cr
#'     altToLocal2: \tab Identifies how to find lat/lon for alternative choices such as 'Centroid of Zonal Assignment'\cr 
#'     bCHeader: \tab Variables to include in the model that do not vary by zone. Includes independent variables and interactions\cr
#'     gridVaryingVariables: \tab Variables to include in the model that do vary by zone such as expected catch (from \code{\link{create_expectations}} function)
#'  }
  #' @examples
#' \dontrun{
#' make_model_design(MainDataTable, catchID = 'HAUL', alternativeMatrix = "loadedData", 
#'                   'LonLat_START_LON', 'LonLat_START_LAT', project = 'pcod')
#' }


make_model_design <- function(dat, catchID, alternativeMatrix = c("loadedData", "griddedData"), lon.dat, lat.dat, project, 
                               likelihood= NULL, vars1 = NULL, vars2 = NULL, priceCol = NULL) {
  
  fishset_db <- DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite")
  if(is.character(dat)==TRUE){
    if(is.null(dat)==TRUE | table_exists(dat)==FALSE){
      print(DBI::dbListTables(fishset_db))
      stop(paste(dat, 'not defined or does not exist. Consider using one of the tables listed above that exist in the database.'))
    } else {
      dataset <- table_view(dat)
    }
  } else {
    dataset <- dat  
  }
 
  indeVarsForModel = vars1
  gridVariablesInclude=vars2
  
  if (!exists("Alt")) {
    if (!exists('AltMatrixName')) {
      Alt <- unserialize(DBI::dbGetQuery(fishset_db, paste0("SELECT AlternativeMatrix FROM ", project, "altmatrix LIMIT 1"))$AlternativeMatrix[[1]])
      if (!exists("Alt")) {
        stop("Alternative Choice Matrix does not exist. Please run the createAlternativeChoice() function.")
      }
    }
  }
  
  if(table_exists(paste0(project,'ExpectedCatch'))){
  ExpectedCatch <- unserialize(DBI::dbGetQuery(fishset_db, paste0("SELECT data FROM ", project, "ExpectedCatch LIMIT 1"))$data[[1]])
  }
  if (!exists("ExpectedCatch")) {
    ExpectedCatch=''
    warning("Expected Catch Matrix does not exist. Please run the create_expectations function if expected catch will be included in the model.")
  }  
  
  alt_var <- Alt[["alt_var"]]
  occasion <- Alt[["occasion"]]
  dataZoneTrue <- Alt[["dataZoneTrue"]]  
  int <- Alt[["int"]]
  choice <- Alt[["choice"]]
  units <- Alt[["altChoiceUnits"]]
  
   if (FishSET:::is_empty(gridVariablesInclude)) {
    gridVariablesInclude = as.data.frame(matrix(1, nrow=nrow(choice), ncol=max(as.numeric(as.factor(unlist(choice))))))
   } else {
    gridVariablesInclude
  }
  if (!exists("newDumV")) {
    newDumV <- 1
  } else {
    newDumV <- newDumV
    #bCHeader <- list(bCHeader, newDumV)
  }
  # 
  if (FishSET:::is_empty(indeVarsForModel)) {
    bCHeader <- list(units=units, gridVariablesInclude=gridVariablesInclude, newDumV=newDumV, indeVarsForModel = as.data.frame(rep(1, nrow(choice))))
    bColumnsWant <- ""
    bInterAct <- ""
  } else {
    if (any(indeVarsForModel %in% c("Miles * Miles", "Miles*Miles, Miles x Miles"), 
            ignore.case = TRUE)) {
      bCHeader = list(units=units, gridVariablesInclude=gridVariablesInclude, newDumV=newDumV, lapply(indeVarsForModel[-1], function(x) dataset[[x]][which(dataZoneTrue == 1)]))
    } else {
      bCHeader = list(units=units, gridVariablesInclude=gridVariablesInclude, newDumV=newDumV, lapply(indeVarsForModel, function(x) dataset[[x]][which(dataZoneTrue == 1)]))
    }
  }
  
  
  ################### Steps if alternative matrix come from grid file use loaded alternatives matrix
  if (alternativeMatrix == "griddedData") {
    
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
          if (any(FishSET:::is_empty(dataset[[alt_var]]))) {
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
        
        if (any(FishSET:::is_empty(dataset[[occasion[1]]]))) {
          stop("occasion does not exist in datset")
        }
        if (any(FishSET:::is_empty(dataset[[occasion[2]]]))) {
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
 # if (is.null(vesselID) & any(grepl("vesselID", names(dataset), ignore.case = T)) == F) {
 #   warning("ModelInputData matrix generated but Vessel ID not found. Rerun if Vessel ID is needed for models.")
 # } else {
 #   if (!is.null(vesselID)) {
 #     vesselID <- dataset[which(dataZoneTrue == 1), vesselID]
 #   } else {
 #     vesselID <- dataset[which(dataZoneTrue == 1), "vesselID"]  #data([data.vesselID]).dataColumn(dataZoneTrue)   
 #   }
 # }
  
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
  modelInputData <- list(likelihood=likelihood,
                          catch = catch, 
                          choice = choice[which(dataZoneTrue == 1), ], 
                          scales = c(catch = yscale, zonal = mscale, data = dscale), 
                          distance = X, 
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
                          #bColumnsWant = bColumnsWant, 
                         # bInterAct = bInterAct, 
                          gridVaryingVariables = ExpectedCatch)
  
  
  fishset_db <- DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite")
  single_sql <- paste0(project, 'modelinputdata')
  date_sql <- paste0(project, 'modelinputdata', format(Sys.Date(), format="%Y%m%d"))
  if(table_exists(single_sql)){
    modelInputData <- table_view()
    single_sql[[length(single_sql)+1]] <- list(name=expname,errorExplain = errorExplain, OutLogit = OutLogit, optoutput = optoutput, 
                                           seoutmat2 = seoutmat2, MCM = MCM, H1 = H1, choice.table=choice.table)
  } else {
    modelInputData <-  list()
    modelInputData[[length(modelInputData)+1]] <- list(name=expname,errorExplain = errorExplain, OutLogit = OutLogit, optoutput = optoutput, 
                                           seoutmat2 = seoutmat2, MCM = MCM, H1 = H1, choice.table=choice.table)
  }

  single_sql <- paste0(project, 'modelinputdata')
  if(table_exists(single_sql)){
    table_remove(single_sql)
  } 
  if(table_exists(date_sql)){
    table_remove(date_sql)
  }
  
  DBI::dbExecute(fishset_db, paste("CREATE TABLE IF NOT EXISTS", single_sql, "(ModelInputData MODELINPUTDATA)"))
  DBI::dbExecute(fishset_db, paste("INSERT INTO", single_sql, "VALUES (:ModelInputData)"), 
                 params = list(ModelInputData = list(serialize(modelInputData, NULL))))
  DBI::dbExecute(fishset_db, paste("CREATE TABLE IF NOT EXISTS", date_sql, "(ModelInputData MODELINPUTDATA)"))
  DBI::dbExecute(fishset_db, paste("INSERT INTO", date_sql, "VALUES (:ModelInputData)"), 
                 params = list(ModelInputData = list(serialize(modelInputData, NULL))))
  DBI::dbDisconnect(fishset_db)
  
 
  if(!exists('logbody')) { 
    logbody <- list()
    infoBodyout <- list()
    functionBodyout <- list()
    infobody <- list()
    
    infobody$rundate <- Sys.Date()
    infoBodyout$info <- list(infobody)
    
    functionBodyout$function_calls <- list()
    
    logbody$fishset_run <- list(infoBodyout, functionBodyout)
  } 
  make_model_design_function <- list()
  make_model_design_function$functionID <- 'make_model_design'
  make_model_design_function$args <- c(deparse(substitute(dat)), catchID, alternativeMatrix, lon.dat, lat.dat, project)
  make_model_design_function$kwargs <- list('indeVarsForModel'=indeVarsForModel, 'gridVariablesInclude'=gridVariablesInclude, 'priceCol'=priceCol)
  make_model_design_function$output <- c('')
  functionBodyout$function_calls[[length(functionBodyout$function_calls)+1]] <- (make_model_design_function)
  logbody$fishset_run <- list(infoBodyout, functionBodyout)
  write(jsonlite::toJSON(logbody, pretty = TRUE, auto_unbox = TRUE),paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""))
  assign("functionBodyout", value = functionBodyout, pos = 1)
 
   
   assign('modelInputData', modelInputData, pos=1)
}
