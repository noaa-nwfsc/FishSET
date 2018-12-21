#'  Make model design
#'
#' @param dataset dataframe or matrix
#' @param indeVarsForModel Independent variables to include in the model
#' @param gridVariablesInclude Variables from gridded dataset to include in the model
#' @param catchID  Name of variable that containts catch data such as "HAUL"
#' @param alternativeMatrix Whether the alternative choice matrix should come from 'loaded data' or 'gridded data'
#' @param lon.dat Variable containing longitude data
#' @param lat.dat Variable containing latitude data
#' @param priceCol=NULL If required, specify which variable contains price data
#' @param vesselID=NULL If required, specify which varible defines vessel
#' @return  ModelInputData a list containing information on alternative choice
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
#'    Depends on geospheres
# @examples 
 
      
#require(geosphere) #for distance calculations


makeModelDesign  <- function(dataset, catchID='HAUL', indeVarsForModel, gridVariablesInclude='', alternativeMatrix=c('loaded data','gridded data'), 
                             gridVaryingVariables='', lon.dat, lat.dat, priceCol=NULL, vesselID=NULL){                                                                                                                                                                                         
      if(!exists('Alt')){
           if(!exists(AltMatrixName)) {
           stop('Alternative Choice Matrix does not exist. Please run the createAlternativeChoice() function.')
      }}

     alt_var <- Alt[['alt_var']]
     occasion <- Alt[['occasion']]
     dataZoneTrue <- Alt[['dataZoneTrue']] # used for catch and other variables
     int <- Alt[['int']]
     choice <- Alt[['choice']]
     
      bCHeader <- Alt[['altChoiceUnits']]                                                                                                                                                                                                
                                                                                                                                                                                                                                          
      # add header names from grid varying variables                                                                                                                                                                                      
      # this is available when you have a grid file                                                                                                                                                                   
      if (!is.empty(gridVariablesInclude)){                                                                                                                                                                                                
           bCHeader <- as.list(bCHeader, gridVariablesInclude)   #'     <-- Finish this part, Expectation.name, paste('dummy_', Expectation.name), Interaction.terms                                                                                                                                                                  
                                                                                        
        }                                                                                                                                                                                                                                   
      #                                                                                                                                                                                                                                   
      if (is.empty(indeVarsForModel)){                                                                                                                                                                                               
          bColumnsWant <- ''                                                                                                                                                                                                              
          bInterAct <- ''                                                                                                                                                                                                                 
      } else {                                                                                                                                                                                                                            
                                                                                                                                                                                                                                          
          if (any(indeVarsForModel %in% c('Miles * Miles', 'Miles*Miles, Miles x Miles'), ignore.case=TRUE)){
              bCHeader=list(bCHeader,  as.list(indevarsForModel[-1]))                                                                                                                                                
         }}
                                                                                                                                                                                                                                          

      ###################       
      #Steps if alternative matrix come from grid file
      if (alternativeMatrix=='grid matrix'){  # use loaded alternatives matrix                                                                                                                                                                         
                                                                                                                                                                                                                                          
           X <- Alt$matrix                                                                                                                                                                                                                
                                                                                                                                                                                                                                          
          altChoiceUnits <- Alt[['altChoiceUnits']]                                                                                                                                                                                         
          allZP <- dataset[,grepl('AREA',colnames(dataset))]# get zonal type variables                                                                                                                                                                     
          if (all(is.null(allZP)) || Alt[['alt_var']]>length(allZP)){                                                                                                                                                                               
              v2 <- 0# zonal centroid                                                                                                                                                                                                     
          } else {                                                                                                                                                                                                                        
              v2 <- allZP(Alt[['alt_var']])        # <<- What is this doing?  Getting an index to find the name?                                                                                                                                                                                         
          }                                                                                                                                                                                                                               
          if (v2==0){                                                                                                                                                                                                                     
              altToLocal1 <- ''                                                                                                                                                                                                        
              altToLocal2 <- 'Centroid of Zonal Assignment'                                                                                                                                                                             
                                                                                                                                                                                    
          } else {                                                                                                                                                                                                                        
              altToLocal1 <- ''                                                                                                                                                                                                        
              altToLocal2 <- ''# ---> HERE dataset(v2).name                                                                                                                                                                                                
                                                                                                                                                                                                                                          
          }                                                                                                                                                                                                                               
          altChoiceType <- 'loaded grid data matrix'                                                                                                                                                                                      
          B <- Alt[['zoneRow']]                                                                                                                                                                                                                
          choiceZ <- ''                                                                                                                                                                                                                   
            #End Grid Matrix                                                                                                                                                                                                                              
      } else {

#steps if alternatie matrix comes from loaded data (expectations)
    
      #####---Begin Alt Var--###
                  if (grepl('zon', alt_var, ignore.case=T)) { #(alt_var==c('Zonal centroid')){ #(v1==0){ #Zonal centroid                                                                                                                                                                                                             
                       #toXY1 <- assignmentColumn()  #M.CentroidArcView(grindInfo['assignmentColumn',,][which(dataZoneTrue==1)])  #gridInfo.assignmentColumn(dataZoneTrue),:)      #                                                                                                                                         
                       #toXY1 <- toXY1[,c('cent.long','cen.lat','ID')][which(is.data.frame(dataZoneTrue)==1)]
                       toXY1 <- int[which(dataZoneTrue==1),2:3]
                                      
               altToLocal1 <- 'Centroid of Zonal Assignment'                                                                                                                                                                      
                  } else { #Port  
                      if(grepl('Port', alt_var, ignore.case=TRUE) ==T) { # (isfield(data,'isPort') && data(v1).isPort){
                           #Data from dataframe
                         if(is.data.frame(dataset)){
                              if(any(is.empty(dataset[[alt_var]]))){
                                   stop('alt_var does not exist in datset')
                              }
                              
                              toXYa <- data.frame(dataset[[alt_var]][which(dataZoneTrue==1)])#  data[[altToLocal1]]data(v1).dataColumn(dataZoneTrue,:)      #subset data to when dataZoneTrue==1                                                                                                                                                              
                              
                              colnames(toXYa) <- c(alt_var)
                                #portLL <-   data[[alt_var]].codeID[,2] # Extract lat long for selected port variable, cell2mat(data(v1).codeID(:,2))        # convert cell array to an ordinary array
                              temp <- data.frame(unique(dataset[[alt_var]]), tapply(dataset[[lon.dat]], dataset[[alt_var]],  mean),
                                            tapply(dataset[[lat.dat]], dataset[[alt_var]],  mean))
                              colnames(temp)=c(alt_var, 'LON','LAT')
                              toXY1 <-merge(toXYa, temp)   #portLL(toXYa,:) 
                              toXY1 <- unique(toXY1)
                              #Data from list
                          } else { 
                               toXYa <- data.frame(dataset[['data']][,,which(unlist(dataset[['data']][,1,][3,])==alt_var)]$dataColumn[which(dataZoneTrue==1)]) #data.frame(dataset[[alt_var]][which(dataZoneTrue==1)])#  data[[altToLocal1]]data(v1).dataColumn(dataZoneTrue,:)                                                                                                                                                              
                               colnames(toXYa) <- c(alt_var)
                               #portLL <-   data[[alt_var]].codeID[,2] # Extract lat long for selected port variable, cell2mat(data(v1).codeID(:,2))        # convert cell array to an ordinary array
                               temp <- data.frame(unique(data.frame(dataset[['data']][,,which(unlist(dataset[['data']][,1,][3,])==alt_var)]$dataColumn)), 
                                                  tapply(data.frame(dataset[['data']][,,which(unlist(dataset[['data']][,1,][3,])=='LonLat_START')]$dataColumn)[,1], data.frame(dataset[['data']][,,which(unlist(out$data[,1,][3,])==alt_var)]$dataColumn),  mean),
                                                 tapply(data.frame(dataset[['data']][,,which(unlist(dataset[['data']][,1,][3,])=='LonLat_START')]$dataColumn)[,2], data.frame(dataset[['data']][,,which(unlist(out$data[,1,][3,])==alt_var)]$dataColumn),  mean))
                               colnames(temp)=c(alt_var, 'LON','LAT')
                               toXY1 <-merge(toXYa, temp)   #portLL(toXYa,:)  
                          }
                          #Lat/Lon
                      } else {   
                           #Data is from a dataframe or matrix
                           if(is.data.frame(dataset)){
                               if(length(alt_var)>1){
                              toXY1 <- data.frame(dataset[[alt_var[1]]][which(dataZoneTrue==1)],#Lon
                                              dataset[[alt_var[2]]][which(dataZoneTrue==1)])  #Lat
                                } else {
                                     toXY1 <- data.frame(dataset[[lon.dat]][which(dataZoneTrue==1)],#Lon
                                                         dataset[[lat.dat]][which(dataZoneTrue==1)])  #Lat
                                }
                                #Data from a list
                           } else {
                              toXY1 <- data.frame(dataset[['data']][,,which(unlist(dataset[['data']][,1,][3,])==alt_var)]$dataColumn)[which(dataZoneTrue==1),] #data.frame(dataset[[alt_var]][which(dataZoneTrue==1)])#  data[[altToLocal1]]data(v1).dataColumn(dataZoneTrue,:)      #subset data to when dataZoneTrue==1                                                                                                                                                              
                           }
                      }                                                                                                                                                                                                                   
                      altToLocal1 <- alt_var                                                                                                                                                                                        
                  } 
               ###--End Alt Var---###  
               ####---Begin Occasion Var--##
                  if( any(grepl( 'zon|cent', occasion, ignore.case = T))) {  #(v2==0){    #Zonal centroid                                                                                                                                                                                                         
                       #[B,I,choiceZ] <- unique(gridInfo.assignmentColumn(dataZoneTrue))#
                       B <- unique(int$ZoneID[which(dataZoneTrue==1)])
                       choiceZ <- match(int$ZoneID[which(dataZoneTrue==1)], 
                                        unique(int$ZoneID[which(dataZoneTrue==1)]))
                       
                      centersZone <- cbind(int$cent.lon, int$cent.lat) #M.CentroidArcView[B,] #Lat and Long                                                                                                                                                                              
                      altToLocal2 <- 'Centroid of Zonal Assignment'                                                                                                                                                                     
                  } else {                                                                                                                                                                                                                
                       if(is.data.frame(dataset)){
                            if(length(occasion)<2){
                                 stop('Define both lat and long in occasion variable.')
                            }
                            
                       if(any(is.empty(dataset[[occasion[1]]]))){
                                      stop('occasion does not exist in datset')
                       }
                            if(any(is.empty(dataset[[occasion[2]]]))){
                                 stop('occasion does not exist in datset')
                            }
                       toXY2 <- data.frame(dataset[[occasion[1]]][which(dataZoneTrue==1)],
                                           dataset[[occasion[2]]][which(dataZoneTrue==1)]) 
                       } else {
                       toXY2 <- dataset[[occasion]][which(dataZoneTrue==1)]# MUST be a LAT/LONG data(v2).dataColumn(dataZoneTrue,:)                                                                                                                                                                        
                       }  
                       #[Bb,I,choiceZ] <- unique(cbind(gridInfo['assignmentColumn',,][which(dataZoneTrue==1)],toXY),'rows') #unique([gridInfo.assignmentColumn(dataZoneTrue),toXY],'rows')#                                                                                                                                   
                      Bb <- unique(cbind(choice[which(dataZoneTrue==1)],data.frame(toXY2)))
                      B <- Bb[,1]    #Assignment column                                                                                                                                                                                                     
                      centersZone <- Bb[,2:3]    #Latitude aned Longitude                                                                                                                                                                                        
                      altToLocal2 <- occasion                                                                                                                                                                                        
                                                                                                                                                                                                                                          
                  }                                                                                                                                                                                                                       
          } 
      ##-End Occasion Var--##
#End From loaded data                                                                                                                                                                                                                        
                    
##------ Generate Distance Matrix ----##       
         #Test for   potential issues with data         
                  if (any(do.call(cbind,lapply(toXY1, is.nan)))) {                                                                                                                                                                                            
                      stop(paste('NaN found in ', altToLocal1, '. Design file aborted.'))                                                                                                                         
                  } 
                    if (any(do.call(cbind, lapply(centersZone, is.nan)))){                                                                                                                                                                               
                       stop(paste('NaN found in ', altToLocal2, '. Design file aborted.'))                                                                                                                         
                  }                                                                                                                                                                                                                     
          #Generate distances using distm function                                                                                                                                                                                                   
                     # [distAll,!,!] <- #m_idist(toXY1[q,1],toXY1[q,2], centersZone[,1], centersZone[,2])  
                    if(dim(toXY1)[2]>2){
                         distMatrix <- distm(toXY1[,2:3], centersZone[,1:2])   
                    } else {
                      distMatrix <- distm(toXY1[,1:2], centersZone[,1:2])                                                                                                                                                                                   
                    }                                                                                                                                                                                                                     
                                                                                                                                                                                                                                          
                  altChoiceType <- 'distance'
                                                                                                                                                                                                                                          
                  if (Alt[['altChoiceUnits']] %in% c('meters', 'M')) {
                          X <- distMatrix                                                                                                                                                                                                 
                          altChoiceUnits <- 'meters'                                                                                                                                                                                      
                      } else if(Alt[['altChoiceUnits']] %in% c('kilometers', 'KM')) {                                                                                                                                                                                               
                          X <- distMatrix/1000                                                                                                                                                                                            
                          altChoiceUnits <- 'kilometers'                                                                                                                                                                                  
                      } else if(Alt[['altChoiceUnits']] == 'miles'){                                                                                                                                                                                                        
                          X <- distMatrix*0.000621371192237334 #meter* miles/meter                                                                                                                                                        
                          altChoiceUnits <- 'miles'                                                                                                                                                                                       
                      }
        
                                                                                                                                                                                                                                           
                                                                                                                                                                                                                                  
                                                                                                                                                                                                                                          
### ---- add special terms: ----###                                                                                                                                                                                                               
#add only for EPM model                                                                                                                                                                                                             
     catch <- dataset[which(dataZoneTrue==1), catchID]                                                                                                                                                                                                                           
      r <- nchar(sub('\\.[0-9]+', '', max(catch,na.rm=T),na.rm=T)) #regexp(num2str(max(modelInputData.catch)),'\\.','split')                                                                                                                                                                       
      yscale <- 10^(r-1)                                                                                                                                                                                                       
                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                          
# some models need vessel ID                                                                                                                                                                                                        
      if (is.null(vesselID) & any(grepl('vesselID', names(dataset), ignore.case=T))==F){ #} !isfield(data,'vesselID')){                                                                                                                                                                                                    
            warning('ModelInputData matrix generated but Vessel ID not found. Rerun if Vessel ID is needed for models.')                                                                                                                                                                                              
      } else {  
           if(!is.null(vesselID)){
          vesselID <- dataset[which(dataZoneTrue==1), vesselID]
           } else {
          vesselID <- dataset[which(dataZoneTrue==1),'vesselID'] #data([data.vesselID]).dataColumn(dataZoneTrue)   
           }
      }                                                                                                                                                                                                                                   

#Some models need price data                                                                                                                                                                                                                                          
      if (!is.null(priceCol)){                                                                                                                                                                                                            
           epmDefaultPrice <- dataset[which(dataZoneTrue==1), priceCol]                                                                                                                                                                     
      } else {                                                                                                                                                                                                                            
          epmDefaultPrice <- ''                                                                                                                                                                                                           
      }      
      
      #scales zonal
      r <- nchar(sub('\\.[0-9]+', '', max(max(zonalChoices,na.rm=T),na.rm=T))) #regexp(num2str(max(max(modelInputData.zonalChoices))),'\\.','split')                                                                                                                                                           
      mscale <- 10^(r-1)                                                                                                                                                                                                       

      #scales data
      #r in regexp(arrayfun(@num2str,nanmax(dataPerZone),'UniformOutput',false),'\\.','split')){){
#      dscale <- cellfun(@(x) 10^length(x{1}-1),r)                                                                                                                                                                     
       dscale = 1

### -- Create output list --- ###       
      modelInputData <<- list(
          catch=catch,
          choice=choice[which(dataZoneTrue==1)],
          scales= c(catch=yscale, zonal=mscale, data=dscale),      
          zonalChoices = X, # area choice from data
          instances =  dim(X)[1],
          alts = dim(X)[2], #size of X
          epmDefaultPrice = epmDefaultPrice,                                                                                                                                                                                   
          dataZoneTrue <- dataZoneTrue,
#         combineData <- [data(numData).dataColumn],# make a matrix of data                                                                                                                                                                    
#         dataPerZone <- combineData(modelInputData.dataZoneTrue,:),# get data from matrix only for necessary zones, as choosen by the user                                                                                                    
          numOfNecessary = Alt[['numOfNecessary']],  # # of hauls/trips per zone                                                                                                                                                      
          typeOfNecessary = Alt[['zoneType']],  #haul or trip
#         dataFilename <- getappdata(hfig,'filename')
          altChoiceType = altChoiceType, # fucntion choice                                                                                                                                                                      
          altChoiceUnits = altChoiceUnits, # units of X                                                                                                                                                                         
          altToLocal1 = altToLocal1,                                                                                                                                                                                        
          altToLocal2 = altToLocal2,
          bCHeader =  bCHeader,                                                                                                                                                                                               
          bColumnsWant = bColumnsWant ,                                                                                                                                                                                        
          bInterAct = bInterAct,                                                                                                                                                                                               
          msq =  msq,     
#         gridID <- B# map choice assignment will need for backtracking                                                                                                                                                        
#         gridChoice <- gridInfo.mapChoice                                                                                                                                                                                     
#         gridName <- gridInfo.name
      #grid varrying variables                                                                                                                                                                                                            
          gridVaryingVariables = gridVaryingVariables,
          created = Sys.time()                                                                                                                                                                        
#      fileInfo <- getappdata(hfig,'fileInfo')                                                                                                                                                                                             
 #     if (!isfield('tagName','fileInfo')){                                                                                                                                                                                                
                                                                                                                                                                                                                                          
#          [!, tagName, !] <- fileparts(fileInfo.fileName)                                                                                                                                                                                 
#      } else {                                                                                                                                                                                                                            
#          tagName <- fileInfo.tagName                                                                                                                                                                                                     
#      }                                                                                                                                                                                                                                   
#      modelInputData.name <-  [ 'design_' tagName '_' modelInputData.created]                                                                                                                                                             
#      }                                                                                                                                                                                                                                    
      )
      
#      dbWriteTable(testdb,'modelInputData', modelInputData)
      
#      write_lines(layout.json.ed(trace, 'createAlternativeChoice', dataset, x='', 
#                                 msg=paste('catchID:', catchID, 'indeVarsForModel:', indeVarsForModel, 
#                                           'gridVariablesInclude:', gridVariablesInclude, 'alternativeMatrix:', alternativeMatrix, 
#                                           'lon.dat:', lon.dat, 'lat.dat:', lat.dat, 'priceCol:', priceCol, 'vesselID:', vesselID)), 
#                  paste(getwd(),'/Logs/',Sys.Date(),'.json', sep=''), append=T )
      
}

######################add FILEINFO                                                                                                                                                                                                  
                                                                                                                                                                                                                                          
#      tempFileInfo.fileName <- [modelInputData.name '.mat']                                                                                                                                                                               
#      tempFileInfo.projectName <- fileInfo.projectName#FIX ME.. move from earlier                                                                                                                                                         
#      tempFileInfo.authorName <- fileInfo.authorName#FIX ME.. move from earlier                                                                                                                                                           
#      tempFileInfo.timeCreated <- modelInputData.created                                                                                                                                                                                  
##      tempFileInfo.timeAltered <- modelInputData.created                                                                                                                                                                                  
#      tempFileInfo.fileType <- 'model design'                                                                                                                                                                                             
#      tempFileInfo.softwareVersion <- '1'                                                                                                                                                                                                 
#      tempFileInfo.matTime <- datenum(modelInputData.created,'yyyy_mm_dd_HH_MM_SS')                                                                                                                                                       
#      fileInfo <- tempFileInfo                                                                                                                                                                                                            
                                                                                                                                                                                                                                          
                                                                                                                                                                                                                                          
      # note that choice is from                                                                                                                                                                                                          
      # [B,I,choiceZ]=unique(gridInfo.assignmentColumn(dataZoneTrue)); # zones                                                                                                                                                            
      # and choices array for going backwards later                                                                                                                                                                                       
      ##############!!!!!!!!!!!!!!!!!!!!!!!need to save parameters and model                                                                                                                                                              
      ##############choices                                                                                                                                                                                                               
                                                                                                                                                                                                                                          

