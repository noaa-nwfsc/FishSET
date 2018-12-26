#'  Create alternative choice matrix
#'
#' 
#' 

#' @param dataset dataframe or matrix
#' @param gridfile name of gridded dataset
#' @param case Centroid='Centroid of Zonal Assignment', Port, Other
#' @param contents Value of variable to subset dataset by. For instance, include only zones with at least 10 hauls.
#' @param hull.polygon Using in assignmentColumn function. Creates polying using convex hull method.
#' @param Haul.Trip Whether data is at trip or haul. Default to haul.
#' @param alt_var # Identifies how to find lat/lon for starting point (must have a lat/lon associated with it) 
#' @param occasion # Identifies how to find lat/lon for alternative choices such as 'Centroid of Zonal Assignment' 
#' @param lon.dat #Longitude variable in dataset
#' @param lat.dat #Latitude variable in dataset
#' @param lon.grid #Longitude variable in gridfile
#' @param lat.grid #Latitude variable in gridfile
#' @param cat #Variable defining zones or areas. Must be defined for dataset or gridfile.
#' @param use.grid #TRUE/FALSE. If TRUE, gridded data is used to create centroids
#' @param weight.var #Variable for weighted centroids




#' @return returns list containing information on alternative choice
#' @details Functions returns alternative choice matrix
#' #outputs:                                                                                                                                                                                                                   
#'         Alt.dataZoneTrue - # array of logical values to identify which are to be used in model                                                                                                                           
#'         Alt.greaterNZ                                                                                                                                                                                                     
#'         Alt.numOfNecessary                                                                                                                                                                                                
#'         Alt.altChoiceUnits                                                                                                                                                                                                
#'         Alt.alt_var- alternative variable                                                                                                                                                                                 
#'         Alt.occassion- occassion variable                                                                                                                                                                                   
#'         Alt.functionType                                                                                                                                                                                                  
#'         Alt.zoneRow                                                                                                                                                                                                       
#'         Alt.zoneType 
#'         Alt.int 
                                                                                                                                                                                                     
# @examples 
# 
# 

 
createAlternativeChoice <- function(dataset, gridfile, case=c('Centroid','Port','Other'), contents, Haul.Trip=C('Haul','Trip'), alt_var, occasion, 
                                       lon.dat, lat.dat, lon.grid, lat.grid, cat, use.grid=c(TRUE,FALSE), weight.var=NULL,hull.polygon){
                     gridfile <- as.data.frame(gridfile)
                     int <- findCentroid(use.grid = use.grid, dataset = dataset, gridfile = gridfile, lon.grid = lon.grid, lat.grid = lat.grid, 
                                         lat.dat = lat.dat, lon.dat = lon.dat, cat = cat, weight.var = weight.var)   
                     
                     if(!is.empty(weight.var)) {
                          int.data <- assignmentColumn(dataset = dataset, gridfile = gridfile, hull.polygon=hull.polygon, lon.grid =lon.grid, lat.grid = lat.grid, 
                                                     lon.dat = lon.dat, lat.dat = lat.dat, cat = cat)
                          choice <- data.frame(int.data$ZoneID)
                     } else if(use.grid==T) {
                          int.data <- assignmentColumn(dataset = dataset, gridfile = gridfile, lon.grid =lon.grid, lat.grid = lat.grid, 
                                                     lon.dat = lon.dat, lat.dat = lat.dat, cat = cat, hull.polygon=hull.polygon)
                          choice <- data.frame(int.data$ZoneID)
                     } else {
                          choice <- dataset[[cat]]
                     }
                     if(is.null(choice)) {
                          stop('Choice must be defined. Ensure that the zone or area assignment variable (cat) is defined.')
                     }
 #                    df.name <- deparse(substitute(dataset))
 #                    x.name <- deparse(substitute(x))
 #                    write(layout.json.ed(trace, 'DummyMatrix', df.name, x=x.name), 
 #                          paste('~/FistSET_RPackage/Logs/Log_file', Sys.Date(), '.json'), append=T)
                     
if( case == 'Centroid' ){                                                                                                                                                                             
                  B <- unique(int.data$ZoneID)  #unique(unlist(gridInfo['assignmentColumn',,]))
                  C <- match(int.data$ZoneID, unique(int.data$ZoneID))#  match(unlist(gridInfo['assignmentColumn',,]), unique(unlist(gridInfo['assignmentColumn',,])))
             } else {                                                                                                                                                                                                  
                 a <- names(dataset[,which(grepl('zon|area', colnames(dataset), ignore.case=TRUE)==TRUE)])  #find(zp)   #find data that is zonal type                                                                                                                                                                                            
                         
 #                [B,I,C]=unique([gridInfo.assignmentColumn(~isnan(gridInfo.assignmentColumn)), data(a(v)).dataColumn(~isnan(gridInfo.assignmentColumn),:) ],'rows');%FIXME check that the order of output zones is consistent
                  temp <- cbind(as.character(int.data$ZoneID), dataset[[a[1]]]) #cbind(unlist(gridInfo['assignmentColumn',,]), unlist(dataset[[a]]))
                  B <- unique(temp) # Correct ->> Needs to be lat/long
                  C <- match(paste(temp[,1],temp[,2],sep="*"), paste(B[,1],B[,2],sep="*"))#    C <- data(a(v))[dataColumn,'rows'] #FIXME check that the order of output zones is consistent
             } 
                     
                     numH <-  accumarray(C,C)                                                                                                                                                                                            
                     binH <- 1:length(numH)                                                                                                                                                                                              
                     numH <-  numH/t(binH)                                                                                                                                                                                                
                     zoneHist <- data.frame(numH=as.vector(numH), binH=as.vector(binH), B=as.vector(B))                                                                                                                                                                                         
              
        
         zoneHist[ which(zoneHist[,1]<contents),3] <- NA
         
         if(any(is.empty(which(is.na(zoneHist[,3])==F)))){
              stop('No zones meet criteria. Check the contents parameter or zone identification.')
         }
         
         dataZoneTrue <- cbind(int.data$ZoneID %in% zoneHist[, 3], match(int.data$ZoneID, zoneHist[, 3], nomatch = 0))#ismember(int.data$ZoneID, zoneHist[, 3]) #unlist(gridInfo['assignmentColumn' ,,])
                                                                                                                                                                                    
         greaterNZ <- ifelse(!is.na(zoneHist[,1])&zoneHist[, 1] >= 0, 1, 0)                                                                                                                                                                        
         numOfNecessary <- contents #Need to figure this out
                                                                                                                                                                                                                             
                                                                                                                                                                                                                             
        Alt <<- list(
         dataZoneTrue = dataZoneTrue[,2], # array of logical values to identify which are to be used in model                                                                                              
         greaterNZ = greaterNZ,                                                                                                                                                                       
         numOfNecessary = numOfNecessary,  #input
         choice = choice,
         altChoiceUnits = 'miles',    #miles   
         altChoiceType = 'distance',
         alt_var =  alt_var, #altToLocal1
         occasion = occasion,  #altToLocal2                                                                                                                                                                          
         zoneHist = zoneHist,                                                                                                                                                                              
         zoneRow = zoneHist[greaterNZ, 3], # zones and choices array                                                                                                                                                  
        # assignChoice = gridInfo['dataColumnLink',,],                                                                                                                                                                            
         zoneType = ifelse(Haul.Trip == 'Haul', 'Hauls', 'Trips'),
         int = int # centroid data
        )  
        
        #write Alt to datafile
  #      dbWriteTable(testdb,'Alt', Alt)
        
                                                                                                                                                                                                                        
  #      write_lines(layout.json.ed(trace, 'createAlternativeChoice', dataset, x='', 
  #                  msg=paste('gridfile:', gridfile, 'case:', case, 'contents:', contents, 'Haul.Trip:', Haul.Trip, 'alt_var:', alt_var, 
  #                            'occasion:', occasion, 'lon.dat:', lon.dat, 'lat.dat:', lat.dat, 'lon.grid:', lon.grid,
  #                            'lat.grid:', lat.grid, 'cat:', cat, 'use.grid:', use.grid, 'weight.var:', weight.var)), 
  #                  paste(getwd(),'/Logs/',Sys.Date(),'.json', sep=''), append=T )
        
}                                                                                                                                                                                                                           
   
    
    
