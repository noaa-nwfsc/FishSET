#' Create alternative choice matrix
#'
#' Required step. Creates a list containing information on how alternative fishing choices should be defined.  Output is called by \code{\link{make_model_design}}. See function details for more information on output. Run this function before running models.  Output is saved to the FishSET database. 
#'
#' @param dat  Main data frame containing data on hauls or trips. 
#' Table in FishSET database should contain the string `MainDataTable`.
#' @param gridfile Spatial data containing information on fishery management or regulatory zones. 
#' Shape, json, geojson, and csv formats are supported. \code{gridfile} should be NULL if \code{cat} 
#' is a variable in the primary dataset.
# @param case Centroid='Centroid of Zonal Assignment', Port, Other
#' @param min.haul Numeric, minimum number of hauls. Zones with fewer hauls than the \code{min.haul} 
#' value will not be included in model data.
#' @param hull.polygon Used in \code{\link{assignment_column}} function. Creates polygon using convex 
#' hull method. Required if zonal assignments for observations in \code{dat} should be identified 
#' and \code{gridfile} is not NULL.
#' @param alt_var String, identifies how to find lat/lon for starting point (must have a lat/lon associated with it). 
#' \code{alt_var} maybe the ‘centroid of zonal assignment’, a port variable or a lat/lon variable in the primary dataset. 
#' If a port variable is defined, a corresponding port table must exist which contains the port name and the latitude and 
#' longitude of each port. 
#' @param occasion Identifies how to find lat/lon for alternative choices. Occasion maybe the ‘centroid of zonal assignment’,
#'  a port variable or a lat/lon variable in the primary dataset. If a port variable is defined, a corresponding port table
#'   must exist which contains the port name and the latitude and longitude of each port.
#' @param dist.unit String, how distance measure should be returned. 
#' Choices are 'meters’ or ‘M’, ‘kilometers’ or ‘KM’, or ‘miles’. Defaults to miles.
#' @param lon.dat Longitude variable from \code{dat}. Required if zonal assignments for observations in \code{dat} 
#' should be identified and \code{gridfile} is not NULL.
#' @param lat.dat Latitude variable from \code{dat}. Required if zonal assignments for observations in \code{dat} 
#' should be identified and \code{gridfile} is not NULL.
#' @param cat Variable in either \code{dat} or \code{gridfile} that identifies the individual areas or zones.
#'  If \code{cat} is a variable of assigned zones for each occurrence records, set \code{gridfile} to NULL. 
#'  Otherwise, if \code{gridfile} is class sf, `cat` should be name of list containing information on zones. 
#' @param lon.grid Variable or list from \code{gridfile} containing longitude data. Required for csv files.
#'  Leave as NULL if \code{gridfile} is a shape or json file, Required if zonal assignments for observations 
#'  in \code{dat} should be identified and \code{gridfile} is not NULL.
#' @param lat.grid Variable or list from \code{gridfile} containing latitude data. Required for csv files. 
#' Leave as NULL if \code{gridfile} is a shape or json file, Required if zonal assignments for observations 
#' in \code{dat} should be identified and \code{gridfile} is not NULL.
#' @param weight.var Variable for calculating weighted centroids. Required if zonal assignments for observations 
#' in \code{dat} should be identified and \code{gridfile} is not NULL.
#' @param closest.pt Logical, if true, zone ID identified as the closest polygon to the point. 
#' Called in \code{\link{assignment_column}}. Required if zonal assignments for observations in \code{dat} 
#' should be identified and \code{gridfile} is not NULL.
#' @param project String, name of project.  
#' @param griddedDat Optional data frame. Data must contain a variable that varies by the spatial dataset \code{gridfile}.
#'  First variable in \code{griddedDat} should match a column in \code{dat}. The remaining columns should match the 
#'  zone IDs in the \code{gridfile}.
#' @importFrom DBI dbExecute
#' @export create_alternative_choice
#' @details Functions returns alternative choice matrix and saves the output to the FishSET database. The matrix is 
#' pulled by \code{\link{create_expectations}}, \code{\link{make_model_design}}, and the model run 
#' function (\code{\link{discretefish_subroutine}}). Function assigns each observation to a zone and identifies 
#' which zones are to be included based on a minimum number of hauls per trip within a zone. Functions that call the 
#' alternative choice matrix will exclude observations in zones that do meet the minimum haul criteria. Function defines 
#' how start and end points for calculating the distance matrix in \code{\link{make_model_design}}. 
#' Note that if the alternative choice matrix is modified, the \code{\link{create_expectations}} 
#' and \code{\link{make_model_design}} functions should also be updated before rerunning the model run 
#' function (\code{\link{discretefish_subroutine}}).
#' @return Saves the alternative choice matrix to the FishSET database as a list.
#' Output includes: \cr
#' \tabular{rlll}{                                                                                                                                                                                                           
#'         dataZoneTrue: \tab Vector of 0/1 indicating whether the data from that zone is to be included in the model\cr
#'         greaterNZ: \tab Zone which pass numofNecessary test\cr
#'         numOfNecessary: \tab Minimum number of hauls for zone to be included\cr
#'         altChoiceUnits: \tab Set to miles\cr
#'         altChoiceType: \tab Set to distance\cr
#'         alt_var: \tab Identifies how to find latitude and longitude for starting point\cr
#'         occasion: \tab Identifies how to find latitude and longitude for alternative choice \cr
#'         zoneRow: \tab Zones and choices array\cr
#'         int: \tab Centroid for each zone. Generated from \code{\link{find_centroid}}
#'         }
 
create_alternative_choice <- function(dat, project, gridfile=NULL, min.haul, 
                                      alt_var, occasion, dist.unit='miles', lon.dat, lat.dat, cat, 
                                      hull.polygon = c(TRUE, FALSE), closest.pt = FALSE, lon.grid=NULL, lat.grid=NULL,  
                                      griddedDat=NULL, weight.var = NULL) {
  
  stopanaly <- 0
  case <- 'centroid'
  #Call in datasets
    dataset <- dat
    dat <- deparse(substitute(dat))
  
  int <- find_centroid(dat=dataset, gridfile = gridfile, lon.grid = lon.grid, lat.grid = lat.grid, 
                       lat.dat = lat.dat, lon.dat = lon.dat, cat = cat, weight.var = weight.var)
  
  if(!is.null(gridfile)){
  #if (!FishSET::is_empty(weight.var)) {
    int.data <- assignment_column(dat=dataset, gridfile = gridfile, hull.polygon = hull.polygon, 
                                  lon.grid = lon.grid, lat.grid = lat.grid, lon.dat = lon.dat, 
                                  lat.dat = lat.dat, cat = cat, closest.pt = closest.pt)
  } else {
      int.data <- dataset
    }
   if(anyNA(int.data[[cat]])==TRUE){
      warning(paste("No zone identified for", sum(is.na(int.data[[cat]])), "observations. These observations will be removed in future analyses."))
    }
    
    choice <- data.frame(int.data[[cat]])
    startingloc <- if(!'startingloc' %in% int.data) { rep(NA, nrow(int.data))} else { data.frame(int.data$startingloc)}
    
   if (is.null(choice)) {
    warning("Choice must be defined. Ensure that the zone or area assignment variable (cat parameter) is defined.")
     stopanaly = 1
  }
  
  if (case == "Centroid") {
    B <- as.data.frame(unique(int.data[[cat]][!is.na(int.data[[cat]])]))  #unique(unlist(gridInfo['assignmentColumn',,]))
    C <- match(int.data[[cat]][!is.na(int.data[[cat]])], unique(int.data[[cat]][!is.na(int.data[[cat]])]))  #  match(unlist(gridInfo['assignmentColumn',,]), unique(unlist(gridInfo['assignmentColumn',,])))
  
    } else {
    a <- colnames(dataset)[grep("zon|area", colnames(dataset), ignore.case = TRUE)] #find(zp)   #find data that is zonal type                                                                                                                                                                                            
    
    # [B,I,C]=unique([gridInfo.assignmentColumn(~isnan(gridInfo.assignmentColumn)),
    # data(a(v)).dataColumn(~isnan(gridInfo.assignmentColumn),:) ],'rows');%FIXME check that the order of output zones is consistent
    temp <- cbind(as.character(int.data[[cat]]), dataset[[a[1]]])  #cbind(unlist(gridInfo['assignmentColumn',,]), unlist(dataset[[a]]))
    B <- unique(temp)  # Correct ->> Needs to be lat/long
    C <- match(paste(temp[, 1], temp[, 2], sep = "*"), paste(B[, 1], B[, 2], sep = "*"))  #    C <- data(a(v))[dataColumn,'rows'] 
  }
  
  numH <- accumarray(C, C)
  binH <- 1:length(numH)
  numH <- numH/t(binH)
  zoneHist <- data.frame(numH = as.vector(numH), binH = as.vector(binH), B[, 1])
  
  zoneHist[which(zoneHist[, 1] < min.haul), 3] <- NA
  
  if (any(is_empty(which(is.na(zoneHist[, 3]) == F)))) {
    warning("No zones meet criteria. No data will be included in further analyses. Check the min.haul parameter or zone identification.")
    stopanaly = 1
  }
  
  if(stopanaly==0){
  #dataZoneTrue=ismember(gridInfo.assignmentColumn,zoneHist(greaterNZ,3));
  dataZoneTrue <- cbind(int.data[[cat]] %in% zoneHist[, 3], match(int.data[[cat]], zoneHist[, 3], nomatch = 0))  
        #dataZoneTrue=ismember(gridInfo.assignmentColumn,zoneHist(greaterNZ,3));
  greaterNZ <-  which(zoneHist[,1] >= min.haul) # ifelse(!is.na(zoneHist[, 1]) & zoneHist[, 1] >= 0, 1, 0)
  numOfNecessary <- min.haul
  
  fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase())
  
        Alt <- list(
         dataZoneTrue = dataZoneTrue[,1], # array of logical values to identify which are to be used in model                                                                                              
         greaterNZ = greaterNZ,                                                                                                                                                                       
         numOfNecessary = numOfNecessary,  #input
         choice = choice,
         altChoiceUnits = dist.unit,    #miles   
         altChoiceType = 'distance',
         alt_var =  alt_var, #altToLocal1
         occasion = occasion,  #altToLocal2  
         startingloc = startingloc,
         zoneHist = zoneHist,                                                                                                                                                                              
         zoneRow = zoneHist[greaterNZ, 3], # zones and choices array                                                                                                                                                  
        # assignChoice = gridInfo['dataColumnLink',,],                                                                                                                                                                            
         #zoneType = ifelse(haul.trip == 'Haul', 'Hauls', 'Trips'),
         int = int # centroid data
        )  
  } 
        ### Add gridded data ###
      if(!is.null(griddedDat)){
        gridVar <- griddedDat
        
        if (DBI::dbExistsTable(fishset_db, griddedDat) == FALSE) {
          DBI::dbWriteTable(fishset_db, griddedDat, gridVar)
        } 
        
        int <- noquote(gsub("[^0-9]", "", colnames(gridVar)))
        
        if(any(noquote(gsub("[^0-9]", "", colnames(temp))) %in% int.data[[cat]])==FALSE){
          stop('Cannot use griddedDat. Column names of griddedDat do not match zone ids in gridfile.')
        }
        
        #If gridded data is not an array, need to create matrix
        if (dim(gridVar)[1]==1) { #(is_empty(gridVar.row.array)){ #1d
          biG <- match(Alt[['zoneRow']], int) #[aiG,biG] = ismember(Alt.zoneRow, gridVar.col.array) #FIXME FOR STRING CONNECTIONS
          numRows <- nrow(dataset) #size(data(1).dataColumn,1)  #
          if (!any(biG)){
            stop('The map associated to the data and the grid information in the gridded variable do not overlap.')
          }
          allMat <- matrix(1, numRows, 1) %x% as.matrix(gridVar[1,biG]) # repmat(gridVar.matrix(1,biG), numRows, 1)
          
        } else {
          #[aiG,biG] = ismember(Alt.zoneRow, gridVar.col.array)#FIXME FOR STRING CONNECTIONS
          biG <- match(Alt[['zoneRow']], int[-1]) #gridVar.col.array
          if (!any(biG)) {
            stop('The map associated to the data and the grid information in the gridded variable do not overlap.')
          }
          
          if (names(gridVar)[1] %in% colnames(dataset)==FALSE){
            #wrong occourance variable to connect data
            stop('The data in the workspace and the loaded grid file do not have a matching variable for connecting.')
          }
          
          biD <- match(dataset[,names(gridVar)[1]], gridVar[,1]) #[aiD,biD]=ismember(data(occasVar).dataColumn,gridVar.row.array)
          
          if (!any(biD)){
            print('The data in the workspace and the loaded grid file do not have a matching variable for connecting.')
          }
          
          allMat <- gridVar[,-1][biD,biG]
        }
         if(anyNA(allMat[Alt[['dataZoneTrue']],])) {
          stop('Problem with loaded matrix, NaN found.')
         }
        
        Alt <- list.append(Alt, matrix = allMat[Alt[['dataZoneTrue']],])  #allMat[Alt[[dataZoneTrue]],]
      }
        
        
  #write Alt to datafile
        if(stopanaly==0){
        single_sql <- paste0(project, 'altmatrix')
        date_sql <- paste0(project, 'altmatrix', format(Sys.Date(), format="%Y%m%d"))
        if(table_exists(single_sql)){
          table_remove(single_sql)
        } 
        if(table_exists(date_sql)){
          table_remove(date_sql)
        }
        DBI::dbExecute (fishset_db, paste("CREATE TABLE IF NOT EXISTS", single_sql, "(AlternativeMatrix ALT)"))
        DBI::dbExecute (fishset_db, paste("INSERT INTO", single_sql, "VALUES (:AlternativeMatrix)"), 
                                          params = list(AlternativeMatrix = list(serialize(Alt, NULL))))
        DBI::dbExecute (fishset_db, paste("CREATE TABLE IF NOT EXISTS", date_sql, "(AlternativeMatrix ALT)"))
        DBI::dbExecute (fishset_db, paste("INSERT INTO", date_sql, "VALUES (:AlternativeMatrix)"), 
                        params = list(AlternativeMatrix = list(serialize(Alt, NULL))))
        #DBI::dbExecute (fishset_db, "CREATE TABLE IF NOT EXISTS altmatrix (AlternativeMatrix ALT)")
        #DBI::dbExecute (fishset_db, "INSERT INTO altmatrix VALUES (:AlternativeMatrix)", params = list(AlternativeMatrix = list(serialize(Alt, NULL))))
        DBI::dbDisconnect(fishset_db)
        
     
       create_alternative_choice_function <- list()
       create_alternative_choice_function$functionID <- 'create_alternative_choice'
       create_alternative_choice_function$args <- list(dat, project, deparse(substitute(gridfile)), min.haul,
                                                   alt_var, occasion, dist.unit, lon.dat, lat.dat, cat,  
                                                   hull.polygon, closest.pt)
       create_alternative_choice_function$kwargs <- list('lon.grid'=lon.grid, 'lat.grid'=lat.grid, 'griddedDat'= griddedDat, 'weight.var'= weight.var)
       create_alternative_choice_function$output <- list()
       
       log_call(create_alternative_choice_function)
    }
}                                                                                                                                                                                                                           
   
