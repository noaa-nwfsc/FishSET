#' Create alternative choice matrix
#'
#' Creates a list containing information on alternative choices. 
#'
#' @param dat  Main data frame containing data on hauls or trips. Table in fishset_db database should contain the string `MainDataTable`.
#' @param gridfile Spatial data. Shape, json, and csv formats are supported.
#' @param case Centroid='Centroid of Zonal Assignment', Port, Other
#' @param griddedDat Data frame containing a variable that varies by the map grid. First column should match a column in the dataset. The remaining columns should match the zone IDs in the gridfile.
#' @param contents Value of variable to subset dataset by. For instance, include only zones with at least 10 hauls.
#' @param hull.polygon Used in assignment_column function. Creates polygon using convex hull method.
#' @param haul.trip Should data be at trip or haul level. Defaults to haul.
#' @param alt_var  Identifies how to find lat/lon for starting point (must have a lat/lon associated with it) 
#' @param occasion  Identifies how to find lat/lon for alternative choices such as 'Centroid of Zonal Assignment' 
#' @param lon.dat Longitude variable in dataset
#' @param lat.dat Latitude variable in dataset
#' @param lon.grid Longitude variable in gridfile
#' @param lat.grid Latitude variable in gridfile
#' @param cat Variable defining zones or areas. Must be defined for dataset or gridfile.
#' @param use.grid TRUE/FALSE. If TRUE, griddedDat is used to create centroids
#' @param weight.var variable weighted centroids
#' @param remove.na TRUE/FALSE Remove points where zone ID not identified. Called in assignment_column function.
#' @param closest.pt  TRUE/FALSE If true, zone ID identified as the closest polygon to the point. Called in assignment_column function.
#' @param project Name of project. Used for naming table saved to database
#' @importFrom DBI dbExecute
#' @export create_alternative_choice
#' @details Functions returns alternative choice matrix. Function must be called before the create_expectations, make_model_design, or discretefish_subroutine functions can be called.
#' Function identifies which zones are to be included, based on number of hauls per trip, and assigns each observation to a zone.
#' @return Alternative choice matrix\cr
#' \tabular{rlll}{                                                                                                                                                                                                           
#'         dataZoneTrue: \tab Vector of 0/1 indicating whether the data from that zone is to be included in the model\cr
#'         greaterNZ: \tab zone which pass numofNecessary test\cr
#'         numOfNecessary: \tab Minimum number of hauls necessary within a zone for zone to be included\cr
#'         altChoiceUnits: \tab set to miles\cr
#'         altChoiceType: \tab set to distance\cr
#'         alt_var: \tab Identifies how to find lat/lon for starting point\cr
#'         occasion: \tab Identifies how to find lat/lon for alternative choices such as 'Centroid of Zonal Assignment' \cr
#'         zoneRow: \tab zones and choices array\cr
#'         zoneType: \tab haul or trip\cr
#'         int: \tab Centroid for each zone. Generated from find_centroid function
#'         }
 
create_alternative_choice <- function(dat, gridfile, case = c("Centroid", "Port", "Other"), contents, 
                                      haul.trip = c("Haul", "Trip"), alt_var, occasion, lon.dat, lat.dat, lon.grid, lat.grid, 
                                      cat, use.grid = c(TRUE, FALSE),  hull.polygon = c(TRUE, FALSE), remove.na = FALSE, 
                                      closest.pt = FALSE, project, griddedDat=NULL, weight.var = NULL) {
  
  
  #Call in datasets
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
  DBI::dbDisconnect(fishset_db)

  int <- find_centroid(use.grid = use.grid, dataset = dataset, gridfile = gridfile, 
                       lon.grid = lon.grid, lat.grid = lat.grid, lat.dat = lat.dat, lon.dat = lon.dat, 
                       cat = cat, weight.var = weight.var)
  
  #if (!is.empty(weight.var)) {
    int.data <- assignment_column(dataset = dataset, gridfile = gridfile, hull.polygon = hull.polygon, 
                                  lon.grid = lon.grid, lat.grid = lat.grid, lon.dat = lon.dat, 
                                  lat.dat = lat.dat, cat = cat, closest.pt = closest.pt)
    if (remove.na == TRUE) {
      dataset <- dataset[-which(is.na(int.data$ZoneID) == TRUE), ]
      int.data <- subset(int.data, is.na(int.data$ZoneID) == FALSE)
    }
    
    
    if(any(is.na(int.data$ZoneID)==TRUE)==TRUE){
      stop('No NAs allowed for the choice vector. Consider reunning the function using remove.na=TRUE.')
    }
    
    choice <- data.frame(int.data$ZoneID)
    
    
  #} else if (use.grid == T) {
   # int.data <- assignment_column(dataset = dataset, gridfile = grid.file, hull.polygon = hull.polygon, 
   #                               lon.grid = lon.grid, lat.grid = lat.grid, lon.dat = lon.dat, 
   #                               lat.dat = lat.dat, cat = cat, closest.pt = closest.pt)
   # if (remove.na == TRUE) {
   #   dataset <- dataset[-which(is.na(int.data$ZoneID) == TRUE), ]
  #    int.data <- subset(int.data, is.na(int.data$ZoneID) == FALSE)
   # }
    
  #  choice <- data.frame(int.data$ZoneID)
  #} else {
  #  choice <- dataset[[cat]]
  #}
  if (is.null(choice)) {
    stop("Choice must be defined. Ensure that the zone or area assignment variable (cat) is defined.")
  }
  
  if (case == "Centroid") {
    B <- as.data.frame(unique(int.data$ZoneID))  #unique(unlist(gridInfo['assignmentColumn',,]))
    C <- match(int.data$ZoneID, unique(int.data$ZoneID))  #  match(unlist(gridInfo['assignmentColumn',,]), unique(unlist(gridInfo['assignmentColumn',,])))
  
    } else {
    a <- names(dataset[, which(grepl("zon|area", colnames(dataset), ignore.case = TRUE) == TRUE)])  #find(zp)   #find data that is zonal type                                                                                                                                                                                            
    
    # [B,I,C]=unique([gridInfo.assignmentColumn(~isnan(gridInfo.assignmentColumn)),
    # data(a(v)).dataColumn(~isnan(gridInfo.assignmentColumn),:) ],'rows');%FIXME check that the order of output zones is consistent
    temp <- cbind(as.character(int.data$ZoneID), dataset[[a[1]]])  #cbind(unlist(gridInfo['assignmentColumn',,]), unlist(dataset[[a]]))
    B <- unique(temp)  # Correct ->> Needs to be lat/long
    C <- match(paste(temp[, 1], temp[, 2], sep = "*"), paste(B[, 1], B[, 2], sep = "*"))  #    C <- data(a(v))[dataColumn,'rows'] 
  }
  
  numH <- accumarray(C, C)
  binH <- 1:length(numH)
  numH <- numH/t(binH)
  zoneHist <- data.frame(numH = as.vector(numH), binH = as.vector(binH), B[, 1])
  
  zoneHist[which(zoneHist[, 1] < contents), 3] <- NA
  
  if (any(is.empty(which(is.na(zoneHist[, 3]) == F)))) {
    stop("No zones meet criteria. Check the contents parameter or zone identification.")
  }
  #dataZoneTrue=ismember(gridInfo.assignmentColumn,zoneHist(greaterNZ,3));
  dataZoneTrue <- cbind(int.data$ZoneID %in% zoneHist[, 3], match(int.data$ZoneID, zoneHist[, 3], nomatch = 0))  
        #dataZoneTrue=ismember(gridInfo.assignmentColumn,zoneHist(greaterNZ,3));
  greaterNZ <-  which(zoneHist[,1] >= contents) # ifelse(!is.na(zoneHist[, 1]) & zoneHist[, 1] >= 0, 1, 0)
  numOfNecessary <- contents
  
  fishset_db <- DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite")
  
        Alt <- list(
         dataZoneTrue = dataZoneTrue[,1], # array of logical values to identify which are to be used in model                                                                                              
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
         zoneType = ifelse(haul.trip == 'Haul', 'Hauls', 'Trips'),
         int = int # centroid data
        )  
   
        ### Add gridded data ###
      if(!is.null(griddedDat)){
        gridVar <- griddedDat
        
        if (DBI::dbExistsTable(fishset_db, griddedDat) == FALSE) {
          DBI::dbWriteTable(fishset_db, griddedDat, gridVar)
        } 
        
        int <- noquote(gsub("[^0-9]", "", colnames(gridVar)))
        
        if(any(noquote(gsub("[^0-9]", "", colnames(temp))) %in% int.data$ZoneID)==FALSE){
          stop('Cannot use griddedDat. Column names of griddedDat do not match zone ids in gridfile.')
        }
        
        #If gridded data is not an array, need to create matrix
        if (dim(gridVar)[1]==1) { #(is.empty(gridVar.row.array)){ #1d
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
         if (any(is.na(allMat[Alt[['dataZoneTrue']],]))) {
          stop('Problem with loaded matrix, NaN found.')
         }
        
        Alt <-list.append(Alt, matrix = allMat[Alt[['dataZoneTrue']],])  #allMat[Alt[[dataZoneTrue]],]
      }
        
        #write Alt to datafile
        single_sql <- paste0(project, 'altmatrix', format(Sys.Date(), format="%Y%m%d"))
        DBI::dbExecute (fishset_db, paste("CREATE TABLE IF NOT EXISTS", single_sql, "(AlternativeMatrix ALT)"))
        DBI::dbExecute (fishset_db, paste("INSERT INTO", single_sql, "VALUES (:AlternativeMatrix)"), 
                                          params = list(AlternativeMatrix = list(serialize(Alt, NULL))))
        #DBI::dbExecute (fishset_db, "CREATE TABLE IF NOT EXISTS altmatrix (AlternativeMatrix ALT)")
        #DBI::dbExecute (fishset_db, "INSERT INTO altmatrix VALUES (:AlternativeMatrix)", params = list(AlternativeMatrix = list(serialize(Alt, NULL))))
        DBI::dbDisconnect(fishset_db)
        
       Alt <<- Alt        
       

       if(!exists('logbody')) { 
         logging_code()
       } 
       create_alternative_choice_function <- list()
       create_alternative_choice_function$functionID <- 'create_alternative_choice'
       create_alternative_choice_function$args <- c(deparse(substitute(dat)), deparse(substitute(gridfile)), case, contents,
                                                   haul.trip, alt_var, occasion, lon.dat, lat.dat, lon.grid,  lat.grid, cat,  use.grid, 
                                                   hull.polygon, remove.na, closest.pt, project)
       create_alternative_choice_function$kwargs <- list('griddedDat'= griddedDat, 'weight.var'= weight.var)
       create_alternative_choice_function$output <- c()
       functionBodyout$function_calls[[length(functionBodyout$function_calls)+1]] <- (create_alternative_choice_function)
       logbody$fishset_run <- list(infoBodyout, functionBodyout)
       write(jsonlite::toJSON(logbody, pretty = TRUE, auto_unbox = TRUE), paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""))
       assign("functionBodyout", value = functionBodyout, pos = 1)
}                                                                                                                                                                                                                           
   



    