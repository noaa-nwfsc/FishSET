#' Collapse the dataframe from haul to trip.
#'
#' @param dat Data frame containing haul level data. In the fishset_db database, the table will contain the phrase `MainDataTable`
#' @param dataindex Data frame that contains information on each column of the main data frame. In fishset_db database the table will contain the phrase `MainDataTableInfo`
#' @param fun.time Time units for function. Defaults to minutes.
#' @param fun.numeric Defaults to mean
#' @param ... Column(s) that identify the individual trip.
#' @export haul_to_trip
#' @return Data frame with each row representing a trip or haul
#' @details Collapses the main data table from haul to trip level. Requires the MainDataTableInfo table associated with the main data table. The MainDataTableInfo table is first updated
#' using the dataindex_update function. Unique trips are defined based on selected column(s). For example, landing permit number and disembarked port.
#'  This id column is used to collapse the data to trip level. Users can define how time and numeric variables with multiple observations for a trip are collapsed.
#' For instance, the mean value of the numeric observations. For non-numeric and non-time variables, the first observation is used.
#' 
#' @examples
#' \dontrun{
#'  dat <- haul_to_trip(MainDataTable, 'MainDataTableInfo',min,mean,'PERMIT','DISEMBARKED_PORT')
#'  }


haul_to_trip <- function(dat, dataindex, fun.time = min, fun.numeric = mean, ...) {
  # Create a column that indicates unique trip levels
  
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
  
  #Load in dataindex
  dataIndex <- dataindex_update(dataset, dataindex)
#    suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite"))
#    single_sql <- paste("select * from ", dataindex, sep='')
#    dataindex <- DBI::dbGetQuery(fishset_db,  single_sql)
#    DBI::dbDisconnect(fishset_db)

    argList <- (as.character(match.call(expand.dots = FALSE)$...))
  
  idmaker = function(vec) {
    return(paste(sort(vec), collapse = ""))
  }
  int <- as.data.frame(cbind(dataset, rowID = as.numeric(factor(apply(as.matrix(dataset[, eval(substitute(argList))]), 1, idmaker)))))
  #int <- int[, c(colnames(sapply(dataindex[[varnameindex]], grepl, colnames(int))), "rowID")]
  
  # Handling of empty variables
  if (any(apply(int, 2, function(x) all(is.na(x))) == TRUE)) {
    int <- int[, -which(apply(int, 2, function(x) all(is.na(x))) == TRUE)]
  } else {
    int <- int
  }
  
  
  drop <- if (length(which(grepl('DUR', names(int[,c(which(as.data.frame(dataIndex[dataIndex[, 'variable_name'] == 
                          colnames(int[,-which(colnames(int)=='rowID')]), 'generalType']) == "Time"))]), ignore.case=T)==T)) == 0 ) {
          0 } else {
            which(grepl('DUR', names(int[,c(which(as.data.frame(dataIndex[dataIndex[, 'variable_name'] == 
                         colnames(int[,-which(colnames(int)=='rowID')]), 'generalType']) == "Time"))]), ignore.case=T)==T)
          }
  # Collapse data based on rowID and defined function
  out <- data.frame(drop=rep(0, length(unique(int$rowID))))
   
    #Nothing listed
        if(length(which(is.na(as.data.frame(
          dataIndex[dataIndex[, 'variable_name'] %in% colnames(int[,-which(colnames(int)=='rowID')]), 'generalType']) == TRUE)))>0) {
          out <- cbind(out, 
                      stats::aggregate(int[,c(which(is.na(as.data.frame(dataIndex[dataIndex[, 'variable_name'] %in% 
                                                      colnames(int[,-which(colnames(int)=='rowID')]), 'generalType']) == TRUE)), 
                                             which(colnames(int)=='rowID'))], 
                                        list(int$rowID), FUN = head,  1)[,-1])
        } 
    #Time - not duration
      if(length(which(as.data.frame(
        dataIndex[dataIndex[, 'variable_name'] %in% colnames(int[,-which(colnames(int)=='rowID')]), 'generalType']) == "Time"))>0) {
        out2 <-  suppressWarnings ( stats::aggregate(cbind(
                as.data.frame(lapply(
            if(drop>0){    
                int[,c(which(as.data.frame(dataIndex[dataIndex[, 'variable_name'] %in% 
                                               colnames(int[,-which(colnames(int)=='rowID')]), 'generalType']) == "Time"))][-drop]
              } else{
                as.data.frame(int[,c(which(dataIndex[dataIndex[, 'variable_name'] %in% 
                                               colnames(int[,-which(colnames(int)=='rowID')]), 'generalType'] == "Time"))])
              }, 
            FishSET:::date_parser)), rowID=int$rowID), list(int$rowID), match.fun(fun.time), na.rm=T))[,-1]
          }
        names(out2)[1:dim(out2)[2]-1] <- names(int)[which(dataIndex[dataIndex[, 'variable_name'] %in% 
                                                          colnames(int[,-which(colnames(int)=='rowID')]), 'generalType'] == "Time")] 
        out <- cbind(out, out2)
      #Time - duration
    if(length(which(as.data.frame(
        dataIndex[dataIndex[, 'variable_name'] %in% colnames(int[,-which(colnames(int)=='rowID')]), 'generalType']) == "Time"))>0 & 
                  any(grepl('dur', colnames(int)[which(as.data.frame(dataIndex[dataIndex[, 'variable_name'] %in% 
                               colnames(int[,-which(colnames(int)=='rowID')]), 'generalType']) == "Time")], ignore.case=TRUE)) == TRUE) {
      out <- cbind(out, 
                   suppressWarnings ( stats::aggregate(cbind(
                    as.data.frame(
                      int[,c(which(as.data.frame(dataIndex[dataIndex[, 'variable_name'] %in% 
                                                 colnames(int[,-which(colnames(int)=='rowID')]), 'generalType']) == "Time"))][-
                                    which(grepl('dur', names(int[,c(which(as.data.frame(dataIndex[dataIndex[, 'variable_name'] %in% 
                                     colnames(int[,-which(colnames(int)=='rowID')]), 'generalType']) == "Time"))]), 
                                     ignore.case=T)==FALSE)]), rowID=int$rowID), list(int$rowID), match.fun(fun.time), na.rm = TRUE))[,-1])
      }
    #Other numeric  
    if(length(which(as.data.frame(
        dataIndex[dataIndex[, 'variable_name'] %in% colnames(int[,-which(colnames(int)=='rowID')]), 'generalType']) == "Other Numeric"))>0) {
      out <- cbind(out, 
                   stats::aggregate(int[,c(which(as.data.frame(dataIndex[dataIndex[, 'variable_name'] %in% 
                                                   colnames(int[,-which(colnames(int)=='rowID')]), 'generalType']) == "Other Numeric"), 
                                           which(colnames(int)=='rowID'))], 
                  list(int$rowID), match.fun(fun.numeric), na.action = na.pass)[,-1])
    }
    #Latitude
    if(length(which(as.data.frame(
        dataIndex[dataIndex[, 'variable_name'] %in% colnames(int[,-which(colnames(int)=='rowID')]), 'generalType']) == "Latitude"))>0) {
      out <- cbind(out, 
                   stats::aggregate(int[,c(which(as.data.frame(dataIndex[dataIndex[, 'variable_name'] %in% 
                                                 colnames(int[,-which(colnames(int)=='rowID')]), 'generalType']) == "Latitude"), 
                                           which(colnames(int)=='rowID'))], 
                  list(int$rowID),  FUN = head, 1)[,-1])
    }
    #Coded
    if(length(which(as.data.frame(
        dataIndex[dataIndex[, 'variable_name'] %in% colnames(int[,-which(colnames(int)=='rowID')]), 'generalType']) == "Code Numeric"))>0) {
      out <- cbind(out, 
                   stats::aggregate(int[,c(which(as.data.frame(dataIndex[dataIndex[, 'variable_name'] %in% 
                                                 colnames(int[,-which(colnames(int)=='rowID')]), 'generalType'])== "Code Numeric"), 
                                           which(colnames(int)=='rowID'))], 
                  list(int$rowID),  FUN = head, 1)[,-1])
    }
    #Coded
    if(length(which(as.data.frame(
        dataIndex[dataIndex[, 'variable_name'] %in% colnames(int[,-which(colnames(int)=='rowID')]), 'generalType']) == "Code"))>0) {
      out <- cbind(out, 
                   stats::aggregate(int[,c(which(as.data.frame(dataIndex[dataIndex[, 'variable_name'] %in% 
                                                 colnames(int[,-which(colnames(int)=='rowID')]), 'generalType']) == "Code"), 
                                           which(colnames(int)=='rowID'))], 
              list(int$rowID),  FUN = head, 1)[,-1])
    }
    #Coded
    if(length(which(as.data.frame(
      dataIndex[dataIndex[, 'variable_name'] %in% colnames(int[,-which(colnames(int)=='rowID')]), 'generalType']) == "Code String"))>0) {
      out <- cbind(out, 
                   stats::aggregate(int[,c(which(as.data.frame(dataIndex[dataIndex[, 'variable_name'] %in%
                                                 colnames(int[,-which(colnames(int)=='rowID')]), 'generalType']) == "Code String"),
                                           which(colnames(int)=='rowID'))], 
              list(int$rowID),  FUN = head, 1)[,-1])
    }
    #Not in the dataIndex file
    if(length(colnames(int[,-grep('rowID', colnames(int[,-grep('rowID', colnames(int))]))])
              [!(colnames(int[,-grep('rowID', colnames(int))]) %in% dataIndex[, 'variable_name'])])>0) {
      out <- cbind(out, 
                   stats::aggregate(int[,c(colnames(int)[!(colnames(int) %in% dataIndex[, 'variable_name'])])], 
                                        list(int$rowID),  FUN = head, 1)[,-1])
    }

      
  out <-out[-which(colnames(out)=='rowID')[-length(which(colnames(out)=='rowID'))]]
  out <- out[-which(colnames(out)=='drop')]
  out <- data.frame(out)

    
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
  haul_to_trip_function <- list()
   haul_to_trip_function$functionID <- 'haul_to_trip'
   haul_to_trip_function$args <- c(paste0('dat=',deparse(substitute(dat))), dataindex)
   haul_to_trip_function$kwargs <- list(fun.time, fun.numeric, ...)
   haul_to_trip_function$output <- c(deparse(substitute(dat)))
   functionBodyout$function_calls[[length(functionBodyout$function_calls)+1]] <- (haul_to_trip_function)
   logbody$fishset_run <- list(infoBodyout, functionBodyout)
   write(jsonlite::toJSON(logbody, pretty = TRUE, auto_unbox = TRUE),paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""))
   assign("functionBodyout", value = functionBodyout, pos = 1)
   
  return(out)
}


