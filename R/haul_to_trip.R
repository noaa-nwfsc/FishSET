#' Collapse the dataframe from haul to trip.
#' Each row should be a unique trip. Reduces the number of rows.
#' Unique trips are defined based on selected column(s). For example, landing permit number and disembared port. This id column is used to collapse the data to trip level.
#'
#' @param dataset dataframe or matrix over which to apply filter
#' @param dataindex Dataframe that contains information on each column of the dataset
#' @param varnameindex The column in dataindex that contains the column names of dataset. Should be 'variable_name' if dataindex was created using the load_maindata function
#' @param genTypeName column in dataindex containing information on the general category of each column in dataset (time, numeric, etc). Should be 'generalType' if dataindex created using the load_maindata function.
#' @param fun.time Time units for function. Defaults to minutes
#' @param fun.numeric Defaults to mean
#' @param ... Column(s) that define the individual trip.
#' @export haul_to_trip
#' @return Dataframe with each row representing a trip or haul
# @examples

# dat <- haul_to_trip(MainDataTable, 'MainDataTableInfo','variable_name','generalType',min,mean,'PERMIT','DISEMBARKED_PORT')


haul_to_trip <- function(dataset, dataindex, varnameindex='variable_name', genTypeName='generalType', 
                         fun.time = min, fun.numeric = mean, ...) {
  # Create a column that indicates unique trip levels
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
  
  
  drop <- if (length(which(grepl('DUR', names(int[,c(which(as.data.frame(dataIndex[dataIndex[, varnameindex] == 
                          colnames(int[,-which(colnames(int)=='rowID')]), genTypeName]) == "Time"))]), ignore.case=T)==T)) == 0 ) {
          0 } else {
            which(grepl('DUR', names(int[,c(which(as.data.frame(dataIndex[dataIndex[, varnameindex] == 
                         colnames(int[,-which(colnames(int)=='rowID')]), genTypeName]) == "Time"))]), ignore.case=T)==T)
          }
  # Collapse data based on rowID and defined function
  out <- data.frame(drop=rep(0, length(unique(int$rowID))))
   
    #Nothing listed
        if(length(which(is.na(as.data.frame(
          dataIndex[dataIndex[, varnameindex] %in% colnames(int[,-which(colnames(int)=='rowID')]), genTypeName]) == TRUE)))>0) {
          out <- cbind(out, 
                      stats::aggregate(int[,c(which(is.na(as.data.frame(dataIndex[dataIndex[, varnameindex] %in% 
                                                      colnames(int[,-which(colnames(int)=='rowID')]), genTypeName]) == TRUE)), 
                                             which(colnames(int)=='rowID'))], 
                                        list(int$rowID), FUN = head,  1)[,-1])
        } 
    #Time - not duration
      if(length(which(as.data.frame(
        dataIndex[dataIndex[, varnameindex] %in% colnames(int[,-which(colnames(int)=='rowID')]), genTypeName]) == "Time"))>0) {
        out2 <-  suppressWarnings ( stats::aggregate(cbind(
                as.data.frame(lapply(
            if(drop>0){    
                int[,c(which(as.data.frame(dataIndex[dataIndex[, varnameindex] %in% 
                                               colnames(int[,-which(colnames(int)=='rowID')]), genTypeName]) == "Time"))][-drop]
              } else{
                as.data.frame(int[,c(which(dataIndex[dataIndex[, varnameindex] %in% 
                                               colnames(int[,-which(colnames(int)=='rowID')]), genTypeName] == "Time"))])
              }, 
              date_parser)), rowID=int$rowID), list(int$rowID), match.fun(fun.time), na.rm=T))[,-1]
          }
        names(out2)[1:dim(out2)[2]-1] <- names(int)[which(dataIndex[dataIndex[, varnameindex] %in% 
                                                          colnames(int[,-which(colnames(int)=='rowID')]), genTypeName] == "Time")] 
        out <- cbind(out, out2)
      #Time - duration
    if(length(which(as.data.frame(
        dataIndex[dataIndex[, varnameindex] %in% colnames(int[,-which(colnames(int)=='rowID')]), genTypeName]) == "Time"))>0 & 
                  any(grepl('dur', colnames(int)[which(as.data.frame(dataIndex[dataIndex[, varnameindex] %in% 
                               colnames(int[,-which(colnames(int)=='rowID')]), genTypeName]) == "Time")], ignore.case=TRUE)) == TRUE) {
      out <- cbind(out, 
                   suppressWarnings ( stats::aggregate(cbind(
                    as.data.frame(
                      int[,c(which(as.data.frame(dataIndex[dataIndex[, varnameindex] %in% 
                                                 colnames(int[,-which(colnames(int)=='rowID')]), genTypeName]) == "Time"))][-
                                    which(grepl('dur', names(int[,c(which(as.data.frame(dataIndex[dataIndex[, varnameindex] %in% 
                                     colnames(int[,-which(colnames(int)=='rowID')]), genTypeName]) == "Time"))]), 
                                     ignore.case=T)==FALSE)]), rowID=int$rowID), list(int$rowID), match.fun(fun.time), na.rm = TRUE))[,-1])
      }
    #Other numeric  
    if(length(which(as.data.frame(
        dataIndex[dataIndex[, varnameindex] %in% colnames(int[,-which(colnames(int)=='rowID')]), genTypeName]) == "Other Numeric"))>0) {
      out <- cbind(out, 
                   stats::aggregate(int[,c(which(as.data.frame(dataIndex[dataIndex[, varnameindex] %in% 
                                                   colnames(int[,-which(colnames(int)=='rowID')]), genTypeName]) == "Other Numeric"), 
                                           which(colnames(int)=='rowID'))], 
                  list(int$rowID), match.fun(fun.numeric), na.action = na.pass)[,-1])
    }
    #Latitude
    if(length(which(as.data.frame(
        dataIndex[dataIndex[, varnameindex] %in% colnames(int[,-which(colnames(int)=='rowID')]), genTypeName]) == "Latitude"))>0) {
      out <- cbind(out, 
                   stats::aggregate(int[,c(which(as.data.frame(dataIndex[dataIndex[, varnameindex] %in% 
                                                 colnames(int[,-which(colnames(int)=='rowID')]), genTypeName]) == "Latitude"), 
                                           which(colnames(int)=='rowID'))], 
                  list(int$rowID),  FUN = head, 1)[,-1])
    }
    #Coded
    if(length(which(as.data.frame(
        dataIndex[dataIndex[, varnameindex] %in% colnames(int[,-which(colnames(int)=='rowID')]), genTypeName]) == "Code Numeric"))>0) {
      out <- cbind(out, 
                   stats::aggregate(int[,c(which(as.data.frame(dataIndex[dataIndex[, varnameindex] %in% 
                                                 colnames(int[,-which(colnames(int)=='rowID')]), genTypeName])== "Code Numeric"), 
                                           which(colnames(int)=='rowID'))], 
                  list(int$rowID),  FUN = head, 1)[,-1])
    }
    #Coded
    if(length(which(as.data.frame(
        dataIndex[dataIndex[, varnameindex] %in% colnames(int[,-which(colnames(int)=='rowID')]), genTypeName]) == "Code"))>0) {
      out <- cbind(out, 
                   stats::aggregate(int[,c(which(as.data.frame(dataIndex[dataIndex[, varnameindex] %in% 
                                                 colnames(int[,-which(colnames(int)=='rowID')]), genTypeName]) == "Code"), 
                                           which(colnames(int)=='rowID'))], 
              list(int$rowID),  FUN = head, 1)[,-1])
    }
    #Coded
    if(length(which(as.data.frame(
      dataIndex[dataIndex[, varnameindex] %in% colnames(int[,-which(colnames(int)=='rowID')]), genTypeName]) == "Code String"))>0) {
      out <- cbind(out, 
                   stats::aggregate(int[,c(which(as.data.frame(dataIndex[dataIndex[, varnameindex] %in%
                                                 colnames(int[,-which(colnames(int)=='rowID')]), genTypeName]) == "Code String"),
                                           which(colnames(int)=='rowID'))], 
              list(int$rowID),  FUN = head, 1)[,-1])
    }
    #Not in the dataIndex file
    if(length(colnames(int[,-grep('rowID', colnames(int[,-grep('rowID', colnames(int))]))])
              [!(colnames(int[,-grep('rowID', colnames(int))]) %in% dataIndex[, varnameindex])])>0) {
      out <- cbind(out, 
                   stats::aggregate(int[,c(colnames(int)[!(colnames(int) %in% dataIndex[, varnameindex])])], 
                                        list(int$rowID),  FUN = head, 1)[,-1])
    }

      
  out <-out[-which(colnames(out)=='rowID')[-length(which(colnames(out)=='rowID'))]]
  out <- out[-which(colnames(out)=='drop')]
  out <- data.frame(out)

    
  if(!exists('logbody')) { 
    logging_code()
  } 
  haul_to_trip_function <- list()
   haul_to_trip_function$functionID <- 'haul_to_trip'
   haul_to_trip_function$args <- c(paste0('dataset=',deparse(substitute(dataset))), dataindex, varnameindex, genTypeName)
   haul_to_trip_function$kwargs <- list(fun.time, fun.numeric, ...)
   haul_to_trip_function$output <- c(deparse(substitute(dataset)))
   functionBodyout$function_calls[[length(functionBodyout$function_calls)+1]] <- (haul_to_trip_function)
   logbody$fishset_run <- list(infoBodyout, functionBodyout)
   write(jsonlite::toJSON(logbody, pretty = TRUE, auto_unbox = TRUE),paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""))
   assign("functionBodyout", value = functionBodyout, pos = 1)
   
  return(out)
}


