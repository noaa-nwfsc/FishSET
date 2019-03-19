#' Collapse the dataframe from haul to trip.
#' Each row should be a unique trip. Reduces the number of rows.
#' Unique trips are defined based on selected column(s). For example, landing permit number and disembared port. This id column is used to collapse the data to trip level.
#'
#' @param dataset dataframe or matrix over which to apply filter
#' @param dataindex Dataframe that contains information on each column of the dataset
#' @param varnameindex The column in dataindex that contains the column names of dataset
#' @param genTypeName column in dataindex containing information on the general category of each column in dataset (time, numeric, etc)
#' @param fun.time Time units for function. Defaults to minutes
#' @param fun.numeric Defaults to mean
#' @param ... Column(s) that define the individual trip.
#' @export haul_to_trip
#' @return Dataframe with each row representing a trip or haul
# @examples

# dat <- haul_to_trip(MainDataTable, MainDataTableInfo,'variable_name','generalType',min,mean,'PERMIT','DISEMBARKED_PORT')


haul_to_trip <- function(dataset, dataindex, varnameindex, genTypeName, fun.time = min, fun.numeric = mean, ...) {
  # Create a column that indicates unique trip levels

   # write(layout.json.ed(trace, "haul_to_trip", deparse(substitute(dataset)), x = "", 
   #                    msg = paste("dataindex:", deparse(substitute(dataindex)), ", varnameindex:", 
   #                                varnameindex, ", genTypeName:", genTypeName, ", fun.time:", deparse(substitute(fun.time)), 
   #                                ", fun.numeric:", deparse(substitute(fun.numeric)), sep = "")), 
   #     paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""), append = T)

    argList <- (as.character(match.call(expand.dots = FALSE)$...))
  
  idmaker = function(vec) {
    return(paste(sort(vec), collapse = ""))
  }
  int <- as.data.frame(cbind(dataset, rowID = as.numeric(factor(apply(as.matrix(dataset[, eval(substitute(argList))]), 1, idmaker)))))
  int <- int[, c(colnames(sapply(dataindex[[varnameindex]], grepl, colnames(int))), "rowID")]
  
  # Handling of empty variables
  if (any(apply(int, 2, function(x) all(is.na(x))) == TRUE)) {
    int <- int[, -which(apply(int, 2, function(x) all(is.na(x))) == TRUE)]
  } else {
    int <- int
  }
  
  # Collapse data based on rowID and defined function
  out <- 
    #Nothing listed
      cbind(
        stats::aggregate(int[,c(which(is.na(as.data.frame(dataindex[dataindex[, varnameindex] == colnames(int[,-which(colnames(int)=='rowID')]), 
                                                                 genTypeName]) == TRUE)), which(colnames(int)=='rowID'))], 
               list(int$rowID), FUN = head,  1)[,-1],
    #Time - note duration
    stats::aggregate(cbind(
        as.data.frame(lapply(
          int[,c(which(as.data.frame(dataindex[dataindex[, varnameindex] == 
                                                         colnames(int[,-which(colnames(int)=='rowID')]), 'generalType']) == "Time"))][-
          which(grepl('DURATION', names(int[,c(which(as.data.frame(dataindex[dataindex[, 'variable_name'] == 
            colnames(int[,-which(colnames(int)=='rowID')]), genTypeName]) == "Time"))]), ignore.case=T)==T)],  as.Date)), rowID=int$rowID), 
                list(int$rowID), fun.time, na.rm = TRUE)[,-1],
      #Time - duration
    stats::aggregate(cbind(
        as.data.frame(
          int[,c(which(as.data.frame(dataindex[dataindex[, varnameindex] == 
                                                         colnames(int[,-which(colnames(int)=='rowID')]), 'generalType']) == "Time"))][-
          which(grepl('DURATION', names(int[,c(which(as.data.frame(dataindex[dataindex[, 'variable_name'] == 
                colnames(int[,-which(colnames(int)=='rowID')]), genTypeName]) == "Time"))]), ignore.case=T)==FALSE)]), rowID=int$rowID), 
        list(int$rowID), fun.time, na.rm = TRUE)[,-1],
      
    #Other numeric  
    stats::aggregate(int[,c(which(as.data.frame(dataindex[dataindex[, varnameindex] == colnames(int[,-which(colnames(int)=='rowID')]), 
                                                     genTypeName]) == "Other Numeric"), which(colnames(int)=='rowID'))], 
                list(int$rowID), fun.numeric, na.action = na.pass)[,-1],
    #Latitude
    stats::aggregate(int[,c(which(as.data.frame(dataindex[dataindex[, varnameindex] == colnames(int[,-which(colnames(int)=='rowID')]), 
                                                     genTypeName]) == "Latitude"), which(colnames(int)=='rowID'))], 
                list(int$rowID),  FUN = head, 1)[,-1],
      
    #Coded
    stats::aggregate(int[,c(which(as.data.frame(dataindex[dataindex[, varnameindex] == colnames(int[,-which(colnames(int)=='rowID')]), 
                                                     genTypeName])== "Code Numeric"), which(colnames(int)=='rowID'))], 
                list(int$rowID),  FUN = head, 1)[,-1],
    #Coded
    stats::aggregate(int[,c(which(as.data.frame(dataindex[dataindex[, varnameindex] == colnames(int[,-which(colnames(int)=='rowID')]), 
                                                   genTypeName]) == "Code"), which(colnames(int)=='rowID'))], 
              list(int$rowID),  FUN = head, 1)[,-1],
    #Coded
    stats::aggregate(int[,c(which(as.data.frame(dataindex[dataindex[, varnameindex] == colnames(int[,-which(colnames(int)=='rowID')]), 
                                                   genTypeName]) == "Code String"), which(colnames(int)=='rowID'))], 
              list(int$rowID),  FUN = head, 1)[,-1]
    
    
      )
      
  out <-out[-which(colnames(out)=='rowID')[-1]]
  out <- data.frame(out)

    
    
   #write(layout.json.ed(trace, 'haul_to_trip', dataset=deparse(substitute(dataset)), x='', 
   #                    msg=paste('dataindex:', deparse(substitute(dataindex)),  ', varnameindex:', varnameindex, ',
   #                             genTypeName:', genTypeName, ',  fun.time:' , fun.time, ', fun.numeric:', fun.numeric)), 
   #     paste(getwd(),'/Logs/',Sys.Date(),'.json', sep=''), append=T )
  
  if(!exists('logbody')) { 
    logging_code()
  } 
  haul_to_trip_function <- list()
   haul_to_trip_function$functionID <- 'haul_to_trip'
   haul_to_trip_function$args <- c(deparse(substitute(dataset)), deparse(substitute(dataindex)), varnameindex, genTypeName)
   #haul_to_trip_function$kwargs <- list('fun.time'=fun.time, 'fun.numeric'=fun.numeric, 'argList'=idmaker)
   functionBodyout$function_calls[[length(functionBodyout$function_calls)+1]] <- (haul_to_trip_function)
   logbody$fishset_run <- list(infoBodyout, functionBodyout)
   write(jsonlite::toJSON(logbody, pretty = TRUE, auto_unbox = TRUE),paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""))
   assign("functionBodyout", value = functionBodyout, pos = 1)
   
  #return(out)
}


