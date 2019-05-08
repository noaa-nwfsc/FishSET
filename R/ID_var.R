#' Create ID variable from one or more variable.s
#'
#' @param dat Main data frame over which to apply function. Table in fishset_db database should contain the string `MainDataTable`.
#' @param newID name of new ID column
#' @param ... Column(s) in main data frame that define unique observations.
#' @export ID_var
#' @return Data frame with `newID` ID variable included.
#' @details Function is used to create a variable that indicates distinct hauls or trips.
#' 
#' @examples 
#' \dontrun{
#' dat <- ID_var(MainDataTable, newID='PermitID','GEAR_TYPE','TRIP_SEQ')
#' }


ID_var <- function(dat, newID, ...) {
  
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
  
  
  argList <- (as.character(match.call(expand.dots = FALSE)$...))
  
  idmaker = function(vec) {
    return(paste(sort(vec), collapse = ""))
  }
  int <- as.data.frame(cbind(dataset, rowID = as.numeric(factor(apply(as.matrix(dataset[, eval(substitute(argList))]), 1, idmaker)))))
  colnames(int)[which(colnames(int) == "rowID")] = newID
  
  # logging function information
  #write(layout.json.ed(trace, "ID_var", deparse(substitute(dataset)), newID, 
  #                     msg = paste(newID, "created based on", deparse(substitute(argList)))),
  #      paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""), append = T)
  
  if(!exists('logbody')) { 
    logging_code()
  } 
  ID_var_function <- list()
  ID_var_function$functionID <- 'ID_var'
  ID_var_function$args <- c(deparse(substitute(dat)), newID)
  ID_var_function$kwargs <- list('argList'=idmaker)
  ID_var_function$output <- c('')
  functionBodyout$function_calls[[length(functionBodyout$function_calls)+1]] <- (ID_var_function)
  logbody$fishset_run <- list(infoBodyout, functionBodyout)
  write(jsonlite::toJSON(logbody, pretty = TRUE, auto_unbox = TRUE),paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""))
  assign("functionBodyout", value = functionBodyout, pos = 1)
  
  return(int)
}

