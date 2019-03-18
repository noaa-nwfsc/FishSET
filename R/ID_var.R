#' Create ID variable from another variable.
#'
#' @param dataset dataframe or matrix
#' @param ... Column(s) that define the variable(s) over which to make new ID
#' @param newID name of new ID column
#' @export ID_var
#' @return Returns the dataframe with new ID variable included
#' @details Creates a variable to indicate distinct hauls or trips
#' 
# @examples dat <- ID_var(MainDataTable, newID='PermitID','GEAR_TYPE','TRIP_SEQ')


ID_var <- function(dataset, newID, ...) {
  
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
  
  ID_var_function <- list()
  ID_var_function$functionID <- 'ID_var'
  ID_var_function$args <- c(deparse(substitute(dataset)), newID)
  #ID_var_function$kwargs <- list('argList'=idmaker)
  functionBodyout$function_calls[[length(functionBodyout$function_calls)+1]] <- (ID_var_function)
  body$fishset_run <- list(infoBodyout, functionBodyout)
  write(jsonlite::toJSON(body, pretty = TRUE, auto_unbox = TRUE),paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""))
  list2env(functionBodyout, envir = .GlobalEnv)
  
  return(int)
}

