#' Create ID variable from one or more variables
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
  out <- data_pull(dat)
  dat <- out$dat
  dataset <- out$dataset
  
  if(grepl('input', as.character(match.call(expand.dots = FALSE)$...)[1])==TRUE){
    argList <- eval(...) } else {
    argList <- (as.character(match.call(expand.dots = FALSE)$...))
  }
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
  ID_var_function$args <- c(dat, newID)
  ID_var_function$kwargs <- list('argList'=idmaker)
  ID_var_function$output <- c('')
  log.call(ID_var_function)
  return(int)
}

