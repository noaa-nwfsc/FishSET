#' Create ID variable from another variable.
#'
#' @param dataset dataframe or matrix
#' @param ... Column(s) that define the variable(s) over which to make new ID
#' @param newID name of new ID column
#' @export ID_var
#' @return Returns the dataframe with new ID variable included
#' @details Creates an variable to indicate distinct hauls or trips
#' 
# @examples dat <- IDvar(MainDataTable, newID='PermitID','GEAR_TYPE','TRIP_SEQ')


ID_var <- function(dataset, newID, ...) {
  
  argList <- (as.character(match.call(expand.dots = FALSE)$...))
  
  idmaker = function(vec) {
    return(paste(sort(vec), collapse = ""))
  }
  int <- as.data.frame(cbind(dataset, rowID = as.numeric(factor(apply(as.matrix(dataset[, eval(substitute(argList))]), 1, idmaker)))))
  colnames(int)[which(colnames(int) == "rowID")] = newID
  
  # logging function information
  write(layout.json.ed(trace, "ID_var", deparse(substitute(dataset)), newID, 
                       msg = paste(newID, "created based on", deparse(substitute(argList)))),
        paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""), append = T)
  
  return(int)
}

