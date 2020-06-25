#' Create ID variable
#' 
#' Create ID variable from one or more variables
#'
#' @param dat Primary data containing information on hauls or trips. 
#' Table in FishSET database contains the string 'MainDataTable'.
#' @param newID String, name of new ID column.
#' @param vars Character string, additional column(s) in \code{dat} that define unique observations.
#' @export ID_var
#' @return Data frame with `newID` ID variable included.
#' @details ID variable can be based on a single or multiple variables.
#' 
#' @examples 
#' \dontrun{
#' pcodMainDataTable <- ID_var(pcodMainDataTable, newID='PermitID', c('GEAR_TYPE','TRIP_SEQ'))
#' }


ID_var <- function(dat, newID, vars) {
    
    # Call in datasets
    out <- data_pull(dat)
    dat <- out$dat
    dataset <- out$dataset
    
    idmaker = function(vec) {
        return(paste(sort(vec), collapse = ""))
    }
    
    int <- as.data.frame(cbind(dataset, rowID = as.numeric(factor(apply(as.matrix(dataset[, vars]), 1, idmaker)))))
    colnames(int)[which(colnames(int) == "rowID")] = newID
    
    # logging function information write(layout.json.ed(trace, 'ID_var', deparse(substitute(dataset)), newID, msg = paste(newID, 'created based on',
    # deparse(substitute(argList)))), paste(getwd(), '/Logs/', Sys.Date(), '.json', sep = ''), append = T)
    
    ID_var_function <- list()
    ID_var_function$functionID <- "ID_var"
    ID_var_function$args <- list(dat, newID, vars)
    ID_var_function$ouput <- list(dat)

    log_call(ID_var_function)
    return(int)
}

