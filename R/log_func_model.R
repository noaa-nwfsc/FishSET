#' Log user-created functions or models
#'
#' @param x Name of function
#' @details Logs names of the function, parameters in function, and function call in the log file. Use this function to log user-defined likelihood functions.
#' @export log_func_model
#' @examples 
#' \dontrun{
#' my_func <- function(a,b){ a + b }
#' log_func_model(my_func)
#' }
#' 

log_func_model <- function(x) {
    
    user_function <- list()
    user_function$functionID <- deparse(substitute(x))
    user_function$args <- names(dput(formals(x)))
    user_function$kwargs <- list()
    user_function$output <- c("")
    user_function$msg <- args(x)
    log_call(user_function)
    
    write(jsonlite::toJSON(logbody, pretty = TRUE, auto_unbox = TRUE), paste0(loclog(), Sys.Date(), ".json", sep = ""))
}
