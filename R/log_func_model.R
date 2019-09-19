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
  user_function <- list()
  user_function$functionID <- deparse(substitute(x))
  user_function$args <- names(dput(formals(x)))
  user_function$kwargs <- list()
  user_function$output <- c('')
  user_function$msg <- args(x)
  functionBodyout$function_calls[[length(functionBodyout$function_calls)+1]] <- (user_function)
  logbody$fishset_run <- list(infoBodyout, functionBodyout)
  write(jsonlite::toJSON(logbody, pretty = TRUE, auto_unbox = TRUE), paste(getwd(), "/inst/Logs/", Sys.Date(), ".json", sep = ""))
  assign("functionBodyout", value = functionBodyout, pos = 1)

}
