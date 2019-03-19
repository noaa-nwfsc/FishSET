#' Log user created functions or models
#'
#' @param x Name of cunction
#' @details Logs names of the function, parameters in function, and function call in the dated log file.
#' @export log_func_model
# example log_func_model(nan_filter)

log_func_model <- function(x) {

  if(!exists('logbody')) { 
    logging_code()
  } 
  user_function <- list()
  user_function$functionID <- deparse(substitute(x))
  user_function$args <- names(dput(formals(x)))
  user_function$msg <- args(x)
  functionBodyout$function_calls[[length(functionBodyout$function_calls)+1]] <- (user_function)
  logbody$fishset_run <- list(infoBodyout, functionBodyout)
  write(jsonlite::toJSON(logbody, pretty = TRUE, auto_unbox = TRUE), paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""))
  assign("functionBodyout", value = functionBodyout, pos = 1)

}
