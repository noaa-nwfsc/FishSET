log_reset <- function(){
#' Reset function calls saved in log file
#' @details Removes the three log file lists from the working directory
#' @export log_reset
#' @examples 
#' \dontrun{ 
#' log_reset() 
#' }
#' 

  if(exists('functionBodyout')){
  rm('functionBodyout')
  rm('logbody')
  rm('infoBodyout')
  }
}

log_call <- function(fun.name){
  #' Reset function calls saved in log file
  #' @param fun.name Function name
  #' @details uplodate log file
  #' @importFrom jsonlite read_json toJSON
  #' 
  
if(!file_test("-f", paste(getwd(), "/inst/Logs/", Sys.Date(), ".json", sep = ""))) { 
  logbody <- list()
  infoBodyout <- list()
  functionBodyout <- list()
  infobody <- list()
  
  infobody$rundate <- Sys.Date()
  infoBodyout$info <- list(infobody)
  
  functionBodyout$function_calls <- list()
  
  logbody$fishset_run <- list(infoBodyout, functionBodyout)
  functionBodyout$function_calls[[length(functionBodyout$function_calls)+1]] <- fun.name
  logbody$fishset_run <- list(infoBodyout, functionBodyout)
} else {
  logbody <- jsonlite::read_json(paste(getwd(), '/inst/Logs/', Sys.Date(), ".json", sep = ""))
  logbody$fishset_run[[2]]$function_calls[[length(logbody$fishset_run[[2]]$function_calls)+1]] <- fun.name
}
  write(jsonlite::toJSON(logbody, pretty = TRUE, auto_unbox = TRUE),paste(getwd(), "/inst/Logs/", Sys.Date(), ".json", sep = ""))
  
}
