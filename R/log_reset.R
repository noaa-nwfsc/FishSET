#' Reset function calls saved in log file
#' 
#' @details Removes the three log file lists from the working directory
#' @export log_reset
#' 

log_reset <- function(){
  rm(functionBodyout)
  rm(logbody)
  rm(infoBodyout)
}