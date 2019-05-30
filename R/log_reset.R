#' Reset function calls saved in log file
#' 
#' @details Removes the three log file lists from the working directory
#' @export log_reset
#' @examples 
#' \dontrun{ 
#' log_reset() 
#' }
#' 

log_reset <- function(){
  if(exists('functionBodyout')){
  rm(functionBodyout)
  rm(logbody)
  rm(infoBodyout)
  }
}