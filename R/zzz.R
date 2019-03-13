#' @importFrom  jsonlite toJSON
#' 
.onLoad <- function(libname, pkgname) {
 # fishset_db <- DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite")
  if (!file.exists('Logs')){
    dir.create("Logs")
  }
  #other layout option <-
  body <- list()
  infoBodyout <<- list()
  functionBodyout <<- list()
  infobody <- list()
  
  infobody$rundate <- Sys.Date()
  infoBodyout$info <- list(infobody)
  
  functionBodyout$function_calls <- list()
  
  body$fishset_run <- list(infoBodyout, functionBodyout)
  
  jsonlite::toJSON(body, pretty = TRUE, auto_unbox = TRUE)
}

#fishset_db <- DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite")

.onAttach <- function(libname, pkgname) {
  packageStartupMessage('Welcome to FishSET')
#  fishset_db <- DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite")
  if (!file.exists('Logs')){
    dir.create("Logs")
    #other layout option <-
    body <- list()
    infoBodyout <- list()
    functionBodyout <- list()
    infobody <- list()
    
    infobody$rundate <- Sys.Date()
    infoBodyout$info <- list(infobody)
    
    functionBodyout$function_calls <- list()
    
    body$fishset_run <- list(infoBodyout, functionBodyout)
    
    jsonlite::toJSON(body, pretty = TRUE, auto_unbox = TRUE)
    list2env(functionBodyout, envir = .GlobalEnv)
  }
#  writeLines(layout.json.ed(trace, '', '', 'LogFile'), paste(getwd(),'/Logs/',Sys.Date(),'.json', sep=''))
}

.onDetach <- function(libpath){
  DBI::dbDisconnect(fishset_db)
  #close(fileConn)
}