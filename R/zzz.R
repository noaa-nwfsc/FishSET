#' @import RSQLite
#' @import jsonlite
#' 
.onLoad <- function(libname, pkgname) {
  packageStartupMessage('Welcome to FishSET')
  fishset_db <- DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite")
  if (!file.exists('Logs')){
    dir.create("Logs")
  }
}

fishset_db <- DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite")

.onAttach <- function(libname, pkgname) {
  fishset_db <- DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite")
  if (!file.exists('Logs')){
    dir.create("Logs")
  }
  writeLines(layout.json.ed(trace, '', '', 'LogFile'), paste(getwd(),'/Logs/',Sys.Date(),'.json', sep=''))
}

.onDetach <- function(libpath){
  DBI::dbDisconnect(fishset_db)
  close(fileConn)
}