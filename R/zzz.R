#' @importFrom  jsonlite toJSON
#' 
.onLoad <- function(libname, pkgname) {
  if (!file.exists('Logs')){
    dir.create("Logs")
  }
}


.onAttach <- function(libname, pkgname) {
  packageStartupMessage('Welcome to FishSET')
  if (!file.exists('Logs')){
    dir.create("Logs")
  }
}

