#' @importFrom  jsonlite toJSON
#' 
.onLoad <- function(libname, pkgname) {
  if (!file.exists('Logs')){
    dir.create("Logs")
  }
}


.onAttach <- function(libname, pkgname) {
  packageStartupMessage('Welcome to FishSET \n If you would like to track or share your work, we recommend running scripts in a R notebook file.')
  if (!file.exists('Logs')){
    dir.create("Logs")
  }
}

