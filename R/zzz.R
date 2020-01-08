
.onAttach <- function(libname, pkgname) {
  packageStartupMessage('Welcome to FishSET \n If you would like to track or share your work, we recommend running scripts in a R notebook file.')
}


.onLoad <- function(libname, pkgname) {
  options(locdatabase=paste0(system.file(package='FishSET'), 'fishset_db.sqlite'))
  
}
