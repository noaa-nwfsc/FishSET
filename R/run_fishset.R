#' Interactive tool to run FishSET functions 
#' Runs functions associated with loading data, exploring data, checking for data quality issues, generating new variables, and basic data analysis function.

#' run_fishset_gui
#'
#' @import shiny
#' @import ggplot2
#' @importFrom DT DTOutput renderDT
#' @importFrom ggpubr ggarrange
#' @importFrom ggcorrplot ggcorrplot
#' @importFrom DBI  dbDisconnect dbConnect dbListTables dbWriteTable 
# @importFrom gridExtra grid.table
#' @importFrom stringi stri_count_regex
#' @importFrom shinycssloaders withSpinner
#' @export run_fishset_gui
#' @details Opens an interactive page that allows users to select which functions to run by clicking check boxes. 
#' @examples
#' \dontrun{
#' run_fishset_gui()
#' }


run_fishset_gui <- function(){
   
    appDir <- system.file("ShinyFiles", "MainApp", package = "FishSET")
    if (appDir == "") {
      stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
    }
    
    shiny::runApp(appDir, display.mode = "normal")

  
#shiny app call

  
    if(!is.null(dat)){
  out <- data_pull(dat)
  dat <- out$dat
  dataset <- out$dataset
    }
  
#shinyAppDir(paste0(loc, '/inst/ShinyFiles'))
}
