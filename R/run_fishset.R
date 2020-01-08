#' Interactive tool to run FishSET functions 
#' Runs functions associated with loading data, exploring data, checking for data quality issues, generating new variables, and basic data analyis function.

#' run_fishset_gui
#'
#' @param dat Main data frame containing data on hauls or trips. Table in fishset_db database should contain the string `MainDataTable`. Can be NULL if importing data through the app.
#' @param project Name of project. Parameter is used to generate meaningful table names in fishset_db database.
#' @import shiny
#' @import ggplot2
#' @importFrom DT DTOutput renderDT
#' @importFrom ggpubr ggarrange
#' @importFrom ggcorrplot ggcorrplot
#' @importFrom DBI  dbDisconnect dbConnect dbListTables dbWriteTable 
# @importFrom gridExtra grid.table
#' @importFrom stringi stri_count_regex
#' @export run_fishset_gui
#' @details Opens an interactive page that allows users to select which functions to run by clicking check boxes. 
#' @examples
#' \dontrun{
#' run_fishset_gui(pcodMainDataTable, 'pcod')
#' }


run_fishset_gui <- function(project, dat=NULL){

    appDir <- system.file("ShinyFiles", "MainApp", package = "FishSET")
    if (appDir == "") {
      stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
    }
    
    shiny::runApp(appDir, display.mode = "normal")

  
#shiny app call
    loc = system.file(package='FishSET')
  
  
#  if(grepl('ShinyFiles', getwd())){
#    setwd('..') 
#  }
#  if(grepl('inst', getwd())){
#    setwd('..')
#  }
 # if(!exists('loc')){
#    loc = getwd()
 # } else {
#    loc = loc
#  }
  
    if(!is.null(dat)){
  out <- data_pull(dat)
  dat <- out$dat
  dataset <- out$dataset
    }
  
#shinyAppDir(paste0(loc, '/inst/ShinyFiles'))
}
