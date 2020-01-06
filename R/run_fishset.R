#' Interactive tool to run FishSET functions 
#' Runs functions associated with loading data, exploring data, checking for data quality issues, generating new variables, and basic data analyis function.

#' run_fishset_gui
#'
#' @param dat Main data frame containing data on hauls or trips. Table in fishset_db database should contain the string `MainDataTable`.
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


run_fishset_gui <- function(dat, project){

#shiny app call
  if(!exists('loc')){
    loc = getwd()
  } else {
    loc = loc
  }
  
shinyAppDir(paste0(loc, '/inst/ShinyFiles'))
}
