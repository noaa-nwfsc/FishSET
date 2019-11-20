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


run_fishset_gui <- function(dat, project){#
  requireNamespace(shiny)
  requireNamespace(ggplot2)
  #----
  #Helper functions
  #----
  if(!exists('loc')){
    loc = getwd()
  } else {
    loc = loc
  }
  
  if(is.character(dat)==TRUE){
    suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), paste0(loc,"/fishset_db.sqlite")))
    dataset <- table_view(dat)
    DBI::dbDisconnect(fishset_db)
  } else {
    dataset <- dat  
  }
  
  if(is.character(dat)==TRUE){
    dat <- dat
  } else {
    dat <- deparse(substitute(dat))
  }
  # default global search value
  if (!exists("default_search")) {default_search <- ""}
  
  # default column search values
  if (!exists("default_search_columns")) {default_search_columns <- NULL}
  
  simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=" ")
  }
  
#shiny app call

shinyAppDir(paste0(loc, '/inst/ShinyFiles'))
}