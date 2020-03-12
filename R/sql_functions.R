# SQL call functions
# @description Functions call sql functions to view tables in the FishSET database. Functions allow users to view names of tables in the database, view fields of selected 
# table, view the selected table, remove tables from the database, and check whether a specific table exists in the database.

tables_database <- function() { 
#' View names of tables in the FishSET database
#' @description Function utilizes sql functions to view names of tables in the FishSET database. 
#' @export tables_database
#' @importFrom DBI dbConnect dbRemoveTable dbListTables dbExistsTable dbGetQuery
#' @examples 
#' \dontrun{  
#' tables_database() 
#' }
  
  fishset_db <- suppressWarnings(DBI::dbConnect(RSQLite::SQLite(), locdatabase()))
  return(DBI::dbListTables(fishset_db))
  DBI::dbDisconnect(fishset_db)
  }

table_fields <- function(table) { 
#' Lists fields for the selected FishSET database table
  #' @param table name of table in FishSET database
  #' @export table_fields
  #' @description Function utilizes sql functions to view tables in the FishSET database. View fields of selected table.
  #' @importFrom DBI dbConnect dbDisconnect dbListFields  
  #' @examples 
  #' \dontrun{  
  #' table_fields('MainDataTable') 
  #' }

  suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase()))
  return(DBI::dbListFields(fishset_db, table)) 
  DBI::dbDisconnect(fishset_db)
  }

table_view <- function(table) { 
#' View the selected FishSETn database table 
  #' @param table name of table in FishSET database
  #' @export table_view
  #' @description Function utilizes sql functions to view or call the selected table from the FishSET database in a function. 
  #' Use this function if you do not want the data loaded into the global environment. 
  #' @importFrom DBI dbConnect dbDisconnect  dbGetQuery
  #' @examples 
  #' \dontrun{  
  #' head(table_view('MainDataTable')) 
  #' }
  
  if(table_exists(table)==FALSE){
    return('Table not found. Check spelling.')
  } else {
  suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase()))
  return(DBI::dbGetQuery(fishset_db, paste0("SELECT * FROM", paste0("'", noquote(table), "'")))) 
  DBI::dbDisconnect(fishset_db)
  }
}

table_remove <- function(table) { 
#' Remove a table from the FishSET database
  #' @param table Name of table in FishSET database
  #' @export table_remove
  #' @details Function utilizes sql functions to permanently remove a table from the FishSET database.
  #' @importFrom DBI dbConnect dbDisconnect dbRemoveTable 
  #' @examples 
  #' \dontrun{  
  #' table_remove('MainDataTable') 
  #' }
  
  suppressWarnings(fishset_db <-DBI::dbConnect(RSQLite::SQLite(),locdatabase()))
  DBI::dbRemoveTable(fishset_db, table) 
  DBI::dbDisconnect(fishset_db)
  }

table_exists <- function(table) { 
#' Check if table exists in the FishSET database
  #' @param table Name of table in FishSET database
  #' @export table_exists
  #' @description Function utilizes sql functions to check if a table exists in the FishSET database.
  #' @return True or False
  #' @importFrom DBI dbConnect dbDisconnect dbExistsTable   
  #' @examples 
  #' \dontrun{  
  #' table_exists('MainDataTable') 
  #' }
  
  suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase()))
  return(DBI::dbExistsTable(fishset_db, table)) 
  DBI::dbDisconnect(fishset_db)
}

model_out_view <- function(table){
  #' View discrete choice model output
  #' @param table  Table name in FishSET database. Should contain the phrase modelout.
  #' @export
  #' @description Returns output from running the discretefish_subroutine function. The table parameter must be the full name of the table name in the FishSET database.
  #' @examples 
  #' \dontrun{
  #' model_out_view('pcodmodelout20190604')
  #' }
  #
  if(table_exists(table)==FALSE){
    return('Table not found. Check spelling.')
  } else {
  suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(),locdatabase()))
  x <- unserialize(DBI::dbGetQuery(fishset_db, paste0("SELECT data FROM ", table, " LIMIT 1"))$data[[1]])
  return(x)
  DBI::dbDisconnect(fishset_db)
  }
}

globalcheck_view <- function(table){
  #' View error output from discrete choice model 
  #' @param table  Table name in FishSET database. Should contain the phrase modelout.
  #' @export
  #' @description Returns error output from running the discretefish_subroutine function. The table parameter must be the full name of the table name in the FishSET database.
  #' @examples 
  #' \dontrun{
  #' globalcheck_view('pcodldglobalcheck20190604')
  #' }
 
  if(table_exists(table)==FALSE){
    return('Table not found. Check spelling.')
  } else {
  suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase()))
  x <- unserialize(DBI::dbGetQuery(fishset_db, paste0("SELECT data FROM ", table , " LIMIT 1"))$data[[1]])
  return(x)
  DBI::dbDisconnect(fishset_db)
  }
}
