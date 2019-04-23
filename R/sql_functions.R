#' Functions to work with sql database

tables_database <- function() { 
#' View tables in the fishset_db database
#' @export tables_database
#' @importFrom DBI dbConnect dbRemoveTable dbListTables dbExistsTable dbGetQuery
  DBI::dbListTables(DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite"))
  DBI::dbDisconnect(fishset_db)
  }

table_fields <- function(table) { 
#' View fields in the selected table in the fishset_db database
  #' @param table name of table in sql database
  #' @export table_fields
  #' @importFrom DBI dbConnect dbDisconnect dbRemoveTable dbListFields  
  return(DBI::dbListFields(DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite"), table)) 
  DBI::dbDisconnect(fishset_db)
  }

table_view <- function(table) { 
#' view the selected table in the fishset_db database
  #' @param table name of tablel in sql database
  #' @export table_view
  #' @importFrom DBI dbConnect dbDisconnect dbRemoveTable  dbGetQuery
  return(DBI::dbGetQuery(DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite"), 
                  paste0("SELECT * FROM", paste0("'", noquote(table), "'")))) 
  DBI::dbDisconnect(fishset_db)
  }

table_remove <- function(table) { 
#' remove a table from the fishset_db database
  #' @param table name of tablel in sql database
  #' @export table_remove
  #' @importFrom DBI dbConnect dbDisconnect dbRemoveTable   
  DBI::dbRemoveTable(DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite"), table) 
  DBI::dbDisconnect(fishset_db)
  }

table_exists <- function(table) { 
#' Check whether a specific table exists in the fishset_db database
  #' @param table name of tablel in sql database
 #' @export table_exists
  #' @importFrom DBI dbConnect dbDisconnect dbExistsTable 
  DBI::dbExistsTable(DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite"), table) 
  DBI::dbDisconnect(fishset_db)
  }