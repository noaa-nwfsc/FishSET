#' SQL call functions
#' @description Functions call sql functions to view tables in the sqlite database. Functions allow users to view names of tables in the database, view fields of selected table, view the selected table, remove tables from the database, and check whether a specific table exists in the database.

tables_database <- function() { 
#' View names of tables in the fishset_db database
#' @export tables_database
#' @importFrom DBI dbConnect dbRemoveTable dbListTables dbExistsTable dbGetQuery
#'  @examples 
#' \dontrun{ 
#' tables_database()
#' }
  
  fishset_db <- DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite")
  return(DBI::dbListTables(fishset_db))
  DBI::dbDisconnect(fishset_db)
  }

table_fields <- function(table) { 
#' Lists fields for the selected table in the fishset_db database
  #' @param table name of table in sql database
  #' @export table_fields
  #' @description For a dataframe or matrix, returns column names, for a list, returns names of lists.
  #' @importFrom DBI dbConnect dbDisconnect dbListFields  
  #'  @examples 
  #' \dontrun{ 
  #' table_fields('MainDataTable')
  #' }
  
  fishset_db <- DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite")
  return(DBI::dbListFields(fishset_db, table)) 
  DBI::dbDisconnect(fishset_db)
  }

table_view <- function(table) { 
#' View the selected table in the fishset_db database
  #' @param table name of table in sql database
  #' @export table_view
  #' @description Prints the entire table of selected table.
  #' @importFrom DBI dbConnect dbDisconnect  dbGetQuery
  #' \dontrun{ 
  #' head(table_view('MainDataTable'))
  #' }
  
  fishset_db <- DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite")
  return(DBI::dbGetQuery(fishset_db, paste0("SELECT * FROM", paste0("'", noquote(table), "'")))) 
  DBI::dbDisconnect(fishset_db)
  }

table_remove <- function(table) { 
#' Remove a table from the fishset_db database
  #' @param table name of table in sql database
  #' @export table_remove
  #' @importFrom DBI dbConnect dbDisconnect dbRemoveTable 
  #' \dontrun{ 
  #' table_remove('MainDataTable')
  #' }
  
  fishset_db <-DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite")
  DBI::dbRemoveTable(fishset_db, table) 
  DBI::dbDisconnect(fishset_db)
  }

table_exists <- function(table) { 
#' Check whether a specific table exists in the fishset_db database
  #' @param table name of tablel in sql database
 #' @export table_exists
  #' @importFrom DBI dbConnect dbDisconnect dbExistsTable   
  #' \dontrun{ 
  #' table_view(MainDataTable)
  #' }
  
  fishset_db <- DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite")
  DBI::dbExistsTable(fishset_db, table) 
  DBI::dbDisconnect(fishset_db)
}
