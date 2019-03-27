#' Functions to work with sql database
#' @importFrom DBI dbConnect dbRemoveTable dbListTables dbExistsTable dbGetQuery
#' @export 

tables_database <- function() { 
# View tables in the fishset_db database
  DBI::dbListTables(DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite"))
  }

table_fields <- function(table) { 
# View fields in the selected table in the fishset_db database
#' @param table name of table in sql database
  return(DBI::dbListFields(DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite"), table)) 
  DBI::dbDisconnect(fishset_db)
  }

table_view <- function(table) { 
# view the selected table in the fishset_db database
  #' @param table name of tablel in sql database
  return(DBI::dbGetQuery(DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite"), 
                  paste0("SELECT * FROM", paste0("'", noquote(table), "'")))) 
  DBI::dbDisconnect(fishset_db)
  }

table_remove <- function(table) { 
# remove a table from the fishset_db database
  #' @param table name of tablel in sql database
  DBI::dbRemoveTable(DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite"), table) 
  DBI::dbDisconnect(fishset_db)
  }

table_exists <- function(table) { 
# Check whether a specific table exists in the fishset_db database
  #' @param table name of tablel in sql database
  DBI::dbExistsTable(DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite"), table) 
  DBI::dbDisconnect(fishset_db)
  }