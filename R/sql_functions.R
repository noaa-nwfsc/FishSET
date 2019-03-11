# Functions to work with sql database

#' @param table name of tablel in sql database
#' @export

#View tables in the fishset_db database
tablesDatabase <- function() { 
  dbListTables(DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite"))
  }

#View fields in the selected table in the fishset_db database
tableFields <- function(table) { 
  dbListFields(DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite"), table) }

#view the selected table in the fishset_db database
tableView <- function(table) { 
  DBI::dbGetQuery(DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite"), paste0("SELECT * FROM", paste0("'", noquote(table), "'"))) 
  }

#remove a table from the fishset_db database
tableRemove <- function(table) { 
  DBI::dbRemoveTable(DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite"), table) 
  }

#Check whether a specific table exists in the fishset_db database
tableExists <- function(table) { 
  dbExistsTable(DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite"), table) 
  }