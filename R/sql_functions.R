#' Functions to work with sql database
#' @importFrom DBI dbConnect dbRemoveTable dbListTables dbExistsTable dbGetQuery

tablesDatabase <- function() { 
# View tables in the fishset_db database
  DBI::dbListTables(DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite"))
  }

tableFields <- function(table) { 
#' View fields in the selected table in the fishset_db database
#' @param table name of table in sql database
  DBI::dbListFields(DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite"), table) }

tableView <- function(table) { 
#' view the selected table in the fishset_db database
  #' @param table name of tablel in sql database
  DBI::dbGetQuery(DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite"), paste0("SELECT * FROM", paste0("'", noquote(table), "'"))) 
  }

tableRemove <- function(table) { 
#' remove a table from the fishset_db database
  #' @param table name of tablel in sql database
  DBI::dbRemoveTable(DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite"), table) 
  }

tableExists <- function(table) { 
#' Check whether a specific table exists in the fishset_db database
  #' @param table name of tablel in sql database
  DBI::dbExistsTable(DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite"), table) 
  }