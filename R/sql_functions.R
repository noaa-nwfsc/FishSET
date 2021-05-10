# SQL call functions @description Functions call sql functions to view tables in the FishSET database. Functions allow users to view names of tables
# in the database, view fields of selected table, view the selected table, remove tables from the database, and check whether a specific table exists
# in the database.

tables_database <- function() {
  #' View names of tables in the FishSET database
  #'
  #' Wrapper for \code{\link[DBI]{dbListTables}}. View names of tables in the FishSET database.
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
  #' Lists fields for FishSET database table
  #' @param table String, name of table in FishSET database. Table name must be in quotes.
  #' @export table_fields
  #' @description Wrapper for \code{\link[DBI]{dbListFields}}.  View fields of selected table.
  #' @importFrom DBI dbConnect dbDisconnect dbListFields
  #' @examples
  #' \dontrun{
  #' table_fields('pollockMainDataTable')
  #' }

  suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase()))
  return(DBI::dbListFields(fishset_db, table))
  DBI::dbDisconnect(fishset_db)
}

table_view <- function(table) {
  #' View FishSET database table
  #' @param table String, name of table in FishSET database. Table name must be in quotes.
  #' @export table_view
  #' @description Wrapper for \code{\link[DBI]{dbGetQuery}}. View or call the selected table from the FishSET database.
  #' @importFrom DBI dbConnect dbDisconnect  dbGetQuery
  #' @examples
  #' \dontrun{
  #' head(table_view('pollockMainDataTable'))
  #' }

  if (table_exists(table) == FALSE) {
    return("Table not found. Check spelling.")
  } else {
    suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase()))
    return(DBI::dbGetQuery(fishset_db, paste0("SELECT * FROM", paste0("'", noquote(table), "'"))))
    DBI::dbDisconnect(fishset_db)
  }
}

table_remove <- function(table) {
  #' Remove table from FishSET database
  #'
  #' Wrapper for \code{\link[DBI]{dbRemoveTable}}. Remove a table from the FishSET database.
  #' @param table String, name of table in FishSET database. Table name must be in quotes.
  #' @export table_remove
  #' @details Function utilizes sql functions to remove tables from the FishSET database.
  #' @importFrom DBI dbConnect dbDisconnect dbRemoveTable
  #' @examples
  #' \dontrun{
  #' table_remove('pollockMainDataTable')
  #' }

  suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase()))
  DBI::dbRemoveTable(fishset_db, table)
  DBI::dbDisconnect(fishset_db)
}

table_exists <- function(table) {
  #' Check if table exists in the FishSET database
  #' @param table Name of table in FishSET database.Table name must be in quotes.
  #' @export table_exists
  #' @description Wrapper for \code{\link[DBI]{dbExistsTable}}. Check if a table exists in the FishSET database.
  #' @return Returns a logical statement of table existence.
  #' @importFrom DBI dbConnect dbDisconnect dbExistsTable
  #' @examples
  #' \dontrun{
  #' table_exists('pollockMainDataTable')
  #' }

  suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase()))
  return(DBI::dbExistsTable(fishset_db, table))
  DBI::dbDisconnect(fishset_db)
}

model_out_view <- function(table) {
  #' Load model output to console
  #'
  #' Returns output from running \code{\link{discretefish_subroutine}}. The table argument must be the full name of the table name in the FishSET database. Output includes information on model convergence, standard errors, t-stats, etc.
  #' @param table  Table name in FishSET database. Should contain the phrase 'modelout'. Table name must be in quotes.
  #' @export
  #' @description Returns output from running the discretefish_subroutine function.
  #'   The table parameter must be the full name of the table name in the FishSET database.
  #' @examples
  #' \dontrun{
  #' model_out_view('pcodmodelout20190604')
  #' }
  #
  if (table_exists(table) == FALSE) {
    return("Table not found. Check spelling or tables in database using 'tables_database()'.")
  } else {
    suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase()))
    x <- unserialize(DBI::dbGetQuery(fishset_db, paste0("SELECT data FROM ", table, " LIMIT 1"))$data[[1]])
    return(x)
    DBI::dbDisconnect(fishset_db)
  }
}

globalcheck_view <- function(table) {
  #' View model error output
  #'
  #' Returns error output from running the discretefish_subroutine function.
  #' The table argument must be the full name of the table name in the FishSET database.
  #' Use 'tables_database()' to view table names in FishSET database.
  #'
  #' @param table  Table name in FishSET database. Should contain the project, the 
  #'    phrase 'ldglobalcheck', and a date in YMD format (20200101). 
  #'  Table name must be in quotes.
  #' @export
  #' @examples
  #' \dontrun{
  #' globalcheck_view('pcodldglobalcheck20190604')
  #' }

  if (table_exists(table) == FALSE) {
    return("Table not found. Check spelling or view available tables with 'tables_database()'.")
  } else {
    suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase()))
    x <- unserialize(DBI::dbGetQuery(fishset_db, paste0("SELECT data FROM ", table, " LIMIT 1"))$data[[1]])
    return(x)
    DBI::dbDisconnect(fishset_db)
  }
}

model_fit <- function(project) {
  #' Load model comparison metrics to console
  #'
  #' Load model comparison metrics to console. Metrics are displayed for each model that was fun. 
  #'   Metrics produced by \code{\link{discretefish_subroutine}}.
  #' @param project String, name of project.
  #' @export
  #' @examples
  #' \dontrun{
  #' model_fit('pollock')
  #' }
  suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase()))
  return(DBI::dbGetQuery(DBI::dbConnect(RSQLite::SQLite(), locdatabase()), paste0("SELECT * FROM ", paste0(project, "modelfit"))))
  DBI::dbDisconnect(fishset_db)
}

projects <- function() {
  #' Display projects n  ames
  #' @export
  #' @details Lists the unique project names currently in the FishSET Database. 
  #' @importFrom stringr str_extract
  #' @examples 
  #' \dontrun{
  #' projects()
  #' } 
  p_tabs <- grep("MainDataTable", tables_database(), value = TRUE)
  
  if (length(p_tabs) == 0) {
    
    warning("No projects found. Upload a new file to create a project.")
  
  } else {
    
    p_tabs <- stringr::str_extract(p_tabs, "^.+MainDataTable")
    
    p_names <- gsub("MainDataTable", "", p_tabs)
    p_names <- unique(p_names)
    
    p_names
  }
}

project_tables <- function(project, ...) {
  #' Display database table names by project
  #' 
  #' @param project Name of project.
  #' @param ... String, additional characters to match by. 
  #' @export
  #' @examples 
  #' \dontrun{
  #' project_tables("pollock")
  #' project_tables("pollock", "main")
  #' }

  out <- grep(paste0("^", project, ...), tables_database(), 
              value = TRUE, ignore.case = TRUE)
  
  if (length(out) == 0) {
    
    paste0("No database tables found for project ", project)
  } else {
    
    out
  }
}

main_tables <- function(project = NULL) {
  #' Display MainDataTables
  #' 
  #' @param project A project name to filter main tables by. Returns all MainDataTables
  #'   if \code{NULL}.
  #' @export
  #' @examples 
  #' \dontrun{
  #' main_tables()
  #' main_tables("pollock")
  #' }
  
  if (is.null(project)) {
    
    p_tabs <- grep("MainDataTable", tables_database(), value = TRUE)
    p_tabs <- p_tabs[!grepl("MainDataTableInfo", p_tabs)]
    
    p_tabs
    
  } else {
    
    if (project %in% projects()) {
      
      p_tabs <- grep(paste0(project, "MainDataTable"), tables_database(), value = TRUE)
      p_tabs <- p_tabs[!grepl("MainDataTableInfo", p_tabs)]
      
      p_tabs
      
    } else {
      
      warning("Project name not found in FishSET Database. Check spelling.")
    }
  }
}

list_tables <- function(project = NULL, type = "main") {
  #' Display tables in fishset_db by project and type.
  #' 
  #' @param project A project name to filter main tables by. Returns all tables
  #'   of \code{type} if \code{NULL}.
  #' @param type the type of fishset_db table to search for. Options include 
  #'   "main" (MainDataTable), "ec" (ExpectedCatch),  "altc" (altmatrix), 
  #'   "port", "info" (MainDataTableInfo), "gc" (ldglobalcheck), "fleet" (FleetTable), 
  #'   "model" (modelOut), "model_data" (modelinputdata), and "model_design" 
  #'   (modelDesignTable).
  #' @export
  #' @examples 
  #' \dontrun{
  #' list_tables(type = "main")
  #' list_tables("pollock", "ec")
  #' }
  
  sql_tab <- 
    switch(type, 
           "main" = "MainDataTable", "ec" = "ExpectedCatch",  "altc" = "altmatrix", 
           "port" = "port", "info" = "MainDataTableInfo", "gc" = "ldglobalcheck", 
           "fleet" = "FleetTable", "model" = "modelOut", "model_data" = "modelinputdata", 
           "model_design" = "modelDesignTable")
  
  if (is.null(project)) {
    
    tabs <- grep(sql_tab, tables_database(), value = TRUE)
    
    if (type == "main") tabs <- tabs[!grepl("MainDataTableInfo", tabs)]
    
    if (length(tabs) > 0) tabs
    else {
      
      warning("No ", sql_tab, " tables were found")
      invisible(NULL)
    }
    
  } else {
    
    if (project %in% projects()) {
      
      tabs <- grep(paste0(project, sql_tab), tables_database(), value = TRUE)
      
      if (type == "main") tabs <- tabs[!grepl("MainDataTableInfo", tabs)]
      
      if (length(tabs) > 0) tabs
      else {
        
        warning("No ", sql_tab, " tables were found for project '", project, "'")
        invisible(NULL)
      }
      
    } else {
      
      warning("Project name not found in FishSET Database. Check spelling.")
      invisible(NULL)
    }
  }
}
