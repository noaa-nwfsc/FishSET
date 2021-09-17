# SQL call functions @description Functions call sql functions to view tables in the FishSET database. Functions allow users to view names of tables
# in the database, view fields of selected table, view the selected table, remove tables from the database, and check whether a specific table exists
# in the database.

tables_database <- function(project) {
  #' View names of tables in the FishSET database
  #'
  #' Wrapper for \code{\link[DBI]{dbListTables}}. View names of tables in the FishSET database.
  #' @param project Project name
  #' @export tables_database
  #' @importFrom DBI dbConnect dbRemoveTable dbListTables dbExistsTable dbGetQuery
  #' @examples
  #' \dontrun{
  #' tables_database('pollock')
  #' }

  fishset_db <- suppressWarnings(DBI::dbConnect(RSQLite::SQLite(), locdatabase(project)))
  on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
  
  return(DBI::dbListTables(fishset_db))
}

table_fields <- function(table, project) {
  #' Lists fields for FishSET database table
  #' @param table String, name of table in FishSET database. Table name must be in quotes.
  #' @param project Project name
  #' @export table_fields
  #' @description Wrapper for \code{\link[DBI]{dbListFields}}.  View fields of selected table.
  #' @importFrom DBI dbConnect dbDisconnect dbListFields
  #' @examples
  #' \dontrun{
  #' table_fields('pollockMainDataTable', 'pollock')
  #' }

  suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project)))
  on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
  
  return(DBI::dbListFields(fishset_db, table))
}

table_view <- function(table, project) {
  #' View FishSET database table
  #' @param table String, name of table in FishSET database. Table name must be in quotes.
  #' @param project Name of project
  #' @export table_view
  #' @description Wrapper for \code{\link[DBI]{dbGetQuery}}. View or call the selected table from the FishSET database.
  #' @importFrom DBI dbConnect dbDisconnect  dbGetQuery
  #' @examples
  #' \dontrun{
  #' head(table_view('pollockMainDataTable'))
  #' }
  
  if (table_exists(table, project) == FALSE) {
    
    return("Table not found. Check spelling.")
    
  } else {
    
    suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project)))
    on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
    
    return(DBI::dbGetQuery(fishset_db, paste0("SELECT * FROM", paste0("'", noquote(table), "'"))))
  }
}

table_remove <- function(table, project) {
  #' Remove table from FishSET database
  #'
  #' Wrapper for \code{\link[DBI]{dbRemoveTable}}. Remove a table from the FishSET database.
  #' @param table String, name of table in FishSET database. Table name must be in quotes.
  #' @param project Name of project
  #' @export table_remove
  #' @details Function utilizes sql functions to remove tables from the FishSET database.
  #' @importFrom DBI dbConnect dbDisconnect dbRemoveTable
  #' @examples
  #' \dontrun{
  #' table_remove('pollockMainDataTable')
  #' }

  suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project)))
  on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
  
  DBI::dbRemoveTable(fishset_db, table)
  invisible(TRUE)
}

table_exists <- function(table, project) {
  #' Check if table exists in the FishSET database
  #' @param table Name of table in FishSET database.Table name must be in quotes.
  #' @param project Name of project
  #' @export table_exists
  #' @description Wrapper for \code{\link[DBI]{dbExistsTable}}. Check if a table exists in the FishSET database.
  #' @return Returns a logical statement of table existence.
  #' @importFrom DBI dbConnect dbDisconnect dbExistsTable
  #' @examples
  #' \dontrun{
  #' table_exists('pollockMainDataTable')
  #' }

  suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project)))
  on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
  
  return(DBI::dbExistsTable(fishset_db, table))
}

model_out_view <- function(table, project) {
  #' Load model output to console
  #'
  #' Returns output from running \code{\link{discretefish_subroutine}}. The table argument must be the full name of the table name in the FishSET database. Output includes information on model convergence, standard errors, t-stats, etc.
  #' @param table  Table name in FishSET database. Should contain the phrase 'modelout'. Table name must be in quotes.
  #' @param project Name of project
  #' @export
  #' @description Returns output from running the discretefish_subroutine function.
  #'   The table parameter must be the full name of the table name in the FishSET database.
  #' @examples
  #' \dontrun{
  #' model_out_view('pcodmodelout20190604')
  #' }
  #
  if (table_exists(table, project) == FALSE) {
    
    return("Table not found. Check spelling or tables in database using 'tables_database()'.")
    
  } else {
    
    suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project)))
    on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
    
    x <- unserialize(DBI::dbGetQuery(fishset_db, paste0("SELECT data FROM ", table, " LIMIT 1"))$data[[1]])
    return(x)
  }
}

globalcheck_view <- function(table, project) {
  #' View model error output
  #'
  #' Returns error output from running the discretefish_subroutine function.
  #' The table argument must be the full name of the table name in the FishSET database.
  #' Use 'tables_database()' to view table names in FishSET database.
  #'
  #' @param table  Table name in FishSET database. Should contain the project, the 
  #'    phrase 'ldglobalcheck', and a date in YMD format (20200101). 
  #'  Table name must be in quotes.
  #' @param project Name of project
  #' @export
  #' @examples
  #' \dontrun{
  #' globalcheck_view('pcodldglobalcheck20190604')
  #' }

  if (table_exists(table, project) == FALSE) {
    
    return("Table not found. Check spelling or view available tables with 'tables_database()'.")
    
  } else {
    
    suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project)))
    on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
    
    x <- unserialize(DBI::dbGetQuery(fishset_db, paste0("SELECT data FROM ", table, " LIMIT 1"))$data[[1]])
    return(x)
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
  suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project)))
  on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
  
  return(DBI::dbGetQuery(fishset_db, paste0("SELECT * FROM ", paste0(project, "modelfit"))))
}

projects <- function() {
  #' Display projects names
  #' @export
  #' @details Lists the unique project names currently in the FishSET Database. 
  #' @importFrom stringr str_extract
  #' @examples 
  #' \dontrun{
  #' projects()
  #' } 
  #' 
 
  
  if (!exists('loc2')||is.null(loc2)) {
    projloc <- paste0(system.file(package = "FishSET"), '/projects')
  } else {
    projloc <- paste0(loc2, '/projects')
  }
  
   tab <- list_dirs(path=projloc)#paste0(unlist(strsplit(loc2, '/'))[-length(unlist(strsplit(loc2, '/')))], collapse = '/'))
   p_tabs <- subset(tab, tab!='extdata'&tab!='MapViewer'&tab!='output'&tab!='report'&tab!='ShinyFiles'&tab!='Logs')
  
  
  #p_tabs <- grep("MainDataTable", tables_database(project), value = TRUE)
  
  if (length(p_tabs) == 0) {
    
    warning("No projects found. Upload a new file to create a project.")
    p_names <- NULL
  } else {
    
   # p_tabs <- stringr::str_extract(p_tabs, "^.+MainDataTable")
    
   # p_names <- gsub("MainDataTable", "", p_tabs)
   # p_names <- unique(p_names)
    p_names <- unique(p_tabs)
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

  if(is_empty(tables_database(project = project))){
    out <- NULL
  } else {
    out <- grep(paste0("^", project, ...), tables_database(project = project), 
              value = TRUE, ignore.case = TRUE)
  }
  
  if (length(out) == 0) {
    
    paste0("No database tables found for project ", project)
  } else {
    
    out
  }
}

list_tables <- function(project, type = "main") {
  #' Display tables in fishset_db by project and type.
  #' 
  #' @param project A project name to show main tables by. 
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
  
  # check_proj(project)
  
  sql_tab <- 
    switch(type, 
           "info" = "MainDataTableInfo", "main" = "MainDataTable", "ec" = "ExpectedCatch", 
           "altc" = "altmatrix", "port" = "PortTable", "gc" = "ldglobalcheck", 
           "fleet" = "FleetTable", "model" = "modelOut", "model_data" = "modelinputdata", 
           "model_design" = "modelDesignTable", "grid" = "GridTable", "aux" = "AuxTable")
  
  if (is.null(project)) {
    
    warning('Project must be specified.')
 #   tabs <- grep(sql_tab, tables_database(), value = TRUE)
    
 #   if (type == "main") tabs <- tabs[!grepl("MainDataTableInfo", tabs)]
    
 #   if (length(tabs) > 0) tabs
 #   else {
      
 #      warning("No ", sql_tab, " tables were found")
#      invisible(NULL)
 #   }
    
  } else {
    
    if (project %in% projects()) {
      
      tabs <- grep(paste0("^", project), tables_database(project), value = TRUE)
      tabs <- grep(sql_tab, tabs, value = TRUE)
      
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

main_tables <- function(project = NULL, show_all = TRUE) {
  #' Display MainDataTables
  #' 
  #' @param project A project name to filter main tables by. Returns all MainDataTables
  #'   if \code{NULL}.
  #' @param show_all Logical, whether to show all main tables (including raw and final 
  #'   tables) or just editable tables. 
  #' @export
  #' @examples 
  #' \dontrun{
  #' main_tables()
  #' main_tables("pollock")
  #' }
  
  m_tabs <- list_tables(project = project, type = "main")
  
  if (show_all == FALSE) {
    
    m_tabs <- m_tabs[!grepl("_raw|_final", m_tabs)]
  }
  
  m_tabs
}


fishset_tables <- function(project = NULL) {
  #' Show all SQL Tables in fishset_db with project name and table type
  #' 
  #' Returns a data frame containing all tables along with their project names
  #' and table type.
  #'
  #'@param project Project name. If NULL, tables from all available projects will
  #'  be displayed. 
  #'@importFrom stringr str_extract
  #'@export
  
  # dataframe containing all sql tables 
  
  if (length(projects()) > 0) {
    
    if (is.null(project)) {
      
      db_tabs <- unlist(lapply(projects(), tables_database)) 
      
    } else {
      
      db_tabs <- tables_database(project)
    }
    
    db_tabs <- data.frame(table = db_tabs)
    
    # add a project column
    p_regex <- paste0(projects(), collapse = "|")
    
    p_str <- stringr::str_extract(db_tabs$table, p_regex)
    p_str[is.na(p_str)] <- "no project"
    db_tabs$project <- p_str
    
    # add a type column (order matters)
    db_type <- c("MainDataTableInfo", "MainDataTable_raw", "MainDataTable_final", 
                 "MainDataTable", "ExpectedCatch", "altmatrix", "PortTable", "port", 
                 "ldglobalcheck", "FleetTable", "modelOut", "modelfit", "modelinputdata", 
                 "modelDesignTable", "FilterTable", "GridTable", "AuxTable")
    
    t_regex <- paste0(db_type, collapse = "|")
    t_str <- stringr::str_extract(db_tabs$table, t_regex)
    t_str[is.na(t_str)] <- "other"
    
    t_str <- 
      vapply(t_str, function(i) {
        
        switch(i, 
               "MainDataTable" = "main table", "MainDataTable_final" = "final table", 
               "MainDataTable_raw" = "raw table", "ExpectedCatch" = "expected catch matrix", 
               "altmatrix" = "alt choice matrix", "PortTable" = "port table", 
               "port" = "port table", "MainDataTableInfo" = "info table",
               "FilterTable" = "filter table", "ldglobalcheck" = "global check", 
               "FleetTable" = "fleet table", "modelOut" = "model output", 
               "modelfit" = "model fit", "modelinputdata" = "model data", 
               "modelDesignTable" = "model design", "other" = "other",
               "GridTable" = "grid table", "AuxTable" = "aux table")
      }, character(1))
    
    db_tabs$type <- t_str
    
    db_tabs[c("project", "type")] <- lapply(db_tabs[c("project", "type")], as.factor)
    
    db_tabs
  }
}
