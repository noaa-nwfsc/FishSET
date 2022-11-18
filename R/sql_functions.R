# SQL call functions @description Functions call sql functions to view tables in 
# the FishSET database. Functions allow users to view names of tables in the 
# database, view fields of selected table, view the selected table, remove tables 
# from the database, and check whether a specific table exists in the database.

tables_database <- function(project) {
  #' View names of project tables
  #'
  #' Wrapper for \code{\link[DBI]{dbListTables}}. View names of tables in a  
  #' project's FishSET database.
  #' @param project Project name
  #' @export tables_database
  #' @importFrom DBI dbConnect dbRemoveTable dbListTables dbExistsTable dbGetQuery
  #' @examples
  #' \dontrun{
  #' tables_database('pollock')
  #' }
  if (project_exists(project)) {
    
    fishset_db <- suppressWarnings(DBI::dbConnect(RSQLite::SQLite(), locdatabase(project)))
    on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
    db_tabs <- DBI::dbListTables(fishset_db)
    spat_tabs <- suppressWarnings(list_tables(project, type = "spat"))
    return(c(db_tabs, spat_tabs))
  }
  else { cat('Project not found')}
}


table_save <- function(table, project, type, name = NULL) {
 #' Save an existing FishSET DB table
 #' 
 #' \code{table_save()} updates existing FishSET DB tables. If the table doesn't
 #' exist, the user is reminded to use the appropriate \code{load_} function. 
 #' 
 #' @param table A dataframe to save to the FishSET Database.
 #' @param project Name of project.
 #' @param type The table type. Options include, \code{"main"} for main data tables,
 #'    \code{"port"} for port tables, \code{"grid"} for gridded tables, \code{"aux"}
 #'    for auxiliary tables. 
 #' @param name String, table name. Applicable only for gridded, auxiliary, and
 #'   spatial tables.
 #' @export
 #' @importFrom DBI dbConnect dbDisconnect dbWriteTable
 #' @importFrom RSQLite SQLite
  
  if (type %in% c("grid", "aux", "spat") & is_value_empty(name)) {
    
    stop("Name must be provided or grid, auxilary, or spatial tables.")
  }
  
  if (!inherits(table, c("data.frame", "matrix"))) {
    
    stop("Table must be a data.frame or matrix.")
  }
  
  if (type == "main") {
    
    tab_name <- paste0(project, 'MainDataTable')
    
    if (!table_exists(tab_name, project)) {
      
     stop("Main table '", tab_name, "' does not exist. Check spelling or use ", 
          "load_maindata() to import a main data table.")
    }
    
  } else if (type == "port") {
    
    tab_name <- paste0(project, 'PortTable')
    
    if (!table_exists(tab_name, project)) {
      
      stop("Port table '", tab_name, "' does not exist. Check spelling or use ", 
           "load_port() to import a port table.")
    }
    
  } else if (type == "grid") {
    
    tab_name <- paste0(project, name, "GridTable")
    
    if (!table_exists(tab_name, project)) {
      
      stop("Grid table '", tab_name, "' does not exist. Check spelling or use ", 
           "load_grid() to import a gridded table.")
    }
    
  } else if (type == "aux") {
    
    tab_name <- paste0(project, name, "AuxTable")
    
    if (!table_exists(tab_name, project)) {
      
      stop("Auxiliary table '", tab_name, "' does not exist. Check spelling or use ", 
           "load_aux() to import an auxiliary table.")
    }
  
  } else if (type == "spat") {
    
    tab_name <- paste0(project, name, "SpatTable")
    
    if (!table_exists(tab_name, project)) {
      
      stop("Spatial table '", tab_name, "' does not exist. Check spelling or use ", 
           "load_spat() to import an auxiliary table.")
    }
  
  } else {
    
    stop("Table type not recognized. Table not saved.")
  }

  suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project)))
  on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
  
  DBI::dbWriteTable(fishset_db, tab_name, table, overwrite = TRUE)
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
  
  if (table_exists(table, project) == FALSE) {
    
    return("Table not found. Check spelling.")
    
  } else {
    
    if (table_type(table) == "spatial") {
      
      spat <- table_view(table, project)
      return(names(spat))
      
    } else {
      
      suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project)))
      on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
      
      return(DBI::dbListFields(fishset_db, table))
    }
  }
}

table_view <- function(table, project) {
  #' View FishSET Database table
  #' 
  #' \code{table_view()} returns a table from a project's FishSET Database. 
  #' 
  #' @param table String, name of table in FishSET database. Table name must be in quotes.
  #' @param project Name of project.
  #' @export table_view
  #' @description Wrapper for \code{\link[DBI]{dbGetQuery}}. View or call the 
  #'   selected table from the FishSET database.
  #' @importFrom DBI dbConnect dbDisconnect dbGetQuery
  #' @importFrom sf st_read
  #' @importFrom lubridate as_date as_datetime
  #' @importFrom tibble as_tibble
  #' @seealso \code{\link{list_tables}} to show existing tables by project and type.
  #'   \code{\link{fishset_tables}} to show all tables in the FishSETFolder. 
  #' @examples
  #' \dontrun{
  #' head(table_view('pollockMainDataTable', project = 'pollock'))
  #' }
  
  if (table_exists(table, project) == FALSE) {
    
    warning("Table not found. Check spelling.", call. = FALSE)
    
  } else {
    
    if (table_type(table) == "spatial") {
      
      filename <- paste0(loc_data(project), "spat/", table, ".geojson")
      sf::st_read(filename)
      
    } else {
      
      suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project)))
      on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
      
      tab_out <- DBI::dbGetQuery(fishset_db, 
                                 paste0("SELECT * FROM", paste0("'", noquote(table), "'")))
      
      # TODO: need a better method for converting back to date
      # Ex: this approach won't convert a date column named "TRIP_START"
      # Alternative would be to save all date variables as character then check
      # if any columns have a date format (date_cols())
      # Best if character conversion was done in a table saving function
      
      # convert date and date-time from numeric
      d_cols <- grep("date", names(tab_out), ignore.case = TRUE, value = TRUE)
      dt_cols <- grep("date.*time", names(tab_out), ignore.case = TRUE, value = TRUE)
      
      d_cols <- d_cols[!d_cols %in% dt_cols]
      
      date_numeric <- numeric_cols(tab_out[d_cols], out = "names")
      dt_numeric <- numeric_cols(tab_out[dt_cols], out = "names")
      
      if (length(date_numeric) > 0) {
        # date saved as days since 1970-01-01
        tab_out[date_numeric] <- lapply(tab_out[date_numeric], lubridate::as_date)
      }
      
      if (length(dt_numeric) > 0) {
        # date-time saved as secs since 1970-01-01
        tab_out[dt_numeric] <- lapply(tab_out[dt_numeric], lubridate::as_datetime)
      }
      
      tibble::as_tibble(tab_out)
    }
  }
}


unserialize_table <- function(table, project) {
  #' Unserialize special tables in FishSET DB
  #' 
  #' @param table The name of the special table to unserialize. Special tables
  #'   include alternative choice matrix output, expected catch matrix output,
  #'   the model data list, and the prediction output (not available yet). 
  #' @param project Name of project.
  #' @keywords internal
  #' @importFrom DBI dbConnect dbDisconnect dbGetQuery
  #' @importFrom RSQLite SQLite
  
  if (!table_exists(table, project)) {
    
    stop("Table '", table, "' does not exist.", call. = FALSE)
  }
  
  tab_type <- table_type(table)
  
  serial_tabs <- c("alt choice matrix", "expected catch matrix", "model data", 
                   "predict output")
  
  if (!tab_type %in% serial_tabs) {
    
    stop("Invaid table.", call. = FALSE)
  }
  
  tab_qry <- switch(tab_type, 
                    "alt choice matrix" = "AlternativeMatrix", 
                    "expected catch matrix" = "data",
                    "model data" = "ModelInputData", # Note: check for consistency, seen lowercase version 
                                                     # (depends on whether created in app or console)
                    "predict output" = "PredictOutput") # Note: Hasn't been added to table_type
  
  sql_qry <- paste0("SELECT ", tab_qry, " FROM ", table, " LIMIT 1")
  
  fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project = project))
  on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)

  unserialize(DBI::dbGetQuery(fishset_db, sql_qry)[[tab_qry]][[1]])
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
  #' table_remove('pollockMainDataTable', 'pollock')
  #' }
  
  if (table_exists(table, project) == FALSE) {
    
    return("Table not found. Check spelling.")
    
  } else {
    
    if (table_type(table) == "spatial") {
      
      filename <- paste0(loc_data(project), "spat/", table, ".geojson")
      file.remove(filename)
      invisible(TRUE)
      
    } else {
      
      suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project)))
      on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
      
      DBI::dbRemoveTable(fishset_db, table)
      invisible(TRUE)
    }
  }
}

table_exists <- function(table, project) {
  #' Check if table exists in the FishSET database for the defined project
  #' @param table Name of table in FishSET database.Table name must be in quotes.
  #' @param project Name of project
  #' @export table_exists
  #' @description Wrapper for \code{\link[DBI]{dbExistsTable}}. Check if a table 
  #'   exists in the FishSET database.
  #' @return Returns a logical statement of table existence.
  #' @importFrom DBI dbConnect dbDisconnect dbExistsTable
  #' @examples
  #' \dontrun{
  #' table_exists('pollockMainDataTable', 'pollock')
  #' }
  
  if (!project_exists(project)) {
    
    stop("Project '", project, "' does not exist.")
  }
  
  if (table_type(table) == "spatial") {
    
    table %in% list_tables(project, type = "spat")
    
  } else {
    
    suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project)))
    on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
    
    return(DBI::dbExistsTable(fishset_db, table))
  }
}

model_out_view <- function(table, project) {
  #' Load discrete choice model output to console for the defined project
  #'
  #' Returns output from running \code{\link{discretefish_subroutine}}. The table 
  #' argument must be the full name of the table name in the FishSET database. 
  #' Output includes information on model convergence, standard errors, t-stats, etc.
  #' @param table  Table name in FishSET database. Should contain the phrase 
  #'   'ModelOut'. Table name must be in quotes.
  #' @param project Name of project
  #' @export
  #' @description Returns output from running the discretefish_subroutine function.
  #'   The table parameter must be the full name of the table name in the FishSET database.
  #' @examples
  #' \dontrun{
  #' model_out_view('pcodModelOut20190604', 'pcod')
  #' }
  #
  if (table_exists(table, project) == FALSE) {
    
    # TODO: Change to stop()?
    stop("Table not found. Check spelling or tables in database using 'tables_database()'.",
         call. = FALSE)
    
  } else {
    
    suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project)))
    on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
    
    x <- unserialize(DBI::dbGetQuery(fishset_db, paste0("SELECT data FROM ", table, " LIMIT 1"))$data[[1]])
    return(x)
  }
}


model_params <- function(table, project) {
  #' Load model parameter estimates, standard errors, and t-statistic to console 
  #' for the defined project
  #'
  #' Returns parameter estimates from running \code{\link{discretefish_subroutine}}. 
  #' The table argument must be the full name of the table name in the FishSET 
  #' database. 
  #' 
  #' @param table Table name in FishSET database. Should contain the phrase 
  #'   'ModelOut'. Table name must be in quotes.
  #' @param project Name of project
  #' @export
  #' @description Returns parameter estimates, standard errors, and t-statistic 
  #'  from running the discretefish_subroutine function. The table parameter must 
  #'  be the full name of the table name in the FishSET database.
  #' @examples
  #' \dontrun{
  #' model_params('pcodModelOut20190604', 'pcod')
  #' }
  #'

  mod_out <- model_out_view(table, project)
  
  mod_list <- lapply(mod_out, function(x) x$OutLogit)
  mod_names <- vapply(mod_out, function(x) x$name, character(1))
  names(mod_list) <- mod_names
  
  mod_list
}


globalcheck_view <- function(table, project) {
  #' View error output from discrete choice model for the defined project
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
  #' globalcheck_view('pcodldglobalcheck20190604', 'pcod')
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
  #' Load model comparison metrics to console for the defined project
  #'
  #' Load model comparison metrics to console. Metrics are displayed for each 
  #' model that was fun. Metrics produced by \code{\link{discretefish_subroutine}}.
  #' @param project String, name of project.
  #' @export
  #' @examples
  #' \dontrun{
  #' model_fit('pollock')
  #' }
  suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project)))
  on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
  
  return(DBI::dbGetQuery(fishset_db, paste0("SELECT * FROM ", paste0(project, "ModelFit"))))
}

model_names <- function(project) {
  #' Return model names 
  #' 
  #' Returns model names saved to to the model design file. 
  #' 
  #' @param project Name of project.
  #' @export
  #' 
  
  tab_name <- paste0(project, "ModelInputData")
  mod_design_list <- unserialize_table(tab_name, project)
  
  vapply(mod_design_list, function(x) x$mod.name, character(1))
}


projects <- function() {
  #' Display projects names
  #' @export
  #' @details Lists the unique project names currently in the FishSET Database. 
  #' @examples 
  #' \dontrun{
  #' projects()
  #' } 
  #' 
 
  projloc <- locproject()

  tab <- list_dirs(path = projloc)
 
  if (length(tab) == 0) {
    
    warning("No projects found. Upload a new file to create a project.", 
            call. = FALSE)
    invisible(NULL)
    
  } else {
    
    unique(tab)
  }
}

project_tables <- function(project, ...) {
  #' Display database table names by project
  #' 
  #' @param project Name of project.
  #' @param ... String, additional characters to match by. 
  #' @export
  #' @seealso \code{\link{list_tables}}, \code{\link{fishset_tables}}
  #' @examples 
  #' \dontrun{
  #' project_tables("pollock")
  #' project_tables("pollock", "main")
  #' }

  # NOTE: this is equivalent to tables_database()
  # TODO: remove project_tables() or replace tables_database()
  
  if (!project_exists(project)) {
    
    out <- NULL
    
  } else {
    
    if (is_value_empty(tables_database(project = project))) {
      
      out <- NULL
      
    } else {
      
      out <- grep(paste0("^", project, ...), tables_database(project = project), 
                value = TRUE, ignore.case = TRUE)
    }
    
    if (length(out) == 0) {
      
      message("No database tables found for project ", project)
      
    } else {
      
      out
    }
  }
}


main_tables <- function(project, show_all = TRUE) {
  #' View list of MainDataTables in FishSET database
  #' 
  #' @param project A project name to filter main tables by. 
  #' @param show_all Logical, whether to show all main tables (including raw and 
  #'   final tables) or just editable tables. 
  #' @export
  #' @examples 
  #' \dontrun{
  #' main_tables("pollock")
  #' }
  
  m_tabs <- list_tables(project = project, type = "main")
  
  if (show_all == FALSE) {
    
    m_tabs <- m_tabs[!grepl("_raw|_final", m_tabs)]
  }
  
  m_tabs
}

list_tables <- function(project, type = "main") {
  #' Display FishSET database tables by type
  #' 
  #' Show project table names by table type. To see all tables for all projects 
  #' in the FishSETFolder, use \code{\link{fishset_tables}}. 
  #' 
  #' @param project A project name to show main tables by. 
  #' @param type the type of fishset_db table to search for. Options include 
  #'   "main" (MainDataTable), "port" (PortTable), "spat" (SpatTable), "grid" 
  #'   (GridTable), "aux" (AuxTable) "ec" (ExpectedCatch),  "altc" (altmatrix), 
  #'   "info" (MainDataTableInfo), "gc" (ldglobalcheck), "fleet" (FleetTable), 
  #'   "filter" (FilterTable), "centroid" (Centroid or FishCentroid),  "model" 
  #'   (ModelOut), "model data" or "model design" (ModelInputData).
  #' @export
  #' @examples 
  #' \dontrun{
  #' list_tables("pollock", type = "main")
  #' list_tables("pollock", "ec")
  #' }
  #' 

  tab_types <- c("info", "main", "ec", "altc", "port", "gc", "fleet", "model", 
                 "model data", "model design", "grid", "aux", "spat", "filter",
                 "centroid")
  
  if (!type %in% tab_types) {
    
    stop("Invalid table type. Currently options are: ", 
         paste(tab_types, collapse = ", "), ".", call. = FALSE)
  }
  
  sql_tab <- 
    switch(type, 
           "info" = "MainDataTableInfo", "main" = "MainDataTable", "ec" = "ExpectedCatch", 
           "altc" = "altmatrix", "port" = "PortTable", "gc" = "ldglobalcheck", 
           "fleet" = "FleetTable", "model" = "ModelOut", "model data" = "ModelInputData", 
           "model design" = "ModelInputData", "grid" = "GridTable", "aux" = "AuxTable",
           "spat" = "SpatTable", "filter" = "FilterTable", "centroid" = "Centroid")
  
  if (is_value_empty(project)) {
    
    warning('Project must be specified.', call. = FALSE)
    
  } else {
    
    if (project %in% projects()) {
      
      if (type == "spat") {
        
        tabs <- list.files(paste0(loc_data(project), "spat"))
        tabs <- grep("\\.geojson$", tabs, value = TRUE)
        tabs <- gsub("\\.geojson$", "", tabs)
        
      } else {
        
        tabs <- grep(paste0("^", project), tables_database(project), value = TRUE)
        tabs <- grep(sql_tab, tabs, value = TRUE)
        
        if (type == "main") tabs <- tabs[!grepl("MainDataTableInfo", tabs)]
        
      }
      
      if (length(tabs) > 0) tabs
      else {
        
        warning("No ", sql_tab, " tables were found for project '", project, "'",
                call. = FALSE)
        invisible(NULL)
      }
      
    } else {
      
      warning("Project name not found in FishSET Database. Check spelling.",
              call. = FALSE)
      invisible(NULL)
    }
  }
}

fishset_tables <- function(project = NULL) {
  #' Show all SQL Tables in FishSET Folder
  #' 
  #' Returns a data frame containing all tables from each project by project name
  #' and table type.
  #'
  #'@param project Project name. If \code{NULL}, tables from all available projects 
  #'  will be displayed. 
  #'@importFrom stringi stri_extract_first_regex
  #'@export
  #'@examples 
  #'\dontrun{
  #' # return all tables for all projects
  #' fishset_tables()
  #' 
  #' # return all tables for a specific project
  #' fishset_tables("pollock")
  #'}
  
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
    # TODO: this isn't working correctly. If a project name contains another 
    # ("pollock", "pollockChoice") then the smaller name is used ("pollock")
    # Affects manage tables tab in app
    p_str <- stringi::stri_extract_first_regex(db_tabs$table, p_regex)
    p_str[is.na(p_str)] <- "no project"
    db_tabs$project <- p_str
    
    # add a type column (order matters)
    db_type <- c("MainDataTableInfo", "MainDataTable\\d{8}", "MainDataTable_final", 
                 "MainDataTable", "ExpectedCatch", "altmatrix", "PortTable\\d{8}",
                 "PortTable", "ldglobalcheck", "FleetTable", "ModelOut", "ModelFit",
                 "ModelInputData", "modelDesignTable", "FilterTable", "GridTable\\d{8}",  
                 "GridTable", "AuxTable\\d{8}", "AuxTable", "SpatTable\\d{8}", "SpatTable")
    
    raw <- c("MainDataTable\\d{8}", "PortTable\\d{8}", "GridTable\\d{8}", 
             "AuxTable\\d{8}", "SpatTable\\d{8}")
    
    t_regex <- paste0(db_type, collapse = "|")
    t_str <- stringi::stri_extract_first_regex(db_tabs$table, t_regex)
    
    # replace raw table dates with "_raw"
    for (i in raw) {
      
      r_ind <- grepl(i, t_str)
      t_str[r_ind] <- gsub("\\d{8}", "_raw", t_str[r_ind])
    }
    
    t_str[is.na(t_str)] <- "other"
    
    t_str <- 
      vapply(t_str, function(i) {
        
        switch(i, 
               "MainDataTable" = "main table", "MainDataTable_final" = "final table", 
               "MainDataTable_raw" = "raw main table", "ExpectedCatch" = "expected catch matrix", 
               "altmatrix" = "alt choice matrix", "PortTable_raw" = "raw port table",
               "PortTable" = "port table", "MainDataTableInfo" = "info table",
               "FilterTable" = "filter table", "ldglobalcheck" = "global check", 
               "FleetTable" = "fleet table", "ModelOut" = "model output", 
               "ModelFit" = "model fit", "ModelInputData" = "model data", 
               "modelDesignTable" = "model design", "other" = "other",
               "GridTable_raw" = "raw grid table",  "GridTable" = "grid table",
               "AuxTable_raw" = "raw aux table", "AuxTable" = "aux table", 
               "SpatTable_raw" = "raw spat table",  "SpatTable" = "spat table")
      }, character(1))
    
    db_tabs$type <- t_str
    
    db_tabs[c("project", "type")] <- lapply(db_tabs[c("project", "type")], as.factor)
    
    db_tabs
  }
}
