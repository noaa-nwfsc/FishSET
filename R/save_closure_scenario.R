get_closure_scenario <- function(project) {
  #' Retrieve closure scenario by project
  #' 
  #' @param project Name of project.
  #' @importFrom DBI dbConnect dbDisconnect dbGetQuery dbExecute dbQuoteIdentifier
  #' @importFrom RSQLite SQLite
  #' @export
  #' @examples 
  #' \dontrun{
  #' get_closure_scenario("pollock")
  #' }
  
  table_name <- paste0(project, "ClosureScenarios")
  if (!table_exists(table_name, project)) {
    return(NULL)
  }
  
  fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project))
  on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
  result <- DBI::dbGetQuery(
    fishset_db,
    paste0("SELECT data FROM ", DBI::dbQuoteIdentifier(fishset_db, table_name),
           " LIMIT 1")
  )
  
  if (nrow(result) == 0) NULL else unserialize(result$data[[1]])
}


unique_closure <- function(project, c_list, ind = TRUE) {
  #' Check for unique closure scenarios
  #' 
  #' @param project Name of project
  #' @param c_list List of closure scenarios to check.
  #' @param ind Logical, whether to return an index of unique closure scenarios
  #' from \code{c_list} or a single TRUE/FALSE value indicating that one or more
  #' closure scenarios are unique. 
  #' @keywords internal
  #' @export
  
  c_log <- get_closure_scenario(project)
  
  if (is.null(c_log)) TRUE
  else {
    
    no_match <- 
      vapply(c_list, function(clst) {
          
        out <- vapply(c_log, function(clg) !identical(clst, clg),
                      FUN.VALUE = logical(1))
       
        all(out)
      }, logical(1))
    
    if (ind) no_match
    else any(no_match)
  }
}


save_closure_scenario <- function(project, c_list) {
  #' Save unique closure scenarios
  #' 
  #' Saves closure scenarios in the project's internal database.
  #' 
  #' @param project Name of project.
  #' @param c_list List of closure scenarios to check and save. 
  #' @importFrom DBI dbConnect dbDisconnect dbExecute dbQuoteIdentifier
  #' @importFrom RSQLite SQLite
  #' @keywords internal
  #' @export
  
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  
  serialize_table(paste0(project, "ClosureScenarios"), c_list, project)
  
  cat("Closure scenario saved", file = tmp)
  
  msg_print(tmp)
  
  # Log function
  save_closure_scenario_function <- list()
  save_closure_scenario_function$functionID <- "save_closure_scenario"
  save_closure_scenario_function$args <- list(project, c_list)
  save_closure_scenario_function$msg <- suppressWarnings(readLines(tmp))
  log_call(project, save_closure_scenario_function)
}


serialize_table <- function(table, object, project) {
  #' Serialize an object into a FishSET project database table
  #'
  #' @param table Database table name.
  #' @param object R object to serialize.
  #' @param project Project name.
  #' @keywords internal
  
  fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project))
  on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
  table_name <- DBI::dbQuoteIdentifier(fishset_db, table)
  
  DBI::dbExecute(fishset_db, paste("DROP TABLE IF EXISTS", table_name))
  DBI::dbExecute(fishset_db, paste("CREATE TABLE", table_name, "(data BLOB)"))
  DBI::dbExecute(
    fishset_db,
    paste("INSERT INTO", table_name, "(data) VALUES (:data)"),
    params = list(data = list(serialize(object, NULL)))
  )
}


close_names <- function(project) {
  #' Retrieve closure scenario names
  #' 
  #' A helper function used to display the names of currently saved closure
  #' scenarios. 
  #' 
  #' @param project Name of project
  #' @export
  #' @details To retrieve the complete closure scenario file, use 
  #' \code{\link{get_closure_scenario}}.
  
  c_list <- get_closure_scenario(project)
  
  vapply(c_list, function(cs) cs$scenario, character(1))
}
