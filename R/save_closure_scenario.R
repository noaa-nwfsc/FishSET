get_closure_scenario <- function(project) {
  #' Retrieve closure scenario by project
  #' 
  #' @param project Name of project.
  #' @importFrom yaml read_yaml
  #' @export
  #' @examples 
  #' \dontrun{
  #' get_closure_scenario("pollock")
  #' }
  
  filename <- paste0(locoutput(project), project, "_closures.yaml")
  
  if (file.exists(filename)) {
    
    yaml::read_yaml(filename)
    
  } else return(NULL)
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
  #' Used to save unique closures scenarios to the closures yaml file located in
  #' the project output folder.
  #' 
  #' @param project Name of project.
  #' @param c_list List of closure scenarios to check and save. 
  #' @importFrom yaml write_yaml read_yaml
  #' @keywords internal
  #' @export
  
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  
  filename <- paste0(locoutput(project), project, "_closures.yaml")
  
  if (!file.exists(filename)) {

    yaml::write_yaml(c_list, filename)

  } else {
    
    keep <- unique_closure(project, c_list, ind = TRUE)
    
    c_list <- c_list[keep]

    c_file <- yaml::read_yaml(filename)

    c_file <- c(c_file, c_list)

    yaml::write_yaml(c_file, filename)
  }
  
  cat("Closure scenario saved", file = tmp)
  
  msg_print(tmp)
  
  # Log function
  save_closure_scenario_function <- list()
  save_closure_scenario_function$functionID <- "save_closure_scenario"
  save_closure_scenario_function$args <- list(project, c_list)
  save_closure_scenario_function$msg <- suppressWarnings(readLines(tmp))
  log_call(project, save_closure_scenario_function)
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
