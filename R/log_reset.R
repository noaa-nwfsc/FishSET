log_reset <- function(project, over_write = FALSE) {
  #' Reset log file
  #' 
  #' @param project Project name.
  #' @param over_write Logical, whether to over write an existing log file. This
  #'   only applies if a log was created and reset in the same day for the same 
  #'   project. See "Details". 
  #' @details Logs are saved by project name and date (date created, not date modified). 
  #'   For example, "pollock_2021-05-12.json". Calls to log functions are 
  #'   automatically appended to the existing project log file. Resetting the log 
  #'   file will create a new project log file with the current date. A log will 
  #'   not be reset if \code{log_reset()} is run the same day the log was created
  #'   (or if the log is reset two or more times in a single day), unless 
  #'   \code{over_write = TRUE}. This will replace that day's log file. 
  #' @export log_reset
  #' @seealso \code{\link{list_logs}} \code{\link{project_logs}}
  #' @examples
  #' \dontrun{
  #' log_reset("pollock")
  #' }
  #'

  pass <- TRUE
  
  if (!(project %in% projects())) {
    
    warning("log_reset() only applies to existing projects. Project \"", project, 
            "\" does not exist.")
    pass <- FALSE
  }
  
  log_file <- paste0(project, "_", Sys.Date(), ".json")
  
  if (log_file %in% project_logs(project) & over_write == FALSE) {
    
    warning("The log file \"", log_file, "\" already exists. Set over_write = TRUE to reset log.")
    pass <- FALSE
  }
  
  if (pass) {
  
    logbody <- list()
    infoBodyout <- list()
    functionBodyout <- list()
    infobody <- list()
    
    infobody$rundate <- Sys.Date()
    infobody$project <- project
    infoBodyout$info <- list(infobody)
    
    functionBodyout$function_calls <- list()
    
    logbody$fishset_run <- list(infoBodyout, functionBodyout)
    logbody$fishset_run <- list(infoBodyout, functionBodyout)
    
    write(
      jsonlite::toJSON(logbody, pretty = TRUE, auto_unbox = TRUE, null = "null", 
                       na = "string"), 
      paste0(loclog(project=project), log_file)
    )
    
    message("Log has been reset for project \"", project, "\"")
    invisible(TRUE)
 
   } else invisible(FALSE)
}

log_call <- function(project, fun.name) {
  #' Update function calls saved in log file
  #' @param project Name of project
  #' @param fun.name Function name
  #' @details update log file
  #' @importFrom jsonlite read_json toJSON
  #' @export
  #' @keywords internal
  
  create_new <- FALSE
  log_file <- suppressMessages(current_log(project))

  if (is.null(log_file)) create_new <- TRUE
  
  
  if (create_new) {
    
    log_file <- paste0(project, "_", Sys.Date(), ".json")
    
    logbody <- list()
    infoBodyout <- list()
    functionBodyout <- list()
    infobody <- list()

    infobody$rundate <- Sys.Date()
    infobody$project <- project
    infoBodyout$info <- list(infobody)

    functionBodyout$function_calls <- list()

    logbody$fishset_run <- list(infoBodyout, functionBodyout)
    functionBodyout$function_calls[[length(functionBodyout$function_calls) + 1]] <- fun.name
    logbody$fishset_run <- list(infoBodyout, functionBodyout)
    
  } else {
    if(any(is_empty(readLines(paste0(loclog(project=project), log_file))))){
    log_file <- paste0(project, "_", Sys.Date(), ".json")
    
    logbody <- list()
    infoBodyout <- list()
    functionBodyout <- list()
    infobody <- list()
    
    infobody$rundate <- Sys.Date()
    infobody$project <- project
    infoBodyout$info <- list(infobody)
    
    functionBodyout$function_calls <- list()
    
    logbody$fishset_run <- list(infoBodyout, functionBodyout)
    functionBodyout$function_calls[[length(functionBodyout$function_calls) + 1]] <- fun.name
    logbody$fishset_run <- list(infoBodyout, functionBodyout)

    } else {
    
      logbody <- jsonlite::read_json(paste0(loclog(project=project), log_file))
      logbody$fishset_run[[2]]$function_calls[[length(logbody$fishset_run[[2]]$function_calls) + 1]] <- fun.name
    }
  }
  

  write(
    jsonlite::toJSON(logbody, pretty = TRUE, auto_unbox = TRUE, null = "null", 
                     na = "string", force=TRUE), 
    paste0(loclog(project=project), log_file)
  )
}
