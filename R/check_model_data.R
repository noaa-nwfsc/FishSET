#' Check data for issues that will affect modeling functions. 
#'
#'  Function tests for common data quality that will affect modeling functions. The function stops if a check does not pass.

#' @param dataset  Main dataframe containing data on hauls or trips
#' @param uniqueID Variable that identifies unique occurrences.
#' @param save.file Save dataframe as sql table (sqlsave) or csv file (csvsave)
#' @param save.name name for verified and saved data
#' @return Returns statements as to whether issues in the data may exist
#' @export check_model_data
#' @details Checks data to be used for modeling for presence of NAs, NaNs and Inf and that each row is a unique choice occurrence. 
#'    Model functions may fail or return inaccurate results if any of these issues exist.
#'    The verified data will not save if any of these issues are in the data set. 
#'    If data passes all tests, the verified data can then be saved as sql table or csv file. 
#'    When saving to sql, the modified table will be saved and the previous, unmodified version of the table 
#'    (if it exists), will be saved with the prefix 'raw'.

#' @examples 
#' \dontrun{ 
#' check_model_data(MainDataTable, uniqueID='uniqueID_Code', save.name='modelData')
#' }



check_model_data <- function(dataset, uniqueID, save.name, save.file = "sqlfile") {
    tmp <- tempfile()
    x <- 0
    if (any(apply(dataset, 2, function(x) any(is.nan(x)))==TRUE)) {
    cat("\nNaNs are present in", 
              names(which(apply(dataset, 2, function(x) any(is.nan(x)))==TRUE)), file=tmp, append=T)
    cat("\nNaNs are present in", 
               names(which(apply(dataset, 2, function(x) any(is.nan(x)))==TRUE)))
     x<- 1
  }

    if (any(apply(dataset, 2, function(x) any(is.na(x)))==TRUE)) {
      cat(paste("\nNAs are present in", 
                names(which(apply(dataset, 2, function(x) any(is.na(x)))==TRUE))), file=tmp, append=T)
      cat("\nNAs are present in", 
                 names(which(apply(dataset, 2, function(x) any(is.na(x)))==TRUE)))
      x <- 1
    }
    
  # is.inf
  if (any(apply(dataset, 2, function(x) any(is.infinite(x)))==TRUE)) {
    cat(paste("\nInfinite values are present in",
              names(which(apply(dataset, 2, function(x) any(is.infinite(x)))==TRUE))), file=tmp, append=T)
    
    cat("\nInfinite values are present in",
               names(which(apply(dataset, 2, function(x) any(is.infinite(x)))==TRUE)))
    x <- 1
  }

    if (length(dataset[[uniqueID]]) != length(unique(dataset[[uniqueID]]))) {
      cat("\nThe uniqueID variable should define the length of unique occurrences in the dataset. Use the haul_to_trip function to collapse data.",
          file=tmp, append=T)
      
     cat("\nThe uniqueID variable should define the length of unique occurrences in the dataset. 
         Use the haul_to_trip function to collapse data.")
     x <- 1
  }

       if(x <- 1) {
         stop('At least one test did not pass. Data set will not be saved.')
       }
    if (save.file == "csvfile") {
    write.csv(dataset, paste(save.name, ".csv", sep = ""))
      cat(paste("\nModified datas et saved to csv file under", save.name), file=tmp, append=T)
  } else {
    cat(paste("\nModified data set saved to sql database under", save.name), file=tmp, append=T)
    fishset_db <- DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite")
    if(DBI::dbExistsTable(fishset_db, deparse(substitute(save.name)))==TRUE){
      single_sql <- paste0("raw", deparse(substitute(save.name)))
      DBI::dbWriteTable(fishset_db, single_sql, save.name)
    }
    single_sql <- paste0(deparse(substitute(save.name)))
    DBI::dbWriteTable(fishset_db, single_sql, dataset, overwrite=TRUE)
    DBI::dbDisconnect(fishset_db)
    # logging function information
  }
  
    suppressWarnings(readLines(tmp))
  #logging
  #write(layout.json.ed(trace, "checkModelData", deparse(substitute(dataset)), x, msg = paste("Saved as", save.name)),
  #                      paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""), append = T)
    if(!exists('logbody')) { 
      logging_code()
    } 
  checkModelData_function <- list()
  checkModelData_function$functionID <- 'check_model_data'
  checkModelData_function$args <- c(deparse(substitute(dataset)), uniqueID, save.name, save.file)
  checkModelData_function$kwargs <- list()
  checkModelData_function$output <-  c('')
  checkModelData_function$msg <- suppressWarnings(readLines(tmp))
  functionBodyout$function_calls[[length(functionBodyout$function_calls)+1]] <- checkModelData_function
  logbody$fishset_run <- list(infoBodyout, functionBodyout)
  write(jsonlite::toJSON(logbody, pretty = TRUE, auto_unbox = TRUE),paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""))
  assign("functionBodyout", value = functionBodyout, pos = 1)
  rm(tmp)
}
