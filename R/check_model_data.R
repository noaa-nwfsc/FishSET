#' Check model data. Save output.
#'
#'  Contains one function that tests for NaNs, Inf, and that each row is unique. The function stops if an if statement does not pass.

#' @param dataset  Main data frame containing data on hauls or trips
#' @param uniqueID Variable that identifies unique occurrences.
#' @param save.file Save data frame as sql table (sqlsave) or csv file (csvsave)
#' @param save.name name for verified and saved data
#' @return Returns statements as to whether issues in the data may exist
#' @export check_model_data
#' @details Checks data to be used for modelling. Checks for presence of NaNs and Inf. 
#'    The verified data will not save if NaNs or infinite values are in the dataset. 
#'    Verified data can then be saved as sql table or csv file. 
#'    When saving to sql, the modified table will be saved and the previous, unmodified version of the table 
#'    (if it exists),will be saved with the prefix 'raw'.

# @examples checkModelData(MainDataTable, uniqueID='uniqueID_Code',
# db.name=testdb, save.name='modelData')



check_model_data <- function(dataset, uniqueID, save.name, save.file = "sqlfile") {
    tmp <- tempfile()
    if (any(apply(dataset, 2, function(x) any(is.na(x)))==TRUE)) {
    cat(paste("\nNaNs are present in", 
              names(which(apply(dataset, 2, function(x) any(is.na(x)))==TRUE))), file=tmp, append=T)
    stop(paste("\nNaNs are present in", 
               names(which(apply(dataset, 2, function(x) any(is.na(x)))==TRUE))))
  }
  
  # is.inf
  if (any(apply(dataset, 2, function(x) any(is.infinite(x)))==TRUE)) {
    cat(paste("\nInfinite values are present in",
              names(which(apply(dataset, 2, function(x) any(is.infinite(x)))==TRUE))), file=tmp, append=T)
    
    stop(paste("\nInfinite values are present in",
               names(which(apply(dataset, 2, function(x) any(is.infinite(x)))==TRUE))))
  }

    if (length(dataset[[uniqueID]]) != length(unique(dataset[[uniqueID]]))) {
      cat("\nThe uniqueID variable should define the length of unique occurrences in the dataset. Use the haul_to_trip function to collapse data.",
          file=tmp, append=T)
      
     stop("\nThe uniqueID variable should define the length of unique occurrences in the dataset. Use the haul_to_trip function to collapse data.")
  }

    if (save.file == "csvfile") {
    write.csv(dataset, paste(save.name, ".csv", sep = ""))
      cat(paste("\nModified dataset saved to csv file under", save.name), file=tmp, append=T)
  } else {
    cat(paste("\nModified dataset saved to sql database under", save.name), file=tmp, append=T)
    fishset_db <- DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite")
    if(DBI::dbExistsTable(fishset_db, deparse(substitute(save.name)))==TRUE){
      single_sql <- paste("raw", deparse(substitute(save.name)), sep='')
      DBI::dbWriteTable(fishset_db, single_sql, save.name)
    }
    DBI::dbWriteTable(fishset_db, deparse(substitute(save.name)), dataset, overwrite=TRUE)
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
  checkModelData_function$msg <- suppressWarnings(readLines(tmp))
  functionBodyout$function_calls[[length(functionBodyout$function_calls)+1]] <- checkModelData_function
  logbody$fishset_run <- list(infoBodyout, functionBodyout)
  write(jsonlite::toJSON(logbody, pretty = TRUE, auto_unbox = TRUE),paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""))
  assign("functionBodyout", value = functionBodyout, pos = 1)
  rm(tmp)
}
