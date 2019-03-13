#' Check model data. Save output.
#'
#'  Contains one function that tests for NaNs, Inf, and that each row is unique. The function stops if an if statement does not pass.

#' @param dataset input dataframe or matrix
#' @param uniqueID dataframe that contains information on each column of the dataset
#' @param save.file whether to save as sql table (sqlsave) or csv file (csvsave)
#' @param db.name names of sql database to save dataset into
#' @param save.name name for verified and saved data
#' @return Returns statements as to whether issues in the data may exist
#' @export check_model_data
#' @details Checks data to be used for modelling. Checks for presence of NaNs and Inf. The verified data will not saves unless no NaNs or infinite values in the dataset. Verified data can then be saves as sql table or csv file.

# @examples checkModelData(MainDataTable, uniqueID='uniqueID_Code',
# db.name=testdb, save.name='modelData')



check_model_data <- function(dataset, uniqueID, save.file = "sqlfile", db.name, save.name) {
    tmp <- tempfile()
    if (length(which(is.nan.data.frame(dataset)) != 0) > 0) {
    cat(paste("\nNaNs are present in", 
              names(which(colSums(is.nan.data.frame(dataset)) != 0))),file=tmp, append=T)
    stop(paste("NaNs are present in", 
               names(which(colSums(is.nan.data.frame(dataset)) != 0))))
  }
  
  # is.inf
  if (length(which(is.inf.data.frame(dataset)) != 0) > 0) {
    cat(paste("\nInfinite values are present in",
               names(which(colSums(is.inf.data.frame(dataset)) != 0))), file=tmp, append=T)
    
    stop(paste("Infinite values are present in",
               names(which(colSums(is.inf.data.frame(dataset)) != 0))))
  }

    if (length(dataset[[uniqueID]]) != length(unique(dataset[[unique]]))) {
      cat("\nThe uniqueID variable should define the length of unique occurrences in the dataset. Use the HaulToTrip function to collapse data.",
          file=tmp, append=T)
      
     stop("The uniqueID variable should define the length of unique occurrences in the dataset. Use the HaulToTrip function to collapse data.")
  }

    if (save.file == "csvfile") {
    write.csv(dataset, paste(save.name, ".csv", sep = ""))
      cat(paste("\nModified dataset saved to csv file under", save.name), file=tmp, append=T)
  } else {
    cat(paste("\nModified dataset saved to sql database under", save.name), file=tmp, append=T)
    fishset_db <- DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite")
    DBI::dbWriteTable(fishset_db, save.name, dataset)
    DBI::dbDisconnect(fishset_db)
    # logging function information
  }
  

  #logging
  #write(layout.json.ed(trace, "checkModelData", deparse(substitute(dataset)), x, msg = paste("Saved as", save.name)),
  #                      paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""), append = T)
  checkModelData_function <- list()
  checkModelData_function$functionID <- 'check_model_data'
  checkModelData_function$args <- c(deparse(substitute(dataset)), uniqueID, save.file, db.name, save.name)
  checkModelData_function$msg <- suppressWarnings(readLines(tmp))
  functionBodyout$function_calls[[length(functionBodyout$function_calls)+1]] <- (checkModelData_function)
  body$fishset_run <- list(infoBodyout, functionBodyout)
  write(jsonlite::toJSON(body, pretty = TRUE, auto_unbox = TRUE),paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""))
  list2env(functionBodyout, envir = .GlobalEnv)
  
}
