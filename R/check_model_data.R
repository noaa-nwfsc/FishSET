#' Check data table for data quality issues that will affect modeling functions. 
#'

#' @param dat Main data frame over which to apply function. Table in fishset_db database should contain the string `MainDataTable`.
#' @param dataindex Data frame that contains information on each column of the main data frame. In fishset_db database the table will contain the phrase `MainDataTableInfo`
#' @param uniqueID Variable that identifies unique occurrences.
#' @param save.file TRUE/FALSE Save to fishset_db SQLite database? Defaults to TRUE
#' @return Returns statements as to data quality issues in the data may exist.
#' @export check_model_data
#' @details Checks data to be used for modeling for presence of NAs, NaNs and Inf and that each row is a unique choice occurrence. 
#'    Model functions may fail or return inaccurate results if any of these issues exist.
#'    The verified data will not save if any of these issues are in the data set. 
#'    If data passes all tests, the verified data will  then be saved as SQL table. 
#'    When saving to SQL, the modified table will be saved and the previous, unmodified version of the table 
#'    (if it exists), will be saved with the prefix 'raw'.
#'    The data index table will also be updated and saved.

#' @examples 
#' \dontrun{ 
#' check_model_data('MainDataTable', 'MainDataTableInfo', uniqueID='uniqueID_Code', save.file=TRUE)
#' }


check_model_data <- function(dat, dataindex, uniqueID, save.name, save.file = TRUE) {
 
   #Call in data sets
  fishset_db <- DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite")
  if(is.character(dat)==TRUE){
    if(is.null(dat)==TRUE | table_exists(dat)==FALSE){
      print(DBI::dbListTables(fishset_db))
      stop(paste(dat, 'not defined or does not exist. Consider using one of the tables listed above that exist in the database.'))
    } else {
      dataset <- table_view(dat)
    }
  } else {
    dataset <- dat  
  }
  DBI::dbDisconnect(fishset_db)
  
  #update dataindex
  dataIndex <- dataindex_update(dataset, dataindex)
  
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
    if (save.file == TRUE) {
      cat(paste("\nModified data set saved to fishset_db database"), file=tmp, append=T)
    fishset_db <- DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite")
    if(DBI::dbExistsTable(fishset_db, deparse(substitute(dat)))==TRUE){
      single_sql <- paste0("raw", deparse(substitute(dat)))
      DBI::dbWriteTable(fishset_db, single_sql, dataset)
    }
    single_sql <- deparse(substitute(dat))
    DBI::dbWriteTable(fishset_db, single_sql, dataset, overwrite=TRUE)
    DBI::dbDisconnect(fishset_db)
    # logging function information
  }
  
    suppressWarnings(readLines(tmp))
     if(!exists('logbody')) { 
       logbody <- list()
       infoBodyout <- list()
       functionBodyout <- list()
       infobody <- list()
       
       infobody$rundate <- Sys.Date()
       infoBodyout$info <- list(infobody)
       
       functionBodyout$function_calls <- list()
       
       logbody$fishset_run <- list(infoBodyout, functionBodyout)
    } 
  checkModelData_function <- list()
  checkModelData_function$functionID <- 'check_model_data'
  checkModelData_function$args <- c(deparse(substitute(dat)), deparse(substitute(dataindex)), uniqueID, save.file)
  checkModelData_function$kwargs <- list()
  checkModelData_function$output <-  c('')
  checkModelData_function$msg <- suppressWarnings(readLines(tmp))
  functionBodyout$function_calls[[length(functionBodyout$function_calls)+1]] <- checkModelData_function
  logbody$fishset_run <- list(infoBodyout, functionBodyout)
  write(jsonlite::toJSON(logbody, pretty = TRUE, auto_unbox = TRUE),paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""))
  assign("functionBodyout", value = functionBodyout, pos = 1)
  rm(tmp)
}
