table_info_verification <- function(dataindex) {
  #' Check for common data issues
  #' @param dataindex Data table saved in fishset_db database that contains information on each column of the main dataset. 
  #' Table name should contain the phrase `MainDataTableInfo`.
  #' Can be called from the global environment or the fishset_db database.
  #' @return Returns statements as to whether issues in the `dataindex` table may exist
  #' @export table_info_verification
  #' @details Checks whether specialized variables have been identified in the `dataindex` table and if units are defined and recognized.
  #' @examples 
  #' \dontrun{
  #' #Table loaded from fishet_db database
  #'    table_info_verification('MainDataTableInfo') 
  #' #Table loaded from the global environment.
  #'    table_info_verification(MainDataTableInfo) 
  #' }
  
    tmp <- tempfile()
  
  #Call in datasets
  fishset_db <- DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite")
  if(is.character(dataindex)==TRUE){
    if(is.null(dataindex)==TRUE | table_exists(dataindex)==FALSE){
      print(DBI::dbListTables(fishset_db))
      stop(paste(dataindex, 'not defined or does not exist. Consider using one of the tables listed above that exist in the database.'))
    } else {
    dataindex_pull <- table_view(dataindex)
    }
  } else {
    dataindex_pull <- dataindex 
   }
  DBI::dbDisconnect(fishset_db)
  
  # Identify if any specialized variables are not identified
  allNecFields = c("name", "units", "general", "XY", "ID", "Time", "Catch", "Effort", 
                   "CPUE", "Lat", "Value", "Area", "Port", "Price", "Trip", "Haul", "Other")
  indx <- colSums(sapply(allNecFields, grepl, colnames(dataindex_pull), ignore.case = TRUE)) 
  
  if (length(which(colSums(sapply(allNecFields, grepl, colnames(dataindex_pull))) == 0)) < 1) {
    cat("Pass: All specialized variables identified.", file=tmp, append=TRUE)
  } else {
    cat(paste("\nThe following specialized variables were not specified:", 
              names(which(colSums(sapply(allNecFields, grepl, colnames(dataindex_pull))) == 0))), file=tmp, append=TRUE)
    
    #warning(paste("The following specialized variables are not specified:", 
    #            names(which(colSums(sapply(allNecFields, grepl, colnames(dataindex))) == 0))))
  }
  
  # Units are sensible
  unitsAvailable <- c("Day of Month", "Day of Year", "Decimal Degree", "Dollar", 
                      "Fathom", "Feet", "Horsepower", "ID", "Kilometer", "Pound", "lbs", "Meter", 
                      "Metric Ton", "Mile", "Minute", "mm", "mmdd", "N/A", "Numeric", "Percent", 
                      "Tonne", "Week", "wk", "Y/N", "T/F", "yyyymmdd", "yyyy", "yyyy-mm-dd HH:MM:SS", 
                      "mm-dd-yyyy HH:MM:SS", "mm/dd/yyyy HH:MM:SS", "yyyy/mm/dd HH:MM:SS", "min")
  indx <- colSums(sapply(unitsAvailable, grepl, colnames(dataindex_pull), ignore.case = TRUE))
  if (length(which(colSums(sapply(unitsAvailable, grepl, colnames(dataindex_pull))) == 0))) {
    cat("\nPass: All units are specified.", append=TRUE, file=tmp)
  } else {
    cat(paste("\nThe units are not recognized for the following variables:", 
              which(colSums(sapply(unitsAvailable, grepl, colnames(dataindex_pull))) == 0)), file=tmp, append=TRUE)
    
    #warning(paste("The units are not recognized for the following variables:", 
    #            which(colSums(sapply(unitsAvailable, grepl, colnames(dataindex))) == 0)))
  }
  print(suppressWarnings(readLines(tmp)))
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
  table_info_verification_function <- list()
  table_info_verification_function$functionID <- 'table_info_verification'
  table_info_verification_function$args <- c(deparse(substitute(dataindex)))
  table_info_verification_function$kwargs <- list()
  table_info_verification_function$output <- c('')
  table_info_verification_function$msg <- suppressWarnings(readLines(tmp))
  functionBodyout$function_calls[[length(functionBodyout$function_calls)+1]] <- (table_info_verification_function)
  logbody$fishset_run <- list(infoBodyout, functionBodyout)
  write(jsonlite::toJSON(logbody, pretty = TRUE, auto_unbox = TRUE),paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""))
  assign("functionBodyout", value = functionBodyout, pos = 1)
  rm(tmp)
}
