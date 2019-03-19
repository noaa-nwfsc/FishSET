table_info_verification <- function(dataset, dataindex) {
  #'  Contains one function that tests several if statements. The function stops if a if statement does not pass.
  #' @param dataset dataframe or matrix
  #' @param dataindex dataframe that contains information on each column of the dataset
  #' @return Returns statements as to whether issues in the data may exist
  #' @export table_info_verification
  #' @details checks whether specialized variables have been identified in the index dataset and if units are defined and recognized
  tmp <- tempfile()
  
  # Identify if any specialized variables are not identified
  allNecFields = c("name", "units", "general", "XY", "ID", "Time", "Catch", "Effort", 
                   "CPUE", "Lat", "Value", "Area", "Port", "Price", "Trip", "Haul", "Other")
  indx <- colSums(sapply(allNecFields, grepl, colnames(dataindex), ignore.case = TRUE)) 
  
  if (length(which(colSums(sapply(allNecFields, grepl, colnames(dataindex))) == 0)) < 1) {
    cat("Pass: All specialized variables indentified.", file=tmp)
  } else {
    cat(paste("\nThe following specialized variables were not specified:", 
              names(which(colSums(sapply(allNecFields, grepl, colnames(dataindex))) == 0))), file=tmp)
    
    #warning(paste("The following specialized variables are not specified:", 
    #            names(which(colSums(sapply(allNecFields, grepl, colnames(dataindex))) == 0))))
  }
  
  # Units are sensible
  unitsAvailable <- c("Day of Month", "Day of Year", "Decimal Degree", "Dollar", 
                      "Fathom", "Feet", "Horsepower", "ID", "Kilometer", "Pound", "lbs", "Meter", 
                      "Metric Ton", "Mile", "Minute", "mm", "mmdd", "N/A", "Numeric", "Percent", 
                      "Tonne", "Week", "wk", "Y/N", "T/F", "yyyymmdd", "yyyy", "yyyy-mm-dd HH:MM:SS", 
                      "mm-dd-yyyy HH:MM:SS", "mm/dd/yyyy HH:MM:SS", "yyyy/mm/dd HH:MM:SS", "min")
  indx <- colSums(sapply(unitsAvailable, grepl, colnames(dataindex), ignore.case = TRUE))
  if (length(which(colSums(sapply(unitsAvailable, grepl, colnames(dataindex))) == 0))) {
    print("Pass: All units are specified.")
  } else {
    cat(paste("\nThe units are not recognized for the following variables:", 
              which(colSums(sapply(unitsAvailable, grepl, colnames(dataindex))) == 0)), file=tmp)
    
    #warning(paste("The units are not recognized for the following variables:", 
    #            which(colSums(sapply(unitsAvailable, grepl, colnames(dataindex))) == 0)))
  }
  print(suppressWarnings(readLines(tmp)))
  table_info_verification_function <- list()
  table_info_verification_function$functionID <- 'table_info_verification'
  table_info_verification_function$args <- c(deparse(substitute(dataset)), deparse(substitute(dataindex)))
  table_info_verification_function$msg <- paste('See', paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""), 'for record of data verification checks.')
  functionBodyout$function_calls[[length(functionBodyout$function_calls)+1]] <- (table_info_verification_function)
  body$fishset_run <- list(infoBodyout, functionBodyout)
  write(jsonlite::toJSON(body, pretty = TRUE, auto_unbox = TRUE),paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""))
  list2env(functionBodyout, envir = .GlobalEnv)
  
}
