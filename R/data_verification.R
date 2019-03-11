#' Check for serveral issues that may be present in the data.
#'
#'  Contains one function that tests several if statements. The function stops if a if statement does not pass.
#' @param dataset dataframe or matrix
#' @param dataindex dataframe that contains information on each column of the dataset
#' @return Returns statements as to whether issues in the data may exist
#' @export data_verification
#' @details dataVerification checks that all columnn names in the dataset are unique, whether specialized variables have been identified in the index 
#' dataset, whether any columns in the dataset are empty, if units are defined and recognized, whether each row is a unique choice occurrence at the haul
#' or trip level, and that data for either lat/long or fishing area are included.

# @examples dataVerification(MainDataTable, MainDataTableInfo)

data_verification <- function(dataset, dataindex) {
  
  # check that names are unique in dataset
  x <- colnames(dataset)
  if (length(x) == length(unique(x))) {
    print("Variable names are unique within dataset.")
  } else {
    cat("\nVariable names are notunique.", file=paste(getwd(),'/Logs/InforMessage',Sys.Date(),'.txt', sep=''), append=TRUE)
    stop("Variable names must be unique.")
  }
  
  # check each row of data is a unique choice occurrence at haul or trip level
  if (dim(dataset)[1] == dim(unique(dataset))[1]) {
    print("Each row is a unique choice occurrence.")
  } else {
    cat("\nEach row in dataset is not a unique choice occurrence at haul or trip level.", 
          file=paste(getwd(),'/Logs/InforMessage',Sys.Date(),'.txt', sep=''), append=TRUE)
    stop("Each row in dataset must be a unique choice occurrence at haul or trip level.")
  }
  
  # Check to see if lat/long or fish area is in dataset
  indx <- grepl("lat|lon|area", colnames(dataset), ignore.case = TRUE)
  if (length(dataset[indx]) > 0) {
    print("Latitude and longitude or fishing area included in the dataset.")
  } else {
    cat("\nDataset must contain either latitude and longitude or fishing area designation.", 
          file=paste(getwd(),'/Logs/InforMessage',Sys.Date(),'.txt', sep=''), append=TRUE)
    stop("Dataset must contain either latitude and longitude or fishing area designation.")
  }
  
  # Identify if any specialized variables are not identified
  allNecFields = c("name", "units", "general", "XY", "ID", "Time", "Catch", "Effort", 
                   "CPUE", "Lat", "Value", "Area", "Port", "Price", "Trip", "Haul", "Other")
  indx <- colSums(sapply(allNecFields, grepl, colnames(dataindex), ignore.case = TRUE)) 
    
  if (length(which(colSums(sapply(allNecFields, grepl, colnames(dataindex))) == 0)) < 1) {
    print("All specialized variables indentified.")
  } else {
    cat(paste("\nThe following specialized variables were not specified:", 
                  names(which(colSums(sapply(allNecFields, grepl, colnames(dataindex))) == 0))), 
        file=paste(getwd(),'/Logs/InforMessage',Sys.Date(),'.txt', sep=''), append=TRUE)
    
    warning(paste("The following specialized variables are not specified:", 
                names(which(colSums(sapply(allNecFields, grepl, colnames(dataindex))) == 0))))
  }
  
  # Handling of empty variables
  if (any(apply(dataset, 2, function(x) all(is.na(x))) == TRUE)) {
    warning(paste(names(which(apply(dataset, 2, function(x) all(is.na(x))) == TRUE)), 
                "is empty. Consider removing the column from the dataset."))
  } else {
    print("No empty variables exist in the dataset.")
  }
  
  # Units are sensible
  unitsAvailable <- c("Day of Month", "Day of Year", "Decimal Degree", "Dollar", 
                      "Fathom", "Feet", "Horsepower", "ID", "Kilometer", "Pound", "lbs", "Meter", 
                      "Metric Ton", "Mile", "Minute", "mm", "mmdd", "N/A", "Numeric", "Percent", 
                      "Tonne", "Week", "wk", "Y/N", "T/F", "yyyymmdd", "yyyy", "yyyy-mm-dd HH:MM:SS", 
                      "mm-dd-yyyy HH:MM:SS", "mm/dd/yyyy HH:MM:SS", "yyyy/mm/dd HH:MM:SS", "min")
  indx <- colSums(sapply(unitsAvailable, grepl, colnames(dataindex), ignore.case = TRUE))
  if (length(which(colSums(sapply(unitsAvailable, grepl, colnames(dataindex))) == 0))) {
    print("All units are specified.")
  } else {
    cat(paste("\nThe units are not recognized for the following variables:", 
                  which(colSums(sapply(unitsAvailable, grepl, colnames(dataindex))) == 0)),
        file=paste(getwd(),'/Logs/InforMessage',Sys.Date(),'.txt', sep=''), append=TRUE)
    
    warning(paste("The units are not recognized for the following variables:", 
                which(colSums(sapply(unitsAvailable, grepl, colnames(dataindex))) == 0)))
  }
  
  data_verification_function <- list()
  data_verification_function$functionID <- 'data_verification'
  data_verification_function$args <- c(deparse(substitute(dataset)), deparse(substitute(dataindex)))
  data_verification_function$msg <- paste('See', paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""), 'for record of data verification checks.')
  functionBodyout$function_calls[[length(functionBodyout$function_calls)+1]] <<- (data_verification_function)
  body$fishset_run <- list(infoBodyout, functionBodyout)
  write(jsonlite::toJSON(body, pretty = TRUE, auto_unbox = TRUE),paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""))
  
}
