#' Check for serveral issues that may be present in the data.
#'
#'  Contains one function that tests several if statements. The function stops if a if statement does not pass.
#' @param dataset dataframe or matrix
#' @param dataindex dataframe that contains information on each column of the dataset
#' @return Returns statements as to whether issues in the data may exist
#' @export data_verification
#' @export table_info_verification
#' @details data_verification checks that all columnn names in the dataset are unique, whether specialized variables have been identified in the index 
#' dataset, whether any columns in the dataset are empty, if units are defined and recognized, whether each row is a unique choice occurrence at the haul
#' or trip level, and that data for either lat/long or fishing area are included.

# @examples dataVerification(MainDataTable, MainDataTableInfo)

data_verification <- function(dataset) {
  tmp <- tempfile()
  cat("Data verification checks", file=tmp)
  check <- 0
  # check that names are unique in dataset
  x <- colnames(dataset)
  if (length(x) == length(unique(x)) & length(toupper(x)) == length(unique(toupper(x)))) {
    cat("Pass: Variable names as written are unique within dataset.", file=tmp)
  } else if (length(x) == length(unique(x)) & length(toupper(x)) != length(unique(toupper(x)))) {
    cat('Data set will not be saved to database. Duplicate case insensitive colnames. Sqlite column names are case insensitive.', file=tmp)
    check <- 1
  } else {
    cat("Variable names are not unique.", file=tmp)
    check <- 1
    #stop("Variable names must be unique.")
  }
  
  if (length(toupper(x)) == length(unique(toupper(x)))) {
    cat("Pass: Variable names are unique within dataset.", file=tmp)
  } else {
    cat("Variable names are not unique.", file=tmp)
    check <- 1
    #stop("Variable names must be unique.")
  }
  
  # check each row of data is a unique choice occurrence at haul or trip level
  if (dim(dataset)[1] == dim(unique(dataset))[1]) {
    cat("Pass: Each row is a unique choice occurrence.", file=tmp)
  } else {
    cat("Each row in dataset is not a unique choice occurrence at haul or trip level.", file=tmp)
    #stop("Each row in dataset must be a unique choice occurrence at haul or trip level.")
    check <- 1
  }
  
  # Check to see if lat/long or fish area is in dataset
  indx <- grepl("lat|lon|area", colnames(dataset), ignore.case = TRUE)
  if (length(dataset[indx]) > 0) {
    print("Pass: Latitude and longitude or fishing area included in the dataset.", file=tmp)
  } else {
    cat("Dataset must contain either latitude and longitude or fishing area designation.", file=tmp)
    #stop("Dataset must contain either latitude and longitude or fishing area designation.")
    check <- 1
  }
  
  
  # Handling of empty variables
  if (any(apply(dataset, 2, function(x) all(is.na(x))) == TRUE)) {
    cat(paste(names(which(apply(dataset, 2, function(x) all(is.na(x))) == TRUE)), 
              "is empty. Consider removing the column from the dataset."), file=tmp)
  } else {
    cat("Pass: No empty variables exist in the dataset.", file=tmp)
  }
  
  print(suppressWarnings(readLines(tmp)))
  data_verification_function <- list()
  data_verification_function$functionID <- 'data_verification'
  data_verification_function$args <- c(deparse(substitute(dataset)))
  data_verification_function$msg <- paste('See', paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""), 'for record of data verification checks.')
  functionBodyout$function_calls[[length(functionBodyout$function_calls)+1]] <- (data_verification_function)
  body$fishset_run <- list(infoBodyout, functionBodyout)
  write(jsonlite::toJSON(body, pretty = TRUE, auto_unbox = TRUE), paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""))
  list2env(functionBodyout, envir = .GlobalEnv)
  if(check==1) {
    stop('Data cannot be saved, at least one error exists')
  }
}


table_info_verification <- function(dataset, dataindex) {
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
