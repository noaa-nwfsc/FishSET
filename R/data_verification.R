#' Check for serveral issues that may be present in the data.
#'

# @examples dataVerification(MainDataTable, MainDataTableInfo)

data_verification <- function(dataset) {
#'  Contains one function that tests several if statements. The function stops if a if statement does not pass.
#' @param dataset dataframe or matrix
#' @return Returns statements as to whether issues in the data may exist
#' @export data_verification
#' @details checks that all columnn names in the dataset are unique, whether any columns in the dataset are empty, whether each row is a unique choice 
#' occurrence at the haul or trip level, and that data for either lat/long or fishing area are included.
  tmp <- tempfile()
  cat("Data verification checks", file=tmp, append=TRUE)
  check <- 0
  # check that names are unique in dataset
  x <- colnames(dataset)
  if (length(x) == length(unique(x)) & length(toupper(x)) == length(unique(toupper(x)))) {
    cat("\nPass: Variable names as written are unique within dataset.", file=tmp, append=T)
  } else if (length(x) == length(unique(x)) & length(toupper(x)) != length(unique(toupper(x)))) {
    cat('\nData set will not be saved to database. Duplicate case insensitive colnames. Sqlite column names are case insensitive.', file=tmp, append=T)
    check <- 1
  } else {
    cat("\nVariable names are not unique.\n", file=tmp, append=T)
    check <- 1
    #stop("Variable names must be unique.\n")
  }
  
  if (length(toupper(x)) == length(unique(toupper(x)))) {
    cat("\nPass: Variable names are unique within dataset.", file=tmp, append=T)
  } else {
    cat("\nVariable names are not unique.", file=tmp, append=T)
    check <- 1
    #stop("Variable names must be unique.")
  }
  
  # check each row of data is a unique choice occurrence at haul or trip level
  if (dim(dataset)[1] == dim(unique(dataset))[1]) {
    cat("\nPass: Each row is a unique choice occurrence.", file=tmp, append=T)
  } else {
    cat("\nEach row in dataset is not a unique choice occurrence at haul or trip level.", file=tmp, append=T)
    #stop("Each row in dataset must be a unique choice occurrence at haul or trip level.")
    check <- 1
  }
  
  # Check to see if lat/long or fish area is in dataset
  indx <- grepl("lat|lon|area", colnames(dataset), ignore.case = TRUE)
  if (length(dataset[indx]) > 0) {
    cat("\nPass: Latitude and longitude or fishing area included in the dataset.", file=tmp, append=T)
  } else {
    cat("\nDataset must contain either latitude and longitude or fishing area designation.", file=tmp, append=T)
    #stop("Dataset must contain either latitude and longitude or fishing area designation.")
    check <- 1
  }
  
  
  # Handling of empty variables
  if (any(apply(dataset, 2, function(x) all(is.na(x))) == TRUE)) {
    cat('\n',names(which(apply(dataset, 2, function(x) all(is.na(x))) == TRUE), 
              "is empty. Consider removing the column from the dataset."), file=tmp, append=T)
  } else {
    cat("\nPass: No empty variables exist in the dataset.", file=tmp, append=TRUE)
  }
  
  print(suppressWarnings(readLines(tmp)))
  body <- list()  
  logging_code()  
  data_verification_function <- list()
  data_verification_function$functionID <- 'data_verification'
  data_verification_function$args <- c(deparse(substitute(dataset)))
  data_verification_function$msg <- suppressWarnings(readLines(tmp))
  functionBodyout$function_calls[[length(functionBodyout$function_calls)+1]] <- data_verification_function
  body$fishset_run <- list(infoBodyout, functionBodyout)
  write(jsonlite::toJSON(body, pretty = TRUE, auto_unbox = TRUE), paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""))
  list2env(functionBodyout, envir = .GlobalEnv)
  unlink(tmp)  
  
  if(check==1) {
    stop('Data cannot be saved, at least one error exists')
  }
  

}

