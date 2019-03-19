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

