#' Check for common data quality issues that may be present in the data set.
#'
#' Function tests for common data quality issues.
#' @param dat Main data frame over which to apply function. Table in fishset_db database should contain the string `MainDataTable`.
#' @return Statements as to whether data quality issues may exist.
#' @importFrom stringi stri_count_regex
#' @export data_verification
#' @details Checks that all columnn names in the data frame are unique, whether any columns in the data frame are empty, whether each row is a unique choice 
#' occurrence at the haul or trip level, and that either latitude and longitude or fishing area are included.
  #' @examples 
  #' \dontrun{ 
  #' data_verification(MainDataTable)
  #' }

data_verification <- function(dat) {
  
  #Call in datasets
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
  
  # check each row of data is a unique choice occurrence at haul or trip level
  if (dim(dataset)[1] == dim(unique(dataset))[1]) {
    cat("\nPass: Each row is a unique choice occurrence.", file=tmp, append=T)
  } else {
    cat("\nEach row in dataset is not a unique choice occurrence at haul or trip level.", file=tmp, append=T)
    #stop("Each row in dataset must be a unique choice occurrence at haul or trip level.")
    check <- 1
  }
  
  # Handling of empty variables
  if (any(apply(dataset, 2, function(x) all(is.na(x))) == TRUE)) {
    cat("\n",names(which(apply(dataset, 2, function(x) all(is.na(x))) == TRUE)), "is empty. 
        Consider removing the column from the dataset.", file=tmp, append=T)
  } else {
    cat("\nPass: No empty variables exist in the dataset.", file=tmp, append=TRUE)
  }
  
  if(any(grepl('lat|lon', names(dataset), ignore.case=TRUE))){
    lat <- dataset[,which(grepl('lat', names(dataset), ignore.case=TRUE)==TRUE)]
    lon <- dataset[,which(grepl('lon', names(dataset), ignore.case=TRUE)==TRUE)]
    cat('At least one lat/lon variable is not in degrees. Use the degree function to convert to degrees.')
  } 
    
    
  graphics::par(mar=c(1,1,1,1)) 
  longitude <- which(stringi::stri_count_regex(colnames(dataset), '(?=LON|Lon|lon)', ignore.case=TRUE)==max(stringi::stri_count_regex(colnames(dataset), '(?=LON|Lon|lon)', ignore.case=TRUE)))[1]
  latitude <- which(stringi::stri_count_regex(colnames(dataset), '(?=LAT|Lat|lat)', ignore.case=TRUE)==max(stringi::stri_count_regex(colnames(dataset), '(?=LAT|Lat|lat)', ignore.case=TRUE)))[1]
  maps::map('world', ylim=c(min(dataset[, latitude], na.rm=TRUE), max(dataset[, latitude], na.rm=TRUE)), 
      xlim=c(min(dataset[, longitude], na.rm=TRUE), max(dataset[, longitude], na.rm=TRUE)))
  pts <- sample(nrow(dataset), nrow(dataset)/10)
  points(as.numeric(as.character(dataset[pts, longitude])), 
         as.numeric(as.character(dataset[pts, latitude])))
  print('10% of samples plotted. Verify that points occur in correct geographic area.')
  
  
  
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
  
  data_verification_function <- list()
  data_verification_function$functionID <- 'data_verification'
  data_verification_function$args <- c(deparse(substitute(dat)))
  data_verification_function$kwargs <- list()
  data_verification_function$output <- c('')
  data_verification_function$msg <- suppressWarnings(readLines(tmp))
  functionBodyout$function_calls[[length(functionBodyout$function_calls)+1]] <- data_verification_function
  logbody$fishset_run <- list(infoBodyout, functionBodyout)
  write(jsonlite::toJSON(logbody, pretty = TRUE, auto_unbox = TRUE), paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""))
  assign("functionBodyout", value = functionBodyout, pos = 1)
  rm(tmp)  
  
  if(check==1) {
    stop('At least one error exists')
  }
  

}

