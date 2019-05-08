#' Guided steps to cleaning data frame

#' @param dat Main data frame over which to apply function. Table in fishset_db database should contain the string `MainDataTable`.
#' @param x Column in data frame to check for outliers.
#' @param dataindex MainDataTableInfo table from fishet_db database that is associated with the data set. This table contains information on each column of the data frame.
#'  Must be in quotes if called from the fishset_db database.
#' @export data_check
#' @details Prints summary stats for all variable in the main data frame. Checks for NaNs and NAs and prints column names that contain NaNs or NAs.
#' Checks for outliers for specified data column. Further actions may be taken to further evaluate and remove NaNs and outliers.
#' Checks that all column names in the data frame are unique, whether specialized variables have been identified in the index data set, whether any columns in the data frame are empty, if units are defined and recognized, whether each row is a unique choice occurrence at the haul or trip level, and that data for either lat/long or fishing area are included.
#' The function is also called in other functions to check for common issues with data. 
#' @examples
#' \dontrun{
#' data_check(MainDataTable, 'OFFICIAL_TOTAL_CATCH_MT', 'MainDataTableInfo')
#' }
#'

data_check <- function(dat, x, dataindex) {
#Call in data
      suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite"))
  #Call in main data set
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
  #call in data index
  if(is.character(dataindex)==TRUE){
    if(is.null(dataindex)==TRUE | table_exists(dataindex)==FALSE){
      print(DBI::dbListTables(fishset_db))
      stop(paste(dataindex, 'not defined or does not exist. Consider using one of the tables listed above that exist in the database.'))
    } else {
      dataindex2 <- table_view(dataindex)
    }
    } else {
    dataindex2 <- dataindex 
    }
      #single_sql <- paste("select * from ", dataindex, sep='')
      #dataindex2 <- DBI::dbGetQuery(fishset_db,  single_sql)
      DBI::dbDisconnect(fishset_db)

#Run checks
    print(summary_stats(dataset))
     cat('\nNaN checks\n')
          if (any(apply(dataset, 2, function(x) any(is.na(x)))==TRUE)) {
            cat("The", names(which(apply(dataset, 2, function(x) any(is.na(x)))==TRUE)), "columns contain NaNs. Consider using nan_filter to replace or remove NaNs")
          } else {
            cat("No columns in the dataframe contain NaNs")
          }
     
     cat('\n')
     cat('\nOutlier checks')
     print(outlier_table(dataset, x))
     cat('\n')
     cat('\n')
     outlier_plot(dataset, x)
     cat('The plot shows the data with no adjustments (distribution specified or points removed). 
         Consider further visualizing the data with outlier_plot. Remove outliers with outlier_remove.')
     cat('\n')
     cat('\nData verification checks.\n')
     data_verification_call(dataset)
     #Table_verification_function
        allNecFields = c("name", "units", "general", "XY", "ID", "Time", "Catch", "Effort", 
                        "CPUE", "Lat", "Value", "Area", "Port", "Price", "Trip", "Haul", "Other")
        indx <- colSums(sapply(allNecFields, grepl, colnames(dataindex2), ignore.case = TRUE)) 
     
        if (length(which(colSums(sapply(allNecFields, grepl, colnames(dataindex2))) == 0)) < 1) {
            cat("Pass: All specialized variables identified.")
        } else {
            cat(paste("\nThe following specialized variables were not specified:", 
                 names(which(colSums(sapply(allNecFields, grepl, colnames(dataindex2))) == 0))))
        }
     
        # Units are sensible
        unitsAvailable <- c("Day of Month", "Day of Year", "Decimal Degree", "Dollar", 
                         "Fathom", "Feet", "Horsepower", "ID", "Kilometer", "Pound", "lbs", "Meter", 
                         "Metric Ton", "Mile", "Minute", "mm", "mmdd", "N/A", "Numeric", "Percent", 
                         "Tonne", "Week", "wk", "Y/N", "T/F", "yyyymmdd", "yyyy", "yyyy-mm-dd HH:MM:SS", 
                         "mm-dd-yyyy HH:MM:SS", "mm/dd/yyyy HH:MM:SS", "yyyy/mm/dd HH:MM:SS", "min")
        indx <- colSums(sapply(unitsAvailable, grepl, colnames(dataindex2), ignore.case = TRUE))
        if (length(which(colSums(sapply(unitsAvailable, grepl, colnames(dataindex2))) == 0))) {
          cat("\nPass: All units are specified.")
        } else {
          cat(paste("\nThe units are not recognized for the following variables:", 
                 which(colSums(sapply(unitsAvailable, grepl, colnames(dataindex2))) == 0)))
        }
     
      if(!exists('logbody')) { 
        logging_code()
      } 
        data_check_function <- list()
        data_check_function$functionID <- 'data_check'
        data_check_function$args <- c(deparse(substitute(dat)), x, dataindex)
        data_check_function$kwargs <- list()
        data_check_function$output <- c('')
      functionBodyout$function_calls[[length(functionBodyout$function_calls)+1]] <- (data_check_function)
      logbody$fishset_run <- list(infoBodyout, functionBodyout)
      write(jsonlite::toJSON(logbody, pretty = TRUE, auto_unbox = TRUE),paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""))
      assign("functionBodyout", value = functionBodyout, pos = 1)
    
}
