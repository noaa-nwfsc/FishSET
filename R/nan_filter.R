# Filters NaN's from variable


nan_identify <- function(dat){
  #' Identify NaN's and NAs in data set
  #'
  #' @param dat Main data frame over which to apply function. Table in fishset_db database should contain the string `MainDataTable`.
  #' @keywords NaN, NA
  #' @description Check whether any columns in the data frame contain NAs or NaNs. Returns column names containing NAs or NaNs.
  #' @return Message with names of columns containing NAs or NaNs, if any.
  #' @export nan_identify
#' @examples 
#' \dontrun{
#' nan_identify(MainDataTable)
#' mod.dat <-nan_filter(MainDataTable, 'OFFICIAL_TOTAL_CATCH_MT') 
#' mod.dat <- nan_filter(MainDataTable, 'OFFICIAL_TOTAL_CATCH_MT', replace=T) 
#' mod.dat <- nan_filter(MainDataTable, 'OFFICIAL_TOTAL_CATCH_MT', replace=T, rep.value=0) 
#' mod.dat <- nan_filter(MainDataTable, 'OFFICIAL_TOTAL_CATCH_MT', remove=T) 
#' }

  #Call in datasets
  out <- data_pull(dat)
  dat <- out$dat
  datset <- out$dataset
  
   tmp <- tempfile()
   #Check for NAs
  if (any(apply(dataset, 2, function(x) any(is.na(x)))==TRUE)) {
    cat("The", names(which(apply(dataset, 2, function(x) any(is.na(x)))==TRUE)), "columns contain NAs. Consider using nan_filter to replace or remove NAs", file=tmp)
  
  } else {
    #cat("\nNo columns in the dataframe contain NaNs", file=paste(getwd(),'/Logs/InforMessage',Sys.Date(),'.txt', sep=''), append=TRUE)
    cat("No columns in the dataframe contain NAs", file=tmp, append=TRUE)
  }
  
   #check for NaNs
   if (any(apply(dataset, 2, function(x) any(is.nan(x)))==TRUE)) {
     #cat("\nThe", names(which(colSums(is.nan.data.frame(dataset)) != 0)), "columns contain", 
     #unname(which(colSums(is.nan.data.frame(dataset)) != 0)), "NaNs. Consider using nan.filter to replace or remove NaNs", 
     #file=paste(getwd(),'/Logs/InforMessage',Sys.Date(),'.txt', sep=''), append=TRUE)
     cat("The", names(which(apply(dataset, 2, function(x) any(is.nan(x)))==TRUE)), "columns contain NaNs. Consider using nan_filter to replace or remove NaNs", file=tmp, append=TRUE)
     
   } else {
     #cat("\nNo columns in the dataframe contain NaNs", file=paste(getwd(),'/Logs/InforMessage',Sys.Date(),'.txt', sep=''), append=TRUE)
     cat("No columns in the dataframe contain NaNs", file=tmp, append=TRUE)
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
  nan_identify_function <- list()
  nan_identify_function$functionID <- 'nan_identify'
  nan_identify_function$args <- c(dat)
  nan_identify_function$kwargs <- list()
  nan_identify_function$output <- c('')
  nan_identify_function$msg <- suppressWarnings(readLines(tmp))
  functionBodyout$function_calls[[length(functionBodyout$function_calls)+1]] <- (nan_identify_function)
  logbody$fishset_run <- list(infoBodyout, functionBodyout)
  write(jsonlite::toJSON(logbody, pretty = TRUE, auto_unbox = TRUE), paste(getwd(), "/inst/Logs/", Sys.Date(), ".json", sep = ""))
  assign("functionBodyout", value = functionBodyout, pos = 1)
  rm(tmp)
}


# Replaces nans in the data column with the choosen value or removes rows containing NaNs
nan_filter <- function(dat, x, replace = F, remove = F, rep.value=NA, over_write=FALSE) {
  #' Filters NaN's from data frame
  #'
  #' @param dat Main data frame over which to apply function. Table in fishset_db database should contain the string `MainDataTable`.
  #' @param x Column(s) in data framce in which to remove or replace NaNs. If multiple columns are passed use x=c().
  #' @param replace TRUE/FALSE Replace NaNs in a vector? Defaults to FALSE.
  #' @param remove TRUE/FALSE Remove all remove the entire row of the dataframe where NaN is present in a specified column? Defaults to FALSE.
  #' @param rep.value Value to replace all NaNs in a column. Defaults to the mean value of the column.
  #' @param over_write Over_write modified data set in fishset_db database?
  #' @details Replaces nans in the data column with the chosen value or removes rows containing NaNs. Modified data frame saved to fishset_db database.
  #' @keywords NaN
  #' @return Returns the modified data frame
  #' @export nan_filter
  #' @examples 
  #' \dontrun{
  #' nan_identify(MainDataTable)
  #' mod.dat <-nan_filter(MainDataTable, 'OFFICIAL_TOTAL_CATCH_MT') 
  #' mod.dat <- nan_filter(MainDataTable, 'OFFICIAL_TOTAL_CATCH_MT', replace=T) 
  #' mod.dat <- nan_filter(MainDataTable, 'OFFICIAL_TOTAL_CATCH_MT', replace=T, rep.value=0) 
  #' mod.dat <- nan_filter(MainDataTable, 'OFFICIAL_TOTAL_CATCH_MT', remove=T) 
  #' }
  
  #Call in datasets
  out <- data_pull(dat)
  dat <- out$dat
  datset <- out$dataset
  
    int <- dataset
    tmp <- tempfile()
    for(i in 1:length(x)){
    x.name <- x[i]
    if (any(is.nan(int[, x.name])) == T) {
      cat(length(which(is.nan(int[, x.name]) == T)), "NaNs identified in variable",  x.name, file=tmp)
      
      # the identified rep.value (defaults to mean value)
      if (replace == T) {
        if(is.nan(rep.value)==TRUE) {
          rep.value <- mean(int[, x.name], na.rm = T)
        }
          if (is.numeric(int[, x.name]) == T) {
          # Further actions are only taken if NaNs exist in the selected variable
        int[is.nan(int[, x.name]), x.name] = rep.value
        cat("\nAll NaNs in", x.name, "have been replaced with", rep.value, file=tmp, append=T)
        } else {
          # Message returned if the selected variable is not numeric
          cat("Variable is not numeric. Function not applied", file=tmp, append=T)
        }
   
        # If remove is true then row inwhich the NaN occurs for selected column will be removed.
      } else if (remove == T) {
        cat("\nThe entire row will be removed from the dataframe.", file=tmp, append=T)
        int <- int[!is.nan(int[, x.name]), ]
      }
    } else {
      cat("\nNo NaNs present in variable", x.name, file=tmp)
    }
    }  
    
  print(suppressWarnings(readLines(tmp)))  
  
  if(over_write==TRUE&any(is.nan(dataset))==TRUE){
  suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite"))
  DBI::dbWriteTable(fishset_db, deparse(substitute(dat)), int, overwrite=over_write)
  DBI::dbDisconnect(fishset_db)
  }
  
  
  
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
  nan_filter_function <- list()
  nan_filter_function$functionID <- 'nan_filter'
  nan_filter_function$args <-  c(dat,  deparse(substitute(x)), replace, remove, rep.value)
  nan_filter_function$kwargs <- list()
  nan_filter_function$output <-  c(deparse(substitute(dat)))
  nan_filter_function$msg <- suppressWarnings(readLines(tmp))
  functionBodyout$function_calls[[length(functionBodyout$function_calls)+1]] <- nan_filter_function
  logbody$fishset_run <- list(infoBodyout, functionBodyout)
  write(jsonlite::toJSON(logbody, pretty = TRUE, auto_unbox = TRUE), paste(getwd(), "/inst/Logs/", Sys.Date(), ".json", sep = ""))
  assign("functionBodyout", value = functionBodyout, pos = 1)
  rm(tmp)
  
  return(int)
}



# Replaces nans in the dataColumn with the choosen value or removes rows containing NaNs
na_filter <- function(dat, x, replace = F, remove = F, rep.value=NA, over_write=FALSE) {
  #' Filters NA's from variable
  #'
  #'  Function to return a modified dataframe where NAs have been replaced or removed.
  #'
  #' @param dat Main data frame over which to apply function. Table in fishset_db database should contain the string `MainDataTable`.
  #' @param x Column(s) in data framce in which to remove or replace NaNs. If multiple columns are passed use x=c().
  #' @param replace TRUE/FALSE Replace NaNs in a vector? Defaults to FALSE.
  #' @param remove TRUE/FALSE Remove all remove the entire row of the dataframe where NaN is present in a specified column? Defaults to FALSE.
  #' @param rep.value Value to replace all NaNs in a column. Defaults to the mean value of the column.
  #' @param over_write Over_write modified data set in fishset_db database?
  #' @details Function to return a modified dataframe where NAs have been replaced or removed. Modified data frame saved to fishset_db database.
  #' @keywords NA
  #' @return Returns the modified dataframe
  #' @export na_filter
  #' @examples 
  #' \dontrun{
  #' nan_identify(MainDataTable)
  #' mod.dat <- na_filter(MainDataTable, 'OFFICIAL_TOTAL_CATCH_MT') 
  #' mod.dat <- na_filter(MainDataTable, 'OFFICIAL_TOTAL_CATCH_MT', replace=T) 
  #' mod.dat <- na_filter(MainDataTable, 'OFFICIAL_TOTAL_CATCH_MT', replace=T, rep.value=0) 
  #' mod.dat <- na_filter(MainDataTable, 'OFFICIAL_TOTAL_CATCH_MT', remove=T) 
  #' }
  
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
  
  
  int <- dataset
  tmp <- tempfile()
  for(i in 1:length(x)){
    x.name <- x[i]
    if (any(is.na(int[, x.name])) == T) {
       cat(length(which(is.na(int[, x.name]) == T)), "NAs identified in variable",  x.name, file=tmp)
      
      # the identified rep.value (defaults to mean value)
      if (replace == T) {
        if(is.na(rep.value)==TRUE) {
          rep.value <- mean(int[, x.name], na.rm = T)
        }
        if (is.numeric(int[, x.name]) == T) {
          # Further actions are only taken if NAs exist in the selected variable
          int[is.na(int[, x.name]), x.name] = rep.value
          cat("\nAll NAs in", x.name, "have been replaced with", rep.value, file=tmp, append=T)
        } else {
          # Message returned if the selected variable is not numeric
          cat("Variable is not numeric. Function not applied", file=tmp, append=T)
        }
        
        # If remove is true then row inwhich the NaN occurs for selected column will be removed.
      } else if (remove == T) {
        cat("\nThe entire row will be removed from the dataframe.", file=tmp, append=T)
        int <- int[!is.na(int[, x.name]), ]
      }
    } else {
      cat("\nNo NAs present in variable", x.name, file=tmp)
    }
  }  
  print(suppressWarnings(readLines(tmp)))
  
  #Save the revised data set
  if(over_write==TRUE&any(is.na(dataset))==TRUE){
  suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite"))
  DBI::dbWriteTable(fishset_db, deparse(substitute(dat)), int, overwrite=over_write)
  DBI::dbDisconnect(fishset_db)
  print('Data saved to database')
  }
  
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
  nan_filter_function <- list()
  nan_filter_function$functionID <- 'na_filter'
  nan_filter_function$args <-  c(dat,  deparse(substitute(x)), replace, remove, rep.value)
  nan_filter_function$kwargs <- list()
  nan_filter_function$output <-  c(deparse(substitute(dat)))
  nan_filter_function$msg <- suppressWarnings(readLines(tmp))
  functionBodyout$function_calls[[length(functionBodyout$function_calls)+1]] <- nan_filter_function
  logbody$fishset_run <- list(infoBodyout, functionBodyout)
  write(jsonlite::toJSON(logbody, pretty = TRUE, auto_unbox = TRUE), paste(getwd(), "/inst/Logs/", Sys.Date(), ".json", sep = ""))
  assign("functionBodyout", value = functionBodyout, pos = 1)
  rm(tmp)
  
  return(int)
}
