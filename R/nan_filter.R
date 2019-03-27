#' Filters NaN's from variable

# @examples my.df <- data.frame(length=c(1, 2, 3,4), height=c(NaN, 2, NaN,10), age=c(1,2,4,6),ID=c('a',Inf,'c','d'),speed=c(NaN,NA,44,40)) nan.identify(my.df)
# nan.filter(my.df, 'speed') mod.dat <- nan.filter(my.df, 'speed', replace=T) 
# mod.dat <- nan.filter(my.df, 'speed', replace=T, rep.value=0) 
# mod.dat <- nan.filter(my.df, 'speed', remove=T) 

nan_identify <- function(dataset) {
  #' Identify NaN's in dataset
  #'
  #' @param dataset dataframe or matrix over which to check for NaNs
  #' @keywords NaN
  #' @export nan_identify

  #df.name <- deparse(substitute(dataset))
  #write(layout.json.ed(trace, "nan.identify", df.name, "all"), paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""), append = T)
  tmp <- tempfile()
  if (any(apply(dataset, 2, function(x) any(is.na(x)))==TRUE)) {
        #cat("\nThe", names(which(colSums(is.nan.data.frame(dataset)) != 0)), "columns contain", 
        #unname(which(colSums(is.nan.data.frame(dataset)) != 0)), "NaNs. Consider using nan.filter to replace or remove NaNs", 
        #file=paste(getwd(),'/Logs/InforMessage',Sys.Date(),'.txt', sep=''), append=TRUE)
    cat("The", names(which(apply(dataset, 2, function(x) any(is.na(x)))==TRUE)), "columns contain NaNs. Consider using nan_filter to replace or remove NaNs", file=tmp)
  
  } else {
    #cat("\nNo columns in the dataframe contain NaNs", file=paste(getwd(),'/Logs/InforMessage',Sys.Date(),'.txt', sep=''), append=TRUE)
    cat("No columns in the dataframe contain NaNs", file=tmp, append=TRUE)
  }
  
  print(suppressWarnings(readLines(tmp)))
  if(!exists('logbody')) { 
    logging_code()
  } 
  nan_identify_function <- list()
  nan_identify_function$functionID <- 'nan_identify'
  nan_identify_function$args <- c(deparse(substitute(dataset)))
  nan_identify_function$msg <- suppressWarnings(readLines(tmp))
  functionBodyout$function_calls[[length(functionBodyout$function_calls)+1]] <- (nan_identify_function)
  logbody$fishset_run <- list(infoBodyout, functionBodyout)
  write(jsonlite::toJSON(logbody, pretty = TRUE, auto_unbox = TRUE), paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""))
  assign("functionBodyout", value = functionBodyout, pos = 1)
  rm(tmp)
}


# Replaces nans in the dataColumn with the choosen value or removes rows containing NaNs
nan_filter <- function(dataset, x, replace = F, remove = F, rep.value = mean(dataset[, x], na.rm = T)) {
  #' Filters NaN's from variable
  #'
  #'  Function to return a modified dataframe where NaNshave been replaced or removed.
  #'
  #' @param dataset dataframe or matrix over which to check for NaNs
  #' @param x column in dataframe over which to remove or replace NaNs
  #' @param replace whether to (TRUE) or not to (FALSE) replace NaNs in a column. Defaults to FALSE.
  #' @param remove whether to (TRUE) or not to (FALSE) remove all remove the entire row of the dataframe where NaN is present in a specified column. Defaults to FALSE.
  #' @param rep.value value to replace all NaNs in a column. Defaults to the mean value of the column.
  #' @keywords NaN
  #' @return Returns the modified dataframe
  #' @export nan_filter
    int <- dataset
    tmp <- tempfile()
    for(i in 1:length(x)){
    x.name <- x[i]
    if (any(is.na(int[, x.name])) == T) {
      #cat('\n',length(which(is.nan(int[, x]) == T)), "NaNs identified in variable",  x, ".\n",
      #    file=paste(getwd(),'/Logs/InforMessage',Sys.Date(),'.txt', sep=''), append=TRUE)
      cat(length(which(is.na(int[, x.name]) == T)), "NaNs identified in variable",  x.name, file=tmp)
      
      # the identified rep.value (defaults to mean value)
      if (replace == T) {
          if (is.numeric(int[, x.name]) == T) {
          # Further actions are only taken if NaNs exist in the selected variable
        int[is.na(int[, x.name]), x.name] = rep.value
        cat("\nAll NaNs in", x.name, "have been replaced with", rep.value, file=tmp, append=T)
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
      cat("\nNo NaNs present in variable", x.name, file=tmp, append=T)
    }
  }  
  print(suppressWarnings(readLines(tmp)))
  
  if(!exists('logbody')) { 
    logging_code()
  } 
  nan_filter_function <- list()
  nan_filter_function$functionID <- 'nan_filter'
  nan_filter_function$args <-  c(deparse(substitute(dataset)),  deparse(substitute(x)), replace, remove, deparse(substitute(rep.value)))
  nan_filter_function$output <-  c(deparse(substitute(dataset)))
  nan_filter_function$msg <- suppressWarnings(readLines(tmp))
  functionBodyout$function_calls[[length(functionBodyout$function_calls)+1]] <- nan_filter_function
  logbody$fishset_run <- list(infoBodyout, functionBodyout)
  write(jsonlite::toJSON(logbody, pretty = TRUE, auto_unbox = TRUE), paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""))
  assign("functionBodyout", value = functionBodyout, pos = 1)
  rm(tmp)
  return(int)
}
