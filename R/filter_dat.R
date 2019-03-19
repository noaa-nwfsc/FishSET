filter_data <- function(dataset, x, exp, save.filter = FALSE, flog.dat = TRUE) {
  #'  Define and store filters
  #'
  #' @param dataset dataframe or matrix over which to apply filter
  #' @param x column in dataframe over which to filter will be applied
  #' @param exp Filter expression. Should take on the form of 'x<100' or is.na(x)==F.
  #' @param save.filter Whether to save the filterTable as a csv file
  #' @param flog.dat Whether to print filterTable to the log file
  #' @importFrom utils head read.csv write.csv
  #' @keywords filter, subset
  #' @export filter_data
  #' @return  filterTable saved into the global environment. The data table will grow with each run of the function.
  #' @details This function allows users to define and store data filters which can then be applied to the data. The filter dataframe can be saved and will be logged in the log file.
  
  # @examples Generate data for example: 
  # filter_data(MainDataTable, 'PERFORMANCE_Code','PERFORMANCE_Code==1') 

  if (exists("filterTable") == F) {
    filterTable <- data.frame(dataframe = NA, vector = NA, FilterFunction = NA)
    filterTable[1, ] <- c(deparse(substitute(dataset)), deparse(substitute(x)), exp)
  } else {
    filterTable <- rbind(filterTable, c(deparse(substitute(dataset)), deparse(substitute(x)), exp))
  }
  if (save.filter == TRUE) {
    write.csv(filterTable, paste(filterTable, "_", deparse(substitute(dataset)), ".csv", sep=''), sep = F, row.names = FALSE)
  }
  if (flog.dat == TRUE & save.filter == FALSE) {
    body <- list()
    logging_code()  
    filter_data_function <- list()
    filter_data_function$functionID <- 'filter_data'
    filter_data_function$args <- c(deparse(substitute(dataset)), x, exp)
    filter_data_function$msg <- filterTable
    functionBodyout$function_calls[[length(functionBodyout$function_calls)+1]] <- (filter_data_function)
    body$fishset_run <- list(infoBodyout, functionBodyout)
    write(jsonlite::toJSON(body, pretty = TRUE, auto_unbox = TRUE),paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""))
    list2env(functionBodyout, envir = .GlobalEnv)
    
    #write.table(filterTable, file = paste("Logs/ Log_file_", Sys.Date(), ".log"))
  }
  list2env(filterTable, envir = .GlobalEnv)
  print(filterTable)
}



filter_dat <- function(dataset, exp, use.filter.table = F) {
  #' Remove rows based on user-defined filter
  #'
  #' @param dataset dataframe or matrix over which to apply filter
  #' @param exp Filter expression. Use row row number if use.filter.table==TRUE. Otherwise, should take on the form of 'x<100' or is.na(x)==F.
  #' @param use.filter.table TRUE or FALSE. If true then data is subsetted based on a filter in filterTable and exp is the row containing the filter.
  #' @keywords filter, subset
  #' @export filter_dat
  #' @return filter_dat applies user-definted filters. Output must be saved
  #' @details This function allows users to define and store data filters which can then be applied to the data. The filter dataframe can be saved and will be logged in the log file.
  # Example 
  # newdat <- filter_dat(MainDataTable, exp=3, use.filter.table=T)
  # newdat <- filter_dat(MainDataTable, exp='PERFORMANCE_Code==1', use.filter.table=F)
  
  # NaNs only occurs on Numeric Variables
  if (use.filter.table == T) {
    filterTable <- read.csv(paste(filterTable, "_", deparse(substitute(dataset)), ".csv", sep=''))
#        write(layout.json.ed(trace, "filter_dat", deparse(substitute(dataset)), x = "user-defined", 
#                         msg = paste("Rows have been removed based on", filterTable[exp, 3])),
#          paste(getwd(), "/Logs/", 'Messages', Sys.Date(), ".json", sep = ""), append = T)

        cat("The entire row will be removed from the dataframe.")
    dataset <- subset(dataset, eval(parse(text = filterTable[exp, 3])))
    
    
    filter_dat_function <- list()
    filter_dat_function$functionID <- 'filter_dat'
    filter_dat_function$args <- c(deparse(substitute(dataset)), exp, use.filter.table)
    filter_dat_function$output <- deparse(substitute(dataset))
    filter_dat_function$msg <- paste("Rows have been removed based on", filterTable[exp, 3])
    functionBodyout$function_calls[[length(functionBodyout$function_calls)+1]] <- (filter_dat_function)
    body$fishset_run <- list(infoBodyout, functionBodyout)
    write(jsonlite::toJSON(body, pretty = TRUE, auto_unbox = TRUE),paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""))
    list2env(functionBodyout, envir = .GlobalEnv)
    
    return(dataset)
  } else {
    dataset <- subset(dataset, eval(parse(text = exp)))
    #write(layout.json.ed(trace, "filter_dat", deparse(substitute(dataset)), x = "user-defined", 
    #                     msg = paste("Rows have been removed based on", exp)),
    #      paste(getwd(), "/Logs/", 'Messages', Sys.Date(), ".json", sep = ""), append = T)

    body <- list()
    logging_code()  
    filter_dat_function <- list()
    filter_dat_function$functionID <- 'filter_dat'
    filter_dat_function$args <- c(deparse(substitute(dataset)), exp, use.filter.table)
    filter_dat_function$output <- deparse(substitute(dataset))
    filter_dat_function$msg <- paste("Rows have been removed based on", exp)
    functionBodyout$function_calls[[length(functionBodyout$function_calls)+1]] <- (filter_dat_function)
    body$fishset_run <- list(infoBodyout, functionBodyout)
    write(jsonlite::toJSON(body, pretty = TRUE, auto_unbox = TRUE),paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""))
    list2env(functionBodyout, envir = .GlobalEnv)   
        
    return(dataset)
  }
}
