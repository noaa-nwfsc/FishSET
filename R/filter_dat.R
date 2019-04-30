filter_table <- function(dataset, x, exp, project) {
  #'  Define and store filter expressions
  #'
  #' @param dataset dataframe or matrix over which to apply filter
  #' @param x column in dataframe over which filter will be applied
  #' @param exp Filter expression. Should take on the form of 'x<100' or 'is.na(x)==F'.
  #' @param project name of project
  #' @importFrom utils head read.csv write.csv 
  #' @importFrom DBI dbConnect dbWriteTable dbDisconnect
  #' @keywords filter, subset
  #' @export filter_table
  #' @return  Filter expressions saved as a table into the global environment. The data table will grow with each run of the function.
  #' @details This function allows users to define and store data filter expressions which can then be applied to the data.  The filter table will be saved in the sqlite database under the project name (\emph{project}), the date the table was created and the word \emph{filterTable}.  The new filter functions are added each time the function is run and the table is also automatically updated in the fishet_db database. The function call will be logged in the log file.
  
  #' @examples Generate data for example:
  #' \dontrun{  
  #' filter_data(MainDataTable, 'PERFORMANCE_Code','PERFORMANCE_Code==1', pcod') 
  #' }

  if (exists("filterTable") == F) {
    filterTable <- data.frame(dataframe = NA, vector = NA, FilterFunction = NA)
    filterTable[1, ] <- c(deparse(substitute(dataset)), deparse(substitute(x)), exp)
  } else {
    filterTable <- rbind(filterTable, c(deparse(substitute(dataset)), deparse(substitute(x)), exp))
  }
  if (save.filter == TRUE) {
    write.csv(filterTable, paste(filterTable, "_", deparse(substitute(dataset)), ".csv", sep=''), sep = F, row.names = FALSE)
  }
  
  fishset_db <- DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite")
  DBI::dbWriteTable(fishset_db, paste(project, 'filterTable', Sys.Date(), sep=''),  filterTable, overwrite=TRUE)
  DBI::dbDisconnect(fishset_db)
  cat('Data saved to fishset_db database')
  
  
     if(!exists('logbody')) { 
      logging_code()
    } 
    filter_data_function <- list()
    filter_data_function$functionID <- 'filter_table'
    filter_data_function$args <- c(deparse(substitute(dataset)), x, exp, save.filter, flog.dat)
    filter_data_function$kwargs <- list()
    filter_data_function$output <- c('')
    filter_data_function$msg <- filterTable
    functionBodyout$function_calls[[length(functionBodyout$function_calls)+1]] <- (filter_data_function)
    logbody$fishset_run <- list(infoBodyout, functionBodyout)
    write(jsonlite::toJSON(logbody, pretty = TRUE, auto_unbox = TRUE),paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""))
    assign("functionBodyout", value = functionBodyout, pos = 1)
    
  assign("filterTable", filterTable, pos=1)
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
    
    
    if(!exists('logbody')) { 
      logging_code()
    } 
    filter_dat_function <- list()
    filter_dat_function$functionID <- 'filter_dat'
    filter_dat_function$args <- c(deparse(substitute(dataset)), exp, use.filter.table)
    filter_dat_function$kwargs <- list()
    filter_dat_function$output <- deparse(substitute(dataset))
    filter_dat_function$msg <- paste("Rows have been removed based on", filterTable[exp, 3])
    functionBodyout$function_calls[[length(functionBodyout$function_calls)+1]] <- (filter_dat_function)
    logbody$fishset_run <- list(infoBodyout, functionBodyout)
    write(jsonlite::toJSON(logbody, pretty = TRUE, auto_unbox = TRUE),paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""))
    assign("functionBodyout", value = functionBodyout, pos = 1)
    
    return(dataset)
  } else {
    dataset <- subset(dataset, eval(parse(text = exp)))
    #write(layout.json.ed(trace, "filter_dat", deparse(substitute(dataset)), x = "user-defined", 
    #                     msg = paste("Rows have been removed based on", exp)),
    #      paste(getwd(), "/Logs/", 'Messages', Sys.Date(), ".json", sep = ""), append = T)

    if(!exists('logbody')) { 
      logging_code()
    } 
    filter_dat_function <- list()
    filter_dat_function$functionID <- 'filter_dat'
    filter_dat_function$args <- c(deparse(substitute(dataset)), exp, use.filter.table)
    filter_dat_function$kwargs <- list()
    filter_dat_function$output <- deparse(substitute(dataset))
    filter_dat_function$msg <- paste("Rows have been removed based on", exp)
    functionBodyout$function_calls[[length(functionBodyout$function_calls)+1]] <- (filter_dat_function)
    logbody$fishset_run <- list(infoBodyout, functionBodyout)
    write(jsonlite::toJSON(logbody, pretty = TRUE, auto_unbox = TRUE),paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""))
    assign("functionBodyout", value = functionBodyout, pos = 1) 
        
    return(dataset)
  }
}
