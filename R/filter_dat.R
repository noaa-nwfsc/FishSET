filter_table <- function(dat, x, exp, project) {
  #'  Define and store filter expressions
  #'
  #' @param dat Main data frame over which to apply function. Table in fishet_db database should contain the string `MainDataTable`.
  #' @param x Column in dat aframe over which filter will be applied
  #' @param exp Filter expression. Should take on the form of `x<100` or `is.na(x)==F`.
  #' @param project Name of project
  #' @importFrom utils head read.csv write.csv 
  #' @importFrom DBI dbConnect dbWriteTable dbDisconnect
  #' @keywords filter, subset
  #' @export filter_table
  #' @return  Filter expressions saved as a table into the global environment. 
  #' @details This function allows users to define and store data filter expressions which can then be applied to the data.  
  #' The filter table will be saved in the SQLite database under the project name (\emph{project}), the date the table was created and the 
  #' string \emph{filterTable}. The new filter functions are added each time the function is run and the table is also automatically updated in the 
  #' fishet_db database. The function call will be logged in the log file.
  #' @examples 
  #' \dontrun{  
  #' filter_data(MainDataTable, 'PERFORMANCE_Code','PERFORMANCE_Code==1', 'pcod') 
  #' }
  #' 

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
    filter_data_function$args <- c(deparse(substitute(dat)), x, exp, save.filter, flog.dat)
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



filter_dat <- function(dat, exp, filterTable) {
  #' Remove rows based on user-defined filter expressions
  #'
  #' @param dat Main data frame over which to apply function. Table in fishet_db database should contain the string `MainDataTable`.
  #' @param exp Filter expression. Use row number if use.filter.table==TRUE. Otherwise, should take on the form of `x<100` or `is.na(x)==F`.
  #' @param filterTable Name of filter table in fishset_db database. Name should contain the phrase 'filterTable'.
  #' @keywords filter, subset
  #' @export filter_dat
  #' @return Filtered data frame
  #' @details Filter data frame based on pre-defined filter expression from filterTable or user-defined filter expression.
  #' @examples 
  #' \dontrun{  
  #' newdat <- filter_dat(MainDataTable, exp=3, filterTable='pcodfilterTable01012011')
  #' newdat <- filter_dat(MainDataTable, exp='PERFORMANCE_Code==1', filteTable='')
  #' }
  #' 
  
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
  
  
  # NaNs only occurs on Numeric Variables
  if (!is.null(filterTable)) {

    fishset_db <- DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite")
    DBI::dbGetQuery(fishset_db, paste0("SELECT * FROM", paste0("'", noquote(filterTable), "'"))) 
    DBI::dbDisconnect(fishset_db)

     cat("The entire row will be removed from the dataframe.")
    dataset <- subset(dataset, eval(parse(text = filterTable[exp, 3])))
    
    
     } else {
    dataset <- subset(dataset, eval(parse(text = exp)))
     }
  
    if(!exists('logbody')) { 
      logging_code()
    } 
    filter_dat_function <- list()
    filter_dat_function$functionID <- 'filter_dat'
    filter_dat_function$args <- c(deparse(substitute(dat)), exp, filterTable)
    filter_dat_function$kwargs <- list()
    filter_dat_function$output <- deparse(substitute(dataset))
    filter_dat_function$msg <- paste("Rows have been removed based on", exp)
    functionBodyout$function_calls[[length(functionBodyout$function_calls)+1]] <- (filter_dat_function)
    logbody$fishset_run <- list(infoBodyout, functionBodyout)
    write(jsonlite::toJSON(logbody, pretty = TRUE, auto_unbox = TRUE),paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""))
    assign("functionBodyout", value = functionBodyout, pos = 1) 
        
    return(dataset)
 
}
