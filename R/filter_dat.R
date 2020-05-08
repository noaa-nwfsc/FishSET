filter_table <- function(dat, project, x, exp) {
  #'  Define and store filter expressions
  #'
  #' @param dat Main data frame over which to apply function. Table in FishSET database should contain the string `MainDataTable`.
  #' @param project Name of project
  #' @param x Column in dataaframe over which filter will be applied
  #' @param exp Filter expression. Should take on the form of `x<100` or `is.na(x)==F`.
  #' @importFrom utils head read.csv write.csv 
  #' @importFrom DBI dbConnect dbWriteTable dbDisconnect
  #' @keywords filter, subset
  #' @export filter_table
  #' @return  Filter expressions saved as a table into the global environment. 
  #' @details This function allows users to define and store data filter expressions which can then be applied to the data.  
  #' The filter table will be saved in the SQLite fishset_db  database under the project name (\emph{project}) and (\emph{filterTable}). 
  #' The new filter functions are added each time the function is run and the table is also automatically updated in the 
  #' FishSET database. The function call will be logged in the log file.
  #' @examples 
  #' \dontrun{  
  #' filter_table('pcodMainDataTable', 'pcod', 'PERFORMANCE_Code','PERFORMANCE_Code==1') 
  #' }
  #' 

  #Call in datasets
  out <- data_pull(dat)
  dat <- out$dat
  dataset <- out$dataset

  if (table_exists(paste0(project, "FilterTable")) == F) {
    filterTable <- data.frame(dataframe = NA, vector = NA, FilterFunction = NA)
    filterTable[1, ] <- c(dat, x, exp)
  } else {
    filterTable <- table_view(paste0(project, "FilterTable"))
    filterTable <- rbind(filterTable, c(dat, x, exp))
  }

  
  fishset_db <- suppressWarnings(DBI::dbConnect(RSQLite::SQLite(), locdatabase()))
  DBI::dbWriteTable(fishset_db, paste0(project, 'FilterTable'),  filterTable, overwrite=TRUE)
  DBI::dbDisconnect(fishset_db)
  cat('Table saved to fishset_db database')
  
    filter_data_function <- list()
    filter_data_function$functionID <- 'filter_table'
    filter_data_function$args <- c(dat, project, x, exp)
    filter_data_function$kwargs <- list()
    filter_data_function$output <- c('')
    filter_data_function$msg <- filterTable
    log_call(filter_data_function)
    
    save_table(dataset, project, "filter_table")
     
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
  out <- data_pull(dat)
  dat <- out$dat
  dataset <- out$dataset
  
  
  # NaNs only occurs on Numeric Variables
  if (!is.null(filterTable)) {

    fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase())
    f_tab <- DBI::dbGetQuery(fishset_db, paste0("SELECT * FROM", paste0("'", noquote(filterTable), "'"))) 
    DBI::dbDisconnect(fishset_db)

     cat("The entire row will be removed from the dataframe.")
    dataset <- subset(dataset, eval(parse(text = f_tab[exp, 3])))
    
    
     } else {
    dataset <- subset(dataset, eval(parse(text = exp)))
     }
  
    filter_dat_function <- list()
    filter_dat_function$functionID <- 'filter_dat'
    filter_dat_function$args <- c(dat, exp, filterTable)
    filter_dat_function$kwargs <- list()
    filter_dat_function$output <- deparse(substitute(dataset))
    filter_dat_function$msg <- paste("Rows have been removed based on", exp)
    log_call(filter_dat_function)
    
    return(dataset)
 
}
