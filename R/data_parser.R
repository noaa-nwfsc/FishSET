#'  Import data
#' @export read_dat
#' @export fishset_compare
#' @export load_maindata
#' @export load_port
#' @export load_aux
#' @export load_grid
#' @export dataindex_update
#' @export main_mod


read_dat <- function(x, data.type ) { 
#' @param x Name and directory of data frame to be read in. For example, `nmfs_manage_simple.shp`.
#' @param data.type csv, mat, json, shape, txt, spss, stata, R)
#' @importFrom sf read_sf
#' @importFrom R.matlab readMat
#' @importFrom jsonlite fromJSON
#' @importFrom foreign read.spss read.dta
#' @importFrom utils read.table
#' @details Uses the appropriate function to read in data based on data type. Supported data types include shape, csv, json, matlab, R, spss, and stata files.
#' @examples 
#' \dontrun{ 
#' dat <- read_dat('nmfs_manage_simple.shp', 'shape')
#' }
  
  if(data.type=='R'){
    return(get(load(x)))
  } else if(data.type=='mat'){
    R.matlab::readMat(x) 
  } else if(data.type=='json'){
    sf::st_read(x)
  } else if(data.type=='csv'){
    read.csv(x)
  } else if(data.type=='spss'){
    foreign::read.spss(x)
  } else if(data.type=='stata'){
    foreign::read.dta(x) 
  } else if(data.type == 'shape'){
    sf::st_read(x)#'~/path/to/file.shp' 
  } else {
    utils::read.table(x)
  }
}


fishset_compare <- function(x, y, compare=c(TRUE,FALSE)){
  #' Compare column names of new data frame to previously saved version
  #' @param x Updated data frame to be saved
  #' @param y Previously saved version of data frame
  #' @param compare TRUE/FALSE Compare new data frame to previously saved data frame before saving data frame `x` to SQLite database.
  #' @export
  #' @importFrom DBI dbConnect dbDisconnect dbListTables
  #' @details This function is called indirectly by the data import functions (\code{\link{load_maindata}}, \code{\link{load_port}}, \code{\link{load_aux}}, 
  #' \code{\link{load_grid}}). The function is designed to check for consistency between versions of the same data frame so that the logged functions can be used to rerun the previous analysis on the updated data. 
  #' To use the logged functions to rerun code after data has been updated (i.e., new year of data), the column names, including spelling and capitalization, must match the previous version.
  #' Set the `compare` parameter to TRUE to compare column names of the new and previously saved data frames. The new data frame will be saved to the SQLite database if column names match.
  #' If no previous versions of the data frame exist in the SQLite database or the analysis will not be rerun using saved function calls in the log file, set the `compare` parameter to FALSE.
  #'  No comparison will be made and the new file will be saved to the database.

  fishset_db <- suppressWarnings(DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite"))
  if(compare==TRUE){
    if(is.null(y)==TRUE | table_exists(y)==FALSE){
      print(DBI::dbListTables(fishset_db))
      stop(paste(y, 'not defined or does not exist. Consider using one of the tables listed above that exist in the database.'))
    } else {
      old <- table_view(y)
      new <- toupper(colnames(x))
      old <- toupper(colnames(old))
      if(is.na(table(is.na(match(new, old)))[2])==TRUE){
        cat('Column names match between previous and new data sets. New data frame uploaded to database')
        #DBI::dbWriteTable(fishset_db, paste(paste(deparse(substitute(x)),Sys.Date(), sep=''), deparse(substitute(x))))
      } else {
        cat(length(table(is.na(match(new,old)))[2]),'/',length(match(new,old)), ' mismatches', sep='')
        cat(noquote(paste(deparse(substitute(x)),'[',as.character(which(is.na(match(new,old))==TRUE)),']', sep='')),':', new[which(is.na(match(new,old))==TRUE)])
        cat(noquote(paste(deparse(substitute(y)),'[',as.character(which(is.na(match(new,old))==TRUE)),']', sep='')),':', old[which(is.na(match(new,old))==TRUE)])
        stop('Column names did not match. Data set will not be uploaded to database')
        
      }
    }
  } else {
    cat('')
    #DBI::dbWriteTable(fishset_db, paste(deparse(substitute(x)), Sys.Date(), sep=''), deparse(substitute(x)))
  }
  DBI::dbDisconnect(fishset_db)
}


load_maindata <- function(dat, over_write=TRUE, project=NULL, compare=FALSE, y=NULL){
  #' Load data into SQL database
  #' @param dat Main data frame over which to apply function. Table in fishset_db database should contain the string `MainDataTable`.
  #' @param over_write TRUE/FALSE Save over data table previously saved in fishset_db database?
  #' @param project Name of project. Parameter is used to generate meaningful table names in fishset_db database.
  #' @param compare TRUE/FALSE If TRUE, compare new data frame to previously saved data frame `y` in fishset_db before saving new data frame to the fishset_db database
  #' @param y Name of previously saved table in fishset_db database. y must be defined if `compare==TRUE`.
  #' @importFrom jsonlite toJSON
  #' @importFrom DBI dbConnect dbDisconnect dbWriteTable
  #' @details Runs the fishset_compare function if `compare` is TRUE.  Then checks the data for common data issues using the \code{\link{data_verification}} function and that latitude and longitude are defined. 
  #' The index table that contains information for each variable on units, data format, and specialized variable is then generated. Finally, the data sets (main and index tables) are saved 
  #' in the fishset_db as raw and working tables. In both cases, the table name is the `project`, if defined, and the table type `MainDataTable` or `MainDataTableInfo`. Date is also attached to the name for the raw data. 
  #' 
  #' @examples 
  #' \dontrun{  
  #' load_maindata(dataset='MainDataTable', over_write=TRUE, project='', 
  #'               compare=TRUE, y='MainDataTable01012011') 
  #' }
  
   dataset <- dat 
     #Call in datasets
  suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite"))
  if(compare==TRUE){

  fishset_compare(dataset,y,compare)
  }
  ##-----------MainDataTable--------------------##
  data_verification_call(dataset)
  # Check to see if lat/long or fish area is in dataset
  indx <- grepl("lat|lon|area", colnames(dataset), ignore.case = TRUE)
  if (length(dataset[indx]) > 0) {
    cat("Pass: Latitude and longitude or fishing area included in the data frame")
  } else {
    stop("Dataset must contain either latitude and longitude or fishing area designation.")
  }
  
  n <- which(grepl('DATE|TRIP_END|TRIP_START',colnames(dataset), ignore.case=TRUE))
  for(i in 1:length(n)){
    dataset[,n[i]] <- format(date_parser(dataset[,n[i]]), '%Y-%m-%d %H:%M:%S')
  }
  
  
  ## --------- MainDataTableInfo -------------- ##
  MainDataTableInfo <- data.frame(variable_name=colnames(dataset),
                                  units=c(ifelse(grepl('DATE|TRIP_END|TRIP_START',colnames(dataset), ignore.case=TRUE), 'yyyymmdd',
                                                 ifelse(grepl('MIN',colnames(dataset), ignore.case=TRUE), 'min',
                                                        ifelse(grepl('FATHOMS',colnames(dataset)), 'fathoms',
                                                               ifelse(grepl('HOURS|CHINOOK|CHUM|PROPORTION|SIZE', colnames(dataset), ignore.case=TRUE), 'numeric',
                                                                      ifelse(grepl('DOLLARS',colnames(dataset), ignore.case=TRUE), 'dollars',
                                                                             ifelse(grepl('POUNDS|LBS',colnames(dataset), ignore.case=TRUE), 'lbs',
                                                                                    ifelse(grepl('Lon|Lat|',colnames(dataset), ignore.case=TRUE), 'decimal degrees',
                                                                                           ifelse(grepl('PERCENT',colnames(dataset), ignore.case=TRUE), 'percent',
                                                                                                  ifelse(grepl('MT',colnames(dataset), ignore.case=TRUE), 'metric tons',
                                                                                                         ifelse(grepl('WEEK',colnames(dataset), ignore.case=TRUE), 'WK',
                                                                                                                ifelse(grepl('WEEK',colnames(dataset), ignore.case=TRUE), 'Y/N',NA
                                                                                                                )))))))))))),
                                  generalType=c(ifelse(grepl('DATE|MIN',colnames(dataset), ignore.case=TRUE), 'Time',
                                                       ifelse(grepl('IFQ',colnames(dataset), ignore.case=TRUE), 'Flag',
                                                              ifelse(grepl('ID',colnames(dataset), ignore.case=TRUE), 'Code',
                                                                     ifelse(grepl('Long|Lat',colnames(dataset), ignore.case=TRUE), 'Latitude',
                                                                            ifelse(grepl('TYPE|PROCESSOR|LOCATION|METHOD',colnames(dataset), ignore.case=TRUE), 'Code String',
                                                                                   ifelse(grepl('CHINOOK|CHUM|FATHOMS|DOLLARS|LBS|PROPORTION|VALUE|PERCENT|MT',colnames(dataset), ignore.case=TRUE), 'Other Numeric',
                                                                                          ifelse(grepl('HAUL|AREA|PERFORMANCE|PERMIT',colnames(dataset), ignore.case=TRUE), 'Code Numeric', NA)
                                                                                   ))))))),
                                  isXY=ifelse(grepl('HOURS|CHINOOK|CHUM|PROPORTION|SIZE', colnames(dataset), ignore.case=TRUE), 1,0),
                                  isID=ifelse(grepl('ID', colnames(dataset), ignore.case=TRUE), 1,0),
                                  variable_link=rep(NA, length(colnames(dataset))),
                                  isTime=ifelse(grepl('DATE|MIN', colnames(dataset), ignore.case=TRUE), 1,0),
                                  isCatch=ifelse(grepl('CATCH|POUNDS|LBS', colnames(dataset), ignore.case=TRUE), 1,0),
                                  isEffort=ifelse(grepl('DURATION',colnames(dataset), ignore.case=TRUE), 1,0),
                                  isCPUE=rep(0, length(colnames(dataset))),
                                  isLon=ifelse(grepl('LON',colnames(dataset), ignore.case=TRUE), 1,0),
                                  isLat=ifelse(grepl('LAT',colnames(dataset), ignore.case=TRUE), 1,0),
                                  isValue=ifelse(grepl('DOLLARS',colnames(dataset), ignore.case=TRUE), 1,0),
                                  isZoneArea=ifelse(grepl('AREA',colnames(dataset), ignore.case=TRUE), 1,0),
                                  isPort=ifelse(grepl('PORT', colnames(dataset), ignore.case=TRUE), 1,0),
                                  isPrice= rep(0, length(colnames(dataset)), ignore.case=TRUE),
                                  isTrip=ifelse(grepl('TRIP', colnames(dataset), ignore.case=TRUE), 1,0),
                                  isHaul=ifelse(grepl('HAUL', colnames(dataset), ignore.case=TRUE), 1,0),
                                  isOther=rep(0, length(colnames(dataset))),
                                  tableLink=rep(NA, length(colnames(dataset))))
  
  if(table_exists(paste0(project, 'MainDataTable', format(Sys.Date(), format="%Y%m%d")))==FALSE | over_write==TRUE){
    DBI::dbWriteTable(fishset_db, paste0(project, 'MainDataTable', format(Sys.Date(), format="%Y%m%d")),  dataset, overwrite=over_write)
    DBI::dbWriteTable(fishset_db, paste0(project, 'MainDataTableInfo', format(Sys.Date(), format="%Y%m%d")), MainDataTableInfo, overwrite=over_write)
    print('Table saved to database')
  } else {
    warning(paste('Table not saved.', paste0(project, 'MainDataTable', format(Sys.Date(), format="%Y%m%d")), 'exists in database, and overwrite is FALSE.'))
  }
  if(table_exists(paste0(project, 'MainDataTable'))==FALSE | over_write==TRUE){
    DBI::dbWriteTable(fishset_db, paste0(project, 'MainDataTable'),  dataset, overwrite=over_write)
    DBI::dbWriteTable(fishset_db, paste0(project, 'MainDataTableInfo'), MainDataTableInfo, overwrite=over_write)
  } else {
    warning(paste('Table not saved.', paste0(project, 'MainDataTable'), 'exists in database, and overwrite is FALSE.'))
  }
  DBI::dbDisconnect(fishset_db)
  
#log function
    load_maindata_function <- list()
    load_maindata_function$functionID <- 'load_maindata'
    load_maindata_function$args <- c(deparse(substitute(dat)), over_write, project, compare, y)
    load_maindata_function$kwargs <- list()
    load_maindata_function$output <- c('')
    log_call(load_maindata_function)
 
        assign(paste0(project, 'MainDataTable'), value = dataset, pos=1)
    cat('\n!!! -> Raw data saved as', paste0(project, 'MainDataTable', format(Sys.Date(), format="%Y%m%d"),'.'), 
        'Working data saved to the database as', paste0(project, 'MainDataTable.'), 'Table is also in the working environment. 
        To improve ease of reproducing work, please use this name in future analysis. <- !!!')
}

main_mod <- function(dat, x, new.unit=NULL, new.type=NULL, new.class=NULL) {
  #' Modify the MainDataTableInfo table
  #' @param dat MainDataTableInfo table. Table in fishset_db database should contain the string `MainDataTableInfo`.
  #' @param x Name of variable in the MainDataTableInfo table that is to be modified.
  #' @param new.unit Units. Current units include fathoms, decimal degrees, dollars, lbs, metric tons, min, numeric, percent, WK, 'Y/N', yyyymmdd.
  #' @param new.type General type. Current categories include Time, Flag, Code, Latitude, Code String, Other Numeric, Code Numeric
  #' @param new.class New specialized variable category. For example, isCPUE, isLon, isLat, isValue, isZoneArea, isPort.
  #' @details Modify the units `new.unit`, data format `new.type`, and specialized variable from old category `old.class` to new category `new.class` of the MainDataTableInfo table.
  #' Updated MainDataTableInfo file is saved to the fishset_db database. It is advisable to use the working table and not the raw table as the modifications will automatically be saved over the input table name.
  #' 
  #' @examples 
  #' \dontrun{  
  #' main_mod(dataset='MainDataTableInfo01012011', x='DISEMBARKED_PORT', 
  #'          new.unit='yyyymmdd', new.type='Other',  new.class='isPort') 
  #' }

 #Call in data sets
  suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite"))
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
  
  
  if(!is.null(new.unit)){
    dataset[dataset[['variable_name']]==x, 'units'] <- new.unit
  }
  if(!is.null(new.type)){
    dataset[dataset[['variable_name']]==x, 'generalType'] <- new.type
  }
  if(!is.null(new.class)){
    dataset[dataset[['variable_name']]==x, c(4,5,7:19)] <- 0
    dataset[dataset[['variable_name']]==x, new.class] <- 1
  }
  
  fishset_db <- DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite")
  DBI::dbWriteTable(fishset_db, dataset, dataset, overwrite=TRUE)
  DBI::dbDisconnect(fishset_db)
  print('Data saved to database')
 
#log function
  main_mod_function <- list()
  main_mod_function$functionID <- 'main_mod'
  main_mod_function$args <- c(deparse(substitute(dat)), deparse(substitute(x)), new.unit, new.type, old.class, new.class)
  main_mod_function$kwargs <- list()
  main_mod_function$output <- c('')
  log_call(main_mod_function)
  return(dataset)
}

load_port <- function(dat, port_name, over_write=TRUE, project=NULL, compare=FALSE, y=NULL){
  #' Save port data to fishset_db SQLite database
  #' @param dat Data frame to be saved to fishset_db SQLite database.
  #' @param port_name Column name or number containing port names. Names should match port names in main data set.
  #' @param over_write TRUE/FALSE Save over data table previously saved in fishset_db database?
  #' @param project Name of project. Parameter is used to generate meaningful table names in fishset_db database.
  #' @param compare TRUE/FALSE If TRUE, compare new data frame to previously saved data frame `y` in fishset_db before saving new data frame to the fishset_db database
  #' @param y Name of previously saved table in fishset_db database. y must be defined if `compare==TRUE`.
  #' @importFrom jsonlite toJSON
  #' @importFrom DBI dbConnect dbDisconnect dbWriteTable
  #' @details Runs a series of checks on the port data. If checks pass, runs the fishset_compare function and saves the new data frame to the fishset_db database.  
  #' The data is saved in the fishset_db database as the raw data and the working data. In both cases, the table name is the `project`, if defined, and the file type `PortTable`. 
  #' Date is also attached to the name for the raw data. 
  #' @examples 
  #' \dontrun{  
  #' load_port(dataset='PortTable', over_write=TRUE, project='', compare=TRUE, y='PortTable01012011') 
  #' }
  
  val <- 0
  x <- dat
  if(all(grepl('Lon', names(x), ignore.case=TRUE)==FALSE)==TRUE) { 
    warning('Latitude and Longitude must be specified')
    val <- 1
  }
  if(is.na(table(grepl('Lon', names(x), ignore.case=TRUE))[2])==FALSE & table(grepl('Lon', names(x), ignore.case=TRUE))[2]>1) { 
    warning('Multiple latitude or longitude columns. Only one allowed.') 
    val <- 1
  } 
  if(all(grepl('name|id|code|PORT', names(x), ignore.case = TRUE)==FALSE)==TRUE){
    warning('Port identification not found. Check that unique port ID (name, id, code) is included.')
    val <- 1
  } 
  
  if(!is.numeric(port_name)){
    colnames(x)[grep(port_name, colnames(x))] <- 'Port_Name'
  } else {
    colnames(x)[port_name] <- 'Port_Name'
  }
  
  colnames(x)[grep('LON', colnames(x), ignore.case=TRUE)] <- "Port_Long"
  colnames(x)[grep('LAT', colnames(x), ignore.case=TRUE)] <- "Port_Lat"  
  
  data_verification_call(x)
  
  if(compare==TRUE){
  fishset_compare(x,y,compare)
  }
  
  if(val==0){
  suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite"))
  if(table_exists(paste0(project, 'PortTable'))==FALSE | over_write==TRUE){
    DBI::dbWriteTable(fishset_db, paste0(project, 'PortTable', format(Sys.Date(), format="%Y%m%d")), x, overwrite=over_write)
    DBI::dbWriteTable(fishset_db, paste0(project, 'PortTable'), x, overwrite=over_write)
  } else {
    warning(paste('Table not saved.', paste0(project, 'PortTable'), 'exists in database, and overwrite is FALSE.')) 
  }
  DBI::dbDisconnect(fishset_db)
  print('Data saved to database')
  
  #write(layout.json.ed(trace, "load_port", '', x = deparse(substitute(x)), 
  #                     msg = paste("y:", deparse(substitute(y)), "compare:", compare, sep = "")),  
  #      paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""), append = T)

   load_port_function <- list()
  load_port_function$functionID <- 'load_port'
  load_port_function$args <- c(deparse(substitute(dat)), deparse(substitute(port_name)), over_write, project, compare, deparse(substitute(y)))
  load_port_function$kwargs <- list()
  load_port_function$output <- c('')
  log_call(load_port_function)
    }
}

load_aux <- function(dat, x, over_write=TRUE, project=NULL){
  #' Save auxiliary data
  #' @param dat Main data frame. Table in fishset_db database should contain the string `MainDataTable`.
  #' @param x Name of auxiliary data frame to be saved.
  #' @param over_write TRUE/FALSE Save over previously saved file or not.
  #' @param project Name of project for attaching to name of the table.
  #' @importFrom jsonlite toJSON
  #' @importFrom DBI dbConnect dbDisconnect dbWriteTable
  #' @details Auxiliary data is any additional data required beyond the main data and the port data. Auxiliary data can be anything you want to merge with the main data frame (ex. prices by date, vessel characteristics, or fishery season).
  #' The auxilliary data does not have to be at a haul or trip level but must contain a variable to connect the auxilliary data to the main data.
  #' The function checks that at least one column name of the auxiliary data matches a column name in the main data table.  Further checks are run using the data_verification_call function before saving the new data frame to the fishset_db database.
  #' The data is saved in the fishset_db database as the raw data and the working data. In both cases, the table name is the `project`, if defined, and the file name `x`. Date is also attached to the name for the raw data. 
  #' @examples 
  #' \dontrun{  
  #' load_aux(dataset='pcodMainDataTable', x=FisherySeason, over_write=TRUE, project='pcod') 
  #' }
  
  #Call in datasets
  suppressWarnings( fishset_db <- DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite"))
  if(is.character(dat)==TRUE){
    if(is.null(dat)==TRUE | table_exists(dat)==FALSE){
      print(DBI::dbListTables(fishset_db))
      stop(paste(dat, 'not defined or does not exist. Consider using one of the tables listed above that exist in the database.'))
    } else {
      old <- table_view(dat)
    }
  } else {
    old <- dat  
  }
  DBI::dbDisconnect(fishset_db)
  

  if(any(colnames(x)==colnames(old))==FALSE) {
     stop('No shared columns. Column names do not match between two data sets.')
  }
  
  data_verification_call(x)
  
  fishset_db <- suppressWarnings(DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite"))
  if(table_exists(paste0(project, x))==FALSE | over_write==TRUE){
  DBI::dbWriteTable(fishset_db, paste0(project, x, format(Sys.Date(), format="%Y%m%d")), x, overwrite=over_write)
  DBI::dbWriteTable(fishset_db, paste0(project, x), x, overwrite=over_write)
   print('Data saved to database')
  } else {
    warning(paste('Table not saved.', paste0(project, x), 'exists in database, and overwrite is FALSE.')) 
  }
  DBI::dbDisconnect(fishset_db)

   
 load_aux_function <- list()
  load_aux_function$functionID <- 'load_aux'
  load_aux_function$args <- c(deparse(substitute(dat)), deparse(substitute(x)), over_write, project)
  load_aux_function$kwargs <- list()
  load_aux_function$output <- c('')
  log_call(load_aux_function)
}

load_grid <- function(dat, x, over_write=TRUE, project=NULL){
  #' Save gridded data
  #' @param  dat Main data frame. Table in fishset_db database should contain the string `MainDataTable`.
  #' @param x Name of gridded data frame to be saved
  #' @param over_write TRUE/FALSE Save over previously saved file or not.
  #' @param project Name of project for attaching to name of the table.
  #' @details Grid data is an optional data frame that contains a variable that varies by the map grid (ex. sea surface temperature, wind speed). It can also vary by a second dimension (e.g., date/time). Both dimensions in the gridded data file need to be variables included in the main data frame. 
  #' The grid locations (zones) must define the columns and the optional second dimension defines the rows. The row variable must have the exact name as the variable 
  #' in the main data frame that it will be linked to. The data is saved in the fishset_db database as the raw data and the working data. In both cases, the table name is the `project`, if defined, and the file name `x`. Date is also attached to the name for the raw data. 
  #' @examples 
  #' \dontrun{  
  #' load_grid(dataset='pcodMainDataTable', x=SeaSurfaceTemp, over_write=TRUE, project='pcod') 
  #' }
  
  fishset_db <- suppressWarnings(DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite"))
  if(is.character(dat)==TRUE){
    if(is.null(dat)==TRUE | table_exists(dat)==FALSE){
      print(DBI::dbListTables(fishset_db))
      stop(paste(dat, 'not defined or does not exist. Consider using one of the tables listed above that exist in the database.'))
    } else {
      old <- table_view(dat)
    }
  } else {
    old <- dat  
  }
  DBI::dbDisconnect(fishset_db)
  
  if(any(colnames(x)==colnames(old))==FALSE) {
    stop('No shared columns. Column names do not match between two data sets.')
  }
  
  data_verification_call(x)
  
  fishset_db <- suppressWarnings(DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite"))
  if(table_exists(paste0(project, x))==FALSE | over_write==TRUE){
    DBI::dbWriteTable(fishset_db, paste0(project, x), x, overwrite=over_write)
  print('Data saved to database')
  } else {
    warning(paste('Table not saved.', paste0(project, x), 'exists in database, and overwrite is FALSE.')) 
  }
  DBI::dbDisconnect(fishset_db)

  load_gridded_function <- list()
  load_gridded_function$functionID <- 'load_gridded'
  load_gridded_function$args <- c(deparse(substitute(dataset)), over_write, project)
  load_gridded_function$kwargs <- list()
  load_gridded_function$output <- c()
  log_call(load_gridded_function)
}

dataindex_update <- function(dat, dataindex){
  #' Update dataindex file
  #' @param  dat Main data frame. Table in fishset_db database should contain the string `MainDataTable`.
  #' @param dataindex Name dataindex file should be saved as in database. Table name should exist in the fishset_db database. Name must be in quotes.
  #' @importFrom DBI dbConnect dbDisconnect dbWriteTable
  #' @details The MainDataTableInfo table is first created when the MainDataTable is loaded and saved to the fishset_db. However, this table may not match the variables in 
  #' the main data table after the FishSET variable creation functions have been run. It may be necessary to update the MainDataTableInfo table in the fishset_db database. 
  #' Running this function adds information on variables created using the FishSET data creation functions. 
  #' @examples
  #' \dontrun{  
  #' dataindex_update(dat='pcodMainDataTable', dataindex='pcodMainDataTableInfo') 
  #' }

  fishset_db <- suppressWarnings(DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite"))
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
  
MainDataTableInfo <- data.frame(variable_name=colnames(dataset),
                                units=c(ifelse(grepl('DATE|TRIP_END|TRIP_START',colnames(dataset), ignore.case=TRUE), 'yyyymmdd',
                                               ifelse(grepl('MIN',colnames(dataset), ignore.case=TRUE), 'min',
                                                      ifelse(grepl('FATHOMS',colnames(dataset)), 'fathoms',
                                                             ifelse(grepl('HOURS|CHINOOK|CHUM|PROPORTION|SIZE', colnames(dataset), ignore.case=TRUE), 'numeric',
                                                                    ifelse(grepl('DOLLARS',colnames(dataset), ignore.case=TRUE), 'dollars',
                                                                           ifelse(grepl('POUNDS|LBS',colnames(dataset), ignore.case=TRUE), 'lbs',
                                                                                  ifelse(grepl('Lon|Lat|',colnames(dataset), ignore.case=TRUE), 'decimal degrees',
                                                                                         ifelse(grepl('PERCENT',colnames(dataset), ignore.case=TRUE), 'percent',
                                                                                                ifelse(grepl('MT',colnames(dataset), ignore.case=TRUE), 'metric tons',
                                                                                                       ifelse(grepl('WEEK',colnames(dataset), ignore.case=TRUE), 'WK',
                                                                                                              ifelse(grepl('WEEK',colnames(dataset), ignore.case=TRUE), 'Y/N',NA
                                                                                                              )))))))))))),
                                generalType=c(ifelse(grepl('DATE|MIN',colnames(dataset), ignore.case=TRUE), 'Time',
                                                     ifelse(grepl('IFQ',colnames(dataset), ignore.case=TRUE), 'Flag',
                                                            ifelse(grepl('ID',colnames(dataset), ignore.case=TRUE), 'Code',
                                                                   ifelse(grepl('Long|Lat',colnames(dataset), ignore.case=TRUE), 'Latitude',
                                                                          ifelse(grepl('TYPE|PROCESSOR|LOCATION|METHOD',colnames(dataset), ignore.case=TRUE), 'Code String',
                                                                                 ifelse(grepl('CHINOOK|CHUM|FATHOMS|DOLLARS|LBS|PROPORTION|VALUE|PERCENT|MT',colnames(dataset), ignore.case=TRUE), 'Other Numeric',
                                                                                        ifelse(grepl('HAUL|AREA|PERFORMANCE|PERMIT',colnames(dataset), ignore.case=TRUE), 'Code Numeric', NA)
                                                                                 ))))))),
                                isXY=ifelse(grepl('HOURS|CHINOOK|CHUM|PROPORTION|SIZE', colnames(dataset), ignore.case=TRUE), 1,0),
                                isID=ifelse(grepl('ID', colnames(dataset), ignore.case=TRUE), 1,0),
                                variable_link=rep(NA, length(colnames(dataset))),
                                isTime=ifelse(grepl('DATE|MIN', colnames(dataset), ignore.case=TRUE), 1,0),
                                isCatch=ifelse(grepl('CATCH|POUNDS|LBS', colnames(dataset), ignore.case=TRUE), 1,0),
                                isEffort=ifelse(grepl('DURATION',colnames(dataset), ignore.case=TRUE), 1,0),
                                isCPUE=rep(0, length(colnames(dataset))),
                                isLon=ifelse(grepl('LON',colnames(dataset), ignore.case=TRUE), 1,0),
                                isLat=ifelse(grepl('LAT',colnames(dataset), ignore.case=TRUE), 1,0),
                                isValue=ifelse(grepl('DOLLARS',colnames(dataset), ignore.case=TRUE), 1,0),
                                isZoneArea=ifelse(grepl('AREA',colnames(dataset), ignore.case=TRUE), 1,0),
                                isPort=ifelse(grepl('PORT', colnames(dataset), ignore.case=TRUE), 1,0),
                                isPrice= rep(0, length(colnames(dataset)), ignore.case=TRUE),
                                isTrip=ifelse(grepl('TRIP', colnames(dataset), ignore.case=TRUE), 1,0),
                                isHaul=ifelse(grepl('HAUL', colnames(dataset), ignore.case=TRUE), 1,0),
                                isOther=rep(0, length(colnames(dataset))),
                                tableLink=rep(NA, length(colnames(dataset))))


DBI::dbWriteTable(fishset_db, dataindex, MainDataTableInfo, overwrite=TRUE)
DBI::dbDisconnect(fishset_db)

return(MainDataTableInfo)
}
