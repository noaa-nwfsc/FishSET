#'  Import data
#' @export read_dat
#' @export fishset_compare
#' @export load_maindata
#' @export load_port
#' @export load_aux
#' @export load_seasonal


read_dat <- function(x, data.type = c('csv', 'mat', 'json', 'shape', 'txt', 'spss', 'stata', 'R')) { 
#' @param x name and directory of dataframe to be read in
#' @param data.type csv, mat, json, shape
#' @importFrom sf read_sf
#' @importFrom R.matlab readMat
#' @importFrom jsonlite fromJSON
#' @importFrom foreign read.spss read.dta
#' @importFrom utils read.table
#' @details Uses the appropriate function to read in data based on data type.
  if(data.type == 'shape'){
    shapeName <- sf::read_sf(x)#'~/path/to/file.shp'
  } else if(data.type=='mat') {
    R.matlab::readMat(x) 
  } else if(data.type=='json') {
    jsonlite::fromJSON(x)
  } else if(data.type=='csv'){
    read.csv(x)
  } else if(data.type=='spss'){
    foreign::read.spss(x)
  } else if(data.type=='stata') {
    foreign::read.dta(x) 
  } else if(data.type=='R') {
    load(x) 
    } else {
    utils::read.table(x)
  }
}


fishset_compare <- function(x, y, compare=c(TRUE,FALSE)){
  #' Compare new dataset to previous version
  #' @param x name dataframe to be saved
  #' @param y name of previously saved dataframe
  #' @param compare TRUE/FALSE Compare new dataframe to previously saved dataframe before saving dataframe x to databases
  #' @importFrom DBI dbWriteTable dbDisconnect
  #' @details If compare is TRUE, colnames of the new and previously saved dataframe are compared for consistency. If false, no comparison is made and the new file is saved to the database.
  fishset_db <- DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite")
  if(compare==TRUE){
    if(is.null(y)==TRUE){
      print(DBI::dbListTables(fishset_db))
      stop(paste(y, 'not defined. Consider using one of the tables listed above that exist in database.'))
    } else {
      new <- toupper(colnames(x))
      old <- toupper(colnames(y))
      if(is.na(table(is.na(match(new, old)))[2])==TRUE){
        print('Column names match between previous and new datasets. New dataset uploaded to database')
        #DBI::dbWriteTable(fishset_db, paste(paste(deparse(substitute(x)),Sys.Date(), sep=''), deparse(substitute(x))))
      } else {
        cat(length(table(is.na(match(new,old)))[2]),'/',length(match(new,old)), ' mismatches', sep='')
        cat(noquote(paste(deparse(substitute(x)),'[',as.character(which(is.na(match(new,old))==TRUE)),']', sep='')),':', new[which(is.na(match(new,old))==TRUE)])
        cat(noquote(paste(deparse(substitute(y)),'[',as.character(which(is.na(match(new,old))==TRUE)),']', sep='')),':', old[which(is.na(match(new,old))==TRUE)])
        stop('Column names did not match. Dataset will not be uploaded to database')
        
      }
    }
  } else {
    cat('')
    #DBI::dbWriteTable(fishset_db, paste(deparse(substitute(x)), Sys.Date(), sep=''), deparse(substitute(x)))
  }
  DBI::dbDisconnect(fishset_db)
}


load_maindata <- function(x, over_write=TRUE, project=NULL,  compare=FALSE, y=NULL){
  #' Load data into sql database
  #' @param x name dataframe to be saved
  #' @param over_write TRUE/FALSE Save over previously saved file or not
  #' @param y name of previously saved dataframe. y must be defined if compare==TRUE
  #' @param project name of project
  #' @param compare TRUE/FALSE Compare new dataframe to previously saved dataframe before saving dataframe x to databases
  #' @importFrom jsonlite toJSON
  #' @details Runs the fishset.compare function. Then uses the new dataframe x to generate the the information table that contains information for each variable on units, data format, and specialied variable.
  #
  fishset_compare(x,y,compare)
  ##-----------MainDataTable--------------------##
  data_verification(x)
  # Check to see if lat/long or fish area is in dataset
  indx <- grepl("lat|lon|area", colnames(x), ignore.case = TRUE)
  if (length(x[indx]) > 0) {
    cat("Pass: Latitude and longitude or fishing area included in the dataset.")
  } else {
    stop("Dataset must contain either latitude and longitude or fishing area designation.")
  }
  
  ## --------- MainDataTableInfo -------------- ##
  MainDataTableInfo <- data.frame(variable_name=colnames(x),
                                  units=c(ifelse(grepl('DATE|TRIP_END|TRIP_START',colnames(x)), 'yyyymmdd',
                                                 ifelse(grepl('MIN',colnames(x)), 'min',
                                                        ifelse(grepl('FATHOMS',colnames(x)), 'fathoms',
                                                               ifelse(grepl('HOURS|CHINOOK|CHUM|PROPORTION|SIZE', colnames(x)), 'numeric',
                                                                      ifelse(grepl('DOLLARS',colnames(x)), 'dollars',
                                                                             ifelse(grepl('POUNDS|LBS',colnames(x)), 'lbs',
                                                                                    ifelse(grepl('Lon|Lat|LON|LAT',colnames(x)), 'decimal degrees',
                                                                                           ifelse(grepl('PERCENT',colnames(x)), 'percent',
                                                                                                  ifelse(grepl('MT',colnames(x)), 'metric tons',
                                                                                                         ifelse(grepl('WEEK',colnames(x)), 'WK',
                                                                                                                ifelse(grepl('WEEK',colnames(x)), 'Y/N',NA
                                                                                                                )))))))))))),
                                  generalType=c(ifelse(grepl('DATE|MIN',colnames(x)), 'Time',
                                                       ifelse(grepl('IFQ',colnames(x)), 'Flag',
                                                              ifelse(grepl('ID',colnames(x)), 'Code',
                                                                     ifelse(grepl('Long|Lat',colnames(x)), 'Latitude',
                                                                            ifelse(grepl('TYPE|PROCESSOR|LOCATION|METHOD',colnames(x)), 'Code String',
                                                                                   ifelse(grepl('CHINOOK|CHUM|FATHOMS|DOLLARS|LBS|PROPORTION|VALUE|PERCENT|MT',colnames(x)), 'Other Numeric',
                                                                                          ifelse(grepl('HAUL|AREA|PERFORMANCE|PERMIT',colnames(x)), 'Code Numeric', NA)
                                                                                   ))))))),
                                  isXY=ifelse(grepl('HOURS|CHINOOK|CHUM|PROPORTION|SIZE', colnames(x)), 1,0),
                                  isID=ifelse(grepl('ID', colnames(x)), 1,0),
                                  variable_link=rep(NA, length(colnames(x))),
                                  isTime=ifelse(grepl('DATE|MIN', colnames(x)), 1,0),
                                  isCatch=ifelse(grepl('CATCH|POUNDS|LBS', colnames(x)), 1,0),
                                  isEffort=ifelse(grepl('DURATION',colnames(x)), 1,0),
                                  isCPUE=rep(0, length(colnames(x))),
                                  isLon=ifelse(grepl('LON',colnames(x)), 1,0),
                                  isLat=ifelse(grepl('LAT',colnames(x)), 1,0),
                                  isValue=ifelse(grepl('DOLLARS',colnames(x)), 1,0),
                                  isZoneArea=ifelse(grepl('AREA',colnames(x)), 1,0),
                                  isPort=ifelse(grepl('PORT', colnames(x)), 1,0),
                                  isPrice= rep(0, length(colnames(x))),
                                  isTrip=ifelse(grepl('TRIP', colnames(x)), 1,0),
                                  isHaul=ifelse(grepl('HAUL', colnames(x)), 1,0),
                                  isOther=rep(0, length(colnames(x))),
                                  tableLink=rep(NA, length(colnames(x))))
  
  table_info_verification(x, MainDataTableInfo)
  fishset_db <- DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite")
  DBI::dbWriteTable(fishset_db, paste(project, 'MainDataTable', sep=''), x, overwrite=over_write)
  DBI::dbWriteTable(fishset_db, paste(project, 'MainDataTableInfo', sep=''), MainDataTableInfo, overwrite=over_write)
  DBI::dbDisconnect(fishset_db)
  print('Data saved to database')
    #write(layout.json.ed(trace, "load_maindata", '', x = deparse(substitute(x)), 
    #                   msg = paste("y:", deparse(substitute(y)), "compare:", compare, sep = "")),  
     #   paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""), append = T)
  
  if(!exists('logbody')) { 
    logging_code()
  } 
    load_maindata_function <- list()
    load_maindata_function$functionID <- 'load_maindata'
    load_maindata_function$args <- c(deparse(substitute(x)), project, compare, deparse(substitute(y)))
    functionBodyout$function_calls[[length(functionBodyout$function_calls)+1]] <- (load_maindata_function)
    logbody$fishset_run <- list(infoBodyout, functionBodyout)
    write(jsonlite::toJSON(logbody, pretty = TRUE, auto_unbox = TRUE), paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""))
    assign("functionBodyout", value = functionBodyout, pos = 1)
}

main_mod <- function(dataset, x, over_write=TRUE, project=NULL, change.col=NULL, new.unit=NULL, new.type=NULL, new.class=NULL) {
  if(!is.null(new.unit)){
  dataset[dataset[['variable_name']]==x, 'units'] <- new.unit
  }
  if(!is.null(new.type)){
    dataset[dataset[['variable_name']]==x, 'generalType'] <- new.type
  }
  if(!is.null(new.class)){
    dataset[dataset[['variable_name']]==x, change.col] <- new.class
  }
  
  fishset_db <- DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite")
  DBI::dbWriteTable(fishset_db, paste(project, 'MainDataTableInfo', sep=''), dataset, overwrite=over_write)
  DBI::dbDisconnect(fishset_db)
  print('Data saved to database')
  #write(layout.json.ed(trace, "main_mod", deparse(substitute(dataset)), x = deparse(substitute(x)), 
  #                     msg = paste("change.col:", change.col, "new.unit:", new.unit, "new.type:", new.type, "new.class:", new.class, sep = "")),  
  #      paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""), append = T)

  if(!exist(logbody)) { 
    logging_code()
  } 
  main_mod_function <- list()
  main_mod_function$functionID <- 'main_mod'
  main_mod_function$args <- c(deparse(substitute(dataset)), deparse(substitute(x)), project, change.col, new.unit, new.type, new.class)
  main_mod_function$function_calls[[length(functionBodyout$function_calls)+1]] <- (main_mod_function)
  logbody$fishset_run <- list(infoBodyout, functionBodyout)
  write(jsonlite::toJSON(logbody, pretty = TRUE, auto_unbox = TRUE),paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""))
  assign("functionBodyout", value = functionBodyout, pos = 1)
  
  return(dataset)
}

load_port <- function(x, over_write=TRUE, project=NULL, compare=FALSE, y=NULL){
  #' Save port data
  #' @param x name dataframe to be saved
  #' @param over_write TRUE/FALSE Save over previously saved file or not
  #' @param project name of project for attaching to table
  #' @param y name of previously saved dataframe
  #' @param compare TRUE/FALSE Compare new dataframe to previously saved dataframe before saving dataframe x to databases
  #' @details Runs a series of checks on the port data. If checks pass, runs the fishset_compare function and save the new dataframe x to the database.
  #' 
  #
  
  if(all(grepl('Lon', names(x), ignore.case=TRUE)==FALSE)==TRUE) { 
    stop('Latitude and Longitude must be specified')
  }
  if(is.na(table(grepl('Lon', names(x), ignore.case=TRUE))[2])==FALSE & table(grepl('Lon', names(x), ignore.case=TRUE))[2]>1) { 
    stop('Multiple latitude or longitude columns. Only one allowed.') 
  } 
  if(all(grepl('name|id|code', names(x), ignore.case = TRUE)==FALSE)==TRUE){
    warning('Port identification not found. Check that unique port ID (name, id, code) is included.')
  } 
  
  data_verification(x)

  fishset_compare(x,y,compare)
  
  fishset_db <- DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite")
  DBI::dbWriteTable(fishset_db, paste(project, 'PortTable', sep=''), x, overwrite=over_write)
  DBI::dbDisconnect(fishset_db)
  print('Data saved to database')
  #write(layout.json.ed(trace, "load_port", '', x = deparse(substitute(x)), 
  #                     msg = paste("y:", deparse(substitute(y)), "compare:", compare, sep = "")),  
  #      paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""), append = T)

  if(!exists('logbody')) { 
    logging_code()
  } 
  
  load_port_function <- list()
  load_port_function$functionID <- 'load_port'
  load_port_function$args <- c(deparse(substitute(x)), project, compare, deparse(substitute(y)))
  functionBodyout$function_calls[[length(functionBodyout$function_calls)+1]] <- (load_port_function)
  logbody$fishset_run <- list(infoBodyout, functionBodyout)
  write(jsonlite::toJSON(logbody, pretty = TRUE, auto_unbox = TRUE), paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""))
  assign("functionBodyout", value = functionBodyout, pos = 1)
}

load_aux <- function(x, over_write=TRUE, project=NULL, compare=FALSE, y=NULL){
  #' Save auxilliary data
  #' @param x name dataframe to be saved
  #' @param over_write TRUE/FALSE Save over previously saved file or not
  #' @param y name of previously saved dataframe
  #' @param compare TRUE/FALSE Compare new dataframe to previously saved dataframe before saving dataframe x to databases
  #' @param project name of project for attaching to table
  #' @details Runs a series of checks on the auxilliary data. If checks pass, runs the fishset_compare function and save the new dataframe x to the database.
  #
  
   if(all(grepl('Lon', names(x), ignore.case=TRUE)==FALSE)==TRUE) { 
    stop('Latitude and Longitude must be specified')
  }
  if(is.na(table(grepl('Lon', names(x), ignore.case=TRUE))[2])==FALSE & table(grepl('Lon', names(x), ignore.case=TRUE))[2]>1) { 
    stop('Multiple latitude or longitude columns. Only one allowed.') 
  } 
  
  data_verification(x)
  
 fishset_compare(x,y,compare)
 
 fishset_db <- DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite")
 DBI::dbWriteTable(fishset_db, paste(project, x, sep=''), x, overwrite=over_write)
 DBI::dbDisconnect(fishset_db)
 print('Data saved to database')
  #write(layout.json.ed(trace, "load_aux", '', x = deparse(substitute(x)), 
  #                     msg = paste("y:", deparse(substitute(y)), "compare:", compare, sep = "")),  
  #      paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""), append = T)
 
 if(!exists('logbody')) { 
   logging_code()
 } 
 
 load_aux_function <- list()
  load_aux_function$functionID <- 'load_aux'
  load_aux_function$args <- c(deparse(substitute(x)), project, compare, deparse(substitute(y)))
  load_aux_function$function_calls[[length(functionBodyout$function_calls)+1]] <- (load_aux_function)
  logbody$fishset_run <- list(infoBodyout, functionBodyout)
  write(jsonlite::toJSON(logbody, pretty = TRUE, auto_unbox = TRUE), paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""))
  assign("functionBodyout", value = functionBodyout, pos = 1)
}


load_seasonal <- function(x, over_write=TRUE, project=NULL, compare=FALSE, y=NULL){
  #' Save auxilliary data
  #' @param x name dataframe to be saved
  #' @param over_write TRUE/FALSE Save over previously saved file or not
  #' @param y name of previously saved dataframe
  #' @param compare TRUE/FALSE Compare new dataframe to previously saved dataframe before saving dataframe x to databases
  #' @param project name of project for attaching to table
  #' @details Runs a series of checks on the port data. If checks pass, runs the fishset_compare function and save the new dataframe x to the database.
  #
  
  if(all(grepl('date', names(x), ignore.case=TRUE)==FALSE)==TRUE) { 
    stop('Date variable must be specified')
  } 
  
  data_verification(x)
  
  fishset_compare(x,y,compare)
  fishset_db <- DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite")
  DBI::dbWriteTable(fishset_db, paste(project, 'SesaonalData', sep=''), x, overwrite=over_write)
  DBI::dbDisconnect(fishset_db)
  print('Data saved to database')
  
  #write(layout.json.ed(trace, "load_seasonal", '', x = deparse(substitute(x)), 
  #                     msg = paste("y:", deparse(substitute(y)), "compare:", compare, sep = "")),  
  #      paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""), append = T)

  if(!exists('logbody')) { 
    logging_code()
  } 
  
  load_seasonal_function <- list()
  load_seasonal_function$functionID <- 'load_seasonal'
  load_seasonal_function$args <- c(deparse(substitute(x)), project, compare, deparse(substitute(y)))
  load_seasonal_function$function_calls[[length(functionBodyout$function_calls)+1]] <- (load_seasonal_function)
  logbody$fishset_run <- list(infoBodyout, functionBodyout)
  write(jsonlite::toJSON(logbody, pretty = TRUE, auto_unbox = TRUE), paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""))
  assign("functionBodyout", value = functionBodyout, pos = 1)
}
