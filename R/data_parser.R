#'  Import data
#' @export read.dat
#' @export fishset.compare
#' @export load_maindata
#' @export load_port


read.dat <- function(x, data.type = c('csv', 'mat', 'json', 'shape')) { 
#' @param x name and directory of dataframe to be read in
#' @param data.type csv, mat, json, shape
#' @importFrom sf read_sf
#' @importFrom R.matlab readMat
#' @importFrom rjson fromJSON
#' @details Uses the appropriate function to read in data based on data type.
  if(data.type == 'shape'){
    shapeName <- sf::read_sf(x)#'~/path/to/file.shp'
  } else if(data.type=='mat') {
    R.matlab::readMat(x) 
  } else if(data.type=='json') {
    rjson::fromJSON(x)
  } else {
    
  }
}


fishset.compare <- function(x, y, compare=c(TRUE,FALSE)){
  #' @param x name dataframe to be saved
  #' @param y name of previously saved dataframe
  #' @param compare TRUE/FALSE Compare new dataframe to previously saved dataframe before saving dataframe x to databases
  #' @details If compare is TRUE, colnames of the new and previously saved dataframe are compared for consistency. If false, no comparison is made and the new file is saved to the database.
  
  if(compare==TRUE){
    if(is.null(y)==TRUE){
      print(DBI::dbListTables(fishset_db))
      stop('y not defined. Consider using one of the tables listed above that exist in database.')
    } else {
      new <- toupper(colnames(x))
      old <- toupper(colnames(y))
      if(is.na(table(is.na(match(new, old)))[2])==TRUE){
        print('Column names match between previous and new datasets. New dataset uploaded to database')
        dbWriteTable(fishset_db, paste(paste(deparse(substitute(X)),Sys.Date(), sep=''), deparse(substitute(X))))
      } else {
        cat(length(table(is.na(match(new,old)))[2]),'/',length(match(new,old)), ' mismatches', sep='')
        cat(noquote(paste(deparse(substitute(x)),'[',as.character(which(is.na(match(new,old))==TRUE)),']', sep='')),':', new[which(is.na(match(new,old))==TRUE)])
        cat(noquote(paste(deparse(substitute(y)),'[',as.character(which(is.na(match(new,old))==TRUE)),']', sep='')),':', old[which(is.na(match(new,old))==TRUE)])
        print('Column names did not match. Dataset not uploaded to database')
      }
    }
  } else {
    dbWriteTable(fishset_db, paste(deparse(substitute(X)), Sys.Date(), sep=''), deparse(substitute(X)))
  }
}


load_maindata <- function(x,y,compare){
  #' @param x name dataframe to be saved
  #' @param y name of previously saved dataframe
  #' @param compare TRUE/FALSE Compare new dataframe to previously saved dataframe before saving dataframe x to databases
  #' @details Runs the fishset.compare function. Then uses the new dataframe x to generate the the information table that contains information for each variable on units, data format, and specialied variable.
  #
  fishset.compare(x,y,compare)
  ## --------- MainDataTableInfo -------------- ##
  MainDataTableInfo <- data.frame(variable_name=colnames(X),
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
  dbWriteTable(fishset_db,'MainDataTableInfo', MainDataTableInfo)

    write(layout.json.ed(trace, "load_maindata", '', x = deparse(substitute(x)), 
                       msg = paste("y:", deparse(substitute(y)), "compare:", compare, sep = "")),  
        paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""), append = T)
  
}

main_mod <- function(dataset, x, change.col=NULL, new.unit=NULL, new.type=NULL, new.class=NULL) {
  if(!is.null(new.unit)){
  dataset[dataset[['variable_name']]==x, 'units'] <- new.unit
  }
  if(!is.null(new.type)){
    dataset[dataset[['variable_name']]==x, 'generalType'] <- new.type
  }
  if(!is.null(new.class)){
    dataset[dataset[['variable_name']]==x, change.col] <- new.class
  }
  
  write(layout.json.ed(trace, "main_mod", deparse(substitute(dataset)), x = deparse(substitute(x)), 
                       msg = paste("change.col:", change.col, "new.unit:", new.unit, "new.type:", new.type, "new.class:", new.class, sep = "")),  
        paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""), append = T)
  
  return(dataset)
}

load_port <- function(x, y, compare){
  #' Save port data
  #' @param x name dataframe to be saved
  #' @param y name of previously saved dataframe
  #' @param compare TRUE/FALSE Compare new dataframe to previously saved dataframe before saving dataframe x to databases
  #' @details Runs a series of checks on the port data. If checks pass, runs the fishset.compare function and save the new dataframe x to the database.
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
  fishset.compare(x,y,compare)
  
  write(layout.json.ed(trace, "load_port", '', x = deparse(substitute(x)), 
                       msg = paste("y:", deparse(substitute(y)), "compare:", compare, sep = "")),  
        paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""), append = T)
  
}

