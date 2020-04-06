degree <- function(dat, lat=NULL, lon=NULL, latsign=FALSE, lonsign=FALSE, replace=TRUE){
  #' Convert lat/long coordinates to decimal degrees
  #' @param dat Data table containing latitude and longitude data
  #' @param lat Name of vector containing latitude data
  #' @param lon Name of vector containg longitude data
  #' @param latsign If TRUE, transforms sign from positive to minus or minus to positive
  #' @param lonsign If TRUE, transforms sign from positive to minus or minus to positive
  #' @param replace Defaults to TRUE. Set to FALSE if only checking for errors
  #' @export degree
  #' @importFrom OSMscale degree
  #' @importFrom stringr str_replace
  #' @details Uses the degree function to convert lat long coordinates to decimal degrees.
  #' @return The original dataframe with the latitudes and longitudes converted to decimal degrees.
  #' Changing the sign, transforms all values in the variable. 
  #' @examples 
  #' \dontrun{
  #' dat <- degree(MainDataTable, 'LatLon_START_LAT', 'LatLon_START_LON', 
  #'            latsign=FALSE, lonsign=FALSE, replace=TRUE)
  #' }
  #' 
  
  #Call in datasets
  out <- data_pull(dat)
  dat <- out$dat
  dataset <- out$dataset  
  
  tmp <- tempfile()
  
    if(any(apply(dataset[,grep('lat|lon', names(dataset), ignore.case=TRUE)], 2, function(x) !is.numeric(x))==TRUE)==TRUE){
      cat('At least one latitude or longitude variable is not in decimal degrees. Select a lat and lon variable below and then select a conversion option on the the left to convert to decimal degrees.', file=tmp)
    } else if(any(apply(dataset[,grep('lat|lon', names(dataset), ignore.case=TRUE)], 2, function(x) nchar(trunc(abs(x))))>3)==TRUE){
      cat('At least one latitude or longitude variable is not in decimal degrees. Select a lat and lon variable below and then select a conversion option on the the left to convert to decimal degrees.', file=tmp)
    } else {
      cat('Latitude and longitude variables in decimal degrees. No further action required.', file=tmp)
    }
  
  print(suppressWarnings(readLines(tmp)))

  degree_function <- list()
  degree_function$functionID <- 'degree'
  degree_function$args <-  c(dat, lat, lon, latsign, lonsign)
  degree_function$output <-  c(dat)
  degree_function$msg <- suppressWarnings(readLines(tmp))
  log_call(degree_function)

  if(replace==TRUE){
  if(!is.null(lat)){
    if(!is.numeric(dat[[lat]])) {
      temp = gsub("\u00b0|'|\"", "", dat[[lat]])
      temp[lengths(gregexpr(" ", temp))==1&!is.na(temp)] <- paste(temp[lengths(gregexpr(" ", temp))==1&!is.na(temp)], '00')
      dat[[lat]] <- as.numeric(sapply(strsplit(temp, "\\s+"), '[', 1))+ as.numeric(sapply(strsplit(temp, "\\s+"), '[', 2))/60+as.numeric(sapply(strsplit(temp, "\\s+"), '[', 3))/360
      
    }  else if(any(nchar(trunc(abs(dat[[lat]])))>2, na.rm=T)){
      nm <- !is.na(dat[[lat]])&dat[[lat]] < 0
      dat[[lat]] <- abs(dat[[lat]])
      i <- nchar(abs(dat[[lat]]))<=4&!is.na(dat[[lat]])
      dat[[lat]][i] <- paste0(dat[[lat]][i], '00')
      dat[[lat]] <-  format(as.numeric(stringr::str_pad(abs(as.numeric(dat[[lat]])), 6, pad = "0")), scientific=FALSE)
      dat[[lat]] <- as.numeric(substr(dat[[lat]], start = 1, stop = 2)) + as.numeric(substr(dat[[lat]], start = 3, stop = 4))/60 + as.numeric(substr(dat[[lat]], start = 5, stop = 6))/3600  
      dat[[lat]][nm] <- dat[[lat]][nm]*-1
    } else {
      dat <- dat
    }
  }
  if(!is.null(lon)){
    if(!is.numeric(dat[[lon]])) {
      temp = gsub("\u00b0|'|\"", "", dat[, lon])
      temp[lengths(gregexpr(" ", temp))==1&!is.na(temp)] <- paste(temp[lengths(gregexpr(" ", temp))==1&!is.na(temp)], '00')
      dat[[lon]] <- as.numeric(sapply(strsplit(temp, "\\s+"), '[', 1))+ as.numeric(sapply(strsplit(temp, "\\s+"), '[', 2))/60+as.numeric(sapply(strsplit(temp, "\\s+"), '[', 3))/360
      
    } else if(any(nchar(trunc(abs(as.numeric(dat[[lon]]))))>3, na.rm=T)){
      nm <- !is.na(dat[[lon]])&as.numeric(dat[[lon]]) < 0
      dat[[lon]] <- abs(dat[[lon]])
      i <- nchar(dat[[lon]])<=5&!is.na(dat[[lon]])
      dat[[lon]][i] <- paste0(dat[[lon]][i], '00')
      dat[[lon]] <-  format(as.numeric(stringr::str_pad(as.numeric(dat[[lon]]), 7, pad = "0")), scientific=FALSE)
      dat[[lon]] <- as.numeric(substr(dat[[lon]], start = 1, stop = 3)) + as.numeric(substr(dat[[lon]], start = 4, stop = 5))/60 + as.numeric(substr(dat[[lon]], start = 6, stop = 7))/3600  
      dat[[lon]][nm] <- dat[[lon]][nm]*-1
    } else {
      dat <- dat
    }
  }
  
  if(latsign==TRUE&!is.null(lat)){
    dat[[lat]] <- -1*dat[[lat]]
  } else {
    dat <- dat
  }
  if(lonsign==TRUE&!is.null(lon)){
    dat[[lon]] <- -1*dat[[lon]]
  } else {
    dat <- dat
  }
  return(dat) 
    }
  rm(tmp)
}
