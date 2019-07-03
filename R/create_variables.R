# Create variables or matrix.

##--- CPUE ----##
#' Create catch per unit effort 
cpue <- function(dat, xWeight, xTime, name='cpue') {
  #' @param dat Main data frame over which to apply function. Table in fishset_db database should contain the string `MainDataTable`.
  #' @param xWeight Weight variable
  #' @param xTime Time variable. Must be weeks, days, hours, or minutes.
  #' @param name Name of created variable. Used in the logging function to reproduce work flow. Defaults to name of the function if not defined.
  #' @export cpue 
  #' @details Creates the catch per unit effort variable. Catch variable must be in weight (lbs, mts). Effort variable should be a measurement of duration in time.
  #' @examples 
  #' \dontrun{
  #' MainDataTable$cpue <- cpue(MainDataTable, 'OFFICIAL_TOTAL_CATCH_MT', 'DURATION_IN_MIN') 
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
  
  
 
         if(!is.numeric(dataset[[xTime]])|!is.numeric(dataset[[xWeight]])){
          stop('Data must be numeric. CPUE not calculated')
        }
  # Check that Weight variable is indeed a weight variable
       if (!grepl("Duration", xTime, ignore.case = TRUE)) {
        warning("xTime should be a measurement of time. CPUE calculated.")
      } 
      if (!grepl("LB|Pounds|MT", xWeight, ignore.case = TRUE)){
        warning("xWeight must a measurement of mass. CPUE calculated.")
      }
  
  cpue <- dataset[[xWeight]]/dataset[[xTime]]
 
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
  create_var_cpue_function <- list()
  create_var_cpue_function$functionID <- 'cpue'
  create_var_cpue_function$args <- c(deparse(substitute(dat)), xWeight, xTime)
  create_var_cpue_function$kwargs <- list()
  create_var_cpue_function$output <- paste0(deparse(substitute(dat)),'$',name)
  functionBodyout$function_calls[[length(functionBodyout$function_calls)+1]] <- (create_var_cpue_function)
  logbody$fishset_run <- list(infoBodyout, functionBodyout)
  write(jsonlite::toJSON(logbody, pretty = TRUE, auto_unbox = TRUE),paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""))
  assign("functionBodyout", value = functionBodyout, pos = 1)
  
  return(cpue)
}


##---- Dummy  Variables ----##
#' Create new dummy variable
dummy_var <- function(dat, DumFill = 'TRUE', name='dummy_var') {
  #' @param dat Main data frame over which to apply function. Table in fishset_db database should contain the string `MainDataTable`.
  #' @param DumFill Fill the dummy variable with TRUE or FALSE
  #' @param name Name of created dummy variable. Used in the logging function to reproduce work flow. Defaults to name of the function if not defined.
  #' @export dummy_var
  #' @details Creates a dummy variable of either FALSE or TRUE with length of the number of rows of the data set. 
  #' @examples 
  #' \dontrun{
  #' MainDataTable$dummyvar <- dummy_var(MainDataTable, DumFill=TRUE)
  #' }

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
  
  dummyvar <- as.vector(rep(DumFill, nrow(dataset)))

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
  create_var_dummy_var_function <- list()
  create_var_dummy_var_function$functionID <- 'dummy_var'
  create_var_dummy_var_function$args <- c(deparse(substitute(dat)), DumFill)
  create_var_dummy_var_function$kwargs <- list()
  create_var_dummy_var_function$output <- paste0(deparse(substitute(dat)),'$',name)
  functionBodyout$function_calls[[length(functionBodyout$function_calls)+1]] <- (create_var_dummy_var_function)
  logbody$fishset_run <- list(infoBodyout, functionBodyout)
  write(jsonlite::toJSON(logbody, pretty = TRUE, auto_unbox = TRUE),paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""))
  assign("functionBodyout", value = functionBodyout, pos = 1)
  
  return(dummyvar)
}


#' Create dummy matrix from a coded ID variable
dummy_matrix <- function(dat, x) {
  #' @param dat Main data frame over which to apply function. Table in fishset_db database should contain the string `MainDataTable`.
  #' @param x Variable in dataset used to generate dummy matrix
  #' @export dummy_matrix
  #' @details Creates a dummy matrix of TRUE/FALSE with dimensions \emph{(number of observations in dataset) x (number of factors in x)} where each column is a unique factor level. Values are TRUE if the value in the column matches the column factor level and FALSE otherwise.
  #' @examples 
  #' \dontrun{
  #' PortMatrix <- dummy_matrix(MainDataTable, 'PORT_CODE')
  #'}
   
  
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
  
    # create the matrix
  factor.levels <- levels(as.factor(dataset[[x]]))
  int <- data.frame(matrix(rep(dataset[[x]], length(factor.levels)), ncol = length(factor.levels)))
  colnames(int) = factor.levels
  # change matrix to TRUE/FALSE
  int <- data.frame(lapply(1:length(factor.levels), function(x) ifelse(int[, x] == colnames(int)[x], TRUE, FALSE)))
  colnames(int) = paste(x, "_", levels(as.factor(dataset[[x]])))
  
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
  create_var_dummy_matrix_function <- list()
  create_var_dummy_matrix_function$functionID <- 'dummy_matrix'
  create_var_dummy_matrix_function$args <- c(deparse(substitute(dat)), x)
  create_var_dummy_matrix_function$kwargs <- list()
  create_var_dummy_matrix_function$output <- c('')
  functionBodyout$function_calls[[length(functionBodyout$function_calls)+1]] <- (create_var_dummy_matrix_function)
  logbody$fishset_run <- list(infoBodyout, functionBodyout)
  write(jsonlite::toJSON(logbody, pretty = TRUE, auto_unbox = TRUE),paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""))
  assign("functionBodyout", value = functionBodyout, pos = 1)
  
  return(int)
}


##---- Coded variables ----##
#' Create quantile variable
set_quants <- function(dat, x, quant.cat = c(0.2, 0.25, 0.4), name='set_quants') {
  #' @param dat Main data frame over which to apply function. Table in fishset_db database should contain the string `MainDataTable`.
  #' @param x Variable to transform into quantiles
  #' @param quant.cat Quantile categories. Includes 0.2, 0.25, and 0.4.
  #' @param name Name of created vector. Used in the logging function to reproduce work flow. Defaults to name of the function if not defined.
  #' @export set_quants
  #' @details Creates a coded variable of 5-6 levels based on the quantiles of x. 
  #' Quantile options are: 
  #' \itemize{
  #'   \item{.2:  (0\%, 20\%, 40\%, 60\%, 80\%, 100\%)}
  #'   \item{.25: (0\%, 25\%, 50\%, 75\%, 100\%)}
  #'   \item{.4:  (0\%, 10\%, 50\%, 90\%, 100\%)}
  #'   }
  #' @examples 
  #' \dontrun{
  #' MainDataTable <- set_quants(MainDataTable, 'HAUL', quant.cat=.2)
  #' }
  #
  
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
  
  
if (quant.cat == 0.2) {
    prob.def = c(0, 0.2, 0.4, 0.6, 0.8, 1)
  } else if (quant.cat == 0.25) {
    prob.def = c(0, 0.25, 0.5, 0.75, 1)
  } else if (quant.cat == 0.4) {
    prob.def = c(0, 0.1, 0.5, 0.9, 1)
  }
  var.name <- paste("TRIP_OTC_MT", "quantile", sep = ".")
  var.name <- as.integer(cut(dataset[[x]], quantile(dataset[[x]], probs = prob.def), 
                             include.lowest = TRUE))
  
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
  create_var_set_quants_function <- list()
  create_var_set_quants_function$functionID <- 'set_quants'
  create_var_set_quants_function$args <- c(deparse(substitute(dat)), x, quant.cat)
  create_var_set_quants_function$kwargs <- list()
  create_var_set_quants_function$output <- paste0(deparse(substitute(dat)),'$',name)
  functionBodyout$function_calls[[length(functionBodyout$function_calls)+1]] <- (create_var_set_quants_function)
  logbody$fishset_run <- list(infoBodyout, functionBodyout)
  write(jsonlite::toJSON(logbody, pretty = TRUE, auto_unbox = TRUE),paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""))
  assign("functionBodyout", value = functionBodyout, pos = 1)
  
  return(var.name)
}


##---- Numeric  Variables ----##
#' Create numeric variables using arithmetic expression
create_var_num <- function(dat, x, y, method, name='create_var_num') {
  #' @param dat Main data frame over which to apply function. Table in fishset_db database should contain the string `MainDataTable`.
  #' @param x variable  Variable will be the numerator if `method` is division. 
  #' @param y variable  Variable will be the denominator if `method` is division.
  #' @param method Arithmetic expression. Options include: addition, subtraction, multiplication, and division.
  #' @param name Name of created vector. Used in the logging function to reproduce work flow. Defaults to name of the function if not defined.
  #' @export create_var_num
  #' @details Creates a new numeric variable based on defined arithmetic expression `method`. New variable is added to the data set.
  #' @examples 
  #' \dontrun{
  #' MainDataTable$tot_salmon <- create_var_num(MainDataTable, 'HAUL_CHINOOK', 'HAUL_CHUM',
  #'                                             'sum',' TimeChange')
  #' }
  
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
  
  
  if (is.numeric(dataset[[x]]) == FALSE | is.numeric(dataset[[y]]) == FALSE) {
    stop("Variables must be numeric")
   }
  
  if (grepl("add|sum", method, ignore.case = TRUE)) {
    name <- dataset[[x]] + dataset[[y]]
  } else if (grepl("sub", method, ignore.case = TRUE)) {
    name <- dataset[[x]] - dataset[[y]]
  } else if (grepl("mult", method, ignore.case = TRUE)) {
    name <- dataset[[x]] * dataset[[y]]
  } else if (grepl("div", method, ignore.case = TRUE)) {
    name <- dataset[[x]]/dataset[[y]]
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
  create_var_num_function <- list()
   create_var_num_function$functionID <- 'create_var_num'
   create_var_num_function$args <- c(deparse(substitute(dat)), x, y, method)
   create_var_num_function$kwargs <- list()
   create_var_num_function$output <- paste0(deparse(substitute(dat)),'$',name)
   functionBodyout$function_calls[[length(functionBodyout$function_calls)+1]] <- (create_var_num_function)
   logbody$fishset_run <- list(infoBodyout, functionBodyout)
   assign("functionBodyout", value = functionBodyout, pos = 1)
   
   write(jsonlite::toJSON(logbody, pretty = TRUE, auto_unbox = TRUE), paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""))
   
  return(name)
}


##---- Spatial  Variables ----##
#' Calculate latitude and longitude of haul midpoint 
create_mid_haul <- function(dat, start=c('lon', 'lat'), end=c('lon','lat')) {
#' @param dat Main data frame over which to apply function. Table in fishset_db database should contain the string `MainDataTable`.
#' @param start Starting location of haul. Must be specificied as vector containing longitudinal data and vector containing latitudinal data separated by comma.
#' @param end Ending location of haul. Must be specificied as vector containing longitudinal data and vector containing latitudinal data separated by comma.
#' @details Returns midpoint of each haul. Each row of data must be a unique haul. Requires a start and end point for each observations.
#' @importFrom geosphere distGeo midPoint
#' @export
#' @examples 
#' \dontrun{
#' MainDataTable$haulMidPoint <- create_mid_haul(MainDataTable, start = c("LonLat_START_LON", "LonLat_START_LAT"), end = c("LonLat_END_LON", "LonLat_END_LAT")) 
#' }
#
 if(FishSET:::is_empty(start)||FishSET:::is_empty(end)) {
   stop('Starting and end locations must both be specified')
 }
 
  if(dim(dat[,start])[1] != dim(dat[,end])[1]){
    stop('Starting and ending locations are of different lengths')
  }

  distBetween <- geosphere::midPoint(dat[,start], dat[,end])
}

#trip centroid


#' Histogram of latitude and longitude by grouping variable
spatial_hist <- function(dat, group){
#' @param dat Main data frame over which to apply function. Table in fishset_db database should contain the string `MainDataTable`.
#' @param group Vector containing grouping categories
#' @import ggplot2
#' @return histogram of latitude and longitude by grouping variable
#' @export
#' @examples 
#' \dontrun{
#' spatial_summary(MainDataTable, 'GEAR_TYPE')
#' }

  library(ggplot2)
  
  dat <- dat[, c(grep(group, names(dat)), grep('lon|lat', names(dat), ignore.case=TRUE))]
  melt.dat <- melt(dat)
  ggplot(melt.dat,aes(value,group=group,fill=group))+
         geom_histogram(position="identity",alpha=0.5,binwidth=0.25)+ 
          facet_wrap(~ variable, scales = "free")+ scale_color_grey() + scale_fill_grey() +theme_classic()
}

#hot-spot analysis ( getis ord and morans I)

#'spatial summary statistics
spatial_summary <- function(dat, stat.var=c('length','no_unique_obs','perc_total','mean','median','min','max','sum'), variable,
                gridfile, lon.grid, lat.grid, lon.dat, lat.dat, cat){
  #' @param dat Main data frame containing data on hauls or trips. Table in fishset_db database should contain the string `MainDataTable`.
  #' @param stat.var Options are length, no_unique_obs, perc_total, mean, median, min, max, and sum
  #' @param gridfile Spatial data. Shape, json, and csv formats are supported.
  #' @param variable Vector to summarize over date and zone
  #' @param lon.dat Column containing longitude data in main data frame.
  #' @param lat.dat Column containing latitude data in main data frame.
  #' @param lon.grid Column containing longitude data in gridfile.
  #' @param lat.grid lColumn containing latitude data in gridfile.
  #' @param cat  Column in gridfile that identifies the individual areas or zones. If gridfile is class sf, `cat` should be name of list containing information on zones. 
  #' @export
  #' @description Returns plot of selected variable by against date and zone.
  #' @details
  #' \tabular{rlll}{
  #' length: \tab Number of observations \cr
  #' no_unique_obs: \tab Number of unique observations \cr 
  #' perc_total: \tab Percent of total observations \cr 
  #' mean: \tab Mean \cr
  #' median: \tab  Median \cr 
  #' min: \tab  Minimum\cr
  #' max: \tab Maximum \cr
  #' sum: \tab Sum \cr
  #' }
  dat <- assignment_column(dat=dat,  gridfile=gridfile, hull.polygon = TRUE, lon.grid, lat.grid, 
                    lon.dat, lat.dat, cat, closest.pt = TRUE, epsg=NULL)
  date.var <- grep('date', names(dat), ignore.case=TRUE)[1]
  var <- grep(variable, names(dat), ignore.case=TRUE)[1]
  
   if(stat.var=='mean'){ 
     lab <- paste('mean', names(dat)[var])
   } else if(stat.var=='median'){
     lab <- paste('median', names(dat)[var])
   } else if(stat.var=='min'){
     lab <- paste('min', names(dat)[var])
   } else if(stat.var=='max'){
     lab <- paste('max', names(dat)[var])
   } else if(stat.var=='sum'){
     lab <- paste('sum', names(dat)[var])
   } else if(stat.var=='length'){
     lab <- paste('No. observations', names(dat)[var])
   }
  par(mfrow=c(1,2))
  
  if(stat.var=='no_unique_obs'){
    plot(aggregate(dat[[variable]], by=list(dat[,date.var]), function(x) length(unique(x))), type = "l", lty = 1, ylab=paste('No. unique obs', names(dat)[var], 'per day'), xlab='Date')
    lines(aggregate(dat[[variable]], by=list(dat[,date.var]), function(x) length(unique(x))))
    plot(aggregate(dat[[variable]], by=list(as.factor(dat$ZoneID)), function(x) length(unique(x))), type = "l", lty = 1, ylab=paste('No. unique obs', names(dat)[var]), xlab='Zone')
    lines(aggregate(dat[[variable]], by=list(as.factor(dat$ZoneID)), function(x) length(unique(x))))
  } else if(stat.var=='perc_total'){
    plot(aggregate(dat[[variable]], by=list(dat[,date.var]), function(x) length((x))/length(dat[[variable]])*100), type = "l", lty = 1, ylab=paste('Percent total', names(dat)[var], 'per day'), xlab='Date')
    lines(aggregate(dat[[variable]], by=list(dat[,date.var]), function(x) length((x))/length(dat[[variable]])*100))
    plot(aggregate(dat[[variable]], by=list(as.factor(dat$ZoneID)), function(x) length((x))/length(dat[[variable]])*100), type = "l", lty = 1, ylab=paste('Percent total', names(dat)[var]), xlab='Zone')
    lines(aggregate(dat[[variable]], by=list(as.factor(dat$ZoneID)), function(x) length((x))/length(dat[[variable]])*100))
  } else {
    plot(aggregate(dat[[variable]], by=list(dat[,date.var]), stat.var), type = "l", lty = 1, ylab=paste(lab, 'per day'), xlab='Date')
    lines(aggregate(dat[[variable]], by=list(dat[,date.var]), stat.var))
    plot(aggregate(dat[[variable]], by=list(as.factor(dat$ZoneID)), stat.var), type='l', ylab=lab, xlab='Zone')
    lines(aggregate(dat[[variable]], by=list(as.factor(dat$ZoneID)), stat.var))
  }
}

#' Distance between two points
create_dist_between <- function(dat, start, end, units=c('miles','meters','km','midpoint')){
 #' @param dat Main data frame over which to apply function. Table in fishset_db database should contain the string `MainDataTable`.
 #' @param start  Starting location. Should be a port, lat/long location, or the centroid of zonal assignment. If port is desired, start should be the vector name containing the port name. Latitude and longitude for the port are extracted from the port table. If a lat, long location is desired then start should be specified as c(name of lon vector, name of lat vector). The order must be lon, lat. If the center point of the fishing zone or area is to be used then start should be 'centroid'.
 #' @param end  Ending location. Should be a port, lat/long location, or the centroid of the fishing zone or area. If port is desired, end should be the vector name containing the port name. Latitude and longitude for the port are extracted from the port table. If a lat, long location is desired then end should be specified as c(name of lat vector, name of lon vector). If the center point of the fishing zone or area is to be used then end should be 'centroid'.
 #' @param units  Unit of measurement for calculated distance between start and ending points. Can be in miles, meters, kilometers, or midpoint location
 #' @export
 #' @importFrom geosphere distGeo midPoint
 #' @details   Creates a vector of distance between two points. The start and end points must be different vectors. If the start or ending points are from a port or the center of a fishing zone or area, then a prompt will appear asking for further parameters to be specified. If the starting or ending points are a port, then latitude and longitude are extracted from the port table stored in the fishset_db database.  In this case, PortTable must be specified.  If the starting or ending points are the center of the fishing zone or area, then the assignment_column function will be called to assign each observation to a zone. The find_centroid function will then be called to determine the centroid of each zone. Distance measurements will be between these centroids. 
 #' @return
  #' \tabular{rlll}{
  #' portTable: \tab Port table from fishset_db database Required if start or end is a port vector \cr
  #' gridfile: \tab patial data set Can be shape file data frame or list Required if start or end is centroid \cr 
  #' lon.dat: \tab Longitude of points from dataset Required if start or end is centroid \cr 
  #' lat.dat: \tab Latitude of points from dataset Required if start or end is centroid \cr
  #' lon.grid: \tab Longitude of points from gridfile Required if start or end is centroid \cr 
  #' lat.grid: \tab Longitude of points from gridfile Required if start or end is centroid \cr
  #' cat: \tab Variable defining the individual areas or zones Required if start or end is centroid \cr 
  #' }
#' @examples 
#' \dontrun{
#' MainDataTable$DistCentPort <- create_dist_between(MainDataTable,'centroid','EMBARKED_PORT', units='miles')
#' MainDataTable$DistLocLock <- create_dist_between(MainDataTable,c('LonLat_START_LON','LonLat_START_LAT'),c('LonLat_END_LON','LonLat_END_LAT'), units='midpoint')
#' MainDataTable$DistPortPort <- create_dist_between(MainDataTable,'DISEMBARKED_PORT','EMBARKED_PORT', units='meters')
#' }

  # \tabular{AddPromptparams}{

#    head(create_dist_between(dat,'centroid','EMBARKED_PORT', units='miles'))
#   head(create_dist_between(dat,c('LonLat_START_LON','LonLat_START_LAT'),c('LonLat_END_LON','LonLat_END_LAT'), units='midpoint'))
#       head(create_dist_between(dat,'DISEMBARKED_PORT','EMBARKED_PORT', units='meters'))
  
  #Call in datasets
  if(start[1]==end[1]){
    stop('Starting and ending vectors are identical.')
  }
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
  
  if(any(grepl('port', c(start[1],end[1]), ignore.case=TRUE))){
    #  in port table
    fun <- function(){
      readline("What is the table name in fishset_db containing port data?  ")
      #return(PortTable)
    }
    vars <- if(interactive()) fun()
      if(table_exists(gsub("\'|\"","",vars[1]))==FALSE){
        print(DBI::dbListTables(fishset_db))
        stop(paste(PortTable, 'not defined or does not exist. Consider using one of the tables listed above that exist in the database.'))
      } else {
        port.table <- table_view(gsub("\'|\"","",vars[1]))
      }
  }
  DBI::dbDisconnect(fishset_db)

  if(any(grepl('centroid', c(start[1],end[1]), ignore.case=TRUE))){
    fun <- function(){
     gridfile <- readline("What is the name of the spatial data set? Can be shape file, data frame, or list?")
     long.grid <- readline("What is the name of the vector containing longitude of points from spatial data set?")
     lat.grid <- readline("What is the name of the vector containing latitude of points from spatial data set?")
     lon.dat <- readline("What is the name of the vector containing longitude of points from data set?")
     lat.dat <-readline("What is the name of the vector containing latitude of points from data set?") 
     cat <- readline("What is the name of the vector defining the individual areas or zones from the spatial data set?")
     out <- c(gridfile, long.grid,lat.grid,lon.dat,lat.dat,cat )
    }
    vars <- if(interactive()) fun()
    
    dataset <- assignment_column(dat=dataset, gridfile=eval(parse(text=vars[1])),hull.polygon=FALSE, lon.grid=gsub('\"|\'',"",vars[2]), lat.grid=gsub('\"|\'',"",vars[3]), 
                                 lon.dat = gsub('\"|\'',"",vars[4]), lat.dat=gsub('\"|\'',"",vars[5]), cat=gsub('\"|\'',"",vars[6]), closest.pt = TRUE)
    int <- find_centroid(dataset, gridfile=eval(parse(text=vars[1])), lon.grid==gsub('\"|\'',"",vars[2]), lat.grid==gsub('\"|\'',"",vars[3]), 
                         lon.dat=gsub('\"|\'',"",vars[4]), lat.dat=gsub('\"|\'',"",vars[5]), cat=gsub('\"|\'',"",vars[6]), weight.var=NULL)
  }
  
  if(grepl('port', start[1], ignore.case=TRUE)){
    start.lat <- as.numeric(sapply(trimws(dataset[[start]]), 
                          function(x) port.table[which(port.table[['Port_Name']] == x), "Port_Lat"]))
    
    start.long <- as.numeric(sapply(trimws(dataset[[start]]), 
                      function(x) port.table[which(port.table[['Port_Name']] == x), "Port_Long"]))
   
  } else if(start[1]=='centroid'){
    start.lat <- as.numeric(sapply(trimws(dataset[['ZoneID']]), 
                                        function(x) int[which(int[['ZoneID']] == x), "cent.lat"]))
    start.long <- as.numeric(sapply(trimws(dataset[['ZoneID']]), 
                                         function(x) int[which(int[['ZoneID']] == x), "cent.lon"]))
    
  } else {
    start.long <- dataset[[start[1]]]
    start.lat <- dataset[[start[2]]]
  }
  
   
  if(grepl('port', end[1], ignore.case=TRUE)){
    end.lat <- as.numeric(sapply(trimws(dataset[[end]]), 
                                           function(x) port.table[which(port.table[['Port_Name']] == x), "Port_Lat"]))
    end.long <- as.numeric(sapply(trimws(dataset[[end]]), 
                                           function(x) port.table[which(port.table[['Port_Name']] == x), "Port_Long"]))
  } else if(end[1]=='centroid'){
    end.lat <- as.numeric(sapply(trimws(dataset[['ZoneID']]), 
                                        function(x) int[which(int[['ZoneID']] == x), "cent.lat"]))
    end.long <- as.numeric(sapply(trimws(dataset[['ZoneID']]), 
                                         function(x) int[which(int[['ZoneID']] == x), "cent.lon"]))
  } else {
    end.lat <- dataset[[end[2]]]
    end.long <- dataset[[end[1]]]
  }

  # Get distance between points
  if(units=='midpoint'){
     distBetween <- geosphere::midPoint(cbind(start.long,start.lat), cbind(end.long,end.lat))
   } else {
     distBetween <- geosphere::distGeo(cbind(start.long,start.lat), 
                                    cbind(end.long,end.lat), a = 6378137, f = 1/298.257223563)
   }
  
  if(units=='miles') {
    distBetween <- distBetween*0.000621371192237334 
   } else if(units=='kilometers'){
      distBetween <- distBetween/1000
   } 
  
 #Log the function 
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
  
  create_dist_between_function <- list()
  create_dist_between_function$functionID <- 'create_dist_between'
  create_dist_between_function$args <- c(deparse(substitute(dat)), start, end, units)
  create_dist_between_function$kwargs <- list(vars)
  #create_dist_between_function$output <- paste0(deparse(substitute(dat)),'$',name)
  functionBodyout$function_calls[[length(functionBodyout$function_calls)+1]] <- (create_dist_between_function)
  logbody$fishset_run <- list(infoBodyout, functionBodyout)
  write(jsonlite::toJSON(logbody, pretty = TRUE, auto_unbox = TRUE),paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""))
  assign("functionBodyout", value = functionBodyout, pos = 1)
  
  return(distBetween)
  }




##---- Temporal  Variables ----##
#' Create duration of time variable
create_duration <- function(dat, start, end, units = c("week", "day", "hour", "minute"), name='create_duration') {
  #' @param dat Main data frame over which to apply function. Table in fishset_db database should contain the string `MainDataTable`.
  #' @param start Variable indicating start of time period
  #' @param end Variable indicating end of time period
  #' @param units Unit of time for calculating duration. Must be weeks, days, hours, or minutes.
  #' @param name Name of created vector. Used in the logging function to reproduce work flow. Defaults to name of the function if not defined.
  #' @importFrom lubridate interval as.duration dweeks ddays dhours dminutes
  #' @export create_duration 
  #' @details Calculates the duration of time between two temporal variables based on defined unit. The new variable is added to the dataset. 
  #' @examples 
  #' \dontrun{
  #' MainDataTable$TripDur <- create_duration(MainDataTable, 'TRIP_START', 'TRIP_END',  units='minute')
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
  
  
  if (any(grepl("date|min|hour|week|month|TRIP_START|TRIP_END", start, ignore.case = TRUE)) == FALSE) {
    warning("Function is designed for temporal variables")
  }
  if (any(grepl("date|min|hour|week|month|TRIP_START|TRIP_END", end, ignore.case = TRUE)) == FALSE) {
    warning("Function is designed for temporal variables")
  }
  
  elapsed.time <- lubridate::interval(FishSET:::date_parser(dataset[[start]]), FishSET:::date_parser(dataset[[end]]))
  if (units == "week") {
    dur <- lubridate::as.duration(elapsed.time)/lubridate::dweeks(1)
  } else if (units == "day") {
    dur <- lubridate::as.duration(elapsed.time)/lubridate::ddays(1)
  } else if (units == "hour") {
    dur <- lubridate::as.duration(elapsed.time)/lubridate::dhours(1)
  } else if (units == "minute") {
    dur <- lubridate::as.duration(elapsed.time)/lubridate::dminutes(1)
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
  create_var_temp_function <- list()
  create_var_temp_function$functionID <- 'create_duration'
  create_var_temp_function$args <- c(deparse(substitute(dat)), start, end, units)
  create_var_temp_function$kwargs <- list()
  create_var_temp_function$output <- paste0(deparse(substitute(dat)),'$',name)
  logbody$fishset_run <- list(infoBodyout, functionBodyout)
  write(jsonlite::toJSON(logbody, pretty = TRUE, auto_unbox = TRUE), paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""))
  assign("functionBodyout", value = functionBodyout, pos = 1)

 return(dur)
}
