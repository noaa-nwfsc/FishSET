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
#mid-haul
#trip centroid
#mean, max, min latitude by year and other grouping
#hot-spot analysis ( getis ord and morans I)
#spatial summary statistics
#spatial kernel?



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
