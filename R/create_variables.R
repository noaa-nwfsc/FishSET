# Create variables or matrix.

# @examples 
# DumVar <- DummyVar(MainDataTable) 
# DummyMatrix(MainDataTable, 'DISEMBARKED_PORT')

##--- CPUE ----##
#' Create catch per unit effort variable
cpue <- function(dataset, xWeight, xTime) {
  #' @param dataset dataframe or matrix
  #' @param xWeight Weight variable
  #' @param xTime Time variables. Must be weeks, days, hours, or minutes
  #' @export cpue 
  #' @details Function for generating new or specialized variables. cpue function create catch per unit effort variable. 
  # @example MainDataTable$cpue <- cpue(MainDataTable, 'OFFICIAL_TOTAL_CATCH_MT', 'DURATION_IN_MIN')   
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
    logging_code()
  } 
  create_var_cpue_function <- list()
  create_var_cpue_function$functionID <- 'cpue'
  create_var_cpue_function$args <- c(deparse(substitute(dataset)), xWeight, xTime)
  functionBodyout$function_calls[[length(functionBodyout$function_calls)+1]] <- (create_var_cpue_function)
  logbody$fishset_run <- list(infoBodyout, functionBodyout)
  write(jsonlite::toJSON(logbody, pretty = TRUE, auto_unbox = TRUE),paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""))
  assign("functionBodyout", value = functionBodyout, pos = 1)
  
  return(cpue)
}


##---- Dummy  Variables ----##
#' Create a new dummy variable
dummy_var <- function(dataset, DumFill = 'TRUE') {
  #' @param dataset dataframe or matrix
  #' @param DumFill Fill the dummy variable with TRUE or FALSE
  #' @export dummy_var
  #' @details Function for generating new or specialized variables. dummy_var creates a dummy variable and dummy_matrix creates a dummy matrix. 
  # @example MainDataTable$dummyvar <- dummy_var(MainDataTable, DumFill=TRUE)
  
  dummyvar <- as.vector(rep(DumFill, nrow(dataset)))
  # logging function information

  #write(layout.json.ed(trace, "DummyVar", deparse(substitute(dataset)), x = ""), 
  #      paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""), append = T)
  
  if(!exists('logbody')) { 
    logging_code()
  } 
  create_var_dummy_var_function <- list()
  create_var_dummy_var_function$functionID <- 'dummy_var'
  create_var_dummy_var_function$args <- deparse(substitute(dataset))
  create_var_dummy_var_function$kwargs <- list('DumFill'=DumFill)
  functionBodyout$function_calls[[length(functionBodyout$function_calls)+1]] <- (create_var_dummy_var_function)
  logbody$fishset_run <- list(infoBodyout, functionBodyout)
  write(jsonlite::toJSON(logbody, pretty = TRUE, auto_unbox = TRUE),paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""))
  assign("functionBodyout", value = functionBodyout, pos = 1)
  
  return(dummyvar)
}


#' Create dummy matrix from a coded ID variable
dummy_matrix <- function(dataset, x) {
  #' @param dataset dataframe or matrix
  #' @param x Variable to create 
  #' @export dummy_matrix
  #' @details Function for generating new or specialized variables. dummy_matrix creates a dummy matrix.
  # @example PortMatrix <- dummy_matrix(MainDataTable, 'PORT_CODE')
  
  #write(layout.json.ed(trace, "DummyMatrix", deparse(substitute(dataset)), x = deparse(substitute(x))), 
   #     paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""), append = T)
  
  if(!exists('logbody')) { 
    logging_code()
  } 
  create_var_dummy_matrix_function <- list()
  create_var_dummy_matrix_function$functionID <- 'dummy_matrix'
  create_var_dummy_matrix_function$args <- c(deparse(substitute(dataset)), x)
  functionBodyout$function_calls[[length(functionBodyout$function_calls)+1]] <- (create_var_dummy_matrix_function)
  logbody$fishset_run <- list(infoBodyout, functionBodyout)
  write(jsonlite::toJSON(logbody, pretty = TRUE, auto_unbox = TRUE),paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""))
  assign("functionBodyout", value = functionBodyout, pos = 1)
  
    # create the matrix
  factor.levels <- levels(as.factor(dataset[[x]]))
  int <- data.frame(matrix(rep(dataset[[x]], length(factor.levels)), ncol = length(factor.levels)))
  colnames(int) = factor.levels
  # change matrix to TRUE/FALSE
  int <- data.frame(lapply(1:length(factor.levels), function(x) ifelse(int[, x] == colnames(int)[x], TRUE, FALSE)))
  colnames(int) = paste(x, "_", levels(as.factor(dataset[[x]])))
  return(int)
}


##---- Coded variables ----##
#' Create quantile variable
# Quantile are set as: .2 (0, 20%, 40%, 60%, 80%, 100%), 
# .25 (0%, 25%, 50%, 75%, 100%),
# .4 (0%, 10%, 40%, 90%, 100%)
set_quants <- function(dataset, x, quant.cat = c(0.2, 0.25, 0.4)) {
  #' @param dataset dataframe or matrix
  #' @param x Variable to create 
  #' @param quant.cat Quantile categories. Includes 0.2, 0.25, 0.4.
  #' @export set_quants
  #' @details Function for generating new or specialized variables. setQuants creates a coded variable based on the quantiles of x. 
  # #Quantile are set as: .2 (0, 20%, 40%, 60%, 80%, 100%), .25 (0%, 25%, 50%, 75%, 100%), .4 (0%, 10%, 40%, 90%, 100%).
  # @example MainDataTable <- set_quants(MainDataTable, 'HAUL', quant.cat=.2)
  

  #write(layout.json.ed(trace, "setQuants", deparse(substitute(dataset)), x = deparse(substitute(x)), 
  #                     msg = paste("quant.cat:", quant.cat, sep = "")),  
  #      paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""), append = T)
  if(!exists('logbody')) { 
    logging_code()
  } 
  create_var_set_quants_function <- list()
  create_var_set_quants_function$functionID <- 'set_quants'
  create_var_set_quants_function$args <- c(deparse(substitute(dataset)), x, quant.cat)
  create_var_set_quants_function$output <- deparse(substitute(dataset))
  functionBodyout$function_calls[[length(functionBodyout$function_calls)+1]] <- (create_var_set_quants_function)
  logbody$fishset_run <- list(infoBodyout, functionBodyout)
  write(jsonlite::toJSON(logbody, pretty = TRUE, auto_unbox = TRUE),paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""))
  assign("functionBodyout", value = functionBodyout, pos = 1)
  
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
  dataset$var.name = var.name
  return(dataset)
}


##---- Numeric  Variables ----##
#' Create numeric variables
create_var_num <- function(dataset, x, y, method, name) {
  #' @param dataset dataframe or matrix
  #' @param x Variable to create 
  #' @param y Second variable Note that in division  x is divided by y.
  #' @param method Addition, subtraction, multiplication, division
  #' @param name Name of new variable
  #' @export create_var_num
  #' @details Function for generating new or specialized variables. create_var_num creates a new numeric variable based on defined arithmetic function. New variable is added to the dataset.
  # example MainDataTable <- create_var_num(MainDataTable, 'TRIP_NUMBER_CHINOOK','TRIP_NUMBER_CHUM', 'sum','TimeChange')
  #'
   if (is.numeric(dataset[[x]]) == FALSE | is.numeric(dataset[[y]]) == FALSE) {
    stop("Variables must be numeric")
   }
  
   #write(layout.json.ed(trace, "create_var_num", deparse(substitute(dataset)), x = x, 
   #                    msg = paste("y:", y, ", method:", method, ", name:", name, sep = "")), 
   #     paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""), append = T)
  
   #logging
   #write(layout.json.ed(trace, "checkModelData", deparse(substitute(dataset)), x, msg = paste("Saved as", save.name)),
   #                      paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""), append = T)
  if(!exists('logbody')) { 
    logging_code()
  } 
  create_var_num_function <- list()
   create_var_num_function$functionID <- 'create_var_num'
   create_var_num_function$args <- c(deparse(substitute(dataset)), x, y, method, name)
   create_var_num_function$output <- deparse(substitute(dataset))
   functionBodyout$function_calls[[length(functionBodyout$function_calls)+1]] <- (create_var_num_function)
   logbody$fishset_run <- list(infoBodyout, functionBodyout)
   assign("functionBodyout", value = functionBodyout, pos = 1)
   
   write(jsonlite::toJSON(logbody, pretty = TRUE, auto_unbox = TRUE), paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""))
   
   
  if (grepl("add|sum", method, ignore.case = TRUE)) {
    dataset[[name]] <- dataset[[x]] + dataset[[y]]
  } else if (grepl("sub", method, ignore.case = TRUE)) {
    dataset[[name]] <- dataset[[x]] - dataset[[y]]
  } else if (grepl("mult", method, ignore.case = TRUE)) {
    dataset[[name]] <- dataset[[x]] * dataset[[y]]
  } else if (grepl("div", method, ignore.case = TRUE)) {
    dataset[[name]] <- dataset[[x]]/dataset[[y]]
  }

  
  
  return(dataset)
}


##---- Spatial  Variables ----##



##---- Temporal  Variables ----##
#' Create temporal variables
create_var_temp <- function(dataset, start, end, units = c("week", "day", "hour", "minute")) {
  #' @param dataset dataframe or matrix
  #' @param start Variable indicating start of time period
  #' @param end Variable indicating end of time period
  #' @param units Units of time varibles. Must be weeks, days, hours, or minutes
  #' @importFrom lubridate interval as.duration dweeks ddays dhours dminutes
  #' @export create_var_temp 
  #' @details Function for generating new or specialized variables. create_var_temp calculates the duration of time between two temporal variables based on defined time format. The new variable is added to the dataset. 
  # @example MainDataTable$TripDur <- create_var_temp(MainDataTable, 'TRIP_START', 'TRIP_END',  units='minute')
  
  
  if (any(grepl("dat|min|hour|week|month|TRIP_START|TRIP_END", start, ignore.case = TRUE)) == FALSE) {
    warning("Function is designed for temporal variables")
  }
  if (any(grepl("dat|min|hour|week|month|TRIP_START|TRIP_END", end, ignore.case = TRUE)) == FALSE) {
    warning("Function is designed for temporal variables")
  }
  
  elapsed.time <- lubridate::interval(date_parser(dataset[[start]]), date_parser(dataset[[end]]))
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
    logging_code()
  } 
  create_var_temp_function <- list()
 create_var_temp_function$functionID <- 'create_var_temp'
 create_var_temp_function$args <- c(deparse(substitute(dataset)), start, end, name, units)
 create_var_temp_function$output <- deparse(substitute(dataset))
 logbody$fishset_run <- list(infoBodyout, functionBodyout)
 write(jsonlite::toJSON(logbody, pretty = TRUE, auto_unbox = TRUE),paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""))
 assign("functionBodyout", value = functionBodyout, pos = 1)

 return(dur)
}
