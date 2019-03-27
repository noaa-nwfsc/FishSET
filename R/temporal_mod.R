#'  Transform units of a temporal variables 
#'
#' @param dataset dataframe or matrix
#' @param x Time variable to modify 
#' @param fun.mod Defines temporal time frame such as year, month, day
#' @param define.format Format of temporal data 
#' @keywords Date, as.Date
#' @return Returns variable with modified time unites
#' @export temporal_mod
#' @details define.format defines the format that the variable should take on. Examples include '%Y%m%d', '%Y-%m-%d %H:%M:%S'.
#' The function also has options to extract year, month, minute, or month/day. These are defined through fun.mod
#' 
# @examples  Date_Landed_YMD <- temp_mod(MainDataTable, 'DATE_LANDED', fun.mod = '',define.format = '%Y%m%d') 
#' Date_Landed_year <- temp_mod(MainDataTable, 'DATE_LANDED', fun.mod = 'year', define.format = '')

# Change to Year, Month, day, minutes
temporal_mod <- function(dataset, x, fun.mod = "", define.format) {
  if (fun.mod == "") {
    # User defines the format of the time variable
    int <- format(date_parser(dataset[[x]]), format = define.format)
    # Extract specific time unit
  } else {
    if (fun.mod == "month") {
      # Month:
      int <- format(as.Date(date_parser(dataset[[x]])), format = "%m")
    } else if (fun.mod == "year") {
      # Year:
      int <- format(as.Date(date_parser(dataset[[x]])), format = "%Y")
    } else if (fun.mod == "month/day") {
      # Month/day
      int <- format(as.Date(date_parser(dataset[[x]])), format = "%m/%d")
    } else {
      warning("fun.mod is not recognized. Choices include, year, month, month/day, minute")
    }
  }
  
  # logging function information
  #df.name <- deparse(substitute(dataset))
  #write(layout.json.ed(trace, "TempMod", df.name, x, msg = paste("fun.mod:", fun.mod, "; define.format:", define.format)),
  #                     paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""), append = T)
  if(!exists('logbody')) { 
    logging_code()
  } 
  temp_mod_function <- list()
  temp_mod_function$functionID <- 'temp_mod'
  temp_mod_function$args <- c(deparse(substitute(dataset)), x, fun.mod, define.format)
  functionBodyout$function_calls[[length(functionBodyout$function_calls)+1]] <- (temp_mod_function)
  logbody$fishset_run <- list(infoBodyout, functionBodyout)
  write(jsonlite::toJSON(logbody, pretty = TRUE, auto_unbox = TRUE),paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""))
  assign("functionBodyout", value = functionBodyout, pos = 1)
  
  return(int)
  
}
