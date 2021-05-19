#' Collapse data frame from haul to trip
#'
#' @param dat Primary data containing information on hauls or trips.
#' Table in the FishSET database contains the string 'MainDataTable'.
#' @param project String, name of project.
#' @param fun.time How to collapse temporal data. For example, \code{min}, \code{mean}, \code{max}. 
#'   Cannot be \code{sum} for temporal variables.
#' @param fun.numeric How to collapse numeric or temporal data. For example, \code{min}, \code{mean}, 
#'    \code{max}, \code{sum}. Defaults to \code{mean}.
#' @param tripID Column(s) that identify the individual trip.
#' @param haul_count Logical, whether to return a column of the number of hauls per trip. 
#' @param log_fun Logical, whether to log function call (for internal use).
#' @export haul_to_trip
#' @return Returns the primary dataset where each row is a trip.
#' @details Collapses primary dataset from haul to trip level. Unique trips are defined based on selected column(s), for 
#'   example, landing permit number and disembarked port. This id column is used to collapse the
#'   data to trip level.  \code{fun.numeric} and \code{fun.time} define how multiple observations for a trip
#'   are collapsed. For variables that are not numeric or dates, the first observation is used.
#'
#' @examples
#' \dontrun{
#' pollockMainDataTable <- haul_to_trip("pollockMainDataTable","pollock",
#'     min, mean, "PERMIT", "DISEMBARKED_PORT"
#'     )
#' }
#'
haul_to_trip <- function(dat, project, fun.numeric = mean, fun.time = mean, tripID, 
                         haul_count = TRUE, log_fun = TRUE) {
  # fun.time = min, Create a column that indicates unique trip levels

  out <- data_pull(dat)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main")
  
  # Load in dataindex
  #dataIndex <- dataindex_update(dataset, pull_info_data(project))
  
  # create rowID variable
  int <- ID_var(dataset, project = project, vars = tripID, type = "integer", name = "rowID", 
                drop = FALSE, log_fun = FALSE)
  
  cat(length(unique(int$rowID)), "unique trips were identified using", tripID, "\n")
  
  # Handling of empty variables
  if (any(vapply(int, function(x) all(is.na(x)), logical(1)))) {
    
    drop_empty <- names(int)[which(vapply(int, function(x) all(is.na(x)), logical(1)))]
    if (length(drop_empty) > 0) int[drop_empty] <- NULL
  }
  
  row_ind <- which(colnames(int) == "rowID")
  
#  if (length(drop_empty) > 0) {
    
#    DI_mod <- dataIndex[!(dataIndex$variable_name %in% drop_empty), 
#                        c("variable_name", "generalType")] 
    
#  } else {
    
     
#  }
  
  DI_var <-  colnames(dataset)
  DI_type <- c(ifelse(grepl("DATE|MIN", colnames(dataset), ignore.case = TRUE), "Time", 
                      ifelse(grepl("IFQ", colnames(dataset), ignore.case = TRUE), "Flag", 
                             ifelse(grepl("ID", colnames(dataset),ignore.case = TRUE), "Code", 
                                    ifelse(grepl("Long|Lat", colnames(dataset), ignore.case = TRUE), "Latitude", 
                                           ifelse(grepl("TYPE|PROCESSOR|LOCATION|METHOD",colnames(dataset), ignore.case = TRUE), "Code String", 
                                                  ifelse(grepl("CHINOOK|CHUM|FATHOMS|DOLLARS|LBS|PROPORTION|VALUE|PERCENT|MT", colnames(dataset), ignore.case = TRUE), "Other Numeric", 
                                                         ifelse(grepl("HAUL|AREA|PERFORMANCE|PERMIT", colnames(dataset), ignore.case = TRUE), "Code Numeric", NA)
                                                  )))))))
  
  # Collapse data based on rowID and defined function
  out <- data.frame(drop = rep(0, length(unique(int$rowID))))
  
  # Nothing listed (grab first value)
  na_type <- DI_var[is.na(DI_type)]
  
  if (length(na_type) > 0) {
    
    out[na_type] <- aggregate(int[na_type], by = list(int$rowID), 
                              FUN = head, 1)[, -1] #remove Group.1
  }
  # recode NAs as character (makes searching by name easier)
  DI_type[is.na(DI_type)] <- "NA"
  
  # Time - not duration
  dur_type <- grep("DUR", DI_var[DI_type %in% "Time"], ignore.case = TRUE, value = TRUE)
  
  if (length(which(DI_type == "Time")) > 0) {
    
    time_vars <- DI_var[DI_type == "Time"]
    time_drop <- which(time_vars %in% dur_type)
    
    if (length(time_drop) > 0) time_vars <- time_vars[-time_drop]
    
    int[time_vars] <- lapply(int[time_vars], date_parser)
    out[time_vars] <- aggregate(int[time_vars], by = list(int$rowID), 
                                FUN = match.fun(fun.time), na.rm = TRUE)[, -1]
  }
  
  # Time - duration
  if (length(dur_type) > 0) {
    
    out[dur_type] <- aggregate(int[dur_type], by = list(int$rowID), 
                               FUN = match.fun(fun.numeric), na.rm = TRUE)[, -1]
  }
  
  # Other numeric
  if (sum(DI_type == "Other Numeric") > 0) {
    
    other_num_vars <- DI_var[DI_type == "Other Numeric"]
    out[other_num_vars] <- aggregate(int[other_num_vars], by = list(int$rowID), 
                                     FUN = match.fun(fun.numeric), na.rm = TRUE)[, -1]
  }
  
  # Latitude
  if (sum(DI_type == "Latitude") > 0) {
    
    lat_vars <- DI_var[DI_type == "Latitude"]
    out[lat_vars] <- aggregate(int[lat_vars], by = list(int$rowID), FUN = head, 1)[, -1]
  }
  
  # Code numeric
  if (sum(DI_type == "Code Numeric") > 0) {
    
    cn_vars <- DI_var[DI_type == "Code Numeric"]
    out[cn_vars] <- aggregate(int[cn_vars], by = list(int$rowID), FUN = head, 1)[, -1]
  }
  
  # Coded
  if (sum(DI_type == "Code") > 0) {
    
    c_vars <- DI_var[DI_type == "Code"]
    out[c_vars] <- aggregate(int[c_vars], by = list(int$rowID), FUN = head, 1)[, -1]
  }
 
  # Code string
  if (sum(DI_type == "Code String") > 0) {
    
    cs_vars <- DI_var[DI_type == "Code String"]
    out[cs_vars] <- aggregate(int[cs_vars], by = list(int$rowID), FUN = head, 1)[, -1]
  }
  
  # Not in the dataIndex file
#  not_in_DI <- names(int)[!(names(int)[-row_ind] %in% dataIndex$variable_name)]
  
#  if (length(not_in_DI) > 0) {
#    out[not_in_DI] <- aggregate(int[not_in_DI], by = list(int$rowID), FUN = head, 1)[, -1]
#  }
  
  # Add hauls per trip 
  if (haul_count) {
    
    out$HAUL_COUNT <- agg_helper(int, value = "rowID", group = tripID, fun = length)$rowID
  }
  
  # remove "drop" column
  out$drop <- NULL
  
  if (log_fun) {
    
    haul_to_trip_function <- list()
    haul_to_trip_function$functionID <- "haul_to_trip"
    haul_to_trip_function$args <- list(dat, project, deparse(substitute(fun.numeric)), 
                                       deparse(substitute(fun.time)), tripID, haul_count, 
                                       log_fun)
    haul_to_trip_function$output <- list(dat)
    log_call(project, haul_to_trip_function)
  }

  return(out)
}
