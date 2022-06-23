#' View summary statistics
#'
#' View summary statistics in table format for entire dataset or for a specific
#' variable.
#' 
#' @param dat Primary data containing information on hauls or trips.
#' Table in FishSET database contains the string 'MainDataTable'.
#' @param project String, name of project.
#' @param x Optional. Variable in \code{dat} to view summary statistics for. If not 
#'   defined, summary stats are displayed for all columns in the dataset.
#' @param project Name of project
#' @param log_fun Logical, whether to log function call (for internal use).
#' @keywords summary statistics
#' @export summary_stats
#' @details Prints summary statistics for each variable in the data set. If 
#'   \code{x} is specified, summary stats will be returned only for that variable.
#'   Numeric variables are summarized by minimum, median, mean, maximum, and the 
#'   number of NA's, unique values, and zeros. Non-numeric variables are summarized
#'   by first value and the number of NA's, unique values, and empty values. 
#'   Function is called in the \code{\link{data_check}} function.
#' @examples
#' \dontrun{
#' summary_stats(pcodMainDataTable, project = "pcod")
#' 
#' summary_stats(pcodMainDataTable, project = "pcod", x = "HAUL")
#' }
#'
summary_stats <- function(dat, project, x = NULL, log_fun = TRUE) {

  # Call in datasets
  out <- data_pull(dat, project)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main", project)
  
  if (!is_value_empty(x)) {
    
    column_check(dataset, cols = x)
  }
  
  # Extract only numeric 
  # include date?
  # is_num <- function(x) {
  #   is.numeric(x) || lubridate::is.Date(x) || lubridate::is.POSIXt(x)
  # }

  # all columns
  if (is_value_empty(x)) {
    
    numeric_only <- vapply(dataset, is.numeric, logical(1))
    
    numdat <- dataset[ , numeric_only]
    chardat <- dataset[ , !numeric_only]
    
      sum_table <- 
      cbind(  
      as.data.frame(rbind(
        summary(numdat, digits = 2)[-c(2,5,7),], 
        apply(numdat, 2, function(x) paste("NA's:", sum(is.na(x)))),
        apply(numdat, 2, function(x) paste("Unique Obs:", length(unique(x)))),
        apply(numdat, 2, function(x) paste("No. 0's:", sum(x == 0)))
      ), row.names = FALSE),
      
      as.data.frame(rbind(
        apply(chardat, 2, function(x) paste("First:", x[1])),
        apply(chardat, 2, function(x) paste("", NA)),
        apply(chardat, 2, function(x) paste("", NA)),
        apply(chardat, 2, function(x) paste("", NA)),
        apply(chardat, 2, function(x) paste("NA's:", sum(is.na(x)))),
        apply(chardat, 2, function(x) paste("Unique Obs:", length(unique(x)))),
        apply(chardat, 2, function(x) paste("No. Empty:", sum(is_value_empty(x))))
        ), row.names = FALSE)
      )
      
  } else {
    
    if (is.numeric(dataset[[x]])) {
      
      sum_table <- c(round(summary(dataset[[x]]), 2)[-c(2,5)],
                     "NA's" = sum(is.na(dataset[[x]])), 
                     "Unique Obs" = length(unique(dataset[[x]])), 
                     "No. 0's" = sum(dataset[[x]] == 0))
      
    } else {
      
      sum_table <- c(
        "First" = dataset[[x]][1],
        "NA's" = sum(is.na(dataset[[x]])),
        "Unique Obs" = length(unique(dataset[[x]])),
        "No. Empty" = sum(is_value_empty(dataset[[x]]))
        )
    }
  }

  if (log_fun) {
    
    summary_stats_function <- list()
    summary_stats_function$functionID <- "summary_stats"
    summary_stats_function$args <- list(dat, project, x, log_fun)
    log_call(project, summary_stats_function)
  }

  save_table(sum_table, project, "summary_stats")
  return(sum_table)
}
