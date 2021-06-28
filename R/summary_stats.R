#' View summary statistics
#'
#' View summary statistics in table format for all variables in primary dataset.
#' @param dat Primary data containing information on hauls or trips.
#' Table in FishSET database contains the string 'MainDataTable'.
#' @param project String, name of project.
#' @param x Optional. Variable in \code{dat} to apply function over. If not defined, summary stats
#'   are displayed for all columns in the dataset.
#' @param project Name of project
#' @param log_fun Logical, whether to log function call (for internal use).
#' @keywords summary statistics
#' @export summary_stats
#' @details Prints summary statistics for each variable in the data set. If \code{x} is specified, summary stats will be returned only for that variable.
#' Function is called in the \code{\link{data_check}} function.
#' @examples
#' \dontrun{
#' summary_stats(pcodMainDataTable)
#' summary_stats(pcodMainDataTable, x = "HAUL")
#' }
#'
summary_stats <- function(dat, project, x = NULL, log_fun = TRUE) {

  # Call in datasets
  out <- data_pull(dat)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main")
  
  # Min apply(dataset, 2, function(x) min(x, na.rm=T)) 1st Quartile apply(dataset, 2, function(x) quantile(x)[2]) Median apply(dataset, 2, function(x)
  # median(x, na.rm=T)) Mean apply(dataset, 2, function(x) mean(x, na.rm=TRUE)) 3rd Quartile apply(dataset, 2, function(x) length(unique(x))) Max
  # apply(dataset, 2, function(x) max(x, n.arm=TRUE)) UniqueObs apply(dataset, 2, function(x) paste('UniqueObs:', length(unique(x))))

#Extract only numeric 
numeric_only <- unlist(lapply(dataset, is.numeric))

  numdat <- dataset[ , numeric_only]
  chardat <- dataset[,which(numeric_only==FALSE)]
  
  # all columns
  if (is_empty(x)) {
      sum_table <- 
      cbind(  
      as.data.frame(as.matrix(rbind(
        summary(numdat, digits = 2)[-c(2,5,7),], 
        apply(numdat, 2, function(x) paste("NA's:", sum(is.na(x)))),
        apply(numdat, 2, function(x) paste("UniqueObs:", length(unique(x)))),
        apply(numdat, 2, function(x) paste("No. 0's:", length(which(x == 0))))
      )), row.names = FALSE),
      
      as.data.frame(as.matrix(rbind(
        summary(chardat, digits = 2), 
        apply(chardat, 2, function(x) paste("First:", x[1])),
        apply(chardat, 2, function(x) paste("NA's:", sum(is.na(x)))),
        apply(chardat, 2, function(x) paste("UniqueObs:", length(unique(x)))),
        apply(chardat, 2, function(x) paste("No. 0's:", length(which(x == 0))))
        )), row.names = FALSE)
      )
  } else {
      sum_table <- c(summary(dataset[[x]]),
                     paste("NA's:", sum(is.na(dataset[[x]]))), 
                     paste("UniqueObs:", length(unique(dataset[[x]]))), 
                     paste("No. 0's:", length(which(dataset[[x]] == 0))))[-c(2,5)]
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
