#' View summary statistics 
#'
#' @param dat Main data frame over which to apply function. Table in fishset_db database should contain the string `MainDataTable`.
#' @param x Optional variable to apply function over.
#' @keywords summary statistics
#' @export summary_stats
#' @details Prints summary statistics for each variable in the data set. If `x` is specified, summary stats will be returned only
#' for that variable. Function is called in the \code{\link{data_check}} function.
#' @examples
#' \dontrun{
#' summary_stats(MainDataTable, x='')
#' summary_stats(MainDataTable, x='HAUL')
#' }

summary_stats <- function(dat, x=NULL) {
  
  
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
  
  
  if(FishSET:::is_empty(x)){
    rbind(summary(dataset), apply(dataset, 2, function(x) paste("UniqueObs:", length(unique(x)))))
  } else {
    c(summary(dataset[[x]]), paste("UniqueObs:", length(unique(dataset[[x]]))))
  }
}
