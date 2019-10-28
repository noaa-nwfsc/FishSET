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
  out <- data_pull(dat)
  dat <- out$dat
  dataset <- out$dataset
  
  # Min
  #apply(dataset, 2, function(x) min(x, na.rm=T))
  #1st Quartile
  #apply(dataset, 2, function(x) quantile(x)[2])
  #Median
  #apply(dataset, 2, function(x) median(x, na.rm=T))
  #Mean
  #apply(dataset, 2, function(x) mean(x, na.rm=TRUE))
  #3rd Quartile
  #apply(dataset, 2, function(x) length(unique(x)))
  #Max
  #apply(dataset, 2, function(x) max(x, n.arm=TRUE))
  #UniqueObs 
 #apply(dataset, 2, function(x) paste("UniqueObs:", length(unique(x))))
  
  
  if(FishSET:::is_empty(x)){
    as.data.frame(as.matrix(rbind(summary(dataset, digits=2), 
                                  apply(dataset, 2, function(x) paste("UniqueObs:", length(unique(x)))),
                                  apply(dataset, 2, function(x) paste("No. 0's:", length(which(x==0))))
                                  )), row.names=FALSE)[-c(2,5),]
  } else {
    c(summary(dataset[[x]]), paste("UniqueObs:", length(unique(dataset[[x]]))), 
                            paste("No. 0's:", length(which(dataset[[x]]==0)))
      )[-c(2,5)]
  }
}
