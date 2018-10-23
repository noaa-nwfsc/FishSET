#' View summary statistics for a dataframe
#'
#'
#' @param dataset dataframe or matrix over which to apply function
#' @keywords summary statistics
#' @method
#' @export
#' @details This function print summary statistics. It prints the number of unique observations for each column along with standard summary statistics

#' @examples
#' Generate data for example: outliers inejcted into cars dataset.
#' cars1 <- cbind(rbind(cars[1:30, ], data.frame(speed=c(19,19,20,20,20), dist=c(190, 186, 210, 220, 218))), ID=rep(c('a','b','c','d','e'), 7))
#' summary_stats(cars1)



summary_stats <- function(dataset) {
     rbind(summary(dataset),
      apply(dataset, 2, function(x) paste('UniqueObs:',length(unique(x))))
)
}
