#' View summary statistics for a dataframe
#'
#' @param dataset dataframe or matrix over which to apply function
#' @keywords summary statistics
#' @export summary_stats
#' @details This function print summary statistics. It prints the number of unique observations for each column along with standard summary statistics

# @examples
# summary_stats(MainDataTable[,'HAUL'])

summary_stats <- function(dataset) {
    rbind(summary(dataset), apply(dataset, 2, function(x) paste("UniqueObs:", length(unique(x)))))
}
