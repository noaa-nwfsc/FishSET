#' Guided steps to cleaning the datafile

#' @param dataset dataframe or matrix
#' @param x column in dataframe to check for outliers
#' @param dataindex dataframe that contains information on each column of the dataset
#' @export data_check
#' @details Prints summary stats for all variable in the dataframe. Checks for NaNs and prints vectors that contain NaNs.
#' Checks for outliers for specified vector. Further actions may be taken to further evaluate and remove NaNs and outliers.
#' Checks that all columnn names in the dataset are unique, whether specialized variables have been identified in the index dataset, whether any columns in the dataset are empty, if units are defined and recognized, whether each row is a unique choice occurrence at the haul or trip level, and that data for either lat/long or fishing area are included.

# example
# load('Data/MainDataTable.rda')
# MainDataTableInfo <- dbGetQuery(fishset_db, "select * from MainDataTableInfo")
# data_check(MainDataTable, 'OFFICIAL_TOTAL_CATCH_MT', MainDataTableInfo)


data_check <- function(dataset, x, dataindex) {
     print(summary_stats(dataset))
     cat('\nNaN checks\n')
     nan_identify(dataset)
     cat('\n')
     cat('\nOutlier checks')
     print(outlier_table(dataset, x))
     cat('\n')
     cat('\n')
     outlier_plot(dataset, x)
     cat('The plot shows the data with no adjustments (distribution specified or points removed). Consider further visualizing the data with outlier_plot. Remove outliers with outlier_remove.')
     cat('\n')
     cat('\nData verification checks.\n')
     data_verification(dataset)
     table_info_verification(dataset, dataindex)
}

