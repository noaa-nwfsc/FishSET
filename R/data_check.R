#' Check for common data quality issues
#'
#' Check primary data for common data quality issues, such as NaNs, NAs, outliers, unique rows, and empty variables.

#' @param dat Primary data containing information on hauls or trips. Table in the FishSET database contains the string 'MainDataTable'.
#' @param project String, name of project.
#' @param x Variable in \code{dat} to check for outliers.
# @param dataindex MainDataTableInfo table from FishSET database that is associated with the dataset. This table contains information on each column of the data frame.
#'  Must be in quotes if called from the FishSET database.
#' @export data_check
#' @details Prints summary stats for all variables in \code{dat}. Prints column names that contain NaNs or NAs. Checks 
#'   for outliers for specified variable \code{x}. Checks that all column names are unique, whether any columns in 
#'    \code{dat} are empty, whether each row is a unique choice occurrence at the haul or trip level, that data for 
#'    either lat/lon or fishing area are included. The function is also called by other functions.
#' @examples
#' \dontrun{
#' data_check(pcodMainDataTable, "OFFICIAL_TOTAL_CATCH_MT")
#' }
#'
data_check <- function(dat, project, x){#, dataindex) {

  # Call in main data set
  out <- data_pull(dat)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main")
  
  # call in data index
#  out <- data_pull(dataindex)
 # dataindex <- out$dat
  #dataindex2 <- out$dataset

  # Run checks
  print(summary_stats(dataset, project))
  cat("\nNA checks\n")
  if (any(apply(dataset, 2, function(x) anyNA(x))) == TRUE) {
    cat("The", names(which(apply(dataset, 2, function(x) anyNA(x)) == TRUE)), "columns contain NAs. Consider using na_filter to replace or remove NAs")
  } else {
    cat("No columns in the dataframe contain NAs")
  }

  cat("\nNaN checks\n")
  if (any(apply(dataset, 2, function(x) any(is.nan(x)))) == TRUE) {
    cat("The", names(which(apply(dataset, 2, function(x) any(is.nan(x))) == TRUE)), "columns contain NaNs. Consider using nan_filter to replace or remove NaNs")
  } else {
    cat("No columns in the dataframe contain NaNs")
  }

  cat("\n")
  cat("\nOutlier checks")
  cat("\n Use the table and plot printed below to assess whether whether outlying points may exist in the selected variable.\n
         If further checking is needed use the outlier_plot function to assess the impact of removing points.")
  print("The outlier table shows basic summary statistics for subsets of the selected variable.")
  print(outlier_table(dataset, project, x))
  cat("\n")
  cat("\n")
  outlier_plot(dataset, project, x, dat.remove = "none", x.dist = "normal", output.screen = TRUE)
  cat("The plot shows the data with no adjustments (distribution specified or points removed). 
         If potential outliers are visible on the null plot, consider further visualizing the data with outlier_plot. 
         Start by using the outlier_plot function ans subsetting the data using the most conservative method: outlier_plot(dataset, x, dat.remove = \"5_95_quant\"). 
         If outliers are present, remove with the outlier_remove function.")
  cat("\n")
  cat("\nData verification checks.\n")
  data_verification(dataset, project)
  # Table_verification_function
#  allNecFields <- c(
#    "name", "units", "general", "XY", "ID", "Time", "Catch", "Effort", "CPUE", "Lat", "Value", "Area", "Port", "Price", "Trip", "Haul",
#    "Other"
#  )
#  indx <- colSums(sapply(allNecFields, grepl, colnames(dataindex2), ignore.case = TRUE))
#
#  if (length(which(colSums(sapply(allNecFields, grepl, colnames(dataindex2))) == 0)) < 1) {
#    cat("Pass: All specialized variables identified.")
#  } else {
#    cat(paste("\nThe following specialized variables were not specified:", names(which(colSums(sapply(allNecFields, grepl, colnames(dataindex2))) ==
#      0))), "Use the main_mod function to specify unites for these variables.")
#  }

  # Units are sensible
#  unitsAvailable <- c(
#    "Day of Month", "Day of Year", "Decimal Degree", "Dollar", "Fathom", "Feet", "Horsepower", "ID", "Kilometer", "Pound", "lbs",
#    "Meter", "Metric Ton", "Mile", "Minute", "mm", "mmdd", "N/A", "Numeric", "Percent", "Tonne", "Week", "wk", "Y/N", "T/F", "yyyymmdd", "yyyy", "yyyy-mm-dd HH:MM:SS",
#    "mm-dd-yyyy HH:MM:SS", "mm/dd/yyyy HH:MM:SS", "yyyy/mm/dd HH:MM:SS", "min"
#  )
#  indx <- colSums(sapply(unitsAvailable, grepl, colnames(dataindex2), ignore.case = TRUE))
#  if (length(which(colSums(sapply(unitsAvailable, grepl, colnames(dataindex2))) == 0))) {
#    cat("\nPass: All units are specified.")
#  } else {
#    cat(paste("\nThe units are not recognized for the following variables:", which(colSums(sapply(unitsAvailable, grepl, colnames(dataindex2))) ==
#      0)), "Use the main_mod function to specify unites for these variables.")
#  }


  data_check_function <- list()
  data_check_function$functionID <- "data_check"
  data_check_function$args <- list(dat, project, x)#, dataindex)
  data_check_function$kwargs <- list()
  data_check_function$output <- list()
  log_call(data_check_function)
}
