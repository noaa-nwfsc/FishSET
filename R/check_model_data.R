#' Check data table for common data quality issues that may affect modeling functions
#'
#' @param dat Main data frame over which to apply function. Table in FishSET database should contain the string `MainDataTable`.
#' @param dataindex Data frame that contains information on each column of the main data frame. In FishSET database the table will contain the phrase `MainDataTableInfo`
#' @param uniqueID Variable that identifies unique occurrences.
#' @param save.file TRUE/FALSE Save to FishSET database? Defaults to TRUE
#' @return Returns statements of data quality issues in the data.
#' @export check_model_data
#' @details Checks data to be used for modeling for presence of NAs, NaNs and Inf, and that each row is a unique choice occurrence. 
#'    Model functions may fail or return inaccurate results if any of these issues exist.
#'    The verified data will not save if any of these issues are in the data set. 
#'    If data passes all tests, the verified data will then be saved in the FishSET database. 
#'    The unmodified version of the table in the FishSET database (if it exists), will be saved with the prefix 'raw'.
#'    The data index table will also be updated and saved.

#' @examples 
#' \dontrun{ 
#' check_model_data(MainDataTable, 'MainDataTableInfo', uniqueID='uniqueID_Code', save.file=TRUE)
#' }


check_model_data <- function(dat, dataindex, uniqueID, save.file = TRUE) {
    x <- 0
    
    # Call in data sets
        dataset <- dat
        dat <- deparse(substitute(dat))
    
    # update dataindex
    dataindex_update(dataset, dataindex)
    
    tmp <- tempfile()
    
    if (any(apply(dataset, 2, function(x) any(is.nan(x))) == TRUE)) {
        cat("\nNaNs are present in", names(which(apply(dataset, 2, function(x) any(is.nan(x))) == TRUE)), file = tmp, append = T)
        cat("\nNaNs are present in", names(which(apply(dataset, 2, function(x) any(is.nan(x))) == TRUE)))
        x <- 1
    }
    
    if (any(apply(dataset, 2, function(x) anyNA(x)) == TRUE)) {
        cat(paste("\nNAs are present in", names(which(apply(dataset, 2, function(x) anyNA(x)) == TRUE))), file = tmp, append = T)
        cat("\nNAs are present in", names(which(apply(dataset, 2, function(x) anyNA(x)) == TRUE)))
        x <- 1
    }
    
    
    # is.inf
    if (any(apply(dataset, 2, function(x) any(is.infinite(x))) == TRUE)) {
        cat(paste("\nInfinite values are present in", names(which(apply(dataset, 2, function(x) any(is.infinite(x))) == TRUE))), file = tmp, append = T)
        
        cat("\nInfinite values are present in", names(which(apply(dataset, 2, function(x) any(is.infinite(x))) == TRUE)))
        x <- 1
    }
    
    if (length(dataset[[uniqueID]]) != length(unique(dataset[[uniqueID]]))) {
        cat("\nThe uniqueID variable should define the length of unique occurrences in the dataset. Use the haul_to_trip function to collapse data.", 
            file = tmp, append = T)
        
        cat("\nThe uniqueID variable should define the length of unique occurrences in the dataset. 
         Use the haul_to_trip function to collapse data.")
        x <- 1
    }
    
    if (x == 1) {
        suppressWarnings(readLines(tmp))
        warning("At least one test did not pass. Data set will not be saved.")
    }
    if (x == 0) {
        if (save.file == TRUE) {
            cat(paste("\nModified data set saved to fishset_db database"), file = tmp, append = T)
            fishset_db <- suppressWarnings(DBI::dbConnect(RSQLite::SQLite(), locdatabase()))
                single_sql <- paste0("final_", deparse(substitute(dat)))
                DBI::dbWriteTable(fishset_db, single_sql, dataset, overwrite = TRUE)
                DBI::dbDisconnect(fishset_db)
            # logging function information
        }
    }
    
    checkModelData_function <- list()
    checkModelData_function$functionID <- "check_model_data"
    checkModelData_function$args <- list(dat, dataindex, uniqueID, save.file)
    checkModelData_function$msg <- suppressWarnings(readLines(tmp))
    
    suppressWarnings(readLines(tmp))

    log_call(checkModelData_function)
    
    rm(tmp)
}
