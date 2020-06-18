#' Check for common data quality issues affecting modeling functions
#'
#' @param dat Primary data containing information on hauls or trips. Table in FishSET database contains the string 'MainDataTable'.
#' @param dataindex Data table that contains information on each column of the main data frame. In the FishSET database the table will contain the phrase `MainDataTableInfo`
#' @param uniqueID Variable in \code{dat} containing unique occurrence identifier.
#' @param save.file Logical, if TRUE and no data issues are identified, the dataset is saved to the FishSET database. Defaults to TRUE.
#' @return Returns statements of data quality issues in the data. Saves table to FishSET database.
#' @export check_model_data
#' @description Check the primary dataset for NAs, NaNs, Inf, and that each row is a unique choice occurrence 
#' @details It is best to check the data for NAs, NaNs and Inf, and that each row is a unique choice occurrence after data creation functions 
#' have been run but before making the model design file (\code{make_model_design}), even if the data passed earlier data verification checks 
#' as data quality issues can arise in the creation or modification of data. Model functions may fail or return inaccurate results if data 
#' quality issues exist. The integrated data will not save if any of these issues are in the dataset. If data passes all tests, the data 
#' will then be saved in the FishSET database with the prefix ‘final’. The data index table will also be updated and saved.
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
