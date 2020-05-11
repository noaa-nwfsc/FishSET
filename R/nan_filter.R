# Filters NaN's from variable


nan_identify <- function(dat) {
    #' Identify NaN's and NAs in data set
    #'
    #' @param dat Main data frame over which to apply function. Table in FishSET database should contain the string `MainDataTable`.
    #' @keywords NaN, NA
    #' @description Check whether any columns in the data frame contain NAs or NaNs. Returns column names containing NAs or NaNs.
    #' @return Message with names of columns containing NAs or NaNs, if any.
    #' @export nan_identify
    #' @examples 
    #' \dontrun{
    #' nan_identify(pcodMainDataTable)
    #' mod.dat <-nan_filter('pcodMainDataTable', 'OFFICIAL_TOTAL_CATCH_MT') 
    #' mod.dat <- nan_filter(pcodMainDataTable, 'OFFICIAL_TOTAL_CATCH_MT', replace=T) 
    #' mod.dat <- nan_filter(pcodMainDataTable, 'OFFICIAL_TOTAL_CATCH_MT', replace=T, rep.value=0) 
    #' mod.dat <- nan_filter(pcodMainDataTable, 'OFFICIAL_TOTAL_CATCH_MT', remove=T) 
    #' }
    
    # Call in datasets
    out <- data_pull(dat)
    dat <- out$dat
    dataset <- out$dataset
    
    tmp <- tempfile()
    # Check for NAs
    if (any(apply(dataset, 2, function(x) anyNA(x)) == TRUE)) {
        cat("The", names(which(apply(dataset, 2, function(x) anyNA(x)) == TRUE)), "columns contain NAs. Consider using nan_filter to replace or remove NAs", 
            file = tmp)
        
    } else {
        # cat('No columns in the dataframe contain NaNs', file=paste(getwd(),'/Logs/InforMessage',Sys.Date(),'.txt', sep=''), append=TRUE)
        cat("No columns in the dataframe contain NAs", file = tmp, append = TRUE)
    }
    
    # check for NaNs
    if (any(apply(dataset, 2, function(x) any(is.nan(x))) == TRUE)) {
        # cat('The', names(which(colSums(is.nan.data.frame(dataset)) != 0)), 'columns contain', unname(which(colSums(is.nan.data.frame(dataset)) != 0)),
        # 'NaNs. Consider using nan.filter to replace or remove NaNs', file=paste(getwd(),'/Logs/InforMessage',Sys.Date(),'.txt', sep=''), append=TRUE)
        cat("The", names(which(apply(dataset, 2, function(x) any(is.nan(x))) == TRUE)), "columns contain NaNs. Consider using nan_filter to replace or remove NaNs", 
            file = tmp, append = TRUE)
        
    } else {
        # cat('No columns in the dataframe contain NaNs', file=paste(getwd(),'/Logs/InforMessage',Sys.Date(),'.txt', sep=''), append=TRUE)
        cat("No columns in the dataframe contain NaNs", file = tmp, append = TRUE)
    }
    
    
    print(suppressWarnings(readLines(tmp)))
    nan_identify_function <- list()
    nan_identify_function$functionID <- "nan_identify"
    nan_identify_function$args <- c(dat)
    nan_identify_function$kwargs <- list()
    nan_identify_function$output <- c("")
    nan_identify_function$msg <- suppressWarnings(readLines(tmp))
    log_call(nan_identify_function)
    
    rm(tmp)
}


# Replaces nans in the data column with the choosen value or removes rows containing NaNs
nan_filter <- function(dat, x, replace = F, remove = F, rep.value = NA, over_write = FALSE) {
    #' Filters NaN's from data frame
    #'
    #' @param dat Main data frame over which to apply function. Table in FishSET database should contain the string `MainDataTable`.
    #' @param x Column(s) in data framce in which to remove or replace NaNs. If multiple columns are passed use x=c().
    #' @param replace TRUE/FALSE Replace NaNs in a vector? Defaults to FALSE.
    #' @param remove TRUE/FALSE Remove all remove the entire row of the dataframe where NaN is present in a specified column? Defaults to FALSE.
    #' @param rep.value Value to replace all NaNs in a column. Defaults to the mean value of the column.
    #' @param over_write Over_write modified data set in FishSET database?
    #' @details Replaces nans in the data column with the chosen value or removes rows containing NaNs. Modified data frame saved to FishSET database.
    #' @keywords NaN
    #' @return Returns the modified data frame
    #' @export nan_filter
    #' @examples 
    #' \dontrun{
    #' nan_identify(pcodMainDataTable)
    #' mod.dat <-nan_filter(pcodMainDataTable, 'OFFICIAL_TOTAL_CATCH_MT') 
    #' mod.dat <- nan_filter(pcodMainDataTable, 'OFFICIAL_TOTAL_CATCH_MT', replace=T) 
    #' mod.dat <- nan_filter(pcodMainDataTable, 'OFFICIAL_TOTAL_CATCH_MT', replace=T, rep.value=0) 
    #' mod.dat <- nan_filter(pcodMainDataTable, 'OFFICIAL_TOTAL_CATCH_MT', remove=T) 
    #' }#
    
    
    # Call in datasets
    out <- data_pull(dat)
    dat <- out$dat
    dataset <- out$dataset
    
    int <- dataset
    tmp <- tempfile()
    
    if (any(apply(dataset, 2, function(x) any(is.nan(x))) == TRUE)) {
        cat("The", names(which(apply(dataset, 2, function(x) any(is.nan(x))) == TRUE)), "columns contain NaNs. Consider using nan_filter to replace or remove NaNs", 
            file = tmp)
        x <- names(which(apply(dataset, 2, function(x) any(is.nan(x))) == TRUE))
        for (i in 1:length(x)) {
            x.name <- x[i]
            
            # the identified rep.value (defaults to mean value)
            if (replace == T) {
                if (is.nan(rep.value) == TRUE) {
                  rep.value <- mean(int[, x.name], na.rm = T)
                }
                if (is.numeric(int[, x.name]) == T) {
                  # Further actions are only taken if NaNs exist in the selected variable
                  int[is.nan(int[, x.name]), x.name] = rep.value
                  cat("All NaNs in", x.name, "have been replaced with", rep.value, file = tmp, append = T)
                } else {
                  # Message returned if the selected variable is not numeric
                  cat("Variable is not numeric. Function not applied", file = tmp, append = T)
                }
                
                # If remove is true then row inwhich the NaN occurs for selected column will be removed.
            } else if (remove == T) {
                cat("The entire row will be removed from the dataframe.", file = tmp, append = T)
                int <- int[!is.nan(int[, x.name]), ]
            }
        }
    } else {
        cat("No NaNs present", file = tmp)
    }
    
    
    print(suppressWarnings(readLines(tmp)))
    
    if (over_write == TRUE & any(apply(dataset, 2, function(x) any(is.nan(x)))) == TRUE) {
        suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase()))
        DBI::dbWriteTable(fishset_db, deparse(substitute(dat)), int, overwrite = over_write)
        DBI::dbDisconnect(fishset_db)
    }
    
    nan_filter_function <- list()
    nan_filter_function$functionID <- "nan_filter"
    nan_filter_function$args <- c(dat, x, replace, remove, rep.value)
    nan_filter_function$kwargs <- list()
    nan_filter_function$output <- c(dat)
    nan_filter_function$msg <- suppressWarnings(readLines(tmp))
    log_call(nan_filter_function)
    
    rm(tmp)
    if (remove == TRUE | replace == T) {
        return(int)
    }
}


####----
# Replaces nans in the dataColumn with the choosen value or removes rows containing NaNs
na_filter <- function(dat, x, replace = F, remove = F, rep.value = NA, over_write = FALSE) {
    #' Filters NA's from variable
    #'
    #'  Function to return a modified dataframe where NAs have been replaced or removed.
    #'
    #' @param dat Main data frame over which to apply function. Table in FishSET database should contain the string `MainDataTable`.
    #' @param x Column(s) in data frame in which to remove or replace NAs. If multiple columns are passed use x=c().
    #' @param replace TRUE/FALSE Replace NAs in a vector? Defaults to FALSE.
    #' @param remove TRUE/FALSE Remove all remove the entire row of the dataframe where NA is present in a specified column? Defaults to FALSE.
    #' @param rep.value Value to replace all NAs in a column. Defaults to the mean value of the column.
    #' @param over_write Over_write modified data set in FishSET database?
    #' @details Function to return a modified dataframe where NAs have been replaced or removed. Modified data frame saved to FishSET database.
    #' @keywords NA
    #' @return Returns the modified dataframe
    #' @export na_filter
    #' @examples 
    #' \dontrun{
    #' nan_identify(pcodMainDataTable)
    #' mod.dat <- na_filter(pcodMainDataTable, 'OFFICIAL_TOTAL_CATCH_MT') 
    #' mod.dat <- na_filter(pcodMainDataTable, 'OFFICIAL_TOTAL_CATCH_MT', replace=T) 
    #' mod.dat <- na_filter(pcodMainDataTable, 'OFFICIAL_TOTAL_CATCH_MT', replace=T, rep.value=0) 
    #' mod.dat <- na_filter(pcodMainDataTable, 'OFFICIAL_TOTAL_CATCH_MT', remove=T) 
    #' }
    
    # Call in datasets
    out <- data_pull(dat)
    dat <- out$dat
    dataset <- out$dataset
    
    int <- dataset
    tmp <- tempfile()
    
    if ((!is_empty(names(which(apply(dataset, 2, function(x) anyNA(x)) == TRUE))) && any(apply(dataset, 2, function(x) anyNA(x)) == TRUE)) == TRUE) {
        cat("The", names(which(apply(dataset, 2, function(x) anyNA(x)) == TRUE)), "columns contain NAs. Consider using na_filter to replace or remove NAs", 
            file = tmp)
        # x <- names(which(apply(dataset, 2, function(x) anyNA(x))==TRUE))
        
        for (i in 1:length(x)) {
            x.name <- x[i]
            
            # the identified rep.value (defaults to mean value)
            if (replace == T) {
                if (is.na(rep.value) == TRUE) {
                  rep.value <- mean(int[, x.name], na.rm = T)
                }
                if (is.numeric(int[, x.name]) == T) {
                  # Further actions are only taken if NAs exist in the selected variable
                  int[is.na(int[, x.name]), x.name] = rep.value
                  cat("All NAs in", x.name, "have been replaced with", rep.value, file = tmp, append = T)
                } else {
                  # Message returned if the selected variable is not numeric
                  cat("Variable is not numeric. Function not applied", file = tmp, append = T)
                }
                
                # If remove is true then row inwhich the NaN occurs for selected column will be removed.
            } else if (remove == T) {
                cat("The entire row will be removed from the dataframe.", file = tmp, append = T)
                int <- int[!is.na(int[, x.name]), ]
            }
        }
    } else {
        cat("No NAs present in selections", file = tmp)
    }
    
    print(suppressWarnings(readLines(tmp)))
    
    # Save the revised data set
    if (over_write == TRUE & anyNA(dataset) == TRUE) {
        suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase()))
        DBI::dbWriteTable(fishset_db, deparse(substitute(dat)), int, overwrite = over_write)
        DBI::dbDisconnect(fishset_db)
        print("Data saved to database")
    }
    
    na_filter_function <- list()
    na_filter_function$functionID <- "na_filter"
    na_filter_function$args <- c(dat, x, replace, remove, rep.value)
    na_filter_function$kwargs <- list()
    na_filter_function$output <- c(dat)
    na_filter_function$msg <- suppressWarnings(readLines(tmp))
    log_call(na_filter_function)
    
    rm(tmp)
    
    if (remove == TRUE | replace == T) {
        return(int)
    }
}

