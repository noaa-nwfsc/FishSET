# Filters NaN's from variable


nan_identify <- function(dat) {
  #' Check whether any columns in the primary dataset contain NAs or NaNs.
  #' 
  #' @description Check whether any columns in the primary dataset contain NAs or NaNs. 
  #'   Returns column names containing NAs or NaNs.
  #' @param dat Primary data containing information on hauls or trips. 
  #'   Table in the FishSET database contains the string 'MainDataTable'.
  #' @keywords NaN, NA
  #' @return Returns names of columns containing NAs or NaNs, if any.
  #' @export nan_identify
  #' @examples
  #' \dontrun{
  #' nan_identify(pcodMainDataTable)
  #' }

  # Call in datasets
  out <- data_pull(dat)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main")
  
  tmp <- tempfile()
  # Check for NAs
  if (any(apply(dataset, 2, function(x) anyNA(x)) == TRUE)) {
    cat("The", names(which(apply(dataset, 2, function(x) anyNA(x)) == TRUE)), "columns contain NAs. Consider using na_filter to replace or remove NAs",
      file = tmp
    )
  } else {
    # cat('No columns in the dataframe contain NaNs', file=paste(getwd(),'/Logs/InforMessage',Sys.Date(),'.txt', sep=''), append=TRUE)
    cat("No columns in the dataframe contain NAs", file = tmp, append = TRUE)
  }

  
  # check for -999
  if(any(apply(dataset, 2, function(x) x == -999))){
    cat('The', names(which(apply(dataset, 2, function(x) any(x == -999)))), 'variable has -999. 
        Check that this is a valid value or stands for NA.', file = tmp, append = TRUE)
  }
  # check for NaNs
  if (any(unlist(lapply(dataset, function(x) any(is.nan(x))))) == TRUE) {
    # cat('The', names(which(colSums(is.nan.data.frame(dataset)) != 0)), 'columns contain', unname(which(colSums(is.nan.data.frame(dataset)) != 0)),
    # 'NaNs. Consider using nan.filter to replace or remove NaNs', file=paste(getwd(),'/Logs/InforMessage',Sys.Date(),'.txt', sep=''), append=TRUE)
    cat("The", names(which(unlist(lapply(dataset, function(x) any(is.nan(x)))) == TRUE)), "columns contain NaNs. Consider using nan_filter to replace or remove NaNs",
      file = tmp, append = TRUE
    )
  } else {
    # cat('No columns in the dataframe contain NaNs', file=paste(getwd(),'/Logs/InforMessage',Sys.Date(),'.txt', sep=''), append=TRUE)
    cat("No columns in the dataframe contain NaNs", file = tmp, append = TRUE)
  }


  print(suppressWarnings(readLines(tmp)))

  nan_identify_function <- list()
  nan_identify_function$functionID <- "nan_identify"
  nan_identify_function$args <- list(dat)
  nan_identify_function$msg <- suppressWarnings(readLines(tmp))
  log_call(nan_identify_function)

  rm(tmp)
}


# Replaces nans in the data column with the chosen value or removes rows containing NaNs
nan_filter <- function(dat, x=NULL, replace = F, remove = F, rep.value = NA, over_write = FALSE) {
  #' Remove NaNs
  #'
  #' Remove or replace NaNs in primary dataset.
  #'
  #' @param dat Primary data containing information on hauls or trips. Table in the FishSET database contains the string 'MainDataTable'.
  #' @param x  Character string of variables to remove or replace NaNs.
  #' @param replace Logical, If TRUE, NaNs are replaced. Defaults to FALSE.
  #' @param remove  Logical, if TRUE, removes the entire row of the dataset where NaN is present? Defaults to FALSE.
  #' @param rep.value Value to replace all NaNs in a column. Defaults to the mean value of the column.
  #' @param over_write Logical, If TRUE, saves data over previously saved data table in the FishSET database.
  #' @details To check for NaNs across \code{dat} run the function specifying only \code{dat} (\code{nan_filter(dataset)}). 
  #' The function will return a statement of which variables, if any, contain NaNs. 
  #' To remove NaNs, use \code{remove = TRUE}. All rows containing NAs in \code{x} will be removed from \code{dat}. 
  #' To replace NaNs, use \code{replace = TRUE}. If \code{replace} is FALSE and \code{rep.value} is not defined, 
  #' then NaNs are replaced with mean value. The modified dataset will be returned if \code{replace=TRUE} or 
  #' \code{remove=TRUE}. Save the modified data table to the FishSET database by setting \code{over_write = TRUE)}.
  #' @keywords NaN
  #' @return Returns the modified primary dataset.
  #' @export nan_filter
  #' @examples
  #' \dontrun{
  #' nan_identify(pcodMainDataTable)
  #' mod.dat <- nan_filter(pcodMainDataTable, 'OFFICIAL_TOTAL_CATCH_MT')
  #' mod.dat <- nan_filter(pcodMainDataTable, 'OFFICIAL_TOTAL_CATCH_MT', replace=T)
  #' mod.dat <- nan_filter(pcodMainDataTable, 'OFFICIAL_TOTAL_CATCH_MT', replace=T, rep.value=0)
  #' mod.dat <- nan_filter(pcodMainDataTable, 'OFFICIAL_TOTAL_CATCH_MT', remove=T)
  #' }#


  # Call in datasets
  out <- data_pull(dat)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main")
  

  int <- dataset
  tmp <- tempfile()
  
  if(is.null(x)){
    if (any(apply(dataset, 2, function(g) any(is.na(g)) == TRUE))) {
      cat("The", names(which(apply(dataset, 2, function(g) any(is.nan(g)) == TRUE))), "columns contain NaNs. Consider using nan_filter to replace or remove NaNs",
          file = tmp
      )
    } else {
      cat("No NaNs found.", file=tmp)
    }
  } else {
  if (any(unlist(lapply(dataset, function(x) any(is.nan(x))))) == TRUE) {
    cat("The", names(which(unlist(lapply(dataset, function(x) any(is.nan(x)))) == TRUE)), "columns contain NaNs. Consider using nan_filter to replace or remove NaNs",
      file = tmp
    )
    x <- names(which(unlist(lapply(dataset, function(x) any(is.nan(x)))) == TRUE))
    for (i in 1:length(x)) {
      x.name <- x[i]

      # the identified rep.value (defaults to mean value)
      if (replace == T) {
        if (is.na(rep.value) == TRUE) {
          rep.value <- mean(int[, x.name], na.rm = T)
        }
        if (is.numeric(int[, x.name]) == T) {
          # Further actions are only taken if NaNs exist in the selected variable
          int[is.nan(int[, x.name]), x.name] <- rep.value
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

  if (over_write == TRUE) {
    suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase()))
    DBI::dbWriteTable(fishset_db, dat, int, overwrite = over_write)
    DBI::dbDisconnect(fishset_db)
  }
  }
  
  nan_filter_function <- list()
  nan_filter_function$functionID <- "nan_filter"
  nan_filter_function$args <- list(dat, x, replace, remove, rep.value, over_write)
  nan_filter_function$output <- list(dat)
  nan_filter_function$msg <- suppressWarnings(readLines(tmp))
  log_call(nan_filter_function)

  rm(tmp)
  if (remove == TRUE | replace == T) {
    return(int)
  }
}


#### ----
# Replaces NAs in the dataColumn with the chosen value or removes rows containing NAs
na_filter <- function(dat, x=NULL, replace = F, remove = F, rep.value = NA, over_write = FALSE) {
  #' Identify, remove or replace NAs 
  #'
  #' @param dat Primary data containing information on hauls or trips. Table in FishSET database contains the string 'MainDataTable'.
  #' @param x Character string. Column(s) in \code{dat} in which to remove or replace NAs.
  #' @param replace Logical, if TRUE, replaces NAs in a vector with \code{rep.value}. Defaults to FALSE.
  #' @param remove Logical, if TRUE removes the entire row of the \code{dat} where NA is present in a \code{dat}. Defaults to FALSE.
  #' @param rep.value Value to replace all NAs in a column. Defaults to the mean value of the column.
  #' @param over_write Logical, If TRUE, saves data over previously saved data table in the FishSET database.
  #' @details To check for NAs across \code{dat} run the function specifying only \code{dat} (\code{na_filter(dataset)}). 
  #' The function will return a statement of which variables, if any, contain NAs. 
  #' To remove NAs, use \code{remove = TRUE}. All rows containing NAs in \code{x} will be removed from \code{dat}. 
  #' To replace NAs, use \code{replace = TRUE}. If \code{replace} is FALSE and \code{rep.value} is not defined, 
  #' then NAs are replaced with mean value. The modified dataset will be returned if \code{replace=TRUE} or 
  #' \code{remove=TRUE}. Save the modified data table to the FishSET database by setting \code{over_write = TRUE)}.
  #' @keywords NA
  #' @return Returns a statement on whether NAs are found and the modified primary dataset.
  #' @export na_filter
  #' @examples
  #' \dontrun{
  #' na_filter(pcodMainDataTable)
  #' mod.dat <- na_filter(pcodMainDataTable, 'OFFICIAL_TOTAL_CATCH_MT')
  #' mod.dat <- na_filter(pcodMainDataTable, 'OFFICIAL_TOTAL_CATCH_MT', replace=T)
  #' mod.dat <- na_filter(pcodMainDataTable, 'OFFICIAL_TOTAL_CATCH_MT', replace=T, rep.value=0)
  #' mod.dat <- na_filter(pcodMainDataTable, c('OFFICIAL_TOTAL_CATCH_MT', 'CATCH_VALUE'), remove=T)
  #' }

  # Call in datasets
  out <- data_pull(dat)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main")
  

  int <- dataset
  tmp <- tempfile()
  
  if(is.null(x)){
  if (any(apply(dataset, 2, function(g) anyNA(g)) == TRUE)) {
    cat("The", names(which(apply(dataset, 2, function(g) anyNA(g)) == TRUE)), "columns contain NAs. Consider using na_filter to replace or remove NAs",
        file = tmp
    )
  } else {
   cat("No NAs found.", file=tmp)
  }
  } else {

  if ((!is_empty(names(which(apply(dataset, 2, function(x) anyNA(x)) == TRUE))) && any(apply(dataset, 2, function(x) anyNA(x)) == TRUE)) == TRUE) {
    cat("The", names(which(apply(dataset, 2, function(x) anyNA(x)) == TRUE)), "columns contain NAs. Consider using na_filter to replace or remove NAs",
      file = tmp
    )
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
          int[is.na(int[, x.name]), x.name] <- rep.value
          cat("All NAs in", x.name, "have been replaced with", rep.value, file = tmp, append = T)
        } else {
          # Message returned if the selected variable is not numeric
          cat("Variable is not numeric. Function not applied", file = tmp, append = T)
        }

        # If remove is true then row in which the NA occurs for selected column will be removed.
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
  if (over_write == TRUE) {
    suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase()))
    DBI::dbWriteTable(fishset_db, dat, int, overwrite = over_write)
    DBI::dbDisconnect(fishset_db)
    print("Data saved to database")
  }
}
  na_filter_function <- list()
  na_filter_function$functionID <- "na_filter"
  na_filter_function$args <- list(dat, x, replace, remove, rep.value, over_write)
  na_filter_function$output <- list(dat)
  na_filter_function$msg <- suppressWarnings(readLines(tmp))
  log_call(na_filter_function)

  rm(tmp)

  if (remove == TRUE | replace == T) {
    return(int)
  }
}
