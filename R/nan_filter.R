# Filters NaN's from variable


nan_identify <- function(dat, project) {
  #' Identify NaNs and NAs
  #' 
  #' Check whether any columns in the primary dataset contain NAs or NaNs.
  #' 
  #' @description Check whether any columns in the primary dataset contain NAs or NaNs. 
  #'   Returns column names containing NAs or NaNs.
  #' @param dat Primary data containing information on hauls or trips. 
  #'   Table in the FishSET database contains the string 'MainDataTable'.
  #' @param project Project name. 
  #' @keywords NaN, NA
  #' @return Returns names of columns containing NAs or NaNs, if any.
  #' @export nan_identify
  #' @seealso \code{\link{na_filter}} and  \code{\link{nan_filter}}
  #' @examples
  #' \dontrun{
  #' nan_identify(pcodMainDataTable, "pcod")
  #' }

  # Call in datasets
  out <- data_pull(dat, project)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main", project)
  
  tmp <- tempfile()
  
  # Check for NAs
  na_cols <- qaqc_helper(dataset, "NA", "names")
  
  if (length(na_cols) > 0) {
    
    cat("The following columns contain NAs: ", paste0(na_cols, collapse = ", "),
        ". Consider using na_filter to replace or remove NAs.\n", file = tmp, sep = "")

  } else {
    
    cat("No columns in the dataframe contain NAs.\n", file = tmp, append = TRUE)
  }
  
  # check for -999
  na2_cols <- qaqc_helper(dataset, function(x) any(x == -999), "names")
  
  if (length(na2_cols) > 0) {
     
     cat("The following columns contain the value -999: ", paste(na2_cols, collapse = ", "),
         ". Check that this is a valid value or stands for NA.\n", file = tmp,sep = "", 
         append = TRUE)
  }
  
  # check for NaNs
  nan_cols <- qaqc_helper(dataset, "NaN", "names")
  
  if (length(nan_cols) > 0) {
    
    cat("The following columns contain NaNs: ", paste(nan_cols, collapse = ", "),
        ". Consider using na_filter to replace or remove NaNs.\n", file = tmp, sep = "", 
        append = TRUE)

  } else {
   
    cat("No columns in the dataframe contain NaNs.\n", file = tmp, append = TRUE)
  }

  msg_print(tmp)

  nan_identify_function <- list()
  nan_identify_function$functionID <- "nan_identify"
  nan_identify_function$args <- list(dat, project)
  nan_identify_function$msg <- suppressWarnings(readLines(tmp))
  log_call(project, nan_identify_function)

  unlink(tmp)
}


nan_filter <- function(dat, project, x = NULL, replace = FALSE, remove = FALSE, 
                       rep.value = "mean", over_write = FALSE) {
  #' Identify, remove, or replace NaNs
  #'
  #' Replaces NaNs in the primary data with the chosen value or removes rows 
  #' containing NaNs
  #'
  #' @param dat Primary data containing information on hauls or trips. Table in 
  #'   the FishSET database contains the string 'MainDataTable'.
  #' @param project Project name.
  #' @param x  Character string of variables to remove or replace NaNs.
  #' @param replace Logical, If \code{TRUE}, NaNs are replaced. Defaults to \code{FALSE}.
  #' @param remove  Logical, if \code{TRUE}, removes the entire row of the dataset 
  #'   where NaN is present. Defaults to \code{FALSE}.
  #' @param rep.value Value to replace all NaNs in a numeric column. Defaults to 
  #'   the mean value of the column. Other options include \code{"median"} or a
  #'   numeric value, e.g. \code{rep.value = 0}. 
  #' @param over_write Logical, If \code{TRUE}, saves data over previously saved 
  #'   data table in the FishSET database. Defaults to \code{FALSE}.
  #' @details To check for NaNs across \code{dat} run the function specifying only 
  #'   \code{dat} (\code{nan_filter(dataset, project)}). The function will return 
  #'   a statement of which variables, if any, contain NaNs. To remove NaNs, use 
  #'   \code{remove = TRUE}. All rows containing NaNs in \code{x} will be removed 
  #'   from \code{dat}. To replace NaNs, use \code{replace = TRUE}. If both 
  #'   \code{replace} and \code{remove} are \code{TRUE} then \code{replace} is used. 
  #'   If \code{replace} is \code{FALSE} and \code{rep.value} is not defined, then 
  #'   NaNs are replaced  with mean value. The modified dataset will be returned 
  #'   if \code{replace = TRUE} or \code{remove = TRUE}. Save the modified data 
  #'   table to the FishSET database by setting \code{over_write = TRUE)}.
  #' @keywords NaN
  #' @return If \code{replace} and \code{remove} are \code{FALSE} then a statement 
  #' of whether NaNs are found is returned. If either \code{replace} or \code{remove} 
  #' is \code{TRUE} the modified primary dataset is returned.
  #' @importFrom shiny isRunning
  #' @export nan_filter
  #' @examples
  #' \dontrun{
  #' nan_filter(pcodMainDataTable, 'pcod', 'OFFICIAL_TOTAL_CATCH_MT')
  #' 
  #' mod.dat <- nan_filter(pcodMainDataTable, 'pcod', 'OFFICIAL_TOTAL_CATCH_MT', 
  #'                       replace = TRUE)
  #'                       
  #' mod.dat <- nan_filter(pcodMainDataTable, 'pcod', 'OFFICIAL_TOTAL_CATCH_MT',
  #'                       replace = TRUE, rep.value = 0)
  #'                       
  #' mod.dat <- nan_filter(pcodMainDataTable, 'pcod', 'OFFICIAL_TOTAL_CATCH_MT', 
  #'                       remove = TRUE)
  #' }

  # Call in datasets
  out <- data_pull(dat, project)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main", project)
  
  tmp <- tempfile()
  
  nan_cols <- qaqc_helper(dataset, "NaN", "names")
  
  if (length(nan_cols) == 0) {
    
    cat("No NaNs found.", file = tmp)
    
  } else {
    
    cat("The following columns contain NaNs: ", paste0(nan_cols, collapse = ", "),
        ". Consider using nan_filter to replace or remove NaNs.\n", file = tmp, sep = "")
    
    if (!is.null(x)) { 
      
      x_ind <- x %in% nan_cols # check that all vars in x have NaNs
      x_nan <- x[x_ind]
      
      if (any(!x_ind)) {
        
        warning(paste(x[!x_ind], collapse = ", "), " do not contain NaNs.", call. = FALSE) 
      }
      
      if (length(x_nan) > 0) {
        
        if (replace) {
          
          dataset[x_nan] <- lapply(x_nan, function(i) {
            
          if (is.numeric(dataset[[i]])) {
              mean_function <- mean
              if (rep.value == "mean") {
                rep.value <- do.call(mean_function, list(dataset[[i]], na.rm = TRUE))
              }
              
              # # TODO: extend rep.value to non-numeric vars
               if (!is.numeric(rep.value)) {
                
                stop("Replacement value must be numeric.", call. = FALSE)
              }
              
              out <- dataset[[i]]
              out[is.nan(out)] <- rep.value
              cat("All NaNs in", i, "have been replaced with", rep.value, "\n", 
                  file = tmp, append = TRUE)
              
              out
              
            } else {
              
              cat("Variable is not numeric. Function not applied.\n", file = tmp, 
                  append = TRUE)
              
              dataset[[i]]
            }
          })
          
        } else if (remove) {
          
          nan_ind <- lapply(dataset[x_nan], function(i) which(is.nan(i)))
          nan_ind <- sort(unique(unlist(nan_ind)))
          dataset <- dataset[-nan_ind, ]
          
          cat("All rows containing NaNs have been removed from the dataframe.\n", file = tmp, 
              append = TRUE)
        }
      }
      
      if (over_write == TRUE) {
        suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), 
                                                      locdatabase(project = project)))
        on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
        
        DBI::dbWriteTable(fishset_db, dat, dataset, overwrite = over_write)
      }
    }
  }
  
  msg_print(tmp)
  
    
  # Read the messages from the temp file into a vector
  messages_vector <- suppressWarnings(readLines(tmp))
  
  # Attach this vector as an attribute to the dataframe
  attr(dataset, "messages") <- messages_vector
  
  nan_filter_function <- list()
  nan_filter_function$functionID <- "nan_filter"
  nan_filter_function$args <- list(dat, project, x, replace, remove, rep.value, 
                                   over_write)
  nan_filter_function$output <- list(dat)
  nan_filter_function$msg <- suppressWarnings(readLines(tmp))
  log_call(project, nan_filter_function)
  

    return(dataset)
  
}


na_filter <- function(dat, project, x = NULL, replace = FALSE, remove = FALSE, 
                      rep.value = "mean", over_write = FALSE) {
  #' Identify, remove, or replace NAs and NaNs
  #' 
  #' Replaces NAs and NaNs in the primary data with the chosen value or removes rows 
  #' containing NAs and NaNs.
  #'
  #' @param dat Primary data containing information on hauls or trips. Table in 
  #'   FishSET database contains the string 'MainDataTable'.
  #' @param project Project name. 
  #' @param x Character string. Column(s) in \code{dat} in which to remove or replace NAs.
  #' @param replace Logical, if \code{TRUE}, replaces NAs in a vector with \code{rep.value}. 
  #'   Defaults to \code{FALSE}.
  #' @param remove Logical, if \code{TRUE} removes the entire row of the \code{dat} where 
  #'   NA is present in a \code{dat}. Defaults to \code{FALSE}.
  #' @param rep.value Value to replace all NAs in a numeric column. Defaults to 
  #'   the mean value of the column. Other options include \code{"median"} or a
  #'   numeric value, e.g. \code{rep.value = 0}. 
  #' @param over_write Logical, If \code{TRUE}, saves data over previously saved 
  #'   data table in the FishSET database.
  #' @details To check for NAs across \code{dat} run the function specifying only 
  #'   \code{dat} (\code{na_filter(dataset, project)}). The function will return a statement 
  #'   of which variables, if any, contain NAs. To remove NAs, use \code{remove = TRUE}. 
  #'   All rows containing NAs in \code{x} will be removed from \code{dat}. To replace 
  #'   NAs, use \code{replace = TRUE}. If \code{replace = FALSE} and \code{rep.value} 
  #'   is not defined, then NAs are replaced with mean value. The modified dataset will
  #'   be returned if \code{replace = TRUE} or \code{remove = TRUE}. If both 
  #'   \code{replace} and \code{remove} are \code{TRUE} then \code{replace} is used. 
  #'   Save the modified data table to the FishSET database by setting 
  #'   \code{over_write = TRUE)}.
  #' @keywords NA
  #' @return If \code{replace} and \code{remove} are \code{FALSE} then a statement 
  #' of whether NAs are found is returned. If either \code{replace} or \code{remove} 
  #' is \code{TRUE} the modified primary dataset is returned.
  #' @export na_filter
  #' @examples
  #' \dontrun{
  #' na_filter(pcodMainDataTable, 'pcod', 'OFFICIAL_TOTAL_CATCH_MT')
  #' 
  #' mod.dat <- na_filter(pcodMainDataTable, 'pcod', 'OFFICIAL_TOTAL_CATCH_MT', 
  #'                      replace = TRUE)
  #'                      
  #' mod.dat <- na_filter(pcodMainDataTable,'pcod', 'OFFICIAL_TOTAL_CATCH_MT',
  #'                      replace = TRUE, rep.value = 0)
  #'                      
  #' mod.dat <- na_filter(pcodMainDataTable, 'pcod',
  #'                      c('OFFICIAL_TOTAL_CATCH_MT', 'CATCH_VALUE'), 
  #'                      remove = TRUE)
  #' }
  #' 
  # Call in datasets
  out <- data_pull(dat, project)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main", project)
  
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  
  na_cols <- qaqc_helper(dataset, "NA", "names")
  
  if (length(na_cols) == 0) {
    
    cat("No NAs found.", file = tmp)
    
  } else {
    
    cat("The following columns contain NAs: ", paste0(na_cols, collapse = ", "),
        ". Consider using na_filter to replace or remove NAs.\n", sep = "", file = tmp)
    
    if (!is.null(x)) { 
      
      x_ind <- x %in% na_cols # check that all vars in x have NAs
      x_na <- x[x_ind]
      
      # avoid removing all rows by excluding empty variables
      x_empty <- qaqc_helper(dataset[x_na], function(i) all(is.na(i)), "logical")
      x_na <- x_na[!x_empty]
      
      if (any(x_empty)) {
        
        cat("The following variables are empty (contain all NAs): ",
                paste(x[x_empty], collapse = ", "), 
                ". Use empty_vars_filter() to remove empty variables.", file = tmp, 
                  append = TRUE)
      }
      
      if (any(!x_ind)) {
        
        cat(paste(x[!x_ind], collapse = ", "), " do not contain NAs.", file = tmp, 
                  append = TRUE) 
      }
      
      if (length(x_na) > 0) {
        
        if (replace) {
          
          dataset[x_na] <- lapply(x_na, function(i) {
            
               if (is.numeric(dataset[[i]])) {
              mean_function <- mean
              if (rep.value == "mean") {
                rep.value <- do.call(mean_function, list(dataset[[i]], na.rm = TRUE))
              }
              
             
              if (!is.numeric(rep.value)) {
                
                stop("Replacement value must be numeric.", call. = FALSE)
              }
              out <- dataset[[i]]
              out[is.na(out)] <- rep.value
              cat("All NAs in", i, "have been replaced with", rep.value, "\n", 
                  file = tmp, append = TRUE)
              
              out
            } else {
              
              cat("Variable is not numeric. Function not applied.\n", file = tmp, 
                  append = TRUE)
              
              dataset[[i]]
            }
          })
          
        } else if (remove) {
          
          na_ind <- lapply(dataset[x_na], function(i) which(is.na(i)))
          na_ind <- sort(unique(unlist(na_ind)))
          dataset <- dataset[-na_ind, ]
          
          cat("All rows containing NAs have been removed from the dataframe.\n", file = tmp, 
              append = TRUE)
        }
      }
      
      if (over_write == TRUE) {
        
        suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), 
                                                      locdatabase(project = project)))
        on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
        DBI::dbWriteTable(fishset_db, dat, dataset, overwrite = TRUE)
      }
    }
  }
  
  msg_print(tmp)
  
  # Read the messages from the temp file into a vector
  messages_vector <- suppressWarnings(readLines(tmp))
  
  # Attach this vector as an attribute to the dataframe
  attr(dataset, "messages") <- messages_vector
  
  na_filter_function <- list()
  na_filter_function$functionID <- "na_filter"
  na_filter_function$args <- list(dat, project, x, replace, remove, rep.value, 
                                  over_write)
  na_filter_function$output <- list(dat)
  na_filter_function$msg <- suppressWarnings(readLines(tmp))
  log_call(project, na_filter_function)
  

  

    return(dataset)

}
