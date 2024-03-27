#' Check for common data quality issues affecting modeling functions
#'
#' @param dat Primary data containing information on hauls or trips. Table in 
#'   FishSET database contains the string 'MainDataTable'.
#' @param project Project name. 
#' @param uniqueID Variable in \code{dat} containing unique occurrence identifier.
#' @param save.file Logical, if TRUE and no data issues are identified, the dataset 
#'   is saved to the FishSET database. Defaults to \code{TRUE}.
#' @return Returns statements of data quality issues in the data. Saves table to 
#'   FishSET database.
#' @export check_model_data
#' @description Check the primary dataset for NAs, NaNs, Inf, and that each row 
#'   is a unique choice occurrence
#' @details It is best to check the data for NAs, NaNs and Inf, and that each row 
#'   is a unique choice occurrence after data creation functions have been run but 
#'   before making the model design file (\code{make_model_design}). These steps 
#'   should be taken even if the data passed earlier data verification checks, as 
#'   data quality issues can arise in the creation or modification of data. Model 
#'   functions may fail or return inaccurate results if data quality issues exist. 
#'   The integrated data will not save if any of these issues are in the dataset. 
#'   If data passes all tests, then data will be saved in the FishSET database with 
#'   the prefix ‘final’. The data index table will also be updated and saved.
#' @examples
#' \dontrun{
#' check_model_data(MainDataTable, uniqueID = "uniqueID_Code", save.file = TRUE)
#' }
#'
check_model_data <- function(dat, project, uniqueID, save.file = TRUE) {
  
  end <- FALSE

  # Call in data sets
  out <- data_pull(dat, project)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main", project)
  
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  
  if (any(qaqc_helper(dataset, "NaN"))) {
    
    nan_cols <- qaqc_helper(dataset, "NaN", "names")
    nan_msg <- paste("\nNaNs are present in", paste(nan_cols, collapse = ", "))
    cat(nan_msg, file = tmp, append = TRUE)
    warning(nan_msg)
    end <- TRUE
  }

  if (any(qaqc_helper(dataset, "NA"))) {
    
    na_cols <- qaqc_helper(dataset, "NA", "names")
    na_msg <- paste("\nNAs are present in", paste(na_cols, collapse = ", "))
    cat(na_msg, file = tmp, append = TRUE)
    warning(na_msg)
    end <- TRUE
    
  }

  # is.inf
  if (any(qaqc_helper(dataset, "Inf"))) {
    
    inf_cols <- qaqc_helper(dataset, "Inf", "names")
    inf_msg <- paste("\nInfinite values are present in", paste(inf_cols, collapse = ", "))
    cat(inf_msg, file = tmp, append = TRUE)
    warning(inf_msg)
    end <- TRUE
  }

  # unique rows
  # TODO: Make this work for trip and haul level, multiple ID cols (switch to dplyr::distict)
  if (length(dataset[[uniqueID]]) != length(unique(dataset[[uniqueID]]))) {
    
    unique_msg <- paste("\nThe uniqueID variable should define the length of unique",
                        "occurrences in the dataset. Use the haul_to_trip function to collapse data.")
    cat(unique_msg, file = tmp, append = TRUE)
    warning(unique_msg)
    end <- TRUE
  }
  
  # lat/lon degree format
  # TODO: omit variables with NAs otherwise this will break 
  lat_lon <- grep("lat|lon", names(dataset), ignore.case = TRUE)
  
  ## NEED TO CHANGE
  tmp1 <- grep("weight", names(dataset), ignore.case = TRUE)
  lat_lon <- lat_lon[which(!(lat_lon %in% tmp1))]
  ###
  
  tmp_latlon_df <- dataset[lat_lon] # create a temporary dataframe and replace na and nan values with numeric 0. 
  tmp_latlon_df[is.na(tmp_latlon_df)] <- 0 # setting nas to 0 allows test to run on non-na and non-nan values.
  tmp_latlon_df[apply(tmp_latlon_df,2,is.nan)] <- 0
  
  num_ll <- qaqc_helper(tmp_latlon_df, function(x) !is.numeric(x))
  deg_ll <- qaqc_helper(tmp_latlon_df, function(x) any(nchar(trunc(abs(x))) > 3)) 
  
  if (any(c(deg_ll, num_ll))) {
    
    deg_ind <- which(deg_ll)
    num_ind <- which(num_ll)
    degree_msg <- 
      paste("The following latitude/longitude variables are not in decimal degrees:", 
            paste(names(dat)[unique(c(num_ind, deg_ind))], collapse = ","))
    
    cat(degree_msg, file = tmp, append = TRUE)
    warning(degree_msg)
    end <- TRUE
  }

  if (end) warning("At least one test did not pass. Data set will not be saved.")
  
  if (end == FALSE) {
    
    if (save.file == TRUE) {
      
      cat(paste("\nModified data set saved to fishset_db database"), file = tmp, append = TRUE)
      fishset_db <- suppressWarnings(DBI::dbConnect(RSQLite::SQLite(), locdatabase(project = project)))
      on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
      
      single_sql <- paste0(dat, "_final")
      DBI::dbWriteTable(fishset_db, single_sql, dataset, overwrite = TRUE)
    }
  }
  
  # logging function information
  check_model_data_function <- list()
  check_model_data_function$functionID <- "check_model_data"
  check_model_data_function$args <- list(dat, project, uniqueID, save.file)
  check_model_data_function$msg <- tmp
  
  if (file.exists(tmp)) {
    # if checks are passed but save.file = FALSE
    check_model_data_function$msg <- suppressWarnings(readLines(tmp))

    } else {

    check_model_data_function$msg <- ""
  }
  
  log_call(project, check_model_data_function)

  fun_out <- list(msg = check_model_data_function$msg,
                  save_out = (end == FALSE & save.file == TRUE))
  
  invisible(fun_out)
}
