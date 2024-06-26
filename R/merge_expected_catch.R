#' Merge expected catch
#' 
#' Merge expected catch matrices to the primary dataset.
#' 
#' @param dat Primary data containing information on hauls or trips. Table in FishSET 
#'   database contains the string 'MainDataTable'.
#' @param project String, name of project.
#' @param zoneID zone ID Variable in `dat` that identifies the individual zones or 
#'   areas.
#' @param date Date variable used to create the expected catch matrix.
#' @param exp.name Name(s) of expected catch matrix to merge into `dat`.
#' @param new.name Optional, new name for `exp.name`. These should be in the same
#'   order as `exp.name`. 
#' @param ec.table Optional, the name of a specific expected catch table to use.
#'   Defaults to `projectnameExpectedCatch`. 
#' @param log_fun For internal use. Whether to log the function call. 
#' @md
#' @export
#' @returns Merges an expected catch matrix created using [create_expectations()]
#' to the primary dataset, `dat`. 


merge_expected_catch <- function(dat, project, zoneID, date, exp.name, new.name = NULL, 
                                 ec.table = NULL, log_fun = TRUE) {
  
  # call in dataset
  out <- data_pull(dat, project = project)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main", project)
  
  # pull expected catch list
  ecl <- expected_catch_list(project, name = ec.table)
  
  # vector of expected catch matrix names
  ecn <- exp_catch_names(project)
  
  column_check(dataset, c(zoneID, date))
  
  if (is.null(new.name)) new.name <- exp.name
  
  # check that new column names won't conflict w/ existing 
  if (any(new.name %in% names(dataset))) {
    non_unique <- new.name[new.name %in% names(dataset)]
    stop(paste(non_unique, collapse = ', '), ' already exists in data.', call. = FALSE)
    # note: allow users to override/rename their exp catch matrices?
  }
  
  # create new column in dataset from expected catch matrix
  dataset[new.name] <- lapply(exp.name, function(x) {
    if (!x %in% ecn) {
      stop('exp.name "', x, '" does not exist', call. = FALSE)
    }
    ecMat <- ecl[[x]]
    # use the ec value that matches data's date and zone  
    ecMat[cbind(match(as.character(dataset[[date]]), row.names(ecMat)),
                match(dataset[[zoneID]], colnames(ecMat)))]
  })
  
  if (log_fun) {
    # log function
    merge_expected_catch_function <- list()
    merge_expected_catch_function$functionID <- "merge_expected_catch"
    merge_expected_catch_function$args <- list(dat, project, zoneID, date, exp.name, 
                                               new.name, ec.table, log_fun)
    merge_expected_catch_function$kwargs <- list()
    
    log_call(project, merge_expected_catch_function)
  }
  # output: full dataset
  dataset
}



