

merge_dat <- function(dat, other, project, main_key, other_key, other_type, 
                      merge_type = "left") {
  #'
  #' Merge data tables using a left join
  #'
  #' @param dat Primary data containing information on hauls or trips. Table in 
  #'   the FishSET database contains the string 'MainDataTable'.
  #' @param other A second data table to join to MainDataTable. Use string if 
  #'   referencing a table saved in the FishSET database. 
  #' @param project Project name. 
  #' @param main_key String, name of column(s) in MainDataTable to join by. The 
  #'   number of columns must match \code{other_key}.  
  #' @param other_key String, name of column(s) in the \code{other} table to join 
  #'   by. The number of columns must match \code{main_key}.
  #' @param other_type String, the type of secondary data being merged. Options 
  #'   include "aux" (auxiliary), "grid" (gridded), "spat" (spatial), and "port". 
  #' @param merge_type String, the type of merge to perform. \code{"left"} keeps all
  #'   rows from \code{dat} and merges shared rows from \code{other}. \code{"full"}
  #'   keeps all rows from each table. 
  #' @importFrom dplyr left_join full_join
  #' @importFrom purrr pmap pmap_lgl
  #' @importFrom stats setNames
  #' @export
  #' @details 
  #'   This function merges two datasets using a left join: all columns and rows 
  #'   from the MainDataTable are kept while only matching columns and rows from 
  #'   the secondary table are joined. 
  #' @examples
  #' \dontrun{
  #'  pollockMainDataTable <- 
  #'     merge_dat("pollockMainDataTable", "pollockPortTable", "pollock", 
  #'               main_key = "PORT_CODE", other_key = "PORT_CODE")
  #' } 

  # pull main data
  out <- data_pull(dat, project)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main", project)
  
  # pull secondary data
  other_out <- data_pull(other, project)
  other_dat <- other_out$dataset
  
  # check column names exist
  column_check(dataset, main_key)
  column_check(other_dat, other_key)
  
  if (other_type == "aux") other <- parse_data_name(other, "aux", project)
  else if (other_type == "grid") other <- parse_data_name(other, "grid", project)
  else if (other_type == "port") other <- parse_data_name(other, "port", project)
  else if (other_type == "spat") other <- parse_data_name(other, "spat", project)

  if (length(main_key) != length(other_key)) {
    
    stop("Key lengths must match.", call. = FALSE)
    
  } else {
    
    merge_by <- stats::setNames(other_key, c(main_key))
  }
  # determine class of key columns
  m_class <- lapply(main_key, function(x) class(dataset[[x]]))
  o_class <- lapply(other_key, function(x) class(other_dat[[x]]))
  
  # find key(s) classes that don't match
  no_match_class <- purrr::pmap(list(m = seq_along(m_class), o = seq_along(o_class)), 
                                function(m, o) !all(m_class[[m]] == o_class[[o]]))
  
  if (any(unlist(no_match_class))) {
    
    # define exceptions for mismatched classes
    except_match <- purrr::pmap_lgl(list(m = m_class, o = o_class), function(m, o) {
      
      (m %in% c("character", "factor")) & (o %in% c("character", "factor")) |
        (m %in% c("numeric", "integer")) & (o %in% c("numeric", "integer"))
    })
    
    if (any(!except_match)) {
      
      ind <- which(unlist(no_match_class) & !except_match)
      
      class_error <- vapply(ind, FUN = function(x) {
        
        paste0("'", main_key[x], "' and '", other_key[x], "' class types are incompatible (", 
               class(dataset[[main_key[x]]]), "/", class(other_dat[[other_key[x]]]), 
               "). Unable to merge datasets.")
        
      }, FUN.VALUE = character(1))
      
      stop(paste0(class_error, collapse = "\n"), call. = FALSE)
    }
  }
  
  is_factor_char <- function(x) (is.character(x) | is.factor(x)) 
  
  # if key is character or factor class, trim white spaces
  if (any(vapply(dataset[main_key], FUN = is_factor_char, FUN.VALUE = logical(1)))) {
    
    dataset[main_key] <- lapply(main_key, function(x) trimws(dataset[[x]]))
  }
  
  if (any(vapply(other_dat[other_key], FUN = is_factor_char, FUN.VALUE = logical(1)))) {
    
    other_dat[other_key] <- lapply(other_key, function(x) trimws(other_dat[[x]]))
  }
  
  if (merge_type == "left") {
    
    dataset <-
      dplyr::left_join(dataset,
                       other_dat,
                       by = merge_by,
                       suffix = c("_main", paste0("_", other_type)))
  
  } else if (merge_type == "full") {
    
    dataset <-
      dplyr::full_join(dataset,
                       other_dat,
                       by = merge_by,
                       suffix = c("_main", paste0("_", other_type)))
  }
  
  # Log function
  merge_dat_function <- list()
  merge_dat_function$functionID <- "merge_dat"
  merge_dat_function$args <- list(dat, other, project, main_key, other_key, 
                                  other_type, merge_type)
  log_call(project, merge_dat_function)
  
  dataset
}


split_dat <- function(dat, aux = NULL, project, split_by = NULL, key, output = "main") {
  #'
  #' Separate secondary data table from MainDataTable
  #' 
  #' @param dat Primary data containing information on hauls or trips. Table in the FishSET
  #'   database contains the string 'MainDataTable'.
  #' @param aux Auxiliary data table in fishset_db or environment. Use string if referencing 
  #'   a table saved in the FishSET database. The column names from "aux"
  #'   will be used to find and separate the auxiliary table from the MainDataTable.
  #' @param project String, name of project.
  #' @param split_by String, columns in MainDataTable to split by. These columns will be 
  #'   separated from MainDataTable. Must contain values from "key". 
  #' @param key String, the column(s) that link the main and auxiliary data tables. If using "aux" method, "key" 
  #'   must match a column in both MainDataTable and "aux" data table. If using "split_by",
  #'   "key" must match a column in "MainDataTable and also be included in "split_by". 
  #' @param output String, return either the "main" data table, "aux" data table, or
  #'   "both" main and aux data tables in a list. 
  #' @importFrom dplyr distinct
  #' @export
  #' @details 
  #'   This function separates auxiliary data (or gridded and port data) from the MainDatatable.
  #'   Users can either input the secondary data table (from environment or fishset_db) to determine
  #'   which columns to remove or by passing a string of columns names to "split_by". Use either
  #'   the "aux" or the "split_by" method. Defaults to "aux" method if both arguments are used.
  #' @examples 
  #' \dontrun{
  #' split_dat("pollockMainDataTable", "pollock", aux = "pollockPortTable", key = "PORT_CODE")
  #' }

  # pull main data
  out <- data_pull(dat, project)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main", project)
  
  aux_dat <- NULL
  
  if (!is.null(aux)) { 
    # pull aux data
    out_aux <- data_pull(aux, project)
    aux_dat <- out_aux$dataset
    aux <- parse_data_name(aux, 'aux', project)
  }
  
  # TODO: allow key arg to work w/ differently named keys (ex: "PORT_NAME" and "port")
  # This functionality exists in merge_dat() so should also be included here
  
  if (is.null(aux_dat) & is.null(split_by)) {
    
    stop("Both 'aux' and 'split_by' arguments are missing. Please select a splitting option.",
         call. = FALSE)
  }
  
  main_cols <- colnames(dataset)
  
  if (!is.null(aux_dat)) {
    
    aux_cols <- colnames(aux_dat) 
    split_check(main_cols, aux_cols, key, "aux")
  
  } else if (!is.null(split_by)) {
    
    split_check(main_cols, split_by, key, "split_by")
  }

  # determine split by aux colnames
  if (!is.null(aux_dat)) {
    
    matched_cols <- aux_cols[aux_cols %in% main_cols]
    matched_cols <- matched_cols[!(matched_cols %in% key)]
    
    dataset <- dataset[, !(main_cols %in% matched_cols)]

  } else if (!is.null(split_by)) {

    if (any(output %in% c("aux", "both"))) {
      
      aux_dat <- dataset[split_by] 
      
      # return unique rows
      aux_dat <- dplyr::distinct(aux_dat)
    }
      
    # remove all aux columns from main data (except key) 
    split_by <- split_by[!(split_by %in% key)]
    dataset <- dataset[, !(main_cols %in% split_by)]
  }
  
  # Log function
  split_dat_function <- list()
  split_dat_function$functionID <- "split_dat"
  split_dat_function$args <- list(dat, aux, project, split_by, key, output)
  log_call(project, split_dat_function)
    
  if (output == "main") {
    
    dataset
  
  } else if (output == "aux") {
    
    aux_dat
  
  } else if (output == "both") {
    
    list("main" = dataset, "aux" = aux_dat)
  }
}                                                                               


split_check <- function(main, split, key, arg) {
  #'
  #' Perfom quality check for \code{split_dat}
  #'
  #' @param main String, column names from MainDataTable.
  #' @param split String, column names from "split_by" argument.
  #' @param key String, column names from "key" argument.
  #' @param arg String, whether "aux" or "split_by" argument was used.
  #' @keywords internal
  #' @export 
  
  if (!all(split %in% main)) {
    
    # note which colnames aren't in main data
    not_found <- split[!(split %in% main)]
    stop(paste0("Main data does not contain column(s) ", paste(not_found, collapse = ", "), 
                ". Did you specify the correct columns in '", arg, "'?"), call. = FALSE)
  }
  
  if (!all(key %in% main)) {
    
    # note which keys aren't in main data
    not_found <- key[!(key %in% main)]
    stop(paste0("Main data does not contain column(s) ", paste(not_found, collapse = ", "), 
                ". Did you specify the correct columns in 'key'?"), call. = FALSE)
  }
  
  if (!all(key %in% split)) {
    
    not_found <- key[!(key %in% split)]
    stop(paste0("'", arg, "' does not contain column(s) ", paste(not_found, collapse = ", "), 
                ". Did you specify the correct columns in 'key'?"), call. = FALSE)
  }
}
