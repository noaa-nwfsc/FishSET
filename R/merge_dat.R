

merge_dat <- function(dat, aux, project, mainKey, auxKey) {
  #'
  #' Merge datasets using a left join
  #'
  #' @param dat Primary data containing information on hauls or trips. Table in the FishSET
  #' database contains the string 'MainDataTable'.
  #' @param aux Auxiliary data table to join to MainDataTable. Use string if referencing 
  #'   a table saved in the FishSET database. 
  #' @param project String, name of project. 
  #' @param mainKey String, name of column(s) in MainDataTable to join by. The number
  #'   of columns must match "auxKey".  
  #' @param auxKey String, name of column(s) in the Auxiliary table to join by. The number
  #'   of columns must match "mainKey". 
  #' @importFrom dplyr left_join
  #' @importFrom purrr pmap pmap_lgl
  #' @importFrom stats setNames
  #' @export
  #' @details 
  #'   This function merges two datasets using a left join: all columns and rows from the
  #'   MainDataTable are kept while only matching columns and rows from the auxiliary table 
  #'   are joined. 
  #' @examples
  #' \dontrun{
  #'  merge_dat("pollockMainDataTable", "pollockPortTable", 
  #'            mainKey = "PORT_CODE", auxKey = "PORT_CODE")
  #' } 

  # pull main data
  out <- data_pull(dat)
  dataset <- out$dataset
  
  if (shiny::isRunning()) {
    if (deparse(substitute(dat)) == "values$dataset") dat <- get("dat_name")
  } else { 
    if (!is.character(dat)) dat <- deparse(substitute(dat)) }
  
  # pull aux data
  aux_out <- data_pull(aux)
  aux_dat <- aux_out$dataset
  
  if (shiny::isRunning()) {
    if (deparse(substitute(aux)) == "aux$dataset") aux <- get("aux_name")
  } else { 
    if (!is.character(aux)) aux <- deparse(substitute(aux)) }
  
  end <- FALSE 

  if (length(mainKey) != length(auxKey)) {
    
    warning("Key lengths must match.")
    end <- TRUE
  } else {
    
    merge_by <- stats::setNames(auxKey, c(mainKey))
  }
  # determine class of key columns
  m_class <- lapply(mainKey, function(x) class(dataset[[x]]))
  a_class <- lapply(auxKey, function(x) class(aux_dat[[x]]))
  
  # find key(s) classes that don't match
  no_match_class <- purrr::pmap(list(m = seq_along(m_class), a = seq_along(a_class)), 
                                function(m, a) !all(m_class[[m]] == a_class[[a]]))
  
  if (any(unlist(no_match_class))) {
    
    # define exceptions for mismatched classes
    except_match <- purrr::pmap_lgl(list(m = m_class, a = a_class), function(m, a) {
      
      (m %in% c("character", "factor")) & (a %in% c("character", "factor")) |
        (m %in% c("numeric", "integer")) & (a %in% c("numeric", "integer"))
    })
    
    if (any(!except_match)) {
      
      ind <- which(!except_match)
      
      class_error <- vapply(ind, FUN = function(x) {
        
        paste0("'", mainKey[x], "' and '", auxKey[x], "' class types are incompatible (", 
               class(dataset[[mainKey[x]]]), "/", class(aux_dat[[auxKey[x]]]), 
               "). Unable to merge datasets.")
        
      }, FUN.VALUE = character(1))
      
      warning(paste0(class_error, collapse = "\n"))
      
      end <- TRUE
    }
  }
  
  if (end == FALSE) {
    
    is_factor_char <- function(x) (is.character(x) | is.factor(x)) 
    
    # if key is character or factor class, trim white spaces
    if (any(vapply(dataset[mainKey], FUN = is_factor_char, FUN.VALUE = logical(1)))) {
      
      dataset[mainKey] <- lapply(mainKey, function(x) trimws(dataset[[x]]))
    }
    
    if (any(vapply(aux_dat[auxKey], FUN = is_factor_char, FUN.VALUE = logical(1)))) {
      
      aux_dat[auxKey] <- lapply(auxKey, function(x) trimws(aux_dat[[x]]))
    }
    
    dataset <-
      dplyr::left_join(dataset,
                       aux_dat,
                       by = merge_by,
                       suffix = c("_MAIN", "_AUX"))
    
    # Log function
    merge_dat_function <- list()
    merge_dat_function$functionID <- "merge_dat"
    merge_dat_function$args <- list(dat, aux, project, mainKey, auxKey)
    log_call(merge_dat_function)
    
    dataset
  }
}


split_dat <- function(dat, project, aux = NULL, split_by = NULL, key, output = "main") {
  #'
  #' Separate secondary data table from MainDataTable
  #' 
  #' @param dat Primary data containing information on hauls or trips. Table in the FishSET
  #'   database contains the string 'MainDataTable'.
  #' @param project String, name of project.
  #' @param aux Auxiliary data table in fishset_db or environment. Use string if referencing 
  #'   a table saved in the FishSET database. The column names from "aux"
  #'   will be used to find and separate the auxiliary table from the MainDataTable.
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
  #' split_dat("pollockMainDataTable", aux = "pollockPortTable", key = "PORT_CODE")
  #' }

  # pull main data
  out <- data_pull(dat)
  dat <- out$dat
  dataset <- out$dataset
 
  aux_dat <- NULL
  
  if (!is.null(aux)) { 
    # pull aux data
    out_aux <- data_pull(aux)
    aux <- out_aux$aux
    aux_dat <- out_aux$dataset
  }
  
  end <- FALSE
  
  if (is.null(aux_dat) & is.null(split_by)) {
    
    warning("Both 'aux' and 'split_by' arguments are missing. Please select a splitting option.")
    end <- TRUE
  }
  
  main_cols <- colnames(dataset)
  
  if (!is.null(aux_dat)) {
    
    aux_cols <- colnames(aux_dat) 
    end <- split_check(main_cols, aux_cols, key, "aux")
  
  } else if (!is.null(split_by)) {
    
    end <- split_check(main_cols, split_by, key, "split_by")
  }
  
  if (end == FALSE) {
  
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
    split_dat_function$args <- list(dat, project, aux, split_by, key, output)
    log_call(split_dat_function)
      
    if (output == "main") {
      
      dataset
    
    } else if (output == "aux") {
      
      aux_dat
    
    } else if (output == "both") {
      
      list("main" = dataset, "aux" = aux_dat)
    }
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
  
  end <- FALSE
  
  if (!all(split %in% main)) {
    
    # note which colnames aren't in main data
    not_found <- split[!(split %in% main)]
    #warning(paste("Column(s)", paste(not_found, collapse = ", "), "in", arg, "are not in main data."))
    warning(paste0("Main data does not contain column(s) ", paste(not_found, collapse = ", "), 
                  ". Did you specify the correct columns in '", arg, "'?"))
    end <- TRUE
  }
  
  if (!all(key %in% main)) {
    
    # note which keys aren't in main data
    not_found <- key[!(key %in% main)]
    #warning(paste("Column(s)", paste(not_found, collapse = ", "), "in 'key' are not in main data."))
    warning(paste0("Main data does not contain column(s) ", paste(not_found, collapse = ", "), 
                   ". Did you specify the correct columns in 'key'?"))
    end <- TRUE
  }
  
  if (!all(key %in% split)) {
    
    not_found <- key[!(key %in% split)]
    #warning(paste0("Column(s) ", paste(not_found, collapse = ", "), " in 'key' are not in ", arg, "."))
    warning(paste0("'", arg, "' does not contain column(s) ", paste(not_found, collapse = ", "), 
                   ". Did you specify the correct columns in 'key'?"))
    end <- TRUE
  }
  
  end
}
