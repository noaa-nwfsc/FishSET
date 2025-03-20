#' Change variable data class
#'
#' View data class for each variable and call appropriate functions to change 
#' data class as needed.
#'
#' @param dat Primary data frame over which to apply function. Table in FishSET 
#'   database should contain the string `MainDataTable`.
#' @param project Name of project.
#' @param x A character string of variable(s) in \code{dat} that will be changed 
#'   to \code{new_class}. One ore more variables may be included. Default set to NULL.   
#' @param new_class A character string of data classes that \code{x} should be 
#'   changed to. Length of \code{new_class} should match the length of \code{x} 
#'   unless all variables in \code{x} should be the same \code{new_class}.
#'   Defaults to NULL. Options are "numeric", "factor", "date", "character". Must be in quotes.
#' @param save Logical. Should the data table be saved in the FishSET database, 
#'   replacing the working data table in the database? Defaults to FALSE.
#' @details Returns a table with data class for each variable in \code{dat} and 
#'   changes variable classes. To view variable classes run the function with default 
#'   settings, specifying only \code{dat} and \code{project}. If variable class 
#'   should be changed, run the function again, specifying the variable(s) \code{(x)} 
#'   to be changed and the new_class(es) \code{(new_class)}. Set \code{save} to 
#'   TRUE to save modified data table.
#' @return Table with data class for each variable and the working data with modified 
#'   data class as specified.
#' @importFrom DBI dbWriteTable dbConnect dbDisconnect
#' @importFrom tibble enframe tibble
#' @importFrom dplyr bind_cols
#' @importFrom lubridate as_date
#' @importFrom purrr safely quietly
#' @examples
#' \dontrun{
#' #View table without changing class or saving
#' change_class(pollockMainDataTable, "myproject")
#'
#' #Change class for a single variable and save data table to FishSET database
#' change_class(pollockMainDataTable, "myproject", x = "HAUL", new_class = 'numeric', save=TRUE)
#' 
#' #Change class for multiple variables and save data table to FishSET database
#' change_class(pollockMainDataTable, "myproject", x = c("HAUL","DISEMBARKED_PORT"),
#'  new_class = c('numeric', 'factor'), save=TRUE)
#' }
#' @export change_class

change_class <- function(dat, project, x = NULL, new_class = NULL, save = FALSE) {
 
  # Call in datasets
  out <- data_pull(dat, project)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main", project)
  
  class_change <- TRUE # should class change occur?
  change_flag <- FALSE # was a class change attempted?
  # TODO add msg for each variable that was not converted
  
  # better solution for 2+ classes? Collapse classes into single string? 
  first_class <- function(x) class(x)[[1]]
  
  
  if (is_value_empty(x) & is_value_empty(new_class)) {
    
    class_change <- FALSE
  }
  
  # change data ----
  # Conversion is based on starting and ending class
  if (is_value_empty(x) & !is_value_empty(new_class)) {
    
    stop("argument 'x' is missing.")
  }
  
  if (!is_value_empty(x) & !is_value_empty(new_class)) {
    
    x_len <- length(x)
    nc_len <- length(new_class) 
    
    if (x_len != nc_len) {
      
      if (nc_len != 1) {
        
        stop("'new_class' should have length 1 or the same length as 'x'.")
      }
    }
  }
  
  if (!is_value_empty(x)) {
    
    origclass <- vapply(dataset[x], first_class, FUN.VALUE = character(1))
    origclass <- toupper(origclass)
  }
  
  if (!is_value_empty(new_class)) {
    
    if (is.character(new_class)) {
      
     new_class <- toupper(new_class)
     
    } else {
      
      stop("'new_class' must be a character vector.")
    }
    
    # accepted classes 
    # TODO: add integer to available classes
    available_classes <- c("NUMERIC", "CHARACTER", "FACTOR", "DATE")
    
    if (any(!new_class %in% available_classes)) {
      
      stop("Invalid class selected. Available options are: ", 
           paste(available_classes, collapse = ", "))
    }
  }
  
  if (class_change) {
    
    # date-time options?
    
    di_n <- names(which((origclass == "DATE" | origclass == 'INTEGER') & (new_class=="NUMERIC")))
    c_n <- names(which((origclass == "CHARACTER") & (new_class=="NUMERIC")))
    f_n <- names(which((origclass == "FACTOR") & (new_class=="NUMERIC")))
    ni_c <- names(which((origclass == "NUMERIC" | origclass == "INTEGER")
                       & (new_class=="CHARACTER")))
    f_c <- names(which((origclass == "FACTOR") & (new_class=="CHARACTER")))
    d_c <- names(which((origclass == "DATE") & (new_class=="CHARACTER")))
    ncd_f <- names(which((origclass == "NUMERIC" | origclass == "CHARACTER" | 
                            origclass == "DATE" | origclass == "INTEGER") & (new_class=="FACTOR")))
    cn_d <- names(which((origclass == "CHARACTER" | origclass == "NUMERIC" | origclass == "INTEGER") & 
                          (new_class=="DATE")))
    f_d <- names(which((origclass == "FACTOR") & (new_class=="DATE")))
    
    
    # TODO use error handlers to check for invalid output
    
    ## To Numeric ----
    # Change date/integer to numeric
    if (length(di_n) > 0) {
      
      dataset[di_n] <- lapply(dataset[di_n], function(x) as.numeric)
      change_flag <- TRUE
    }
    
    #from character to numeric
    if (length(c_n) > 0) {
      
      dataset[c_n] <- lapply(dataset[c_n], function(x) as.numeric(as.character(x)))
      change_flag <- TRUE
    }
    
    #from factor to numeric
    if (length(f_n) > 0) {
      
      dataset[f_n] <- lapply(dataset[f_n], function(x) {
        # returns original values back to numeric
        out <- suppressWarnings(as.numeric(levels(x))[x])
        # returns factor levels as numeric values
        if (anyNA(out)) out <- as.numeric(x) 
        out
      })
      
      change_flag <- TRUE
    }
    
    ## To character ---- 
    # numeric, integer to character
    if (length(ni_c) > 0) {
      
      dataset[ni_c] <- lapply(dataset[ni_c], as.character)
      change_flag <- TRUE
    }
    
    # factor to character
    if (length(f_c) > 0) {
      
      dataset[f_c] <- lapply(dataset[f_c], as.character.factor)
      change_flag <- TRUE
    }
    
    # date to character
    if (length(d_c) > 0) {
      
      dataset[d_c] <- lapply(dataset[d_c], as.character.Date)
      change_flag <- TRUE
    }
    
    ## To factor ----
    # numeric, character, date to factor
    if (length(ncd_f) > 0) {
      
      dataset[ncd_f] <- lapply(dataset[ncd_f], as.factor)
      change_flag <- TRUE
    }
    
    ## To date ---- 
    # character to date
    if (length(cn_d) > 0) {

      dataset[cn_d] <- lapply(dataset[cn_d], date_parser)
      change_flag <- TRUE
    }
 
    if (length(f_d) > 0) {
      
      # underlying values can be date, date string, numeric, or numeric as string
   
      num_to_date <- function(x) {
        
        out <- as.numeric(as.character(x))
        lubridate::as_date(out)
      }
      
      qs_test <- quiet_safe_test(date_parser)
   
      # results of attempting as_date
      res_char <- lapply(dataset[f_d], qs_test)
      
      char_keep <- 
        vapply(res_char, function(x) !is_value_empty(x$result$result), logical(1))
      
      qs_test <- quiet_safe_test(num_to_date)
      
      res_num <- lapply(dataset[f_d], qs_test)
      
      num_keep <- 
        vapply(res_num, function(x) !is_value_empty(x$result$result), logical(1))
      
      if (any(char_keep)) {
        
        c_ind <- names(char_keep[char_keep])
        
        dataset[c_ind] <- lapply(res_char[c_ind], function(x) x$result$result)
        change_flag <- TRUE
      }
      
      if (any(num_keep)) {
        
        n_ind <- names(num_keep[num_keep])
        # if a var happens to not be empty in both num and char results, default to char result
        if (any(c_ind %in% n_ind)) {
          
          n_ind <- n_ind[!n_ind %in% c_ind]
        }
        
        if (length(n_ind) > 0) {
          
          dataset[n_ind] <- lapply(res_num[n_ind], function(x) x$result$result)
          change_flag <- TRUE
        }
      }
    }
  }

  # make into separate helper function?
  # class table ----
  var_class <- vapply(dataset, first_class, character(1))
  
  class_tab <- 
    tibble::enframe(var_class, name = "Variable", value = "Class") %>% 
    dplyr::bind_cols(Value = as.character(dataset[1, ]))
  
  print(class_tab, n = nrow(class_tab))
  
  # save data ----
  if (save & class_change) {
    
    suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project = project)))
    on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
    DBI::dbWriteTable(fishset_db, paste0(project, "MainDataTable"), dataset, overwrite = TRUE)
  }
  
  #Log the function
  change_class_function <- list()
  change_class_function$functionID <- "change_class"
  change_class_function$args <- list(dat, project, x, new_class, save)

  log_call(project, change_class_function)
    
  if (change_flag) {
    
    message("Variable class changed.")
    
  } else {
    
    warning("Variable class was not changed.", call. = FALSE)
  }

  if (class_change) {
    
    return(dataset)
  } 
}

