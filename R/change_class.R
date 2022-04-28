#' Change variable data class
#'
#' View data class for each variable and call appropriate functions to change data class as needed.
#'
#' @param dat Main data frame over which to apply function. Table in FishSET 
#'   database should contain the string `MainDataTable`.
#' @param project Name of project.
#' @param x A character string of variable(s) in \code{dat} that will be changed to \code{new_class}. 
#'   One ore more variables may be included. Default set to NULL.   
#' @param new_class A character string of data classes that \code{x} should be changed to. Length of \code{new_class}
#'   should match the length of \code{x} unless all variables in \code{x} should be the same \code{new_class}.
#'   Defaults to NULL. Options are "numeric", "factor", "date", "character". Must be in quotes.
#' @param save Logical. Should the data table be saved in the FishSET database, replacing the working data table
#'  in the database? Defaults to FALSE.
#' @details Returns a table with data class for each variable in \code{dat} and changes variable classes.   
#'   To view variable classes run the function with default settings, specifying only \code{dat} and \code{project}. 
#'   If variable class should be changed, run the function again, specifying the variable(s) \code{(x)} to be changed 
#'   and the new_class(es) \code{(new_class)}. Set \code{save} to TRUE to save modified data table.
#' @return Table with data class for each variable and the working data with modified data class as specified.
#' @importFrom DBI dbWriteTable dbConnect dbDisconnect
#' @importFrom tibble enframe tibble
#' @importFrom dplyr bind_cols
#' @importFrom lubridate as_date
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
    available_classes <- toupper(c("numeric", "character", "date", "factor"))
    
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
    n_c <- names(which((origclass == "NUMERIC") & (new_class=="CHARACTER")))
    i_c <- names(which((origclass == "INTEGER") & (new_class=="CHARACTER")))
    f_c <- names(which((origclass == "FACTOR") & (new_class=="CHARACTER")))
    d_c <- names(which((origclass == "DATE") & (new_class=="CHARACTER")))
    ncd_f <- names(which((origclass == "NUMERIC" | origclass == "CHARACTER" | 
                            origclass == "DATE" | origclass == "INTEGER") & (new_class=="FACTOR")))
    c_d <- names(which((origclass == "CHARACTER") & (new_class=="DATE")))
    nf_d <- names(which((origclass == "NUMERIC" | origclass == "FACTOR" | origclass == "INTEGER") & (new_class=="DATE")))
    
    
    # TODO use error handlers to check for invalid output
    
    ## To Numeric ----
    # Change date/integer to numeric
    if (length(di_n) > 0) {
      
      dataset[di_n] <- lapply(di_n, function(x) as.numeric(dataset[[x]]))
      change_flag <- TRUE
    }
    
    #from character to numeric
    if (length(c_n) > 0) {
      
      dataset[c_n] <- lapply(c_n, function(x) as.numeric(as.character(dataset[[x]])))
      change_flag <- TRUE
    }
    
    #from factor to numeric
    if (length(f_n) > 0) {
      
      dataset[f_n] <- lapply(f_n, function(x) {
        # returns original values back to numeric
        out <- suppressWarnings(as.numeric(levels(dataset[[x]]))[dataset[[x]]])
        # returns factor levels as numeric values
        if (anyNA(out)) out <- as.numeric(dataset[[x]]) 
        out
      })
      
      change_flag <- TRUE
    }
    
    ## To character ---- 
    # numeric to character
    if (length(n_c) > 0) {
      
      dataset[n_c] <- lapply(n_c, function(x) as.character(dataset[[x]]))
      change_flag <- TRUE
    }
    # integer to character
    if (length(i_c) > 0) {
      
      dataset[i_c] <- lapply(i_c, function(x) as.character(dataset[[x]]))
      change_flag <- TRUE
    }
    
    # factor to character
    if (length(f_c) > 0) {
      
      dataset[f_c] <- lapply(f_c, function(x) as.character.factor(dataset[[x]]))
      change_flag <- TRUE
    }
    
    # date to character
    if (length(d_c) > 0) {
      
      dataset[d_c] <- lapply(d_c, function(x) as.character.Date(dataset[[x]]))
      change_flag <- TRUE
    }
    
    ## To factor ----
    # numeric, character, date to factor
    if (length(ncd_f) > 0) {
      
      dataset[ncd_f] <- lapply(ncd_f, function(x) as.factor(dataset[[x]]))
      change_flag <- TRUE
    }
    
    ## To date ---- 
    # character to date
    if (length(c_d) > 0) {

      dataset[c_d] <- lapply(c_d, function(x) lubridate::as_date(dataset[[x]]))
      change_flag <- TRUE
    }
    
    #numeric #factor
    #as.Date.numeric(x)
    if (length(nf_d) > 0) {
      
      # TODO find more reliable approach
      charlength <- nchar(as.character(dataset[[nf_d[1]]][1]))
      
      if (charlength > 8) {
        
        dataset[nf_d] <- lapply(nf_d, function(x) {
          
          as.Date(as.POSIXct(dataset[[nf_d]], origin="1970-01-01"))
          })
        change_flag <- TRUE
        
      } else if (charlength == 8) {
        
        dataset[nf_d] <- lapply(nf_d, function(x) {
          
          as.Date(as.character(dataset[[x]]), format = "%Y%m%d")
          })
        change_flag <- TRUE
        
      } else if (charlength == 6) {
        
        dataset[nf_d] <- lapply(nf_d, function(x) {
          
          as.Date(paste0(as.character(dataset[[x]], "01")), format = "%Y%m%d")
          })
        change_flag <- TRUE
        
      } else if (charlength) {
        
        dataset[nf_d] <- lapply(nf_d, function(x) {
          
          as.Date(as.character(paste0(dataset[[x]], "0101")), format = "%Y%m%d")
          })
        change_flag <- TRUE
        
      } else {
        
        message("Unable to convert ", paste(nf_d, collapse = ", "), ' to date class.')
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
    
    warning("Variable class was not changed.")
  }

  if (class_change) {
    
    return(dataset)
  } 
}

