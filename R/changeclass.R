

#' Change variable data class
#'
#' View data class for each variable and call appropriate functions to change data class as needed.
#'
#' @param dat Main data frame over which to apply function. Table in FishSET 
#'   database should contain the string `MainDataTable`.
#' @param project Name of project.
#' @param x A character string of variable(s) in \code{dat} that will be changed to \code{newclass}. 
#'   One ore more variables may be included. Default set to NULL.   
#' @param newclass A character string of data classes that \code{x} should be changed to. Length of \code{newclass}
#'   should match the length of \code{x} unless all variables in \code{x} should be the same \code{newclass}.
#'   Defaults to NULL. Options are "numeric", "factor", "date", "character". Must be in quotes.
#' @param savedat Logical. Should the data table be saved in the FishSET database, replacing the working data table
#'  in the database? Defaults to FALSE.
#' @details Returns a table with data class for each variable in \code{dat} and changes variable classes.   
#'   To view variable classes run the function with default settings, specifying only \code{dat} and \code{project}. 
#'   If variable class should be changed, run the function again, specifying the variable(s) \code{(x)} to be changed 
#'   and the newclass(es) \code{(newclass)}. Set \code{savedat} to TRUE to save modified data table.
#' @return Table with data class for each variable and the working data with modified data class as specified.
#' @importFrom DBI dbWriteTable dbConnect dbDisconnect
#' @importFrom shiny isRunning
#' @examples
#' \dontrun{
#' #View table without changing class or saving
#' changeclass(pollockMainDataTable, "myproject")
#'
#' #Change class for a single variable and save data table to FishSET database
#' changeclass(pollockMainDataTable, "myproject", x = "HAUL", newclass = 'numeric', savedat=TRUE)
#' 
#' #Change class for multiple variables and save data table to FishSET database
#' changeclass(pollockMainDataTable, "myproject", x = c("HAUL","DISEMBARKED_PORT"),
#'  newclass = c('numeric', 'factor'), savedat=TRUE)
#' }
#' @export changeclass

changeclass <- function(dat, project, x=NULL, newclass=NULL, savedat=FALSE){
 
  # Call in datasets
  out <- data_pull(dat, project)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main")
  
 end <- FALSE
  
  
  #change data
  #Conversion is based on starting and ending class
  if (!is.null(x)) {
    origclass <- vapply(x, function(v) class(dataset[[v]]), FUN.VALUE = character(1))
    origclass <- toupper(origclass)
  }
  
  if (!is.null(newclass)) {
    if(is.character(newclass)|is.list(newclass)){
     newclass <- toupper(newclass)
    } else {
    end <- TRUE
    }
  }
  

 
  if(end == FALSE) {
  d_n <-  names(which((origclass == "DATE" | origclass == 'INTEGER') & (newclass=="NUMERIC")))
  c_n <- names(which((origclass == "CHARACTER") & (newclass=="NUMERIC")))
  f_n <- names(which((origclass == "FACTOR") & (newclass=="NUMERIC")))
  n_c <- names(which((origclass == "NUMERIC") & (newclass=="CHARACTER")))
  i_c <- names(which((origclass == "INTEGER") & (newclass=="CHARACTER")))
  f_c <- names(which((origclass == "FACTOR") & (newclass=="CHARACTER")))
  d_c <- names(which((origclass == "DATE") & (newclass=="CHARACTER")))
  ncd_f <- names(which((origclass == "NUMERIC" | origclass == "CHARACTER" | origclass == "DATE" | origclass == "INTEGER") & (newclass=="FACTOR")))
  c_d <- names(which((origclass == "CHARACTER") & (newclass=="DATE")))
  nf_d <- names(which((origclass == "NUMERIC" | origclass == "FACTOR" | origclass == "INTEGER") & (newclass=="DATE")))

  #Change to numeric
  #from date
  if (length(d_n) > 0) {
    
    dataset[d_n] <- lapply(d_n, function(x) as.numeric(dataset[[x]]))
  }
  
  #from character
  if (length(c_n) > 0) {
    
    dataset[c_n] <- lapply(c_n, function(x) as.numeric(as.character(dataset[[x]])))
  }
  
  #from factor
  if (length(f_n) > 0) {
    
    dataset[f_n] <- lapply(f_n, function(x) {
      
      out <- suppressWarnings(as.numeric(levels(dataset[[x]]))[dataset[[x]]]) # returns original values back to numeric
      
      if (anyNA(out)) out <- as.numeric(dataset[[x]]) # returns factor levels as numeric values
      out
     })
  }
  
  #Change to character    
  #numeric
  if (length(n_c) > 0) {
    
    dataset[n_c] <- lapply(n_c, function(x) as.character(dataset[[x]]))
  }
  #integer
  if (length(i_c) > 0) {
    
    dataset[i_c] <- lapply(i_c, function(x) as.character(dataset[[x]]))
  }
  
  #factor
  if (length(f_c) > 0) {
    
    dataset[f_c] <- lapply(f_c, function(x) as.character.factor(dataset[[x]]))
  }
  
  #date
  if (length(d_c) > 0) {
    
    dataset[d_c] <- lapply(d_c, function(x) as.character.Date(dataset[[x]]))
  }
  
  #Change to factor
  #numeric, character, date
  if (length(ncd_f) > 0) {
    
    dataset[ncd_f] <- lapply(ncd_f, function(x) as.factor(dataset[[x]]))
  }
  
  #Change to date  
  ##character
  if (length(c_d) > 0) {
    charlength <- nchar(as.character(dataset[[c_d[1]]][1]))
    if (charlength > 8) {
      dataset[c_d] <- lapply(c_d, function(x) as.Date(as.POSIXct(dataset[[c_d]], origin="1970-01-01"))) 
    } else {
    dataset[c_d] <- lapply(c_d, function(x) as.Date(dataset[[x]]))
    }
  }
 
    #numeric #factor
  #as.Date.numeric(x)
  if (length(nf_d) > 0) {
      charlength <- nchar(as.character(dataset[[nf_d[1]]][1]))
    if (charlength > 8) {
      dataset[nf_d] <- lapply(nf_d, function(x) as.Date(as.POSIXct(dataset[[nf_d]], origin="1970-01-01")))
    } else if (charlength == 8) {
      dataset[nf_d] <- lapply(nf_d, function(x) as.Date(as.character(dataset[[x]]), format = "%Y%m%d"))
    } else if (charlength == 6) {
      dataset[nf_d] <- lapply(nf_d, function(x) as.Date(paste0(as.character(dataset[[x]], "01")), format = "%Y%m%d"))
    } else if (charlength) {
      dataset[nf_d] <- lapply(nf_d, function(x) as.Date(as.character(paste0(dataset[[x]], "0101")), format = "%Y%m%d"))
    } else {
    dataset[nf_d] <- dataset[nf_d]
    message("Unable to convert ", nf_d, ' to date class.')
    }
  }
  

  #Print table  
    g <- as.data.frame(cbind(sapply(dataset, class), t(dataset[1, ])))
    colnames(g) = c("Class", "Value")
    print(g)

  
  #save data
  if(savedat == TRUE & (is.null(x) & is.null(newclass)) == FALSE){
    suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project = project)))
    on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
    DBI::dbWriteTable(fishset_db, paste0(project, "MainDataTable"), dataset, overwrite = TRUE)
  }
  
    #Log the function
    changeclass_function <- list()
    changeclass_function$functionID <- "changeclass"
    changeclass_function$args <- list(dat, project, x, newclass, savedat)
  
    log_call(project, changeclass_function)
  }

  if ((is.null(x) & is.null(newclass)) == FALSE) {
    return(dataset)
  }
}

