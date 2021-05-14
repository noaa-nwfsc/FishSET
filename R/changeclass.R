

#' Change varialble data class
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
#'   Defaults to NULL. Options are "numeric", "factor", "date", "character".
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
#' changeclass(pollockMainDataTable, "myproject", x = "HAUL", newclass = numeric, savedat=TRUE)
#' 
#' #Change class for multiple variables and save data table to FishSET database
#' changeclass(pollockMainDataTable, "myproject", x = c("HAUL","DISEMBARKED_PORT"),
#'  newclass = c(numeric, factor), savedat=TRUE)
#' }
#' @export changeclass

changeclass <- function(dat, project, x=NULL, newclass=NULL, savedat=FALSE){

  # Call in datasets
  out <- data_pull(dat)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main")
  
  #change data
  #Conversion is based on starting and ending class
  if (!is.null(x)) {
    origclass <- vapply(x, function(v) class(dataset[[v]]), FUN.VALUE = character(1))
    origclass <- toupper(origclass)
  }
  
  if (!is.null(newclass)) newclass <- toupper(newclass)
  
  cd_n <-  names(which((origclass == "CHARACTER" | origclass == "DATE") & (newclass=="NUMERIC")))
  f_n <- names(which((origclass == "FACTOR") & (newclass=="NUMERIC")))
  n_c <- names(which((origclass == "NUMERIC") & (newclass=="CHARACTER")))
  f_c <- names(which((origclass == "FACTOR") & (newclass=="CHARACTER")))
  d_c <- names(which((origclass == "DATE") & (newclass=="CHARACTER")))
  ncd_f <- names(which((origclass == "NUMERIC" | origclass == "CHARACTER" | origclass == "DATE") & (newclass=="FACTOR")))
  ncf_d <- names(which((origclass == "NUMERIC" | origclass == "CHARACTER" | origclass == "FACTOR") & (newclass=="DATE")))
  
  #Change to numeric
  #from character and date
  if (length(cd_n) > 0) {
    
    dataset[cd_n] <- lapply(cd_n, function(x) as.numeric(dataset[[x]]))
  }
  
  #from factor
  if (length(f_n) > 0) {
    
    #dataset[f_n] <- lapply(names(f_n), function(x) as.numeric(as.character(dataset[[x]])))
    dataset[f_n] <- lapply(f_n, function(x) as.numeric(levels(dataset[[x]]))[dataset[[x]]])
  }
  
  #Change to character    
  #numeric
  if (length(n_c) > 0) {
    
    dataset[n_c] <- lapply(n_c, function(x) as.character(dataset[[x]]))
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
  #numeric  #character #factor
  #as.Date.numeric(x)
  if (length(ncf_d) > 0) {
    
    dataset[ncf_d] <- lapply(ncf_d, function(x) as.POSIXct(dataset[[x]]))
  }
  
  #Print table  
  if (any(newclass == "DATE")) {
    
    temp <- as.data.frame(sapply(dataset, class))[2, ] 
    temp <- droplevels(temp)
    g <- data.frame(cbind(t(temp), t(dataset[1, ])))
    colnames(g)=c("Class", "Value")
    print(g)
  } else {
    g <- as.data.frame(cbind(sapply(dataset, class), t(dataset[1, ])))
    colnames(g) = c("Class", "Value")
    print(g)
  }
  
  #save data
  if(savedat == TRUE & (is.null(x) & is.null(newclass)) == FALSE){
    suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase()))
    DBI::dbWriteTable(fishset_db, paste0(project, "MainDataTable"), dataset, overwrite = TRUE)
    DBI::dbDisconnect(fishset_db)
  }
  
  #Log the function
  changeclass_function <- list()
  changeclass_function$functionID <- "changeclass"
  changeclass_function$args <- list(dat, project, x, newclass, savedat)
  log_call(project, changeclass_function)
  
  if ((is.null(x) & is.null(newclass)) == FALSE) {
    return(dataset)
  }
}
