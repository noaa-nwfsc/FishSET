#' Create ID variable
#'
#' Create ID variable from one or more variables
#'
#' @param dat Primary data containing information on hauls or trips.
#' Table in FishSET database contains the string `MainDataTable`.
#' @param project Project name.
#' @param vars Character string, additional column(s) in \code{dat} that define unique observations.
#' @param name String, name of new ID column.
#' @param type String, the class type of the new ID column. Choices are `string`` or `integar`.
#'   `string` returns a character vector where each column in \code{vars} 
#'   is combined and separated by \code{sep}. 
#'   `integer` returns an integer vector where each value corresponds to a unique
#'   group in \code{vars}.
#' @param drop Logical, whether to drop columns in \code{vars}.  
#' @param sep Symbol used to combined variables. 
#' @param log_fun Logical, whether to log function call (for internal use).
#' @export ID_var
#' @return Returns the `MainDataTable` with the ID variable included.
#' @details ID variable can be based on a single or multiple variables.
#'  Use \code{sep = TRUE} if dropping variables that create the ID variable. 
#' @examples
#' \dontrun{
#' pcodMainDataTable <- ID_var(pcodMainDataTable, "pcod", name = "PermitID", 
#'         vars = c("GEAR_TYPE", "TRIP_SEQ"), type = 'integar')
#' pcodMainDataTable <- ID_var(pcodMainDataTable, "pcod", name = "PermitID", 
#'         vars = c("GEAR_TYPE", "TRIP_SEQ"), type = 'string', sep="_")

#' }
#'
ID_var <- function(dat, project, vars, name = NULL, type = "string", drop = FALSE, 
                   sep = "_", log_fun = TRUE) {

  # Call in datasets
  out <- data_pull(dat, project)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main", project)
  

  if (is_empty(name))  name <- paste0(vars, collapse = sep)
  else name <- make.names(name)
  
  n <- length(vars) - 1
  
  plist <- lapply(vars, function(x) trimws(dataset[[x]]))
  
  plist[1:n] <- lapply(seq_along(n), function(x) paste0(plist[[x]], sep))
  
  dataset[[name]] <- do.call(paste0, plist)
  
  if (type == "integer") {
    
    dataset[[name]] <- as.integer(as.factor(dataset[[name]]))
  }
  
  if (drop == TRUE) dataset[vars] <- NULL
 
  if (log_fun) {
    
    ID_var_function <- list()
    ID_var_function$functionID <- "ID_var"
    ID_var_function$args <- list(dat, project, vars, name, type, drop, sep, log_fun)
    ID_var_function$ouput <- list(dat)
    
    log_call(project, ID_var_function)
  }
  
  dataset
}
