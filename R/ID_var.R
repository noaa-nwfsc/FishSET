#' Create ID variable
#'
#' Create ID variable from one or more variables
#'
#' @param dat Primary data containing information on hauls or trips.
#' Table in FishSET database contains the string `MainDataTable`.
#' @param vars Character string, additional column(s) in \code{dat} that define unique observations.
#' @param name String, name of new ID column.
#' @param type String, the class type of the new ID column. "string" returns a character
#'   vector where each column in \code{vars} is combined and separated with an underscore 
#'   "_". "integer" returns an integer vector where each value corresponds to a unique
#'   group in \code{vars}. 
#' @export ID_var
#' @return Returns the `MainDataTable` with the ID variable included.
#' @details ID variable can be based on a single or multiple variables.
#'
#' @examples
#' \dontrun{
#' pcodMainDataTable <- ID_var(pcodMainDataTable, name = "PermitID", c("GEAR_TYPE", "TRIP_SEQ"))
#' }
#'
ID_var <- function(dat, vars, name = NULL, type = "string") {

  # Call in datasets
  out <- data_pull(dat)
  dataset <- out$dataset
  
  if (shiny::isRunning()) {
    if (deparse(substitute(dat)) %in% c("dataset", "values$dataset")) dat <- get("dat_name")
  } else { 
    if (!is.character(dat)) dat <- deparse(substitute(dat)) }

  if (is.null(name))  name <- paste0(vars, collapse = "_")
  else name <- make.names(name)
  
  n <- length(vars) - 1
  
  plist <- lapply(vars, function(x) trimws(dataset[[x]]))
  
  plist[1:n] <- lapply(seq_along(n), function(x) paste0(plist[[x]], "_"))
  
  dataset[[name]] <- do.call(paste0, plist)
  
  if (type == "integer") {
    
    dataset[[name]] <- as.integer(as.factor(dataset[[name]]))
  }

  # logging function information write(layout.json.ed(trace, 'ID_var', deparse(substitute(dataset)), newID, msg = paste(newID, 'created based on',
  # deparse(substitute(argList)))), paste(getwd(), '/Logs/', Sys.Date(), '.json', sep = ''), append = T)

  ID_var_function <- list()
  ID_var_function$functionID <- "ID_var"
  ID_var_function$args <- list(dat, vars, name, type)
  ID_var_function$ouput <- list(dat)

  log_call(ID_var_function)
  
  dataset
}
