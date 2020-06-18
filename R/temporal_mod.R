#'  Transform units of date variables
#' Creates a new temporal variable by extraciting temporal unit, such as year, month, or day from a date variable. 
#' @param dat Primary data containing information on hauls or trips. Table in FishSET database contains the string 'MainDataTable'.
#' @param x Time variable to modify from \code{dat}.
#' @param define.format Format of temporal data. Format can be user-defined or from pre-defined choices.  Format follows as.Date format. 
#' See Details for more information.  
#' @param name String, name of created variables. Defaults to `TempMod`.
#' @keywords Date as.Date
#' @return Primary dataset with new variable added.
#' @details Converts a date variable to desired units using \code{\link[base]{as.Date}}. \code{\link{date_parse}} is 
#' also called to ensure the date variable is in an acceptable format for \code{\link[base]{as.Date}}.
#'  \code{define.format} defines the format that the variable should take on. Examples include `\%Y\%m\%d`, `\%Y-\%m-\%d \%H:\%M:\%S`. Users can define their own format or use one of the predefined ones.

#' \itemize{
#' Predefined formats
#' \item{year: Takes on the format `\%Y` and returns the year.}
#' \item{month: Takes on the format `\%Y/\%m` and returns the year and month.}
#' \item{day: Takes on the format `\%Y/\%m/\%d` and returns the year, month, and day.}
#' \item{hour: Takes on the format `\%Y/\%m/\%d \%H` and returns the year, month, day and hour.}
#' \item{minute: Takes on the format `\%Y/\%m/\%d \%H:\%M` and returns the year, month, day, hour, and minute.}
#' }
#' For more information on formats, see \url{https://www.stat.berkeley.edu/~s133/dates.html}.
#' @export temporal_mod
#' @examples  
#' \dontrun{
#' pcodMainDataTable <- temporal_mod(pcodMainDataTable, 'DATE_LANDED', define.format = '%Y%m%d') 
#' pcodMainDataTable <- temporal_mod(pcodMainDataTable, 'DATE_LANDED', define.format = 'year')
#' }


# Change to Year, month, day, minutes
temporal_mod <- function(dat, x, define.format, name) {
    
    # Call in datasets
    out <- data_pull(dat)
    dat <- out$dat
    dataset <- out$dataset
    
    
    if (!define.format %in% c("year", "month", "day", "hour", "minute")) {
        # User defines the format of the time variable
        name <- format(date_parser(dataset[[x]]), format = define.format)
        # Extract specific time unit
    } else {
        if (define.format == "month") {
            # Month:
          name <- format(as.Date(date_parser(dataset[[x]])), format = "%Y/%m")
        } else if (define.format == "year") {
            # Year:
          name <- format(as.Date(date_parser(dataset[[x]])), format = "%Y")
        } else if (define.format == "day") {
            # Month/day
          name <- format(as.Date(date_parser(dataset[[x]])), format = "%Y/%m/%d")
        } else if (define.format == "hour") {
          name <- format(as.Date(date_parser(dataset[[x]])), format = "%Y/%m/%d %H")
        } else if (define.format == "minute") {
          name <- format(as.Date(date_parser(dataset[[x]])), format = "%Y/%m/%d %H:%M")
        } else {
            warning("define.format is not recognized. Pre-formatted choices include, year, month, day, hour, minute")
        }
    }
    
    temp_mod_function <- list()
    temp_mod_function$functionID <- "temp_mod"
    temp_mod_function$args <- list(dat, x, define.format, deparse(substitute(name)))
    temp_mod_function$output <- list(dat)
    log_call(temp_mod_function)
    
    return(cbind(dataset, name))
    
}
