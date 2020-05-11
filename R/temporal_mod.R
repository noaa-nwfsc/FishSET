#'  Transform units of a date variable
#'
#' @param dat Main data frame over which to apply function. Table in FishSET database should contain the string `MainDataTable`.
#' @param x Time variable to modify 
#' @param define.format Format of temporal data. Format can be user-defined or from pre-defined choices. 
#' @keywords Date as.Date
#' @return Variable with modified time units
#' @details Converts a date variable to desired units using \code{\link[base]{as.Date}}. The FishSET date_parse function is 
#' also called to ensure the date variable is in a format that can be used by the \code{\link[base]{as.Date}} function.
#'  `define.format` defines the format that the variable should take on. Examples include `\%Y\%m\%d`, `\%Y-\%m-\%d \%H:\%M:\%S`. Users can define their own format or use one of the predefined ones.

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
#' Date_Landed_YMD <- temporal_mod(MainDataTable, 'DATE_LANDED', define.format = '%Y%m%d') 
#' Date_Landed_year <- temporal_mod(MainDataTable, 'DATE_LANDED', define.format = 'year')
#' }


# Change to Year, month, day, minutes
temporal_mod <- function(dat, x, define.format) {
    
    # Call in datasets
    out <- data_pull(dat)
    dat <- out$dat
    dataset <- out$dataset
    
    
    if (!define.format %in% c("year", "month", "day", "hour", "minute")) {
        # User defines the format of the time variable
        int <- format(date_parser(dataset[[x]]), format = define.format)
        # Extract specific time unit
    } else {
        if (define.format == "month") {
            # Month:
            int <- format(as.Date(date_parser(dataset[[x]])), format = "%Y/%m")
        } else if (define.format == "year") {
            # Year:
            int <- format(as.Date(date_parser(dataset[[x]])), format = "%Y")
        } else if (define.format == "day") {
            # Month/day
            int <- format(as.Date(date_parser(dataset[[x]])), format = "%Y/%m/%d")
        } else if (define.format == "hour") {
            int <- format(as.Date(date_parser(dataset[[x]])), format = "%Y/%m/%d %H")
        } else if (define.format == "minute") {
            int <- format(as.Date(date_parser(dataset[[x]])), format = "%Y/%m/%d %H:%M")
        } else {
            warning("define.format is not recognized. Pre-formatted choices include, year, month, day, hour, minute")
        }
    }
    
    temp_mod_function <- list()
    temp_mod_function$functionID <- "temp_mod"
    temp_mod_function$args <- c(dat, x, define.format)
    temp_mod_function$kwargs <- list()
    temp_mod_function$output <- c("")
    log_call(temp_mod_function)
    
    return(int)
    
}
