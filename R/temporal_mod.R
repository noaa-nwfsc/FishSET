#' Transform units of date variables
#'
#' Creates a new temporal variable by extracting temporal unit, such as year, month, or day
#' from a date variable.
#' @param dat Primary data containing information on hauls or trips.
#' Table in FishSET database contains the string 'MainDataTable'.
#' @param project Project name.
#' @param x Time variable to modify from \code{dat}.
#' @param define.format Format of temporal data. \code{define.format} should be NULL if converting timezone for \code{x} but not
#'   changing format. Format can be user-defined or from pre-defined choices. Format follows \code{\link{as.Date}} format.
#' See Details for more information.
#' @param timezone String, defaults to NULL. Returns the date-time in the specified time zone. 
#'   Must be a recognizable timezone, such as "UTC", "America/New_York", "Europe/Amsterdam".
#' @param name String, name of created variables. Defaults to `TempMod`.
#' @param log_fun Logical, whether to log function call (for internal use).
#' @param ... Additional arguments. Use \code{tz=''} to specify time zone.
#' @keywords Date as.Date
#' @return Primary data set with new variable added.
#' @details Converts a date variable to desired timezone or units using \code{\link[base]{as.Date}}. \code{\link{date_parser}} is
#' also called to ensure the date variable is in an acceptable format for \code{\link[base]{as.Date}}.
#'  \code{define.format} defines the format that the variable should take on. Examples include \code{"\%Y\%m\%d"}, \code{"\%Y-\%m-\%d \%H:\%M:\%S"}.
#'    Users can define their own format or use one of the predefined ones. Hours is 0-23. 
#'    To return a list of time-zone name in the Olson/IANA database paste \code{OlsonNames()} to the console.

#' \itemize{
#' Predefined formats:
#' \item{year: Takes on the format \code{"\%Y"} and returns the year.}
#' \item{month: Takes on the format \code{"\%Y/\%m"} and returns the year and month.}
#' \item{day: Takes on the format \code{"\%Y/\%m/\%d"} and returns the year, month, and day.}
#' \item{hour: Takes on the format \code{"\%Y/\%m/\%d \%H"} and returns the year, month, day and hour.}
#' \item{minute: Takes on the format \code{"\%Y/\%m/\%d \%H:\%M"} and returns the year, month, day, hour, and minute.}
#' }
#' For more information on formats, see \url{https://www.stat.berkeley.edu/~s133/dates.html}.
#' @export temporal_mod
#' @examples
#' \dontrun{
#' pcodMainDataTable <- temporal_mod(pcodMainDataTable, "pcod", 
#'    "DATE_LANDED", define.format = "%Y%m%d")
#' pcodMainDataTable <- temporal_mod(pcodMainDataTable, "pcod", 
#'    "DATE_LANDED", define.format = "year")
#' }
#'
#'
#' # Change to Year, month, day, minutes
temporal_mod <- function(dat, project, x, define.format=NULL, timezone=NULL, name = NULL, log_fun = TRUE, ...) {

  # Call in datasets
  out <- data_pull(dat, project)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main", project)
  
  args <- list(...) 
  
  if(!is.null(timezone)){
      dataset[[x]]<- lubridate::force_tz(dataset[[x]], tzone=timezone, roll=FALSE)
    }
    
  if(!is.null(define.format)){
  if (!define.format %in% c("year", "month", "day", "hour", "minute")) {
    # User defines the format of the time variable
    temp_out <- format(date_parser(dataset[[x]]), format = define.format)
    # Extract specific time unit
  } else {
    var <- do.call(date_parser, c(list(dataset[[x]]), args))
    if (define.format == "month") {
      # Month:
      temp_out <- format(lubridate::as_date(var), format = "%Y/%m")
    } else if (define.format == "year") {
      # Year:
      temp_out <- format(lubridate::as_date(var), format = "%Y")
    } else if (define.format == "day") {
      # Month/day
      temp_out <- format(lubridate::as_date(var), format = "%Y/%m/%d")
    } else if (define.format == "hour") {
      temp_out <- format(lubridate::as_date(var), format = "%Y/%m/%d %H")
    } else if (define.format == "minute") {
      temp_out <- format(lubridate::as_date(var), format = "%Y/%m/%d %H:%M")
    } else {
      warning("define.format is not recognized. Pre-formatted choices include, year, month, day, hour, minute")
    }
  }
  }

  if (log_fun) {
    
    temp_mod_function <- list()
    temp_mod_function$functionID <- "temporal_mod"
    temp_mod_function$args <- list(dat, x, define.format, timezone, deparse(substitute(name)),
                                   log_fun, args)
    temp_mod_function$output <- list(dat)
    log_call(project, temp_mod_function)
  }

  if (is_empty(name)) name <- paste0("temp_mod_", define.format)

  dataset[[name]] <- temp_out
  return(dataset)
}
