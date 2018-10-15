#'  Log all actions and informative messages in an exterior file.
#'
#'  Exterior file is dated to the date the code is run
#'  Requires the futile.library package and functions built from the package

#'
#' @param dat dataframe or matrix
#' @param x column in dataframe
#' @param fun.name name of the function being applied
#' @keywords logging
#' @method
#' @export
#' @details Calls the futile.logger package. Calls to log messages are saved in the specified dated file.
#' @details
#' @details Messages can be saves as informational (flog.info), as a record of actions taken (flog.trace), as error or warning messages (flog.error, flog.warn), as debug message (flog.debug).
#' @details The flog_func function is used within other functions to log actions taken. These trace messages are saved to an external file

#' @examples

require('futile.logger')

#Save log to both the console and a file or just to the file with the date in the file name
# Recommend using file_both for informational and warning messages.
# Recommend using file_save for trace messages, which are recordings of actions taken, such as function applied to dataframe
flog.appender(appender.tee(paste('Logs/Log_file_',Sys.Date(),'.log')), name='file_both')
flog.appender(appender.file(paste('Logs/Log_file_',Sys.Date(),'.log')), name='file_save')

# Set the threshold levels for which messages will print.
# Here, we have set that all messages (trace and higher level messages) will print for 'file_save' but
# trace messages and debug messages will not print to console or log if name='file_both'.
# Only info, warn, error, and crash messages will print.
flog.threshold(TRACE, name='file_save')
#Want to save to both at info level or higher (won't print trace or debug messages on console)
flog.threshold(INFO, name='file_both')

#Function to print trace messages to log file.
flog_func <- function(dat, x, fun.name){
     flog.trace('Applied the %s function to %s column of dataframe %s', fun.name, x, dat, name='file_save')
}

