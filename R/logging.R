#  Log all actions and informative messages in an exterior file.
#
#  Exterior file is dated to the date the code is run

# @param dataset dataframe or matrix
# @param x column in dataframe
# @param fun.name name of the function being applied
# @keywords logging
# @return Log file
# @export layout.json.ed
# @importFrom jsonlite toJSON unbox
# @details Calls the futile.logger package. Calls to log messages are saved in the specified dated file.
# @details Messages can be saves as informational (flog.info), as a record of actions taken (flog.trace), as error or warning messages (flog.error, flog.warn), as debug message (flog.debug).
# @details The flog_func function is used within other functions to log actions taken. These trace messages are saved to an external file
# @details The layout.json.ed writes the logged messages in a json format.

# @examples
# write_lines(layout.json.ed(trace, 'dataset: x', 'y'), paste(getwd(),'/Logs/',Sys.Date(),'.json'), append=T )


#Write log in json code
#layout.json.ed <- function(level, fun.name, dataset, x, msg='') {
#     if (!requireNamespace("jsonlite", quietly=TRUE))
#          stop("layout.json requires jsonlite. Please install it.", call.=FALSE)
#     
#     where <- 1 # to avoid R CMD CHECK issue
#     output_list <- list(
#          Function=fun.name,
#          Dataset=jsonlite::unbox(dataset), 
#          Vector=jsonlite::unbox(x),
#          message=jsonlite::unbox(msg)#,
         # additional=...
#     )
#     jsonlite::toJSON(output_list, simplifyVector=TRUE)
#}

