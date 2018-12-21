#' Filters NaN's from variable
#'
#'  Contains a function to identify which columns in a dataframe or matrix contain NaNs and a function to return a modified dataframe where NaNs
#'  have been replaced or removed.

#'
#' @param dataset dataframe or matrix over which to check for NaNs
#' @param x column in dataframe over which to remove or replace NaNs
#' @param replace whether to (TRUE) or not to (FALSE) replace NaNs in a column. Defaults to FALSE.
#' @param remove whether to (TRUE) or not to (FALSE) remove all remove the entire row of the dataframe where NaN is present in a specified column. Defaults to FALSE.
#' @param rep.value value to replace all NaNs in a column. Defaults to the mean value of the column.
#' @keywords NaN
#' @return Returns the modified dataframe
#' @details nan_filter is composed of three functions. is.nan.data.frame is a modification to is.nan to allow for seeking any occurrences of NaNs in a dataframe.
#' is.nan only works on lists. na.identify is used to identify if NaNs exist in a dataframe and which dataframes they occur in.
#' It is intended to aid in deciding how to handle NaNs. nan.filter is the function use to replace or remove NaNs.

# @examples
# my.df <- data.frame(length=c(1, 2, 3,4), height=c(NaN, 2, NaN,10), age=c(1,2,4,6),ID=c('a',Inf,'c','d'),speed=c(NaN,NA,44,40))
# nan.identify(my.df)
# nan.filter(my.df, 'speed')
# mod.dat <- nan.filter(my.df, 'speed', replace=T)
# mod.dat <- nan.filter(my.df, 'speed', replace=T, rep.value=0)
# mod.dat <- nan.filter(my.df, 'speed', remove=T)

# modification to is.nan, which idenitifies NaNs in a list. This modification extends the search to dataframes and matrices.
is.nan.data.frame <- function(dataset) {
    do.call(cbind, lapply(dataset, is.nan))
}

# this function identifies whether a dataset contains NaNs and returns the column names containing the NaNs and the number of NaNs in each column
nan.identify <- function(dataset) {
    df.name <- deparse(substitute(dataset))
    write(layout.json.ed(trace, "nan.identify", df.name, "all"), paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""), append = T)
    if (length(which(is.nan.data.frame(dataset)) != 0) > 0) {
        cat("The", names(which(colSums(is.nan.data.frame(dataset)) != 0)), "columns contain", unname(which(colSums(is.nan.data.frame(dataset)) != 0)), "NaNs. Consider using nan.filter to replace or remove NaNs")
    } else {
        paste("No columns in the dataframe contain NaNs")
    }
}

# replaces nans in the dataColumn with the choosen value or removes rows containing NaNs
nan.filter <- function(dataset, x, replace = F, remove = F, rep.value = mean(dataset[, x], na.rm = T)) {
    # logging function information
    df.name <- deparse(substitute(dataset))
    x.name <- deparse(substitute(x))
    # flog_func(dataset=df.name, x=x.name, fun.name='nan.filter') Checking for NaNs only occurs on Numeric Variables
    if (is.numeric(dataset[, x]) == T) {
        # Further actions are only taken if NaNs exist in the selected variable
        if (any(is.nan(dataset[, x])) == T) {
            cat(length(which(is.nan(dataset[, x]) == T)), "NaNs identified in variable", x, ".\n")
            # flog.info('%s NaNs identified in variable %s',length(which(is.nan(dataset[, x])==T)), x.name, name='file_both') If replace is true then NaNs are
            # replaced with the identified rep.value (defaults to mean value)
            if (replace == T) {
                dataset[is.nan(dataset[, x]), x] = rep.value
                cat("All NaNs in", x.name, "have been replaced with", rep.value)
                write(layout.json.ed(trace, "nan.filter", df.name, x.name, msg = paste(df.name, "[is.nan(", df.name, "[,", x.name, "]),", x.name, "] = ", 
                  rep.value)), paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""), append = T)
                
                return(dataset)
                # If remove is true then row inwhich the NaN occurs for selected column will be removed.
            } else if (remove == T) {
                cat("The entire row will be removed from the dataframe.")
                # flog.trace('All rows in which NaNs exist in column %s have been removed from dataframe %s', x.name, df.name, name='file_save')
                dataset <- dataset[!is.nan(dataset[, x]), ]
                return(dataset)
            }
        } else {
            cat("No NaNs present in variable", x.name)
        }
    } else {
        # Message returned if the selected variable is not numeric
        return("Variable is not numeric. Function not applied")
    }
}

