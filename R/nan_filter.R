#' Filters NaN's from variable
#'
#'  Contains a function to identify which columns in a dataframe or matrix contain NaNs and a function to return a modified dataframe where NaNs
#'  have been replaced or removed.

#'
#' @param dat dataframe or matrix over which to check for NaNs
#' @param x column in dataframe over which to remove or replace NaNs
#' @param replace whether to (TRUE) or not to (FALSE) replace NaNs in a column. Defaults to FALSE.
#' @param remove whether to (TRUE) or not to (FALSE) remove all remove the entire row of the dataframe where NaN is present in a specified column. Defaults to FALSE.
#' @param rep.value value to replace all NaNs in a column. Defaults to the mean value of the column.
#' @keywords NaN
#' @method
#' @export
#' @return Returns the modified dataframe
#' @details nan_filter is composed of three functions. is.nan.data.frame is a modification to is.nan to allow for seeking any occurrences of NaNs in a dataframe.
#' is.nan only works on lists. na.identify is used to identify if NaNs exist in a dataframe and which dataframes they occur in.
#' It is intended to aid in deciding how to handle NaNs. nan.filter is the function use to replace or remove NaNs.

#' @examples
#' my.df <- data.frame(length=c(1, 2, 3,4), height=c(NaN, 2, NaN,10), age=c(1,2,4,6),ID=c('a',NA,'c','d'),speed=c(NaN,NA,44,40))
#' nan.identify(my.df)
#' nan.filter(my.df, 'speed')
#' mod.dat <- nan.filter(my.df, 'speed', replace=T)
#' mod.dat <- nan.filter(my.df, 'speed', replace=T, rep.value=0)
#' mod.dat <- nan.filter(my.df, 'speed', remove=T)

#modification to is.nan, which idenitifies NaNs in a list. This modification extends the search to dataframes and matrices.
is.nan.data.frame <- function (dat) { do.call(cbind, lapply(dat, is.nan))
     }

# this function identifies whether a dataset contains NaNs and returns the column names containing the NaNs and the number of NaNs in each column
nan.identify <- function (dat) {
     df.name <- deparse(substitute(dat))
     write(layout.json.ed(trace, 'nan.identify',df.name, 'all'), paste('~/FistSET_RPackage/Logs/Log_file',Sys.Date(),'.json'), append=T)
    if (length(which(is.nan.data.frame(dat))!=0) > 0){
              flog.info('The %s columns contain %s NaNs. Consider using nan.filter to replace or remove NaNs',
                               names(which(colSums(is.nan.data.frame(dat))!=0)),
                     unname(which(colSums(is.nan.data.frame(dat))!=0)), name='file_both'
                     )
         } else {
              flog.info('No columns in the dataframe contain NaNs', name='file_both')
         }
    }

# replaces nans in the dataColumn with the choosen value or removes rows containing NaNs
nan.filter <- function (dat, x, replace=F, remove=F, rep.value = mean(dat[, x], na.rm=T)) {
     #logging function information
     df.name <- deparse(substitute(dat))
     x.name <- deparse(substitute(x))
     flog_func(dat=df.name, x=x.name, fun.name='nan.filter')
     #Checking for NaNs only occurs on Numeric Variables
         if (is.numeric(dat[, x])==T){
              #Further actions are only taken if NaNs exist in the selected variable
              if (any(is.nan(dat[, x]))==T){
                   cat(length(which(is.nan(dat[, x])==T)), 'NaNs identified in variable', x, '.\n')
                   flog.info('%s NaNs identified in variable %s',length(which(is.nan(dat[, x])==T)), x.name, name='file_both')
                   #If replace is true then NaNs are replaced with the identified rep.value (defaults to mean value)
                   if(replace==T){
                        dat[is.nan(dat[, x]), x] = rep.value
                        flog.info('All NaNs in %s have been replaced with %s', x.name, rep.value, name='file_both')
                        write(layout.json.ed(trace, 'nan.filter',df.name, x.name, 
                                             msg=paste(df.name,"[is.nan(",df.name,"[,", x.name,"]),",x.name,"] = ",rep.value)), 
                                             paste('~/FistSET_RPackage/Logs/Log_file',Sys.Date(),'.json'), append=T)
                        
                        return(dat)
                     #If remove is true then row inwhich the NaN occurs for selected column will be removed.
                   } else if(remove==T) {
                        cat('The entire row will be removed from the dataframe.')
                        flog.trace('All rows in which NaNs exist in column %s have been removed from dataframe %s', x.name, df.name, name='file_save')
                       dat <- dat[!is.nan(dat[, x]), ]
                       return(dat)
                   }
              } else {
                   flog.info('No NaNs present in variable %s', x.name, name='file_both')
              }
         } else {
     #Message returned if the selected variable is not numeric
              return('Variable is not numeric. Function not applied')
         }
}

