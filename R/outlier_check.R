#' Evaluate and edit outliers from variable
#'
#'  Contains functions to evaluate the data through histogram, QQ, and x-y plots.
#'  contains functions to evaulate the fit of the data in terms of distributions and analysis

#' @requires futile.logger for logging messages
#' @param dat dataframe or matrix
#' @param x column in dataframe
#' @param outlier.mod defines method to ..
#' Choices include:
#'                 none: No data points are removed
#'                 5_95_quant: Removes data points outside the 5th and 95th quantiles
#'                 25_75_quant: Removes data points outside the 25th and 75th quantiles
#'                 mean_3SD: Removes data points outside +/- 3SD of the mean
#'                 median_3SD: Removes data points outside +/- 3SD of the meadian
#' @param plot.dat Print data plots. If TRUE, three plots are returned.
#' @param table.dat Print a summary data table. If TRUE, eight summary measures are printed
#' @param remove Save data with outliers removed. If TRUE, the revised data tables, based on outlier.mod, is returned.
#' @keywords outliers
#' @method
#' @export
#' @return Returns the modified dataframe
#' @details outlier_check is a series of functions allowing users to evaluate the occurrence of outliers and their effect on the data
#' using summary data and plots. Outliers can onlyl be assessed for numeric vectors.

#' @examples
#' Generate data for example. Inject outliers into cars dataset.
#' cars1 <- cbind(rbind(cars[1:30, ], data.frame(speed=c(19,19,20,20,20), dist=c(190, 186, 210, 220, 218))), ID=rep(c('a','b','c','d','e'), 7))
#' nan.identify(my.df)
#' nan.filter(my.df, 'speed')
#' mod.dat <- nan.filter(my.df, 'speed', replace=T)
#' mod.dat <- nan.filter(my.df, 'speed', replace=T, rep.value=0)
#' mod.dat <- nan.filter(my.df, 'speed', remove=T)
#'
#'
#'

# Logging message: Outliers checked. No modifications made
#                                    Outliers deemed present. Values outside outlier.mod removed.

plot_function <- function(){}
table_function <- function(){}
dist_function <- function(){}

outlier_table <- function(dat, x){
     #logging function information
     df.name <- deparse(substitute(dat))
     x.name <- deparse(substitute(x))
     flog_func(dat=df.name, x=x.name, fun.name='outlier_table')
     #Check if data is numeric. Cannot check outliers if not.
     if(is.numeric(dat[, x])==T){
# Output table of summary statistics
          #Row 1 No data removed
          dat.table <- data.frame(Vector=x.name, outlier_check='None', N=length(dat[, x]), mean=mean(dat[, x], na.rm=T), median=median(dat[, x], na.rm=T),
                                  SD=sd(dat[, x]), min=min(dat[, x], na.rm=T), max=max(dat[, x], na.rm=T),
                                  NAs=sum(length(which(is.na(dat[, x])))), skew=skewness(dat[, x], na.rm=T))
           #Row 2 5-95% quantile
            temp <- dat[dat[,x] < quantile(dat[, x], 0.95) & dat[, x] > quantile(dat[, x], 0.05), ]
            dat.table <- rbind(dat.table, data.frame(Vector=x.name, outlier_check='5_95_quant', N=length(temp[, x]), mean=mean(temp[, x], na.rm=T),
                                                     median=median(temp[, x], na.rm=T),
                                                     SD=sd(temp[, x]), min=min(temp[, x], na.rm=T), max=max(temp[, x], na.rm=T),
                                                     NAs=sum(length(which(is.na(temp[, x])))),   skew=skewness(temp[, x], na.rm=T)))
               #Row 3 25-75% quantile
               temp <- dat[dat[, x] < quantile(dat[, x], 0.75) & dat[, x] > quantile(dat[, x], 0.25), ]
               dat.table <- rbind(dat.table, data.frame(Vector=x.name, outlier_check='25_75_quant', N=length(temp[, x]), mean=mean(temp[, x], na.rm=T),
                                                        median=median(temp[, x], na.rm=T), SD=sd(temp[, x]), min=min(temp[, x], na.rm=T), max=max(temp[, x], na.rm=T),
                                                        NAs=sum(length(which(is.na(temp[, x])))), skew=skewness(temp[, x], na.rm=T)))
               # Row 4 Mean +/2SD
               temp <- dat[dat[, x] < (mean(dat[, x],na.rm=T) + 2*sd(dat[, x], na.rm=T)) & dat[, x] > (mean(dat[, x],na.rm=T) - 2*sd(dat[, x], na.rm=T)), ]
               dat.table <- rbind(dat.table, data.frame(Vector=x.name, outlier_check='mean_2SD', N=length(temp[, x]), mean=mean(temp[, x], na.rm=T),
                                                        median=median(temp[, x], na.rm=T), SD=sd(temp[, x]), min=min(temp[, x], na.rm=T),
                                                        max=max(temp[, x], na.rm=T),  NAs=sum(length(which(is.na(temp[, x])))), skew=skewness(temp[, x], na.rm=T)))
               # Row 5 Mean +/3SD
                    temp <- dat[dat[, x] < (mean(dat[, x],na.rm=T) + 3*sd(dat[, x], na.rm=T)) & dat[, x] > (mean(dat[, x],na.rm=T) - 3*sd(dat[, x], na.rm=T)), ]
                    dat.table <- rbind(dat.table, data.frame(Vector=x.name, outlier_check='mean_3SD', N=length(temp[, x]), mean=mean(temp[, x], na.rm=T),
                                                             median=median(temp[, x], na.rm=T), SD=sd(temp[, x]), min=min(temp[, x], na.rm=T),
                                                             max=max(temp[, x], na.rm=T),  NAs=sum(length(which(is.na(temp[, x])))), skew=skewness(temp[, x], na.rm=T)))
                    #Row 6 Median +/-2SD
                    temp <- dat[dat[, x] < (median(dat[, x],na.rm=T) + 2*sd(dat[, x], na.rm=T)) & dat[, x] > (median(dat[, x],na.rm=T) - 2*sd(dat[, x], na.rm=T)), ]
                    dat.table <- rbind(dat.table, data.frame(Vector=x.name, outlier_check='median_2SD', N=length(temp[, x]), mean=mean(temp[, x], na.rm=T),
                                                             median=median(temp[, x], na.rm=T), SD=sd(temp[, x]), min=min(temp[, x], na.rm=T),
                                                             max=max(temp[, x], na.rm=T),  NAs=sum(length(which(is.na(temp[, x])))), skew=skewness(temp[, x], na.rm=T)))
                    #Row 7 Median +/-3SD
                         temp <- dat[dat[, x] < (median(dat[, x],na.rm=T) + 3*sd(dat[, x], na.rm=T)) & dat[, x] > (median(dat[, x],na.rm=T) - 3*sd(dat[, x], na.rm=T)), ]
                         dat.table <- rbind(dat.table, data.frame(Vector=x.name, outlier_check='median_3SD', N=length(temp[, x]), mean=mean(temp[, x], na.rm=T),
                                                                  median=median(temp[, x], na.rm=T), SD=sd(temp[, x]), min=min(temp[, x], na.rm=T),
                                                                  max=max(temp[, x], na.rm=T),  NAs=sum(length(which(is.na(temp[, x])))), skew=skewness(temp[, x], na.rm=T)))
                         return(dat.table)
     } else {
             print("do something")
     }
}

##---------------------------##
outlier_check <- function(dat, x, outlier.mod='none', plot.dat=TRUE, table.dat=TRUE, remove=F){
#logging function information
     df.name <- deparse(substitute(dat))
     x.name <- deparse(substitute(x))
     flog_func(dat=df.name, x=x.name, fun.name='outlier_check')
#Check if data is numeric. Cannot check outliers if not.
     if(is.numeric(dat[, x])==T){
# Begin outlier check
     if(outlier.mod!='none'){
         dat <- dat
     } else {
          if (outlier.mod=='5_95_quant'){
               dat <- dat[dat$x < quantile(dat$x, 0.95) & dat$x > quantile(dat$x, 0.05), ]
              } else if (outlier.mod=='25_75_quant') {
                   dat <- dat[dat$x < quantile(dat$x, 0.75) & dat$x > quantile(dat$x, 0.25), ]
              } else if (outlier.mod=='mean_3SD') {
                   dat <- dat[dat$x < (mean(dat$x,na.rm=T) + 3*sd(dat$x, na.rm=T)) & dat$x > (mean(dat$x,na.rm=T) - 3*sd(dat$x, na.rm=T)), ]
              } else if (outlier.mod='median_3SD') {
                   dat <- dat[dat$x < (median(dat$x,na.rm=T) + 3*sd(dat$x, na.rm=T)) & dat$x > (median(dat$x,na.rm=T) - 3*sd(dat$x, na.rm=T)), ]  -
              }
          }  #End Outlier check
     if(table.dat==T){

     }
     if(plot.data==TRUE){

     }
     if(remove==FALSE){
          return(...)
          flog.trace('Outliers checked in column %s of dataframe %s. No modifications made.', x.name, df.name)
     } else {
          flog.trace('Outliers checked. Outliers deemed present in %s of dataframe %s. Values outside %s removed.', x.name, df.name, outlier.mod)
     }
     } else {
  #Actions to take if data is not numeric
          print('Data is not numeric. Outliers cannot be checked.')
          if(table.dat==TRUE){

          }
          if(plot.data==TRUE){

          }
     }
}