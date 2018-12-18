#' Create dataframe containining customized filters
#'
#'  Contains a function to to define and store filters
#'  Also contains a function to apply defined filters to dataframe

#'
#' @param dataset dataframe or matrix over which to apply filter
#' @param x column in dataframe over which to filter will be applied
#' @param exp Filter expression. Should take on the form of 'x<100' or is.na(x)==F.
#' @param save.filter Whether to save the filterTable as a csv file
#' @param log.dat Whether to print filterTable to the log file
#' @param use.filter.Table TRUE or FALSE. If true then data is subsetted based on a filter in filterTable and exp is the row containing the filter.
#' @keywords filter, subset
#' @return filter_data returns filterTable into the global environment. The data table will grow with each run of the function.
#' @return filter_dat applies user-definted filters. Output must be saved
#' @details This function allows users to define and store data filters which can then be applied to the data. The filter dataframe can be saved and will be logged in the log file.

#' @examples
#' Generate data for example: outliers inejcted into cars dataset.
#' cars1 <- cbind(rbind(cars[1:30, ], data.frame(speed=c(19,19,20,20,20), dist=c(190, 186, 210, 220, 218))), ID=rep(c('a','b','c','d','e'), 7))
#' filter_data(cars1, 'dist','dist>100')
#' filterTable
#' 
#' newdat <- filter_dat(cars1, exp=1, use.filter.Table=T)
#' newdat <- filter_dat(cars1, exp='dist>100', use.filter.Table=F)


# this function generates a dataframe containing defined filters the dataframe is saved to the global environment and is called in the apply filter
# function
filter_data <- function(dataset, x, exp, save.filter = FALSE, log.dat = TRUE) {
    # df.name <- deparse(substitute(dataset)) x.name <- noquote(x)
    if (exists("filterTable") == F) {
        # flog_func(data=df.name, x='all',fun.name='filter_data')
        filterTable <- data.frame(datframe = NA, vector = NA, FilterFunction = NA)
        filterTable[1, ] <- c(df.name, x.name, exp)
    } else {
        filterTable <- rbind(filterTable, c(df.name, x.name, exp))
    }
    if (save.filter == TRUE) {
        write.csv(filterTable, paste(filterTable, "_", df.name, ".csv"), sep = F, row.names = FALSE)
        # flog.trace(filterTable)
    }
    if (flog.dat == TRUE & save.filter == FALSE) {
        # flog.trace(filterTable, name='file_save')
        write.table(filterTable, file = paste("Logs/ Log_file_", Sys.Date(), ".log"))
    }
    # return(filterTable)
    assign("filterTable", filterTable, envir = .GlobalEnv)
    print(filterTable)
}


# Remove rows based on user-defined filter
filter_dat <- function(dataset, exp, use.filter.Table = F) {
    # logging function information
    df.name <- deparse(substitute(dataset))
    # flog_func(data=df.name, x='user-defined', fun.name='filter_dat') Checking for NaNs only occurs on Numeric Variables
    if (use.filter.Table == T) {
        write(layout.json.ed(trace, "filter_dat", df.name, x = "user-defined", msg = paste("Rows have been removed based on", filterTable[exp, 3])), paste(getwd(), 
            "/Logs/", Sys.Date(), ".json", sep = ""), append = T)
        cat("The entire row will be removed from the dataframe.")
        # flog.trace('All rows in which %s is note true have been removed from dataframe %s', filterTable[exp,3], df.name, name='file_save')
        dataset <- subset(dataset, eval(parse(text = filterTable[exp, 3])))
        return(dataset)
    } else {
        dataset <- subset(dataset, eval(parse(text = exp)))
        write(layout.json.ed(trace, "filter_dat", df.name, x = "user-defined", msg = paste("Rows have been removed based on", exp)), paste(getwd(), "/Logs/", 
            Sys.Date(), ".json", sep = ""), append = T)
    }
}

