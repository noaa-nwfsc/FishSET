#' Check model data. Save output.
#'
#'  Contains one function that tests for NaNs, Inf, and that each row is unique. The function stops if an if statement does not pass.

#' @param dataset input dataframe or matrix
#' @param uniqueID dataframe that contains information on each column of the dataset
#' @param save.file whether to save as sql table (sqlsave) or csv file (csvsave)
#' @param db.name names of sql database to save dataset into
#' @param save.name name for verified and saved data
#' @return Returns statements as to whether issues in the data may exist
#' @details Checks data to be used for modelling. Checks for presence of NaNs and Inf. The verified data will not saves unless no NaNs or infinite values in the dataset.
#' Verified data can then be saves as sql table or csv file.

#' @examples 
#' checkModelData(MainDataTable, uniqueID='uniqueID_Code', db.name=testdb, save.name='modelData')
#' 
# 

# modification to is.nan, which idenitifies NaNs in a list. This modification extends the search to dataframes and matrices.
is.nan.data.frame <- function(dataset) {
    do.call(cbind, lapply(dataset, is.nan))
}
# modification to is.infinte, which idenitifies Inf in a list. This modification extends the search to dataframes and matrices.
is.inf.data.frame <- function(dataset) {
    do.call(cbind, lapply(dataset, is.infinite))
}



checkModelData <- function(dataset, uniqueID, save.file = "sqlfile", db.name, save.name) {
    # is.nan
    if (length(which(is.nan.data.frame(dataset)) != 0) > 0) {
        stop(paste("NaNs are present in", names(which(colSums(is.nan.data.frame(dataset)) != 0))))
    }
    # is.inf
    if (length(which(is.inf.data.frame(dataset)) != 0) > 0) {
        stop(paste("Infinite values are present in", names(which(colSums(is.inf.data.frame(dataset)) != 0))))
    }
    if (length(dataset[[uniqueID]]) != length(unique(dataset[[unique]]))) {
        stop("The uniqueID variable should define the length of unique occurrences in the dataset. Used the HaulToTrip function to collapse data.")
    }
    if (save.file == "csvfile") {
        write.csv(dataset, paste(save.name, ".csv", sep = ""))
    } else {
        dbWriteTable(db.name, save.name, dataset)
        # logging function information
    }
    df.name <- deparse(substitute(dataset))
    write(layout.json.ed(trace, "checkModelData", df.name, x, msg = paste("Saved as", save.name)), paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""), 
        append = T)
    
    
}
