#' Collapse the dataframe from haul to trip.
#' Each row should be a unique trip. Reduces the number of rows.
#' Unique trips are defined based on selected column(s). For example, landing permit number and disembared port. This id column is used to collapse the data to trip level.
#'
#'
#' @param dataset dataframe or matrix over which to apply filter
#' @param dataindex Dataframe that contains information on each column of the dataset
#' @param varnameindex The column in dataindex that contains the column names of dataset
#' @param genTypeName column in dataindex containing information on the general category of each column in dataset (time, numeric, etc)
#' @param fun.time Time units for function. Defaults to minutes
#' @param fun.numeric Defaults to mean
#' @param ... Column(s) that define the individual trip.
#' @return Dataframe with each row representing a trip or haul
# @examples

# dat <- HaulToTrip(MainDataTable, MainDataTableInfo,'variable_name','generalType','PERMIT','DISEMBARKED_PORT')



HaulToTrip <- function(dataset, dataindex, varnameindex, genTypeName, fun.time = min, fun.numeric = mean, ...) {
    # Create a column that indicates unique trip levels
    argList <- (as.character(match.call(expand.dots = FALSE)$...))
    
    idmaker = function(vec) {
        return(paste(sort(vec), collapse = ""))
    }
    int <- as.data.frame(cbind(dataset, rowID = as.numeric(factor(apply(as.matrix(dataset[, eval(substitute(argList))]), 1, idmaker)))))
    int <- int[, c(colnames(sapply(dataindex[[varnameindex]], grepl, colnames(int))), "rowID")]
    
    # Handling of empty variables
    if (any(apply(int, 2, function(x) all(is.na(x))) == TRUE)) {
        int <- int[, -which(apply(int, 2, function(x) all(is.na(x))) == TRUE)]
        # print(paste(names(which(apply(int, 2, function(x) all(is.na(x)))==TRUE))[1], 'is empty and was removed the dataset.'))
    } else {
        int <- int
    }
    
    # Collapse data based on rowID and defined function
    lapply(1:(ncol(int) - 1), function(x) if (is.na(as.data.frame(dataindex[dataindex[, varnameindex] == colnames(int)[x], genTypeName])) == 
        TRUE) {
      out <- aggregate(. ~ rowID, data.frame(int[c("rowID", colnames(int)[x])]), FUN = head, 1)
    } else if (as.data.frame(dataindex[dataindex[, varnameindex] == colnames(int)[x], genTypeName]) == "Time") {
      out <- aggregate(. ~ rowID, data.frame(int[c("rowID", colnames(int)[x])]), fun.time, na.action = na.pass)
    } else if (as.data.frame(dataindex[dataindex[, varnameindex] == colnames(int)[x], genTypeName]) == "Other Numeric") {
      out <- aggregate(. ~ rowID, data.frame(int[c("rowID", colnames(int)[x])]), fun.numeric, na.action = na.pass)
    } else if (as.data.frame(dataindex[dataindex[, varnameindex] == colnames(int)[x], genTypeName]) == "Latitude") {
      out <- aggregate(. ~ rowID, data.frame(int[c("rowID", colnames(int)[x])]), FUN = head, 1)
    } else {
      out <- aggregate(. ~ rowID, data.frame(int[c("rowID", colnames(int)[x])]), FUN = head, 1)
    })
    out <- as.data.frame(out)
    # print(head(results))
    out <- out[, -grep("rowID.", names(out), value = FALSE)]
    # print(head(results))write_lines(layout.json.ed(trace, 'outlier_remove', dataset, x, msg=paste('outliers removed using', outlier.mod)),
    write_lines(layout.json.ed(trace, "HaulToTrip", dataset, x = "", msg = paste("dataindex:", dataindex, "varnameindex:", varnameindex, "genTypeName:", 
        genTypeName, "fun.time:", fun.time, "fun.numeric:", fun.numeric)), paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""), append = T)
    return(out)
}
