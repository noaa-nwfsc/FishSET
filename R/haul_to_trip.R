#' Collapse the dataframe from haul to trip.
#'
#' @param dat Data frame containing haul level data. In the FishSET database, the table will contain the phrase `MainDataTable`
#' @param project Name of project
#' @param fun.time Numeric function defining how to collapse temporal data. For example, min, mean, max. Cannot be sum for temporal variables.
#' @param fun.numeric Numeric function defining how to collapse numeric or temporal data. For example, min, mean, max, sum. Defaults to mean.
#' @param ... Column(s) that identify the individual trip.
#' @export haul_to_trip
#' @return Data frame with each row representing a trip or haul
#' @details Collapses the main data table from haul to trip level. Requires the MainDataTableInfo table associated with the main data table. The MainDataTableInfo table is first updated
#' using the dataindex_update function. Unique trips are defined based on selected column(s). For example, landing permit number and disembarked port.
#'  This id column is used to collapse the data to trip level. Users can define how time and numeric variables with multiple observations for a trip are collapsed.
#' For instance, the mean value of the numeric observations. For non-numeric and non-time variables, the first observation is used.
#' 
#' @examples
#' \dontrun{
#'  pollockMainDataTable <- haul_to_trip('pollockMainDataTable', 'pollock',
#'                                      min,mean,'PERMIT','DISEMBARKED_PORT')
#'  }


haul_to_trip <- function(dat, project, fun.numeric = mean, fun.time = mean, ...) {
    # fun.time = min, Create a column that indicates unique trip levels
    
    # Call in datasets
    out <- data_pull(dat)
    dat <- out$dat
    dataset <- out$dataset
    
    # Load in dataindex
    
    dataIndex <- dataindex_update(dataset, pull_info_data(project))
    # suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase)) single_sql <- paste('select * from ', dataindex, sep='') dataindex <-
    # DBI::dbGetQuery(fishset_db, single_sql) DBI::dbDisconnect(fishset_db)
    
    if (grepl("input", as.character(match.call(expand.dots = FALSE)$...)[1]) == TRUE) {
        argList <- eval(...)
    } else {
        argList <- (as.character(match.call(expand.dots = FALSE)$...))
    }
    
    idmaker = function(vec) {
        return(paste(sort(vec), collapse = ""))
    }
    int <- as.data.frame(cbind(dataset, rowID = as.numeric(factor(apply(as.matrix(dataset[, eval(substitute(argList))]), 1, idmaker)))))
    # int <- int[, c(colnames(sapply(dataindex[[varnameindex]], grepl, colnames(int))), 'rowID')]
    cat(length(unique(int$rowID)), "unique trips were identified using", argList, "\n")
    # Handling of empty variables
    if (any(apply(int, 2, function(x) all(is.na(x))) == TRUE)) {
        int <- int[, -which(apply(int, 2, function(x) all(is.na(x))) == TRUE)]
    } else {
        int <- int
    }
    
    
    drop <- 0
    if (length(grep("DUR", names(int[, c(which(as.data.frame(dataIndex[dataIndex[, "variable_name"] == colnames(int[, -which(colnames(int) == "rowID")]), 
        "generalType"]) == "Time"))]), ignore.case = T)) != 0) {
        drop <- grep("DUR", names(int[, c(which(as.data.frame(dataIndex[dataIndex[, "variable_name"] == colnames(int[, -which(colnames(int) == "rowID")]), 
            "generalType"]) == "Time"))]), ignore.case = T)
    }
    # Collapse data based on rowID and defined function
    out <- data.frame(drop = rep(0, length(unique(int$rowID))))
    
    # Nothing listed
    if (length(which(is.na(as.data.frame(dataIndex[dataIndex[, "variable_name"] %in% colnames(int[, -which(colnames(int) == "rowID")]), "generalType"]) == 
        TRUE))) > 0) 
        out <- cbind(out, stats::aggregate(int[, c(which(is.na(as.data.frame(dataIndex[dataIndex[, "variable_name"] %in% colnames(int[, -which(colnames(int) == 
            "rowID")]), "generalType"]) == TRUE)), which(colnames(int) == "rowID"))], list(int$rowID), FUN = head, 1)[, -1])
    
    # Time - not duration
    if (length(which(as.data.frame(dataIndex[dataIndex[, "variable_name"] %in% colnames(int[, -which(colnames(int) == "rowID")]), "generalType"]) == "Time")) > 
        0) {
        out2 <- suppressWarnings(stats::aggregate(cbind(as.data.frame(lapply(if (drop > 0) {
            int[, c(which(as.data.frame(dataIndex[dataIndex[, "variable_name"] %in% colnames(int[, -which(colnames(int) == "rowID")]), "generalType"]) == 
                "Time"))][-drop]
        } else {
            as.data.frame(int[, c(which(dataIndex[dataIndex[, "variable_name"] %in% colnames(int[, -which(colnames(int) == "rowID")]), "generalType"] == 
                "Time"))])
        }, date_parser)), rowID = int$rowID), list(int$rowID), match.fun(fun.time), na.rm = T))[, -1]
    }
    names(out2)[1:dim(out2)[2] - 1] <- names(int)[which(dataIndex[dataIndex[, "variable_name"] %in% colnames(int[, -which(colnames(int) == "rowID")]), 
        "generalType"] == "Time")]
    out <- cbind(out, out2)
    # Time - duration
    if (length(which(as.data.frame(dataIndex[dataIndex[, "variable_name"] %in% colnames(int[, -which(colnames(int) == "rowID")]), "generalType"]) == "Time")) > 
        0 & any(grepl("dur", colnames(int)[which(as.data.frame(dataIndex[dataIndex[, "variable_name"] %in% colnames(int[, -which(colnames(int) == "rowID")]), 
        "generalType"]) == "Time")], ignore.case = TRUE)) == TRUE) {
        out <- cbind(out, suppressWarnings(stats::aggregate(cbind(as.data.frame(int[, c(which(as.data.frame(dataIndex[dataIndex[, "variable_name"] %in% 
            colnames(int[, -which(colnames(int) == "rowID")]), "generalType"]) == "Time"))][-grep("dur", names(int[, c(which(as.data.frame(dataIndex[dataIndex[, 
            "variable_name"] %in% colnames(int[, -which(colnames(int) == "rowID")]), "generalType"]) == "Time"))]), ignore.case = T, invert = TRUE)]), 
            rowID = int$rowID), list(int$rowID), match.fun(fun.numeric), na.rm = TRUE))[, -1])
    }
    # Other numeric
    if (length(which(as.data.frame(dataIndex[dataIndex[, "variable_name"] %in% colnames(int[, -which(colnames(int) == "rowID")]), "generalType"]) == "Other Numeric")) > 
        0) {
        out <- cbind(out, stats::aggregate(int[, c(which(as.data.frame(dataIndex[dataIndex[, "variable_name"] %in% colnames(int[, -which(colnames(int) == 
            "rowID")]), "generalType"]) == "Other Numeric"), which(colnames(int) == "rowID"))], list(int$rowID), match.fun(fun.numeric), na.rm = T)[, 
            -1])
    }
    # Latitude
    if (length(which(as.data.frame(dataIndex[dataIndex[, "variable_name"] %in% colnames(int[, -which(colnames(int) == "rowID")]), "generalType"]) == "Latitude")) > 
        0) {
        out <- cbind(out, stats::aggregate(int[, c(which(as.data.frame(dataIndex[dataIndex[, "variable_name"] %in% colnames(int[, -which(colnames(int) == 
            "rowID")]), "generalType"]) == "Latitude"), which(colnames(int) == "rowID"))], list(int$rowID), FUN = head, 1)[, -1])
    }
    # Coded numeric
    if (length(which(as.data.frame(dataIndex[dataIndex[, "variable_name"] %in% colnames(int[, -which(colnames(int) == "rowID")]), "generalType"]) == "Code Numeric")) > 
        0) {
        out <- cbind(out, stats::aggregate(int[, c(which(as.data.frame(dataIndex[dataIndex[, "variable_name"] %in% colnames(int[, -which(colnames(int) == 
            "rowID")]), "generalType"]) == "Code Numeric"), which(colnames(int) == "rowID"))], list(int$rowID), FUN = head, 1)[, -1])
    }
    # Coded
    if (length(which(as.data.frame(dataIndex[dataIndex[, "variable_name"] %in% colnames(int[, -which(colnames(int) == "rowID")]), "generalType"]) == "Code")) > 
        0) {
        out <- cbind(out, stats::aggregate(int[, c(which(as.data.frame(dataIndex[dataIndex[, "variable_name"] %in% colnames(int[, -which(colnames(int) == 
            "rowID")]), "generalType"]) == "Code"), which(colnames(int) == "rowID"))], list(int$rowID), FUN = head, 1)[, -1])
    }
    # Coded string
    if (length(which(as.data.frame(dataIndex[dataIndex[, "variable_name"] %in% colnames(int[, -which(colnames(int) == "rowID")]), "generalType"]) == "Code String")) > 
        0) {
        out <- cbind(out, stats::aggregate(int[, c(which(as.data.frame(dataIndex[dataIndex[, "variable_name"] %in% colnames(int[, -which(colnames(int) == 
            "rowID")]), "generalType"]) == "Code String"), which(colnames(int) == "rowID"))], list(int$rowID), FUN = head, 1)[, -1])
    }
    # Not in the dataIndex file
    if (length(colnames(int[, -grep("rowID", colnames(int[, -grep("rowID", colnames(int))]))])[!(colnames(int[, -grep("rowID", colnames(int))]) %in% dataIndex[, 
        "variable_name"])]) > 0) {
        out <- cbind(out, stats::aggregate(int[, c(colnames(int)[!(colnames(int) %in% dataIndex[, "variable_name"])])], list(int$rowID), FUN = head, 1)[, 
            -1])
    }
    
    
    out <- out[-which(colnames(out) == "rowID")[-length(which(colnames(out) == "rowID"))]]
    out <- out[-which(colnames(out) == "drop")]
    out <- data.frame(out)
    
    
    haul_to_trip_function <- list()
    haul_to_trip_function$functionID <- "haul_to_trip"
    haul_to_trip_function$args <- c(dat, deparse(substitute(dataindex)))
    haul_to_trip_function$kwargs <- list(deparse(substitute(fun.numeric)), ...)
    haul_to_trip_function$output <- c(dat)
    log_call(haul_to_trip_function)
    
    return(out)
}


