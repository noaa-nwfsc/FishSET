#' Check for serveral issues that may be present in the data.
#'
#'  Contains one function that tests several if statements. The function stops if a if statement does not pass.


#' @param dataset dataframe or matrix
#' @param dataindex dataframe that contains information on each column of the dataset

#' @return Returns statements as to whether issues in the data may exist
#' @details dataVerification checks that all columnn names in the dataset are unique, whether specialized variables have been identified in the index 
#' dataset, whether any columns in the dataset are empty, if units are defined and recognized, whether each row is a unique choice occurrence at the haul
#' or trip level, and that data for either lat/long or fishing area are included.

#' @examples 
#' dataVerification(MainDataTable, MainDataTableInfo)

####----> NOTE CHECK ON WHAT CORRECT LAT/LON FORMAT IS <-----#####
dataVerification <- function(dataset, dataindex) {
    
    # check that names are unique in dataset
    x <- colnames(dataset)
    if (length(x) == length(unique(x))) {
        print("Variable names are unique within dataset.")
    } else {
        stop("Variable names must be unique. ")
    }
    
    # check each row of data is a unique choice occurrence at haul or trip level
    if (dim(dataset)[1] == dim(unique(dataset))[1]) {
        print("Each row is a unique choice occurrence.")
    } else {
        stop("Each row in dataset must be a unique choice occurrence at haul or trip level. ")
    }
    
    # Check to see if lat/long or fish area is in dataset
    indx <- grepl("lat|lon|area", colnames(dataset), ignore.case = TRUE)
    if (length(dataset[indx]) > 0) {
        print("Latitude and longitude or fishing area included in the dataset.")
    } else {
        stop("Dataset must contain either latitude and longitude or fishing area designation. ")
    }
    
    # Identify if any specialized variables are not identified
    allNecFields = c("name", "units", "general", "XY", "ID", "Time", "Catch", "Effort", "CPUE", "Lat", "Value", "Area", "Port", "Price", "Trip", "Haul", 
        "Other")
    indx <- sapply(allNecFields, grepl, colnames(dataindex), ignore.case = TRUE) %>% colSums()
    
    if (length(which(colSums(sapply(allNecFields, grepl, colnames(dataindex))) == 0)) < 1) {
        print("All specialized variables indentified.")
    } else {
        print(paste("The following specialized variables are not specified:", names(which(colSums(sapply(allNecFields, grepl, colnames(dataindex))) == 0))))
    }
    
    # Handling of empty variables
    if (any(apply(dataset, 2, function(x) all(is.na(x))) == TRUE)) {
        print(paste(names(which(apply(dataset, 2, function(x) all(is.na(x))) == TRUE)), "is empty. Consider removing the column from the dataset."))
    } else {
        print("No empty variables exist in the dataset.")
    }
    
    # Units are sensible
    unitsAvailable <- c("Day of Month", "Day of Year", "Decimal Degree", "Dollar", "Fathom", "Feet", "Horsepower", "ID", "Kilometer", "Pound", "lbs", "Meter", 
        "Metric Ton", "Mile", "Minute", "mm", "mmdd", "N/A", "Numeric", "Percent", "Tonne", "Week", "wk", "Y/N", "T/F", "yyyymmdd", "yyyy", "yyyy-mm-dd HH:MM:SS", 
        "mm-dd-yyyy HH:MM:SS", "mm/dd/yyyy HH:MM:SS", "yyyy/mm/dd HH:MM:SS", "min")
    indx <- colSums(sapply(unitsAvailable, grepl, colnames(dataindex), ignore.case = TRUE))
    if (length(which(colSums(sapply(unitsAvailable, grepl, colnames(dataindex))) == 0))) {
        print("All units are specified.")
    } else {
        print(paste("The units are not recognized for the following variables:", which(colSums(sapply(unitsAvailable, grepl, colnames(dataindex))) == 0)))
    }
}
