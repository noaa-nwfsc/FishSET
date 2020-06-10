data_verification_call <- function(dat, project) {
    #' Checks for common issues with data
    #' @param dat Main data frame over which to apply function. Table in FishSET database should contain the string `MainDataTable`.
    #' @param project Name of project
    #' @return Returns statements as to whether issues in the data may exist
    #' @export data_verification_call
    #' @keywords internal
    #' @importFrom maps map
    #' @details  Contains one function that checks all columnn names in the dataset are unique, whether any columns in the dataset are empty, 
    #' whether each row is a unique choice occurrence at the haul or trip level, and that data for either lat/long or fishing area are included.
    #' Main data table is not saved to fishset_db database if any tests fail.
    #' @examples
    #' \dontrun{
    #' data_verification_call(pollockMainDataTable, 'pollock')
    #' }
    
    
    # Call in datasets
    out <- data_pull(dat)
    dat <- out$dat
    dataset <- out$dataset
    
    
    tmp <- tempfile()
    
    cat("Data verification checks for", project, 'using', dat, 'dataset on', format(Sys.Date(), format = "%Y%m%d"), file = tmp, append = TRUE)
    check <- 0
    # check that names are unique in dataset
    x <- colnames(dataset)
    if (length(x) == length(unique(x)) & length(toupper(x)) == length(unique(toupper(x)))) {
        cat("\nPass: Variable names are unique within dataset..", file = tmp, append = T)
    } else if (length(x) == length(unique(x)) & length(toupper(x)) != length(unique(toupper(x)))) {
        cat("\nData set will not be saved to database. Duplicate case-insensitive colnames. Sqlite column names are case insensitive.", file = tmp, append = T)
        check <- 1
    } else {
        cat("\nVariable names are not unique.\n", file = tmp, append = T)
        check <- 1
        # stop('Variable names must be unique.\n')
    }
    
    
    # check each row of data is a unique choice occurrence at haul or trip level
    if (dim(dataset)[1] == dim(unique(dataset))[1]) {
        cat("\nPass: Each row is a unique choice occurrence.", file = tmp, append = T)
    } else {
        cat("\nEach row in dataset is not a unique choice occurrence at haul or trip level.", file = tmp, append = T)
        # stop('Each row in dataset must be a unique choice occurrence at haul or trip level.')
        check <- 1
    }
    
    # Handling of empty variables
    if (any(apply(dataset, 2, function(x) all(is.na(x))) == TRUE)) {
        cat("\n", names(which(apply(dataset, 2, function(x) all(is.na(x))) == TRUE)), "is empty. Consider removing the column from the dataset.", file = tmp, 
            append = T)
    } else {
        cat("\nPass: No empty variables exist in the dataset.", file = tmp, append = TRUE)
    }
    
    if (any(grepl("lat|lon", names(dataset), ignore.case = TRUE))) {
        lat <- dataset[, grep("lat", names(dataset), ignore.case = TRUE)]
        lon <- dataset[, grep("lon", names(dataset), ignore.case = TRUE)]
        
        if (is.factor(lat)) {
            lat <- as.numeric(as.character(lat))
        }
        if (is.factor(lon)) {
            lon <- as.numeric(as.character(lon))
        }
        # graphics::par(mar=c(1,1,1,1)) maps::map('world', ylim=c(min(lat, na.rm=TRUE), max(lat, na.rm=TRUE)), xlim=c(min(lon, na.rm=TRUE), max(lon,
        # na.rm=TRUE))) pts <- sample(nrow(dataset), nrow(dataset)/10) points(as.numeric(as.character(dataset[pts, grep('lon', names(dataset),
        # ignore.case=TRUE)[1]])), as.numeric(as.character(dataset[pts, grep('lat', names(dataset), ignore.case=TRUE)[1]]))) print('10% of samples plotted.
        # Verify that points occur in correct geographic area.')
    }
    
    print(suppressWarnings(readLines(tmp)))
    
    rm(tmp)
    
    if (check == 1) {
        warning("Data cannot be saved, at least one error exists")
    }
    
    # save_table(dataset, project, 'data_verification_call')
    
}
