#' Check for common data quality issues that may be present in the data set.
#'
#' Function tests for common data quality issues.
#' @param dat Primary data containing information on hauls or trips. 
#' Table in FishSET database contains the string 'MainDataTable'.
#' @param project String, name of project.
#' @return Statements as to whether data quality issues may exist.
#' @keywords internal
#' @importFrom stringi stri_count_regex
#' @export data_verification
#' @details Checks that all columnn names in the data frame are unique, whether any columns in the data frame are empty, 
#' whether each row is a unique choice occurrence at the haul or trip level, and that either latitude and longitude or 
#' fishing area are included.
#' @examples 
#' \dontrun{ 
#' data_verification(MainDataTable)
#' }

data_verification <- function(dat, project) {
    
    # Call in datasets Call in datasets
    out <- data_pull(dat)
    dat <- out$dat
    dataset <- out$dataset
    
    check <- 0
    
    tmp <- tempfile()
    cat("Data verification checks for", project, 'project using', dat, 'dataset on', format(Sys.Date(), format = "%Y%m%d"), file = tmp, append = TRUE)
    
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
        cat("\n", names(which(apply(dataset, 2, function(x) all(is.na(x))) == TRUE)), "is empty. 
        Consider removing the column from the dataset.", 
            file = tmp, append = T)
    } else {
        cat("\nPass: No empty variables exist in the dataset.", file = tmp, append = TRUE)
    }
    
    if (any(grepl("lat|lon", names(dataset), ignore.case = TRUE))) {
        lat <- dataset[, grep("lat", names(dataset), ignore.case = TRUE)]
        lon <- dataset[, grep("lon", names(dataset), ignore.case = TRUE)]
        if (any(sapply(lat, is.numeric) == FALSE) | any(sapply(lon, is.numeric) == FALSE)) {
            cat("At least one lat/lon variable is not in decimal degrees. Use the function degree() to convert to decimal degrees.")
        } else if (any(nchar(abs(trunc(lat[, which(sapply(lat, is.numeric))]))) > 2) | any(nchar(abs(trunc(lon[, which(sapply(lat, is.numeric))]))) > 3)) {
            cat("At least one lat/lon variable is not in decimal degrees. Use the function degree() to convert to decimal degrees.")
        } else {
            cat("\nPass: lat/lon variables in decimal degrees.")
        }
    }
    
    
    graphics::par(mar = c(1, 1, 1, 1))
    longitude <- which(stringi::stri_count_regex(colnames(dataset), "(?=LON|Lon|lon)", ignore.case = TRUE) == max(stringi::stri_count_regex(colnames(dataset), 
        "(?=LON|Lon|lon)", ignore.case = TRUE)))[1]
    latitude <- which(stringi::stri_count_regex(colnames(dataset), "(?=LAT|Lat|lat)", ignore.case = TRUE) == max(stringi::stri_count_regex(colnames(dataset), 
        "(?=LAT|Lat|lat)", ignore.case = TRUE)))[1]
    maps::map("world", ylim = c(min(dataset[, latitude], na.rm = TRUE), max(dataset[, latitude], na.rm = TRUE)), xlim = c(min(dataset[, longitude], na.rm = TRUE), 
        max(dataset[, longitude], na.rm = TRUE)))
    pts <- sample(nrow(dataset), nrow(dataset)/10)
    points(as.numeric(as.character(dataset[pts, longitude])), as.numeric(as.character(dataset[pts, latitude])))
    print("10% of samples plotted. Verify that points occur in correct geographic area.")
    
    
    
    print(suppressWarnings(readLines(tmp)))
    
    
    data_verification_function <- list()
    data_verification_function$functionID <- "data_verification"
    data_verification_function$args <- list(dat, project)
    data_verification_function$kwargs <- list()
    data_verification_function$msg <- suppressWarnings(readLines(tmp))
    log_call(data_verification_function)
    rm(tmp)
    
    if (check == 1) {
        stop("At least one error exists")
    }
    
    
}


### Check that all observations are unique
unique_filter <- function(dat, project, remove = FALSE) {
    #' Check rows are unique
    #'
    #' Check for and remove non-unique rows from primary dataset.
    #'
    #' @param dat Primary data containing information on hauls or trips. 
    #' Table in FishSET database contains the string 'MainDataTable'.
    #' @param project Sring, name of project.
    #' @param remove Logical, if TRUE removes non-unique rows. Defaults to FALSE.
    #' @details Output is determined by \code{remove}. If \code{remove} is TRUE then 
    #' non-unique rows are removed. If \code{remove} is FALSE then only a statement is 
    #' returned regarding the number of rows that are not unique. 
    #' @keywords unique
    #' @return Returns the modified primary dataset with non-unique rows removed if \code{remove} is TRUE. 
    #' @export unique_filter
    #' @examples 
    #' \dontrun{
    #' unique_filter(MainDataTable)
    #' mod.dat <- unique_filter(MainDataTable, remove=TRUE) 
    #' }
    
    # Call in datasets
    out <- data_pull(dat)
    dat <- out$dat
    dataset <- out$dataset
    
    tmp <- tempfile()
    cat("Unique filter checks for", project, 'project using', dat, 'dataset on', format(Sys.Date(), format = "%Y%m%d"), file = tmp, append = TRUE)
    
    
    if (dim(dataset)[1] == dim(unique(dataset))[1]) {
        cat("Each row is a unique choice occurrence. No further action required.", file = tmp)
    } else {
        if (remove == FALSE) {
            cat("Each row in data set is not a unique choice occurrence at haul or trip level. \nConsider removing non-unique rows.", file = tmp)
        } else {
            cat("Each row in data set is not a unique choice occurrence at haul or trip level. \nNon-unique rows removed.", file = tmp)
            dataset <- unique(dataset)
        }
    }
    
    print(suppressWarnings(readLines(tmp)))
    
    unique_filter_function <- list()
    unique_filter_function$functionID <- "unique_filter"
    unique_filter_function$args <- list(dat, project, remove)
    unique_filter_function$output <- c(dat)
    unique_filter_function$msg <- suppressWarnings(readLines(tmp))
    log_call(unique_filter_function)
    
    rm(tmp)
    if (remove == TRUE) {
        return(dataset)
    }
    
}


# EMPTY Vars
empty_vars_filter <- function(dat, project, remove = FALSE) {
    #' Check variables are not empty
    #'
    #' Check for and remove empty variables 
    #'
    #' @param dat Primary data containing information on hauls or trips. 
    #' Table in FishSET database contains the string 'MainDataTable'.
    #' @param project String, name of project.
    #' @param remove Logical, remove empty variables? Defaults to FALSE.
    #' @details Function checks for empty variables and prints an outcome message to the console. If empty variables are present and \code{remove} = TRUE then empty variables will be removed from the dataset. 
    #' @keywords empty
    #' @return Returns the dataset with empty variables removed if \code{remove} is TRUE.
    #' @export empty_vars_filter
    #' @examples 
    #' \dontrun{
    #' empty_vars_filter(MainDataTable)
    #' mod.dat <- empty_vars_filter('pollockMainDataTable', 'pollock', remove=TRUE) 
    #' }
    
    # Call in datasets
    out <- data_pull(dat)
    dat <- out$dat
    dataset <- out$dataset
    
    
    tmp <- tempfile()
    cat("Empty vars checks for", project, 'project using', dat, 'dataset on', format(Sys.Date(), format = "%Y%m%d"), file = tmp, append = TRUE)
    
    if (any(apply(dataset, 2, function(x) all(is.na(x))) == TRUE)) {
        cat(names(which(apply(dataset, 2, function(x) all(is.na(x))) == TRUE)), "is empty. 
                \nConsider removing the column from the data set.", 
            file = tmp)
        
        if (remove == TRUE) {
            dataset <- dataset[, names(dataset) != names(which(apply(dataset, 2, function(x) all(is.na(x))) == TRUE))]
            cat(names(which(apply(dataset, 2, function(x) all(is.na(x))) == TRUE)), "is empty and has been removed from the data set.", file = tmp)
        } else {
            cat(names(which(apply(dataset, 2, function(x) all(is.na(x))) == TRUE)), "is empty. Consider removing from the data set.", file = tmp)
        }
    } else {
        cat("No empty variables identified.", file = tmp)
    }
    
    print(suppressWarnings(readLines(tmp)))
    
    empty_vars_filter_function <- list()
    empty_vars_filter_function$functionID <- "empty_vars_filter"
    empty_vars_filter_function$args <- list(dat, project, remove)
    empty_vars_filter_function$output <- list(dat)
    empty_vars_filter_function$msg <- suppressWarnings(readLines(tmp))
    log_call(empty_vars_filter_function)
    
    rm(tmp)
    if (remove == TRUE) {
        return(dataset)
    }
    
    
}

