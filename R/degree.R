degree <- function(dat, lat = NULL, lon = NULL, latsign = FALSE, lonsign = FALSE, replace = TRUE) {
    #' Check and correct lat/lon format
    #' @param dat Data table containing latitude and longitude data
    #' @param lat Name of vector containing latitude data
    #' @param lon Name of vector containg longitude data
    #' @param latsign If TRUE, transforms sign from positive to minus or minus to positive
    #' @param lonsign If TRUE, transforms sign from positive to minus or minus to positive
    #' @param replace Defaults to TRUE. Set to FALSE if only checking for errors
    #' @export degree
    #' @importFrom OSMscale degree
    #' @importFrom stringr str_replace
    #' @details Uses the degree function to convert lat long coordinates to decimal degrees.
    #' @return The original dataframe with the latitudes and longitudes converted to decimal degrees.
    #' Changing the sign, transforms all values in the variable. 
    #' @examples 
    #' \dontrun{
    #' MainDataTable <- degree(MainDataTable, 'LatLon_START_LAT', 'LatLon_START_LON', 
    #'            latsign=FALSE, lonsign=FALSE, replace=TRUE)
    #' }
    #' 
    
    # Call in datasets
    out <- data_pull(dat)
    dat <- out$dat
    dataset <- out$dataset
 
    tmp <- tempfile()
    
    if (any(apply(dataset[, grep("lat|lon", names(dataset), ignore.case = TRUE)], 2, function(x) !is.numeric(x)) == TRUE) == TRUE) {
        cat("At least one latitude or longitude variable is not in decimal degrees. Select a lat and lon variable below and then select a conversion option on the the left to convert to decimal degrees.", 
            file = tmp)
    } else if (any(apply(dataset[, grep("lat|lon", names(dataset), ignore.case = TRUE)], 2, function(x) nchar(trunc(abs(x)))) > 3) == TRUE) {
        cat("At least one latitude or longitude variable is not in decimal degrees. Select a lat and lon variable below and then select a conversion option on the the left to convert to decimal degrees.", 
            file = tmp)
    } else {
        cat("Latitude and longitude variables in decimal degrees. No further action required.", file = tmp)
    }
    
    print(suppressWarnings(readLines(tmp)))
    
    degree_function <- list()
    degree_function$functionID <- "degree"
    degree_function$args <- list(dat, lat, lon, latsign, lonsign, replace)
    degree_function$output <- list(dat)
    degree_function$msg <- suppressWarnings(readLines(tmp))
    log_call(degree_function)
    
    if (replace == TRUE) {
        if (!is.null(lat)) {
            if (!is.numeric(dataset[[lat]])) {
                temp = gsub("?|'|\"", "", dataset[[lat]])
                temp[lengths(gregexpr(" ", temp)) == 1 & !is.na(temp)] <- paste(temp[lengths(gregexpr(" ", temp)) == 1 & !is.na(temp)], "00")
                dataset[[lat]] <- as.numeric(sapply(strsplit(temp, "\\s+"), "[", 1)) + as.numeric(sapply(strsplit(temp, "\\s+"), "[", 2))/60 + as.numeric(sapply(strsplit(temp, 
                  "\\s+"), "[", 3))/360
                
            } else if (any(nchar(trunc(abs(dataset[[lat]]))) > 2, na.rm = T)) {
                nm <- !is.na(dataset[[lat]]) & dataset[[lat]] < 0
                dataset[[lat]] <- abs(dataset[[lat]])
                i <- nchar(abs(dataset[[lat]])) <= 4 & !is.na(dataset[[lat]])
                dataset[[lat]][i] <- paste0(dataset[[lat]][i], "00")
                dataset[[lat]] <- format(as.numeric(stringr::str_pad(abs(as.numeric(dataset[[lat]])), 6, pad = "0")), scientific = FALSE)
                dataset[[lat]] <- as.numeric(substr(dataset[[lat]], start = 1, stop = 2)) + as.numeric(substr(dataset[[lat]], start = 3, stop = 4))/60 + as.numeric(substr(dataset[[lat]], 
                  start = 5, stop = 6))/3600
                dataset[[lat]][nm] <- dataset[[lat]][nm] * -1
            } else {
              dataset <- dataset
            }
        }
        if (!is.null(lon)) {
            if (!is.numeric(dataset[[lon]])) {
                temp = gsub("?|'|\"", "", dataset[, lon])
                temp[lengths(gregexpr(" ", temp)) == 1 & !is.na(temp)] <- paste(temp[lengths(gregexpr(" ", temp)) == 1 & !is.na(temp)], "00")
                dataset[[lon]] <- as.numeric(sapply(strsplit(temp, "\\s+"), "[", 1)) + as.numeric(sapply(strsplit(temp, "\\s+"), "[", 2))/60 + as.numeric(sapply(strsplit(temp, 
                  "\\s+"), "[", 3))/360
                
            } else if (any(nchar(trunc(abs(as.numeric(dataset[[lon]])))) > 3, na.rm = T)) {
                nm <- !is.na(dataset[[lon]]) & as.numeric(dataset[[lon]]) < 0
                dataset[[lon]] <- abs(dataset[[lon]])
                i <- nchar(dataset[[lon]]) <= 5 & !is.na(dataset[[lon]])
                dataset[[lon]][i] <- paste0(dataset[[lon]][i], "00")
                dataset[[lon]] <- format(as.numeric(stringr::str_pad(as.numeric(dataset[[lon]]), 7, pad = "0")), scientific = FALSE)
                dataset[[lon]] <- as.numeric(substr(dataset[[lon]], start = 1, stop = 3)) + as.numeric(substr(dataset[[lon]], start = 4, stop = 5))/60 + as.numeric(substr(dataset[[lon]], 
                  start = 6, stop = 7))/3600
                dataset[[lon]][nm] <- dataset[[lon]][nm] * -1
            } else {
              dataset <- dataset
            }
        }
        
        if (latsign == TRUE & !is.null(lat)) {
          dataset[[lat]] <- -1 * dataset[[lat]]
        } else {
          dataset <- dataset
        }
        if (lonsign == TRUE & !is.null(lon)) {
          dataset[[lon]] <- -1 * dataset[[lon]]
        } else {
          dataset <- dataset
        }
        return(dataset)
    }
    rm(tmp)
}
