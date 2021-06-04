degree <- function(dat, project, lat = NULL, lon = NULL, latsign = FALSE, lonsign = FALSE, replace = TRUE) {
  #' Check and correct lat/lon format
  #'
  #' Check that latitude and longitude are in decimal degrees and the variable sign is correct. Correct lat/lon if required.
  #' @param dat Dataset containing latitude and longitude data.
  #' @param project Project name. 
  #' @param lat Variable containing latitude data.
  #' @param lon Variable containing longitude data.
  #' @param latsign How should the sign value of \code{lat} be changed? Choices are \code{NULL}, no change, 
  #'    \code{"all"}, change all values, \code{"neg"}, convert all positive values to negative, or 
  #'    \code{"pos"}, convert all negative values to positive.
  #' @param lonsign How should the sign value of \code{lon} be changed? Choices are \code{NULL}, no change, 
  #'    \code{"all"}, change all values, \code{"neg"}, convert all positive values to negative, or 
  #'    \code{"pos"}, convert all negative values to positive.
  #' @param replace Logical, should \code{lat} and \code{lon} in \code{dat} be converted to decimal degrees? 
  #'   Defaults to TRUE. Set to FALSE if checking for compliance
  #' @export degree
  #' @importFrom OSMscale degree
  #' @importFrom stringr str_pad
  #' @details First checks whether any variables containing 'lat' or 'lon' in their names are numeric. Returns 
  #'   a message on results. To convert a variable to decimal degrees, identify the \code{lat} or \code{lon}
  #'   variable(s) and set \code{replace} to TRUE. To change the sign, set \code{latsign} (for \code{lat}) 
  #'   or \code{lonsign} (for \code{lon} to TRUE. FishSET requires that latitude and longitude 
  #'   be in decimal degrees.
  #' @return Returns the primary dataset with the latitudes and longitudes converted to decimal degrees. 
  #'    Changing the sign, transforms all values in the variable.
  #' @examples
  #' \dontrun{
  #' pollockMainDataTable <- degree(pollockMainDataTable, 'pollock', 'LatLon_START_LAT', 
  #'       'LatLon_START_LON', latsign=FALSE, lonsign=FALSE, replace=TRUE)
  #' }
  #'

  out <- data_pull(dat)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main")
  
  tmp <- tempfile()
  
  lat_lon <- grep("lat|lon", names(dataset), ignore.case = TRUE)
  lat_cols <- find_lat(dataset)
  lon_cols <- find_lon(dataset)
  
  num_ll <- !qaqc_helper(dataset[lat_lon], is.numeric)
  
  lat_deg <- qaqc_helper(dataset[lat_cols], function(x) {
    if (!is.numeric(x)) TRUE
    else any(nchar(trunc(abs(x))) > 2)}) 
  
  lon_deg <- qaqc_helper(dataset[lon_cols], function(x) {
    if (!is.numeric(x)) TRUE
    else any(nchar(trunc(abs(x))) > 3)}) 
  
  if (any(c(lat_deg, lon_deg, num_ll))) {
    
    lat_deg_ind <- which(lat_deg)
    lon_deg_ind <- which(lon_deg)
    num_ind <- which(num_ll)
    cat(paste("The following latitude/longitude variables are not in decimal degrees:", 
              paste(names(dataset)[unique(c(num_ind, lat_deg_ind, lon_deg_ind))], collapse = ","), 
              "\nSelect a lat and lon variable below and then select a conversion",
              "option on the the left to convert to decimal degrees."), 
        file = tmp)
  } else {
    
    cat("Latitude and longitude variables in decimal degrees. No further action required.", file = tmp)
  }

  msg_print(tmp)

  degree_function <- list()
  degree_function$functionID <- "degree"
  degree_function$args <- list(dat, project, lat, lon, latsign, lonsign, replace)
  degree_function$output <- list(dat)
  degree_function$msg <- suppressWarnings(readLines(tmp))
  log_call(project, degree_function)
  
  unlink(tmp)

  if (replace == TRUE) {
    if (!is.null(lat)) {
      if (!is.numeric(dataset[[lat]])) {
        
        # temp <- gsub("?|'|\"", "", dataset[[lat]])
        # temp[lengths(gregexpr(" ", temp)) == 1 & !is.na(temp)] <- paste(temp[lengths(gregexpr(" ", temp)) == 1 & !is.na(temp)], "00")
        # dataset[[lat]] <- as.numeric(sapply(strsplit(temp, "\\s+"), "[", 1)) + as.numeric(sapply(strsplit(temp, "\\s+"), "[", 2)) / 60 + as.numeric(sapply(strsplit(
        #   temp,
        #   "\\s+"
        # ), "[", 3)) / 360
        
        dataset[[lat]] <- dms_to_dd(dataset[[lat]])
        
      } else if (any(nchar(trunc(abs(dataset[[lat]]))) > 2, na.rm = TRUE)) {
        nm <- !is.na(dataset[[lat]]) & dataset[[lat]] < 0
        dataset[[lat]] <- abs(dataset[[lat]])
        i <- nchar(abs(dataset[[lat]])) <= 4 & !is.na(dataset[[lat]])
        dataset[[lat]][i] <- paste0(dataset[[lat]][i], "00")
        dataset[[lat]] <- format(as.numeric(stringr::str_pad(abs(as.numeric(dataset[[lat]])), 6, pad = "0")), scientific = FALSE)
        dataset[[lat]] <- as.numeric(substr(dataset[[lat]], start = 1, stop = 2)) + as.numeric(substr(dataset[[lat]], start = 3, stop = 4)) / 60 + as.numeric(substr(dataset[[lat]],
          start = 5, stop = 6
        )) / 3600
        dataset[[lat]][nm] <- dataset[[lat]][nm] * -1
      } 
    }
    
    if (!is.null(lon)) {
      if (!is.numeric(dataset[[lon]])) {
        
        # temp <- gsub("?|'|\"", "", dataset[, lon])
        # temp[lengths(gregexpr(" ", temp)) == 1 & !is.na(temp)] <- paste(temp[lengths(gregexpr(" ", temp)) == 1 & !is.na(temp)], "00")
        # dataset[[lon]] <- as.numeric(sapply(strsplit(temp, "\\s+"), "[", 1)) + as.numeric(sapply(strsplit(temp, "\\s+"), "[", 2)) / 60 + as.numeric(sapply(strsplit(
        #   temp,
        #   "\\s+"
        # ), "[", 3)) / 360
        
        dataset[[lon]] <- dms_to_dd(dataset[[lon]])
        
      } else if (any(nchar(trunc(abs(as.numeric(dataset[[lon]])))) > 3, na.rm = TRUE)) {
        nm <- !is.na(dataset[[lon]]) & as.numeric(dataset[[lon]]) < 0
        dataset[[lon]] <- abs(dataset[[lon]])
        i <- nchar(dataset[[lon]]) <= 5 & !is.na(dataset[[lon]])
        dataset[[lon]][i] <- paste0(dataset[[lon]][i], "00")
        dataset[[lon]] <- format(as.numeric(stringr::str_pad(as.numeric(dataset[[lon]]), 7, pad = "0")), scientific = FALSE)
        dataset[[lon]] <- as.numeric(substr(dataset[[lon]], start = 1, stop = 3)) + as.numeric(substr(dataset[[lon]], start = 4, stop = 5)) / 60 + as.numeric(substr(dataset[[lon]],
          start = 6, stop = 7
        )) / 3600
        dataset[[lon]][nm] <- dataset[[lon]][nm] * -1
      } 
    }

    ##Change latsign
    if (!is.null(latsign)) {
      
      if (latsign == "all" & !is.null(lat)) {
        dataset[[lat]] <- -1 * dataset[[lat]]
      } else if(latsign == "neg" & !is.null(lat)) {
        dataset[dataset[lat]> 0,lat] <- -1 * dataset[dataset[lat]> 0,lat]
      } else if(latsign == "pos" & !is.null(lat)) {
        dataset[dataset[lat]< 0,lat] <- -1 * dataset[dataset[lat]< 0,lat]
      }
    }
    
    ##Change lonsign
    if (!is.null(lonsign)) {
      
      if (lonsign == "all" & !is.null(lon)) {
        dataset[[lon]] <- -1 * dataset[[lon]]
      } else if(lonsign == "neg" & !is.null(lon)) {
        dataset[dataset[lon]> 0,lon] <- -1 * dataset[dataset[lon]> 0,lon]
      } else if(lonsign == "pos" & !is.null(lon)) {
        dataset[dataset[lon]< 0,lon] <- -1 * dataset[dataset[lon]< 0,lon]
      }
    }
 
    return(dataset)
  }
}

dms_to_dd <- function(x) {
  
  #' Convert decimal minutes/seconds to degrees
  #' 
  #' @param x Latitude or longitude vector.
  #' @export
  #' @keywords internal
  #' @importFrom stringi stri_replace_all_regex stri_count_regex
  #' @examples 
  #' \dontrun{
  #' pollockMainDataTable$LonLat_START_LAT <- dms_to_dd(pollockMainDataTable$LonLat_START_LAT)
  #' }
  
  new_dd <- gsub("[^[:digit:]|.|-]", " ", x)
  
  new_dd <- trimws(new_dd)
  
  # for dd-mm-ss format: replace any hyphen preceded by a digit with a space
  new_dd <- stringi::stri_replace_all_regex(new_dd, "(?<=\\d)-", " ")
  
  new_dd <- gsub("\\s{2,}", " ", new_dd) # replace 2 or more spaces with one
  space_count <- stringi::stri_count_regex(new_dd[1], "\\s")
  
  new_dd <- strsplit(new_dd, "\\s")
  new_dd <- lapply(new_dd, as.numeric)
  
  new_dd <- 
    vapply(new_dd, function(x) {
      
      deg_sign <- sign(x[1])
      
      if (space_count == 1) { # deg min
        
        out <- round(abs(x[1])) + x[2]/60
        
      } else if (space_count == 2) { # deg min sec
        
        out <- round(abs(x[1])) + x[2]/60 + x[3]/3600
      }
      
      if (deg_sign < 0) out * -1
      else out
      
    }, FUN.VALUE = numeric(1))
  
  new_dd
}

dms_to_pdms <- function(x, type, dec = FALSE, as_num = FALSE) { 
  #' Convert DMS to Packed DMS 
  #' 
  #' @param x Latitude or longitude vector.
  #' @param type \code{"lat"} or \code{"lon"}.
  #' @param dec Logical, whether to keep decimal if present.
  #' @param as_num Logical, whether to convert to numeric. If \code{FALSE}, a 
  #'   character string is outputted.
  #' @keywords internal
  #' @export
  #' @importFrom stringi stri_replace_all_regex stri_pad_right
  #' @details Primarily used for testing whether \code{degree()} can convert
  #'   Packed DMS to decimal degrees. 
  
  if (dec) pdms <- gsub("[^[:digit:]|.|-]", " ", x)
  else pdms <- gsub("[^[:digit:]|-]", " ", x)
  
  pdms <- trimws(pdms)
  
  # for dd-mm-ss format: replace any hyphen preceded by a digit with a space
  pdms <- stringi::stri_replace_all_regex(pdms, "(?<=\\d)-", " ")
  
  if (type == "lat") {
    
    start_0_pos <- grepl("^[1-9]{1}\\s", pdms)
    start_0_neg <- grepl("^-[1-9]{1}\\s", pdms)
    
    if (any(start_0_pos)) pdms[start_0_pos] <- paste0("0", pdms[start_0_pos])
    if (any(start_0_neg)) pdms[start_0_neg] <- paste0("-0", gsub("^-", "", pdms[start_0_neg]))
    
  } else if (type == "lon") {
    
    start_00_pos <- grepl("^[1-9]{1}\\s", pdms)
    start_00_neg <- grepl("^-[1-9]{1}\\s", pdms)
    start_0_pos <- grepl("^[1-9]{2}\\s", pdms)
    start_0_neg <- grepl("^-[1-9]{2}\\s", pdms)
    
    if (any(start_00_pos)) pdms[start_00_pos] <- paste0("00", pdms[start_00_pos])
    if (any(start_00_neg)) pdms[start_00_neg] <- paste0("-00", gsub("^-", "", pdms[start_00_neg]))
    if (any(start_0_pos)) pdms[start_0_pos] <- paste0("0", pdms[start_0_pos])
    if (any(start_0_neg)) pdms[start_0_neg] <- paste0("-0", gsub("^-", "", pdms[start_0_neg]))
  }
  
  # add 0 in front of any single digit value
  pdms <- stringi::stri_replace_all_regex(pdms, "\\s(?=\\d(\\s|$))", " 0") 
  pdms <- gsub(" ", "", pdms)
  
  # pad with zeros to make width uniform
  width <- max(nchar(pdms), na.rm = TRUE)
  pdms <- stringi::stri_pad_right(pdms, width = width, pad = "0")
  
  if (as_num) pdms <- as.numeric(pdms)
  
  pdms
}