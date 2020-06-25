# create_seasonal_ID
#' Create fishery season identifier  
#' 
#' Creates a fishery season identifier variable
#'
#' @param dat Primary data containing information on hauls or trips. 
#' Table in FishSET database contains the string 'MainDataTable'.
#' @param seasonal.dat Name of table containing date of fishery season(s). Can be pulled from FishSET database.
#' @param use.location  Logical, should fishery season dates depend on fishery location? Column names containing
#' location in \code{dat} and \code{seasonal.dat} must match.
#' @param use.geartype Logical, should fishery season dates depend on gear type. Column name containing gear 
#' type in \code{dat} and \code{seasonal.dat} must match.
#' @param sp.col Variable in \code{seasonal.dat} containing species names. 
#' @param target Name of target species. If \code{target} is NULL, runs through fisheries in order listed in \code{seasonal.dat}
#' @export create_seasonal_ID
#' @return  Returns the primary dataset with the variable SeasonID, or a series of variables identifying by the individual 
#' fisheries included (seasonID*fishery). 
#' @importFrom DBI dbConnect
#' @importFrom RSQLite SQLite
#' @details Uses a table of fishery season dates to create fishery season identifier variables. Output is a SeasonID 
#' variable and/or multiple SeasonID*fishery variables. If fishery season dates vary by location or geartype, 
#' then \code{use.location} and \code{use.geartype} should be TRUE. \cr\cr
#' The function matchers fishery season dates provided in \code{seasonal.dat} to the first date variable in the primary dataset. 
#' The seasonID variable is a vector of fisheries whereas the SeasonID*fishery variables are TRUE/FALSE indicating whether the 
#' fishery was open on the observed date. \cr\cr
#' If \code{target} is not defined, then each row of seasonID is defined as the first fishery listed in \code{seasonal.dat} for 
#' which fishery season date encompasses the date variable in the primary dataset. If \code{target} fishery is defined, then 
#' SeasonID is defined by whether the target fishery is open on the date in the primary dataset or a different fishery. The 
#' vector is filled with 'target' or 'other'.\cr\cr
#' SeasonID*fishery variables are a TRUE/FALSE seasonID vector for each fishery (labeled by seasonID and fishery) where TRUE 
#' indicates the dates for a given row in the main data table fall within the fishery dates for that fishery.

#' @examples 
#' \dontrun{ 
#'  pcodMainDataTable <- create_seasonal_ID('pcodMainDataTable', seasonal_dat, use.location = TRUE,  
#'  use.geartype = TRUE, sp.col = 'SPECIES', target = 'POLLOCK')
#'  }
#'

create_seasonal_ID <- function (dat, seasonal.dat, use.location=c(TRUE,FALSE), use.geartype=c(TRUE,FALSE), sp.col, target=NULL){

  #Call in datasets
  dataset <- dat
  dat <- deparse(substitute(dat))
  
  fishset_db <- suppressWarnings(DBI::dbConnect(RSQLite::SQLite(), locdatabase()))
  
  if(is.character(seasonal.dat)==TRUE){
    if(is.null(seasonal.dat)==TRUE | table_exists(seasonal.dat)==FALSE){
      print(tables_database())
      stop(paste(seasonal.dat, 'not defined or does not exist. Consider using one of the tables listed above that exist in the database.'))
    } else {
        seasonaldat <- seasonal.dat
    }
  } else {
    seasonal.dat <- seasonal.dat
     deparse(substitute(seasonal.dat))
    }
    DBI::dbDisconnect(fishset_db)
    
    
    # Test that location_data match
    if (use.location == TRUE) {
        if (!any(match(names(dataset)[grep("area|zon", names(dataset), ignore.case = TRUE)], names(seasonaldat), nomatch = 0) > 0)) {
            stop("Area or zone must be defined and match in both the main dataset and the seasonal dataset. No match found.")
        } else {
            loc.name <- names(seasonaldat)[match(names(dataset)[grep("area|zon", names(dataset), ignore.case = TRUE)], names(seasonaldat), nomatch = 0)[which(match(names(dataset)[grep("area|zon", 
                names(dataset), ignore.case = TRUE)], names(seasonaldat), nomatch = 0) > 0)]]
        }
    }
    
    if (use.geartype == TRUE) {
        if (!any(match(names(dataset)[grep("gear", names(dataset), ignore.case = TRUE)], names(seasonaldat), nomatch = 0) > 0)) {
            stop("Gear type must be defined and match in both the main dataset and the seasonal dataset. No match found.")
        } else {
            gear.name <- names(seasonaldat)[match(names(dataset)[grep("gear", names(dataset), ignore.case = TRUE)], names(seasonaldat), nomatch = 0)[which(match(names(dataset)[grep("gear", 
                names(dataset), ignore.case = TRUE)], names(seasonaldat), nomatch = 0) > 0)]]
        }
    }
    
    seasonaldat[, sp.col] <- gsub("(\\s+)|([[:punct:]])", "_", seasonaldat[, sp.col])
    sp.use <- unique(as.vector(seasonaldat[[sp.col]]))
    sp.use[grep("(\\s+)|([[:punct:]])", sp.use)] <- paste(strsplit(sp.use[grep("(\\s+)|([[:punct:]])", sp.use)], "(\\s+)|([[:punct:]])")[[1]][1], ".*", 
        strsplit(sp.use[grep("(\\s+)|([[:punct:]])", sp.use)], "(\\s+)|([[:punct:]])")[[1]][2], sep = "")
    
    
    dat.temp <- dataset[grep("date", names(dataset), ignore.case = TRUE)]  #
    dat.start <- as.data.frame(apply(dat.temp, 1, function(x) find_first(x)))
    dat.end <- as.data.frame(apply(dat.temp, 1, function(x) find_last(x)))
    
    rownames(dataset) = make.names(dat.start[, 1], unique = TRUE)
    dat.start <- as.Date(sapply(gsub("\\.|[[:digit:]]", "", rownames(dataset)), function(x) dataset[[x, x]]), origin = "1970-01-01")
    rownames(dataset) = make.names(dat.end[, 1], unique = TRUE)
    dat.end <- as.Date(sapply(gsub("\\.|[[:digit:]]", "", rownames(dataset)), function(x) dataset[[x, x]]), origin = "1970-01-01")
    rownames(dataset) = 1:nrow(dataset)
    
    
    
    # Get names of species that are match seasonal and the dataset
    spp <- unique(gsub(paste0("(", paste(sp.use, collapse = "|"), ")", "(*SKIP)(*FAIL)|."), "", names(dataset)[grep(paste(sp.use, collapse = "|"), colnames(dataset), 
        ignore.case = TRUE)], perl = TRUE, ignore.case = TRUE))
    
    # for(i in 1:length(spp)){ dataset[[paste0('SeasonID',spp[i])]] <- NA }
    loca <- grep(paste0(unique(gsub(",", "|", seasonaldat[[loc.name]])), collapse = "|"), unique(as.character(dataset[[loc.name]])), value = TRUE)
    
    ## --- Create season ID for target species --- ##
    dataset[["SeasonID"]] <- NA
    if (is.null(target) == FALSE) {
        seasonsub <- seasonaldat[grep(target, seasonaldat[[sp.col]], ignore.case = TRUE), ]
        if (use.location == FALSE & use.geartype == FALSE) {
            seasontemp <- seasonsub
            if (dim(seasontemp)[1] > 1) {
                seasontemp <- seasontemp[1, ]
                warning("More than one record exists. Only the first record will be used.")
            }
            if (all(seasontemp[grep("date", names(seasontemp), ignore.case = TRUE)] == "") == TRUE) {
                dataset[["SeasonID"]] <- NA
            } else {
                dataset[["SeasonID"]] <- ifelse((dat.start > date_parser(seasontemp[[find_first(seasontemp)]][1])) %in% (dat.end < date_parser(seasontemp[[find_last(seasontemp)]][1])) == 
                  TRUE, target, "Other")
            }
        } else {
            for (j in 1:length(loca)) {
                seasontemp <- seasonsub[grep(as.character(loca[j]), gsub(",", "|", seasonsub[[loc.name]])), ]
                
                if (dim(seasontemp)[1] > 1) {
                  if (use.geartype == TRUE) {
                    # If more than row of the species need to do something - use gear type?
                    if (grepl("trawl", dataset[[gear.name]][1], ignore.case = TRUE) == TRUE) {
                      seasontemp <- seasontemp[grep("trawl", seasontemp[[gear.name]], ignore.case = TRUE), ]
                    } else if (grepl("seine|gill", dataset[[gear.name]][1], ignore.case = TRUE) == TRUE) {
                      seasontemp <- seasontemp[grep("seine|gill", seasontemp[[gear.name]], ignore.case = TRUE), ]
                    } else if (grepl("hook|line", dataset[[gear.name]][1], ignore.case = TRUE) == TRUE) {
                      seasontemp <- seasontemp[grep("hook|line", seasontemp[[gear.name]], ignore.case = TRUE), ]
                    } else if (grepl("pot", dataset[[gear.name]][1], ignore.case = TRUE) == TRUE) {
                      seasontemp <- seasontemp[grep("pot", seasontemp[[gear.name]], ignore.case = TRUE), ]
                    }
                  } else {
                    seasontemp <- seasontemp[1, ]
                    warning("More than one record exists. Only the first record will be used.")
                  }
                }
                if (dim(seasontemp[grep("date", names(seasontemp), ignore.case = TRUE)])[1] == 0 || all(seasontemp[grep("date", names(seasontemp), ignore.case = TRUE)] == 
                  "") == TRUE) {
                  dataset[which(dataset[[loc.name]] == loca[j]), "SeasonID"] <- NA
                } else {
                  dataset[which(dataset[[loc.name]] == loca[j]), "SeasonID"] <- ifelse((dat.start[which(dataset[[loc.name]] == loca[j])] > date_parser(seasontemp[[find_first(seasontemp)]][1])) %in% 
                    (dat.end[which(dataset[[loc.name]] == loca[j])] < date_parser(seasontemp[[find_last(seasontemp)]][1])) == TRUE, target, "Other")
                }
            }
        }
    } else {
        ## -- No target species -- ##
        seasonsub <- subset(seasonaldat, seasonaldat[grep("date", names(seasonaldat), ignore.case = TRUE)[1]] != "" & seasonaldat[grep("date", names(seasonaldat), 
            ignore.case = TRUE)[2]] != "")
        if (use.location == FALSE & use.geartype == FALSE) {
            i <- 1
            while (all(is.na(dataset[["SeasonID"]])) == TRUE) {
                seasontemp <- seasonsub[i, ]
                dataset[which(is.na(dataset[["SeasonID"]] == TRUE)), "SeasonID"] <- ifelse(((dat.start[which(is.na(dataset[["SeasonID"]]) == TRUE)] > 
                  date_parser(seasontemp[[find_first(seasontemp)]][1])) %in% (dat.end[which(is.na(dataset[["SeasonID"]]) == TRUE)] < date_parser(seasontemp[[find_last(seasontemp)]][1])) == 
                  TRUE), as.character(seasontemp[[sp.col]]), NA)
                i <- i + 1
            }
        } else {
            # Add location now
            i <- 1
            j <- 1
            while (all(is.na(dataset[["SeasonID"]])) == TRUE) {
                seasontemp <- seasonsub[i, ]
                seasontemp <- seasontemp[grep(as.character(loca[j]), gsub(",", "|", seasontemp[[loc.name]])), ]
                if (all(is_empty(seasontemp)) == TRUE) {
                  next
                }
                if (dim(seasontemp)[1] > 1) {
                  if (use.geartype == TRUE) {
                    # If more than row of the species need to do something - use gear type?
                    if (grepl("trawl", dataset[[gear.name]][1], ignore.case = TRUE) == TRUE) {
                      seasontemp <- seasontemp[grep("trawl", seasontemp[[gear.name]], ignore.case = TRUE), ]
                    } else if (grepl("seine|gill", dataset[[gear.name]][1], ignore.case = TRUE) == TRUE) {
                      seasontemp <- seasontemp[grep("seine|gill", seasontemp[[gear.name]], ignore.case = TRUE), ]
                    } else if (grepl("hook|line", dataset[[gear.name]][1], ignore.case = TRUE) == TRUE) {
                      seasontemp <- seasontemp[grep("hook|line", seasontemp[[gear.name]], ignore.case = TRUE), ]
                    } else if (grepl("pot", dataset[[gear.name]][1], ignore.case = TRUE) == TRUE) {
                      seasontemp <- seasontemp[grep("pot", seasontemp[[gear.name]], ignore.case = TRUE), ]
                    }
                  } else {
                    seasontemp <- seasontemp[1, ]
                    warning("More than one record exists. Only the first record will be used.")
                  }
                }
                
                if (dim(seasontemp[grep("date", names(seasontemp), ignore.case = TRUE)])[1] == 0 || all(seasontemp[grep("date", names(seasontemp), ignore.case = TRUE)] == 
                  "") == TRUE) {
                  dataset[which(is.na(dataset$SeasonID) == TRUE) && which(dataset[[loc.name]] == loca[j]), "SeasonID"] <- NA
                } else {
                  dataset[which(is.na(dataset$SeasonID) == TRUE) && which(dataset[[loc.name]] == loca[j]), "SeasonID"] <- ifelse(((dat.start[which(is.na(dataset$SeasonID) == 
                    TRUE) && which(dataset[[loc.name]] == loca[j])] > date_parser(seasontemp[[find_first(seasontemp)]][1])) %in% (dat.end[which(is.na(dataset$SeasonID) == 
                    TRUE) && which(dataset[[loc.name]] == loca[j])] < date_parser(seasontemp[[find_last(seasontemp)]][1])) == TRUE), as.character(seasontemp[[sp.col]]), 
                    NA)
                }
                
                j <- j + 1
                i <- i + 1
            }
        }
    }
    
    ## --- Create seasonID for all species --- ##
    for (i in 1:length(spp)) {
        seasonsub <- seasonaldat[tolower(seasonaldat[[sp.col]]) == tolower(spp)[i], ]
        
        if (use.location == FALSE & use.geartype == FALSE) {
            if (dim(seasontemp)[1] > 1) {
                seasontemp <- seasonsub[1, ]
                warning("More than one record exists. Only the first record will be used.")
            } else {
                seasontemp <- seasonsub[1, ]
            }
            if (all(seasontemp[grep("date", names(seasontemp), ignore.case = TRUE)] == "") == TRUE) {
                dataset[[paste0("SeasonID", spp[i])]] <- FALSE
            } else {
                dataset[[paste0("SeasonID", spp[i])]] <- (dat.start > date_parser(seasontemp[[find_first(seasontemp)]][1])) %in% (dat.end < date_parser(seasontemp[[find_last(seasontemp)]][1]))
            }
        } else if (use.location == FALSE & use.geartype == TRUE) {
            if (dim(seasontemp)[1] > 1) {
                if (grepl("trawl", dataset[[gear.name]][1], ignore.case = TRUE) == TRUE) {
                  seasontemp <- seasontemp[grep("trawl", seasontemp[[gear.name]], ignore.case = TRUE), ]
                } else if (grepl("seine|gill", dataset[[gear.name]][1], ignore.case = TRUE) == TRUE) {
                  seasontemp <- seasontemp[grep("seine|gill", seasontemp[[gear.name]], ignore.case = TRUE), ]
                } else if (grepl("hook|line", dataset[[gear.name]][1], ignore.case = TRUE) == TRUE) {
                  seasontemp <- seasontemp[grep("hook|line", seasontemp[[gear.name]], ignore.case = TRUE), ]
                } else if (grepl("pot", dataset[[gear.name]][1], ignore.case = TRUE) == TRUE) {
                  seasontemp <- seasontemp[grep("pot", seasontemp[[gear.name]], ignore.case = TRUE), ]
                }
            } else {
                seasontemp <- seasonsub[1, ]
            }
            if (all(seasontemp[grep("date", names(seasontemp), ignore.case = TRUE)] == "") == TRUE) {
                dataset[[paste0("SeasonID", spp[i])]] <- FALSE
            } else {
                dataset[[paste0("SeasonID", spp[i])]] <- (dat.start > date_parser(seasontemp[[find_first(seasontemp)]][1])) %in% (dat.end < date_parser(seasontemp[[find_last(seasontemp)]][1]))
            }
        }
        
        if (use.location == TRUE) {
            for (j in 1:length(loca)) {
                seasontemp <- seasonsub[grep(as.character(loca[j]), gsub(",", "|", seasonsub[[loc.name]])), ]
                
                
                if (dim(seasontemp)[1] > 1) {
                  if (use.geartype == TRUE) {
                    # If more than row of the species need to do something - use gear type?
                    if (grepl("trawl", dataset[[gear.name]][1], ignore.case = TRUE) == TRUE) {
                      seasontemp <- seasontemp[grep("trawl", seasontemp[[gear.name]], ignore.case = TRUE), ]
                    } else if (grepl("seine|gill", dataset[[gear.name]][1], ignore.case = TRUE) == TRUE) {
                      seasontemp <- seasontemp[grep("seine|gill", seasontemp[[gear.name]], ignore.case = TRUE), ]
                    } else if (grepl("hook|line", dataset[[gear.name]][1], ignore.case = TRUE) == TRUE) {
                      seasontemp <- seasontemp[grep("hook|line", seasontemp[[gear.name]], ignore.case = TRUE), ]
                    } else if (grepl("pot", dataset[[gear.name]][1], ignore.case = TRUE) == TRUE) {
                      seasontemp <- seasontemp[grep("pot", seasontemp[[gear.name]], ignore.case = TRUE), ]
                    }
                  } else {
                    seasontemp <- seasontemp[1, ]
                    warning("More than one record exists. Only the first record will be used.")
                  }
                }
                if (dim(seasontemp[grep("date", names(seasontemp), ignore.case = TRUE)])[1] == 0 || all(seasontemp[grep("date", names(seasontemp), ignore.case = TRUE)] == 
                  "") == TRUE) {
                  dataset[which(dataset[[loc.name]] == loca[j]), paste0("SeasonID", spp[i])] <- FALSE
                } else {
                  dataset[which(dataset[[loc.name]] == loca[j]), paste0("SeasonID", spp[i])] <- (dat.start[which(dataset[[loc.name]] == loca[j])] > date_parser(seasontemp[[find_first(seasontemp)]][1])) %in% 
                    (dat.end[which(dataset[[loc.name]] == loca[j])] < date_parser(seasontemp[[find_last(seasontemp)]][1]))
                }
            }
        }
    }
    
    create_seaonal_ID_function <- list()
    create_seaonal_ID_function$functionID <- "create_seaonal_ID"
    create_seaonal_ID_function$args <- list(dat, seasonal.dat, use.location, use.geartype, sp.col)
    create_seaonal_ID_function$kwargs <- list(target = target)
    create_seaonal_ID_function$output <- dat
    log_call(create_seaonal_ID_function)
    
    return(dataset)
}

