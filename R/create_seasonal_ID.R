#  seasonalID
#' Create single binary fishery season identifier variable
#'
#' @param dat Primary data containing information on hauls or trips.
#'   Table in the FishSET database contains the string 'MainDataTable'.
#' @param project Project name.
#' @param seasonal.dat Data table containing date of fishery season(s). Data table can be pulled from the FishSET database.
#'   Leave \code{seasonal.dat} as NULL if supplying start and end dates with \code{start} and \code{end} arguments.
#' @param start Date, supplied as a string (example: start='2011/04/22', start='04222011'), or 
#'      variable in \code{seasonal.dat} which identifies start date of fishery season
#' @param end  DDate, supplied as a string (example: start='2011/04/22', start='04222011'), or 
#'      variable in \code{seasonal.dat} which identifies end date of fishery season
#' @param overlap Logical. Should trip or haul dates that start before or end after the fishery season date but starts or ends within the fishery season
#'    dates be included? FALSE indicates to inlude only hauls/trips that fall completely within the bounds of a fishery season date. Defaults to FALSE.
#' @param name String  Seasonal identifier name
#' @export seasonalID
#' @return  Returns a binary variable of within (1) or outside (0) the fishery season.
#' @details Uses a supplied dates or a table of fishery season dates to create fishery season identifier variables. Output is a binary
#' variable called \code{name} or `SeasonID` if \code{name} is not supplied. \cr\cr
#' For each row \code{dat}, the function matches fishery season dates provided in \code{seasonal.dat} to the earliest date variable in \code{dat}.
#' @examples
#' \dontrun{
#' #Example using a table stored in the FishSET database
#' pcodMainDataTable <- season_ID("pcodMainDataTable", 'pcod', seasonal_dat='seasonTable', 
#'      start='SeasonStart', end='SeasonEnd', name='2001A')
#' #Example using manually entered dates
#' pcodMainDataTable <- season_ID("pcodMainDataTable", 'pcod', seasonal.dat=NULL, 
#'     start='04152011', end='06302011', name='2001A')
#' }
#'
seasonalID <- function(dat, project, seasonal.dat=NULL, start, end, overlap=FALSE, name=NULL) {
  
  #### --- > HERE <----  ####
  #Creating as binary variable where data can come from a table or be entered manually#
  
  # Call in datasets
  out <- data_pull(dat, project)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main", project)
  
  if(!is.null(seasonal.dat)){
    out <- data_pull(seasonal.dat, project)
    seasonaldat <- out$dataset
    seasonal.dat <- parse_data_name(seasonal.dat, "aux", project)
  }
  
  if(is.null(name)) {
    name <- 'SeasonID'
  } else {
    name <- name
  }
  
  
  if(is.character(end)){
    season.end <- seasonaldat[[end]]
    if(length(season.end)>1){
      season.end <- season.end[1]
      warning('More than on season end date found, using the first date.')
    }
  } else {
    season.end <- end
  }
  season.end <- date_parser(season.end)
  
  
  if(is.character(start)){
    season.start <- seasonaldat[[start]]
    if(length(season.start)>1){
      season.start <- season.start[1]
      warning('More than on season end date found, using the first date.')
    }
  } else {
    season.start <- start
  }
  
  season.start <- date_parser(season.start)
  
  dat.temp <- date_cols(dataset)
  dat.start <- as.data.frame(apply(dataset[,dat.temp], 1, function(x) find_first(x)))
  dat.end <- as.data.frame(apply(dataset[,dat.temp], 1, function(x) find_last(x)))
  
  rownames(dataset) <- make.names(dat.start[, 1], unique = TRUE)
  dat.start <- as.Date(sapply(gsub("\\.|[[:digit:]]", "", rownames(dataset)), function(x) dataset[[x, x]]), origin = "1970-01-01")
  rownames(dataset) <- make.names(dat.end[, 1], unique = TRUE)
  dat.end <- as.Date(sapply(gsub("\\.|[[:digit:]]", "", rownames(dataset)), function(x) dataset[[x, x]]), origin = "1970-01-01")
  rownames(dataset) <- 1:nrow(dataset)
  
  
  dataset[[name]] <- NA
  
  if(overlap==FALSE){
    for(i in 1:length(dat.start)){
      if(dat.start[i] >= season.start & dat.end[i] <= season.end){
        dataset[i, name] = 1
      } else {
        dataset[i, name] = 0
      }
      }
  } else {
    for(i in 1:length(dat.start)){
      if(season.start <= dat.end[i] & season.end >= dat.start[i]){
        dataset[i, name] <- 1
      } else {
        dataset[i, name] <- 0
      }
      }
  }

seasonalID_function <- list()
seasonalID_function$functionID <- "seasonalID"
seasonalID_function$args <- list(dat, project, seasonal.dat, start, end, overlap, name)
seasonalID_function$output <- dat
log_call(project, seasonalID_function)

return(dataset)
}



# create_seasonal_ID
#' Create fishery season identifier variable
#'
#' @param dat Primary data containing information on hauls or trips.
#'   Table in the FishSET database contains the string 'MainDataTable'.
#' @param project Project name.
#' @param seasonal.dat Table containing date of fishery season(s). Can be pulled from the FishSET database.
#' @param use.location  Logical, should fishery season dates depend on fishery location? Column names containing
#'   location in \code{dat} and \code{seasonal.dat} must match.
#' @param use.geartype Logical, should fishery season dates depend on gear type. Column names containing gear
#'   type in \code{dat} and \code{seasonal.dat} must match.
#' @param sp.col Variable in \code{seasonal.dat} containing species names.
#' @param target Name of target species. If \code{target} is NULL, runs through fisheries in order listed in \code{seasonal.dat}
#' @export create_seasonal_ID
#' @return  Returns the primary dataset with the variable SeasonID, or a series of variables identifying by the individual
#'   fisheries included (seasonID*fishery).
#' @details Uses a table of fishery season dates to create fishery season identifier variables. Output is a SeasonID
#' variable and/or multiple SeasonID*fishery variables. If fishery season dates vary by location or gear type,
#' then \code{use.location} and \code{use.geartype} should be TRUE. \cr\cr
#' The function matches fishery season dates provided in \code{seasonal.dat} to the earliest date variable in \code{dat}.
#' The `seasonID` variable is a vector of fishery seasons whereas the `SeasonID*fishery` variables are 1/0 depending on whether the
#' fishery was open on the observed date. \cr\cr
#' If \code{target} is not defined, then each row of seasonID is defined as the earliest fishery listed in \code{seasonal.dat} for
#' which the fishery season date encompasses the date variable in the primary dataset. If \code{target} fishery is defined, then
#' `SeasonID` is defined by whether the target fishery is open on the date in the primary dataset or a different fishery. The
#' vector is filled with 'target' or 'other'.\cr\cr
#' `SeasonID*fishery` variables are a 1/0 seasonID vector for each fishery (labeled by seasonID and fishery) where 1
#' indicates the dates for a given row in the primary data table fall within the fishery dates for that fishery.

#' @examples
#' \dontrun{
#' pcodMainDataTable <- create_seasonal_ID("pcodMainDataTable", seasonal_dat,
#'   use.location = TRUE, use.geartype = TRUE, sp.col = "SPECIES", target = "POLLOCK"
#' )
#' }
#'
create_seasonal_ID <- function(dat, project, seasonal.dat, use.location = c(TRUE, FALSE), use.geartype = c(TRUE, FALSE), sp.col, target = NULL) {

  # Call in datasets
  out <- data_pull(dat, project)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main", project)

  out <- data_pull(seasonal.dat, project)
  seasonaldat <- out$dataset
  seasonal.dat <- parse_data_name(seasonal.dat, "aux", project)
  

  # Test that location_data match
  if (use.location == TRUE) {
    if (!any(match(names(dataset)[grep("area|zon", names(dataset), ignore.case = TRUE)], names(seasonaldat), nomatch = 0) > 0)) {
      stop("Area or zone must be defined and match in both the primary dataset and the seasonal dataset. No match found.")
    } else {
      loc.name <- names(seasonaldat)[match(names(dataset)[grep("area|zon", names(dataset), ignore.case = TRUE)], names(seasonaldat), nomatch = 0)[which(match(names(dataset)[grep("area|zon",
        names(dataset),
        ignore.case = TRUE
      )], names(seasonaldat), nomatch = 0) > 0)]]
    }
  }

  if (use.geartype == TRUE) {
    if (!any(match(names(dataset)[grep("gear", names(dataset), ignore.case = TRUE)], names(seasonaldat), nomatch = 0) > 0)) {
      stop("Gear type must be defined and match in both the primary dataset and the seasonal dataset. No match found.")
    } else {
      gear.name <- names(seasonaldat)[match(names(dataset)[grep("gear", names(dataset), ignore.case = TRUE)], names(seasonaldat), nomatch = 0)[which(match(names(dataset)[grep("gear",
        names(dataset),
        ignore.case = TRUE
      )], names(seasonaldat), nomatch = 0) > 0)]]
    }
  }

  seasonaldat[, sp.col] <- gsub("(\\s+)|([[:punct:]])", "_", seasonaldat[, sp.col])
  sp.use <- unique(as.vector(seasonaldat[[sp.col]]))
  sp.use[grep("(\\s+)|([[:punct:]])", sp.use)] <- paste(strsplit(sp.use[grep("(\\s+)|([[:punct:]])", sp.use)], "(\\s+)|([[:punct:]])")[[1]][1], ".*",
    strsplit(sp.use[grep("(\\s+)|([[:punct:]])", sp.use)], "(\\s+)|([[:punct:]])")[[1]][2],
    sep = ""
  )


  dat.temp <- dataset[grep("date", names(dataset), ignore.case = TRUE)] #
  dat.start <- as.data.frame(apply(dat.temp, 1, function(x) find_first(x)))
  dat.end <- as.data.frame(apply(dat.temp, 1, function(x) find_last(x)))

  rownames(dataset) <- make.names(dat.start[, 1], unique = TRUE)
  dat.start <- as.Date(sapply(gsub("\\.|[[:digit:]]", "", rownames(dataset)), function(x) dataset[[x, x]]), origin = "1970-01-01")
  rownames(dataset) <- make.names(dat.end[, 1], unique = TRUE)
  dat.end <- as.Date(sapply(gsub("\\.|[[:digit:]]", "", rownames(dataset)), function(x) dataset[[x, x]]), origin = "1970-01-01")
  rownames(dataset) <- 1:nrow(dataset)



  # Get names of species that are match seasonal and the dataset
  spp <- unique(gsub(paste0("(", paste(sp.use, collapse = "|"), ")", "(*SKIP)(*FAIL)|."), "", names(dataset)[grep(paste(sp.use, collapse = "|"), colnames(dataset),
    ignore.case = TRUE
  )], perl = TRUE, ignore.case = TRUE))

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
    seasonsub <- subset(seasonaldat, seasonaldat[grep("date", names(seasonaldat), ignore.case = TRUE)[1]] != "" & 
                          seasonaldat[grep("date", names(seasonaldat),ignore.case = TRUE)[2]] != "")
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
          NA
          )
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
        dataset[[paste0("SeasonID", spp[i])]] <- 0
      } else {
        dataset[[paste0("SeasonID", spp[i])]] <- (dat.start > date_parser(seasontemp[[find_first(seasontemp)]][1])) %in% 
          (dat.end < date_parser(seasontemp[[find_last(seasontemp)]][1]))
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
        dataset[[paste0("SeasonID", spp[i])]] <- 0
      } else {
        dataset[[paste0("SeasonID", spp[i])]] <- (dat.start > date_parser(seasontemp[[find_first(seasontemp)]][1])) %in% 
          (dat.end < date_parser(seasontemp[[find_last(seasontemp)]][1]))
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
          dataset[which(dataset[[loc.name]] == loca[j]), paste0("SeasonID", spp[i])] <- 0
        } else {
          dataset[which(dataset[[loc.name]] == loca[j]), paste0("SeasonID", spp[i])] <- (dat.start[which(dataset[[loc.name]] == loca[j])] > date_parser(seasontemp[[find_first(seasontemp)]][1])) %in%
            (dat.end[which(dataset[[loc.name]] == loca[j])] < date_parser(seasontemp[[find_last(seasontemp)]][1]))
        }
      }
    }
  }

  create_seaonal_ID_function <- list()
  create_seaonal_ID_function$functionID <- "create_seaonal_ID"
  create_seaonal_ID_function$args <- list(dat, project, seasonal.dat, use.location, use.geartype, sp.col)
  create_seaonal_ID_function$kwargs <- list(target = target)
  create_seaonal_ID_function$output <- dat
  log_call(project, create_seaonal_ID_function)

  return(dataset)
}
