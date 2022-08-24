#' Create the distance matrix
#'
#' @param dataset Primary data set
#' @param alt_var Alternative choice location
#' @param occasion Define choice location
#' @param dataZoneTrue Include zone
#' @param int  Zone centroid data frame
#' @param choice Choice zone
#' @param units Distance units
#' @param port Port table
#' @param zoneRow Zone row
#' @param X distance matrix
#' @param zoneID Zone identifier
#' @importFrom geosphere distm
#' @importFrom stringi stri_count
#' @export 
#' @keywords internal
#' @details Function is called by \code{\link{make_model_design}} generate the 
#'   distance matrix. Alternative fishing options come from the Alternative Choice 
#'   list, generated from the \code{\link{create_alternative_choice}} function.
#' @return
#' Distance matrix based on choices made in create_alternative_choice

create_dist_matrix <-
  function(dataset,
           alt_var,
           occasion,
           dataZoneTrue,
           int,
           choice,
           units,
           port,
           zoneRow,
           X,
           zoneID) {
    
  # Q: is int (centroid table) always required?
    # No, not always needed, ex: haul end - all haul locs
  # occasion ----
  
  ## Grid ----
  #Steps if alternative matrix come from gridded data file 
  #The distance matrix is the gridded data file
  if (!is.null(X)) {
    altChoiceUnits <- units
    
    #Identify if using centroid or other for altToLocal2
    allZP <- dataset[, grep("AREA|zone", colnames(dataset), ignore.case =T)[1]] # get zonal type variables
    
    if (all(is.null(allZP)) || alt_var > length(allZP)) {
      
      v2 <- 0 # zonal centroid
      
    } else {
      
      v2 <- allZP(alt_var) #
    }
    
    if (v2 == 0) {
      
      altToLocal1 <- ""
      altToLocal2 <- "Centroid of Zonal Assignment"
      
    } else {
      
      altToLocal1 <- ""
      altToLocal2 <- alt_var
    }
    
    altChoiceType <- "loaded grid data matrix"
    B <- zoneRow
    choiceZ <- ""
    # End Grid Matrix
  } else {
    
    zone_ind <- which(dataZoneTrue == 1)
    
    # steps if alternative matrix comes from loaded data (expectations)
    ## Centroid ----
    if (any(grepl("centroid|zon", occasion, ignore.case = TRUE))) {

      # TODO: use better names for temp and toXY1 (occasion_loc)
      # Note: this is the zone where haul occurred, not where it decided to go next
      # 
      temp <- dataset[zone_ind, zoneID]
      # Q: should this be changed to ZoneID instead of keeping original name?
      colnames(temp) <- "ZoneID"
      # switch to left_join() (faster)
      temp <- merge(temp, int, all.x = TRUE)
      toXY1 <- temp[zone_ind, 2:3]
      altToLocal1 <- "Centroid of Zonal Assignment"
      
    } else {
    ## Port ----
      if (any(grepl("Port", occasion, ignore.case = TRUE))) {
        
        if (is.data.frame(dataset)) {
          
          if (any(is_empty(dataset[[occasion]]))) {
            
            breakearly <- 1
            stop("occasion does not exist in dataset", call. = FALSE)
          }
          # Note: occasion = name of port var in primary data
          toXYa <- data.frame(dataset[[occasion]][zone_ind]) # subset data to when dataZoneTrue==1
          # Note: rename toXYa to something related to ports 
          colnames(toXYa) <- c(occasion) # ports from primary data
          toXYa[[occasion]] <- trimws(toXYa[[occasion]])
          colnames(port)[1] <- occasion
          port[[occasion]] <- trimws(port[[occasion]]) # port table
          
          
          if (all(unique(toXYa[[occasion]]) %in% unique(port[[occasion]])) == FALSE) {
            
            stop("At least one port not included in PortTable.", call. = FALSE)
          }
          
          toXY1 <- merge(toXYa, port) # merge port locations w/ primary data
          
        # Data from list
        } else {
          
          stop("Primary data must be a dataframe.", call. = FALSE)
        }
        
      } else {  #End port
        
        ## Lon-Lat ----
        # Data is from a data frame or matrix
        if (is.data.frame(dataset)) {
          
          if (length(occasion) < 2) {
            
            stop("Please define both lat and long in occasion parameter of ",  
                 "create_alternative_choice function.", call. = FALSE)
          }
          
          if (all(occasion %in% names(dataset)) == FALSE) {
            
            stop('At least one lat/lon variable was not found in the primary ', 
                 'data table. Distance matrix not created.', call. = FALSE)
          }
          
          if (any(grepl('lat', occasion, ignore.case=TRUE))) {
            
            toXY1 <- data.frame(
              
              dataset[[occasion[which(stringi::stri_count(occasion, '(?=LON|Lon|lon)')==max(stringi::stri_count(occasion, '(?=LON|Lon|lon)')))]]][zone_ind],
              dataset[[occasion[which(stringi::stri_count(occasion, '(?=LAT|lat|Lat)')==max(stringi::stri_count(occasion, '(?=LAT|Lat|lat)')))]]][zone_ind]
            )
          
          } else {
            
            # Note: if lat isn't found in col names, assume lon-lat order
            toXY1 <- data.frame(
              dataset[[occasion[1]]][zone_ind],
              dataset[[occasion[2]]][zone_ind]
            )
          }
          
        } else { # if not data.frame
          
          toXY1 <- dataset[[occasion]][zone_ind]
        } 
      }
      
      altToLocal1 <- occasion
    }
    
    # alt_var ----
    
    ## centroid ----
    if (any(grepl("zon|cent", alt_var, ignore.case = TRUE)) & length(alt_var)==1) {
     
      if (!any(int$ZoneID %in% unique(choice[zone_ind, ]))) {
        
        stop('Name of zones in centroid table do not match choice zones. Rerun ',
             'find_centroid', call. = FALSE)
      }
      
      B <- int[int$ZoneID %in% unique(choice[zone_ind, ]), "ZoneID"] 
      # Note: I don't think this works the way it's suppose to (doesn't make
      # sense to use zone_ind w/ the centroid table)
      # Also, choiceZ isn't used or saved
      choiceZ <- match(int$ZoneID[zone_ind], unique(int$ZoneID[zone_ind]))
      
      centersZone <- int[int$ZoneID %in% unique(choice[zone_ind, ]), 2:3] # Lat and Long
      altToLocal2 <- "Centroid of Zonal Assignment"
      
    } else {
      
      ## Lon-Lat ----
      if (is.data.frame(dataset)) {
        
        if (length(alt_var) < 2) {
          
          stop("Please define lon and lat in alt_var argument of ", 
               "create_alternative_choice function.", call. = FALSE)
        }
        
        if (any(is_empty(dataset[[alt_var[1]]]))) {
          
          stop("alt_var does not exist in dataset", call. = FALSE)
        }
        
        if (any(is_empty(dataset[[alt_var[2]]]))) {
          
          stop("alt_var does not exist in dataset", call. = FALSE)
        }
        
        toXY2 <- data.frame(
          dataset[[alt_var[1]]][zone_ind],
          dataset[[alt_var[2]]][zone_ind]
        )
        
      } else {
        
        toXY2 <- dataset[[alt_var]][zone_ind] # MUST be a LAT/LONG 
      }
      
      Bb <- unique(cbind(data.frame(choice[zone_ind, ]), data.frame(toXY2)))
      B <- Bb[, 1] # Assignment column
      centersZone <- Bb[, 2:3] # Latitude aned Longitude
      altToLocal2 <- alt_var
    } 
  } # End From loaded data
  
  # Distance Matrix ----
  # Test for potential issues with data
     
  if (any(do.call(cbind, lapply(toXY1, is.nan)))) {
    
    stop(paste("NaN found in ", altToLocal1, ". Design file aborted."), 
         call. = FALSE)
  }
     
  if (any(do.call(cbind, lapply(centersZone, is.nan)))) {
    
    stop(paste("NaN found in ", altToLocal2, ". Design file aborted."), 
         call. = FALSE)
  }

  # Q: why would toXY1 have more than 2 cols?
  if (ncol(toXY1) > 2) {
    
    # TODO: switch to sf::st_distance()? 
    # Note: different output, check units 
    if (FALSE) {
      
      st_zone <- sf::st_as_sf(toXY1[, 2:3],
                              coords = c(find_lon(toXY1), find_lat(toXY1)))
      st_cent <- sf::st_as_sf(centersZone[, 1:2],
                              coords = c(find_lon(centersZone), find_lat(centersZone)))

      distMatrix <- sf::st_distance(st_zone, st_cent)
    }
    
    # Note: if occasion = centroid, then this is calculating distance after the decision
    # of where to go was made, not before. Would need to know the 
    # centroid of the zone before departure occurred (i.e. location of last haul).
    # Note: distGeo uses meters
    toXY1[, 2] <- as.numeric(toXY1[, 2])
    toXY1[, 3] <- as.numeric(toXY1[, 3])
    distMatrix <- geosphere::distm(toXY1[, c(find_lon(toXY1), find_lat(toXY1))], 
                                   centersZone[, c(find_lon(centersZone), 
                                                   find_lat(centersZone))])
  } else {
    
    # Note: this will break if cols aren't in long-lat order.
    toXY1[, 1] <- as.numeric(toXY1[, 1])
    toXY1[, 2] <- as.numeric(toXY1[, 2])
    
    distMatrix <- geosphere::distm(toXY1[, c(find_lon(toXY1), find_lat(toXY1))], 
                                   centersZone[, c(find_lon(centersZone), 
                                                   find_lat(centersZone))])
  }
  # st_distance() has a units argument, have function take care of this
  #
  altChoiceType <- "distance"
  
  if (units %in% c("meters", "M", "m")) {
    
    X <- distMatrix
    altChoiceUnits <- "meters"
    
  } else if (units %in% c("kilometers", "KM", "km")) {
    
    X <- distMatrix / 1000
    altChoiceUnits <- "kilometers"
    
  } else if (units == "miles") {
    
    X <- distMatrix * 0.000621371192237334
    altChoiceUnits <- "miles"
    
  } else {
    
    X <- distMatrix
    altChoiceUnits <- units
  }
  # DM list ----
  return(list(X = X, 
              altChoiceUnits = altChoiceUnits, 
              altChoiceType = altChoiceType, 
              altToLocal1 = altToLocal1,
              altToLocal2 = altToLocal2))
}
