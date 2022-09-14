#' Create the distance matrix
#'
#' @param dataset Primary data set
#' @param spat Spatial table. 
#' @param spatID Column name for Zone/area ID. 
#' @param alt_var Alternative choice location
#' @param occasion Define choice location
#' @param occasion_var Identify variable(s) needed to define choice location.
#' @param dataZoneTrue Include zone
#' @param zone_cent  Zonal centroid table.
#' @param fish_cent  Fishing centroid table.
#' @param choice Choice zone
#' @param units Distance units
#' @param port Port table
#' @param zoneRow Zone row
#' @param zoneID Zone identifier
#' @importFrom sf st_as_sf st_distance
#' @importFrom dplyr left_join
#' @export 
#' @keywords internal
#' @details Function is called by \code{\link{make_model_design}} generate the 
#'   distance matrix. Alternative fishing options come from the Alternative Choice 
#'   list, generated from the \code{\link{create_alternative_choice}} function.
#' @return
#' Distance matrix based on choices made in create_alternative_choice

create_dist_matrix <-
  function(dataset,
           spat = NULL, 
           spatID = NULL,
           alt_var,
           occasion,
           occasion_var = NULL,
           dataZoneTrue,
           zone_cent = NULL,
           fish_cent = NULL,
           choice,
           units,
           port = NULL,
           zoneRow,
           zoneID) {
    
  # Q: is int (centroid table) always required?
    # No, not always needed, ex: haul end - all haul locs
  # occasion ----
  
  ## Grid ----
  #Steps if alternative matrix come from gridded data file 
  #The distance matrix is the gridded data file
    
  # if (!is.null(X)) {
  #   altChoiceUnits <- units
  # 
  #   #Identify if using centroid or other for altToLocal2
  #   allZP <- dataset[, grep("AREA|zone", colnames(dataset), ignore.case =T)[1]] # get zonal type variables
  # 
  #   if (all(is.null(allZP)) || alt_var > length(allZP)) {
  # 
  #     v2 <- 0 # zonal centroid
  # 
  #   } else {
  # 
  #     v2 <- allZP(alt_var) #
  #   }
  # 
  #   if (v2 == 0) {
  # 
  #     altToLocal1 <- ""
  #     altToLocal2 <- "Centroid of Zonal Assignment"
  # 
  #   } else {
  # 
  #     altToLocal1 <- ""
  #     altToLocal2 <- alt_var
  #   }
  # 
  #   altChoiceType <- "loaded grid data matrix"
  #   B <- zoneRow
  #   choiceZ <- ""
  #   # End Grid Matrix
  # } else {
  
  # index of zones that meet min haul requirement
  zone_ind <- which(dataZoneTrue == 1)
  
  # steps if alternative matrix comes from loaded data (expectations)
  ## Centroid ----
  if (occasion %in% c("zonal centroid", "fishing centroid")) {

    o_var <- ifelse(is_value_empty(occasion_var), zoneID, occasion_var)
    
    fromXY <- dataset[zone_ind, o_var]
    
    if (length(o_var == 1)) {
      # join centroid lon-lat by zoneID
      if (occasion == "zonal centroid") cent_tab <- zone_cent
      else if (occasion == "fishing centroid") cent_tab <- fish_cent 
      
      join_by <- stats::setNames("ZoneID", o_var)
      fromXY <- dplyr::left_join(fromXY, cent_tab, by = join_by)
      
      fromXY <- fromXY[, c("cent.lon", "cent.lat")]
    }

    altToLocal1 <- "Centroid of Zonal Assignment"
    
  } else if (occasion == "port") {
    ## port ----
    fromXY <- dataset[zone_ind, occasion_var]
    
    if (length(occasion_var) == 1) {
      
      fromXY[[occasion_var]] <- trimws(fromXY[[occasion_var]])
      port$Port_Name <- trimws(port$Port_Name)
      
      if (all(unique(fromXY[[occasion_var]]) %in% unique(port$Port_Name)) == FALSE) {
        
        stop("At least one port not included in PortTable.", call. = FALSE)
      }
      
      join_by <- stats::setNames("Port_Name", occasion_var)
      fromXY <- dplyr::left_join(fromXY, port, by = join_by)
      fromXY <- fromXY[, c("Port_Long", "Port_Lat")] # drop port name, lon-lat order
    }
      
  } else {  
      
    ## Lon-Lat ----
    fromXY <- dataset[zone_ind, occasion_var]
  }
    
    altToLocal1 <- occasion

  
  # alt_var ----
  
  ## centroid ----
  if (alt_var %in% c("zonal centroid", "fishing centroid")) {
    
    if (alt_var == "zonal centroid") cent_tab <- zone_cent
    else if (alt_var == "fishing centroid") cent_tab <- fish_cent 
    
    if (!any(cent_tab$ZoneID %in% unique(choice[zone_ind]))) {
      
      stop('Name of zones in centroid table do not match choice zones. Rerun ',
           'find_centroid()', call. = FALSE)
    }
    
    # filter centroid table 
    cent_tab <- cent_tab[cent_tab$ZoneID %in% unique(choice[zone_ind]),] 
    toXY <- cent_tab[, c("cent.lon", "cent.lat")]
    altToLocal2 <- "Centroid of Zonal Assignment"
    
  } else if (alt_var == "nearest point") {
    
    ## nearest point ----
    
    if (is_value_empty(spat) | is_value_empty(spatID)) {
      
      stop("'spat' and 'spatID' are required for alt_var = 'nearest point'.", 
           call. = FALSE)
    }
    
    column_check(spat, spatID)
    
    spat <- check_spatdat(spat)
    
    if (all(unique(choice) %in% spat[[spatID]]) == FALSE) {
      
      stop("'spat' contains zones not found in alternative choice list. Do you ",
           "have the correct spatial table?", call. = FALSE)
    }
    
    altToLocal2 <- alt_var
  } 
  
  
  # Distance Matrix ----
  # Test for potential issues with data
  if (any(qaqc_helper(fromXY, "NaN"))) {
    
    stop(paste("NaN found in ", altToLocal1, ". Design file aborted."), 
         call. = FALSE)
  }
     
    if (any(qaqc_helper(toXY, "NaN"))) {
    
    stop(paste("NaN found in ", altToLocal2, ". Design file aborted."), 
         call. = FALSE)
  }

  # convert to sf object
  fromXY[seq_along(fromXY)] <- lapply(fromXY, as.numeric)
  fromXY <- sf::st_as_sf(fromXY, coords = c(find_lon(fromXY), find_lat(fromXY)))
    
  if (alt_var == "nearest point") {
    
    toXY <- spat[spatID %in% unique(choice), spatID]
    
  } else {
    
    toXY[seq_along(toXY)] <- lapply(toXY, as.numeric)
    toXY <- sf::st_as_sf(toXY, coords = c(find_lon(toXY), find_lat(toXY)))
  }
  
  distMatrix <- sf::st_distance(fromXY, toXY)
  
  # Note: distGeo uses meters
  # # Note: this will break if cols aren't in long-lat order.
  # distMatrix <- geosphere::distm(fromXY[, c(find_lon(fromXY), find_lat(fromXY))], 
  #                                toXY[, c(find_lon(toXY), find_lat(toXY))])
    
  altChoiceType <- "distance"
  
  if (units %in% c("meters", "M", "m")) {
    
    altChoiceUnits <- "meters"
    
  } else if (units %in% c("kilometers", "KM", "km")) {
    
    distMatrix <- distMatrix / 1000
    altChoiceUnits <- "kilometers"
    
  } else if (units == "miles") {
    
    distMatrix <- distMatrix * 0.000621371192237334
    altChoiceUnits <- "miles"
    
  } else {
    
    altChoiceUnits <- units
  }
  # DM list ----
  return(list(distMatrix = distMatrix, 
              altChoiceUnits = altChoiceUnits, 
              altChoiceType = altChoiceType, 
              altToLocal1 = altToLocal1,
              altToLocal2 = altToLocal2))
}
