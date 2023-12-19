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
#' @param crs Coordinate reference system. 
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
           zoneID,
           crs = NULL) {
    
  # index of zones that meet min haul requirement
  zone_ind <- which(dataZoneTrue == 1)
  o_var <- NULL
  
  if (is.null(crs) | is_empty(crs)) {
    
    crs <- 4326
    warning("CRS is not specfied, distance matrix will be created using WGS 84 (4326).",
            call. = FALSE)
  }
  # occasion ----
  
  ## Centroid ----
  if (occasion %in% c("zonal centroid", "fishing centroid")) {
    
    if (is_value_empty(occasion_var)) o_var <- zoneID
    else o_var <- occasion_var
    
    fromXY <- dataset[zone_ind, o_var]
    
      if (occasion == "zonal centroid") cent_tab <- zone_cent
      else if (occasion == "fishing centroid") cent_tab <- fish_cent 
      
    if (o_var == zoneID | !any(fromXY[[o_var]] %in% port$Port_Name)) {
      # o_var == zoneID = single haul
      # otherwise, multi-haul w/ prev area variable
      # join centroid lon-lat by zoneID
      join_by <- stats::setNames("ZoneID", o_var)
      fromXY <- dplyr::left_join(fromXY, cent_tab, by = join_by)
      
    } else { # Previous Area (multi-haul)
      # include port table previous area if o_var contains port names (this happens 
      # when creating a previous area/starting loc variable and the port is not within a zone)
      
      area_tab <- 
        do.call(rbind, 
                lapply(list(port, cent_tab), function(x) {
                  setNames(x, c(o_var, "cent.lon", "cent.lat"))
                })
        )
      
      if (class(area_tab[[o_var]]) != class(fromXY[[o_var]])) {
        
        stop(o_var, ' from dat cannot be joined to port table: \n', o_var, ' is class ',
             class(fromXY[[o_var]]), ' while the port name column is ', 
             class(area_tab[[o_var]]), '. \nChange ', o_var, ' to class ', 
             class(area_tab[[o_var]]), ' by using change_class() ',
             'then rerun create_alternative_choice() and check_model_data().', call. = FALSE)
      }
      
      fromXY <- dplyr::left_join(fromXY, area_tab, by = o_var)
    }
      
    fromXY <- fromXY[, c("cent.lon", "cent.lat")]
      
  } else if (occasion == "port") {
    ## port ----
    fromXY <- dataset[zone_ind, occasion_var] 
    
    # if this will determine whether port lon-lats need to be merged or exist in
    # the primary table
    if (length(occasion_var) == 1) { # merge port table w/ primary
      
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
    # occasion_var should be names of lon-lat cols
    fromXY <- dataset[zone_ind, occasion_var]
  }
  
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
    
  } else if (alt_var == "nearest point") {
    
    ## nearest point ----
    
    if (is_value_empty(spat) | is_value_empty(spatID)) {
      
      stop("'spat' and 'spatID' are required for alt_var = 'nearest point'.", 
           call. = FALSE)
    }
    
    column_check(spat, spatID)
    
    # TODO: check whether shift_long() affects distance matrix
    spat <- check_spatdat(spat)
    
    if (is.na(crs)) {
      # for testing purposes only
      sf::st_crs(spat) <- NA
      warning('crs = NA should only be used for testing purposes.', call. = FALSE)
      
    } else {
      
      spat <- sf::st_transform(spat, crs = crs)
    }
    
    if (all(unique(choice) %in% spat[[spatID]]) == FALSE) {
      
      stop("'spat' contains zones not found in alternative choice list. Do you ",
           "have the correct spatial table?", call. = FALSE)
    }
  } 
  
  # Distance Matrix ----
  # Test for potential issues with data
  if (any(qaqc_helper(fromXY, "NaN"))) {
    
    stop(paste("NaN found in ", occasion, ". Design file aborted."), 
         call. = FALSE)
  }

  # convert to sf object
  fromXY[seq_along(fromXY)] <- lapply(fromXY, as.numeric)
  fromXY <- sf::st_as_sf(fromXY, 
                         coords = c(find_lon(fromXY), find_lat(fromXY)),
                         crs = crs)
    
  if (alt_var == "nearest point") {
    
    toXY <- spat[spat[[spatID]] %in% unique(choice), spatID]
    # transform fromXY CRS to match
    
    if (is.na(crs)) {
      # for testing purposes only
      sf::st_crs(fromXY) <- NA
      
    } else {
      
      fromXY <- sf::st_transform(fromXY, crs = sf::st_crs(toXY))
    }
    
    z_nms <- sort(unique(choice))
    
  } else {
    
    if (any(qaqc_helper(toXY, "NaN"))) {
      
      stop(paste("NaN found in ", alt_var, ". Design file aborted."), 
           call. = FALSE)
    }
    
    toXY[seq_along(toXY)] <- lapply(toXY, as.numeric)
    toXY <- sf::st_as_sf(toXY, 
                         coords = c(find_lon(toXY), find_lat(toXY)),
                         crs = crs)
    
    z_nms <- cent_tab$ZoneID
  }
  
  if (any(is_value_empty(fromXY) | is_value_empty(toXY))) {
    
    stop("Error in creating distance matrix: empty spatial table", call. = FALSE)
  }
  
  distMatrix <- sf::st_distance(fromXY, toXY)
  
  units(distMatrix) <- units
  
  if (is.null(o_var)) o_var <- occasion_var
  
  if (occasion == "lon-lat") r_nms <- NULL
  else r_nms <- dataset[[o_var]][zone_ind]
  
  dimnames(distMatrix) <- list(r_nms, z_nms)
  
  altChoiceType <- "distance"
  altChoiceUnits <- units

  # DM list ----
  return(list(distMatrix = distMatrix, 
              altChoiceUnits = altChoiceUnits, 
              altChoiceType = altChoiceType, 
              occasion = occasion,
              occasion_var = o_var,
              alt_choice = alt_var))
}
