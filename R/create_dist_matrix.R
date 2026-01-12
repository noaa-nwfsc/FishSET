#' Create the distance matrix
#'
#' @param dataset Primary data set
#' @param unique_obs_id Column name in 'dataset' that represents unique observation (row) id.
#' @param zoneID Column name in 'dataset' for the zone identifier.
#' @param spat Spatial table (sf object) containing polygons or points for locations. 
#' @param spatID Column name in 'spat' for the Zone/area ID. 
#' @param alt_var Defines the alternative choice location. One of c("zonal centroid", "fishing
#'   centroid", "nearest point").
#' @param occasion Defines the origin location. One of c("zonal centroid", "fishing centroid", 
#'   "port", "lon-lat").
#' @param occasion_var Variable(s) needed to define the origin location.
#'   - For 'occasion = "centroid"': The column name for the previous area.
#'   - For 'occasion = "port"': The column name in 'dataset' for port.
#'   - For 'occasion = "lon-lat"': A character vector of c(lon, lat) column names. Note that
#'   longitude must be the first column name in the vector.
#' @param dataZoneTrue Logical vector indicating which rows (observations) meet the minimum
#'   haul/obs requirement.
#' @param zone_cent Zonal centroid table (data.frame with zone ID, cent.lon, cent.lat).
#' @param fish_cent  Fishing centroid table (data.frame with zone ID, cent.lon, cent.lat).
#' @param choice Vector of observed choice zones, same length as 'dataset'.
#' @param units Distance units (e.g., "km", "mi", "nm"). Passed to 'sf::st_distance'.
#' @param port Port table (data.frame with Port_Name, Port_Long, Port_Lat).
#' @param crs Coordinate reference system (numeric EPSG code or PROJ string).
#' 
#' @importFrom sf st_as_sf st_distance st_crs st_transform
#' @importFrom dplyr left_join
#' @importFrom stats setNames
#' @export 
#' @keywords internal
#' @details Function is called by \code{\link{format_model_data}} to generate the
#'   distance matrix. Alternative fishing options come from the Alternative Choice
#'   list, generated from the \code{\link{create_alternative_choice}} function.
#' @return
#' A list containing:
#' \itemize{
#'   \item `dist_matrix`: The calculated distance matrix.
#'   \item `alt_choice_units`: The units of distance.
#'   \item `alt_choice_type`: "distance".
#'   \item `occasion`: The origin type used.
#'   \item `occasion_var`: The variable(s) used for the origin.
#'   \item `alt_choice`: The destination type used.
#' }

create_dist_matrix <-
  function(dataset,
           unique_obs_id,
           zoneID,
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
           crs = NULL) {
    
    # Index of zones that meet min haul requirement 
    zone_ind <- which(dataZoneTrue == 1)
    if(length(zone_ind) == 0){
      stop("No observations meet the 'dataZoneTrue' criteria.", call. = FALSE)
    }
    
    o_var <- NULL # Initialize origin variable tracker
    
    ## Set CRS ------------------------------------------------------------------------------------
    if (is.null(crs) | is_empty(crs)) {
      crs <- 4326
      warning("CRS is not specfied, distance matrix will be created using WGS 84 (4326).",
              call. = FALSE)
    }
    
    ## Prepare occasion data (fromXY) -------------------------------------------------------------
    if (occasion %in% c("zonal centroid", "fishing centroid")) {
      ### Centroid --------------------------------------------------------------------------------
      if (is_value_empty(occasion_var)) {
        o_var <- zoneID
      } else {
        o_var <- occasion_var
      }
      
      fromXY <- dataset[zone_ind, c(unique_obs_id, o_var)]
      
      if (occasion == "zonal centroid") {
        cent_tab <- zone_cent
      } else if (occasion == "fishing centroid") {
        cent_tab <- fish_cent 
      }
      
      if (o_var == zoneID){ # Single haul case
        join_by <- stats::setNames("ZoneID", o_var) # join centroid lon-lat by zoneID
        fromXY <- dplyr::left_join(fromXY, cent_tab, by = join_by)
        
      } else { # Multi-haul case
        # Combine port and centroid tables for a single join
        area_tab <- 
          do.call(rbind, 
                  lapply(list(port, cent_tab), function(x) {
                    if (is.null(x)) return (NULL)
                    # Standardize column names for rbind
                    setNames(x, c(o_var, "cent.lon", "cent.lat"))
                  }))
        
        if (is.null(area_tab)) {
          stop("Both 'port' and 'cent_tab' are NULL for a multi-haul occasion.", call. = FALSE)        
        }
        
        # Check for class mismatches before joining
        if (class(area_tab[[o_var]]) != class(fromXY[[o_var]])) {
          stop(o_var, ' from dat cannot be joined to port table: \n', o_var, ' is class ',
               class(fromXY[[o_var]]), ' while the port name column is ', 
               class(area_tab[[o_var]]), '. \nChange ', o_var, ' to class ', 
               class(area_tab[[o_var]]), ' by using change_class() ',
               'then rerun create_alternative_choice() and check_model_data().', call. = FALSE)
        }
        
        fromXY <- dplyr::left_join(fromXY, area_tab, by = o_var)
      }
      
      fromXY <- fromXY[, c(unique_obs_id, "cent.lon", "cent.lat")]
      
    } else if (occasion == "port") {
      ### Port ------------------------------------------------------------------------------------
      fromXY <- dataset[zone_ind, c(unique_obs_id, occasion_var)] 
      
      if (length(occasion_var) == 1) { # merge port table w/ primary
        fromXY[[occasion_var]] <- trimws(fromXY[[occasion_var]])
        port$Port_Name <- trimws(port$Port_Name)
        
        if (all(unique(fromXY[[occasion_var]]) %in% unique(port$Port_Name)) == FALSE) {
          missing_ports <- setdiff(unique(fromXY[[occasion_var]]), unique(port$Port_Name))
          stop("At least one port not included in PortTable: ", 
               paste(missing_ports, collapse = ", "), call. = FALSE)
        }
        
        join_by <- stats::setNames("Port_Name", occasion_var)
        fromXY <- dplyr::left_join(fromXY, port, by = join_by)
        fromXY <- fromXY[, c(unique_obs_id, "Port_Long", "Port_Lat")] # Use port coords
        
      } else{
        # Assume occasion_var = c(lon, lat) and columns already in dataset
        fromXY <- fromXY[, c(unique_obs_id, occasion_var)]
      }
      
    } else if (occasion == 'lon-lat') {
      ### Lon-lat ---------------------------------------------------------------------------------
      # occasion_var should be names of lon-lat cols
      fromXY <- dataset[zone_ind, c(unique_obs_id, occasion_var)]
      
    } else {
      stop("Invalid 'occasion' type specified.", call. = FALSE)
    }
    
    ## Prepare alternative data (toXY) ------------------------------------------------------------
    if (alt_var %in% c("zonal centroid", "fishing centroid")) {
      ### Centroid --------------------------------------------------------------------------------
      if (alt_var == "zonal centroid") {
        cent_tab <- zone_cent
      } else if (alt_var == "fishing centroid") {
        cent_tab <- fish_cent 
      } 
      
      if (!any(cent_tab$ZoneID %in% unique(choice[zone_ind]))) {
        stop('Name of zones in centroid table do not match choice zones. Rerun ',
             'find_centroid()', call. = FALSE)
      }
      
      # Filter centroid table to include only relevant choices
      cent_tab <- cent_tab[cent_tab$ZoneID %in% unique(choice[zone_ind]),]
      cent_tab <- cent_tab[order(cent_tab$ZoneID), ] # Ensure consistent order
      toXY <- cent_tab[, c("cent.lon", "cent.lat")]
      z_nms <- cent_tab$ZoneID # Column names for matrix
      
    } else if (alt_var == "nearest point") {
      ### Nearest point ---------------------------------------------------------------------------
      if (is_value_empty(spat) || is_value_empty(spatID)) {
        stop("'spat' and 'spatID' are required for alt_var = 'nearest point'.", 
             call. = FALSE)
      }
      
      # TODO: check whether shift_long() affects distance matrix
      column_check(spat, spatID)
      spat <- check_spatdat(spat)
      spat <- sf::st_transform(spat, crs = crs)
      
      if (all(unique(choice) %in% spat[[spatID]]) == FALSE) {
        missing_zones <- setdiff(unique(choice[zone_ind]), spat[[spatID]])
        stop("'spat' is missing zones found in choice list: ", 
             paste(missing_zones, collapse = ", "), call. = FALSE)
      }
      
      # Filter and reorder sf object
      toXY <- spat[spat[[spatID]] %in% unique(choice[zone_ind]), spatID]
      toXY <- toXY[order(toXY[[spatID]]), ]
      z_nms <- toXY[[spatID]] # Column names for matrix
      
    } else {
      stop("Invalid 'alt_var' type specified.", call. = FALSE)
    }
    
    ## Create distance matrix ---------------------------------------------------------------------
    # QAQC for occasion points
    if (any(qaqc_helper(fromXY, "NaN"))) {
      stop(paste0("NaN found in ", occasion, ". Design file aborted."), 
           call. = FALSE)
    }
    
    # Convert fromXY to sf object
    lon_name <- find_lon(fromXY)
    lat_name <- find_lat(fromXY)
    fromXY[c(lon_name,lat_name)] <- lapply(fromXY[c(lon_name,lat_name)], as.numeric)
    fromXY <- sf::st_as_sf(fromXY, 
                           coords = c(find_lon(fromXY), find_lat(fromXY)),
                           crs = crs)
    
    # Prepare toXY (if not already sf) and calculate distance
    if (alt_var == "nearest point") {
      # Ensure fromXY CRS matches toXY CRS
      fromXY <- sf::st_transform(fromXY, crs = sf::st_crs(toXY))
      
    } else {
      # toXY is a data.frame
      if (any(qaqc_helper(toXY, "NaN"))) {
        stop(paste("NaN found in ", alt_var, ". Design file aborted."), 
             call. = FALSE)
      }
      
      toXY[seq_along(toXY)] <- lapply(toXY, as.numeric)
      toXY <- sf::st_as_sf(toXY, 
                           coords = c(find_lon(toXY), find_lat(toXY)),
                           crs = crs)
    }
    
    if (any(is_value_empty(fromXY) | is_value_empty(toXY))) {
      stop("Error in creating distance matrix: empty spatial table", call. = FALSE)
    }
    
    dist_matrix <- sf::st_distance(fromXY, toXY)
    units(dist_matrix) <- units
    
    ## Format and return output -------------------------------------------------------------------
    # Set matrix names
    r_nms <- fromXY[[unique_obs_id]]
    dimnames(dist_matrix) <- list(r_nms, z_nms)
    
    # Store occasion_var for return list
    if (is.null(o_var)) o_var <- occasion_var
    
    return(list(dist_matrix = dist_matrix, 
                dist_units = units, 
                alt_choice_type = "distance", 
                occasion = occasion,
                occasion_var = o_var,
                alt_choice = alt_var))
  }
