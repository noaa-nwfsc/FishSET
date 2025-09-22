#' Create Previous Location/Area Variable
#'
#' Creates a variable of the previous port/zone (previous area) or the previous
#' longitude/latitude for a vessel. 
#'
#'@param dat Primary data containing information on hauls or trips. Table in FishSET 
#'  database contains the string 'MainDataTable'.
#'@param spat A spatial data file containing information on fishery management 
#'  or regulatory zones boundaries. `sf` objects are recommended, but `sp` objects 
#'  can be used as well. See [dat_to_sf()] to convert a spatial table read from 
#'  a csv file to an `sf` object. To upload your spatial data to the FishSETFolder 
#'  see [load_spatial()].
#'@param project String, name of project.
#'@param starting_port The name of the starting (or disembarking) port in `dat`.
#'@param v_id The name of the variable in `dat` that uniquely identifies vessels.
#'@param tripID Variable name in `dat` that uniquely identifies trips.
#'@param haulID Variable name in `dat` that uniquely identifies hauls.
#'@param zoneID Name of zone ID column in `dat`. Used to identify the previous
#'  area. Required for previous area variable.
#'@param spatID Name of zone ID column in `spat`. `spat` is used to assign ports
#'  to spatial areas. Required for previous area variable.
#'@param date Optional, a date variable to order hauls by. 
#'@param lon Longitude variable from `dat`. Required for previous location
#'  variable. 
#'@param lat Latitude variable from `dat`. Required for previous location variable.
#'@export
#'@importFrom dplyr arrange across all_of group_by mutate ungroup
#'@md
#'@details `previous_loc()` can create a previous area or location variable. 
#'  "Previous area" is defined as the port or zone the vessel last visited. The first
#'  area for each trip is the disembarking port (`starting_port`). If a
#'  port is within a zone, the zone is returned. If a port is not within a zone, 
#'  the name of the port is returned. "Previous location" is defined as the 
#'  previous longitude and latitude of the vessel. The first set of coordinates
#'  is the location of the port. Users must have a port table saved to the 
#'  FishSET database to use this function (see [load_port()]). This variable 
#'  can be used to define the distance matrix (see [create_alternative_choice()]).
#'
#'

previous_loc <- function(dat,
                         spat,
                         project,
                         starting_port,
                         v_id,
                         tripID,
                         haulID,
                         zoneID = NULL,
                         spatID = NULL,
                         date = NULL,
                         lon = NULL,
                         lat = NULL) {
  
  # TODO: add function to app
  # Note: two scenarios for previous area: port is within zone dataset or is not. 
  # if inside a zone, return the zone the port is in. If outside the zone, return the
  # name of the port. Convert variable to character. 
  
  #call in data
  spat_out <- data_pull(spat, project)
  spatdat <- spat_out$dataset
  spat <- parse_data_name(spat, "spat", project)
  
  out <- data_pull(dat, project)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main", project)
  
  if (!table_exists(paste0(project, 'PortTable'), project)) {
    
    stop('A port table is required. Use load_port() to save a port table to the',
         ' FishSET database.', call. = FALSE)
  }
  
  port <- table_view(paste0(project, 'PortTable'), project)
  
  dataset <- dplyr::arrange(dataset, dplyr::across(dplyr::all_of(c(v_id, date, tripID, haulID))))
  
  lon_empty <- is_value_empty(lon)
  lat_empty <- is_value_empty(lat)
  
  lonlat_check <- sum(c(lon_empty, lat_empty))
  
  if (lonlat_check == 1) {
    
    stop("'lon' or 'lat' is empty.", call. = FALSE)
    
  } else if (lonlat_check == 2) { # previous area
    
    stopifnot('zoneID required to create a previous area variable' = !is_value_empty(zoneID),
              'spatID required to create a previous area variable' = !is_value_empty(spatID))
    
    dataset[[starting_port]] <- trimws(dataset[[starting_port]])
    
    # group by vessel ID and trip ID
    dataset <- 
      dataset %>% 
      dplyr::group_by(dplyr::across(dplyr::all_of(c(v_id, tripID)))) %>% 
      dplyr::mutate(dplyr::across(dplyr::all_of(zoneID), .fns = lag, .names = "prev_{.col}")) %>% 
      dplyr::ungroup()
    
    prev_name <- paste0("prev_", zoneID)
    
    port <- assignment_column(port, project, 
                              spat = spatdat, 
                              lon.dat = find_lon(port), 
                              lat.dat = find_lat(port),
                              name = zoneID,
                              cat = spatID, 
                              log.fun = FALSE)
    
    p_ind <- which(is.na(dataset[[prev_name]]))
    
    # this won't work if port name from dataset isn't in the port table ('other' could also be an issue)
    prev_area_update <- 
      unlist(
        lapply(p_ind, function(i) {
         
          fp <- dataset[[starting_port]][i] 
          
          if (fp %in% port[[1]]) {
            
            out <- port[port[[1]] == fp, ][[zoneID]]
            
            if (is.na(out)) return(fp) # return the name of port if not in zone
            else return(out)
            
          } else NA
        })
      )
    
    if (any(is.na(prev_area_update))) {
      
      warning('NAs produced. At least one disembarked port was not found in the port table.', 
              call. = FALSE)
    }
   
    dataset[[prev_name]][p_ind] <- prev_area_update
    
  } else if (lonlat_check == 0) { # previous location
    
    stopifnot('lon required to create a previous area variable' = !is_value_empty(lon),
              'lat required to create a previous area variable' = !is_value_empty(lat))
    
    # group by vessel ID and trip ID
    dataset <- 
      dataset %>% 
      dplyr::group_by(dplyr::across(dplyr::all_of(c(v_id, tripID)))) %>% 
      dplyr::mutate(dplyr::across(dplyr::all_of(c(lon, lat)), .fns = lag, .names = "prev_{.col}")) %>% 
      ungroup()
    
    prev_lon <- paste0("prev_", lon)
    prev_lat <- paste0("prev_", lat)
    
    p_ind <- which(is.na(dataset[[prev_lon]])) # same as prev_lat
    
    prev_lon_update <- 
      unlist(
        lapply(p_ind, function(i) {
          
          fp <- dataset[[starting_port]][i]
          
          if (fp %in% port[[1]]) {
          
          port[port[[1]] == fp, ][[find_lon(port)]]
          } else NA
        })
      )
    
    prev_lat_update <- 
      unlist(
        lapply(p_ind, function(i) {
          
          fp <- dataset[[starting_port]][i]
          
          if (fp %in% port[[1]]) {
            
            port[port[[1]] == fp, ][[find_lat(port)]]
          } else NA
        })
      )
    
    if (any(is.na(prev_lon_update)) | any(is.na(prev_lat_update))) {
      
      warning('NAs produced. At least one disembarked port was not found in the port table.', 
              call. = FALSE)
    }
      
    dataset[[prev_lon]][p_ind] <- prev_lon_update
    dataset[[prev_lat]][p_ind] <- prev_lat_update
    
  } else stop("Error, only one lon and lat variable should be used.", call. = FALSE)
  
  # log function
  previous_loc_function <- list()
  previous_loc_function$functionID <- "previous_loc"
  previous_loc_function$args <- list(dat, spat, project, starting_port, v_id,
                                     tripID, haulID, zoneID, spatID, date, lon, lat)
  log_call(project, previous_loc_function)
  
  dataset
}

