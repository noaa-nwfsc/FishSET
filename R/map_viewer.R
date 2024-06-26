
# map viewer
map_viewer <- function(dat, project, spat, avd, avm, num_vars, temp_vars, id_vars, 
                       lon_start, lat_start, lon_end = NULL, lat_end = NULL) {
  #' Interactive vessel locations and fishery zones map
  #'
  #' View vessel locations and fishery zones on interactive map.
  #' @param dat Primary data containing information on hauls or trips. Table in 
  #'   FishSET database contains the string 'MainDataTable'.
  #' @param project Project name. 
  #' @param spat Spatial data containing information on fishery management or 
  #'   regulatory zones. Shape, json, geojson, and csv formats are supported.
  #' @param avd Variable name in the primary data file that gives the unique ID associated to the polygon.
  #' @param avm The name of the property in the GeoJson file that identifies the 
  #'   polygon to cross reference to \code{dat}. Variable name in the spatial file that represents the
  #'   unique ID.
  #' @param num_vars List, name of numeric variable(s) in \code{dat} to include for plotting.
  #' @param temp_vars List, name of temporal variable(s) in \code{dat} to include for plotting.
  #' @param id_vars List, name of categorical variable(s) in \code{dat} to group by.
  #' @param lon_start String, variable in \code{dat} that identifies a single 
  #'   longitude point or starting longitude decimal degrees.
  #' @param lat_start String, variable in \code{dat} that identifies a single 
  #'   latitude point or starting latitude decimal degrees.
  #' @param lon_end String, variable in \code{dat} that identifies ending longitude decimal degrees.
  #' @param lat_end String, variable in \code{dat} that identifies ending latitude decimal degrees.
  #' @importFrom sf st_write
  #' @importFrom jsonlite toJSON
  #' @importFrom servr httd
  #' @importFrom shiny isRunning
  #' @importFrom utils browseURL
  #' @export 
  #' @details The map_viewer function creates the files required to run the MapViewer 
  #' program. Users can map points or trip path. To plot points, leave \code{lon_end} 
  #' and \code{lat_end} and \code{NULL}. After creating the inputs, a map with zones 
  #' is opened in the default web browser. To close the server connection run 
  #' \code{servr::daemon_stop()} in the console. Lines on the map represent the starting
  #' and ending lat/long for each observation in the data set color coded based 
  #' on the selected variable. It can take up to a minute for the data to be loaded
  #' onto the map. At this time, the map can only be saved by taking a screen shot.
  #' @examples
  #' \dontrun{
  #' # Plot trip path
  #' map_viewer(scallopMainDataTable, 'scallop', "scallopTMSSpatTable", 
  #'            avd = 'ZoneID', avm = 'TEN_ID', num_vars = 'LANDED_thousands', 
  #'            temp_vars = 'DATE_TRIP', lon_start = 'previous_port_lon', 
  #'            lat_start = 'previous_port_lat', lon_end = 'DDLON', 
  #'            lat_end = 'DDLAT')
  #'    
  #' # Plot observed fishing locations        
  #' map_viewer(scallopMainDataTable, 'scallop', "scallopTMSSpatTable", 
  #'            avd = 'ZoneID', avm = 'TEN_ID', num_vars = 'LANDED_thousands', 
  #'            temp_vars = 'DATE_TRIP', lon_start = 'DDLON', lat_start = 'DDLAT')
  #' 
  #' #Plot haul path
  #' map_viewer(pollockMainDataTable, 'pollock', spat=spatdat, avd='NMFS_AREA',
  #' avm='NMFS_AREA', num_vars=c('HAUL','OFFICIAL_TOTAL_CATCH'),
  #' temp_vars='HAUL_DATE', id_vars=c('GEAR_TYPE', 'PORT'), 
  #'        'Lon_Start', 'Lat_Start', 'Lon_End', 'Lat_End')
  #' 
  #' #Plot haul midpoint
  #' map_viewer(pollockMainDataTable, 'pollock', spat=spatdat, avd='NMFS_AREA',
  #' avm='NMFS_AREA', num_vars=c('HAUL','OFFICIAL_TOTAL_CATCH'),
  #' temp_vars='HAUL_DATE', id_vars=c('GEAR_TYPE', 'PORT'), 'Lon_Mid', 'Lat_Mid')
  
  #' }
  
  
  ##
  # Load primary and spatial data files ----
  ##
  # Primary data
  out <- data_pull(dat, project)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main", project)
  # Spatial data
  out <- data_pull(spat, project)
  spatname <- parse_data_name(spat, 'spat', project)
  spatdat <- out$dataset
  
  if (!is.null(spatdat)) {
    unlink(paste0(loc_map(project = project), "spatdat.geojson"))
    sf::st_write(spatdat, dsn = paste0(loc_map(project = project), "spatdat.geojson"), 
                 overwrite = TRUE)
  }
  
  # Change date to YYYY-MM-DD format to pass to javascript
  dataset[[temp_vars]] <- format(dataset[[temp_vars]], "%Y-%m-%d")
  
  ##
  # Create data file ----
  ##
  #Start with path option
  if(!is.null(lon_end)){  
    # remove NAs from lat and lon
    dataset <- dataset[!is.na(dataset[[lon_start]]) & !is.na(dataset[[lat_start]]) & 
                         !is.na(dataset[[lon_end]]) & !is.na(dataset[[lat_end]]), ]
    
    
    dataset <- unique(dataset[, c(avd, num_vars, temp_vars, id_vars, 
                                  lon_start, lat_start, lon_end, lat_end)])
    dataset$uniqueID <- 1:nrow(dataset)
    
    write.csv(dataset, paste0(loc_map(project=project), "datafile.csv"))
    
    
    # 3. Create map config
    map_config <- list()
    # TODO: need to change this before release
    map_config$mapbox_token <- "pk.eyJ1IjoibWhhcnNjaDEyNSIsImEiOiJjbDI2b244ZmkwMHhjM2NvN3poNHZnajdkIn0.2yYSesDRvw4hSN5gQ1Ja-A"
    map_config$choosen_scatter <- num_vars[1]
    map_config$numeric_vars <- if (length(num_vars) == 1) list(num_vars) else num_vars
    map_config$temporal_vars <- if (length(temp_vars) == 1) list(temp_vars) else temp_vars
    map_config$id_vars <- if(length(id_vars) == 1) list(id_vars) else id_vars
    map_config$longitude_start <- lon_start
    map_config$latitude_start <- lat_start
    map_config$longitude_end <- lon_end
    map_config$latitude_end <- lat_end
    map_config$uniqueID <- "uniqueID"
    multi_grid <- list(list(
      'mapfile' = "spatdat.geojson",
      'area_variable_map' = avm,
      'area_variable_column' = avd
    ))
    map_config$multi_grid <- multi_grid
    
  } else {
    
    # Get rows that do not have NA in lon_start or lat_start
    dataset <- dataset[!is.na(dataset[[lon_start]]) & !is.na(dataset[[lat_start]]), ]
    
    # Get unique rows
    dataset <- unique(dataset[, c(avd, num_vars, temp_vars, id_vars, lon_start, lat_start)])
    dataset$uniqueID <- 1:nrow(dataset) # create a new unique id based on row number
    
    # Write data file
    write.csv(dataset, paste0(loc_map(project=project), "datafile.csv"))
    
    # Create map configuration list
    map_config <- list()
    # need to change this before release
    map_config$mapbox_token <- "pk.eyJ1IjoibWhhcnNjaDEyNSIsImEiOiJjbDI2b244ZmkwMHhjM2NvN3poNHZnajdkIn0.2yYSesDRvw4hSN5gQ1Ja-A"
    map_config$choosen_scatter <- num_vars[1]
    map_config$numeric_vars <- if (length(num_vars) == 1) list(num_vars) else num_vars
    map_config$temporal_vars <- if (length(temp_vars) == 1) list(temp_vars) else temp_vars
    map_config$id_vars <- if (length(id_vars) == 1 ) list(id_vars) else id_vars
    map_config$longitude_pt <- lon_start
    map_config$latitude_pt <- lat_start
    map_config$uniqueID <- "uniqueID"
    multi_grid <- list(list(
      'mapfile' = "spatdat.geojson",
      'area_variable_map' = avm,
      'area_variable_column' = avd
    ))
    map_config$multi_grid <- multi_grid
  }
  
  
  ##
  # Write JSON file and log function call ----
  ##
  # Write json file
  write(jsonlite::toJSON(map_config, pretty = TRUE, auto_unbox = TRUE), 
        paste0(loc_map(project=project), "/map_config.json"))
  # log function
  map_viewer_function <- list()
  map_viewer_function$functionID <- "map_viewer"
  map_viewer_function$args <- list(dat, project, spatname, avd, avm, num_vars, 
                                   temp_vars, id_vars, lon_start, lat_start, 
                                   lon_end, lat_end)
  log_call(project, map_viewer_function)
  
  
  ##
  # Open map viewer ----
  ##
  # working directory
  if (shiny::isRunning()) {
    map_url <- servr::httd(dir = loc_map(project), browser=FALSE)$url
    return(map_url)
    
  } else {
    utils::browseURL(servr::httd(dir =  loc_map(project), browser = FALSE)$url)
  }
}

