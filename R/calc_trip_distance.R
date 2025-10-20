#' Calculate haul level trip distance variable
#'
#' @param name String. Name of new variable for trip distance. Defaults to `TripDistance`.
#' @param project String. project name. 
#' @param dat String or data frame. A string for the name of the main data table in the FishSET 
#'   database (contains 'MainDataTable' in the name). Or a data frame of the main data table.
#' @param port String or data frame. A string for the name of the port table in the FishSET 
#'   project database. Or a data frame of the port table. Note that the port table must at least
#'   include a port name, port latitude and port longitude.
#' @param trip_id String. Column name that represents the unique trip identifier in \code{dat}.
#' @param haul_order String. Column name in \code{dat} that identifies haul order within a trip. 
#'   Can be time, coded variable, etc.
#' @param starting_port String. Column name in \code{dat} containing departure port for each trip.
#' @param return_port String. Column name in \code{dat} containing landing port for each trip.
#' @param start_haul_lat String. Column name in \code{dat} containing haul starting latitude.
#' @param start_haul_lon String. Column name in \code{dat} containing haul starting longitude.
#' @param end_haul_lat String. Column name in \code{dat} containing haul end latitude.
#' @param end_haul_lon String. Column name in \code{dat} containing haul end longitude.
#' @param a Numeric. Major (equatorial) radius of the ellipsoid. The default value is for 
#'   WGS84 ellipsoid.
#' @param f Numeric. Ellipsoid flattening. The default value is for WGS84 ellipsoid.
#' 
#' @importFrom geosphere distGeo
#' 
#' @export calc_trip_distance
#' 
#' @return Returns the main data table with a new variable for trip distance.
#' 
#' @details Summation of distance across a trip based on starting and ending ports and hauls in 
#'   between. The function uses \code{\link[geosphere]{distGeo}} from the geosphere package to 
#'   calculate distances between hauls. Inputs are the trips, ports, and hauls from the main data
#'   table, and the latitude and longitude of ports from the port table. The ellipsoid arguments, 
#'   \code{a} and \code{f}, are numeric and can be changed if an ellipsoid other than WGS84 is 
#'   appropriate. See the geosphere R package for more details
#'   (\url{https://cran.r-project.org/web/packages/geosphere/geosphere.pdf}).
#'   
#' @examples
#' \dontrun{
#' pcodMainDataTable <- calc_trip_distance(pcodMainDataTable, "pcod", "pcodPortTable", 
#'   "TRIP_SEQ", "DISEMBARKED_PORT", c("LonLat_START_LON", "LonLat_START_LAT"),
#'   c("LonLat_END_LON", "LonLat_END_LAT"), "EMBARKED_PORT", "HAUL_SEQ", "TripDistance"
#' )
#' }
#'
calc_trip_distance <- function(name = "TripDistance", 
                               project,
                               dat, 
                               port, 
                               trip_id, 
                               haul_order,
                               starting_port,
                               return_port, 
                               start_haul_lat,
                               start_haul_lon,
                               end_haul_lat,
                               end_haul_lon,
                               a = 6378137, 
                               f = 1/298.257223563) {
  
  # TESTING ######################################
  # name = "TripDistance" 
  # project = "gfbt_erk"
  # dat = "gfbt_erkMainDataTable"
  # port = "gfbt_erkPortTable"
  # trip_id = "trip_id"
  # haul_order = "haul_counter"
  # starting_port = "depart_port"
  # return_port = "return_port"
  # start_haul_lat = "previous_lat"
  # start_haul_lon = "previous_lon"
  # end_haul_lat = "centro_lat"
  # end_haul_lon = "centro_lon"
  # a = 6378137 
  # f = 1/298.257223563
  ################################################
  
  # Pull in datasets ------------------------------------------------------------------------------
  out <- data_pull(dat, project)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main", project)
  
  out <- data_pull(port, project)
  port_table <- out$dataset
  port <- parse_data_name(port, 'port', project)
  
  # Check inputs ----------------------------------------------------------------------------------
  if(is_empty(name)){ 
    name <- "TripDistance" 
  }
  
  port_table_names <- unique(port_table[["Port_Name"]])
  
  if (!any(unique(trimws(dataset[[return_port]])) %in% port_table_names)) {
    stop("One or more port name(s) included in return_port were not found in the port table.")
  }
  
  if (!any(unique(trimws(dataset[[starting_port]])) %in% port_table_names)) {
    stop("One or more port name(s) included in starting_port were not found in the port table.")
  }
  
  if (any(abs(dataset[, c(start_haul_lat, end_haul_lat)]) > 90)) {
    stop("Latitude is not valid (outside -90:90. Function not run.")
  }
  
  if (any(abs(dataset[, c(start_haul_lon, end_haul_lon)]) > 180)) {
    stop("Longitude is not valid (outside -180:180). Function not run.")
  }
  
  # Calculate trip distance -----------------------------------------------------------------------
  trips <- unique(dataset[[trip_id]])
  trips_ind <- match(dataset[[trip_id]], trips) # trips_ind = row ID of those unique items
  
  haulLocalStart <- starting_haul
  haulLocalEnd <- ending_haul
  
  port_start_ind <- rep(NA, length(trips))
  port_end_ind <- port_start_ind
  inner_haul_ind <- list() # cell(length(trips),1)
  simple_ind <- list() # cell(length(trips),1)
  
  for (i in 1:length(trips)) {
    trip_i <- which(trips_ind == i) # find(trips_ind==i)
    val <- sort(dataset[[haul_order]][trip_i]) # (seq).dataColumn(trip_i),'ascend') #Use haul_order
    id <- match(dataset[[haul_order]][trip_i], val)
    port_start_ind[i] <- trip_i[id[1]] # get index of 1st haul to be used when calculating port to 1st haul
    port_end_ind[i] <- trip_i[id[length(id)]] # get index of last  haul to be used when calculatinglast haul to port
    inner_haul_ind[[i]] <- trip_i[id] # indexes of all values associated to a given trip in sorted order
    simple_ind[[i]] <- id # internal sorted index
  }
  
  portLLS <- data.frame(matrix(NA, nrow = length(dataset[[starting_port]][port_start_ind]), ncol = 2))
  portLLE <- data.frame(matrix(NA, nrow = length(dataset[[return_port]][port_end_ind]), ncol = 2)) #
  temp <- sapply(trimws(dataset[[starting_port]][port_start_ind]), function(x) port_table[which(port_table[["Port_Name"]] == x), "Port_Long"])
  # a[lengths(a) == 0] <- NA_character_
  portLLS[, 1] <- as.numeric(temp) # unlist(a)
  portLLS[, 2] <- as.numeric(sapply(trimws(dataset[[starting_port]][port_start_ind]), function(x) port_table[which(port_table[["Port_Name"]] == x), "Port_Lat"]))
  portLLE[, 1] <- as.numeric(sapply(trimws(dataset[[return_port]][port_end_ind]), function(x) port_table[which(port_table[["Port_Name"]] == x), "Port_Long"]))
  portLLE[, 2] <- as.numeric(sapply(trimws(dataset[[return_port]][port_end_ind]), function(x) port_table[which(port_table[["Port_Name"]] == x), "Port_Lat"]))
  
  portToStart <- geosphere::distGeo(cbind(portLLS[, 1], portLLS[, 2]), cbind(dataset[[haulLocalStart[1]]][port_start_ind], dataset[[haulLocalStart[2]]][port_start_ind]),
                                    a = a, f = f
  )
  portToEnd <- geosphere::distGeo(cbind(portLLE[, 1], portLLE[, 2]), cbind(dataset[[haulLocalEnd[1]]][port_end_ind], dataset[[haulLocalEnd[2]]][port_end_ind]),
                                  a = a, f = f
  )
  
  if (anyNA(match(haulLocalStart, haulLocalEnd))) {
    innerHaulDist <- geosphere::distGeo(cbind(dataset[[haulLocalStart[1]]], dataset[[haulLocalStart[2]]]), cbind(dataset[[haulLocalEnd[1]]], dataset[[haulLocalEnd[2]]]),
                                        a = a, f = f
    )
  }
  # now end of haul to start of next haul..
  
  haulidxall <- matrix(inner_haul_ind, ncol = 1)
  haulidxall <- do.call(rbind, apply(haulidxall, 1, function(x) do.call(cbind, x)))
  
  ## ---> Will need to revise based on how lat/long are indexed <----##
  endToStart <- geosphere::distGeo(cbind(dataset[[haulLocalEnd[1]]][haulidxall[1:nrow(haulidxall) - 1]], dataset[[haulLocalEnd[2]]][haulidxall[1:nrow(haulidxall) -
                                                                                                                                                 1]]), cbind(dataset[[haulLocalStart[1]]][haulidxall[-1]], dataset[[haulLocalStart[2]]][haulidxall[-1]]))
  
  endToStart <- c(NA, endToStart)
  # need to unsort it
  backSortId <- match(haulidxall, sort(haulidxall))
  haulEndToStart <- endToStart[backSortId]
  # replace start haul with distance from port haulEndToStart(port_start_ind)=nan
  haulEndToStart[port_start_ind] <- portToStart # haulEndToStart would be haul level distance variable...
  
  # now try to sum all dist for one trip
  
  sumToHaul <- accumarray(trips_ind, haulEndToStart) # summs the port to 1st haul + end  first haul to start of next haul
  if (any(haulLocalStart %in% haulLocalEnd)) {
    tripDist <- rowSums(cbind(sumToHaul, portToEnd), na.rm = T) # need to make a haul level tripdist variable
  } else {
    sumInnerDist <- accumarray(trips_ind, innerHaulDist)
    tripDist <- rowSums(cbind(sumToHaul, sumInnerDist, portToEnd), na.rm = T)
  }
  
  dataset[[name]] <- tripDist[trips_ind]
  
  create_TD_function <- list()
  create_TD_function$functionID <- "calc_trip_distance"
  create_TD_function$args <- list(dat, project, port, trip_id, starting_port, starting_haul, 
                                  ending_haul, return_port, haul_order, name, a, f)
  create_TD_function$output <- list(dat)
  log_call(project, create_TD_function)
  
  return(dataset)
  
}
