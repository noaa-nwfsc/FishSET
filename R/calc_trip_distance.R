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
  
  # Error flag
  flag <- 0
  
  if (!any(unique(trimws(dataset[[return_port]])) %in% unique(port_table[["Port_Name"]]))) {
    warning("return_port from the data set and port_name from the port table do not match. Function not run.")
    flag <- 1
  }
  
  if (!any(unique(trimws(dataset[[starting_port]])) %in% unique(port_table[, "Port_Name"])) == TRUE) {
    warning("starting_port from the data set and port_name from the port table do not match. Funciton not run.")
    flag <- 1
  }
  
  if (any(abs(dataset[, c(starting_haul)][1]) > 180) | any(abs(dataset[, c(ending_haul)][1]) > 180)) {
    warning("Longitude is not valid (outside -180:180). Function not run.")
    flag <- 1
  }
  if (any(abs(dataset[, c(starting_haul)][2]) > 90) | any(abs(dataset[, c(ending_haul)][2]) > 90)) {
    warning("Latitude is not valid (outside -90:90. Function not run.")
    flag <- 1
  }
  
  if (flag == 0) {
    tripsFound <- unique(dataset[[trip_id]])
    C <- match(dataset[[trip_id]], tripsFound) # C = row ID of those unique items
    
    startPort <- starting_port
    haulLocalStart <- starting_haul
    haulLocalEnd <- ending_haul
    endPort <- return_port
    
    
    portStartidx <- rep(NA, length(tripsFound))
    portEndidx <- portStartidx
    innerHaulidx <- list() # cell(length(tripsFound),1)
    simpleidx <- list() # cell(length(tripsFound),1)
    
    for (i in 1:length(tripsFound)) {
      # get all the indexing
      tripInUse <- which(C == i) # find(C==i)
      val <- sort(dataset[[haul_order]][tripInUse]) # (seq).dataColumn(tripInUse),'ascend') #Use haul_order
      id <- match(dataset[[haul_order]][tripInUse], val)
      portStartidx[i] <- tripInUse[id[1]] # get index of 1st haul to be used when calculating port to 1st haul
      portEndidx[i] <- tripInUse[id[length(id)]] # get index of last  haul to be used when calculatinglast haul to port
      innerHaulidx[[i]] <- tripInUse[id] # indexes of all values associated to a given trip in sorted order
      simpleidx[[i]] <- id # internal sorted index
    }
    
    portLLS <- data.frame(matrix(NA, nrow = length(dataset[[startPort]][portStartidx]), ncol = 2))
    portLLE <- data.frame(matrix(NA, nrow = length(dataset[[endPort]][portEndidx]), ncol = 2)) #
    temp <- sapply(trimws(dataset[[startPort]][portStartidx]), function(x) port_table[which(port_table[["Port_Name"]] == x), "Port_Long"])
    # a[lengths(a) == 0] <- NA_character_
    portLLS[, 1] <- as.numeric(temp) # unlist(a)
    portLLS[, 2] <- as.numeric(sapply(trimws(dataset[[startPort]][portStartidx]), function(x) port_table[which(port_table[["Port_Name"]] == x), "Port_Lat"]))
    portLLE[, 1] <- as.numeric(sapply(trimws(dataset[[endPort]][portEndidx]), function(x) port_table[which(port_table[["Port_Name"]] == x), "Port_Long"]))
    portLLE[, 2] <- as.numeric(sapply(trimws(dataset[[endPort]][portEndidx]), function(x) port_table[which(port_table[["Port_Name"]] == x), "Port_Lat"]))
    
    portToStart <- geosphere::distGeo(cbind(portLLS[, 1], portLLS[, 2]), cbind(dataset[[haulLocalStart[1]]][portStartidx], dataset[[haulLocalStart[2]]][portStartidx]),
                                      a = a, f = f
    )
    portToEnd <- geosphere::distGeo(cbind(portLLE[, 1], portLLE[, 2]), cbind(dataset[[haulLocalEnd[1]]][portEndidx], dataset[[haulLocalEnd[2]]][portEndidx]),
                                    a = a, f = f
    )
    
    if (anyNA(match(haulLocalStart, haulLocalEnd))) {
      innerHaulDist <- geosphere::distGeo(cbind(dataset[[haulLocalStart[1]]], dataset[[haulLocalStart[2]]]), cbind(dataset[[haulLocalEnd[1]]], dataset[[haulLocalEnd[2]]]),
                                          a = a, f = f
      )
    }
    # now end of haul to start of next haul..
    
    haulidxall <- matrix(innerHaulidx, ncol = 1)
    haulidxall <- do.call(rbind, apply(haulidxall, 1, function(x) do.call(cbind, x)))
    
    ## ---> Will need to revise based on how lat/long are indexed <----##
    endToStart <- geosphere::distGeo(cbind(dataset[[haulLocalEnd[1]]][haulidxall[1:nrow(haulidxall) - 1]], dataset[[haulLocalEnd[2]]][haulidxall[1:nrow(haulidxall) -
                                                                                                                                                   1]]), cbind(dataset[[haulLocalStart[1]]][haulidxall[-1]], dataset[[haulLocalStart[2]]][haulidxall[-1]]))
    
    endToStart <- c(NA, endToStart)
    # need to unsort it
    backSortId <- match(haulidxall, sort(haulidxall))
    haulEndToStart <- endToStart[backSortId]
    # replace start haul with distance from port haulEndToStart(portStartidx)=nan
    haulEndToStart[portStartidx] <- portToStart # haulEndToStart would be haul level distance variable...
    
    # now try to sum all dist for one trip
    
    sumToHaul <- accumarray(C, haulEndToStart) # summs the port to 1st haul + end  first haul to start of next haul
    if (any(haulLocalStart %in% haulLocalEnd)) {
      tripDist <- rowSums(cbind(sumToHaul, portToEnd), na.rm = T) # need to make a haul level tripdist variable
    } else {
      sumInnerDist <- accumarray(C, innerHaulDist)
      tripDist <- rowSums(cbind(sumToHaul, sumInnerDist, portToEnd), na.rm = T)
    }
    
    dataset[[name]] <- tripDist[C]
    
    create_TD_function <- list()
    create_TD_function$functionID <- "calc_trip_distance"
    create_TD_function$args <- list(dat, project, port, trip_id, starting_port, starting_haul, 
                                    ending_haul, return_port, haul_order, name, a, f)
    create_TD_function$output <- list(dat)
    log_call(project, create_TD_function)
    
    return(dataset)
  }
}
