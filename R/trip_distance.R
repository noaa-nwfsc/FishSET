#' Generate haul level trip distance 
#'
#' @param dat Main data frame. In fishset_db database, table name contains phrase `MainDataTable`.
#' @param PortTable Port data frame. Contains columns: Port_Name, Port_Long, Port_Lat. Table is generated using the load_port function and saved in the fishset_db database.
#' @param trip_id Variable in 'dat' to identify unique trips. 
#' @param starting_port Variable in `dat` to identify port at start of trip
#' @param starting_haul lat/long Variables in `dat` containing lat/long at start of haul. Should contain two vectors.
#' @param ending_haul lat/long Variables in `dat` containing lat/long at end of haul. Should contain two vectors.
#' @param ending_port Variable in `dat` to identify port at end of trip
#' @param haul_order Variable in `dat` containing information on the order that hauls occur within a trip. Can be time, coded variable, etc.
#' @param a  Major (equatorial) radius of the ellipsoid. The default value is for WGS84 ellipsoid.
#' @param f  Ellipsoid flattening. The default value is for WGS84 ellipsoid.
#' @importFrom geosphere distGeo
#' @export create_trip_distance
#' @return Vector of trip distance
#' @details Summation of distance across a trip based on choices by the user for starting and ending ports, and hauls in between.
#'  Uses longitude and latitude to calculate the distance between hauls within a trip and returns as a new variable. 
#'  Utilizes the \code{\link[geosphere]{distGeo}} function from the geosphere package to calculate distances between hauls.
#'   Inputs are the main data table containing information on trips, ports, etc., and a port data table containing latitude and longitude of ports.
#'  The ellipsoid parameters `a` and `f` are numeric and can be changed if an ellipsoid other than WGS84 is appropriate. See the geosphere R package for more details 
#'  \url{https://cran.r-project.org/web/packages/geosphere/geosphere.pdf}.
#' @examples
#' \dontrun{
#'  MainDataTable$TripDistance <- create_trip_distance(MainDataTable, 'pollockPortTable', 'TRIP_SEQ', 
#'                                'DISEMBARKED_PORT', c("LonLat_START_LON","LonLat_START_LAT"),
#'                                c("LonLat_END_LON","LonLat_END_LAT"), 'EMBARKED_PORT', 'HAUL_SEQ')
#'  }

# 
create_trip_distance <- function(dat, PortTable, trip_id, starting_port, starting_haul = c("Lon","Lat"), 
                      ending_haul = c("Lon", "Lat"), ending_port, haul_order, a = 6378137, f = 1/298.257223563) {

  #Call in datasets
  out <- data_pull(dat)
  dat <- out$dat
  dataset <- out$dataset
  
  out <- data_pull(PortTable)
  PortTable <- out$dat
  port.table <- out$dataset
  
  if(!any(unique(trimws(dataset[[ending_port]])) %in% unique(port.table[,'Port_Name']))){
    stop('Ending_port from the data set and port_name from the port table do not match.')
  }

  if(!any(unique(trimws(dataset[[starting_port]])) %in% unique(port.table[,'Port_Name'])) == TRUE){
    stop('starting_port from the data set and port_name from the port table do not match')
  }

  tripsFound <- unique(dataset[[trip_id]])
  C <- match(dataset[[trip_id]], tripsFound)  #C = row ID of those unique items
  
  startPort = starting_port
  haulLocalStart = starting_haul
  haulLocalEnd = ending_haul
  endPort = ending_port
  
  
  portStartidx = rep(NA, length(tripsFound))
  portEndidx = portStartidx
  innerHaulidx = list()  #cell(length(tripsFound),1)
  simpleidx = list()  #cell(length(tripsFound),1)
  
  for (i in 1:length(tripsFound)) {
    # get all the indexing
    tripInUse <- which(C == i)  #find(C==i)
    val <- sort(dataset[[haul_order]][tripInUse])  #(seq).dataColumn(tripInUse),'ascend') #Use haul_order
    id <- match(dataset[[haul_order]][tripInUse], val)
    portStartidx[i] <- tripInUse[id[1]]  # get index of 1st haul to be used when calculating port to 1st haul
    portEndidx[i] <- tripInUse[id[length(id)]]  # get index of last  haul to be used when calculatinglast haul to port
    innerHaulidx[[i]] <- tripInUse[id]  # indexes of all values associated to a given trip in sorted order
    simpleidx[[i]] <- id  # internal sorted index
  }
  
  portLLS <- data.frame(matrix(NA, nrow = length(dataset[[startPort]][portStartidx]),  ncol = 2)) 
  portLLE <- data.frame(matrix(NA, nrow = length(dataset[[endPort]][portEndidx]), ncol = 2))  #
                  temp <- sapply(trimws(dataset[[startPort]][portStartidx]), 
                         function(x) port.table[which(port.table[['Port_Name']] == x), "Port_Long"])
                  #a[lengths(a) == 0] <- NA_character_
  portLLS[, 1] <- as.numeric(temp) #unlist(a)
  portLLS[, 2] <- as.numeric(sapply(trimws(dataset[[startPort]][portStartidx]), 
                         function(x) port.table[which(port.table[['Port_Name']] == x), "Port_Lat"]))
  portLLE[, 1] <- as.numeric(sapply(trimws(dataset[[endPort]][portEndidx]), 
                         function(x) port.table[which(port.table[['Port_Name']] == x), "Port_Long"]))
  portLLE[, 2] <- as.numeric(sapply(trimws(dataset[[endPort]][portEndidx]), 
                         function(x) port.table[which(port.table[['Port_Name']] == x), "Port_Lat"]))
  
  portToStart <- geosphere::distGeo(cbind(portLLS[, 1], portLLS[, 2]), 
                                    cbind(dataset[[haulLocalStart[1]]][portStartidx], dataset[[haulLocalStart[2]]][portStartidx]), a = a, f = f)
  portToEnd <- geosphere::distGeo(cbind(portLLE[, 1], portLLE[, 2]), 
                                  cbind(dataset[[haulLocalEnd[1]]][portEndidx], dataset[[haulLocalEnd[2]]][portEndidx]), a = a, f = f)
  
  if (any(is.na(match(haulLocalStart, haulLocalEnd))==TRUE)) {
    innerHaulDist <- geosphere::distGeo(cbind(dataset[[haulLocalStart[1]]], dataset[[haulLocalStart[2]]]), 
                                        cbind(dataset[[haulLocalEnd[1]]], dataset[[haulLocalEnd[2]]]), a = a, f = f)
  }
  # now end of haul to start of next haul..
  
  haulidxall <- matrix(innerHaulidx, ncol = 1)
  haulidxall <- do.call(rbind, apply(haulidxall, 1, function(x) do.call(cbind, x)))
  
  ## ---> Will need to revise based on how lat/long are indexed <----##
  endToStart <- geosphere::distGeo(cbind(dataset[[haulLocalEnd[1]]][haulidxall[1:nrow(haulidxall) - 1]],
                                         dataset[[haulLocalEnd[2]]][haulidxall[1:nrow(haulidxall) - 1]]), 
                                   cbind(dataset[[haulLocalStart[1]]][haulidxall[-1]], dataset[[haulLocalStart[2]]][haulidxall[-1]]))
  
  endToStart <- c(NA, endToStart)
  # need to unsort it
  backSortId <- match(haulidxall, sort(haulidxall))
  haulEndToStart <- endToStart[backSortId]
  # replace start haul with distance from port haulEndToStart(portStartidx)=nan
  haulEndToStart[portStartidx] <- portToStart  # haulEndToStart would be haul level distance variable...
  
  # now try to sum all dist for one trip
  
  sumToHaul <- accumarray(C, haulEndToStart)  #summs the port to 1st haul + end  first haul to start of next haul
  if (any(haulLocalStart %in% haulLocalEnd)) {
    tripDist <- rowSums(cbind(sumToHaul, portToEnd), na.rm = T) # need to make a haul level tripdist variable
  } else {
   sumInnerDist <- accumarray(C, innerHaulDist)
    tripDist <- rowSums(cbind(sumToHaul, sumInnerDist, portToEnd), na.rm = T)  
    
  }
  haulLevelTripDist = tripDist[C]

  create_TD_function <- list()
  create_TD_function$functionID <- 'create_TD'
  create_TD_function$args <- c(dat, PortTable, trip_id, starting_port, starting_haul, ending_haul, ending_port, haul_order)
  create_TD_function$kwargs <- list('a'=a, 'f'=f)
  create_TD_function$output <- c('')
  log_call(create_TD_function)
 
  return(haulLevelTripDist)
}
