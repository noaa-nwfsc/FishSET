#' Generate haul level trip distance 
#' 
#' @param dataset dataframe or matrix
#' @param PortTable dataframe with columns Port_Name, Port_Long, Port_Lat
#' @param trip_id Vector defining unique trips 
#' @param starting_port Vector defining port at start of trip
#' @param starting_haul Vector defining Lat/Long of start of haul. Should contain two vectors.
#' @param ending_haul Vector defining Lat/Long of end of haul. Should contain two vectors.
#' @param ending_port Vector defining port at end of trip
#' @param haul_order Vector defining order that hauls occur
#' @param a numeric. Major (equatorial) radius of the ellipsoid. The default value is for WGS84
#' @param f numeric. Ellipsoid flattening. The default value is for WGS84
#' @importFrom geosphere distGeo
#' @export create_TD
#' @return vector of trip distance
#' @details Summation of distance across a trip based on choices by the user for starting and ending ports, and hauls in between.
#'  Uses longitude and latitude to calculate the distance between and returns as a new variable. Relies on the distGeo function to calculate distances
#'  between hauls. Inputs are a dataframe or matrix of trips, ports, etc and a datatable on ports including latitude and longitude of ports.
# @example MainDataTable$TripDistance <- create_TD(MainDataTable, PortTable, 'TRIP_SEQ', 'DISEMBARKED_PORT', c("LonLat_START_LON","LonLat_START_LAT"), c("LonLat_END_LON","LonLat_END_LAT"), 'EMBARKED_PORT', 'HAUL_SEQ')

# 
create_TD <- function(dataset, PortTable, trip_id, starting_port, starting_haul = c("Lon","Lat"), 
                      ending_haul = c("Lon", "Lat"), ending_port, haul_order, a = 6378137, f = 1/298.257223563) {

  if(any(unique(trimws(dataset[[ending_port]])) %in% unique(PortTable[,'Port_Name'])) == TRUE){
    ''
  } else {
    stop('Ending port and port name in PortTable do not match')
  }

  if(any(unique(trimws(dataset[[starting_port]])) %in% unique(PortTable[,'Port_Name'])) == TRUE){
    ''
  } else {
    stop('Starting port and port name in PortTable do not match')
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
  portLLS[, 1] <- sapply(trimws(dataset[[startPort]][portStartidx]), 
                         function(x) PortTable[which(PortTable$Port_Name == x), "Port_Long"])
  portLLS[, 2] <- sapply(trimws(dataset[[startPort]][portStartidx]), 
                         function(x) PortTable[which(PortTable$Port_Name == x), "Port_Lat"])
  portLLE[, 1] <- sapply(trimws(dataset[[endPort]][portEndidx]), 
                         function(x) PortTable[which(PortTable$Port_Name == x), "Port_Long"])
  portLLE[, 2] <- sapply(trimws(dataset[[endPort]][portEndidx]), 
                         function(x) PortTable[which(PortTable$Port_Name == x), "Port_Lat"])
  
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
  
  #write(layout.json.ed(trace, "create_TD",  deparse(substitute(dataset)), x = "", 
  #                     msg = paste("PortTable:",  deparse(substitute(PortTable)), ", trip_id:", trip_id, ", starting_port:", starting_port, ", starting_haul:",  deparse(substitute(starting_haul)), 
  #                                ", ending_haul:", deparse(substitute(ending_haul)), ", ending_port:", ending_port, ", haul_order:", haul_order, ", a:", a, ", f:", f, sep = "")), 
  #      paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""), append = T)
  
  create_TD_function <- list()
  create_TD_function$functionID <- 'create_TD'
  create_TD_function$args <- c(deparse(substitute(dataset)), PortTable, trip_id, starting_port, starting_haul, ending_haul, ending_port, haul_order)
  create_TD_function$kwargs <- list('a'=a, 'f'=f)
  functionBodyout$function_calls[[length(functionBodyout$function_calls)+1]] <- (create_TD_function)
  body$fishset_run <- list(infoBodyout, functionBodyout)
  write(jsonlite::toJSON(body, pretty = TRUE, auto_unbox = TRUE),paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""))
  list2env(functionBodyout, envir = .GlobalEnv)
  
  return(haulLevelTripDist)
}
