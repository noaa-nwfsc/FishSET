#' Create haul level trip distance variable
#'
#' @param dat Primary data containing information on hauls or trips.
#' Table in the FishSET database contains the string 'MainDataTable'.
#' @param PortTable Port data frame. Contains columns: Port_Name, Port_Long, Port_Lat.
#' Table is generated using the \code{\link{load_port}} function and saved in the FishSET database as the project and PortTable,
#' for example 'pollockPortTable'.
#' @param trip_id Unique trip identifier in \code{dat}.
#' @param starting_port Variable in \code{dat} containing ports at the start of the trip.
#' @param starting_haul Character string, variables containing latitude and longitude at start of haul in \code{dat}.
#' @param ending_haul Character string, variables containing latitude and longitude at end of haul in \code{dat}.
#' @param ending_port Variable in \code{dat} containing ports at the end of the trip.
#' @param haul_order Variable in \code{dat} that identifies haul order within a trip. Can be time, coded variable, etc.
#' @param name String, name of created variable. Defaults to `TripDistance`.
#' @param a  Numeric, major (equatorial) radius of the ellipsoid. The default value is for WGS84 ellipsoid.
#' @param f  Numeric, ellipsoid flattening. The default value is for WGS84 ellipsoid.
#' @importFrom geosphere distGeo
#' @export create_trip_distance
#' @return Returns the primary dataset with a trip distance variable added.
#' @details Summation of distance across a trip based on starting and ending ports and hauls in between.
#' The function uses \code{\link[geosphere]{distGeo}} from the geosphere package to calculate distances
#'  between hauls. Inputs are the trips, ports, and hauls from the primary dataset, and the latitude and
#'  longitude of ports from the \code{PortTable}. The ellipsoid arguments, \code{a} and \code{f}, are numeric
#'  and can be changed if an ellipsoid other than WGS84 is appropriate. See the geosphere R package for more details
#'  (\url{https://cran.r-project.org/web/packages/geosphere/geosphere.pdf}).
#' @examples
#' \dontrun{
#' pcodMainDataTable <- create_trip_distance(pcodMainDataTable, "pcodPortTable", 
#'   "TRIP_SEQ", "DISEMBARKED_PORT", c("LonLat_START_LON", "LonLat_START_LAT"),
#'   c("LonLat_END_LON", "LonLat_END_LAT"), "EMBARKED_PORT", "HAUL_SEQ", "TripDistance"
#' )
#' }
#'
#' #
create_trip_distance <- function(dat, PortTable, trip_id, starting_port, starting_haul = c("Lon", "Lat"), ending_haul = c("Lon", "Lat"),
                                 ending_port, haul_order, name = "TripDistance", a = 6378137, f = 1 / 298.257223563) {

  # Call in datasets
  out <- data_pull(dat)
  dataset <- out$dataset

  out <- data_pull(PortTable)
  port.table <- out$dataset
  
  if (shiny::isRunning()) 
    if (deparse(substitute(dat)) == "values$dataset") dat <- get("dat_name")
  if (deparse(substitute(PortTable)) == "ptdat$dataset") PortTable <- get("port_name")
  
  else 
    if (!is.character(dat)) dat <- deparse(substitute(dat))
    if (!is.character(PortTable)) PortTable <- deparse(substitute(PortTable))

  x <- 0

  if (!any(unique(trimws(dataset[[ending_port]])) %in% unique(port.table[, "Port_Name"]))) {
    warning("Ending_port from the data set and port_name from the port table do not match. Function not run.")
    x <- 1
  }

  if (!any(unique(trimws(dataset[[starting_port]])) %in% unique(port.table[, "Port_Name"])) == TRUE) {
    warning("starting_port from the data set and port_name from the port table do not match. Funciton not run.")
    x <- 1
  }

  if (any(abs(dataset[, c(starting_haul)][1]) > 180) | any(abs(dataset[, c(ending_haul)][1]) > 180)) {
    warning("Longitude is not valid (outside -180:180). Function not run.")
    x <- 1
  }
  if (any(abs(dataset[, c(starting_haul)][2]) > 90) | any(abs(dataset[, c(ending_haul)][2]) > 90)) {
    warning("Latitude is not valid (outside -90:90. Function not run.")
    x <- 1
  }

  if (x == 0) {
    tripsFound <- unique(dataset[[trip_id]])
    C <- match(dataset[[trip_id]], tripsFound) # C = row ID of those unique items

    startPort <- starting_port
    haulLocalStart <- starting_haul
    haulLocalEnd <- ending_haul
    endPort <- ending_port


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
    temp <- sapply(trimws(dataset[[startPort]][portStartidx]), function(x) port.table[which(port.table[["Port_Name"]] == x), "Port_Long"])
    # a[lengths(a) == 0] <- NA_character_
    portLLS[, 1] <- as.numeric(temp) # unlist(a)
    portLLS[, 2] <- as.numeric(sapply(trimws(dataset[[startPort]][portStartidx]), function(x) port.table[which(port.table[["Port_Name"]] == x), "Port_Lat"]))
    portLLE[, 1] <- as.numeric(sapply(trimws(dataset[[endPort]][portEndidx]), function(x) port.table[which(port.table[["Port_Name"]] == x), "Port_Long"]))
    portLLE[, 2] <- as.numeric(sapply(trimws(dataset[[endPort]][portEndidx]), function(x) port.table[which(port.table[["Port_Name"]] == x), "Port_Lat"]))

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
    create_TD_function$functionID <- "create_trip_distance"
    create_TD_function$args <- list(dat, PortTable, trip_id, starting_port, starting_haul, 
                                    ending_haul, ending_port, haul_order, name, a, f)
    create_TD_function$output <- list(dat)
    log_call(create_TD_function)

    return(dataset)
  }
}
