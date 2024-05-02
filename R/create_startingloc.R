#' Create starting location variable
#'
#' Creates a variable containing the zone/area location of a vessel when choice of 
#' where to fish next was made. This variable is required for data with multiple sets
#' or hauls in a single trip and for the full information model with 
#' Dahl's correction (\code{\link{logit_correction}}).
#'
#' @param dat  Primary data containing information on hauls or trips.
#'   Table in FishSET database contains the string 'MainDataTable'.
#' @param project Name of project
#' @param spat Spatial data. Required if \emph{ZoneID} does not exists in \code{dat}.
#'   Shape, json, geojson, and csv formats are supported.
#' @param port Port data. Contains columns: Port_Name, Port_Long, Port_Lat. 
#'   Table is generated using the \code{\link{load_port}} and saved in the FishSET 
#'   database as the project and port table, for example 'pollockPortTable'.
#' @param port_name Character string indicating the column in port table that contains the port name
#' @param port_lon Character string indication the column in port table that contains port longitude
#' @param port_lat Character string indication the column in port table that contains port latitude
#' @param trip_id Variable in \code{dat} that identifies unique trips.
#' @param haul_order Variable in \code{dat} containing information on the order 
#'   that hauls occur within a trip. Can be time, coded variable, etc.
#' @param starting_port Variable in \code{dat} to identify port at start of trip.
# @param lon.dat Longitude variable from \code{dat}. Required if \emph{ZoneID} 
#   does not exist in \code{dat}.
# @param lat.dat Latitude variable from \code{dat}. Required if \emph{ZoneID} 
#   does not exist in \code{dat}.
#' @param zoneID Variable in \code{dat} that identifies the individual zones or 
#'   areas.
#' @param spatID Variable in \code{spat} that identifies the individual zones or 
#'   areas.
#' @param name String, name of created variable. Defaults to name of the function 
#'   if not defined.
#' @importFrom DBI dbExecute
#' @export create_startingloc
#' @return Primary data set with starting location variable added.
#' @details Function creates the \code{startloc} vector that is required for the 
#'   full information model with Dahl's correction \code{\link{logit_correction}}. 
#'   The vector is the zone location of a vessel when the decision of where to fish 
#'   next was made. Generally, the first zone of a trip is the departure port. 
#'   The \code{\link{assignment_column}} function is called to assign starting port 
#'   locations and haul locations to zones. If ZoneID exists in \code{dat}, 
#'   \code{\link{assignment_column}} is not called and the following arguments are 
#'   not required: \code{spat, lon.dat, lat.dat, cat, lon.grid, lat.grid}. 
#' @examples
#' \dontrun{
#' pcodMainDataTable <- create_startingloc(pcodMainDataTable, 'pcod',
#'     map2, "pcodPortTable", "TRIP_SEQ", "HAUL_SEQ", "DISEMBARKED_PORT", 
#'  "START_LON", "START_LAT", "NMFS_AREA", "STARTING_LOC"
#' )
#' }
#
create_startingloc <- function(dat,
                               project = NULL,
                               spat,
                               port,
                               port_name,
                               port_lon,
                               port_lat,
                               trip_id,
                               haul_order,
                               starting_port,
                               zoneID,
                               spatID, 
                               name = "startingloc") {
    
  # TODO: consider removing assignment_column() functionality
  # TODO: Change this to required? project is required for nearly every other function
  # why not here?
  if (is.null(project)) {
    project <- sub("\\MainDataTable", "", dat)
  }
  
  # Call in data sets
  out <- data_pull(dat, project)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main", project)

  # Call in port table
  out <- data_pull(port, project)
  port.table <- out$dataset
  port <- parse_data_name(port, 'port', project)
  
  # Call in spatial data
  spat_out <- data_pull(spat, project)
  spatdat <- spat_out$dataset
  spat <- parse_data_name(spat, "spat", project)
  
  # Make sure columns are present in primary data table
  column_check(dataset, cols = c(trip_id, haul_order, starting_port, zoneID))
  
  # Make sure columns are present in spat table
  column_check(spatdat, cols = c(spatID))
  
  # Make sure new column name is unique and doesn't already exist
  name <- name_check(dataset, name, repair = TRUE)
  
  port.table <- assignment_column(
    dat = port.table, project = project, spat = spatdat, hull.polygon = FALSE, 
    lon.dat = port_lon, lat.dat = port_lat, cat = spatID, closest.pt = TRUE, 
    log.fun = FALSE, bufferval = 100 # need to consider how this affects things
  )

  # Create starting loc variable
  # TODO: trip_id is currently required; can't be NULL. Resolve.
  if (is.null(trip_id)) {
    dataset <- dataset[order(dataset[[haul_order]]), ]
    
  } else {
    dataset <- dataset[order(dataset[[trip_id]], dataset[[haul_order]]), ]
  }
  
  newvar <- rep(NA, nrow(dataset))
  newvar[2:nrow(dataset)] <- dataset[[zoneID]][1:(nrow(dataset) - 1)]

  # Make starting of trips set to zone of starting port
  if (!is.null(trip_id)) {
    rownumbers <- match(
      trimws(dataset[tapply(seq_along(dataset[[trip_id]]), dataset[[trip_id]], min), starting_port][[1]]), 
      port.table[port_name])
    
    if (any(is.na(rownumbers))) { 
      warning('NAs produced. At least one disembarked port was not found in the port table.')
    }
    
    newvar[tapply(seq_along(dataset[[trip_id]]), dataset[[trip_id]], min)] <- port.table[rownumbers, 'ZoneID']
    
  } else {
    rownumbers <- match(trimws(dataset[1, starting_port]), port.table$Port_Name)
    newvar[1] <- port.table[rownumbers, zoneID]
  }
  
  g <- cbind(dataset, newvar)
  colnames(g)[dim(g)[2]] <- name

  create_startingloc_function <- list()
  create_startingloc_function$functionID <- "create_startingloc"
  create_startingloc_function$args <- list(dat, project, spat, port, trip_id, 
                                           haul_order, starting_port, zoneID, 
                                           spatID, name)

  log_call(project, create_startingloc_function)

  return(g)
}
