#' Create starting location variable
#'
#' Creates a variable containing the zone/area location of a vessel when choice of where to fish next was made. This variable is required for the full information model with Dahl's correction (\code{\link{logit_correction}}).
#'
#' @param dat  Primary data containing information on hauls or trips.
#'   Table in FishSET database contains the string 'MainDataTable'.
#' @param project Name of project
#' @param spat Spatial data. Required if \emph{ZoneID} does not exists in \code{dat}.
#'   Shape, json, geojson, and csv formats are supported.
#' @param portTable Port data. Contains columns: Port_Name, Port_Long, Port_Lat. Table is generated using
#'   the \code{\link{load_port}} and saved in the FishSET database as the project and PortTable, for example 'pollockPortTable'.
#' @param trip_id Variable in \code{dat} that identifies unique trips.
#' @param haul_order Variable in \code{dat} containing information on the order that hauls occur within a trip. Can be time, coded variable, etc.
#' @param starting_port Variable in \code{dat} to identify port at start of trip.
#' @param lon.dat Longitude variable from \code{dat}. Required if \emph{ZoneID} does not exist in \code{dat}.
#' @param lat.dat Latitude variable from \code{dat}. Required if \emph{ZoneID} does not exist in \code{dat}.
#' @param cat Variable or list in \code{spat} that identifies the individual areas or zones. 
#'   Required if \emph{ZoneID} does not exist in \code{dat}.
#'   If \code{spat} is class sf, \code{cat} should be name of list containing information on zones.
#' @param zoneid Variable in \code{dat} that identifies the individual zones or areas. Defaults to NULL. Define if 
#'    the name of the zone identifier variable is not `ZoneID`.
#' @param name String, name of created variable. Defaults to name of the function if not defined.
#' @param lon.spat Variable or list from \code{spat} containing longitude data. 
#'   Required if \emph{ZoneID} does not exist in \code{dat}. Required for csv files. 
#'   Leave as NULL if \code{spat} is a shape or json file.
#' @param lat.spat Variable or list from \code{spat} containing latitude data. 
#'   Required if \emph{ZoneID} does not exist in \code{dat}. Required for csv files. 
#'   Leave as NULL if \code{spat} is a shape or json file.
#' @importFrom DBI dbExecute
#' @export create_startingloc
#' @return Primary data set with starting location variable added.
#' @details Function creates the \code{startloc} vector that is required for the full information model with Dahl's correction \code{\link{logit_correction}}. 
#'   The vector is the zone location of a vessel when the decision of where to fish next was made. Generally, the first zone of a trip is the departure port. 
#'   The \code{\link{assignment_column}} function is called to assign starting port locations and haul locations to zones. 
#'   If ZoneID exists in \code{dat}, \code{\link{assignment_column}} is not called and the following arguments are not required:
#'   \code{spat, lon.dat, lat.dat, cat, lon.grid, lat.grid}. 
#' @examples
#' \dontrun{
#' pcodMainDataTable <- create_startingloc(pcodMainDataTable, 'pcod',
#'     map2, "pcodPortTable", "TRIP_SEQ", "HAUL_SEQ", "DISEMBARKED_PORT", 
#'  "START_LON", "START_LAT", "NMFS_AREA", "STARTING_LOC"
#' )
#' }
#
create_startingloc <- function(dat,project=NULL, spat, portTable, trip_id, haul_order, starting_port, lon.dat, lat.dat,
                               cat, zoneid, name = "startingloc", lon.spat = NULL, lat.spat = NULL) {

  if(is.null(project)){
    project <- sub("\\MainDataTable", "", dat)
  }
  
    # Call in data sets
  out <- data_pull(dat, project)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main", project)
  

  # in port table
  out <- data_pull(portTable, project)
  PortTable <- parse_data_name(portTable, 'port', project)
  port.table <- out$dataset
  
  
  # Call in datasets
  spat_out <- data_pull(spat, project)
  spatdat <- spat_out$dataset
  spat <- parse_data_name(dat, "spat", project)
  
  
 name <- ifelse(is_empty(name), "startingloc", name)

  
  port <- assignment_column(
    dat = port.table, project = project, spat = spatdat, hull.polygon = FALSE, lon.spat = lon.spat, lat.spat = lat.spat, lon.dat = "Port_Long",
    lat.dat = "Port_Lat", cat = cat, closest.pt = TRUE, log.fun = FALSE
  )

  if("ZoneID" %in% names(dataset) == TRUE){
    int.data <- dataset
    zoneid <- 'ZoneID'
  } else if(!is.null(zoneid)){
    int.data <- dataset
    colnames(int.data)[colnames(int.data)==zoneid] <- 'ZoneID'
  } else {
    int.data <- assignment_column(
      dat = dataset, project = project, spat = spatdat, hull.polygon = FALSE, lon.spat = lon.spat, lat.spat = lat.spat, lon.dat = lon.dat,
      lat.dat = lat.dat, cat = cat, closest.pt = TRUE, log.fun = FALSE
    )
  }
  
  # Create starting loc variable
  if (is.null(trip_id)) {
    int.data <- int.data[order(int.data[[haul_order]]), ]
  } else {
    int.data <- int.data[order(int.data[[trip_id]], int.data[[haul_order]]), ]
  }
  newvar <- rep(NA, nrow(int.data))
  newvar[2:nrow(int.data)] <- int.data$ZoneID[1:(nrow(int.data) - 1)]

  # Make starting of trips set to zone of starting port
  if (!is.null(trip_id)) {
    rownumbers <- match(trimws(int.data[tapply(seq_along(int.data[[trip_id]]), int.data[[trip_id]], min), starting_port]), port$Port_Name)
    if(any(is.na(rownumbers))) { warning('NAs produced. At least one disembarked port was not found in the port table.')}
    newvar[tapply(seq_along(int.data[[trip_id]]), int.data[[trip_id]], min)] <- port[rownumbers, "ZoneID"]
  } else {
    rownumbers <- match(trimws(int.data[1, starting_port]), port$PORT)
    newvar[1] <- port[rownumbers, "ZoneID"]
  }
  
  g <- cbind(dataset, newvar)
  colnames(g)[dim(g)[2]] = name

  create_startingloc_function <- list()
  create_startingloc_function$functionID <- "create_startingloc"
  create_startingloc_function$args <- list(
    dat, project, spat, portTable, trip_id, haul_order, starting_port,
    lon.dat, lat.dat, cat, name)
  create_startingloc_function$kwargs <- list("lon.spat" = lon.spat, "lat.spat" = lat.spat)
  create_startingloc_function$output <- list(dat)

  log_call(project, create_startingloc_function)

  return(g)
}
