#' Create Starting location vector
#'
#' Creates a vector containing zone/area location when choice of where to fish next was chosen. 
#'
#' @param dat  Main data frame containing data on hauls or trips. Table in fishset_db database should contain the string `MainDataTable`.
#' @param gridfile Spatial data. Shape, json, and csv formats are supported.
#' @param portTable Port data frame. Contains columns: Port_Name, Port_Long, Port_Lat. Table is generated using the load_port function and saved in the fishset_db database.
#' @param trip_id Variable in 'dat' to identify unique trips. 
#' @param haul_order Variable in `dat` containing information on the order that hauls occur within a trip. Can be time, coded variable, etc.
#' @param starting_port Variable in `dat` to identify port at start of trip
#' @param lon.dat Longitude variable in dataset
#' @param lat.dat Latitude variable in dataset
#' @param lon.grid Longitude variable in gridfile
#' @param lat.grid Latitude variable in gridfile
#' @param cat Variable defining zones or areas. Must be defined for dataset or gridfile.
#' @importFrom DBI dbExecute
#' @export create_startingloc
#' @details Function creates the startloc vector that is needed for the logit_correction function. The assignment_column function is called to assign 
#' port locations and haul locations to zones. The starting port is used to define the starting location at the start of the trip. 
#' @examples
#' \dontrun{
#' MainDataTable$startloc <- create_startingloc(MainDataTable, map2, pollockPortTable, 
#'                               'TRIP_SEQ','HAUL_SEQ','DISEMBARKED_PORT',
#'                               "LonLat_START_LON","LonLat_START_LAT", 'NMFS_AREA', "","")
#' }

create_startingloc <- function(dat, gridfile, portTable, trip_id, haul_order, starting_port, lon.dat, lat.dat, cat, lon.grid=NULL, lat.grid=NULL) {
  #Call in datasets
  out <- data_pull(dat)
  dat <- out$dat
  dataset <- out$dataset
  
  
  #  in port table
  out <- data_pull(portTable)
  PortTable <- out$dat
  port.table <- out$dataset
  
  
  DBI::dbDisconnect(fishset_db)
  
  port <- assignment_column(dat=port.table, gridfile = gridfile, hull.polygon = FALSE, 
                            lon.grid = lon.grid, lat.grid = lat.grid, lon.dat = 'Port_Long', 
                            lat.dat = 'Port_Lat', cat = cat, closest.pt = TRUE)
  
  int.data <- assignment_column(dat=dataset, gridfile = gridfile, hull.polygon = FALSE, 
                                lon.grid = lon.grid, lat.grid = lat.grid, lon.dat = lon.dat, 
                                lat.dat = lat.dat, cat = cat, closest.pt = TRUE)
 
#Create starting loc variable  
  if(is.null(trip_id)){
    int.data <- int.data[order(int.data[[haul_order]]),]
  } else {
  int.data <- int.data[order(int.data[[trip_id]], int.data[[haul_order]]),]
  }
  startingloc <- rep(NA, nrow(int.data))
  startingloc[2:nrow(int.data)] <- int.data$ZoneID[1:(nrow(int.data)-1)]

  #Make starting of trips set to zone of starting port  
  if(!is.null(trip_id)){
       rownumbers <- match(trimws(int.data[tapply(seq_along(int.data[[trip_id]]), int.data[[trip_id]], min), starting_port]), port$Port_Name)
      startingloc[tapply(seq_along(int.data[[trip_id]]), int.data[[trip_id]], min)] <- port[rownumbers, 'ZoneID']
  } else {
    rownumbers <- match(trimws(int.data[1, starting_port]), port$PORT)
    startingloc[1] <- port[rownumbers, 'ZoneID']
  }
  

  create_startingloc_function <- list()
  create_startingloc_function$functionID <- 'create_startingloc'
  create_startingloc_function$args <- c(dat, deparse(substitute(gridfile)), portTable, trip_id, 
                                        haul_order, lon.dat, lat.dat, lon.grid, lat.grid, cat)
  create_startingloc_function$kwargs <- c()
  create_startingloc_function$output <- c()
   
  log_call(create_startingloc_function)
  
  return(startingloc)
}                                                                                                                                                                                                                           





