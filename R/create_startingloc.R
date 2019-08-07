#' Create Starting location vector
#'
#' Creates a vector containing zone/area location when choice of where to fish next was chosen. 
#'
#' @param dat  Main data frame containing data on hauls or trips. Table in fishset_db database should contain the string `MainDataTable`.
#' @param gridfile Spatial data. Shape, json, and csv formats are supported.
#' @param PortTable Port data frame. Contains columns: Port_Name, Port_Long, Port_Lat. Table is generated using the load_port function and saved in the fishset_db database.
#' @param trip_id Variable in 'dat' to identify unique trips. 
#' @param haul_order Variable in `dat` containing information on the order that hauls occur within a trip. Can be time, coded variable, etc.
#' @param starting_port Variable in `dat` to identify port at start of trip
#' @param lon.dat Longitude variable in dataset
#' @param lat.dat Latitude variable in dataset
#' @param lon.grid Longitude variable in gridfile
#' @param lat.grid Latitude variable in gridfile
#' @param cat Variable defining zones or areas. Must be defined for dataset or gridfile.
#' @importFrom DBI dbExecute
#' @export create_startloc
#' @details Function creates the startloc vector that is needed for the logit_correction function. The assignment_column function is called to assign 
#' port locations and haul locations to zones. The starting port is used to define the starting location at the start of the trip. 
#' @examples
#' \dontrun{
#' MainDataTable$startloc <- create_startloc(MainDataTable, map2, pollockPortTable, 'TRIP_SEQ','HAUL_SEQ','DISEMBARKED_PORT',
#'                                 "LonLat_START_LON","LonLat_START_LAT", "","", 'NMFS_AREA')
#' }

create_startloc <- function(dat, gridfile, PortTable, trip_id, haul_order, starting_port, lon.dat, lat.dat, lon.grid, lat.grid, cat) {
  
  #Call in datasets
  fishset_db <- DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite")
  if(is.character(dat)==TRUE){
    if(is.null(dat)==TRUE | table_exists(dat)==FALSE){
      print(DBI::dbListTables(fishset_db))
      stop(paste(dat, 'not defined or does not exist. Consider using one of the tables listed above that exist in the database.'))
    } else {
      dataset <- table_view(dat)
    }
  } else {
    dataset <- dat  
  }
  
  #  in port table
  if(is.character(PortTable)==TRUE){
    if(table_exists(PortTable)==FALSE){
      print(DBI::dbListTables(fishset_db))
      stop(paste(PortTable, 'not defined or does not exist. Consider using one of the tables listed above that exist in the database.'))
    } else {
      port.table <- table_view(PortTable)
    }
  } else {
    port.table <- PortTable
  }
  
  DBI::dbDisconnect(fishset_db)
  
  port <- assignment_column(dat=port.table, gridfile = gridfile, hull.polygon = FALSE, 
                            lon.grid = lon.grid, lat.grid = lat.grid, lon.dat = 'LONGITUDE', 
                            lat.dat = 'LATITUDE', cat = cat, closest.pt = TRUE)
  
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
       rownumbers <- match(trimws(int.data[tapply(seq_along(int.data[[trip_id]]), int.data[[trip_id]], min), starting_port]), port$PORT)
      startingloc[tapply(seq_along(int.data[[trip_id]]), int.data[[trip_id]], min)] <- port[rownumbers, 'ZoneID']
  } else {
    rownumbers <- match(trimws(int.data[1, starting_port]), port$PORT)
    startingloc[1] <- port[rownumbers, 'ZoneID']
  }
  

  if(!exists('logbody')) { 
    logbody <- list()
    infoBodyout <- list()
    functionBodyout <- list()
    infobody <- list()
    
    infobody$rundate <- Sys.Date()
    infoBodyout$info <- list(infobody)
    
    functionBodyout$function_calls <- list()
    
    logbody$fishset_run <- list(infoBodyout, functionBodyout)
    
  } 
  create_startingloc_function <- list()
  create_startingloc_function$functionID <- 'create_startingloc'
  create_startingloc_function$args <- c(deparse(substitute(dat)), deparse(substitute(gridfile)), deparse(substitute(PortTable)), trip_id, 
                                        haul_order, lon.dat, lat.dat, lon.grid, lat.grid, cat)
  create_startingloc_function$kwargs <- c()
  create_startingloc_function$output <- c()
  functionBodyout$function_calls[[length(functionBodyout$function_calls)+1]] <- (create_startingloc_function)
  logbody$fishset_run <- list(infoBodyout, functionBodyout)
  write(jsonlite::toJSON(logbody, pretty = TRUE, auto_unbox = TRUE), paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""))
  assign("functionBodyout", value = functionBodyout, pos = 1)
  
  return(startingloc)
}                                                                                                                                                                                                                           





