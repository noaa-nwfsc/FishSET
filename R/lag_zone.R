#' Lag zone variable
#'
#' Creates a lagged zone ID variable for haul data, where the lagged location of the first haul
#' is filled in with the port of departure. This variable is required for data with multiple sets
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
#' @param port_name String indicating the column in port table that contains the port name
#' @param port_lon String indication the column in port table that contains port longitude
#' @param port_lat String indication the column in port table that contains port latitude
#' @param trip_id Variable in \code{dat} that identifies unique trips.
#' @param haul_order Variable in \code{dat} containing information on the order 
#'   that hauls occur within a trip. Can be time, coded variable, etc.
#' @param starting_port Variable in \code{dat} to identify port at start of trip.
#' @param zoneID_dat Variable in \code{dat} that identifies the individual zones or 
#'   areas.
#' @param zoneID_spat Variable in \code{spat} that identifies the individual zones or 
#'   areas.
#' @param name String, name of created variable. Defaults to name of the function 
#'   if not defined.
#' @importFrom DBI dbExecute
#' @export lag_zone
#' @return Primary data set with starting location variable added.
#' @details Function creates the \code{startloc} vector that is required for multihaul data
#'   and the full information model with Dahl's correction \code{\link{logit_correction}}. 
#'   The vector is the lagged zone ID - zone when the decision of where to fish 
#'   next was made. Generally, the first zone of a trip is the port of departure. 
#'   The \code{\link{assignment_column}} function is called to assign starting port 
#'   locations and haul locations to zones. If ZoneID exists in \code{dat}, 
#'   \code{\link{assignment_column}} is not called and the following arguments are 
#'   not required: \code{spat, zoneID_spat}. 
#' @examples
#' \dontrun{
#' pcodMainDataTable <- lag_zone(pcodMainDataTable, 'pcod',
#'     map2, "pcodPortTable", "TRIP_SEQ", "HAUL_SEQ", "DISEMBARKED_PORT", 
#'  "START_LON", "START_LAT", "NMFS_AREA", "STARTING_LOC"
#' )
#' }
#
lag_zone <- function(dat,
                     project,
                     spat = NULL,
                     port,
                     port_name,
                     port_lon,
                     port_lat,
                     trip_id,
                     haul_order,
                     starting_port,
                     zoneID_dat,
                     zoneID_spat = NULL, 
                     name = "startingloc") {
  
  # Call in data sets
  out <- data_pull(dat, project)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main", project)
  
  # Call in port table
  out <- data_pull(port, project)
  port_table <- out$dataset
  port <- parse_data_name(port, 'port', project)
  
  # Call in spatial data
  spat_out <- data_pull(spat, project)
  spatdat <- spat_out$dataset
  spat <- parse_data_name(spat, "spat", project)
  
  # Input validation
  required_dat_cols <- c(trip_id, haul_order, starting_port, zoneID_dat)
  missing_dat_cols <- required_dat_cols[!required_dat_cols %in% names(dataset)]
  if (length(missing_dat_cols) > 0) {
    stop(paste0("The following required columns are missing from the main data ('dat'): ",
                paste(missing_dat_cols, collapse = ", ")))
  }
  
  # Check for required columns in the port table
  required_port_cols <- c(port_name, port_lon, port_lat)
  missing_port_cols <- required_port_cols[!required_port_cols %in% names(port_table)]
  if (length(missing_port_cols) > 0) {
    stop(paste0("The following required columns are missing from the port data ('port'): ",
                paste(missing_port_cols, collapse = ", ")))
  }
  
  # Check for required columns in the spatial data, if provided
  if (!is.null(spatdat)) {
    if (is.null(zoneID_spat) || !zoneID_spat %in% names(spatdat)) {
      stop(paste0("The specified zone ID column '", 
                  zoneID_spat, 
                  "' is missing from the spatial data ('spat')."))
    }
  }
  
  # Make sure new column name is unique and doesn't already exist
  if (name %in% names(dataset)) {
    stop(paste0("The column name '", name, 
                "' already exists in the data. Please choose a different name."))
  }
  
  # Need to assign zones to ports
  port_table <- assignment_column(dat = port_table, 
                                  project = project, 
                                  spat = spatdat, 
                                  hull_polygon = FALSE, 
                                  lon_dat = port_lon, 
                                  lat_dat = port_lat, 
                                  zoneID_spat = zoneID_spat, 
                                  closest_pt = TRUE, 
                                  log_fun = FALSE, 
                                  bufferval = 100) # need to consider how this affects things
  names(port_table)[which(names(port_table) == "ZoneID")] <- "port_zone"
  
  new_col_name <- name
  
  port_to_join <- port_table %>%
    select(all_of(port_name), "port_zone")
  
  dataset_lagged <- dataset %>%
    left_join(port_to_join, by = setNames(port_name, starting_port)) %>%
    group_by(!!sym(trip_id)) %>%
    arrange(!!sym(haul_order), .by_group = TRUE) %>%
    mutate(
      "{new_col_name}" := coalesce(lag(!!sym(zoneID_dat)), .data[["port_zone"]])
    ) %>%
    ungroup() %>%
    select(-"port_zone")
  
  lag_zone_function <- list()
  lag_zone_function$functionID <- "lag_zone"
  lag_zone_function$args <- list(dat, 
                                 project, 
                                 spat, 
                                 port, 
                                 trip_id, 
                                 haul_order, 
                                 starting_port, 
                                 zoneID_dat, 
                                 zoneID_spat, 
                                 name)
  
  log_call(project, lag_zone_function)
  return(dataset_lagged)
}
