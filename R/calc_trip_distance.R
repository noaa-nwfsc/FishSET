#' Calculate Trip Distance
#' 
#' Calculates the total distance for a fishing trip and adds it as a new column to the dataset.
#'
#' @param name String. Name of new variable for trip distance. Defaults to `trip_distance`.
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
#' @param distance_unit String. The desired unit for the output distance. Options are
#'   \code{"miles"} (default), \code{"kilometers"}, or \code{"meters"}. Aliases \code{"mi"} and
#'   \code{"km"} are also accepted.
#' @param a Numeric. Major (equatorial) radius of the ellipsoid. The default value is for 
#'   WGS84 ellipsoid.
#' @param f Numeric. Ellipsoid flattening. The default value is for WGS84 ellipsoid.
#' 
#' @importFrom geosphere distGeo
#' @importFrom dplyr arrange group_by lag left_join mutate rename select summarise sym
#' 
#' @export calc_trip_distance
#' 
#' @return Returns the main data table with a new variable for trip distance.
#' 
#' @details 
 #' This function calculates the total distance traveled during a fishing trip by summing its four
#' key segments:
#' 1.  Distance from the departure port to the start of the first haul.
#' 2.  The cumulative distance covered *within* each haul.
#' 3.  The cumulative distance traveled *between* consecutive hauls.
#' 4.  The final distance from the end of the last haul to the return port.
#'
#' The function uses a vectorized approach with the `dplyr` package for efficiency and relies on
#' `geosphere::distGeo` for accurate geodesic distance calculations based on the WGS84 
#' ellipsoid model.
#'   
#' @examples
#' \dontrun{
#' main_data_with_distance <- calc_trip_distance(
#'   name = "total_trip_miles",
#'   project = "my_project",
#'   dat = main_data,
#'   port = port_data,
#'   trip_id = "TRIP_ID",
#'   haul_order = "HAUL_NUMBER",
#'   starting_port = "DEPARTURE_PORT_NAME",
#'   return_port = "LANDING_PORT_NAME",
#'   start_haul_lat = "START_LATITUDE",
#'   start_haul_lon = "START_LONGITUDE",
#'   end_haul_lat = "END_LATITUDE",
#'   end_haul_lon = "END_LONGITUDE",
#'   distance_unit = "miles"
#' )
#' }
#'
calc_trip_distance <- function(name = "trip_distance", 
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
                               distance_unit = "miles",
                               a = 6378137, 
                               f = 1/298.257223563) {
  
  # Pull in datasets ------------------------------------------------------------------------------
  out <- data_pull(dat, project)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main", project)
  
  out <- data_pull(port, project)
  port_table <- out$dataset
  port <- parse_data_name(port, 'port', project)
  
  # Prepare data: join port coordinates into the main dataset -------------------------------------
  port_coords <- port_table %>%
    select(Port_Name, start_port_lon = Port_Long, start_port_lat = Port_Lat)
  
  dataset_joined <- dataset %>%
    left_join(port_coords, by = setNames("Port_Name", starting_port)) %>%
    left_join(port_coords %>% rename(return_port_lon = start_port_lon,
                                     return_port_lat = start_port_lat),
              by = setNames("Port_Name", return_port))
  
  # Calculate all distance segments ---------------------------------------------------------------
  trip_distances <- dataset_joined %>%
    group_by(!!sym(trip_id)) %>%
    arrange(!!sym(haul_order), .by_group = TRUE) %>%
    mutate(
      # Get the end coordinates of the *previous* haul within the trip
      prev_end_lon = lag(!!sym(end_haul_lon)),
      prev_end_lat = lag(!!sym(end_haul_lat)),
      
      # Distance within each haul
      within_haul_dist = geosphere::distGeo(
        p1 = cbind(!!sym(start_haul_lon), !!sym(start_haul_lat)),
        p2 = cbind(!!sym(end_haul_lon), !!sym(end_haul_lat)),
        a = a,
        f = f
      ),
      
      # Distance between hauls (end of previous and start of current)
      between_haul_dist = geosphere::distGeo(
        p1 = cbind(prev_end_lon, prev_end_lat),
        p2 = cbind(!!sym(start_haul_lon), !!sym(start_haul_lat)),
        a = a,
        f = f
      )
    ) %>%
    
    # Summarize all segments for each trip
    summarise(
      # Departure port to first haul
      port_to_start_dist = first(geosphere::distGeo(
        p1 = cbind(first(start_port_lon), first(start_port_lat)),
        p2 = cbind(first(!!sym(start_haul_lon)), first(!!sym(start_haul_lat))),
        a = a,
        f = f
      )),
      
      # Last haul to return port
      end_to_port_dist = first(geosphere::distGeo(
        p1 = cbind(last(!!sym(end_haul_lon)), last(!!sym(end_haul_lat))),
        p2 = cbind(last(return_port_lon), last(return_port_lat)),
        a = a,
        f = f
      )),
      
      # Sum distance within each haul and distance between hauls
      total_within_between_dist = 
        sum(within_haul_dist, na.rm = TRUE) + 
        sum(between_haul_dist, na.rm = TRUE),
      
      .groups = 'drop'
    ) %>%
    
    # Calculate total distance in meters
    mutate(
      total_dist_meters = rowSums(select(., ends_with("_dist")), na.rm = TRUE)
    )
  
  # Unit conversion -------------------------------------------------------------------------------
  unit <- tolower(distance_unit)
  trip_distances$final_dist <- switch(unit,
                                      "kilometers" = trip_distances$total_dist_meters / 1000,
                                      "km" = trip_distances$total_dist_meters / 1000,
                                      "miles" = trip_distances$total_dist_meters / 1609.34,
                                      "mi" = trip_distances$total_dist_meters / 1609.34,
                                      "meters" = trip_distances$total_dist_meters,
                                      {
                                        warning(paste("Invalid distance_unit '", 
                                                      unit, 
                                                      "'. Defaulting to meters."), call. = FALSE)
                                        trip_distances$total_dist_meters
                                      })
  
  final_dataset <- dataset %>%
    left_join(trip_distances %>% select(!!sym(trip_id), final_dist), by = trip_id) %>%
    rename(!!sym(name) := final_dist)
  
  create_TD_function <- list()
  create_TD_function$functionID <- "calc_trip_distance"
  create_TD_function$args <- list(name,
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
                                  distance_unit,
                                  a, 
                                  f)
  create_TD_function$output <- list(dat)
  log_call(project, create_TD_function)
  
  return(final_dataset)
}
