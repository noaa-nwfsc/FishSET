#' Calculate trip centroid variable
#'
#' Calculate latitude and longitude variables (columns) containing the geographic
#' centroid of each trip
#'
#' @param dat String or data frame. A string for the name of the main data table in the FishSET 
#'   database (contains 'MainDataTable' in the name). Or a data frame of the main data table.
#' @param project String. project name. 
#' @param lat String. Column name in \code{dat} containing latitudinal data.
#' @param lon String. Column name in \code{dat} containing longitudinal data.
#' @param trip_id String. Column name that represents the unique trip identifier in \code{dat}.
#' @param weight_var String. Optional. Column name in \code{dat} to use for computing a 
#'   weighted weighted average centroid. If \code{NULL} (the default), an unweighted (simple)
#'   average is calculated.
#' @details This function computes the average longitude and latitude for each unique trip, as 
#'   defined by the \code{trip_id} column. If \code{weight_var} is specified, the function 
#'   calculates the weighted centroid.
#' @return Returns the original data frame (\code{dataset}) with two new columns added: 
#'   \code{cent_lon} (centroid longitude) and \code{cent_lat} (centroid latitude).
#' @importFrom stats ave weighted.mean
#' @export
#' @examples
#' \dontrun{
#' # Assuming 'pollockMainDataTable' is a data frame
#'
#' pollockMainDataTable <- calc_trip_centroid(
#'    dat = pollockMainDataTable, 
#'    prpoject = 'pollock', 
#'   lon = 'LonLat_START_LON',
#'   lat = 'LonLat_START_LAT',
#'   trip_id = "TRIP_ID",
#'   weight_var = NULL
#' )
#' }

calc_trip_centroid <- function(dat, 
                               project, 
                               lon, 
                               lat, 
                               trip_id, 
                               weight_var = NULL) {
  
  # Pull in data ----------------------------------------------------------------------------------
  out <- data_pull(dat, project)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main", project)
  
  # Input validation ------------------------------------------------------------------------------
  if (any(abs(dataset[[lon]]) > 180)) {
    stop("Longitude is not valid (outside -180:180). Function aborted")
    
  }
  if (any(abs(dataset[[lat]]) > 90)) {
    stop("Latitude is not valid (outside -90:90. Function aborted")
  }
  
  # Calculate centroid ----------------------------------------------------------------------------
  # Create the grouping factor
  group_factor <- dataset[[trip_id]]
  
  if (is_empty(weight_var)) {
    # Unweighted centroid (simple average)
    dataset$cent_lon <- stats::ave(dataset[[lon]], group_factor)
    dataset$cent_lat <- stats::ave(dataset[[lat]], group_factor)
    
  } else {
    # Weighted centroid
    w <- dataset[[weight_var]]
    lon_vec <- dataset[[lon]]
    lat_vec <- dataset[[lat]]
    
    # Calculate sum of (value * weight) for each group
    sum_lon_x_w <- stats::ave(lon_vec * w, group_factor, FUN = sum)
    sum_lat_x_w <- stats::ave(lat_vec * w, group_factor, FUN = sum)
    
    # Calculate sum of weights for each group
    sum_w <- stats::ave(w, group_factor, FUN = sum)
    
    # Compute the weighted average
    dataset$cent_lon <- sum_lon_x_w / sum_w
    dataset$cent_lat <- sum_lat_x_w / sum_w
  }
  
  # Logging ---------------------------------------------------------------------------------------
  calc_trip_centroid_function <- list()
  calc_trip_centroid_function$functionID <- "calc_trip_centroid"
  calc_trip_centroid_function$args <- list(dat, project, lon, lat, trip_id, weight_var)
  calc_trip_centroid_function$kwargs <- list()
  calc_trip_centroid_function$output <- list(dat)
  log_call(project, calc_trip_centroid_function)
  
  return(dataset)
}