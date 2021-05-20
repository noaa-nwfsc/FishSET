#' Number of observations by temporal unit
#'
#' View the number of observations by year, month, and zone in table format
#'
#' @param dat Primary data containing information on hauls or trips. 
#'   Table in FishSET database contains the string 'MainDataTable'.
#' @param project String, name of project.
#' @param x Variable in \code{dat} containing date variable.
#' @param gridfile Spatial data containing information on fishery management or
#'   regulatory zones. Shape, json, geojson, and csv formats are supported. Required if ‘ZoneID’ does not exist in \code{dat}.
#' @param lon.dat Longitude variable in \code{dat}. Required if ‘ZoneID’ does not exist in \code{dat}.
#' @param lat.dat Latitude variable in \code{dat}. Required if ‘ZoneID’ does not exist in \code{dat}.
#' @param lon.grid Variable or list from \code{gridfile} containing longitude data. Required if ‘ZoneID’ does not
#'   exist in \code{dat} and \code{gridfile} is a csv file. Leave as NULL if \code{gridfile} is a shape or json file.
#' @param lat.grid Variable or list from \code{gridfile} containing latitude data. Required if ‘ZoneID’ does not exist in \code{dat}
#'   and \code{gridfile} is a csv file. Leave as NULL if \code{gridfile} is a shape or json file.
#' @param cat  Variable or list in \code{gridfile} that identifies the individual areas or zones. If \code{gridfile}
#'   is class sf, \code{cat} should be name of list containing information on zones. Required if ‘ZoneID’ does not exist in \code{dat}.
#' @importFrom sp CRS Polygons Polygon SpatialPolygons SpatialPolygonsDataFrame coordinates
#' @importFrom rgeos gDistance
#' @importFrom grDevices chull
#' @importFrom raster projection
#' @details Prints tables displaying the number of observations by year, month, and zone. \code{\link{assignment_column}} is called
#' to assign observations to zones if ‘ZoneID’ does not exist in \code{dat}. Output is not saved.
#' @export
#' @examples
#' \dontrun{
#' temp_obs_table(pollockMainDataTable, gridfile = map2, x = "DATE_FISHING_BEGAN",
#'   lon.dat = "LonLat_START_LON", lat.dat = "LonLat_START_LAT", cat = "NMFS_AREA",
#'   lon.grid = "", lat.grid = ""
#'   )
#' }
#'
temp_obs_table <- function(dat, project, x, gridfile=NULL, lon.dat=NULL, 
                           lat.dat=NULL, cat=NULL, lon.grid = NULL, lat.grid = NULL) {

  # Call in datasets
  out <- data_pull(dat)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main")
  

  if ("ZoneID" %in% names(dat)) {
    out <- dataset
  } else {
    out <- assignment_column(dataset, project=project, gridfile = gridfile, 
     lon.grid = lon.grid, lat.grid = lat.dat, lon.dat = lon.dat, lat.dat = lat.dat, cat = cat,
      closest.pt = FALSE, hull.polygon = TRUE, epsg = NULL, log.fun = FALSE
    )
  }

  out <- temporal_mod(dataset, x, "year", "YEAR", log_fun = FALSE)
  out <- temporal_mod(dataset, x, "%m", "MONTH", log_fun = FALSE)

  cat("Number of observations by year")
  print(table(out$YEAR))
  cat("Number of observations by year and Zone")
  print(table(out$YEAR, out$ZoneID))
  cat("Number of observations by year and month")
  print(table(out$YEAR, out$MONTH))
  cat("Number of observations by year and month split by Zone")
  for (i in 1:length(unique(out$ZoneID))) {
    cat("Zone", unique(out$ZoneID)[i])
    print(table(out$YEAR[out$ZoneID == unique(out$ZoneID)[i]], out$MONTH[out$ZoneID == unique(out$ZoneID)[i]]))
  }

  # Log the function

  temp_obs_table_function <- list()
  temp_obs_table_function$functionID <- "temp_obs_table"
  temp_obs_table_function$args <- list(dat, project, gridfile, x, lon.dat, lat.dat, cat, lon.grid, lat.grid)
  log_call(project, temp_obs_table_function)


  # save_table(out, project, "temp_obs_table")
}
