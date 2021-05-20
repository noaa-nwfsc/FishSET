


#' Create distance between points variable - non-interactive version
create_dist_between_for_gui <- function(dat, start, end, units, name = "DistBetwen", portTable = NULL, gridfile = NULL,
                                        lon.dat = NULL, lat.dat = NULL, cat = NULL, lon.grid = NULL, lat.grid = NULL) {
  #' @param dat Primary data containing information on hauls or trips. Table in FishSET database contains the string 'MainDataTable'.
  #' @param start Starting location. Should be a port, lat/lon location, or the centroid of fishing zone or area. If port is desired,
  #'   \code{start} should be the column name in the \code{dat} containing the port names. Latitude and longitude for the port are extracted
  #'   from the port table. If a lat/lon location is desired then \code{start} should be a character string of column names from \code{dat}.
  #'   The order must be lon, lat. If the centroid of the fishing zone or area is to be used then \code{start} should be \code{"centroid"}
  #'   and \code{\link{find_centroid}} and \code{\link{assignment_column}} will be called to identify the latitude and longitude.
  #' @param end Ending location. Should be a port, lat/lon location, or the centroid of the fishing zone or area. If port is desired,
  #'   \code{end} should be the column name in the \code{dat} containing the port names. Latitude and longitude for the port are extracted from
  #'   the port table. If a lat, long location is desired then \code{end} should be a character string of column names specifying first longitude then
  #'   latitude. If the centroid of the fishing zone or area is to be used then \code{end} should be \code{"centroid"} and \code{\link{find_centroid}}
  #'   and \code{\link{assignment_column}} will be called to identify the latitude and longitude.
  #' @param units Unit of distance. Choices are \code{"miles"}, \code{"kilometers"}, or \code{"midpoint"}.
  #' @param portTable Data table containing port data. Required if \code{start} or \code{end} are a vector from the \code{dat} containing port names.
  #' @param gridfile Spatial data containing information on fishery management or regulatory zones. Shape, json, geojson, and csv formats are supported. Required if \code{start} or \code{end} are \code{"centroid"}.
  #' @param lon.dat Longitude variable from \code{dat}. Required if \code{start} or \code{end} are ‘centroid’.
  #' @param lat.dat Latitude variable from \code{dat}. Required if \code{start} or \code{end} are ‘centroid’.
  #' @param name String, name of new variable. Defaults to `DistBetween`.
  #' @param lon.grid Variable or list from \code{gridfile} containing longitude data. Required for csv files. 
  #'   Leave as NULL if \code{gridfile} is a shape or json file, Required if \code{start} or \code{end} are \code{"centroid"}.
  #' @param lat.grid Variable or list from \code{gridfile} containing latitude data. Required for csv files. Leave as NULL if \code{gridfile} is a shape or json file, Required if \code{start} or \code{end} are \code{"centroid"}.
  #' @param cat Variable or list in \code{gridfile} that identifies the individual areas or zones. If \code{gridfile} is class sf, \code{cat}
  #'   should be name of list containing information on zones. Required if \code{start} or \code{end} are \code{"centroid"}.
  #' @return Primary dataset with distance between points variable added.
  #' @export
  #' @importFrom geosphere distGeo midPoint
  #' @importFrom jsonlite  toJSON
  #' @description Adds distance between two points to the primary dataset. There are two versions of this function. The difference between the two versions is
  #'   how additional arguments specific to start and end locations are added. This
  #'   version requires all necessary arguments to be specified before running and is best used in a non-interactive session.
  #'   The \code{\link{create_dist_between}} version requires only five arguments to be specified before running. Additional arguments
  #'   specific to identifying the lat/long of start or end points are added through prompts. This function is designed for an
  #'   interactive session. Both versions of the distance between function require that the start and end points be different vectors.
  #'   If the start or ending points are from a port, then \code{PortTable} must be specified to obtain lat/lons. If the start or ending
  #'   points are the center of a fishing zone or area then \code{gridfile}, \code{lon.dat}, \code{lat.dat}, \code{cat}, \code{lon.grid}, and \code{lat.grid}
  #'   must be specified to obtain latitude and longitude.


  # Call in datasets
  if (start[1] == end[1]) {
    warning("Starting and ending vectors are identical.")
  } else {
 #   fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase())

    # Call in datasets
    out <- data_pull(dat)
    dataset <- out$dataset
    dat <- parse_data_name(dat, "main")

    if(!exists('project')){
      project <- sub("\\MainDataTable", "", dat)
    }

    # Get location data for centroid or port
    x <- 0

    if (grepl("centroid", start[1], ignore.case = TRUE) | grepl("centroid", end[1], ignore.case = TRUE)) {
      dat2 <- assignment_column(
        dat = dataset, project=project, gridfile = gridfile, hull.polygon = FALSE, lon.grid = lon.grid, lat.grid = lat.grid, lon.dat = lon.dat,
        lat.dat = lat.dat, cat = cat, closest.pt = TRUE, log.fun = FALSE
      )
      int <- find_centroid(dat2,
        gridfile = gridfile, lon.grid == lon.grid, lat.grid == lat.grid, lon.dat = lon.dat, lat.dat = lat.dat, cat = cat,
        weight.var = NULL
      )
    }


    if ((grepl("port", start[1], ignore.case = TRUE) | grepl("port", end[1], ignore.case = TRUE)) == TRUE) {
      # port.table <- table_view(portTable)
      temp <- data_pull(portTable)
      port.table <- temp$dataset
    }

    # start.long <- c() start.lat <- c()


    if (grepl("port", start[1], ignore.case = TRUE)) {
      start.lat <- as.numeric(sapply(trimws(dataset[[start]]), function(x) port.table[which(port.table[["Port_Name"]] == x), "Port_Lat"]))

      start.long <- as.numeric(sapply(trimws(dataset[[start]]), function(x) port.table[which(port.table[["Port_Name"]] == x), "Port_Long"]))
    }
    if (grepl("centroid", start[1], ignore.case = TRUE)) {
      start.lat <- as.numeric(sapply(trimws(dat2[["ZoneID"]]), function(x) int[which(int[["ZoneID"]] == x), "cent.lat"]))
      start.long <- as.numeric(sapply(trimws(dat2[["ZoneID"]]), function(x) int[which(int[["ZoneID"]] == x), "cent.lon"]))
    }


    if (grepl("lat|lon", start[1], ignore.case = TRUE)) {
      start.long <- dataset[[start[2]]]
      start.lat <- dataset[[start[1]]]

      if (any(abs(start.long) > 180)) {
        warning("Longitude is not valid (outside -180:180). Function not run")
        # stop('Longitude is not valid (outside -180:180.')
        x <- 1
      }
      if (any(abs(start.lat) > 90)) {
        warning("Latitude is not valid (outside -90:90. Function not run")
        x <- 1
        # stop('Latitude is not valid (outside -90:90.')
      }
    }


    if (grepl("port", end[1], ignore.case = TRUE)) {
      end.lat <- as.numeric(sapply(trimws(dataset[[end]]), function(x) port.table[which(port.table[["Port_Name"]] == x), "Port_Lat"]))
      end.long <- as.numeric(sapply(trimws(dataset[[end]]), function(x) port.table[which(port.table[["Port_Name"]] == x), "Port_Long"]))
    } else if (grepl("centroid", end[1], ignore.case = TRUE)) {
      end.lat <- as.numeric(sapply(trimws(dat2[["ZoneID"]]), function(x) int[which(int[["ZoneID"]] == x), "cent.lat"]))
      end.long <- as.numeric(sapply(trimws(dat2[["ZoneID"]]), function(x) int[which(int[["ZoneID"]] == x), "cent.lon"]))
    } else {
      end.lat <- dataset[[end[1]]]
      end.long <- dataset[[end[2]]]
      if (any(abs(end.long) > 180)) {
        warning("Longitude is not valid (outside -180:180). Function not run")
        # stop('Longitude is not valid (outside -180:180.')
        x <- 1
      }
      if (any(abs(end.lat) > 90)) {
        warning("Latitude is not valid (outside -90:90. Function not run")
        x <- 1
        # stop('Latitude is not valid (outside -90:90.')
      }
    }

    if (x == 0) {
      # Get distance between points
      if (units == "midpoint") {
        name <- geosphere::midPoint(cbind(start.long, start.lat), cbind(end.long, end.lat))
      } else {
        name <- geosphere::distGeo(cbind(start.long, start.lat), cbind(end.long, end.lat), a = 6378137, f = 1 / 298.257223563)
      }

      if (units == "miles") {
        name <- name * 0.000621371192237334
      } else if (units == "kilometers") {
        name <- name / 1000
      }

      # Log the function
      create_dist_between_function <- list()
      create_dist_between_function$functionID <- "create_dist_between"
      create_dist_between_function$args <- list(dat, start, end, units, deparse(substitute(name)))
      create_dist_between_function$kwargs <- list(portTable, deparse(substitute(gridfile)), lon.dat, lat.dat, cat, lon.grid, lat.grid)
      create_dist_between_function$output <- list(dat)

      log_call(project, create_dist_between_function)

      return(cbind(dataset, name))
    }
  }
}
