


#' Create distance between points variable - non-interactive version
create_dist_between_for_gui <- function(dat, project, start=c('lat','lon'), end=c('lat','lon'), units, name = "DistBetwen",
                                        portTable = NULL, zoneid, spat = NULL, lon.dat = NULL, lat.dat = NULL, 
                                       cat = NULL, lon.spat = NULL, lat.spat = NULL) {
  #' @param dat Primary data containing information on hauls or trips. Table in FishSET database contains the string 'MainDataTable'.
  #' @param project Project name
  #' @param start,end Starting location. Should be a port, lat/lon location, or centroid of regulatory zone/area. 
  #'   \tabular{rlll}{
  #'   port: \tab  \code{start} should be the column name in \code{dat} containing the port names. Latitude and longitude for the port are extracted
  #'   from the port table. \cr
  #'   lat/lon: \tab \code{start} should be a character string of column names from \code{dat}.
  #'   The order must be `lat` then `lon` \code{start=c('lat', 'lon')}. \cr
  #'   }
  #' @param units Unit of distance. Choices are \code{"miles"}, \code{"kilometers"}, or \code{"midpoint"}.
  #' @param portTable Data table containing port data. Required if \code{start} or \code{end} are a vector from the \code{dat} containing port names.
  #' @param zoneid Variable in \code{dat} that identifies the individual zones or areas. Required if zone identifier variable exists and 
  #'   is not `ZoneID`. Defaults to NULL.
  #' @param spat Spatial data containing information on fishery management or regulatory zones. Shape, json, geojson, and csv formats are supported. 
  #'     Required if \code{start} or \code{end} are \code{"centroid"} and a centroid table doesn't exist in the FishSET database.
  #' @param lon.dat Longitude variable from \code{dat}. Required if \code{start} or \code{end} are ‘centroid’.
  #' @param lat.dat Latitude variable from \code{dat}. Required if \code{start} or \code{end} are ‘centroid’.
  #' @param name String, name of new variable. Defaults to `DistBetween`.
  #' @param lon.spat Variable or list from \code{spat} containing longitude data. Required for csv files. 
  #'   Leave as NULL if \code{spat} is a shape or json file, Required if \code{start} or \code{end} are \code{"centroid"}.
  #' @param lat.spat Variable or list from \code{spat} containing latitude data. Required for csv files. Leave as NULL if \code{spat} is a shape or json file, Required if \code{start} or \code{end} are \code{"centroid"}.
  #' @param cat Variable or list in \code{spat} that identifies the individual areas or zones. If \code{spat} is class sf, \code{cat}
  #'   should be name of list containing information on zones. Required if \code{start} or \code{end} are \code{"centroid"}.
  #' @return Primary data set with distance between points variable added.
  #' @export
  #' @importFrom geosphere distGeo midPoint
  #' @description Adds distance between two points to the primary data set. There are two versions of this function. The difference between the two versions is
  #'   how additional arguments specific to start and end locations are added. This
  #'   version requires all necessary arguments to be specified before running and is best used in a non-interactive session.
  #'   The \code{\link{create_dist_between}} version requires only five arguments to be specified before running. Additional arguments
  #'   specific to identifying the lat/long of start or end points are added through prompts. This function is designed for an
  #'   interactive session. Both versions of the distance between function require that the start and end points be different vectors.
  #'   If the start or ending points are from a port, then \code{PortTable} must be specified to obtain lat/lons. If the start or ending
  #'   points are the center of a fishing zone or area then \code{spat}, \code{lon.dat}, \code{lat.dat}, \code{cat}, \code{lon.spat}, and \code{lat.spat}
  #'   must be specified to obtain latitude and longitude.


  # Call in data sets
  if (start[1] == end[1]) {
    warning("Starting and ending vectors are identical.")
  } else {
 #   fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase())

    # Call in datasets
    out <- data_pull(dat, project)
    dataset <- out$dataset
    dat <- parse_data_name(dat, "main", project)
    
    spat_out <- data_pull(spat, project)
    spatdat <- spat_out$dataset
    spat <- parse_data_name(dat, "spat", project)
    
    
    name <- ifelse(is_empty(name), "DistBetween", name)
    
    if(!exists('project')){
      project <- sub("\\MainDataTable", "", dat)
    }

    # Get location data for centroid or port
    x <- 0

    #Get centroid table
    if (grepl("centroid", start[1], ignore.case = TRUE) | grepl("centroid", end[1], ignore.case = TRUE)) {
      if(table_exists(paste0(spat, 'Centroid'), project)|table_exists('spatCentroid', project)){
        if(table_exists(paste0(spat, 'Centroid'), project) ==TRUE) {
          int <- table_view(paste0(spat, 'Centroid'), project)
        } else {
          int <- table_view('spatCentroid', project)
        }
      } else {
        int <- find_centroid(project=project, spat = spatdat, lon.spat = lon.spat, lat.spat = lat.spat, cat = cat)
      }
    }
    
    #Get zoneID information
    if (grepl("centroid", start[1], ignore.case = TRUE) | grepl("centroid", end[1], ignore.case = TRUE)) {
      if("ZoneID" %in% names(dataset) == TRUE){
        dat2 <- dataset
        zoneid <- 'ZoneID'
      } else if(!is.null(zoneid)){
        dat2 <- dataset
        colnames(dat2)[colnames(dat2)==zoneid] <- 'ZoneID'
      } else {
        dat2 <- assignment_column(
        dat = dataset, project=project, spat = spatdat, hull.polygon = FALSE, lon.spat = lon.spat, lat.spat = lat.spat, lon.dat = lon.dat,
        lat.dat = lat.dat, cat = cat, closest.pt = TRUE, log.fun = FALSE
      )
      }
    }
    
    
    if ((grepl("port", start[1], ignore.case = TRUE) | grepl("port", end[1], ignore.case = TRUE)) == TRUE) {
      # port.table <- table_view(portTable)
      temp <- data_pull(portTable, project)
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
        stop("Longitude is not valid (outside -180:180). Function not run")
      }
      if (any(abs(start.lat) > 90)) {
        stop("Latitude is not valid (outside -90:90. Function not run")
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
        stop("Longitude is not valid (outside -180:180). Function not run. Check that lat and lon variables are specified in correct order.")

      }
      if (any(abs(end.lat) > 90)) {
        stop("Latitude is not valid (outside -90:90. Function not run. Check that lat and lon variables are specified in correct order.")
      }
    }


      # Get distance between points
      if (units == "midpoint") {
        newvar <- geosphere::midPoint(cbind(start.long, start.lat), cbind(end.long, end.lat))
      } else {
        newvar <- geosphere::distGeo(cbind(start.long, start.lat), cbind(end.long, end.lat), a = 6378137, f = 1 / 298.257223563)
      }

      if (units == "miles") {
        newvar <- newvar * 0.000621371192237334
      } else if (units == "kilometers") {
        newvar <- newvar / 1000
      }

      g <- cbind(dataset, newvar)
      colnames(g)[dim(g)[2]] = name
      
      # Log the function
      create_dist_between_function <- list()
      create_dist_between_function$functionID <- "create_dist_between"
      create_dist_between_function$args <- list(dat, project, start, end, units, deparse(substitute(name)))
      create_dist_between_function$kwargs <- list(portTable, spat, zoneid, lon.dat, lat.dat, cat, lon.spat, lat.spat)
      create_dist_between_function$output <- list(dat)

      log_call(project, create_dist_between_function)

      return(g)
  }
}
