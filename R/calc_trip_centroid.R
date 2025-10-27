#' Create trip centroid variable
#'
#' Create latitude and longitude variables containing the centroid of each trip
#'
#' @param dat Primary data containing information on hauls or trips. Table in the FishSET database 
#'   contains the string 'MainDataTable'.
#' @param project Project name. 
#' @param lat Variable in \code{dat} containing latitudinal data.
#' @param lon Variable in \code{dat} containing longitudinal data.
#' @param trip_id Variable in \code{dat} containing trip identifier. If trip identifier should be 
#'   defined by more than one variable then list as \code{c('var1', 'var2')}.
#' @param weight_var Variable in \code{dat} for computing the weighted average.
#' @details Computes the average longitude and latitude for each trip. Specify \code{weight_var} 
#'   to calculate the weighted centroid. Additional arguments can be added that define unique 
#'   trips. If no additional arguments are added, each row will be treated as a unique trip.
#' @return Returns the primary dataset with centroid latitude and centroid longitude variables 
#'   added.
#' @importFrom stats ave
#' @export
#' @examples
#' \dontrun{
#' pollockMainDataTable <- create_trip_centroid(pollockMainDataTable, 'pollock', 
#'   'LonLat_START_LON', 'LonLat_START_LAT', weight_var = NULL, 'DISEMBARKED_PORT', 
#'   'EMBARKED_PORT')
#' }

create_trip_centroid <- function(dat, 
                                 project, 
                                 lon, 
                                 lat, 
                                 trip_id, 
                                 weight_var = NULL) {

  
  out <- data_pull(dat, project)
  dataset <- out$dataset
  
  dat <- parse_data_name(dat, "main", project)
  
  x <- 0
  if (any(abs(dataset[[lon]]) > 180)) {
    stop("Longitude is not valid (outside -180:180). Function not run")
    # stop('Longitude is not valid (outside -180:180.')
    
  }
  if (any(abs(dataset[[lat]]) > 90)) {
    stop("Latitude is not valid (outside -90:90. Function not run")
    
    # stop('Latitude is not valid (outside -90:90.')
  }
  
  
  #    if (grepl("input", as.character(match.call(expand.dots = FALSE)$...)[1]) == TRUE) {
  #      argList <- eval(...)
  #    } else {
  #     argList <- (as.character(match.call(expand.dots = FALSE)$...))
  #   }
  
  
  idmaker <- function(vec) {
    return(paste(sort(vec), collapse = ""))
  }
  
  
  int <- as.data.frame(cbind(dataset, rowID = as.numeric(factor(apply(as.matrix(dataset[, trip_id]), 1, idmaker)))))
  # int <- int[, c(colnames(sapply(dataindex[[varnameindex]], grepl, colnames(int))), 'rowID')]
  cat(length(unique(int$rowID)), "unique trips were identified using", trip_id, "\n")
  # Handling of empty variables
  if (any(apply(int, 2, function(x) all(is.na(x))) == TRUE)) {
    int <- int[, -which(apply(int, 2, function(x) all(is.na(x))) == TRUE)]
  } else {
    int <- int
  }
  
  if (is_empty(weight_var)) {
    int$cent.lon <- stats::ave(int[[lon]], int[["rowID"]])
    int$cent.lat <- stats::ave(int[[lat]], int[["rowID"]])
  } else {
    # weighted centroid
    int$cent.lon <- stats::ave(int[c(lon, weight_var)], int[["rowID"]], FUN = function(x) stats::weighted.mean(x[[lon]], x[[weight_var]]))[[1]]
    int$cent.lat <- stats::ave(int[c(lat, weight_var)], int[["rowID"]], FUN = function(x) stats::weighted.mean(x[[lat]], x[[weight_var]]))[[1]]
  }
  
  create_trip_centroid_function <- list()
  create_trip_centroid_function$functionID <- "create_trip_centroid"
  create_trip_centroid_function$args <- list(dat, project, lon, lat, trip_id, weight_var)
  create_trip_centroid_function$kwargs <- list()
  create_trip_centroid_function$output <- list(dat)
  log_call(project, create_trip_centroid_function)
  
  return(int)
  
}