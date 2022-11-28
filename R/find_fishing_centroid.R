#'  Create fishing or weighted fishing centroid

#' @param dat  Primary data containing information on hauls or trips. Table in 
#'   FishSET database contains the string 'MainDataTable'.
#' @param project Name of project
#' @param zoneID Variable in \code{dat} that identifies zonal assignments or the If 
#'   \code{spat} is class sf, \code{zoneID} should be name of list containing 
#'   information on zones.
#' @param weight.var Variable from \code{dat} for weighted average. If 
#'   \code{weight.var} is defined, the centroid is defined by the latitude and 
#'   longitude of fishing locations in each zone weighted by \code{weight.var}.
#' @param lon.dat Required. Longitude variable in \code{dat}.  
#' @param lat.dat Required. Latitude variable in \code{dat}.
#' @param names Then names of the fishing centroid columns to be added. A vector
#'   of length two in the order of \code{c("lon", "lat")}. The default is 
#'   \code{c("fish_cent_lon", "fish_cent_lat")} and 
#'   \code{c("weight_cent_lon", "weight_cent_lat")} if \code{weight.var} is used.
#' @param cent.name A string to include in the centroid table name. Table names 
#'   take the form of `"projectNameFishCentroid"` for fishing centroids.
#' @param log.fun Logical, whether to log function call (for internal use).
#' @keywords centroid, zone
#' @importFrom stats ave weighted.mean setNames
#' @importFrom DBI dbConnect dbDisconnect dbWriteTable
#' @importFrom RSQLite SQLite
#' @return Returns primary dataset with fishing centroid and, if \code{weight.var} 
#'   is specified, the weighted fishing centroid. 
#' @export find_fishing_centroid
#' @details Fishing centroid defines the centroid by mean latitude and longitude 
#'   of fishing locations in each zone. Weighted centroid defines the centroid 
#'   by the mean latitude and longitude of fishing locations in each zone weighted 
#'   by the \code{weight.var}. The fishing and weighted centroid variables can be 
#'   used anywhere latitude/longitude variables appear. Each observation in 
#'   \code{dat} must be assigned to a fishery or regulatory area/zone. If the zone 
#'   identifier exists in \code{dat} and is not called \code{'ZoneID'}, then  
#'   \code{zoneID} should be the variable name containing the zone identifier. If a 
#'   zone identifier variable does not exist in \code{dat}, \code{spat} must be 
#'   be specified and \code{zoneID} must be zone identifier in \code{spat}. The 
#'   \code{assignment_column} function will be run and a zone identifier variable 
#'   added to \code{dat}.

find_fishing_centroid <- function(dat,
                                  project,
                                  zoneID,
                                  weight.var = NULL,
                                  lon.dat,
                                  lat.dat,
                                  names = NULL,
                                  cent.name = NULL,
                                  log.fun = TRUE) {
  
  # Call in datasets
  out <- data_pull(dat, project)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main", project)
  
  column_check(dataset, c(zoneID, lon.dat, lat.dat, weight.var))
  
  if (is_value_empty(names)) {
    
    if (is_value_empty(weight.var)) names <- c("weight_cent_lon", "weight_cent_lat")
    else                            names <- c("fish_cent_lon", "fish_cent_lat")
  }
  
  if (any(names %in% colnames(dataset))) {
    
    stop("'names' must be unique.", call. = FALSE)
  }
  
  # TODO: check this
  # project <- find_project(dat, project)

  
# Call assignment column function if zonal assignment does not exist
 # TODO: consider requiring that zonal assignment col exists before running function.
 # The only purpose for including assignment_column() here is for convenience, drawback is
 # it doesn't allow user to set other args like bufferval, closest.pt, and name.
  
  # Weighted centroid
  
  if (!is_value_empty(weight.var)) {
    # weighted fishing centroid
    dataset[[names[1]]] <- 
      stats::ave(dataset[c(lon.dat, weight.var)], dataset[[zoneID]],
                 FUN = function(x) {
                   stats::weighted.mean(x[[lon.dat]], x[[weight.var]])
                 })[[1]]
    
    dataset[[names[2]]] <- 
      stats::ave(dataset[c(lat.dat, weight.var)], dataset[[zoneID]],
                 FUN = function(x) {
                   stats::weighted.mean(x[[lat.dat]], x[[weight.var]])
                 })[[1]]
  }
  
  #Fishing centroid
  dataset[[names[1]]] <- 
    stats::ave(dataset[c(lon.dat)], dataset[[zoneID]],
               FUN = function(x) stats::ave(x[[lon.dat]]))[[1]]
  
  dataset[[names[2]]] <- 
    stats::ave(dataset[c(lat.dat)], dataset[[zoneID]],
               FUN = function(x) stats::ave(x[[lat.dat]]))[[1]]
  
  # unique centroid rows 
  cent_tab <- unique(dataset[c(zoneID, names)])
  cent_tab <- stats::setNames(cent_tab, c("ZoneID", "cent.lon", "cent.lat"))
  cent_tab <- cent_tab[order(cent_tab$ZoneID), ]
  
  # save fishing centroid to FSDB
  suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), 
                                                locdatabase(project = project)))
  on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
  
  cent_name <- paste0(project, cent.name, "FishCentroid")
  
  DBI::dbWriteTable(fishset_db, cent_name, cent_tab, overwrite = TRUE)
  
  message('Fishing centroid saved to fishset database')

  if (log.fun) {
    
    fish_centroid_function <- list()
    fish_centroid_function$functionID <- "find_fishing_centroid"
    fish_centroid_function$args <- list(dat, project, zoneID, weight.var, 
                                        lon.dat, lat.dat, names, log.fun)
    log_call(project, fish_centroid_function)
  }
   
  return(dataset)
}
