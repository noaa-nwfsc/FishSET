#'  Create fishing or weighted fishing centroid

#' @param dat  Primary data containing information on hauls or trips. Table in FishSET database contains the string 'MainDataTable'.
#' @param project Name of project
#' @param gridfile Spatial data containing information on fishery management or regulatory zones. Can be shape file, json, geojson, data frame, or list.
#'   \code{gridfile} should be NULL if zone identifier variable exists in \code{dat}.
#' @param cat Variable in \code{dat} that identifies zonal assignments or the variable in \code{gridfile} that identifies the individual areas or zones. 
#'    If \code{gridfile} is class sf, \code{cat} should be name of list containing information on zones.
#' @param weight.var Variable from \code{dat} for weighted average. If \code{weight.var} is defined, the centroid is defined by the latitude and 
#'    longitude of fishing locations in each zone weighted by \code{weight.var}.
#' @param lon.dat Required. Longitude variable in \code{dat}.  
#' @param lat.dat Required. Latitude variable in \code{dat}.
#' @param lon.grid Variable or list from \code{gridfile} containing longitude data. Required if \code{gridfile} is a csv file. 
#'   Leave as NULL if zone identifier exists in \code{dat} or \code{gridfile} is a shape or json file.
#' @param lat.grid Variable or list from \code{gridfile} containing latitude data. Required if \code{gridfile} is a csv file. 
#'   Leave as NULL if  zone identifier exists in \code{dat} \code{gridfile} is a shape or json file.
#' @keywords centroid, zone
#' @importFrom stats ave weighted.mean
#' @importFrom methods as
#' @return Returns primary dataset with fishing centroid and, if \code{weight.var} is specified, the weighted fishing centroid. 
#' @export find_fishing_centroid
#' @details Fishing centroid defines the centroids by mean latitude and longitude of fishing locations in each zone. 
#'     Weighted centroid defines the centroids by the mean latitude and longitude of fishing locations in each zone weighted by the \code{weight.var}.
#'     The fishing and weighted centroid variables can be used anywhere latitude/longitude variables appear.
#'     Each observation in \code{dat} must be assigned to a fishery or regulatory area/zone. 
#'     If the zone identifier exists in \code{dat} and is not called \code{'ZoneID'}, then \code{cat} 
#'     should be the variable name containing the zone identifier.

#'     If a zone identifier variable does not exist in \code{dat},   \code{gridfile} must be be specified and \code{cat} must be zone 
#'     identifier in \code{gridfile}. The \code{assignment_column} function will be run and a zone identifier variable added to \code{dat}.


find_fishing_centroid <- function(dat, project=NULL, cat='ZoneID', weight.var = NULL, lon.dat = NULL, 
                           lat.dat = NULL, gridfile, lon.grid = NULL, lat.grid = NULL) {
  
  # Call in datasets
  out <- data_pull(dat, project)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main")
  
  gridname <- deparse(substitute(gridfile))
  
 project <- find_project(dat, project)
  
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  cat("", file = tmp, append = TRUE)
  x <- 0
  
  
  if(is.null(cat)){
    warning('cat argument must be defined')
    x <- 1
  }
  
  if(x == 0){
# Call assignment column function if zonal assignment does not exist
 if(!cat %in% names(dataset)){
   dataset <- assignment_column(dat=dataset, project=project, gridfile=gridfile, lon.dat=lon.dat, lat.dat=lat.dat, cat=cat, 
                     closest.pt = FALSE, bufferval = NULL, lon.grid = lon.grid, lat.grid = lat.grid)
   cat <- 'ZoneID'
 }

  
  # Weighted centroid
    dataset$weight_cent_lon <- stats::ave(dataset[c(lon.dat, weight.var)], dataset[[cat]],
                                   FUN = function(x) stats::weighted.mean(x[[lon.dat]], x[[weight.var]])
        )[[1]]
    dataset$weight_cent_lat <- stats::ave(dataset[c(lat.dat, weight.var)], dataset[[cat]],
                                   FUN = function(x) stats::weighted.mean(x[[lat.dat]], x[[weight.var]])
        )[[1]]


    #Fishing centroid
    dataset$fish_cent_lon <- stats::ave(dataset[c(lon.dat)], dataset[[cat]],
                                       FUN = function(x) stats::ave(x[[lon.dat]]))[[1]]
    dataset$fish_cent_lat <- stats::ave(dataset[c(lat.dat)], dataset[[cat]],
                                       FUN = function(x) stats::ave(x[[lat.dat]]))[[1]]
 
    fish_centroid_function <- list()
    fish_centroid_function$functionID <- "find_fishing_centroid"
    fish_centroid_function$args <- list(dat, project, cat, weight.var, lon.dat, lat.dat, 
                                         gridfile, lon.grid, lat.grid)
    log_call(project, fish_centroid_function)

  return(dataset)
  }
}
