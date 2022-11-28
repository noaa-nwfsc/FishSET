#' Create Centroid Table
#' 
#' Create a zonal or fishing centroid table. The centroid can be joined with the 
#' primary data if `output = "dataset"`. The centroid table is automatically 
#' saved to the FishSET Database. 
#' 
#' @param spat Spatial data containing information on fishery management or 
#'   regulatory zones. Required for `type = "zonal centroid"`, not required 
#'   for `type = "fishing centroid"`. `spat` will be included in centroid
#'   table name. 
#' @param dat Primary data containing information on hauls or trips. Table in 
#'   FishSET database contains the string 'MainDataTable'. `dat` is not 
#'   required if `type = "zonal centroid"` and `output = "centroid table"`.
#' @param project Name of project.
#' @param spatID Variable or list in `spat` that identifies the individual areas 
#'   or zones. If `spat` is class sf, `spatID` should be name of list 
#'   containing information on zones. Ignored if `type = "fishing centroid"`.
#' @param zoneID Variable in `dat` that identifies zonal assignments. 
#'   `zoneID` is not required if `type = "zonal centroid"` and 
#'   `output = "centroid table"`.
#' @param lon.dat Longitude variable in `dat`. Required for 
#'   `type = "fishing centroid"`.
#' @param lat.dat Latitude variable in `dat`. Required for 
#'   `type = "fishing centroid"`.
#' @param weight.var Variable from `dat` for weighted average 
#'   (for `type = "fishing centroid"`. only). If `weight.var` is defined, 
#'   the centroid is defined by the latitude and longitude of fishing locations 
#'   in each zone weighted by `weight.var`.
#' @param type The type of centroid to create. Options include `"zonal centroid"`
#'   and `"fishing centroid"`. See other arguments for `type` requirements.
#' @param names Character vector of length two containing the names of the fishing 
#'   centroid columns. The order should be `c("lon_name", "lat_name")`. The 
#'   default names are `c("weight_cent_lon", "weight_cent_lat")` for weighted 
#'   fishing centroid and `c("fish_cent_lon", "fish_cent_lat")` for unweighted 
#'   fishing centroid.
#' @param cent.name A string to include in the centroid table name. Table names 
#'   take the form of `"projectNameZoneCentroid"` for zonal centroids and 
#'   `"projectNameFishCentroid"` for fishing centroids.
#' @param output Options are `"centroid table"`, `"dataset"`, or `"both"`. 
#'   `"centroid table"` returns a table containing the zone name and the 
#'   longitude and latitude of the centroid. `"dataset"` returns the primary 
#'   table joined with the centroid table. `"both"` returns a list containing 
#'   the merged primary table and the centroid table.  
#' @export
#' @md
#' @importFrom stats setNames
#' @importFrom dplyr left_join
#' @importFrom DBI dbConnect dbDisconnect dbWriteTable
#' @importFrom RSQLite SQLite 

create_centroid <- function(spat = NULL,
                            dat = NULL,
                            project,
                            spatID = NULL,
                            zoneID = NULL,
                            lon.dat = NULL,
                            lat.dat = NULL, 
                            weight.var = NULL,
                            type = "zonal centroid", 
                            names = NULL,
                            cent.name = NULL,
                            output = "dataset") {

  #call in data
  spat_out <- data_pull(spat, project)
  spatdat <- spat_out$dataset
  spat <- parse_data_name(spat, "spat", project)
  
  out <- data_pull(dat, project)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main", project)
  
  # check for invalid args
  if (is_value_empty(dataset) & output %in% c("dataset", "both")) {
    
    stop("'dat' required if output = 'dataset'.", call. = FALSE)
  }
  
  if (type == "zonal centroid") {
    
    if (is_value_empty(spatdat)) {
      
      stop("'spat' required for zonal centroid.", call. = FALSE)
    }
    
    # centroid table saved automatically 
    cent_tab <- find_centroid(spat = spatdat, project = project, spatID = spatID,
                              log.fun = FALSE, cent.name = cent.name) # add ... for spat.lat/lon

    if (output == "dataset") {
      
      if (is_value_empty(dat)) stop("Argument 'dat' required.", call. = FALSE)
      
      stopifnot(
        "'dat' is missing" = !is_value_empty(dat),
        "'zoneID' is missing" = !is_value_empty(zoneID)
      )
      
      by_join <- stats::setNames(spatID, zoneID)
      
      dat_out <- dplyr::left_join(dataset, cent_tab, by = by_join)
    }
      
    # } else {
    #   
    #   stop("Invalid output, check spelling. The options are 'dataset', or ", 
    #        "'centroid table'.", call. = FALSE)
    # }
    
  } else if (type == "fishing centroid") {
    
    stopifnot(
      "'dat' is missing" = !is_value_empty(dat),
      "'zoneID' is missing" = !is_value_empty(zoneID),
      "'lon.dat' is missing" = !is_value_empty(lon.dat),
      "'lat.dat' is missing" = !is_value_empty(lat.dat)
    )
    
    if (is_value_empty(names)) {
      
      if (!is_value_empty(weight.var)) names <- c("weight_cent_lon", "weight_cent_lat")
      else                             names <- c("fish_cent_lon", "fish_cent_lat")
    }
    
    dat_out <- find_fishing_centroid(dat = dataset, project = project, zoneID = zoneID, 
                                     lon.dat = lon.dat, lat.dat = lat.dat, 
                                     weight.var = weight.var, names = names, 
                                     cent.name = cent.name, log.fun = FALSE)
    
    if (output %in% c("centroid table", "both")) {
      
      cent_tab <- unique(dat_out[c(zoneID, names)])
      cent_tab <- cent_tab[order(cent_tab[[zoneID]]), ]
      cent_tab <- stats::setNames(cent_tab, c("ZoneID", "cent.lon", "cent.lat"))
    }
    
  } else {
    
    stop("Invalid type, check spelling. The options are 'zonal centroid', or ", 
         "'fishing centroid'.", call. = FALSE)
  }
  
  # Log function
  create_centroid_function <- list()
  create_centroid_function$functionID <- "create_centroid"
  create_centroid_function$args <- list(spat, dat, project, spatID, zoneID,
                                        lon.dat, lat.dat, type, weight.var, 
                                        names, cent.name, output)
  log_call(project, create_centroid_function)
  
  
  if (output == "centroid table") cent_tab
  else if (output == "dataset") dat_out
  else if (output == "both") list(dat = dat_out, cent_tab = cent_tab)
  
}