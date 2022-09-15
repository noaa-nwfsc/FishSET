#' Save Primary Table's Centroid Columns to FishSET Database
#' 
#' Save the unique centroid values from the primary table to the FishSET Database. 
#' Use this function if zone ID and centroid longitude/latitude are included in
#' the primary table.
#' 
#' In certain cases, the user may have the necessary spatial variables to run a 
#' discrete choice model included in the primary table when uploaded to FishSET, 
#' and does not need a spatial table to assign observations to zones or find centroids 
#' (e.g. by using [create_centroid()]). However, a centroid table table must be 
#' saved to the FishSET Database if a centroid option is used to define 
#' alternative choice (see [create_alternative_choice()]). `cent_to_fsdb()` 
#' allows users to save a zonal or fishing centroid table provided they have 
#' the required variables: a zone ID (`zoneID`), a centroid longitude (`cent.lon`), 
#' and a centroid latitude (`cent.lat`) column.
#' 
#' @param dat Required, main data frame containing data on hauls or trips.
#'   Table in FishSET database should contain the string `MainDataTable`.
#' @param spat Optional, a name to associate with the centroid table. 
#' @param project Name of project.
#' @param zoneID Variable in `dat` that identifies the individual zones or 
#'   areas. 
#' @param cent.lon Required, variable in `dat` that identifies the centroid 
#'   longitude of zones or areas. 
#' @param cent.lat Required, variable in `dat` that identifies the centroid 
#'   latitude of  zones or areas. 
#' @param type The type of centroid. Options include `"zone"` for zonal centroids
#'   and `fish` for fishing centroids. 
#' @export
#' @importFrom RSQLite SQLite
#' @importFrom DBI dbConnect dbDisconnect dbWriteTable
#' @md

centroid_to_fsdb <- function(dat,
                             spat = NULL,
                             project,
                             zoneID,
                             cent.lon,
                             cent.lat,
                             type = "zone") {
  
  
  # TODO this can be extended to port data as well, add third type option
  # and change function name to something more generic
  
  out <- data_pull(dat, project)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main", project)
  
  spat_out <- data_pull(spat, project)
  spatdat <- spat_out$dataset
  spat <- parse_data_name(spat, "spat", project)
  
  column_check(dataset, cols = c(zoneID, cent.lon, cent.lat))
  
  cent_tab <- unique(dataset[, c(zoneID, cent.lon, cent.lat)])
  
  # save centroid table to FSDB
  
  if (type == "zone") {
    
    z_name <- ifelse(!is_value_empty(spat), spat, project)
    
    cent_name <- paste0(z_name, "ZoneCentroid")
    
  } else if (type == "fish") {
    
    f_name <- ifelse(!is_value_empty(spat), spat, project)
    
    cent_name <- paste0(f_name, "FishCentroid")
  }
  
  suppressWarnings(fishset_db <- DBI::dbConnect(RSQLite::SQLite(), 
                                                locdatabase(project = project)))
  on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
  
  DBI::dbWriteTable(fishset_db, cent_name, cent_tab, overwrite = TRUE)
}