#' Table of observations by year, month, and zone
#' 
#' @param dat Main data frame containing data on hauls or trips. Table in fishset_db database should contain the string `MainDataTable`.
#' @param x Column in data containing date variable
#' @param gridfile Spatial data. Shape, json, and csv formats are supported.
#' @param lon.dat Column containing longitude data in main data frame.
#' @param lat.dat Column containing latitude data in main data frame.
#' @param lon.grid Column containing longitude data in gridfile.
#' @param lat.grid lColumn containing latitude data in gridfile.
#' @param cat  Column in gridfile that identifies the individual areas or zones. If gridfile is class sf, `cat` should be name of list containing information on zones. 
#' @importFrom sp CRS Polygons Polygon SpatialPolygons SpatialPolygonsDataFrame coordinates
#' @importFrom rgeos gDistance
#' @importFrom grDevices chull
#' @importFrom raster projection
#' @details Prints tables displaying the number of observations by year, month, and zone. The assignment_column function is called to assign observations to zones. 
#' Use this function before calling the create_expectations function to inform choices in appropriate window size for analysis. 
#' @export
#' @examples 
#' \dontrun{
#' temp_obs_table(MainDataTable, map2, "DATE_FISHING_BEGAN", "", "", 
#'              "LONLAT_START_LONGITUDE", "LONLAT_START_LATITUDE", "NMFS_AREA")
#' }

temp_obs_table <- function(dat, gridfile, x, lon.grid, lat.grid, lon.dat, lat.dat, cat){
  
  #Call in datasets
  fishset_db <- DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite")
  if(is.character(dat)==TRUE){
    if(is.null(dat)==TRUE | table_exists(dat)==FALSE){
      print(DBI::dbListTables(fishset_db))
      stop(paste(dat, 'not defined or does not exist. Consider using one of the tables listed above that exist in the database.'))
    } else {
      dataset <- table_view(dat)
    }
  } else {
    dataset <- dat  
  }
  DBI::dbDisconnect(fishset_db)
  
 out <- assignment_column(dataset, gridfile, hull.polygon = TRUE, lon.grid, lat.grid, 
                          lon.dat, lat.dat, cat, closest.pt =FALSE, epsg=NULL)
 
 out$YEAR <- temporal_mod(dataset, x, 'year') 
 out$MONTH <- temporal_mod(dataset, x, '%m') 
 
 cat('Number of observations by year') 
 print(table(out$YEAR))
  cat('Number of observations by year and Zone') 
 print(table(out$YEAR, out$ZoneID))
cat('Number of observations by year and month') 
 print(table(out$YEAR, out$MONTH))
 cat('Number of observations by year and month split by Zone') 
 for(i in 1:length(unique(out$ZoneID))){
   cat('Zone', unique(out$ZoneID)[i])
 print(table(out$YEAR[out$ZoneID==unique(out$ZoneID)[i]], out$MONTH[out$ZoneID==unique(out$ZoneID)[i]]))
 }
 
}
