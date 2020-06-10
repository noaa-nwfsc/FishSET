#' Examine the number of observations by year, month, and zone in table format
#' 
#' @param dat Main data frame containing data on hauls or trips. Table in FishSET database should contain the string `MainDataTable`.
#' @param project Name of project
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
#' temp_obs_table('pollockMainDataTable', gridfile=map2, x='DATE_FISHING_BEGAN', 
#'                 lon.dat='LonLat_START_LON', lat.dat='LonLat_START_LAT', cat='NMFS_AREA',
#'                 lon.grid='', lat.grid='')
#' }
 
temp_obs_table <- function(dat, project, gridfile, x, lon.dat, lat.dat, cat, lon.grid=NULL, lat.grid=NULL) {
    
    # Call in datasets
    out <- data_pull(dat)
    dat <- out$dat
    dataset <- out$dataset
    
    out <- assignment_column(dataset, gridfile = gridfile, lon.grid = long.grid, lat.grid = lat.dat, lon.dat = lon.dat, lat.dat = lat.dat, cat = cat, 
        closest.pt = FALSE, hull.polygon = TRUE, epsg = NULL)
    
    out$YEAR <- temporal_mod(dataset, x, "year")
    out$MONTH <- temporal_mod(dataset, x, "%m")
    
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
    log_call(temp_obs_table_function)
    
    
    save_table(out, project, "temp_obs_table")
    
}
