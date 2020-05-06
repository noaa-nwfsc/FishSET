map_viewer <- function(dat, project, cs, avc, avm, num_var, temp_var, id_var, 
                       lon_start, lat_start, lon_end, lat_end, row_id, grid = NULL){
  #'
  #' Open interactive map in browser
  #' 
  #' @param dat Main data frame over which to apply function. Table in FishSET 
  #'   database should contain the string `MainDataTable`.
  #' @param cs Variable name in \code{dat} that is being plotted as scatter.
  #' @param avc Variable name in \code{dat} that gives the unique ID associated to the polygon.
  #' @param avm The name of the property in the GeoJson file that identifies the 
  #'   polygon to cross reference to \code{dat}.
  #' @param num_var A list of numeric variable names in \code{dat} to plot. 
  #' @param temp_var A list of temporal variable names in \code{dat} to plot.
  #' @param id_var A list of categorical variables names in \code{dat} to group by.
  #' @param lon_start Variable name in \code{dat} that identifies the 
  #'   starting longitude ( decimal degrees) of the line to plot.
  #' @param lat_start Variable name in \code{dat} that identifies the 
  #'   starting latitude ( decimal degrees) of the line to plot.
  #' @param lon_end Variable name in \code{dat}that identifies the ending 
  #'   longitude ( decimal degrees) of the line to plot.
  #' @param lat_end Variable name in \code{dat} that identifies the ending 
  #'   latitude ( decimal degrees) of the line to plot.
  #' @param row_id  Variable name in \code{dat} that identifies unique observations.
  #' @param grid The filename that contains the geojson grid file
  #' @importFrom dplyr distinct
  #' @importFrom geojsonio geojson_write
  #' @importFrom servr httd
  #' @importFrom utils browseURL
  #' @export map_viewer
  #' @details \code{map_viewer()} saves a CSV file, a map configuration file, and a 
  #'   grid file to the MapViewer folder and opens the map in a browser. To close 
  #'   the server connection run \code{servr::daemon_stop()} in the console. 
  #' @examples 
  #' \dontrun{
  #' map_viewer("pollockMainDataTable", c("OFFICIAL_TOTAL_CATCH_MT", "FT_CFEC_VALUE_DOLLARS"), 
  #' "ZoneID", "ZoneID", c("OFFICIAL_TOTAL_CATCH_MT", "FT_CFEC_VALUE_DOLLARS"),
  #' c("HAUL_DATE","FISHING_START_DATE"), c("PORT_CODE", 'GEAR_TYPE'),
  #' "LonLat_START_LON", "LonLat_START_LAT", "LonLat_END_LON", "LonLat_END_LAT",
  #' "row_id")
  #' }
  
  #Call in datasets
  out <- data_pull(dat)
  dat <- out$dat
  dataset <- out$dataset
  
  
  x <- 0
  
  if (is.null(grid) & !file.exists(paste0(getwd(),"/inst/MapViewer/grid_file.geojson"))) {
    
    warning("A grid object must be entered or a file named 'grid_file.geojson' must exist in Map Viewer folder.")
    x <- 1
  }
  
  if (x == 0) {
    
    # only use necessary columns
    dataset <- dataset[c(cs, avc, num_var, temp_var, id_var, lon_start, lat_start, lon_end, lat_end, row_id)]
    
    dataset <- dplyr::distinct(dataset)  
    
    # remove NAs from lat and lon
    dataset <- dataset[!is.na(dataset[[lon_start]]) | !is.na(dataset[[lat_start]]) | 
                         !is.na(dataset[[lon_end]]) | !is.na(dataset[[lat_end]]), ]
    
    
    # write map_config.json file     
    map_config(cs, avc, avm, num_var, temp_var, id_var, lon_start, lat_start,
               lon_end, lat_end, row_id)
    
    # write datafile.csv
    write.csv(dataset, file = "./inst/MapViewer/datafile.csv")
    
    
    if (!is.null(grid)) {    
      # write geojson grid file 
      geojsonio::geojson_write(grid, file = "./inst/MapViewer/grid_file.geojson", overwrite = T)
    } 
    
    # log function
    map_viewer_function <- list()
    map_viewer_function$functionID <- "map_viewer"
    map_viewer_function$args <- c(dat, project, cs, avc, avm, num_var, temp_var, id_var, lon_start, lat_start,
                                  lon_end, lat_end, row_id, grid)
    log_call(map_viewer_function)
    
    
    current_wd <- getwd()
    
    map_wd <- paste0(getwd(), "/inst/MapViewer")
    
    setwd(map_wd)
    
    utils::browseURL(servr::httd(browser = FALSE)$url)
    
    on.exit(setwd(current_wd))
  }
}



map_config <- function(cs, avc, avm, num_var, temp_var, id_var, lon_start, lat_start,
                       lon_end, lat_end, row_id){
  #' 
  #' Formats and creates the map_config.json file for map_viewer()
  #' 
  #' @param cs Variable name in \code{dat} that is being plotted as scatter.
  #' @param avc Variable name in \code{dat} that gives the unique ID associated to the polygon.
  #' @param avm The name of the property in the GeoJson file that identifies the 
  #'   polygon to cross reference to \code{dat}.
  #' @param num_var A list of numeric variable names in \code{dat} to plot. 
  #' @param temp_var A list of temporal variable names in \code{dat} to plot.
  #' @param id_var A list of categorical variables names in \code{dat} to group by.
  #' @param lon_start Variable name in \code{dat} that identifies the 
  #'   starting longitude ( decimal degrees) of the line to plot.
  #' @param lat_start Variable name in \code{dat} that identifies the 
  #'   starting latitude ( decimal degrees) of the line to plot.
  #' @param lon_end Variable name in \code{dat}that identifies the ending 
  #'   longitude ( decimal degrees) of the line to plot.
  #' @param lat_end Variable name in \code{dat} that identifies the ending 
  #'   latitude ( decimal degrees) of the line to plot.
  #' @param row_id  Variable name in \code{dat} that identifies unique observations.
  #'@export
  #'@importFrom jsonlite toJSON
  #'
  #'
  
  
  if (length(num_var) == 1) {
    
    num_var <- list(num_var) # this variable needs to be wrapped in brackets to work
  }
  
  map_list <- list("mapbox_token" = "pk.eyJ1IjoibWhhcnNjaDEyNSIsImEiOiJjazZ2NXh6dXUwZ25vM25wYXJ6bHFpOGc1In0.nAdb9QQybVd9ESgtr0fjZg", 
                   "choosen_scatter" = cs,
                   "area_variable_column" = avc, 
                   "area_variable_map" = avm,
                   "numeric_vars" = num_var,
                   "temporal_vars" = temp_var, 
                   "id_vars" = id_var,
                   "longitude_start" = lon_start, 
                   "latitude_start" = lat_start,
                   "longitude_end" = lon_end, 
                   "latitude_end" = lat_end,
                   "uniqueID" = row_id, 
                   "grid_file" = "grid_file.geojson")
  
  
  map_list <- jsonlite::toJSON(map_list, pretty = TRUE, auto_unbox = TRUE)
  
  
  fileCon <- file("./inst/MapViewer/map_config.json")
  writeLines(map_list, fileCon)
  close(fileCon)
}