
# map viewer
map_viewer <- function(dat, gridfile, avd, avm, num_vars, temp_vars, id_vars, lon_start, lat_start, lon_end, lat_end) {
    #' Interactive vessel locations and fishery zones map
    #' View vessel locations and fishery zones on interactive map
    #' @param dat Primary data containing information on hauls or trips. Table in FishSET database contains the string 'MainDataTable'.
    #' @param gridfile Spatial data containing information on fishery management or regulatory zones. Shape, json, geojson, and csv formats are supported. 
    #' @param avd Variable name in \code{dat} that gives the unique ID associated to the polygon.
    #' @param avm The name of the property in the GeoJson file that identifies the polygon to cross reference to \code{dat}. Often a list of zones.
    #' @param num_vars List, name of numeric variable(s) in \code{dat} to include for plotting.
    #' @param temp_vars List, name of temporal variable(s) in \code{dat} to include for plotting.
    #' @param id_vars List, name of categorical variable(s) in \code{dat} to group by. 
    #' @param lon_start String, variable in \code{dat} that identifies starting longitude decimal degrees.
    #' @param lat_start String, variable in \code{dat} that identifies staring latitude decimal degrees.
    #' @param lon_end String, variable in \code{dat} that identifies endng longitude decimal degrees.
    #' @param lat_end String, variable in \code{dat} that identifies endng latitude decimal degrees.
    #' @importFrom geojsonio geojson_write
    #' @importFrom jsonlite toJSON 
    #' @importFrom servr httd
    #' @importFrom utils browseURL
    #' @details The map_viewer function creates the files required to run the MapViewer program. 
    #' After creating the inputs, a map with zones is opened in the default web browser. To close 
    #'the server connection run \code{servr::daemon_stop()} in the console. 
    #' Lines on the map represent the starting 
    #' and ending lat/long for each observation in the data set color coded based on the selected variable.
    #' It can take up to a minute for the data to be loaded onto the map.
    #' At this time, the map can only be saved by taking a screen shot.
    #' @examples 
    #' \dontrun{
    #' map_viewer(dat='pollockMainDataTable', gridfile='spatdat', area_variable_column='NMFS_AREA', 
    #' area_variable_map='NMFS_AREA', num_vars=c('HAUL','OFFICIAL_TOTAL_CATCH'), 
    #' temp_vars='HAUL_DATE', id_vars=c('GEAR_TYPE', 'PORT'))
    #' }
    
    
    out <- data_pull(dat)
    dat <- out$dat
    dataset <- out$dataset
    
    out <- data_pull(gridfile)
    spatname <- out$dat
    spatdat <- out$dataset
    
    if (!is.null(spatdat)) {
        geojsonio::geojson_write(spatdat, file = paste0(loc_map(), "spatdat.geojson"), overwrite = TRUE)
    }
    
    # 2 Create data file
    
    # remove NAs from lat and lon
    dataset <- dataset[!is.na(dataset[[lon_start]]) | !is.na(dataset[[lat_start]]) | !is.na(dataset[[lon_end]]) | !is.na(dataset[[lat_end]]), ]
    
    
    dataset <- unique(dataset[, c(avd, num_vars, temp_vars, id_vars, lon_start, lat_start, lon_end, lat_end)])
    dataset$uniqueID <- 1:nrow(dataset)
    
    write.csv(dat, paste0(loc_map(), "/datafile.csv"))
    
    # 3. Create map config
    
    map_config <- list()
    map_config$mapbox_token <- "pk.eyJ1IjoibWhhcnNjaDEyNSIsImEiOiJjazZ2NXh6dXUwZ25vM25wYXJ6bHFpOGc1In0.nAdb9QQybVd9ESgtr0fjZg"
    map_config$choosen_scatter <- num_vars[1]
    map_config$area_variable_column <- avd
    map_config$area_variable_map <- avm
    map_config$numeric_vars <- num_vars
    map_config$temporal_vars <- temp_vars
    map_config$id_vars <- id_vars
    map_config$longitude_start <- lon_start
    map_config$latitude_start <- lat_start
    map_config$longitude_end <- lon_end
    map_config$latitude_end <- lat_end
    map_config$uniqueID <- "uniqueID"
    map_config$grid_file <- "spatdat.geojson"
    
    
    write(jsonlite::toJSON(map_config, pretty = TRUE, auto_unbox = TRUE), paste0(loc_map(), "map_config.json"))
    
    
    # log function
    map_viewer_function <- list()
    map_viewer_function$functionID <- "map_viewer"
    map_viewer_function$args <- list(dat, spatname, avd, avm, num_vars, temp_vars, id_vars, lon_start, lat_start, lon_end, lat_end)
    log_call(map_viewer_function)
    
    # working directory
    current_wd <- getwd()
    
    map_wd <- paste0(getwd(), "/inst/MapViewer")
    
    setwd(map_wd)
    
    utils::browseURL(servr::httd(browser = FALSE)$url)
    
    on.exit(setwd(current_wd))
    
}
