#' Filter out-of-sample data for model predictions
#' 
#' Filter the out-of-sample dataset and prepare for predictions of fishing probability.
#'
#' @param dat Out-of-sample data
#' @param project Name of project
#' @param mod.name Name of saved model to use. Argument can be the name of the model or can pull the name 
#'   of the saved "best" model. Leave \code{mod.name} empty to use the saved "best" model. If more than
#'   one model is saved, \code{mod.name} should be the numeric indicator of which model to use.
#'   Use \code{table_view("modelChosen", project)} to view a table of saved models.
#' @param spatial_outsample Logical, indicate whether the data are out-of-sample spatially or not. 
#'   Note that models with zone-specific coefficients (e.g., zonal logit) cannot be used to predict data that are 
#'   out-of-sample spatially. \code{spatial_outsample = FALSE} can represent data out-of-sample temporally or out-of-sample 
#'   based on another variable (e.g., vessel tonnage, gear type, etc.)
#' @param zone.dat Variable in \code{dat}that identifies the individual areas or zones.
#' @param spat Required, data file or character. 
#'   \code{spat} is a spatial data file containing information on fishery 
#'   management or regulatory zones boundaries. Shape, json, geojson, and csv 
#'   formats are supported. geojson is the preferred format. json files must be 
#'   converted into geoson. This is done automatically when the file is loaded 
#'   with \code{\link{read_dat}} with \code{is.map} set to true. \code{spat} 
#'   cannot, at this time, be loaded from the FishSET database. \cr
#' @param zone.spat Variable in \code{spat} that identifies the individual areas or zones.
#' @param outsample_zones Vector of out-of-sample zones to filter \code{dat}. Only provided as input when running this function
#'   in the main app.
#' @param lon.spat Required for csv files. Variable or list from \code{spat} 
#'   containing longitude data. Leave as NULL if \code{spat} is a shape or json file.
#' @param lat.spat Required for csv files. Variable or list from \code{spat} 
#'   containing latitude data.  Leave as NULL if \code{spat} is a shape or json file.
#' @param use.scalers Input for \code{create_model_input()}. Logical, should data be normalized? Defaults to \code{FALSE}. Rescaling factors are the mean of the 
#' numeric vector unless specified with \code{scaler.func}.
#' @param scaler.func Input for \code{create_model_input()}. Function to calculate rescaling factors.
#' @return Returns probability of logit model by choice
#' @details This function filters the out-of-sample data. If the data is out-of-sample spatially, then set \code{spatial_outsample = TRUE} and 
#' provide a spatial file (\code{spat}) and the zone id in the spatial file \code{zone.spat}. An interactive map is used for selecting out
#' of sample zones. If the data are not spatially out-of-sample, then just filter the data for the zones included in the selected model. Note that 
#' models with zone-specific coefficients (e.g., zonal logit) cannot predict spatial out-of-sample data. Upon successful execution of 
#' \code{filter_outsample()} the filtered dataset will be saved to an RDS file in the outputs folder. This function will overwrite the existing
#' RDS file each time it is run.
#' @export
#' 

filter_outsample <- function(dat, project, mod.name, spatial_outsample = FALSE, zone.dat = NULL,
                             spat = NULL, zone.spat = NULL, outsample_zones = NULL,
                             lon.spat = NULL, lat.spat = NULL, use.scalers = FALSE, scaler.func = NULL){
  
  # Save inputs for logging the function ------------------------------------------------------------------------------------
  dat_in <- deparse(substitute(dat))
  spat_in <- deparse(substitute(spat))
  
  # Check compatibility -----------------------------------------------------------------------------------------------------
  # Pull model design for all saved models
  if(table_exists(paste0(project, "ModelInputData"), project)){
    mdf <- model_design_list(project)
  } else {
    stop('Model input table does not exist.', call. = FALSE)
  }
  
  # Get saved model names
  mdf_n <- model_names(project)
  
  # Get only info for selected model
  flag <- 0
  tryCatch(
    {mdf <- mdf[[which(mdf_n == mod.name)]]},
    error = function(e) {flag <<- 1}
  )
  
  if(flag == 1){
    stop('Model not found. Enter a valid model name.')
  }
  
  # Check that spatial_outsample option can be used with selected model
  if(spatial_outsample == TRUE & mdf$likelihood %in% c("logit_zonal")){
    stop('Selected model cannot be used to predict spatially out-of-sample data. Set spatial_outsample = FALSE or select a different model.')
  }
  
  
  # Not out-of-sample spatially ---------------------------------------------------------------------------------------------
  # If not, then filter the out-of-sample dataset for zones included in the model
  if(spatial_outsample == FALSE){
    zones <- unique(mdf$choice$choice)
    
    # Filter data by zones that were included in the estimation model
    dat <- dat[which(unlist(dat[zone.dat], use.names = FALSE) %in% zones),]
    
    # Display a message if not all model zones are included in the out-of-sample dataset
    dat_zones <- unlist(unique(dat[zone.dat]), use.names=FALSE)
    nonoverlap_zones <- zones[!(zones %in% dat_zones)]
    
    if(!is_empty(nonoverlap_zones)){
      if(isRunning()){
        showNotification(paste0(length(nonoverlap_zones), " zone(s) from model not present in out-of-sample data."), type = "message")
      } else {
        message(paste0(length(nonoverlap_zones), " zone(s) from model not present in out-of-sample data."))
      }  
    }
    
    # Save filtered data as an RDS file
    filename <- paste0(locoutput(project), project, "filtered_outsample.rds")
    tryCatch(
      {saveRDS(dat, filename)},
      error = function(e) {flag <<- 1}
    )
    
    # Output message on saved status
    if(flag == 0){
      if(isRunning()){
        showNotification("Filtered out-of-sample data saved.", type = "message")
      } else {
        message("Filtered out-of-sample data saved.")
      }  
    } else if (flag == 1){
      if(isRunning()){
        showNotification("Save was unsuccessful. Check filter settings.", type = "error")
      } else {
        message("Error: Save was unsuccessful. Check function inputs.")
      }
    }
    
    # Log function
    save_filteroutsample_function <- list()
    save_filteroutsample_function$functionID <- "filter_outsample"
    save_filteroutsample_function$args <- list(dat_in, project, mod.name, spatial_outsample, zone.dat, spat_in, zone.spat,
                                               outsample_zones, lon.spat, lat.spat, use.scalers, scaler.func)
    log_call(project, save_filteroutsample_function)
    
    return(1)
    
    # Out-of-sample spatially--------------------------------------------------------------------------------------------------
    # If spatially out-of-sample, then have the user select zones for the spatial out-of-sample predictions
    # Note: if running through the console version, a shiny app will open, if running in the main app the out-of-sample
    #       zones will be provided as input
  } else {
    
    # leaflet requires WGS84
    spat <- sf::st_transform(spat, "+proj=longlat +datum=WGS84")
    spat <- check_spatdat(spat, id = cat, lon = lon.spat, lat = lat.spat)
    
    # Save a second location ID to create multiple spatial layers for the interactive map. This allows us to create polygons
    # when a user clicks and remove the polygon when it is clicked again while keeping the base layer grid.
    if(zone.spat %in% names(spat)){
      spat$secondLocationID <- paste("Zone_", as.character(spat[[zone.spat]]), sep="")
      names(spat)[which(names(spat) == zone.spat)] <- zone.dat
      spat[[zone.dat]] <- as.character(spat[[zone.dat]])
      
    } else if (zone.dat %in% names(spat)) {
      spat$secondLocationID <- paste("Zone_", as.character(spat[[zone.spat]]), sep="")
      spat[[zone.dat]] <- as.character(spat[[zone.dat]])
      
    } else {
      stop(paste0("Zone identifier(s) not found in spatial or data table"))
    }
    
    # Get unique zones across model design and out-of-sample data
    zones <- unique(c(mdf$choice$choice, get(dat_in)[[zone.dat]]))
    
    # Filter spat based on zones
    spat <- spat[which(get(spat_in)[[zone.dat]] %in% zones),]
    mod.spat <- spat[which(get(spat_in)[[zone.dat]] %in% unique(mdf$choice$choice)),]
    
    # Change coordinate reference system if not WGS84
    if(sf::st_crs(spat)[[1]] != "WGS84"){
      spat <- sf::st_transform(spat, crs = "WGS84")
    }
    
    if(sf::st_crs(mod.spat)[[1]] != "WGS84"){
      mod.spat <- sf::st_transform(mod.spat, crs = "WGS84")
    }
    
    # Need to log before running shiny
    save_filteroutsample_function <- list()
    save_filteroutsample_function$functionID <- "filter_outsample"
    save_filteroutsample_function$args <- list(dat_in, project, mod.name, spatial_outsample, zone.dat, spat_in, zone.spat,
                                               outsample_zones, lon.spat, lat.spat, use.scalers, scaler.func)
    log_call(project, save_filteroutsample_function)
    
    ## Running in shiny ----
    if(isRunning()){
      
      return(list(spat, mod.spat))
      
    ## Running in console ----
    } else {
      # Load shiny modules to run on the console version
      source("inst/ShinyFiles/MainApp/zone_outsample_UI.R", local = TRUE)
      source("inst/ShinyFiles/MainApp/zone_outsample_server.R", local = TRUE)
      
      # Run a shiny app if using the console version of FishSET
      ui <- fluidPage(
        shinyjs::useShinyjs(),
        
        zone_outsample_mapUI("map1"),
        "Click on one or more polygons to select zones for out-of-sample predictions.",
        "\n Click the 'Save out-of-sample zones' button to save choices.",
        
        fluidRow(
          column(4, zone_outsample_tableUI("table1")),
          column(4, zone_outsample_saveUI("save"), offset = 1),
          column(4, zone_outsample_closeUI("close1"), offset = 1)
        )
      )
      
      server <- function(input, output, session){
        # Reactive values
        clicked_ids <- reactiveValues(ids = vector())
        outsample_table <- reactiveValues(data = NULL)
        filename <- reactiveValues(name = NULL)
        
        zone_outsample_mapServer("map1", clicked_ids, spat, mod.spat, zone.dat)
        zone_outsample_tableServer("table1", clicked_ids, outsample_table)
        zone_outsample_saveServer("save", outsample_table, filename, zone.dat, project, dat)
        zone_outsample_closeServer("close1", outsample_table, filename)
        
      }
      
      shinyApp(ui, server)
    }
    
  }
  
}


