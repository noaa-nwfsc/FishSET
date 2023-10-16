#' Design out-of-sample model
#' 
#' @description Use selected model design settings to create a model design for the out-of-sample data
#' 
#' @param project Name of project
#' @param mod.name Name of saved model to use. Argument can be the name of the model or can pull the name 
#'   of the saved "best" model. Leave \code{mod.name} empty to use the saved "best" model. If more than
#'   one model is saved, \code{mod.name} should be the numeric indicator of which model to use.
#'   Use \code{table_view("modelChosen", project)} to view a table of saved models.
#' @param use.scalers Input for \code{create_model_input()}. Logical, should data be normalized? Defaults to \code{FALSE}. Rescaling factors are the mean of the 
#' numeric vector unless specified with \code{scaler.func}.
#' @param scaler.func Input for \code{create_model_input()}. Function to calculate rescaling factors.
#' 
#' @export

model_design_outsample <- function(project, mod.name, use.scalers = FALSE, scaler.func = NULL){
  
  # Load outsample data -----------------------------------------------------------------------------------------------------------------------------
  flag <- 0
  tryCatch(
    {suppressWarnings(outsample_dat <- readRDS(paste0(locoutput(project), project, "filtered_outsample.rds")))},
    error = function(e) {flag <<- 1}
  )
  
  if(flag == 1){
    stop('A filtered out-of-sample dataset is required for model design. First run filter_outsample().')
  }
  

  # Get metadata for selected model -----------------------------------------------------------------------------------------------------------------
  # Pull model design for all saved models
  if (table_exists(paste0(project, "ModelInputData"), project)) {
    mdf <- model_design_list(project)
  } else {
    stop('Model input table does not exist.', call. = FALSE)
  }
  
  # Get model names
  mdf_n <- model_names(project)
  
  # Get only info for selected model
  tryCatch(
    {mdf <- mdf[[which(mdf_n == mod.name)]]},
    error = function(e) {flag <<- 1}
  )

  if(flag == 1){
    stop('Model not found.')
  }
  
  
  # Get info on expected catch/revenue --------------------------------------------------------------------------------------------------------------
  if(length(mdf$expectcatchmodels) > 0){
    e_list <- unlist(mdf$expectcatchmodels)
    e_settings <- expected_catch_list(project)[paste0(e_list,"_settings")]
    
  } else {
    e_settings <- NULL
    
  }
  
  
  # Create alternative choice for out-of-sample data ------------------------------------------------------------------------------------------------
  # Note that this assumes the spatial data file contains all zones (in-sample and out-of-sample)
  # Read main alt choice matrix and extract settings
  tmpAlt <- unserialize_table(paste0(project,"AltMatrix"), project)
  
  # # Create out-of-sample alternative choice matrix
  # create_alternative_choice(dat, project, occasion = tmpAlt$occasion, occasion_var = tmpAlt$occasion_var,
  #                           alt_var = tmpAlt$alt_var, dist.unit = tmpAlt$altChoiceUnits, min.haul = 120, zoneID = tmpAlt$zoneID,
  #                           zone.cent.name = tmpAlt$zone_cent_name, fish.cent.name = tmpAlt$fish_cent_name,
  #                           spat = tmpAlt$spat, spatID = tmpAlt$spatID)

  
  
}
