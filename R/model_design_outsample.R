#' Design hold-out model
#' 
#' @description Use selected model design settings to create a model design for hold-out data. The hold-out data can be 
#' out-of-sample data or subsetted data for k-fold cross validation.
#' 
#' @param project Name of project
#' @param mod.name Name of saved model to use. Argument can be the name of the model or can pull the name 
#'   of the saved "best" model. Leave \code{mod.name} empty to use the saved "best" model. If more than
#'   one model is saved, \code{mod.name} should be the numeric indicator of which model to use.
#'   Use \code{table_view("modelChosen", project)} to view a table of saved models.
#' @param outsample.mod.name Name assigned to out-of-sample model design. Must be unique and not already exist in model design list.
#'   If \code{outsample.mod.name = NULL} then a default name will be chosen based on mod.name, which is the default value. 
#' @param CV Logical, Indicates whether the model design is being created for cross validation \code{TRUE}, or for simple out-
#'    of-sample dataset. Defaults to \code{CV = TRUE}.
#' @param CV_dat Training or testing dataset for k-fold cross validation.
#' @param use.scalers Input for \code{create_model_input()}. Logical, should data be normalized? Defaults to \code{FALSE}. Rescaling factors are the mean of the 
#' numeric vector unless specified with \code{scaler.func}.
#' @param scaler.func Input for \code{create_model_input()}. Function to calculate rescaling factors.
#' 
#' @details
#' This function automatically pulls model settings from the selected model and creates an alternative choice matrix, expected catch/revenue matrices, 
#' and model design for a hold-out dataset. The hold-out data set can be an out-of-sample dataset or subset of main data for cross validation.
#' If running out-of-sample data, this function requires that a filtered out-of-sample data file (.rds file) exists in the output folder. For cross
#' validation, this function is called in the \code{cross_validation()} function.
#' Note: the out-of-sample functions only work with a single selected model at a time. To run out-of-sample functions on a new
#' out-of-sample dataset, start with load_outsample() if an entirely new dataset or filter_outsample(). 
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' 
#' # For out-of-sample dataset
#' model_design_outsample("scallop", "scallopModName")
#' 
#' }
 

model_design_outsample <- function(project, mod.name, outsample.mod.name = NULL, CV = FALSE, CV_dat = NULL, use.scalers = FALSE, scaler.func = NULL){
  
  # Load outsample data -----------------------------------------------------------------------------------------------------------------------------
  flag <- 0
  if(!CV){ # Out-of-sample data
    tryCatch(
      {suppressWarnings(outsample_dat <- readRDS(paste0(locoutput(project), project, "filtered_outsample.rds")))},
      error = function(e) {flag <<- 1}
    )
    
    if(flag == 1) stop('A filtered out-of-sample dataset is required for model design. First run filter_outsample().')
    
  } else { # k-fold cross validation data
    # TODO: Not sure if this check is necessary
    if(is.null(CV_dat)) stop('One or more cross validation datasets are empty.')
    
    outsample_dat <- CV_dat # Reassign because this function was originally developed for simple out-of-sample predictions, and modified to accommodate k-fold cross validation
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
  alt_insample <- unserialize_table(paste0(project,"AltMatrix"), project)
  
  # Create out-of-sample alternative choice matrix
  create_alternative_choice(outsample_dat, project, occasion = alt_insample$occasion, occasion_var = alt_insample$occasion_var,
                            alt_var = alt_insample$alt_var, dist.unit = alt_insample$altChoiceUnits, min.haul = 0, zoneID = alt_insample$zoneID,
                            zone.cent.name = alt_insample$zone_cent_name, fish.cent.name = alt_insample$fish_cent_name,
                            spat = alt_insample$spat, spatID = alt_insample$spatID, outsample = TRUE)
  
  
  # Create expected catch matrix for out-of-sample data ---------------------------------------------------------------------------------------------
  # only generate the necessary catch matrices
  if(!is.null(e_settings)){
    
    ## check for default matrices ----
    defaults_opts <- c("recent","older","oldest","logbook")
    defaults <- e_list[which(e_list %in% defaults_opts)]
    
    # remove defaults from e_list
    e_list <- e_list[which(!(e_list %in% defaults))]
    
    if(is_empty(defaults)){ # no defaults
      default.exp <- FALSE
      
    } else { # get the defaults
      if(length(defaults) == 4){ # all default options
        default.exp <- TRUE
        
      } else { # default options
        default.exp <- defaults
        
      }
    }
    
    
    # iterate through list and create expected catch matrices ----
    for(i in 1:length(e_list)){
      
      # Get settings
      tmp_settings <- e_settings[paste0(e_list[i], "_settings")][[1]]
      
      if(is.null(tmp_settings$empty.catch)){ # set to NA if NULL, calc_exp() should do this, but just to be sure do it here.
        tmp_settings$empty.catch <- NA
      }
      
      if(i > 1){ # only create defaults for the first iteration
        default.exp <- FALSE
      }
      
      create_expectations(dat = outsample_dat, project = project, catch = tmp_settings$catch, price = tmp_settings$price,
                          defineGroup = tmp_settings$defineGroup, temp.var = tmp_settings$temp.var, temporal = tmp_settings$temporal,
                          calc.method = tmp_settings$calc.method, lag.method = tmp_settings$lag.method, empty.catch = tmp_settings$empty.catch,
                          empty.expectation = tmp_settings$empty.expectation, temp.window = tmp_settings$temp.window,
                          temp.lag = tmp_settings$temp.lag, year.lag = tmp_settings$year.lag, dummy.exp = tmp_settings$dummy.exp,
                          default.exp = default.exp, replace.output = TRUE, weight_avg = tmp_settings$weight_avg, outsample = TRUE)
    }
  }
  
  
  # Make model design -------------------------------------------------------------------------------------------------------------------------------
  if(is.null(outsample.mod.name)){
    outsample.mod.name <- paste0(mod.name,"_outsample")  
  } # else do nothing and use the provided name
  
  make_model_design(project = project, catchID = mdf$catchID, likelihood = mdf$likelihood, initparams = mdf$initparams,
                    optimOpt = mdf$optimOpt, methodname = mdf$methodname, mod.name = outsample.mod.name,
                    vars1 = mdf$vars1, vars2 = mdf$vars2, priceCol = mdf$priceCol, expectcatchmodels = mdf$expectcatchmodels,
                    startloc = mdf$startloc, polyn = mdf$polyn, spat = mdf$spat, spatID = mdf$spatID, crs = mdf$crs, 
                    outsample = TRUE, CV_dat = CV_dat)
}
