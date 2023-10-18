#' Predict out-of-sample data
#' 
#' @description Calculate predicted probabilities for out-of-sample dataset
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
#' @details
#' This function automatically pulls model settings from the selected model and creates an alternative choice matrix, expected catch/revenue matrices, 
#' and model design for an out-of-sample dataset. This function requires that a filtered out-of-sample data file (.rds file) exists in the output folder.
#' Note: is that the out-of-sample functions only work with a single selected model at a time. To run out-of-sample functions on a new
#' out-of-sample dataset, start with load_outsample() if an entirely new dataset or filter_outsample(). 
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' 
#' model_design_outsample("scallop", "scallopModName")
#' 
#' }

predict_outsample <- function(project, mod.name, use.scalers = FALSE, scaler.func = NULL){

  # Check that the outsample model design exists ----
  # Pull model design for all saved models
  if (table_exists(paste0(project, "ModelInputData"), project)) {
    mdf <- model_design_list(project)
  } else {
    stop('Model input table does not exist.', call. = FALSE)
  }
  
  # Get model names
  mdf_n <- model_names(project)
  
  # Get only info for selected model
  flag = 0
  tryCatch(
    {mdf <- mdf[[which(mdf_n == mod.name)]]},
    error = function(e) {flag <<- 1}
  )
  
  if(flag == 1){
    stop('Model not found. Run model_design_outsample() to create a model design for the out-of-sample dataset.')
  }
  
  # Run logit model ----
  if(grepl('logit', mdf$likelihood)){
    # Run logit model prediction
    temp <- logit_predict(project = project, mod.name = mod.name, use.scalers = use.scalers, scaler.func = scaler.func, outsample = TRUE)  
    probLogit <- temp[[1]] # Predicted probabilities of selecting each zone
    modelDat <- temp[[2]] # Model data
    probObs <- temp[[3]] # Predicted probabilities for each observation
    
  }
  
  # TODO: add more model options

  
  # Calculate performance metrics ----
  
  
  
}