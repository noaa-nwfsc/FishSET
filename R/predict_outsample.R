#' Predict out-of-sample data
#' 
#' @description Calculate predicted probabilities for out-of-sample dataset
#' 
#' @param project Name of project
#' @param mod.name Name of saved model to use. Argument can be the name of the model or can pull the name 
#'   of the saved "best" model. Leave \code{mod.name} empty to use the saved "best" model. If more than
#'   one model is saved, \code{mod.name} should be the numeric indicator of which model to use.
#'   Use \code{table_view("modelChosen", project)} to view a table of saved models.
#' @param outsample.mod.name Name of the saved out-of-sample model design.
#' @param use.scalers Input for \code{create_model_input()}. Logical, should data be normalized? Defaults to \code{FALSE}. Rescaling factors are the mean of the 
#' numeric vector unless specified with \code{scaler.func}.
#' @param scaler.func Input for \code{create_model_input()}. Function to calculate rescaling factors.
#' 
#' @details
#' This function predicts out-of-sample fishing probabilities and calculates model prediction performance (percent absolute prediction error).
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' 
#' predict_outsample("scallop1", "logit_c_mod1", "logit_c_mod1_outsample")
#' 
#' }

predict_outsample <- function(project, mod.name, outsample.mod.name, use.scalers = FALSE, scaler.func = NULL){

  # Pull model design for all saved models
  if (table_exists(paste0(project, "ModelInputData"), project)) {
    mdf <- model_design_list(project)
  } else {
    stop('Model input table does not exist.', call. = FALSE)
  }
  
  # Get model names
  mdf_n <- model_names(project)
  
  # Get only info for selected out-of-sample model
  flag = 0
  tryCatch(
    {mdf <- mdf[[which(mdf_n == outsample.mod.name)]]},
    error = function(e) {flag <<- 1}
  )
  
  if(flag == 1){
    stop('Model not found. Run model_design_outsample() to create a model design for the out-of-sample dataset.')
  }
  
  # Run logit model ----
  if(grepl('logit', mdf$likelihood)){
    # Run logit model prediction
    logitOutput <- logit_predict(project = project, mod.name = mod.name, 
                                 use.scalers = use.scalers, scaler.func = scaler.func, 
                                 outsample = TRUE, outsample.mod.name = outsample.mod.name)  
    probOutput <- logitOutput[[1]] # Predicted probabilities of selecting each zone
    modelDat <- logitOutput[[2]] # Model data
    probObs <- logitOutput[[3]] # Predicted probabilities for each observation
    
  } else if(grepl('epm', mdf$likelihood)){
    # Run epm model prediction
    epmOutput <- epm_predict(project = project, mod.name = mod.name, mod.type = mdf$likelihood,
                             use.scalers = use.scalers, scaler.func = scaler.func, 
                             outsample = TRUE, outsample.mod.name = outsample.mod.name)  
    probOutput <- epmOutput[[1]] # Predicted probabilities of selecting each zone
    modelDat <- epmOutput[[2]] # Model data
    probObs <- epmOutput[[3]] # Predicted probabilities for each observation
  }
  
  
  # Performance metrics ----
  ## percent absolute prediction error ----
  zones <- probOutput$zoneID
  choice <- mdf$choice$choice
  choice.tab <- table(choice)
  insample_share <- choice.tab/sum(choice.tab)
  
  colnames(probObs) <- probOutput$zoneID
  predicted.tab <- colSums(probObs) # estimated number of trips represented by the sum of preditcted probabilities by zone
  outsample_share <- predicted.tab/sum(predicted.tab)
  
  perc.abs.pred.err <- sum(abs(insample_share - outsample_share)) * 100
  
  return(list(probOutput, perc.abs.pred.err, probObs))
}