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
  if(!grepl("_outsample", mod.name)){
    stop('Selected model is not an out-of-sample data model.')
  }
  
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
    logitOutput <- logit_predict(project = project, mod.name = mod.name, use.scalers = use.scalers, scaler.func = scaler.func, outsample = TRUE)  
    probLogit <- logitOutput[[1]] # Predicted probabilities of selecting each zone
    modelDat <- logitOutput[[2]] # Model data
    probObs <- logitOutput[[3]] # Predicted probabilities for each observation
    
  }
  
  # TODO: add more model options

  
  # Calculate performance metrics ----
  # Find the zone with maximum fishing probability for each observation
  colnames(probObs) <- probLogit$zoneID
  tmpProbs <- cbind(probObs, apply(probObs, 1, max))
  tmpProbs <- tmpProbs[, 1:ncol(tmpProbs)-1] / tmpProbs[, ncol(tmpProbs)]
  tmpProbs[tmpProbs != 1] <- 0
  
  # Create a confusion matrix with predicted zones as rows and choice as columns
  pred.df <- data.frame(pred.zone = apply(tmpProbs, 1, FUN=function(x){colnames(tmpProbs)[x == 1]}),
                        choice = mdf$choice)
  levels <- sort(union(pred.df$pred.zone, pred.df$choice)) # need make sure the rows and columns in the table below match
  conf.matrix <- table(factor(pred.df$pred.zone, levels), factor(pred.df$choice, levels)) # confusion matrix

  # True positives
  tp <- diag(pred.table)
  # False positives
  fp <- rowSums(pred.table) - tp
  # False negatives
  fn <- colSums(pred.table) - tp
  # True negatives
  tn <- sum(pred.table) - tp - fp - fn
  
  # accuracy
  acc <- (tp + tn) / (tp + fp + tn + fn)
  
  # balanced accuracy (accounts for positive and negative outcomes)
  bacc <- (tpr + tnr) / 2
  
  # error rate (complement of accuracy)
  err <- (fp + fn) / (tp + fp + tn + fn)
  
  # false positive rate, fallout
  fpr <- fp / (fp + tn)
  
  # true positive rate, recall, sensitivity
  tpr <- tp / (tp + fn)
  
  # false negative rate, miss
  fnr <- fn / (tp + fn)
  
  # true negative rate, specificity
  tnr <- tn / (fp + tn)
  
  # positive predictive value (precision)
  ppv <- tp / (tp + fp)
  
  # phi correlation coefficient (1 indicates perfect prediction, 0 indicates random prediction, < 0 indicates worse than random prediction)
  phi <- ((tp * tn) - (fp * fn)) / sqrt((tp + fn) * (tn + fp) * (tp + fp) * (tn + fn))
  
  # F1 score ('mean' of precision and recall)
  f1 <- 2 * (ppv * tpr) / (ppv + tpr)
  
  outsample.df <- data.frame(zone = levels, prob = probLogit$prob, accuracy = acc, balanced.accuracy = bacc, precision = ppv, recall = tpr,
                             specificity = tnr, error.rate = err, false.pos.rate  = fpr, false.neg.rate = fnr, 
                             phi.corr = phi, f1.score = f1, row.names = NULL)
  
  return(outsample.df)
}