#' Logit predict
#' 
#' Prediction component from logit models (non mixed) called in Policy3, under predict_model_tempNew.m
#'
#' @param project Name of project
#' @param mod.name Name of saved model to use. Argument can be the name of the model or can pull the name 
#'   of the saved "best" model. Leave \code{mod.name} empty to use the saved "best" model. If more than
#'   one model is saved, \code{mod.name} should be the numeric indicator of which model to use.
#'   Use \code{table_view("modelChosen", project)} to view a table of saved models.
#' @param use.scalers Input for \code{create_model_input()}. Logical, should data be normalized? Defaults to \code{FALSE}. Rescaling factors are the mean of the 
#' numeric vector unless specified with \code{scaler.func}.
#' @param scaler.func Input for \code{create_model_input()}. Function to calculate rescaling factors.
#' @param outsample Logical, \code{FALSE} if predicting probabilities for primary data, and \code{TRUE} if predicting for out-of-sample data. \code{outsample = FALSE} 
#'   is the default setting.
#' @param outsample.mod.name If predicting out-of-sample data, provide the out-of-sample model design name. \code{outsample.mod.name = NULL}
#'   by default.
#' @return Returns probability of logit model by choice
#' @export
#' @keywords internal


logit_predict <- function(project, mod.name, use.scalers = FALSE, scaler.func = NULL, outsample = FALSE, outsample.mod.name = NULL){
  
  # Check if this is for in-sample or out-of-sample data
  if(!outsample){ # IN-SAMPLE
    # Get parameter estimates
    tmpEq <- get_latest_projectfile(project, mod.name)
    logitEq <- tmpEq[[1]]
    
    # Display file used in the gui
    if(isRunning()){
      showNotification(paste0("Pulling from model output file '", tmpEq[[2]]), "'", type = 'message', duration = 10)
    }
    
  } else { # OUT-OF-SAMPLE
    # Get parameter estimates
    tmpEq <- get_latest_projectfile(project, mod.name)
    logitEq <- tmpEq[[1]]
    
    # Display file used in the gui
    if(isRunning()){
      showNotification(paste0("Pulling from model output file '", tmpEq[[2]]), "'", type = 'message', duration = 10)
    }
    
    # Need to save original model in case the number of alternatives are different
    mdf <- model_design_list(project)
    mdf_n <- model_names(project)
    mdf_om <- mdf[[which(mdf_n == mod.name)]] # save original model
    in_zones <- sort(unique(mdf_om$choice$choice)) # get zones in the original
    
    # Overwrite mod.name as the out-of-sample model design name
    mod.name <- outsample.mod.name
  }
  
  # Get model data
  # Including data compile, distance, gridvarying, interaction term
  mdf <- model_design_list(project)
  mdf_n <- model_names(project)
  mdf_new <- mdf[[which(mdf_n == mod.name)]]
  exp.names <- unlist(mdf_new$expectcatchmodels)
  
  mod.dat <- create_model_input(project = project, x = mdf_new, mod.name = mdf_new$mod.name,
                                use.scalers = use.scalers, scaler.func = scaler.func,
                                expected.catch = mdf_new$gridVaryingVariables, exp.names = exp.names)
  
  dataCompile <- as.matrix(mod.dat$dataCompile, nrow = dim(mod.dat$dataCompile)[1], ncol = dim(dim(mod.dat$dataCompile)[1]))
  distance <- as.matrix(mod.dat$distance, nrow = dim(mod.dat$distance)[1], ncol = dim(mod.dat$distance)[2])
  griddat <- as.matrix(do.call(cbind, mod.dat$otherdat$griddat))
  intdat <- as.matrix(do.call(cbind, mod.dat$otherdat$intdat))
  zoneID <- sort(unique(mod.dat$choice.table$choice))
  
  # Get the number of variables
  alts <- dim(unique(mod.dat$choice))[1] # alternatives
  obsnum <- dim(griddat)[1] # number of observations
  
  if(mdf_new$likelihood == "logit_c"){
    gridnum <- dim(griddat)[2] / alts # number of gridvarying variables  
  } else if (mdf_new$likelihood == "logit_zonal") {
    gridnum <- dim(griddat)[2] * (alts - 1) # (grid variables * num of coefficients) calculation here makes following code cleaner
  }
  
  intnum <- dim(intdat)[2] # number of alternative-invariant variables
  
  # Get coefficients 
  z_flag <- 0 # flag, set = 1 if logit_zonal and first alt (in-sample dataset) not in out-of-sample dataset
  if(!outsample | (mdf_new$likelihood == "logit_c")){
    logitEq <- logitEq$estimate
  } else if(outsample & (mdf_new$likelihood == "logit_zonal")){
    # Format out-of-sample coefficients if alts not equal to in-sample alts  
    if(mdf_om$alts != mdf_new$alts){
      tmp <- format_outsample_coefs(in_zones = in_zones, out_zones = zoneID, Eq = logitEq, likelihood = mdf_new$likelihood)
      logitEq <- tmp[[1]]
      z_flag <- tmp[[2]]
      if(z_flag == 1) gridnum <- gridnum + 1 # need to add one because the first alt was removed from gridnum above
    } else {
      # Else when alts are equal just get the original coefficients
      logitEq <- logitEq$estimate
    }
  }
  gridcoef <- logitEq[1:gridnum] # get grid coefficients    

  
  # If running zonal logit, insert 0 for the first alternative (interpretation is relative to the first alternative and this beta_1 = 0)
  if(mdf_new$likelihood == "logit_zonal"){
    if(z_flag == 0){
      gridcoef <- matrix(gridcoef, ncol = (alts - 1), byrow = TRUE) # get grid coefficients for each alternative
      gridcoef <- cbind(matrix(0, nrow = dim(griddat)[2], ncol = 1), gridcoef) # insert 0 for the first alternative (other coefs are relative to the first alt)      
    } else {
      gridcoef <- matrix(gridcoef, ncol = (alts), byrow = TRUE) # get grid coefficients for each alternative
    }
    gridcoef <- as.vector(t(gridcoef)) # save as a single vector 
  }
  
  intcoef <- logitEq[(gridnum+1):(gridnum+intnum)] # get int coefficients
  
  if(mdf_new$likelihood == "logit_c"){
    # calculate the sum of the products of the grid coefficients and the gridvarying variables
    gridbetas <- matrix(rep(gridcoef, each = alts), nrow = obsnum, ncol = alts*gridnum, byrow = TRUE) * griddat 
    dim(gridbetas) <- c(nrow(gridbetas), alts, gridnum)
    gridbetas <- rowSums(gridbetas, dims = 2)
    
  } else if (mdf_new$likelihood == "logit_zonal") {
    # calculate the sum of the products of the alternative-specific coefficients and the alternative-invariant variables
    gridbetas <- matrix(rep(gridcoef), nrow = obsnum, ncol = alts * dim(griddat)[2], byrow = TRUE) 
    gridbetas <- gridbetas * t(apply(griddat, 1, function(x) rep(x, each = alts)))
    dim(gridbetas) <- c(nrow(gridbetas), alts, dim(griddat)[2])
    gridbetas <- rowSums(gridbetas, dims = 2)
    
  }

  # calculate the sum of the products of the distance coefficient, alternative invariant variables that interacts with distance, and the distance between locations
  intbetas <- matrix(intcoef, nrow = obsnum, ncol = intnum, byrow = TRUE) * intdat
  intbetas <- matrix(apply(intbetas, 2, function(x) rep(x, alts)), ncol = intnum * alts) * matrix(rep(distance, intnum), nrow = obsnum, ncol = alts * intnum)
  dim(intbetas) <- c(nrow(intbetas), alts, intnum)
  intbetas <- rowSums(intbetas, dims = 2)
  
  numerLogit <- exp(gridbetas + intbetas)
  denomLogit <- as.matrix(rowSums(numerLogit))
  pLogit <- numerLogit/(matrix(1,1,alts) %x% denomLogit)
  probLogit <- colMeans(pLogit)
  probLogit <- data.frame(zoneID = zoneID, prob = probLogit)
  return(list(probLogit, mod.dat, pLogit))
  
}
