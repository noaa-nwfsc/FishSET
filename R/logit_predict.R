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
#' @param outsample Logical, \code{FALSE} if predicting probabilities for main data, and \code{TRUE} if predicting for out-of-sample data. \code{outsample = FALSE} 
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
    logitEq <- read_dat(paste0(locoutput(project), project_files(project)[grep(mod.name, project_files(project))]), show_col_types = FALSE)  
    
  } else { # OUT-OF-SAMPLE
    # Get the latest model output file
    tmp_files1 <- project_files(project)[grep(mod.name, project_files(project))] # get all output files
    tmp_files2 <- unlist(stringi::stri_extract_all_regex(tmp_files1, "\\d+-\\d+-\\d+")) # get dates of output files
    file_i <- which(tmp_files2 == max(tmp_files2)) # get the index of the latest output file
    # Get parameter estimates
    logitEq <- read_dat(paste0(locoutput(project), project_files(project)[grep(mod.name, project_files(project))])[file_i], show_col_types = FALSE)  
    
    # Display file used in the gui
    if(isRunning()){
      showNotification(paste0("Pulling from model output file '", paste0(project_files(project)[grep(mod.name, project_files(project))])[file_i]), "'", type = 'message', duration = 10)
    }
    
    # Overwrite mod.name as the out-of-sample model design name
    mod.name <- outsample.mod.name
  }
  
  # Get model data
  # Including data compile, distance, gridvarying, interaction term
  mdf <- model_design_list(project)
  mdf_n <- model_names(project)
  mdf_new <- mdf[[which(mdf_n == mod.name)]]
  exp.names <- unlist(mdf_new$expectcatchmodels)
  
  mod.dat <- create_model_input(project = project, x = mdf_new,
                                mod.name = mdf_new$mod.name,
                                use.scalers = use.scalers,
                                scaler.func = scaler.func,
                                expected.catch = mdf_new$gridVaryingVariables,
                                exp.names = exp.names)
  
  dataCompile <- as.matrix(mod.dat$dataCompile, nrow = dim(mod.dat$dataCompile)[1], ncol = dim(dim(mod.dat$dataCompile)[1]))
  distance <- as.matrix(mod.dat$distance, nrow = dim(mod.dat$distance)[1], ncol = dim(mod.dat$distance)[2])
  griddat <- as.matrix(do.call(cbind, mod.dat$otherdat$griddat))
  intdat <- as.matrix(do.call(cbind, mod.dat$otherdat$intdat))
  
  # zoneID <- logitEq$X # WHAT IS THIS SUPPOSED TO BE??? Maybe we can delete?
  zoneID <- sort(unique(mod.dat$choice.table$choice))
  
  # Get the number of variables
  alts <- dim(unique(mod.dat$choice))[1] # alternatives
  obsnum <- dim(griddat)[1] # number of observations
  
  if(mdf_new$likelihood == "logit_c"){
    gridnum <- dim(griddat)[2] / alts # number of gridvarying variables  
  } else if (mdf_new$likelihood == "logit_zonal") {
    gridnum <- dim(griddat)[2] * (alts - 1)
  }
  
  intnum <- dim(intdat)[2] # number of alternative-invariant variables
  
  # Get coefficients
  logitEq <- logitEq$estimate
  gridcoef <- logitEq[1:gridnum] # get grid coefficients
  
  # If running zonal logit, insert 0 for the first alternative (interpretation is relative to the first alternative and this beta_1 = 0)
  if(mdf_new$likelihood == "logit_zonal"){
    gridcoef <- matrix(gridcoef, ncol = (alts - 1), byrow = TRUE) # get grid coefficients for each alternative
    gridcoef <- cbind(matrix(0, nrow = dim(griddat)[2], ncol = 1), gridcoef) # insert 0 for the first alternative (other coefs are relative to the first alt)
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
  
  # p <- length(logitEq)
  # betaLogit <- logitEq[1:alts]
  # ##What do we do if we do not have interaction terms?
  # Beta <- logitEq[-c(1:alts)] 
  # if(is_empty(Beta)){
  #   Beta <- 1
  # }
  # A <- alts*alts+1
  # bTerms <-  tmp.dat[,-c(1:A-1)] #choice matrix
  # numerLogit <- matrix(NA, nrow(tmp.dat), alts) #array(NA, c(nrow(mod.dat), nrow(mod.dat), alts)) # nan(nrow(x), alts)
  # 
  # for(i in 1:nrow(bTerms)){ # for each individual
  #   if(p-alts == 0){
  #     bTermsMatrix <- 0
  #   } else {
  #     bTermsMatrix <- array(bTerms[i,],c(alts, p-alts)) #reshape(bTerms[i,],alts,p-alts)
  #   }
  #   numerLogit[i,] = t(exp(betaLogit + bTermsMatrix*Beta))  #exp(betaLogit+miles*BmilesLogit+ milesSQ*bmilesSQ+Betaf*bf+....ect....
  # }
  # 
  # denomLogit <- rowSums(numerLogit) # sum(numerLogit,2)  # sum by row
  # probLogitzone <-  numerLogit/(matrix(1,1,alts) %x% denomLogit)  #numerLogit./repmat(denomLogit,1,alts)
  # #BML(count,model)=BmilesLogit
  # probLogit <- colMeans(probLogitzone)
  # probLogit <- cbind(zoneID, probLogit)
  # 
  # out <- list(probLogit=probLogit, modelDat=mod.dat)
  # return(out)                                                           
}
