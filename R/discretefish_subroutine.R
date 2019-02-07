#' discretefish_subroutine
#' Subroutine to run chosen discrete choice model
#'
#' @param catch Data corresponding to actual zonal choice
#' @param choice Data corresponding to actual catch
#' @param distance Data corresponding to distance
#' @param otherdat Other data (as a list)
#' @param initparams Initial parameter estimates for revenue/location-specific covariates then cost/distance
#' @param optimOpt Optimization options [max function evaluations, max iterations, (reltol) tolerance of x]
#' @param func Name of likelihood function
#' @param func.name Name of likelihood function for model result output table
#' @param return.table Return an interactive data table of model output
#' @importFrom DBI dbExecute dbWriteTable dbExistsTable dbReadTable
#' @importFrom DT datatable
#' @return
#' OutLogit - [outmat1 se1 tEPM2] (coefs, ses, tstats) \cr 
#' optoutput - optimization information \cr 
#' seoumat2 - ses \cr 
#' MCM - Model Comparison metrics \cr 
#' H1 - inverse hessian \cr 
#' 
# @examples 
#choice <- as.data.frame(modelInputData$choice)
#alt.choice <- as.data.frame(as.numeric(as.factor(as.numeric(as.data.frame(modelInputData$choice)[,1]))))
#griddatfin <- list(predicted_catch=modelInputData$gridVaryingVariables$matrix)
#intdatfin <- list(modelInputData$bCHeader[[-1]])
#results <- discretefish_subroutine(catch=as.data.frame(modelInputData$catch), alt.choice, distance=modelInputData$zonalChoices,
#                                   otherdat=list(griddat=griddatfin,intdat=intdatfin), initparams= c(0.5, -2.8), optimOpt=c(100000,1000000,1.00000000000000e-06),
#                                   func=logit_c, 'newlogit4', return.table=TRUE)




discretefish_subroutine <- function(catch, choice, distance, otherdat, initparams, optimOpt, func, func.name, return.table=FALSE) {
  
  errorExplain <- NULL
  OutLogit <- NULL
  optoutput <- NULL
  seoutmat2 <- NULL
  MCM <- NULL
  H1 <- NULL
  fr <- func  #e.g. clogit
  
  ab <- max(choice) + 1  #no interactions in create_logit_input - interact distances in likelihood function instead
  dataCompile <- create_logit_input(choice)
  
  d <- shiftSortX(dataCompile, choice, catch, distance, max(choice), ab)
  
  MCR <- 1
  starts2 <- initparams
  
  LL_start <- fr(starts2, d, otherdat, max(choice))
  
  if (is.null(LL_start) || is.nan(LL_start) || is.infinite(LL_start)) {
    # haven't checked what happens when error yet
    errorExplain <- "Initial function results bad (Nan, Inf, or undefined), check 'ldglobalcheck'"
    return("Initial function results bad (Nan, Inf, or undefined), check 'ldglobalcheck'")
  }
  
  ############################################################################# 
  mIter <- optimOpt[2]
  MaxFunEvals <- optimOpt[1]
  TolX <- optimOpt[3]
  
  controlin <- list(maxit = mIter, reltol = TolX)
  
  res <- tryCatch({
    
    optim(starts2, fr, dat = d, otherdat = otherdat, alts = max(choice), control = controlin, 
          hessian = TRUE)
    
    # nlm(fr, starts2, dat=d, otherdat=otherdat, alts=max(choice), hessian=TRUE,
    # iterlim = mIter)
    
  }, error = function(e) {
    
    return("Optimization error, check 'ldglobalcheck'")
    
  })
  
  if (res[[1]][1] == "Optimization error, check 'ldglobalcheck'") {
    
    return(list(errorExplain = res, OutLogit = OutLogit, optoutput = optoutput, 
                seoutmat2 = seoutmat2, MCM = MCM, H1 = H1))
    
  }
  
  q2 <- res[["par"]]
  LL <- res[["value"]]
  output <- list(counts = res[["counts"]], convergence = res[["convergence"]], 
                 mesage = res[["message"]])
  H <- res[["hessian"]]
  
  # Model comparison metrics (MCM)
  param <- max(dim(as.matrix(starts2)))
  obs <- dim(dataCompile)[1]
  AIC <- 2 * param - 2 * LL
  
  AICc <- AIC + (2 * param * (param + 1))/(obs - param - 1)
  
  BIC <- -2 * LL + param * log(obs)
  
  PseudoR2 <- (LL_start - LL)/LL_start
  
  if (!exists("mod.out")) {
    mod.out <- data.frame(matrix(NA, nrow = 4, ncol = 1))
    mod.out[, 1] = c(AIC, AICc, BIC, PseudoR2)
    rownames(mod.out) = c("AIC", "AICc", "BIC", "PseudoR2")
    colnames(mod.out) = func.name
  } else {
    temp <- data.frame(c(AIC, AICc, BIC, PseudoR2))
    colnames(temp) = func.name
  }
  
   
  
  if (DBI::dbExistsTable(fishset_db, "out.mod") == FALSE) {
    DBI::dbWriteTable(fishset_db, "out.mod", out)
  } else {
    out.mod <- DBI::dbReadTable(fishset_db, "out.mod")
    if(exists('temp')){
    out.mod <- cbind(out.mod, temp)
    } else {
    out.mod <- cbind(out.mod, mod.out)
    }
    
    if (any(duplicated(colnames(out.mod))) == T) {
      stop("Duplicate columns names. Please define a unique column name for the model output,")
    }
    DBI::dbWriteTable(fishset_db, "out.mod", out.mod, overwrite = T)
  }

    out.mod <<- out.mod

 if(return.table==TRUE){
   rownames(out.mod)=c("AIC", "AICc", "BIC", "PseudoR2")
    print(DT::datatable(t(round(out.mod, 5)), filter='top')) 
  }
  
  MCM <- list(AIC = AIC, AICc = AICc, BIC = BIC, PseudoR2 = PseudoR2)
  
  if (is.null(H) == FALSE) {
    
    H1 <- tryCatch({
      solve(H)
    }, error = function(e) {
      return("Error, singular, check 'ldglobalcheck'")
      
    })
    
    diag2 <- tryCatch({
      diag(H1)
    }, error = function(e) {
      return("Error, NAs, check 'ldglobalcheck'")
    })
    
    se2 <- tryCatch({
      sqrt(diag2)
    }, warning = function(war) {
      print("Check 'ldglobalcheck'")
      sqrt(diag2)
    })
    
    outmat2 <- t(q2)
    seoutmat2 <- t(se2)
    optoutput <- output
    tLogit <- t(outmat2/se2)
    OutLogit <- cbind(t(outmat2), as.matrix(se2), (tLogit))
  }
  
  modelOut <- list(errorExplain = errorExplain, OutLogit = OutLogit, optoutput = optoutput, 
                   seoutmat2 = seoutmat2, MCM = MCM, H1 = H1)
  
  DBI::dbExecute(fishset_db, "CREATE TABLE IF NOT EXISTS data (modelout modelOut)")
  DBI::dbExecute(fishset_db, "INSERT INTO data VALUES (:modelout)", params = list(modelout = list(serialize(modelOut, NULL))))
  
  write(layout.json.ed(trace, "discretefish_subroutine", dataset = "", x = "", 
                       msg = paste("catch:", deparse(substitute(catch)), ", choice:", deparse(substitute(choice)), 
                                   ", distance:", deparse(substitute(distance)), ", otherdat:", deparse(substitute(otherdat)), 
                                   ", initparams:", deparse(substitute(initparams)), ", optimOpt:", deparse(substitute(optimOpt)), 
                                   ", func:", deparse(substitute(func)))), 
        paste(getwd(), "/Logs/", Sys.Date(), ".json", sep = ""), append = T)
  
  ############################################################################# 
  
  return(list(errorExplain = errorExplain, OutLogit = OutLogit, optoutput = optoutput, 
              seoutmat2 = seoutmat2, MCM = MCM, H1 = H1))
  
}
