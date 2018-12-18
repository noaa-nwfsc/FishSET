#' zonal_subroutine
#'
#' Subroutine to run chosen discrete choice model
#'
#' @param catch Data corresponding to actual catch 
#' @param choice Data corresponding to actual zonal. Should be as.integer. 
#' @param distance Data corresponding to distance f
#' @param otherdat Other data (as a list)
#' @param initparams Initial parameter estimates for revenue/location-specific covariates then cost/distance
#' @param optimOpt Optimization options [max function evaluations, max iterations, (reltol) tolerance of x]
#' @param func Name of likelihood function
#' @details Called the ShiftSortX, CreateLogitInput and likekihood functions. Inputs from ModelInputData generated in make_model_design
#' @return
#' OutLogit - [outmat1 se1 tEPM2] (coefs, ses, tstats) \cr 
#' clogitoutput - optimization information \cr 
#' seoumat2 - ses \cr 
#' MCM - Model Comparison metrics \cr 
#' H1 - inverse hessian \cr 
#' @examples
#'

zonal_subroutine <- function(catch, choice, distance, otherdat, initparams, optimOpt, func) {
    errorExplain <- NULL
    OutLogit <- NULL
    clogitoutput <- NULL
    seoutmat2 <- NULL
    MCM <- NULL
    H1 <- NULL
    fr <- func  #e.g. clogit
    
    ab <- if (is.data.frame(choice)) {
        dim(unique(choice))[1] + 1
    } else {
        length(unique(choice))
    }  #no interactions in create_logit_input - interact distances in likelihood function instead
    dataCompile <- create_logit_input(choice)
    
    d <- shiftSortX(dataCompile, choice, catch, distance, max(choice), ab)
    
    MCR <- 1
    starts2 <- initparams
    
    LL_start <- fr(starts2, d, otherdat, max(choice))
    
    if (is.null(LL_start) || is.nan(LL_start) || is.infinite(LL_start)) {
        # haven't checked what happens when error yet
        errorExplain <- "Initial function results bad (Nan, Inf, or undefined)"
        return("Initial function results bad (Nan, Inf, or undefined)")
    }
    
    ############################################################################# 
    mIter <- optimOpt[2]
    MaxFunEvals <- optimOpt[1]
    TolX <- optimOpt[3]
    
    controlin <- list(maxit = mIter, reltol = TolX)
    
    res <- tryCatch({
        
        optim(starts2, fr, dat = d, otherdat = otherdat, alts = max(choice), control = controlin, hessian = TRUE)
        
    }, error = function(e) {
        
        errorExplain <- "Optimization error"
        return("Optimization error")
        
    })
    
    q2 <- res[["par"]]
    LL <- res[["value"]]
    output <- list(counts = res[["counts"]], convergence = res[["convergence"]], mesage = res[["message"]])
    H <- res[["hessian"]]
    
    # Model comparison metrics (MCM)
    param <- max(dim(as.matrix(starts2)))
    obs <- dim(dataCompile)[1]
    AIC <- 2 * param - 2 * LL
    
    AICc <- AIC + (2 * param * (param + 1))/(obs - param - 1)
    
    BIC <- -2 * LL + param * log(obs)
    
    PseudoR2 <- (LL_start - LL)/LL_start
    
    MCM <- list(AIC = AIC, AICc = AICc, BIC = BIC, PseudoR2 = PseudoR2)
    
    if (is.null(H) == FALSE) {
        H1 <- solve(H)
        diag2 <- diag(H1)
        se2 <- sqrt(diag2)
        
        outmat2 <- t(q2)
        seoutmat2 <- t(se2)
        clogitoutput <- output
        tLogit <- t(outmat2/se2)
        OutLogit <- cbind(t(outmat2), as.matrix(se2), (tLogit))
    }
    
    ############################################################################# 
    
    return(list(errorExplain = errorExplain, OutLogit = OutLogit, clogitoutput = clogitoutput, seoutmat2 = seoutmat2, MCM = MCM, H1 = H1))
    
}
