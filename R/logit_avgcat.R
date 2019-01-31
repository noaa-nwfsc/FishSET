logit_avgcat <- function(starts3, dat, otherdat, alts) {
    #' logit_avgcat
    #'
    #' Average catch conditional logit procedure
    #'
    #' @param starts3 Starting values with dimensions equal to (number of alternatives - 1)*(number of grid-varying variables) + (number of interactions).
    #' Recall the user must drop one alternative for identification. e.g. c([grid-varying variables -1], [interaction variables]).
    #' @param dat Data matrix, see output from shift_sort_x, alternatives with distance by column bind
    #' @param otherdat Other data used in model (as list). Any number of grid-varying variables (e.g. variables that affect catches across locations) or 
    #' interaction variables (e.g. vessel characteristics that affect how much disutility is suffered by traveling a greater distance) are allowed. \cr \cr
    #' However, the user must place these in `otherdat` as list objects named `griddat` and `intdat` respectively. Note the variables
    #' within `griddat` and `intdat` have no naming restrictions. \cr \cr
    #' Also note that `griddat` variables are dimension *(number of observations) x 1*, #' to be interacted with each alternative, 
    #' while `intdat` variables are dimension *(number of observations) x 1*, to be interacted with the distance to each alternative. \cr \cr
    #' If there are no other data, the user can set `griddat` as ones with dimension *(number of observations) x 
    #' (number of alternatives)* and `intdat` variables as ones with dimension *(number of observations) x 1*.
    #' @param alts Number of alternative choices in model
    #' @return ld - negative log likelihood
    #' @export
    #' @examples
    #'
    
    ld1 <- list()
    intdat <- (otherdat$intdat)
    griddat <- (otherdat$griddat)
    
    starts3 <- as.matrix(starts3)
    gridcoef <- as.matrix(starts3[1:(length(griddat) * (alts - 1)), ])
    intcoef <- as.matrix(starts3[((length(griddat) * (alts - 1)) + 1):(((length(griddat) * 
        (alts - 1))) + length(intdat)), ])
    
    for (i in 1:dim(dat)[1]) {
        
        betas1 <- c(matrix(as.matrix(gridcoef), (alts - 1), length(griddat)) %*% 
            (as.matrix(do.call(rbind, lapply(griddat, `[`, i, )))), t(as.matrix(do.call(rbind, 
            lapply(intdat, `[`, i, )))) %*% as.matrix(intcoef))
        betas <- t(as.matrix(betas1))
        
        djz <- t(dat[i, (alts + 3):dim(dat)[2]])
        
        dj <- matrix(djz, nrow = alts, ncol = dim(betas)[2])
        
        xb <- dj %*% t(betas)
        xb <- xb - xb[1]
        exb <- exp(xb)
        ld1[[i]] <- (-log(t(exb) %*% (rep(1, alts))))
        
    }
    
    ldglobalcheck <<- unlist(as.matrix(ld1))
    
    ld <- (-do.call("sum", ld1))
    
    return(ld)
    
}