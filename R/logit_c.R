logit_c <- function(starts3, dat, otherdat, alts) {
    #' logit_c
    #'
    #' Conditional logit likelihood
    #'
    #' @param starts3 Starting values. e.g. c([grid-varying variables], [interaction variables]).
    #' @param dat Data matrix, see output from shift_sort_x, alternatives with distance by column bind
    #' @param otherdat Other data used in model (as list). Any number of grid-varying variables (e.g. expected catch that varies by location) or 
    #' interaction variables (e.g. vessel characteristics that affect how much disutility is suffered by traveling a greater distance) are allowed. \cr \cr
    #' However, the user must place these in `otherdat` as list objects named `griddat` and `intdat` respectively. Note the variables 
    #' within `griddat` and `intdat` have no naming restrictions. \cr \cr
    #' Also note that `griddat` variables are dimension *(number of observations) x (number of alternatives)*, while `intdat` variables are 
    #' dimension *(number of observations) x 1*, to be interacted with the distance to each alternative. \cr \cr
    #' If there are no other data, the user can set `griddat` as ones with dimension *(number of observations) x (number of alternatives)*
    #' and `intdat` variables as ones with dimension *(number of observations) x 1*.
    #' @param alts Number of alternative choices in model
    #' @return ld - negative log likelihood
    # @export
    # @examples
    #'
    
    ld1 <- list()
    griddat <- (otherdat$griddat)  #this is a list
    intdat <- (otherdat$intdat)
    
    starts3 <- as.matrix(starts3)
    gridcoef <- as.matrix(starts3[1:length(griddat), ])
    intcoef <- as.matrix(starts3[((length(griddat) + length(intdat)) - 
        length(intdat) + 1):(length(griddat) + length(intdat)), ])
    
    for (i in 1:dim(dat)[1]) {
        
        betas1 <- c(t(as.matrix(do.call(rbind, lapply(griddat, `[`, i, 
            )))) %*% as.matrix(gridcoef), t(as.matrix(do.call(rbind, lapply(intdat, 
            `[`, i, )))) %*% as.matrix(intcoef))
        betas <- t(as.matrix(betas1))
        
        djz <- t(dat[i, 3:dim(dat)[2]])
        
        dj <- matrix(djz, nrow = alts, ncol = dim(betas)[2])
        
        xb <- dj %*% t(betas)
        xb <- xb - xb[1]
        exb <- exp(xb)
        ld1[[i]] <- (-log(t(exb) %*% (rep(1, alts))))
        
    }
    
    ldglobalcheck <- unlist(as.matrix(ld1))
    list2env(ldglobalcheck, envir = .GlobalEnv)
    
    ld <- (-do.call("sum", ld1))
    
    return(ld)
    
}