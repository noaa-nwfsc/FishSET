epm_normal <- function(starts3, dat, otherdat, alts) {
    #' epm_normal
    #'
    #' Expected profit model normal catch function
    #'
    #' @param starts3 Starting values. e.g. c([grid-varying variables], [interaction variables], [catch variance], [scale parameter]).
    #' @param dat Data matrix, see output from shift_sort_x, alternatives with distance by column bind
    #' @param otherdat Other data used in model (as list). Any number of grid-varying variables (e.g. expected catch that varies by location) or 
    #' interaction variables (e.g. vessel characteristics that affect how much disutility is suffered by traveling a greater distance) are allowed. \cr \cr
    #' However, the user must place these in `otherdat` as list objects named `griddat` and `intdat` respectively. Note the variables #' within `griddat` 
    #' and `intdat` have no naming restrictions. \cr \cr
    #' Also note that `griddat` variables are  dimension *(number of observations) x #' (number of alternatives)*, while `intdat` variables are 
    #' dimension *(number of observations) x 1*, to be interacted with the distance to each #' alternative. \cr \cr
    #' If there are no other data, the user can set `griddat` as ones with dimension *(number of observations) x (number of alternatives)*
    #' and `intdat` variables as ones with dimension *(number of observations) x 1*.
    #' @param alts Number of alternative choices in model
    #' @return ld - negative log likelihood
    #' @export
    #' @examples
    #'
    
    ld1 <- list()
    griddat <- (otherdat$griddat)
    intdat <- (otherdat$intdat)
    pricedat <- (otherdat$pricedat)
    
    starts3 <- as.matrix(starts3)
    gridcoef <- as.matrix(starts3[1:(length(griddat) * alts), ])
    
    intcoef <- as.matrix(starts3[(((length(griddat) * alts) + length(intdat)) - 
        length(intdat) + 1):((length(griddat) * alts) + length(intdat)), 
        ])
    
    if ((dim(starts3)[1] - ((length(griddat) * alts) + length(intdat) + 
        1)) == alts) {
        sigmaa <- as.matrix(starts3[((length(griddat) * alts) + length(intdat) + 
            1):((length(griddat) * alts) + length(intdat) + alts), ])
        signum <- alts
    } else {
        sigmaa <- as.matrix(starts3[((length(griddat) * alts) + length(intdat) + 
            1), ])
        signum <- 1
    }
    
    sigmac <- as.matrix(starts3[((length(griddat) * alts) + length(intdat) + 
        1 + signum), ])  #end of vector
    
    for (i in 1:dim(dat)[1]) {
        
        betas1 <- c(t(as.matrix(do.call(rbind, lapply(griddat, `[`, i, 
            ))) * t(matrix(gridcoef, alts, length(griddat)))) %*% rep(as.matrix(do.call(rbind, 
            lapply(pricedat, `[`, i, ))), length(griddat)), t(as.matrix(do.call(rbind, 
            lapply(intdat, `[`, i, )))) %*% as.matrix(intcoef))
        betas <- t(as.matrix(betas1))
        
        djz <- t(dat[i, 3:dim(dat)[2]])
        
        dj <- matrix(djz, nrow = alts, ncol = dim(betas)[2])
        
        xb <- dj %*% t(betas)
        xb <- xb - xb[1]
        exb <- exp(xb/matrix(sigmac, dim(xb)[1], 1))
        
        ldchoice <- (-log(t(exb) %*% (rep(1, alts))))
        
        yj <- dat[i, 1]
        cj <- dat[i, 2]
        empcatches <- as.numeric(rowSums(t(as.matrix(do.call(rbind, lapply(griddat, 
            `[`, i, ))) * t(matrix(gridcoef, alts, length(griddat))))))
        
        if (signum == 1) {
            empsigmaa <- sigmaa
        } else {
            empsigmaa <- sigmaa[cj]
        }
        
        ldcatch1 <- (-(0.5) * log(2 * pi))
        ldcatch2 <- (-(0.5) * log(empsigmaa^2))
        ldcatch3 <- (-(0.5) * (((yj - empcatches[cj])/(empsigmaa))^2))
        ldcatch <- ldcatch1 + ldcatch2 + ldcatch3
        
        ld1[[i]] <- ldcatch + ldchoice
        
    }
    
    ld <- (-do.call("sum", ld1))
    
    if (is.nan(ld) == TRUE) {
        ld <- .Machine$double.xmax
    }
    
    ldsumglobalcheck <<- ld
    paramsglobalcheck <<- starts3
    ldglobalcheck <<- unlist(as.matrix(ld1))
    
    return(ld)
    
}