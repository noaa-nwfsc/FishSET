     #' clogit
     #'
     #' Conditional logit likelihood
     #'
     #' @param starts3 Starting values
     #' @param dat Data matrix, see output from shiftSortX, alternatives with distance by column bind
     #' @param otherdat Other data used in model (as list)
     #' @param alts Number of alternative choices in model
     #' @return ld - negative log likelihood

     #'
     
clogit <- function(starts3,dat,otherdat,alts) {
     ld1 <- list()
     griddat <- (otherdat$griddat)
     intdat <- (otherdat$intdat)
     
     starts3 <- as.matrix(starts3)
     gridcoef <- as.matrix(starts3[1:length(intdat),])
     intcoef <- as.matrix(starts3[((length(griddat)+length(intdat))-length(gridcoef)+1):(length(griddat)+length(intdat)),])
     
     for(i in 1:dim(dat)[1])
     {
          
          betas1 <- c(t(as.matrix(do.call(rbind,lapply(griddat,`[`,i,))))%*%as.matrix(gridcoef), 
                      t(as.matrix(do.call(rbind,lapply(intdat,`[`,i,))))%*%as.matrix(intcoef))
          betas <- t(as.matrix(betas1))
          
          djz <- t(dat[i,3:dim(dat)[2]])
          
          dj <- matrix(djz, nrow = alts, ncol = dim(betas)[2])
          
          xb <- dj%*%t(betas)
          xb <- xb - xb[1]
          exb <- exp(xb)
          ld1[[i]] <- (-log(t(exb)%*%(rep(1, alts))))
          
     }
     
     ld <- (-do.call("sum", ld1))
     
     return(ld)
     
}
