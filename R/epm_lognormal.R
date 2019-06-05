epm_lognormal <- function(starts3, dat, otherdat, alts) {
    #' epm_lognormal
    #'
    #' Expected profit model lognormal catch function
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
    sigmaa <- sqrt(sigmaa^2)
    
    sigmac <- as.matrix(starts3[((length(griddat) * alts) + length(intdat) + 
        1 + signum), ])  #end of vector
    
    for (i in 1:dim(dat)[1]) {
        
        gridmu <- t(t(as.numeric(rowSums(t(as.matrix(do.call(rbind, lapply(griddat, 
            `[`, i, ))) * t(matrix(gridcoef, alts, length(griddat))))))))
        
        expgridcoef <- exp(gridmu + (0.5 * (matrix(sigmaa, alts, 1)^2)))
        
        betas1 <- c((expgridcoef) %*% as.matrix(do.call(rbind, lapply(pricedat, 
            `[`, i, ))), t(as.matrix(do.call(rbind, lapply(intdat, `[`, 
            i, )))) %*% as.matrix(intcoef))
        betas <- t(as.matrix(betas1))
        
        djz <- t(dat[i, 3:dim(dat)[2]])
        
        dj <- matrix(djz, nrow = alts, ncol = dim(betas)[2])
        
        xb <- dj %*% t(betas)
        xb <- xb - xb[1]
        exb <- exp(xb/matrix(sigmac, length(xb), 1))
        
        ldchoice <- (-log(t(exb) %*% (rep(1, alts))))
        
        yj <- dat[i, 1]
        cj <- dat[i, 2]
        
        if (signum == 1) {
            empsigmaa <- sigmaa
        } else {
            empsigmaa <- sigmaa[cj, ]
        }
        
        ldcatch0 <- (-(log(yj)))
        ldcatch1 <- (-(log(empsigmaa)))
        ldcatch2 <- (-(0.5) * log(2 * pi))
        ldcatch3 <- (-(0.5) * (((log(yj) - gridmu[cj, ])/(empsigmaa))^2))
        
        ldcatch <- ldcatch0 + ldcatch1 + ldcatch2 + ldcatch3
        
        ld1[[i]] <- ldcatch + ldchoice
        
    }
	
    #############################################

	betas <- matrix(c(rep(revcoef,obsnum)*(matrix(gridcoef[1:alts,],obsnum,alts,byrow=TRUE)*griddat), intdat*intcoef),obsnum,(alts*gridnum)+intnum)

	djztemp <- betas[1:obsnum,rep(1:ncol(betas), each = alts)]*dat[, 3:(dim(dat)[2])]
	dim(djztemp) <- c(nrow(djztemp), ncol(djztemp)/(alts+1), alts+1)

	prof <- rowSums(djztemp,dim=2)
	profx <- prof - prof[,1]

	exb <- exp(profx/matrix(sigmac, dim(prof)[1], dim(prof)[2]))

	ldchoice <- (-log(rowSums(exb)))

	#############################################

	revside <- rep(revcoef,obsnum)*(matrix(gridcoef[1:alts,],obsnum,alts,byrow=TRUE)*griddat)
	costside <- distance*matrix(intdat,obsnum,alts)*matrix(intcoef,obsnum,alts)

	probprof <- revside + costside

	probprofx <- probprof - probprof[,1]

	probexb <- exp(probprofx/matrix(sigmac, dim(probprof)[1], dim(probprof)[2]))

	probs <- probexb/matrix(rowSums(probexb),obsnum,alts)
	
	yj <- dat[, 1]
	cj <- dat[, 2]

	locstay <- model.matrix(~as.factor(startloc)-1)
	locmove <- model.matrix(~as.factor(cj)-1)

	probstay <- probs*locstay
	probmove <- probs*locmove

	intpoly <- 2

	movemat <- matrix(c(locmove,(matrix(probmove,obsnum,alts*polyn)^matrix(rep(1:polyn,each=alts),obsnum,alts*polyn,byrow=TRUE)),
		(matrix(probmove,obsnum,alts*intpoly)*matrix(rowSums(probstay),obsnum,alts*intpoly))^
		matrix(rep(1:intpoly,each=alts),obsnum,alts*intpoly,byrow=TRUE)),obsnum,alts*(polyn+1+intpoly))*
		matrix(!(startloc == cj),obsnum,alts*(polyn+1+intpoly)) #1 is for constant

	staymat <- matrix(c(locstay,(matrix(probstay,obsnum,alts*polyn)^matrix(rep(1:polyn,each=alts),obsnum,alts*polyn,byrow=TRUE))),obsnum,alts*(polyn+1))*
		matrix((startloc == cj),obsnum,alts*(polyn+1)) #1 is for constant

	Xvar <- matrix(c(griddat*locmove, staymat, movemat), obsnum, dim(gridcoef)[1])

	empcatches <- Xvar%*%gridcoef
	# crossprod(t(Xvar),(gridcoef))
		
	ldcatch <- matrix((-(0.5) * log(2 * pi)),obsnum) + (-(0.5) * log(matrix(sigmaa,obsnum)^2)) + 
			(-(0.5) * (((yj - empcatches)/(matrix(sigmaa,obsnum)))^2))

	ldcatch <- matrix((-(log(yj))),obsnum) + matrix((-(log(empsigmaa))),obsnum) + matrix((-(0.5) * log(2 * pi)),obsnum) +
	(-(0.5) * (((log(yj) - gridmu[cj, ])/(matrix(empsigmaa,obsnum)))^2))
	
	ld1 <- ldcatch + ldchoice
	
	#############################################
	
    ld <- (-do.call("sum", ld1))
    
    if (is.nan(ld) == TRUE) {
        ld <- .Machine$double.xmax
    }
    
    ldsumglobalcheck <- ld
    assign('ldsumglobalcheck', value = ldsumglobalcheck, pos = 1)
    paramsglobalcheck <- starts3
    assign('paramsglobalcheck', value = paramsglobalcheck, pos = 1)
    ldglobalcheck <- unlist(as.matrix(ld1))
    assign('ldglobalcheck', value = ldglobalcheck, pos = 1)
    
    return(ld)
    
}