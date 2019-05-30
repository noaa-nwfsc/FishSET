logit_correction <- function(starts3, dat, otherdat, alts) {
    #' logit_correction
    #'
    #' Full information model with Dahl's correction function
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
    
    ld1 <- matrix(ncol=1, nrow=dim(dat)[1])

    griddat <- (otherdat$griddat)
    intdat <- (otherdat$intdat)
    startloc <- (otherdat$startloc)
    distance <- otherdat$distance
	
	polyn <- otherdat$polyn
	gridnum <- otherdat$gridnum
	intnum <- otherdat$intnum
	
    starts3 <- as.matrix(starts3)
	
	revcoef <- as.matrix(starts3[1:1, ])
	
	gridlength <- (gridnum * alts) + ((((polyn+1)*2)+2)*alts)
	
    gridcoef <- as.matrix(starts3[2:(1 + gridlength), ])
    
	intcoef <- (-1)
	
	sigmaa <- as.matrix(starts3[((1 + gridlength) + 
            1), ])
	signum <- 1
    
    sigmac <- as.matrix(starts3[((1 + gridlength) + 
        1 + signum), ])  #end of vector

	obsnum <- dim(griddat)[1]
	
	#############################################

	betas <- matrix(c(rep(revcoef,obsnum)*(matrix(gridcoef[1:alts,],obsnum,alts,byrow=TRUE)*griddat), intdat*intcoef),obsnum,(alts*gridnum)+intnum)

	djztemp <- betas[1:obsnum,rep(1:ncol(betas), each = alts)]*dat[, 3:(dim(dat)[2])]
	dim(djztemp) <- c(nrow(djztemp), ncol(djztemp)/(alts+1), alts+1)

	prof <- rowSums(djztemp,dim=2)
	profx = prof - prof[,1]

	exb = exp(profx/matrix(sigmac, dim(prof)[1], dim(prof)[2]))

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

	ld1 <- ldcatch + ldchoice

	ld <- -sum(ld1)
    
    if (is.nan(ld) == TRUE) {
        ld <- .Machine$double.xmax
    }
    
    # ldsumglobalcheck <<- ld
    # paramsglobalcheck <<- starts3
    # ldglobalcheck <<- unlist(as.matrix(ld1))
    
    return(ld)
    
}
