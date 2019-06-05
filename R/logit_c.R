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
    #' @export
    # @examples
    #'
    
    ld1 <- list()
    griddat <- (otherdat$griddat)  #this is a list
    intdat <- (otherdat$intdat)
    
    starts3 <- as.matrix(starts3)
    gridcoef <- as.matrix(starts3[1:length(griddat), ])
    intcoef <- as.matrix(starts3[((length(griddat) + length(intdat)) - 
        length(intdat) + 1):(length(griddat) + length(intdat)), ])
    
	#############################################
	
	gridnum <- otherdat$gridnum
	intnum <- otherdat$intnum
	obsnum <- dim(griddat)[1]
	
	betas <- matrix(c((matrix(gridcoef[1:alts,],obsnum,alts,byrow=TRUE)*griddat), intdat*intcoef),obsnum,(alts*gridnum)+intnum)

	djztemp <- betas[1:obsnum,rep(1:ncol(betas), each = alts)]*dat[, 3:(dim(dat)[2])]
	dim(djztemp) <- c(nrow(djztemp), ncol(djztemp)/(alts+1), alts+1)

	prof <- rowSums(djztemp,dim=2)
	profx <- prof - prof[,1]

	exb <- exp(profx/matrix(sigmac, dim(prof)[1], dim(prof)[2]))

	ldchoice <- (-log(rowSums(exb)))

	#############################################

    ldglobalcheck <- unlist(as.matrix(ldchoice))
    assign("ldglobalcheck", value = ldglobalcheck, pos = 1)
	
	ld <- -sum(ldchoice)
    
    if (is.nan(ld) == TRUE) {
        ld <- .Machine$double.xmax
    }
    
    return(ld)
    
}
