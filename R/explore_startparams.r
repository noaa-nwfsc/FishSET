explore_startparams <- function(space, startsr, dev, func, catch, choice,
    distance, otherdat) {
    #' Function to explore starting value parameter space
    #'
    #' Shotgun method to find better starting values by exploring starting value
    #'      parameter space
    #'
    #' @param space Number of starting value permutations to test (the size of
    #'     the space to explore). The greater the dev parameter the larger the
    #'     space parameter should be.
    #' @param startsr Average starting value parameters for
    #'     revenue/location-specific covariates then cost/distance. The best
    #'     guess at what the starting value parameters should be (e.g. all
    #'     ones). Should correspond to the likelihood and data that you want to
    #'     test.
    #' @param dev How far to deviate from the average parameter values when
    #'     exploring (random normal deviates). The less certain the average
    #'     parameters are, the greater the dev parameter should be.
    #' @param func Name of likelihood function to test.
    #' @param catch Data corresponding to actual zonal choice.
    #' @param choice Data corresponding to actual catch.
    #' @param distance Data corresponding to distance.
    #' @param otherdat Other data (as a list, corresponding to the likelihood
    #'     function you want to test.)
    #' @return
    #' newstart: Chosen starting values with smallest likelihood \cr
    #' saveLLstarts: Likelihood values for each starting value permutation \cr
    #' savestarts: Starting value permuations (corresponding to each saved
    #'     likelihood value) \cr
    #' @export
    #' @examples
    #' data(zi)
    #' data(catch)
    #' data(choice)
    #' data(distance)
    #' data(si)
    #' data(prices)
    #'
    #' space <- 15
    #' startsr <- rep(1,7)
    #' dev <- 3
    #' func <- epm_normal
    #'
    #' otherdat <- list(griddat=list(si=as.matrix(cbind(si,si,si,si))),
    #'     intdat=list(zi=as.matrix(zi)),
    #'     pricedat=list(prices=as.matrix(prices)))
    #'
    #' explore_startparams(space,startsr,dev,func,catch,choice,distance,
    #'     otherdat)
    #'
    #' startsr <- c(0.5, 0.4, 0.3, 0.2, -0.4, 2, 1)
    #' explore_startparams(space,startsr,dev,func,catch,choice,distance,
    #'     otherdat)
    #'
    #' space <- 100
    #' dev <- 1
    #'
    #' results <- explore_startparams(space,startsr,dev,func,catch,choice,
    #'     distance,otherdat)
    #' results$newstart
    #' min(unlist(results$saveLLstarts))
    #' match(min(unlist(results$saveLLstarts)),unlist(results$saveLLstarts))
    #'
    
    fr <- func
    # e.g. clogit
    
    ab <- max(choice) + 1
    # no interactions in create_logit_input - interact distances in likelihood
        # function instead
    dataCompile <- create_logit_input(choice)
    
    d <- shift_sort_x(dataCompile, choice, catch, distance, max(choice), ab)
    
    savestarts <- list()
    saveLLstarts <- list()
    
    savestarts[[1]] <- startsr
    saveLLstarts[[1]] <- fr(startsr, d, otherdat, max(choice))
    
    for (i in 2:space) {
        
        savestarts[[i]] <- rnorm(length(startsr), startsr, dev)
        saveLLstarts[[i]] <- fr(savestarts[[i]], d, otherdat, max(choice))
        
    }
    
    minindex <- which.min(unlist(saveLLstarts))
    newstart <- savestarts[[minindex]]
    
    return(list(newstart = newstart, saveLLstarts = saveLLstarts,
        savestarts = savestarts))
    
}
