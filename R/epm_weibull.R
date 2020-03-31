epm_weibull <- function(starts3, dat, otherdat, alts, project, expname, mod.name) {
    #' Expected profit model weibull catch function
    #'
    #' Expected profit model weibull catch function
    #'
    #' @param starts3 Starting values as a vector (num). For this likelihood,
    #'     the order takes: c([catch-function parameters], [travel-distance
    #'     parameters], [catch sigma(s)], [scale parameter]). \cr \cr
    #'     The catch-function and travel-distance parameters are of length (# of
    #'     catch-function variables)*(k) and (# of travel-distance variables)
    #'     respectively, where (k) equals the number of alternatives. The catch
    #'     sigma(s) are either of length equal to unity or length (k) if the
    #'     analyst is estimating location-specific catch sigma parameters. The
    #'     scale parameter is of length equal to unity.
    #' @param dat Data matrix, see output from shift_sort_x, alternatives with
    #'     distance.
    #' @param otherdat Other data used in model (as a list containing objects
    #'     `intdat`, `griddat`, and `prices`). \cr \cr
    #'     For this likelihood, `intdat` are "travel-distance variables", which
    #'     are alternative-invariant variables that are interacted with travel
    #'     distance to form the cost portion of the likelihood. Each variable
    #'     name therefore corresponds to data with dimensions (number of
    #'     observations) by (unity), and returns a single parameter. \cr \cr
    #'     In `griddat` are "catch-function variables" that are
    #'     alternative-invariant variables that are interacted with zonal
    #'     constants to form the catch portion of the likelihood. Each variable
    #'     name therefore corresponds to data with dimensions (number of
    #'     observations) by (unity), and returns (k) parameters where (k) equals
    #'     the number of alternatives. \cr \cr
    #'     For "catch-function variables" `griddat` and "travel-distance
    #'     variables" `intdat`, any number of variables are allowed, as a list
    #'     of matrices. Note the variables (each as a matrix) within `griddat`
    #'     `intdat` have no naming restrictions. "Catch-function variables" may
    #'     correspond to variables that impact catches by location, or
    #'     interaction variables may be vessel characteristics that affect how
    #'     much disutility is suffered by traveling a greater distance. Note in
    #'     this likelihood the "catch-function variables" vary across
    #'     observations but not for each location: they are allowed to impact
    #'     catches differently across alternatives due to the location-specific
    #'     coefficients. If there are no other data, the user can set `griddat`
    #'     as ones with dimension (number of observations) x (number of
    #'     alternatives) and `intdat` variables as ones with dimension (number
    #'     of observations) by (unity). \cr \cr
    #'     The variable `prices` is a matrix of dimension (number of
    #'     observations) by (unity), corresponding to prices.
    #' @param alts Number of alternative choices in model as length equal to
    #'     unity (as a numeric vector).
    #' @param project Name of project
    #' @param expname Expected catch table
    #' @param mod.name Name of model run for model result output table
    #' @return ld: negative log likelihood
    #' @export
    #' @examples
    #' \dontrun{
    #' data(zi)
    #' data(catch)
    #' data(choice)
    #' data(distance)
    #' data(si)
    #' data(prices)
    #'
    #' catch[catch<0] <- 0.00001
    #' #Note weibull catch distribution.
    #'
    #' optimOpt <- c(1000,1.00000000000000e-08,1,0)
    #'
    #' methodname <- 'BFGS'
    #'
    #' si2 <- sample(1:5,dim(si)[1],replace=TRUE)
    #' zi2 <- sample(1:10,dim(zi)[1],replace=TRUE)
    #'
    #' otherdat <- list(griddat=list(si=as.matrix(si),si2=as.matrix(si2)),
    #'     intdat=list(zi=as.matrix(zi),zi2=as.matrix(zi2)),
    #'     pricedat=list(prices=as.matrix(prices)))
    #'
    #' initparams <- c(2.5, 2.0, 1.5, 1.0, 1.1, 1.05, 0.9, 0.8, -0.8, -0.4, 3,
    #'     2, 3.5, 2.5, 1)
    #'
    #' func <- epm_weibull
    #'
    #' results <- discretefish_subroutine(catch,choice,distance,otherdat,
    #'     initparams,optimOpt,func,methodname)
    #' }
    #' @section Graphical examples: 
    #' \if{html}{
    #' \figure{epm_weibull_grid.png}{options: width="40\%" 
    #' alt="Figure: epm_weibull_grid.png"}
    #' \cr
    #' \figure{epm_weibull_travel.png}{options: width="40\%" 
    #' alt="Figure: epm_weibull_travel.png"}
    #' \cr
    #' \figure{epm_weibull_sigma.png}{options: width="40\%" 
    #' alt="Figure: epm_weibull_sigma.png"}
    #' }
    #'
        
    obsnum <- dim(as.data.frame(otherdat$griddat))[1]

    griddat <- as.matrix(do.call(cbind, otherdat$griddat))
    gridnum <- dim(griddat)[2]
    griddat <- matrix(apply(griddat, 2, function(x) rep(x,times=alts)), obsnum,
        gridnum*alts)
    intdat <- as.matrix(do.call(cbind, otherdat$intdat))
    intnum <- dim(intdat)[2]
    
    pricedat <- as.matrix(unlist(otherdat$pricedat))
    
    starts3 <- as.matrix(starts3)
    gridcoef <- as.matrix(starts3[1:(gridnum * alts), ])
    
    intcoef <- as.matrix(starts3[(((gridnum * alts) + intnum) - intnum + 1):
        ((gridnum * alts) + intnum), ])
    
    if ((dim(starts3)[1] - ((gridnum * alts) + intnum + 1)) == alts) {
    
        k <- as.matrix(starts3[((gridnum * alts) + intnum + 1):
            ((gridnum * alts) + intnum + alts), ])
        signum <- alts
    
    } else {
    
        k <- as.matrix(starts3[((gridnum * alts) + intnum + 1), ])
        signum <- 1
    
    }
    # if number of parameters before scale parameter is not equal to number of
        # alts, use first parameter as sigma catch
    
    k <- sqrt(k^2)
    
    sigmac <- as.matrix(starts3[((gridnum * alts) + intnum + 1 + signum), ])
    # end of vector
    
    gridbetas <- (matrix(gridcoef, obsnum, alts * gridnum, byrow = TRUE) *
        griddat)
    dim(gridbetas) <- c(nrow(gridbetas), alts, gridnum)
    gridbetas <- rowSums(gridbetas, dims = 2)
    
    gridmu <- sqrt(gridbetas^2)
    
    expgridcoef <- gridmu * matrix(gamma(1 + (1/k)), obsnum, alts)
    
    intbetas <- .rowSums(intdat * matrix(intcoef, obsnum, intnum, byrow = TRUE),
        obsnum, intnum)
    
    betas <- matrix(c((expgridcoef * matrix(pricedat, obsnum, alts)), intbetas), 
        obsnum, (alts + 1))
    
    djztemp <- betas[1:obsnum, rep(1:ncol(betas), each = alts)] *
        dat[, 3:(dim(dat)[2])]
    dim(djztemp) <- c(nrow(djztemp), ncol(djztemp)/(alts + 1), alts + 1)
    
    prof <- rowSums(djztemp, dims = 2)
    profx <- prof - prof[, 1]
    
    exb <- exp(profx/matrix(sigmac, dim(prof)[1], dim(prof)[2]))
    
    ldchoice <- (-log(rowSums(exb)))
    
    yj <- dat[, 1]
    cj <- dat[, 2]
    
    if (signum == 1) {
        empk <- k
    } else {
        empk <- k[cj]
    }
    
    empgridbetas <- t(gridcoef)
    dim(empgridbetas) <- c(nrow(empgridbetas), alts, gridnum)
    
    empgriddat <- griddat
    dim(empgriddat) <- c(nrow(empgriddat), alts, gridnum)
    
    empgridmu <- .rowSums(empgridbetas[, cj, ] * empgriddat[, 1, ], obsnum,
        gridnum)
    # note grid data same across all alternatives
    
    empgridmu <- sqrt(empgridmu^2)
    
    ldcatch <- (matrix((log(empk)), obsnum)) + (matrix((-(empk)), obsnum) *
        log(empgridmu)) + (matrix((empk - 1), obsnum) * log(yj)) +
        (-((yj/empgridmu)^(matrix(empk, obsnum))))
    
    ld1 <- ldcatch + ldchoice
    
    ld <- -sum(ld1)
    
    if (is.nan(ld) == TRUE) {
        ld <- .Machine$double.xmax
    }
    
    ldsumglobalcheck <- ld
    paramsglobalcheck <- starts3
    ldglobalcheck <- unlist(as.matrix(ld1))
    
    ldglobalcheck <- list(model=paste0(project, expname, mod.name), ldsumglobalcheck=ldsumglobalcheck,
                          paramsglobalcheck=paramsglobalcheck, ldglobalcheck=ldglobalcheck)
    
    fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase())
    single_sql <- paste0(project, "ldglobalcheck", format(Sys.Date(), format="%Y%m%d"))
    second_sql <- paste("INSERT INTO", single_sql, "VALUES (:data)")
    
    if(table_exists(single_sql)==TRUE){
      x <- unserialize(DBI::dbGetQuery(fishset_db, paste0("SELECT data FROM ", single_sql, " LIMIT 1"))$data[[1]])
      table_remove(single_sql)
      ldglobalcheck <- c(x, ldglobalcheck)
    }
    
    DBI::dbExecute(fishset_db, paste0("CREATE TABLE IF NOT EXISTS ", project, "ldglobalcheck", 
                                      format(Sys.Date(), format="%Y%m%d"), "(data ldglobalcheck)"))
    DBI::dbExecute(fishset_db, second_sql, params = list(data = list(serialize(ldglobalcheck, NULL))))
    DBI::dbDisconnect(fishset_db)
    
    return(ld)
    
}