logit_correction <- function(starts3, dat, otherdat, alts, project, expname, mod.name) {
    #' Full information model with Dahl's correction function
    #'
    #' Full information model with Dahl's correction function
    #'
    #' @param starts3 Starting values as a vector (num). For this likelihood,
    #'     the order takes: c([marginal utility from catch], [catch-function
    #'     parameters], [polynomial starting parameters], [travel-distance
    #'     parameters], [catch sigma]). \cr \cr
    #'     The number of polynomial interaction terms is currently set to 2, so
    #'     given the chosen degree 'polyn' there should be
    #'     (((polyn+1)*2) + 2)*(k) polynomial starting parameters, where (k)
    #'     equals the number of alternatives. The marginal utility from catch
    #'     and catch sigma are of length equal to unity respectively. The 
    #'     catch-function and travel-distance parameters are of length (# of
    #'     catch variables)*(k) and (# of cost variables) respectively.
    #' @param dat Data matrix, see output from shift_sort_x, alternatives with
    #'     distance.
    #' @param otherdat Other data used in model (as a list containing objects
    #'     `griddat`, `intdat`, `startloc`, `polyn`, and `distance`). \cr \cr
    #'     For catch-function variables (`griddat`) alternative-invariant
    #'     variables that are interacted with zonal constants to form the catch
    #'     portion of the likelihood. Each variable name therefore corresponds
    #'     to data with dimensions (number of observations) by (unity), and
    #'     returns (k) parameters where (k) equals the number of alternatives.
    #'     For travel-distance variables alternative-invariant
    #'     variables that are interacted with travel distance to form the cost
    #'     portion of the likelihood. Each variable name therefore corresponds
    #'     to data with dimensions (number of observations) by (unity), and
    #'     returns a single parameter. Any number of catch-function and
    #'     travel-distance variables are allowed, as a list of matrices. Note
    #'     the variables (each as a matrix) within `griddat` and `intdat` have
    #'     no naming restrictions. \cr \cr
    #'     Catch-function variables may correspond to variables that affect
    #'     catches across locations, or travel-distance variables may be vessel
    #'     characteristics that affect how much disutility is suffered by
    #'     traveling a greater distance. Note in this likelihood the
    #'     catch-function variables vary across observations but not for each
    #'     location: they are allowed to affect catches across locations due to
    #'     the location-specific coefficients. If there are no other data, the
    #'     user can set catch-function variables as ones with dimension
    #'     (number of observations) by (number of alternatives) and
    #'     travel-distance variables as ones with dimension (number of
    #'     observations) by (unity). \cr \cr
    #'     The variable startloc is a matrix of dimension
    #'     (number of observations) by (unity), that corresponds to the starting
    #'     location when the agent decides between alternatives. \cr \cr
    #'     The variable polyn is a vector of length equal to unity corresponding
    #'     to the chosen polynomial degree. \cr \cr
    #'     The variable distance is a matrix of dimension
    #'     (number of observations) by (number of alternatives) corresponding
    #'     to the distance to each alternative.
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
    #' data(startloc)
    #'
    #' optimOpt <- c(1000,1.00000000000000e-08,1,0)
    #'
    #' methodname <- 'BFGS'
    #'
    #' polyn <- 3
    #' kk <- 4
    #'
    #' si2 <- sample(1:5,dim(si)[1],replace=TRUE)
    #' zi2 <- sample(1:10,dim(zi)[1],replace=TRUE)
    #'
    #' otherdat <- list(griddat=list(si=as.matrix(si),si2=as.matrix(si2)),
    #'     intdat=list(zi=as.matrix(zi),zi2=as.matrix(zi2)),
    #'     startloc=as.matrix(startloc),polyn=polyn,
    #'     distance=as.matrix(distance))
    #'
    #' initparams <- c(3, 0.5, 0.4, 0.3, 0.2, 0.55, 0.45, 0.35, 0.25,
    #'     rep(0, (((polyn+1)*2) + 2)*kk), -0.3,-0.4, 3)
    #'
    #' func <- logit_correction
    #'
    #' results <- discretefish_subroutine(catch,choice,distance,otherdat,
    #'     initparams,optimOpt,func,methodname)
    #' }
    #' @section Graphical examples: 
    #' \if{html}{
    #' \figure{logit_correction_grid.png}{options: width="40\%" 
    #' alt="Figure: logit_correction_grid.png"}
    #' \cr
    #' \figure{logit_correction_travel.png}{options: width="40\%" 
    #' alt="Figure: logit_correction_travel.png"}
    #' \cr
    #' \figure{logit_correction_poly.png}{options: width="40\%" 
    #' alt="Figure: logit_correction_poly.png"}
    #' }
    #'
        
    obsnum <- dim(as.data.frame(otherdat$griddat))[1]

    griddat <- as.matrix(do.call(cbind, otherdat$griddat))
    gridnum <- dim(griddat)[2]
    griddat <- matrix(apply(griddat, 2, function(x) rep(x,times=alts)), obsnum,
        gridnum*alts)
    intdat <- as.matrix(do.call(cbind, otherdat$intdat))
    intnum <- dim(intdat)[2]
    
    startloc <- (otherdat$startloc)
    distance <- otherdat$distance
    
    polyn <- otherdat$polyn
    
    starts3 <- as.matrix(starts3)
    
    revcoef <- as.matrix(starts3[1:1, ])
    
    gridlength <- (gridnum * alts) + ((((polyn + 1) * 2) + 2) * alts)
    
    gridcoef <- as.matrix(starts3[2:(1 + gridlength), ])
    
    signum <- 1
    
    intcoef <- as.numeric(starts3[(1 + 1 + gridlength):((1 + 1 + gridlength) +
        intnum - 1), ])
    
    sigmac <- (1)
    
    sigmaa <- as.matrix(starts3[((1 + 1 + gridlength + intnum - 1) + 1), ])
    # end of vector
    
    gridbetas <- (matrix(gridcoef[1:(alts * gridnum), ], obsnum, alts * gridnum,
        byrow = TRUE) * griddat)
    dim(gridbetas) <- c(nrow(gridbetas), alts, gridnum)
    gridbetas <- rowSums(gridbetas, dims = 2)
    
    intbetas <- .rowSums(intdat * matrix(intcoef, obsnum, intnum, byrow = TRUE), 
        obsnum, intnum)
    
    betas <- matrix(c((gridbetas * matrix(revcoef, obsnum, alts)), intbetas),
        obsnum, (alts + 1))
    
    djztemp <- betas[1:obsnum, rep(1:ncol(betas), each = alts)] *
        dat[, 3:(dim(dat)[2])]
    dim(djztemp) <- c(nrow(djztemp), ncol(djztemp)/(alts + 1), alts + 1)
    
    prof <- rowSums(djztemp, dims = 2)
    profx <- prof - prof[, 1]
    
    exb <- exp(profx/matrix(sigmac, dim(prof)[1], dim(prof)[2]))
    
    ldchoice <- (-log(rowSums(exb)))
    
    revside <- gridbetas * matrix(revcoef, obsnum, alts)
    costside <- distance * intbetas
    
    probprof <- revside + costside
    
    probprofx <- probprof - probprof[, 1]
    
    probexb <- exp(probprofx/matrix(sigmac, dim(probprof)[1], dim(probprof)[2]))
    
    probs <- probexb/matrix(rowSums(probexb), obsnum, alts)
    
    yj <- dat[, 1]
    cj <- dat[, 2]
    
    locstay <- model.matrix(~as.factor(startloc) - 1)
    locmove <- model.matrix(~as.factor(cj) - 1)
    
    probstay <- probs * locstay
    probmove <- probs * locmove
    
    intpoly <- 2
    
    movemat <- matrix(c(locmove, (matrix(probmove, obsnum, alts * polyn)^
        matrix(rep(1:polyn, each = alts), obsnum, alts * polyn, byrow = TRUE)),
        (matrix(probmove, obsnum, alts * intpoly) * matrix(rowSums(probstay),
        obsnum, alts * intpoly))^matrix(rep(1:intpoly, each = alts), obsnum,
        alts * intpoly, byrow = TRUE)), obsnum, alts * (polyn + 1 + intpoly)) *
        matrix(!(startloc == cj), obsnum, alts * (polyn + 1 + intpoly))
    # 1 is for constant
    
    staymat <- matrix(c(locstay, (matrix(probstay, obsnum, alts * polyn)^
        matrix(rep(1:polyn, each = alts), obsnum, alts * polyn, byrow = TRUE))),
        obsnum, alts * (polyn + 1)) * matrix((startloc == cj), obsnum,
        alts * (polyn + 1))
    # 1 is for constant
    
    Xvar <- matrix(c(griddat * matrix(locmove, obsnum, gridnum * alts), staymat, 
        movemat), obsnum, dim(gridcoef)[1])
    
    empcatches <- Xvar %*% gridcoef
    
    ldcatch <- matrix((-(0.5) * log(2 * pi)), obsnum) + (-(0.5) *
        log(matrix(sigmaa, obsnum)^2)) + (-(0.5) * (((yj - empcatches)/
        (matrix(sigmaa, obsnum)))^2))
    
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
    
    fishset_db <- DBI::dbConnect(RSQLite::SQLite(),locdatabase())
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
