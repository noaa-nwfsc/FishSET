logit_avgcat <- function(starts3, dat, otherdat, alts, project, expname, mod.name) {
    #' Average catch multinomial logit procedure
    #'
    #' Average catch multinomial logit procedure
    #'
    #' @param starts3 Starting values as a vector (num). For this likelihood,
    #'     the order takes: c([average-catch parameters], [travel-distance
	#'     parameters]). \cr \cr
    #'     The average-catch and travel-distance parameters are of length (# of
	#'     average-catch variables)*(k-1) and (# of travel-distance variables
	#'     respectively, where (k) equals the number of alternatives.
    #' @param dat Data matrix, see output from shift_sort_x, alternatives with distance.
    #' @param otherdat Other data used in model (as a list containing objects
	#'     `intdat` and `griddat`). \cr \cr
	#'     For this likelihood, `intdat` are "travel-distance variables", which
    #'     are alternative-invariant variables that are interacted with travel
    #'     distance to form the cost portion of the likelihood. Each variable
    #'     name therefore corresponds to data with dimensions (number of
    #'     observations) by (unity), and returns a single parameter. \cr \cr
    #'     In `griddat` are "average-catch variables" that do not vary across
	#'     alternatives, e.g. vessel gross tonnage. Each variable name therefore
	#'     corresponds to data with dimensions (number of observations) by
	#'     (unity), and returns (k-1) parameters where (k) equals the number of
	#'     alternatives, as a normalization of parameters is needed as the
	#'     probabilities sum to one. Interpretation is therefore relative to the
	#'     first alternative. \cr \cr
	#'     For both objects any number of variables are allowed, as a list of
	#'     matrices. Note the variables (each as a matrix) within `griddat` and
	#'     `intdat` have no naming restrictions. "Average-catch variables"
	#'     may correspond to variables that impact average catches by location,
    #'     or "travel-distance variables" may be vessel characteristics that
	#'     affect how much disutility is suffered by traveling a greater
    #'     distance. Note in this likelihood the "average-catch variables" vary
	#'     across observations but not for each location: they are allowed to
    #'     affect alternatives differently due to the location-specific
	#'     coefficients. \cr \cr
    #'     If there are no other data, the user can set `griddat` as ones with
	#'     dimension (number of observations) by (unity) and `intdat` variables
	#'     as ones with dimension (number of observations) by (unity).
    #' @param alts Number of alternative choices in model as length 1 vector (num).
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
	#' 
	#' optimOpt <- c(1000,1.00000000000000e-08,1,0)
	#' 
	#' methodname <- "BFGS"
	#' 
	#' si2 <- sample(1:5,dim(si)[1],replace=TRUE)
	#' zi2 <- sample(1:10,dim(zi)[1],replace=TRUE)
	#'
	#' otherdat <- list(griddat=list(si=as.matrix(si),si2=as.matrix(si2)),
	#' intdat=list(zi=as.matrix(zi),zi2=as.matrix(zi2)))
	#'
	#' initparams <- c(1.5, 1.25, 1.0, 0.9, 0.8, 0.75, -1, -0.5)
	#' 
	#' func <- logit_avgcat
	#' 
	#' results <- discretefish_subroutine(catch,choice,distance,
	#' otherdat,initparams,optimOpt,func,methodname)
	#' }
	
	griddat <- as.matrix(do.call(cbind, otherdat$griddat))
    intdat <- as.matrix(do.call(cbind, otherdat$intdat))
	
	gridnum <- dim(griddat)[2]
	intnum <- dim(intdat)[2]
	#get number of variables
	
	obsnum <- dim(griddat)[1]

    starts3 <- as.matrix(starts3)
    gridcoef <- as.matrix(starts3[1:(gridnum * (alts - 1)), ])
    intcoef <- as.matrix(starts3[((gridnum * (alts - 1)) + 1):(((gridnum * 
        (alts - 1))) + intnum), ])
    
   	#############################################

	gridbetas <- (matrix(gridcoef,obsnum,(alts-1)*gridnum,byrow=TRUE)*griddat[, rep(1:gridnum, each=(alts-1))])
	dim(gridbetas) <- c(nrow(gridbetas), (alts-1), gridnum)
	gridbetas <- rowSums(gridbetas,dim=2)
	
	intbetas <- .rowSums(intdat*matrix(intcoef,obsnum,intnum,byrow=TRUE),obsnum,intnum)
	
	betas <- matrix(c(gridbetas, intbetas),obsnum,(alts-1+1))

	djztemp <- betas[1:obsnum,rep(1:ncol(betas), each = (alts))]*dat[, (alts+3):(dim(dat)[2])]
	dim(djztemp) <- c(nrow(djztemp), ncol(djztemp)/((alts-1)+1), (alts-1)+1)

	prof <- rowSums(djztemp,dim=2)
	profx <- prof - prof[,1]

	exb <- exp(profx)

	ldchoice <- (-log(rowSums(exb)))

	#############################################
	
	ld <- -sum(ldchoice)
	
    if (is.nan(ld) == TRUE) {
        ld <- .Machine$double.xmax
    }
    
    ldsumglobalcheck <- ld
    paramsglobalcheck <- starts3
    ldglobalcheck <- unlist(as.matrix(ldchoice))
    
    ldglobalcheck <- list(model=paste0(project, expname, mod.name), ldsumglobalcheck=ldsumglobalcheck,
                          paramsglobalcheck=paramsglobalcheck, ldglobalcheck=ldglobalcheck)
    
    fishset_db <- DBI::dbConnect(RSQLite::SQLite(), "fishset_db.sqlite")
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