explore_startparams_discrete <- function(space, dev, breakearly=TRUE, max.iterations=500, startsr=NULL,
                                         fr, d, otherdat, choice, project) {
  # func, catch, choice, distance, otherdat) {
  #' Explore starting value parameter space
  #'
  #' Shotgun method to find better parameter starting values by exploring starting value
  #'      parameter space.
  #'
  #' @param space List of length 1 or length equal to the number of models to be evaluated.
  #'   \code{space} is the number of starting value permutations to test (the size of
  #'   the space to explore). The greater the \code{dev} argument, the larger the
  #'   \code{space} argument should be.
  #' @param dev List of length 1 or length equal to the number of models to be evaluated.
  #'   \code{dev} refers to how far to deviate from the average parameter values when
  #'   exploring (random normal deviates). The less certain the average parameters are,
  #'   the greater the \code{dev} argument should be.
  #' @param breakearly Logical, should the function return the first set of inits that do not return INF or search the entire space 
  #'   and return the inits with the lowest LLoglikelihood.
  #' @param max.iterations If \code{explorestarts = TRUE}, max.iterations indicates the
  #'   maximum number of iterations to run in search of valid starting parameter values.
  #'   If the maximum is reached before valid parameter values are found (i.e., likelihood 
  #'   = Inf) the loop will terminate and an error message will be reported for that model.
  #' @param startsr Optional. List, average starting value parameters for
  #'     revenue/location-specific covariates then cost/distance. The best
  #'     guess at what the starting value parameters should be (e.g. all
  #'     ones). Specify starting value parameters for each model if values should be different than ones.
  #'     The number of starting value parameters should correspond to the likelihood and data that you want to
  #'     test. 
  #' @param fr Name of likelihood function to test.
  #' @param d  Data from \code{shift_sort_x}
  #' @param otherdat Other data (as a list, corresponding to the likelihood function you want to test).
  #' @param choice Data corresponding to actual zonal choice.
  #' @param project Project name
  #' @details Function is used to identify better starting parameters when
  #'   convergence is an issue. For more details on the likelihood functions or data, see \code{\link{make_model_design}}.
  #'   Function calls the model design file and should be used after the \code{\link{make_model_design}}
  #'   function is called. \cr
  #'   If more than one model is defined in the model design file, then starting parameters
  #'   must be defined for each model.
  #' @export
  #' @keywords internal
  #' @return
  #' Returns three data frames. \cr
  #'  \tabular{rlll}{
  #' newstart: \tab Chosen starting values with smallest likelihood \cr
  #' saveLLstarts: \tab Likelihood values for each starting value permutation \cr
  #' savestarts: \tab Starting value permutations (corresponding to each saved likelihood value) \cr
  #'     }
  #' @export

      # ab <- max(choice) + 1
      # no interactions in create_logit_input - interact distances in likelihood function instead
      fr.name <- match.fun(find_original_name(match.fun(as.character(fr))))
    
        savestarts <- list()
      saveLLstarts <- list()
    
     
      savestarts[[1]] <- startsr
      saveLLstarts[[1]] <- fr.name(startsr, d, otherdat, alts = max(choice), project=project, 
                                   mod.name=NULL, expname = NULL) #alts,

      k <- length(unlist(saveLLstarts))
      
      while(is.infinite(saveLLstarts[[length(unlist(saveLLstarts))]]) & (k < max.iterations)){
        k <- length(unlist(saveLLstarts)) + 1
        savestarts[[k]] <- rnorm(length(startsr), startsr, dev)
        saveLLstarts[[k]] <- fr.name(savestarts[[k]], d, otherdat, alts = max(choice), project=project, 
                                     mod.name=NULL, expname=NULL)
        
        if(isRunning()){
          cat(file = stderr(), "\n", "Exploring parameter starting values", 
                               "\n", "Log-likelihood estimate: ", saveLLstarts[[k]], 
                               "\n", "iteration: ", k, "\n\n")
        } else {
          cat(paste0("\n", "Exploring parameter starting values", 
                     "\n", "Log-likelihood estimate: ", saveLLstarts[[k]], 
                     "\n", "iteration: ", k, "\n\n"))
        }
      }
       
      if(k == max.iterations && is.infinite(saveLLstarts[[length(unlist(saveLLstarts))]])){
        if(isRunning()){
          showNotification("Not able to find valid starting parameters. Check model specification and/or update initial parameter values.",
                           type = "error", duration = 60)
        } else {
          cat("Initial function results bad (Nan, Inf, or undefined), check 'LDGlobalCheck'")  
        }
      }
      
      if(breakearly==FALSE && (length(unlist(saveLLstarts))) < space){
        k <- length(unlist(saveLLstarts)) + 1
      for (l in k:space) {
        savestarts[[l]] <- rnorm(length(startsr), startsr, dev)
        saveLLstarts[[l]] <- fr.name(savestarts[[l]], d, otherdat, alts = max(choice), project=project, 
                                     mod.name=NULL, expname=NULL)
       # if(breakearly==TRUE & !is.infinite(saveLLstarts[[k]])){
        #  break
       # }
      }}
      
      minindex <- which.min(unlist(saveLLstarts))
      newstart <- savestarts[[minindex]]

  return(newstart)
}




explore_startparams <- function(project, space, dev, startsr=NULL) {
  #' Explore starting value parameter space
  #'
  #' Shotgun method to find better parameter starting values by exploring starting value
  #'      parameter space.
  #'
  #' @param project String, name of project.
  #' @param space List of length 1 or length equal to the number of models to be evaluated.
  #'   \code{space} is the number of starting value permutations to test (the size of
  #'   the space to explore). The greater the \code{dev} argument, the larger the
  #'   \code{space} argument should be.
  #' @param startsr Optional. List, average starting value parameters for
  #'     revenue/location-specific covariates then cost/distance. The best
  #'     guess at what the starting value parameters should be (e.g. all
  #'     ones). Specify starting value parameters for each model if values should be differetn than ones.
  #'     The number of starting value parameters should correspond to the likelihood and data that you want to
  #'     test. 
  #' @param dev List of length 1 or length equal to the number of models to be evaluated.
  #'   \code{dev} refers to how far to deviate from the average parameter values when
  #'   exploring (random normal deviates). The less certain the average parameters are,
  #'   the greater the \code{dev} argument should be.
  # @param func Name of likelihood function to test.
  # @param choice Data corresponding to actual zonal choice.
  # @param catch Data corresponding to actual catch.
  # @param distance Data corresponding to distance.
  # @param otherdat Other data (as a list, corresponding to the likelihood function you want to test).
  #' @details Function is used to identify better starting parameters when
  #'   convergence is an issue. For more details on the likelihood functions or data, see \code{\link{make_model_design}}.
  #'   Function calls the model design file and should be used after the \code{\link{make_model_design}}
  #'   function is called. \cr
  #'   If more than one model is defined in the model design file, then starting parameters
  #'   must be defined for each model.
  #' @return
  #' Returns three data frames. \cr
  #'  \tabular{rlll}{
  #' newstart: \tab Chosen starting values with smallest likelihood \cr
  #' saveLLstarts: \tab Likelihood values for each starting value permutation \cr
  #' savestarts: \tab Starting value permutations (corresponding to each saved likelihood value) \cr
  #'     }
  #' @export
  #' @examples
  #' \dontrun{
  #' Example with only one model specified
  #' results <- explore_startparams('myproject', 15, 3, rep(1,17))
  #'
  #' Example with three models specified
  #' results <- explore_startparams('myproject', space = list(15,10,100),
  #'    dev=list(3,3,1), startsr=list(c(1,2,3), c(1,0, -1), c(0,0,.5)))
  #'
  #' View results
  #' results$startsOut
  # min(unlist(results$saveLLstarts))
  # match(min(unlist(results$saveLLstarts)),unlist(results$saveLLstarts))
  #' }
  
  # Call in datasets
  fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project = project))
  on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
  
  x_temp <- unserialize(DBI::dbGetQuery(fishset_db, paste0("SELECT ModelInputData FROM ", project, "modelinputdata LIMIT 1"))$ModelInputData[[1]])
  polyn <- NA
  
  for (i in 1:length(x_temp)) {
    x <- x_temp[i]
    
    catch <- (data.frame(as.matrix(x_temp[[i]][["catch"]])))/x_temp[[i]]$scales[1]
    choice <- x_temp[[i]][["choice"]]
    distance <- data.frame(x_temp[[i]][["distance"]])/x_temp[[i]]$scales[2]
    choice <- data.frame(as.matrix(as.numeric(factor(choice))))
    # startsr <- x_temp[[i]]["initparams"]
    ab <- max(choice) + 1 # no interactions in create_logit_input - interact distances in likelihood function instead
    fr <- x_temp[[i]][["likelihood"]] # func  #e.g. logit_c
    fr.name <- match.fun(find_original_name(match.fun(as.character(fr))))
    
    dataCompile <- create_logit_input(choice)
    
    d <- shift_sort_x(dataCompile, choice, catch, distance, max(choice), ab)
    
    
    # remove unnecessary lists
    x_temp[[i]][["gridVaryingVariables"]] <- Filter(Negate(function(x) is.null(unlist(x))), x_temp[[i]][["gridVaryingVariables"]])
    x_temp[[i]][["gridVaryingVariables"]] <- x_temp[[i]][["gridVaryingVariables"]][names(x_temp[[i]][["gridVaryingVariables"]]) != "units"]
    x_temp[[i]][["gridVaryingVariables"]] <- x_temp[[i]][["gridVaryingVariables"]][names(x_temp[[i]][["gridVaryingVariables"]]) != "scale"]
    
    
    
    ### Data needs will vary by the likelihood function ###
    if (grepl("epm", fr)) {
      otherdat <- list(
        griddat = list(griddatfin = as.data.frame(x_temp[[i]][["bCHeader"]][["gridVariablesInclude"]])),
        intdat = list(as.data.frame(x_temp[[i]][["bCHeader"]][["indeVarsForModel"]])),
        pricedat = as.data.frame(x_temp[[i]][["epmDefaultPrice"]])
      )
      nexpcatch <- 1
      expname <- fr
    } else if (fr == "logit_correction") {
      otherdat <- list(
        griddat = list(griddatfin = data.frame(rep(1, nrow(choice)))), # x[['bCHeader']][['gridVariablesInclude']]),
        intdat = list(as.data.frame(x_temp[[i]][["bCHeader"]][["indeVarsForModel"]])),
        startloc = as.data.frame(x_temp[[i]][["startloc"]]),
        polyn = as.data.frame(x_temp[[i]][["polyn"]])
      )
      nexpcatch <- 1
      expname <- fr
    } else if (fr == "logit_avgcat") {
      otherdat <- list(
        griddat = list(griddatfin = data.frame(rep(1, nrow(choice)))), # x[['bCHeader']][['gridVariablesInclude']]),
        intdat = list(as.data.frame(x_temp[[i]][["bCHeader"]][["indeVarsForModel"]]))
      )
      nexpcatch <- 1
      expname <- fr
    } else if (find_original_name(match.fun(as.character(fr))) == "logit_c") {
      nexpcatch <- length(names(x_temp[[i]][["gridVaryingVariables"]]))
    }
    # Begin loop
    for (j in 1:nexpcatch) {
      if (fr == "logit_c") {
        expname <- paste0(names(x_temp[[i]][["gridVaryingVariables"]])[j], "_", fr)
        otherdat <- list(
          griddat = list(griddatfin = as.data.frame(x_temp[[i]][["gridVaryingVariables"]][[names(x_temp[[i]][["gridVaryingVariables"]])[j]]])),
          intdat = list(as.data.frame(x_temp[[i]][["bCHeader"]][["indeVarsForModel"]]))
        )
      }

      if(is.null(startsr)){ 
      gridNum <- length(otherdat$griddat[[1]])
      intNum <-  length(otherdat$intdat[[1]])
      if(fr == 'logit_c'){
        numInits <- gridNum+intNum
      } else if(fr == 'logit_avgcat') {
        numInits <- gridNum*(max(choice)-1)+intNum
      } else if(fr == 'logit_correction'){
        numInits <- gridNum*4 + intNum + ((((polyn+1)*2)+2)*4) +1+1
      } else {
        numInits <- gridNum*max(choice)+intNum+1+1
      }
      
      startsr <- rep(1, numInits)
      } else {
        startsr <- startsr[[i]]
      }   
      # fr <- func
      # e.g. clogit
      
      # ab <- max(choice) + 1
      # no interactions in create_logit_input - interact distances in likelihood function instead
      
      savestarts <- list()
      saveLLstarts <- list()
      
      savestarts[[1]] <- startsr
      saveLLstarts[[1]] <- fr.name(startsr, d, otherdat, alts = max(choice), project=project, 
                                   mod.name=NULL, expname = NULL) #alts,
      
      for (k in 2:space[[i]]) {
        savestarts[[k]] <- rnorm(length(startsr), startsr, dev[[i]])
        saveLLstarts[[k]] <- fr.name(savestarts[[k]], d, otherdat, alts = max(choice), project=project, 
                                     mod.name=NULL, expname=NULL)
      }
      
      minindex <- which.min(unlist(saveLLstarts))
      newstart <- savestarts[[minindex]]
      
      startsOut <- list()
      startsOut[[length(startsOut) + 1]] <- list(name = expname, newstart = newstart, saveLLstarts = saveLLstarts, savestarts = savestarts)
    }
  }
  return(startsOut)
}


