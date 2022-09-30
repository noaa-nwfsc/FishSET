#' Create model input data
#' 
#' Takes the structure of data compile, price (for EPM), distance, gridvarying, 
#' interaction terms
#' @param project Project name
#' @param x Optional, Model input data
#' @param mod.name Optional, name of model. Include if building model data for 
#'   specific defined model.
#' @param use.scalers Logical, should data be normalized? Defaults to TRUE. 
#'   Rescaling factors are the mean of the numeric vector unless specified with 
#'   \code{scaler}.
#' @param scaler.func Function to calculate rescaling factors. 
#' @param expected.catch For conditional logit. Expected catch matrices to include
#' @param exp.names The names of expected catch matrices to use in model. Specified
#'   in \code{\link{make_model_design}}. 
#' @return Returns a list of datacompile/choice matrix, dataCompile, distance, 
#'   otherdat, expname, choice, choice.table, and mod.name
#' @importFrom DBI dbConnect dbDisconnect
#' @importFrom RSQLite SQLite
#' @export
#' @keywords internal

create_model_input <-
  function(project,
           x = NULL,
           mod.name = NULL,
           use.scalers = TRUE,
           scaler.func = NULL,
           expected.catch = NULL,
           exp.names = NULL) {
  
  # TODO: add an arg for include specified ec matrices
  # Q: don't scale dummies?
  
  # Two scenarios 
  # 1) Pulling from discrete fish subroutine  - x will be supplied
  # 2) Pulling specific model
  # Call in datasets
  
  
  if (is.null(x)) {
    
    x_temp <- unserialize_table(paste0(project, "ModelInputData"), project)
    
    if (!is.null(mod.name)) {
      
      x <- x_temp[[which(lapply( x_temp , "[[" , "mod.name" ) == mod.name)]]
      
    } else {
      
      x <- NULL
    }
  } 
  
  if (is.null(x)) stop("Model name is not defined.", call. = FALSE)
  
  if (x$likelihood == "logit_c" & is.null(expected.catch)) {

    stop('Expected catch matrix required but not defined. Model not run.', 
         call. = FALSE)
  }
  
  # scalers ----
  if (use.scalers == TRUE && !is.null(scaler.func)) {
    
    if (!is.character(scaler.func)) {
      
      x$scales['catch'] <- scaler.func(x$catch)
      x$scales['zonal'] <- scaler.func(x$distance)
      # There can be multiple griddata vars, loop 
      # x$scales['griddata'] <- scaler.func(x$bCHeader$gridVariablesInclude)
      
      if (is.list(x$bCHeader$gridVariablesInclude)) {
        
        x$scales['griddata'] <- lapply(x$bCHeader$gridVariablesInclude, function(x) scaler.func(unlist(x)))
        
      } else {
        
        x$scales['griddata'] <- scaler.func(x$bCHeader$gridVariablesInclude) 
      }
      
      if (is.list(x$bCHeader$indeVarsForModel)) {
        
        x$scales['intdata'] <- lapply(x$bCHeader$indeVarsForModel, function(x) scaler.func(unlist(x)))
        
      } else {
        
        x$scales['intdata'] <- scaler.func(x$bCHeader$indeVarsForModel) 
      }
    } 
    
  } else if (use.scalers == FALSE) {
    
    for (k in 1:length(x$scales)) { 
      
      x$scales[k] <- 1
    }
  }

  catch <- x$catch / as.numeric(x$scales['catch'])
  choice <- x$choice # Note: is dataframe necessary?
  distance <- data.frame(x$distance) / as.numeric(x$scales['zonal'])
  startingloc <- x$startingloc
  mod.name <- unlist(x$mod.name)
  choice.table <- choice 
  # TODO: fix non-syntactic choice column name (or leave as vector)
  choice <- as.data.frame(as.numeric(factor(as.matrix(choice))))
  # Note: ab is # of cost parameters + # of alts (shift_sort_x)
  ab <- max(choice) + 1 # no interactions in create_logit_input - interact distances in likelihood function instead
  
  fr <- x$likelihood
  
  if (fr == "logit_correction" & all(is.na(startingloc))) {
    # Note: use stop() instead?
    warning("Startingloc parameter is not specified. Rerun the create_alternative_choice function")
  }
  
  dataCompile <- create_logit_input(choice)
  
  d <- shift_sort_x(dataCompile, choice, catch, distance, max(choice), ab)
  
  
  intdat <- mapply("/", x$bCHeader$indeVarsForModel, 
                  grep('intdata', names(x$scales)), 
                  SIMPLIFY = FALSE)
  
  griddat <- mapply("/", x$bCHeader$gridVariablesInclude, 
                    grep('griddata', names(x$scales)), 
                    SIMPLIFY = FALSE)
  
  if (!is_value_empty(exp.names)) {
    
    # use exp.names to pull specified exp matrix
    expected.catch <- expected.catch[exp.names]
    
    expname <- paste0(fr, '.', paste0(names(expected.catch), collapse = '.'))
   
    # Q: Is this right? Also, should dummies by included?
    ec_mean <- vapply(expected.catch, mean, numeric(1))
    
    # Q: filter for ec matrices (exclude dummies)?
    ec_grid <- lapply(seq_along(expected.catch), function(i) {
      
      expected.catch[[i]]/ec_mean[i]
    })
    
    names(ec_grid) <- names(expected.catch)
    # add ec matrices to griddat
    griddat <- c(griddat, ec_grid)
  }
  
  # Data needs will vary by the likelihood function
  # Note: gridVariablesInclude (non-expected catch gridded vars) is only being 
  # used in EPMs, need to include for other models
  if (grepl("epm", fr)) {
    
    
    
    otherdat <- list(
      # griddat = list(griddatfin = as.data.frame(x$bCHeader$gridVariablesInclude)/as.numeric(x$scales['griddata'])),
      griddat = griddat,
      intdat = intdat,
      pricedat = as.data.frame(x$epmDefaultPrice/as.numeric(x$scales['pscale']))
    )
    expname <- fr
    
  } else if (fr == "logit_correction") {
    
    otherdat <- list(
      # griddat = list(griddatfin = data.frame(rep(1, nrow(choice)))), 
      griddat = griddat,
      intdat = intdat,
      startloc = as.data.frame(x$startloc),
      polyn = as.data.frame(x$polyn),
      distance=list(distance)
    )
    expname <- fr
    
  } else if (fr == "logit_avgcat") {
    
    otherdat <- list(
      # griddat = list(griddatfin = data.frame(rep(1, nrow(choice)))),
      griddat = griddat,
      intdat = intdat
    )
    expname <- fr
    
  } else if (fr == "logit_c") {
    
    # # use exp.names to pull specified exp matrix
    # expected.catch <- expected.catch[exp.names]
    # 
    # # Q: should this include all names or just what is being used?
    # # expname <- paste0(fr, '_', paste0(names(x$gridVaryingVariables), collapse = ''))
    # 
    # expname <- paste0(fr, '.', paste0(names(expected.catch), collapse = '.'))
    # 
    # 
    # 
    # # Q: Is this right? Also, should dummies by included?
    # # ec_mean <- mean(do.call(cbind, expected.catch))
    # ec_mean <- vapply(expected.catch, mean, numeric(1))
    # 
    # # Q: filter for ec matrices (exclude dummies)?
    # ec_grid <- lapply(seq_along(expected.catch), function(i) {
    #   
    #   expected.catch[[i]]/ec_mean[i]
    #   })
    #   
    # names(ec_grid) <- names(expected.catch)
    # # Q: if dummies excluded from above, re-add them here? 
    # # griddat <- as.data.frame(griddat)
    # 
    # griddat <- c(griddat, ec_grid)
    
    otherdat <- list(

      # Q: this would divide the dummy matrices by the mean of all ec (also 
      # including dummies) is this right?
      # griddat = list(as.data.frame(lapply(expected.catch, function(sub) {
      #   
      #   sub <- lapply(sub, `/`, mean(unlist(do.call(cbind, expected.catch))))
      #   sub
      # }))),
      
      griddat = griddat,
      
      # intdat = list(as.data.frame(
      #   mapply("/", x$bCHeader$indeVarsForModel,
      #          grep('intdata', names(x$scales)), SIMPLIFY = FALSE)))
      intdat = intdat 
    )
  }
  
  out <- list(
    d = d, #datacompile/choice matrix
    dataCompile = dataCompile,
    distance = distance,
    otherdat = otherdat, #include price
    expname = expname,
    choice = choice,
    choice.table = choice.table,
    mod.name = mod.name
  )
  
  return(out)      
}


