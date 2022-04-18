#' Create model input data
#' 
#' Takes the structure of data compile, price (for EPM), distance, gridvarying, interaction terms
#' @param project Project name
#' @param x Optional, Model input data
#' @param mod.name Optional, name of model. Include if building model data for specific defined model.
#' @param use.scalers Logical, should data be noramalized? Defaults to TRUE. Rescaling factors are the mean of the numeric vector unless specified with \code{scaler}.
#' @param scaler.func Function to calculate rescaling factors. 
#' @param expected.catch For conditional logit. Expected catch matrices to include
#' @return Returns a list of datacompile/choice matrix, dataCompile, distance, otherdat, expname, choice, choice.table, and mod.name
#' @importFrom DBI dbConnect dbDisconnect
#' @importFrom RSQLite SQLite
#' @export
#' @keywords internal

create_model_input <- function(project, x=NULL, mod.name=NULL, use.scalers= TRUE, 
                               scaler.func=NULL, expected.catch=NULL){
  
  # Two scenarios 
  # 1) Pulling from discrete fish subroutine  - x will be supplied
  # 2) Pulling specific model
  # Call in datasets
  
  
  if(!is.null(x)){
    x <- x
  } else {
    fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project=project))
    on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
    x_temp <- unserialize(DBI::dbGetQuery(fishset_db, paste0("SELECT ModelInputData FROM ", project, "modelinputdata LIMIT 1"))$ModelInputData[[1]])
    if(!is.null(mod.name)){
      x <- x_temp[[which(lapply( x_temp , "[[" , "mod.name" ) == mod.name)]]
    } else {
      x <- NULL
    }
  } 
  
  if(is.null(x)) { stop("Model name is not defined.")}
  
  if(x$likelihood == "logit_c"){
    if(is.null(expected.catch)){
      stop('Expected catch matrix required but not defined. Model not run.')
    }
  }
  
  #Get scalers
  if(use.scalers == TRUE && !is.null(scaler.func)){
    if(!is.character(scaler.func)){
      
      x$scales$catch <- scaler.func(x[["catch"]])
      x$scales$zonal <-  scaler.func(x[["distance"]])
      x$scales$griddata <-  scaler.func(x[["bCHeader"]][["gridVariablesInclude"]])
      if(is.list(x[["bCHeader"]][["indeVarsForModel"]])){
        x$scales$intdata <- lapply(x[["bCHeader"]][["indeVarsForModel"]], function(x) scaler.func(unlist(x)))
      } else {
        x$scales$intdata <- scaler.func(x[["bCHeader"]][["indeVarsForModel"]]) 
      }
      #if(is.list(expected.catch)){
      #  x$scales$expdat <- lapply(expected.catch, function(x) scaler.func(unlist(x)))
     # } else {
     #   x$scales$expdat <- scaler.func(expected.catch) 
     # }
      
    } 
  } else if(use.scalers == FALSE) {
    
    for(k in 1:length(x$scales)) { 
      x$scales[k] <- 1
    }
  }
  
  
  
  catch <- (data.frame(as.matrix(x[["catch"]])))/as.numeric(x$scales[which(names(x$scales)=='catch')])
  choice <- x[["choice"]]
  distance <- (data.frame(x[["distance"]]))/as.numeric(x$scales[which(names(x$scales)=='zonal')])
  startingloc <- x[["startingloc"]]
  mod.name <- unlist(x[["mod.name"]])
  choice.table <- choice #as.matrix(choice, as.numeric(factor(as.matrix(choice))))
  choice <- as.data.frame(as.numeric(factor(as.matrix(choice))))#data.frame(as.matrix(as.numeric(factor(choice))))
  ab <- max(choice) + 1 # no interactions in create_logit_input - interact distances in likelihood function instead
  
  fr <- x[["likelihood"]] # func  #e.g. logit_c
  
  # remove unnecessary lists
 # x[["gridVaryingVariables"]] <- Filter(Negate(function(x) is.null(unlist(x))), x[["gridVaryingVariables"]])
 # x[["gridVaryingVariables"]] <- x[["gridVaryingVariables"]][names(x[["gridVaryingVariables"]]) != "units"]
 # x[["gridVaryingVariables"]] <- x[["gridVaryingVariables"]][names(x[["gridVaryingVariables"]]) != "scale"]
  
  
  
  if (fr == "logit_correction" & all(is.na(startingloc))) {
    warning("Startingloc parameter is not specified. Rerun the create_alternative_choice function")
  }
  dataCompile <- create_logit_input(choice)
  
  d <- shift_sort_x(dataCompile, choice, catch, distance, max(choice), ab)
  
  
  ### Data needs will vary by the likelihood function ###
  if (grepl("epm", fr)) {
    otherdat <- list(
      griddat = list(griddatfin = as.data.frame(x[["bCHeader"]][["gridVariablesInclude"]])/as.numeric(x$scales[which(names(x$scales)=='griddata')])),
      intdat = list(as.data.frame(
        mapply("/",x[["bCHeader"]][["indeVarsForModel"]],
               grep('intdata', names(x$scales)),SIMPLIFY = FALSE))),
      pricedat = as.data.frame(x[["epmDefaultPrice"]]/as.numeric(x$scales[which(names(x$scales)=='pscale')]))
    )
    expname <- fr
  } else if (fr == "logit_correction") {
    otherdat <- list(
      griddat = list(griddatfin = data.frame(rep(1, nrow(choice)))), # x[['bCHeader']][['gridVariablesInclude']]),
      intdat = list(as.data.frame(mapply("/",x[["bCHeader"]][["indeVarsForModel"]],
                                         grep('intdata', names(x$scales)),SIMPLIFY = FALSE))),
      startloc = as.data.frame(x[["startloc"]]),
      polyn = as.data.frame(x[["polyn"]]),
      distance=list(distance)
    )
    expname <- fr
  } else if (fr == "logit_avgcat") {
    otherdat <- list(
      griddat = list(griddatfin = data.frame(rep(1, nrow(choice)))),
      intdat = list(as.data.frame(
        mapply("/",x[["bCHeader"]][["indeVarsForModel"]],
               grep('intdata', names(x$scales)),SIMPLIFY = FALSE)))# 
    )
    expname <- fr
  } else if (fr == "logit_c") {
    expname <- paste0(fr, '_', paste0(names(x$gridVaryingVariables), collapse=""))
    otherdat <- list(
      griddat = list(as.data.frame(lapply(expected.catch, function(sub) {sub <- lapply(sub, `/`,  mean(unlist(do.call(cbind,expected.catch))))
      sub}))),
      intdat = list(as.data.frame(
        mapply("/",x[["bCHeader"]][["indeVarsForModel"]],
               grep('intdata', names(x$scales)),SIMPLIFY = FALSE)))
    )
  }
  
  
  out <- list(
    d=d, #datacompile/choice matrix
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


