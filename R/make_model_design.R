#' Make model design file
#'
#' Create a list containing likelihood function, parameters, and data to be pass to model call function
#'
#' @param project String, name of project.
#' @param catchID  String, variable from \code{dat} that contains catch data.
#' @param replace Logical, should the model design file be replaced? If false, appends to existing model design file. 
#'   Defaults to TRUE.
#' @param likelihood String, name of likelihood function. Details on likelihood specific initial parameter specification 
#'   can be found in \code{\link{discretefish_subroutine}} documentation.
#' \tabular{rlll}{
#'  logit_c: \tab  Conditional logit likelihood  \cr
#'  logit_avgcat: \tab Average catch multinomial logit procedure \cr
#'  logit_correction: \tab Full information model with Dahl's correction function  \cr
#'  epm_normal:  \tab  Expected profit model with normal catch function \cr
#'  epm_weibull: \tab Expected profit model with Weibull catch function \cr
#'  epm_lognormal: \tab  Expected profit model with lognormal catch function  \cr
#'  }
#' @param initparams String or list, initial parameter estimates for revenue/location-specific covariates then cost/distance.
#'   The number of parameter estimate varies by likelihood function. See Details section for more information.
#'   If using parameter estimates from previous model,\code{initparams} should be the name of the model the 
#'   parameter estimates should come from. Examples: \code{initparams = 'epm_mod1'}, \code{initparams=list('epm_mod1', 'epm_mod2')}
#' @param optimOpt  String, optimization options [max function evaluations, max iterations, (reltol) tolerance of x, trace].
#' @param methodname String, optimization method (see \code{\link[stats]{optim}} options). Defaults to \code{"BFGS"}.
#' @param mod.name String, name of model run for model result output table.
#' @param vars1  Character string, additional ‘travel-distance’ variables to include in the model. 
#'   These depend on the likelihood. See the Details section for how to specify for each likelihood function.
#' @param vars2 Character string, additional variables to include in the model. These depend on the likelihood.
#'   See the Details section for how to specify for each likelihood function.
#' @param priceCol Variable in \code{dat} containing price information. Required if specifying an expected profit model 
#'   for the likelihood (epm_normal, epm_weibull, epm_lognormal).
#' @param startloc Variable in \code{dat} identifying the location when choice of where to fish next was made.
#'   Required for logit_correction likelihood.
#'   Use the \code{\link{create_startingloc}} function to create the starting location vector.
#' @param polyn Numeric, correction polynomial degree.  Required for logit_correction likelihood.
#' @importFrom geosphere distm
#' @importFrom DBI dbGetQuery dbExecute dbListTables
#' @export make_model_design
#' @details Function creates the model matrix list that contains the data and modeling choices.
#'   The model design list is saved to the FishSET database and called by the
#'   \code{\link{discretefish_subroutine}}. Alternative fishing options come from the
#'   Alternative Choice list, generated from the \code{\link{create_alternative_choice}} function,
#'   and the expected catch matrices from the \code{\link{create_expectations}} function.
#'   The distance from the starting point to alternative choices is calculated. \cr\cr
#'   Variable names details: \cr
#' \tabular{lllllll}{
#' \tab \strong{vars1} \tab \strong{vars2} \tab \cr \cr
#' \strong{logit_c:} \tab 
#'     \preformatted{"travel-distance variables" are
#'     alternative-invariant variables that are
#'     interacted with travel distance to form the cost
#'     portion of the likelihood. Each variable name
#'     therefore corresponds to data with dimensions
#'     (number of observations) by (unity), and returns
#'     a single parameter.} \tab 
#'     \preformatted{"alternative-specific variables"
#'     vary across alternatives, e.g. catch rates.
#'     Each variable name therefore corresponds to data
#'     with dimensions (number of observations) by
#'     (number of alternatives), and returns a single
#'     parameter for each variable (e.g. the marginal
#'     utility from catch).} \cr \cr
#' \strong{logit_avgcat:} \tab 
#'     \preformatted{"travel-distance variables" are
#'     alternative-invariant variables that are
#'     interacted with travel distance to form the cost
#'     portion of the likelihood. Each variable name
#'     therefore corresponds to data with dimensions
#'     (number of observations) by (unity), and returns
#'     a single parameter.} \tab 
#'     \preformatted{"average-catch variables" are
#'     alternative-invariant variables, e.g. vessel
#'     gross tonnage. Each variable name therefore
#'     corresponds to data with dimensions (number of
#'     observations) by (unity), and returns (k-1)
#'     parameters where (k) equals the number of
#'     alternatives, as a normalization of parameters
#'     is needed as the probabilities sum to one.
#'     Interpretation is therefore relative to the
#'     first alternative.} \cr \cr
#' \strong{epm_normal:} \tab 
#'     \preformatted{"travel-distance variables" are
#'     alternative-invariant variables that are
#'     interacted with travel distance to form the
#'     cost portion of the likelihood. Each variable
#'     name therefore corresponds to
#'     data with dimensions (number of observations)
#'     by (unity), and returns a single parameter.} \tab 
#'     \preformatted{"catch-function variables" are
#'     alternative-invariant variables that are
#'     interacted with zonal constants to form the
#'     catch portion of the likelihood. Each variable
#'     name therefore corresponds to data with
#'     dimensions (number of observations) by (unity),
#'     and returns (k) parameters where (k) equals
#'     the number of alternatives.} \cr \cr
#' \strong{epm_lognormal:} \tab 
#'     \preformatted{"travel-distance variables" are
#'     alternative-invariant variables that are
#'     interacted with travel distance to form the
#'     cost portion of the likelihood. Each variable
#'     name therefore corresponds to data with
#'     dimensions (number of observations) by (unity),
#'     and returns a single parameter.} \tab 
#'     \preformatted{"catch-function variables" are
#'     alternative-invariant variables that are
#'     interacted with zonal constants to form the
#'     catch portion of the likelihood. Each variable
#'     name therefore corresponds to data with
#'     dimensions (number of observations) by (unity),
#'     and returns (k) parameters where (k) equals
#'     the number of alternatives.} \cr \cr
#' \strong{epm_weibull:} \tab 
#'     \preformatted{"travel-distance variables" are
#'     alternative-invariant variables that are
#'     interacted with travel distance to form the cost
#'     portion of the likelihood. Each variable name
#'     therefore corresponds to data with dimensions
#'     (number of observations) by (unity), and returns
#'     a single parameter.} \tab 
#'     \preformatted{"catch-function variables" are
#'     alternative-invariant variables that are
#'     interacted with zonal constants to form the catch
#'     portion of the likelihood. Each variable name
#'     therefore corresponds to data with dimensions
#'     (number of observations) by (unity), and returns
#'     (k) parameters where (k) equals the number of
#'     alternatives.} \cr \cr
#' \strong{logit_correction:} \tab 
#'     \preformatted{"travel-distance variables" are
#'     alternative-invariant variables that are
#'     interacted with travel distance to form the cost
#'     portion of the likelihood. Each variable name
#'     therefore corresponds to data with dimensions
#'     (number of observations) by (unity), and returns
#'     a single parameter.} \tab
#'     \preformatted{"catch-function variables" are
#'     alternative-invariant variables that are
#'     interacted with zonal constants to form the catch
#'     portion of the likelihood. Each variable name
#'     therefore corresponds to data with dimensions
#'     (number of observations) by (unity), and returns
#'     (k) parameters where (k) equals the number of
#'     alternatives.} \cr \cr
#' }
#' @return
#' Function creates the model matrix list that contains the data and modeling choices. The model design list is saved to the FishSET database and 
#'   called by the \code{\link{discretefish_subroutine}}.
#' Alternative fishing options come from the `Alternative Choice`` list, generated from the \code{\link{create_alternative_choice}} function,
#'  and the expected catch matrices from the \code{\link{create_expectations}}
#' function. The distance from the starting point to alternative choices is calculated. \cr\cr
#'   Model design list: \cr
#'   \tabular{rlll}{
#'     likelihood: \tab Name of likelihood function\cr
#'     choice: \tab Data corresponding to actual zonal choice\cr
#'     catch: \tab Data corresponding to actual zonal catch\cr
#'     scales: \tab Scale vectors to put catch data, zonal data, and other data on same scale\cr
#'     initparms: \tab Initial parameter values\cr
#'     optimOpt: \tab Optimization options\cr
#'     methodname: \tab Optimization method\cr
#'     mod.name: \tab Model name for referencing\cr
#'     distance: \tab Data corresponding to distance\cr
#'     instances: \tab Number of observations\cr
#'     alt: \tab Number of alternative zones\cr
#'     epmDefaultPrice: \tab Price data\cr
#'     dataZoneTrue: \tab Vector of 0/1 indicating whether the data from that zone is to be included based on the minimum number of hauls.\cr
#'     numOfNecessary: \tab Minimum number of hauls/trips per zone for data from that zone to be included\cr
#'     typeOfNecessary: \tab Whether data is at haul or trip level\cr
#'     altChoiceType: \tab Function choice. Set to distance\cr
#'     altChoiceUnits: \tab Units of distance\cr
#'     altToLocal: \tab Identifies how to find lat/lon for starting point. Can be zonal centroid, port, etc\cr
#'     altToLocal2: \tab Identifies how to find lat/lon for alternative choices such as 'Centroid of Zonal Assignment'\cr
#'     bCHeader: \tab Variables to include in the model that do not vary by zone. Includes independent variables and interactions\cr
#'     gridVaryingVariables: \tab Variables to include in the model that do vary by zone such as expected catch (from \code{\link{create_expectations}} function)
#'   }
#' @examples
#' \dontrun{
#' make_model_design("pollock", catchID= "OFFICIAL_TOTAL_CATCH",  
#'   replace=FALSE, likelihood='logit_avgcat', 
#'   vars1=NULL, vars2=NULL, initparams=c(-0.5,0.5),
#'   optimOpt=c(100000, 1.0e-08, 1, 1), methodname = "BFGS", mod.name = "logit4"
#' )
#' }
#'
make_model_design <- function(project, catchID, replace = TRUE, likelihood = NULL, 
                              initparams, optimOpt=c(100000, 1.0e-08, 1, 1), methodname="BFGS", 
                              mod.name=NULL, vars1 = NULL, vars2 = NULL, 
                              priceCol = NULL, startloc = NULL, polyn = NULL) {
  
  fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project = project))
  on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
  
  if (!table_exists(paste0(project, "MainDataTable_final"), project)) {
    
    stop("Final dataset does not exist. Run check_model_data() to save the final",
            " dataset to the FishSET Database before modeling.")
  } 
    
    dataset <- table_view(paste0(project, "MainDataTable_final"), project)
  
  
  if(is.null(dataset[[catchID]])){
    stop('catchID does not exist in dataset. Check spelling.')
  }
  
  # Script necessary to ensure parameters generated in shiny app are in correct format
  if (is_empty(vars1) || vars1 == "none") {
    indeVarsForModel <- NULL
  } else {
    indeVarsForModel <- vars1
  }
  if (is_empty(vars2) || vars2 == "none") {
    gridVariablesInclude <- NULL
  } else {
    gridVariablesInclude <- vars2
  }
  if (is_empty(priceCol) || priceCol == "none") {
    priceCol <- NULL
  } else {
    priceCol <- priceCol
  }
  if (is_empty(startloc) || startloc == "none") {
    startloc <- NULL
  } else {
    startloc <- startloc
  }
  
  if(is_empty(mod.name)) {
    mod.name <- paste0(likelihood, Sys.time())
  } else {
    mod.name <- mod.name
  }
  # lon.dat <- as.character(lon.dat)
  # lat.dat <- as.character(lat.dat)

#  if (any(!is_empty(lonlat))) {
#    if (lonlat[1] == lonlat[2]) {
#      warning("Longitude and Latitude variables are identical.")
#      end <- TRUE
#    }
#  }
  # indeVarsForModel = vars1
  # gridVariablesInclude=vars2

  
  if (!exists("Alt")) {
    if (!exists("AltMatrixName")) {
      Alt <- unserialize(DBI::dbGetQuery(fishset_db, paste0("SELECT AlternativeMatrix FROM ", project, "altmatrix LIMIT 1"))$AlternativeMatrix[[1]])
      if (!exists("Alt")) {
        stop("Alternative Choice Matrix does not exist. Please run the createAlternativeChoice() function.")
      }
    }
  }

  if (table_exists(paste0(project, "ExpectedCatch"), project)) {
    ExpectedCatch <- unserialize(DBI::dbGetQuery(fishset_db, paste0("SELECT data FROM ", project, "ExpectedCatch LIMIT 1"))$data[[1]])
    
    if(dim(as.data.frame(ExpectedCatch[[1]]))[[1]] != length(Alt$choice[which(Alt[["dataZoneTrue"]]==1),])){
      stop('Number of observations in Expected catch matrix and catch data do not match. Model design file cannot be created.')
    }
  }
  if (!exists("ExpectedCatch")) {
    ExpectedCatch <- ""
    newDumV <- 1
    if (likelihood == "logit_c") {
      
      stop("Expected Catch Matrix does not exist. Please run the create_expectations function if expected catch will be included in the model.")
    }
  }

    if(length(ExpectedCatch)>1){
      if (length(ExpectedCatch$newDumV)==0) {
        newDumV <- 1
      } else {
        newDumV <- ExpectedCatch[["newDumV"]]
        # bCHeader <- list(bCHeader, newDumV)
      }
    }
    
    alt_var <- Alt[["alt_var"]]
    occasion <- Alt[["occasion"]]
    dataZoneTrue <- Alt[["dataZoneTrue"]]
    int <- Alt[["int"]]
    choice_raw <- as.data.frame(Alt$choice)
    choice <- as.data.frame(Alt$choice[which(dataZoneTrue==1),])
    zoneRow <- Alt[["zoneRow"]]
    startingloc <- if (!is.null(startloc) & all(is.na(Alt$startingloc))) {
      dataset[[startloc]]
    } else {
      Alt[["startingloc"]]
    }
    units <- Alt[["altChoiceUnits"]]
    
    
    if (!is.null(Alt[["matrix"]])) {
      X <- Alt[["matrix"]]
    } else {
      X <- NULL
    }
    
    if (any(grepl("Port", occasion, ignore.case = TRUE) == T)) {
      pt <- data_pull(paste0(project, 'PortTable'), project)
      if(exists('pt')){
        ptname <- pt$dat
        port <- pt$dataset
      } else {
        "Port table not found in database. Check spelling and ensure port table is loaded into the FishSET database."
      }
    } else {
      ptname <- NULL
      port <- NULL
    }
    

    
    if (is_empty(gridVariablesInclude)||gridVariablesInclude=='NONE'||gridVariablesInclude=='none') {
      gridVariablesInclude <- as.data.frame(matrix(1, nrow = nrow(choice), ncol = 1)) # max(as.numeric(as.factor(unlist(choice))))))
    } else {
      gridVariablesInclude
    }
    

    if (any(is_empty(indeVarsForModel))) {
      bCHeader <- list(units = units, gridVariablesInclude = gridVariablesInclude, newDumV = newDumV, indeVarsForModel = as.data.frame(matrix(1, nrow = nrow(choice), ncol = 1)))
      #    bColumnsWant <- ""
      #    bInterAct <- ""
    } else {
      if (any(indeVarsForModel %in% c("Miles * Miles", "Miles*Miles", "Miles x Miles"))) 
     {
        bCHeader <- list(units = units, gridVariablesInclude = gridVariablesInclude, newDumV = newDumV, indeVarsForModel = lapply(indeVarsForModel[-1], function(x) dataset[[x]][which(dataZoneTrue == 1)]))
      } else {
        bCHeader <- list(units = units, gridVariablesInclude = gridVariablesInclude, newDumV = newDumV, indeVarsForModel = lapply(indeVarsForModel, function(x) dataset[[x]][which(dataZoneTrue == 1)]))
      }
    }
    
  ### Initial parameters - need to grab inits from previous model run if required
    params <- list()
    for (i in 1:length(initparams)){
      if(is.character(initparams[[i]])){
        x_temp <-  read_dat(paste0(locoutput(project), pull_output(project, type='table', fun=paste0('params_', initparams[[i]]))))
        if(!is.null(x_temp)){
            param_temp <- x_temp$estimate
        } else {
          param_temp <- c(1,1,1,1,1)
          warning('Model not found. Setting parameter estimates to 1.')
        }
        params[[length(params) + 1]] <- list(param_temp)
      } else {
        params[[length(params) + 1]] <- list(initparams[[i]])
      }
    }
    
    browser()
  ### Generate Distance Matrix
     dist_out <- create_dist_matrix(dataset=dataset, alt_var=alt_var, occasion=occasion, dataZoneTrue=dataZoneTrue, 
                                 int=int, choice=choice_raw, units=units, port=port, zoneRow=zoneRow, X=X)
  
  ### ---- add special terms: ----### add only for EPM model
 
      catch <- dataset[which(dataZoneTrue == 1), as.vector(catchID)]
      #r=regexp(num2str(max(modelInputData.catch)),'\.','split');
      #r <- nchar(sub("\\.[0-9]+", "", max(catch, na.rm = T)))
      yscale <- mean(catch, na.rm=T) #2*sd(catch, na.rm=TRUE)#10^(r - 1)

        
  ### Some models need price data
      if (is_empty(priceCol) || is.null(priceCol) || priceCol == "") {
        epmDefaultPrice <- ""
      } else {
        epmDefaultPrice <- dataset[which(dataZoneTrue == 1), as.character(priceCol)]
      }
      pscale <- mean(epmDefaultPrice, na.rm=TRUE)
  ### scales zonal
      #r=regexp(num2str(max(max(modelInputData.zonalChoices))),'\.','split');
      
     # r <- nchar(sub("\\.[0-9]+", "", max(max(dist_out[['X']], na.rm = T), na.rm = T)))
      mscale <-mean(dist_out$X, na.rm = TRUE)# 10^(r - 1)

      # scales data r in
      # r <- regexp(arrayfun(@num2str,nanmax(dataPerZone),'UniformOutput',false),'\\.','split')){){
      # dscale <- cellfun(@(x) 10^length(x{1}-1),r)
      #r <- nchar(sub("\\.[0-9]+", "", max(max(dist_out[['X']], na.rm = T), na.rm = T)))

      r <- if(length(bCHeader$gridVariablesInclude)==0){ 1 } else {
                           as.numeric(lapply(bCHeader$gridVariablesInclude, function(x) mean(as.numeric(unlist(x)), na.rm=TRUE)))}
      if(length(bCHeader$indeVarsForModel)==0){ 
          r2 <- 1 
        } else {
          r2 <- as.numeric(lapply(bCHeader$indeVarsForModel, function(x) mean(as.numeric(unlist(x)), na.rm=TRUE)))
        }
      
      dscale <- list(list(grid=r), list(inde=r2))
                 #10^(nchar(sub("\\.[0-9]+", "", max(max(x, na.rm = T), na.rm = T)))-1))
      #dscale <- if(dscale ==0 ) {1} else {dscale}
      ### -- Create output list --- ###
      modelInputData_tosave <- list(
        likelihood = likelihood,
        catch = catch,
        choice = choice,
        initparams = initparams, 
        optimOpt = optimOpt, 
        methodname = methodname, 
        mod.name  = mod.name,
        startingloc = startingloc[which(dataZoneTrue == 1)],
        scales = c(catch = yscale, zonal = mscale, griddata = r, intdata=r2, pscale=pscale),
        distance = dist_out[['X']],
        instances = dim(dist_out[['X']])[1],
        alts = dim(dist_out[['X']])[2],
        epmDefaultPrice = epmDefaultPrice,
        dataZoneTrue <- dataZoneTrue,
        typeOfNecessary = Alt[["zoneType"]],
        altChoiceType = dist_out[['altChoiceType']],
        altChoiceUnits = dist_out[['altChoiceUnits']],
        altToLocal1 = dist_out[['altToLocal1']],
        altToLocal2 = dist_out[['altToLocal2']],
        bCHeader = bCHeader,
        startloc = startloc,
        polyn = polyn,
        gridVaryingVariables = ExpectedCatch
      )

      
     print(str(modelInputData_tosave)) 
     
      single_sql <- paste0(project, "modelinputdata")
      date_sql <- paste0(project, "modelinputdata", format(Sys.Date(), format = "%Y%m%d"))
      if (table_exists(single_sql, project) & replace == FALSE) {
        # modelInputData <- table_view()
        modelInputData <- unserialize(DBI::dbGetQuery(fishset_db, paste0("SELECT ModelInputData FROM ", project, "modelinputdata LIMIT 1"))$ModelInputData[[1]])
        modelInputData[[length(modelInputData) + 1]] <- modelInputData_tosave
      } else {
        modelInputData <- list()
        modelInputData[[length(modelInputData) + 1]] <- modelInputData_tosave
      }
      
      if (table_exists(single_sql, project)) {
        table_remove(single_sql, project)
      }
      if (table_exists(date_sql, project)) {
        table_remove(date_sql, project)
      }
   
      DBI::dbExecute(fishset_db, paste("CREATE TABLE IF NOT EXISTS", single_sql, "(ModelInputData MODELINPUTDATA)"))
      DBI::dbExecute(fishset_db, paste("INSERT INTO", single_sql, "VALUES (:ModelInputData)"),
                     params = list(ModelInputData = list(serialize(modelInputData, NULL)))
      )
      DBI::dbExecute(fishset_db, paste("CREATE TABLE IF NOT EXISTS", date_sql, "(ModelInputData MODELINPUTDATA)"))
      DBI::dbExecute(fishset_db, paste("INSERT INTO", date_sql, "VALUES (:ModelInputData)"),
                     params = list(ModelInputData = list(serialize(modelInputData, NULL)))
      )
      
      
      make_model_design_function <- list()
      make_model_design_function$functionID <- "make_model_design"
      make_model_design_function$args <- list(
        project, catchID, replace, ptname, likelihood,
        initparams, optimOpt, methodname, as.character(mod.name), 
        vars1, vars2, priceCol, startloc, polyn
      )
      make_model_design_function$kwargs <- list()
      
      log_call(project, make_model_design_function)
      
      print('Model design file done')


}

