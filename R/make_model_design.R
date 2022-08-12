#' Make model design file
#'
#' Create a list containing likelihood function, parameters, and data to be pass 
#' to model call function
#'
#' @param project String, name of project.
#' @param catchID  String, variable from \code{dat} that contains catch data.
#' @param replace Logical, should the model design file be replaced? If \code{FALSE}, 
#'   appends to existing model design file. Defaults to \code{TRUE}.
#' @param likelihood String, name of likelihood function. Details on likelihood 
#'   specific initial parameter specification can be found in 
#'   \code{\link{discretefish_subroutine}} documentation.
#' \tabular{rlll}{
#'  logit_c: \tab  Conditional logit likelihood \cr
#'  logit_avgcat: \tab Average catch multinomial logit procedure \cr
#'  logit_correction: \tab Full information model with Dahl's correction function \cr
#'  epm_normal: \tab  Expected profit model with normal catch function \cr
#'  epm_weibull: \tab Expected profit model with Weibull catch function \cr
#'  epm_lognormal: \tab  Expected profit model with lognormal catch function \cr
#'  }
#' @param initparams String or list, initial parameter estimates for 
#'   revenue/location-specific covariates then cost/distance. The number of 
#'   parameter estimate varies by likelihood function. See Details section for 
#'   more information. If using parameter estimates from previous model, 
#'   \code{initparams} should be the name of the model the parameter estimates 
#'   should come from. Examples: \code{initparams = 'epm_mod1'}, 
#'   \code{initparams=list('epm_mod1', 'epm_mod2')}
#' @param optimOpt  String, optimization options 
#'   [max function evaluations, max iterations, (reltol) tolerance of x, trace]
#'   Note: add optim reference here?.
#' @param methodname String, optimization method (see \code{\link[stats]{optim}} 
#'   options). Defaults to \code{"BFGS"}.
#' @param mod.name String, name of model run for model result output table.
#' @param vars1  Character string, additional ‘travel-distance’ variables to 
#'   include in the model. These depend on the likelihood. See the Details section 
#'   for how to specify for each likelihood function.
#' @param vars2 Character string, additional variables to include in the model. 
#'   These depend on the likelihood. See the Details section for how to specify 
#'   for each likelihood function.
#' @param priceCol Variable in \code{dat} containing price information. Required 
#'   if specifying an expected profit model for the likelihood (epm_normal, 
#'   epm_weibull, epm_lognormal).
#' @param expectcatchmodels List, name of expected catch models to include in 
#'   model run. Defaults to all models. Each list item should be a string of 
#'   expected catch models to include in a model. For example, 
#'   \code{list(c('recent', 'older'), c('user'))} would run one model with the 
#'   medium and long expected catch matrices, and one model with just the user-defined 
#'   expected catch matrix. Choices are "recent", "older", "oldest", "logbook", 
#'   "all", and "individual". 
#'   See \code{\link{create_expectations}} for details on the different models.
#'   Option "all" will run all expected catch matrices jointly. Option "individual" 
#'   will run the model for each expected catch matrix separately. The final 
#'   option is to select one more expected catch matrices to run jointly.
#' @param startloc Variable in \code{dat} identifying the location when choice 
#'   of where to fish next was made. Required for logit_correction likelihood.
#'   Use the \code{\link{create_startingloc}} function to create the starting 
#'   location vector.
#' @param polyn Numeric, correction polynomial degree. Required for 
#'   \code{\link{logit_correction}} likelihood.
#' @importFrom DBI dbGetQuery dbExecute dbListTables
#' @export make_model_design
#' @details Function creates the model matrix list that contains the data and 
#'   modeling choices. The model design list is saved to the FishSET database and 
#'   called by the \code{\link{discretefish_subroutine}}. Alternative fishing 
#'   options come from the Alternative Choice list, generated from the 
#'   \code{\link{create_alternative_choice}} function, and the expected catch 
#'   matrices from the \code{\link{create_expectations}} function. The distance 
#'   from the starting point to alternative choices is calculated. \cr\cr
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
#' @return Function creates the model matrix list that contains the data and 
#'   modeling choices. The model design list is saved to the FishSET database and 
#'   called by the \code{\link{discretefish_subroutine}}. Alternative fishing 
#'   options come from the `Alternative Choice` list, generated from the 
#'   \code{\link{create_alternative_choice}} function, and the expected catch 
#'   matrices from the \code{\link{create_expectations}} function. The distance 
#'   from the starting point to alternative choices is calculated. \cr\cr
#'   Model design list: \cr
#'   \tabular{rlll}{
#'     likelihood: \tab Name of likelihood function\cr
#'     choice: \tab Data corresponding to actual zonal choice\cr
#'     catch: \tab Data corresponding to actual zonal catch\cr
#'     scales: \tab Scale vectors to put catch data, zonal data, and other data 
#'     on same scale\cr
#'     initparms: \tab Initial parameter values\cr
#'     optimOpt: \tab Optimization options\cr
#'     methodname: \tab Optimization method\cr
#'     mod.name: \tab Model name for referencing\cr
#'     distance: \tab Data corresponding to distance\cr
#'     instances: \tab Number of observations\cr
#'     alt: \tab Number of alternative zones\cr
#'     epmDefaultPrice: \tab Price data\cr
#'     dataZoneTrue: \tab Vector of 0/1 indicating whether the data from that 
#'     zone is to be included based on the minimum number of hauls.\cr
#'     numOfNecessary: \tab Minimum number of hauls/trips per zone for data from 
#'     that zone to be included\cr
#'     typeOfNecessary: \tab Whether data is at haul or trip level\cr
#'     altChoiceType: \tab Function choice. Set to distance\cr
#'     altChoiceUnits: \tab Units of distance\cr
#'     altToLocal: \tab Identifies how to find lat/lon for starting point. Can 
#'     be zonal centroid, port, etc\cr
#'     altToLocal2: \tab Identifies how to find lat/lon for alternative choices 
#'     such as 'Centroid of Zonal Assignment'\cr
#'     bCHeader: \tab Variables to include in the model that do not vary by zone. 
#'     Includes independent variables and interactions\cr
#'     gridVaryingVariables: \tab Variables to include in the model that do vary 
#'     by zone such as expected catch (from \code{\link{create_expectations}} function)
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
make_model_design <-
  
  function(project,
           catchID,
           replace = TRUE,
           likelihood = NULL,
           initparams,
           optimOpt = c(100000, 1.0e-08, 1, 1),
           methodname = "BFGS",
           mod.name = NULL,
           vars1 = NULL,
           vars2 = NULL,
           priceCol = NULL,
           expectcatchmodels = list('all'),
           startloc = NULL,
           polyn = NULL) {
    
  # TODO: use formula method for specifying model
  # TODO: standardize arg names: expectcatchmodels is bad, use camel-case or period-case etc.

  fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project = project))
  on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
  
  if (!table_exists(paste0(project, "MainDataTable_final"), project)) {
    
    stop("Final dataset does not exist. Run check_model_data() to save the final",
            " dataset to the FishSET Database before modeling.")
  } 
    
  dataset <- table_view(paste0(project, "MainDataTable_final"), project)
  
  column_check(dataset, c(catchID, priceCol, startloc))
    
  # parameter setup ----
  # Script necessary to ensure parameters generated in shiny app are in correct format
  if (is_empty(vars1) || vars1 == "none") {
    
    indeVarsForModel <- NULL
    
  } else {
    # TODO: find better way to specify vars1 and vars2 (formula method)
    if (any(grepl(',', vars1))) {
      
      indeVarsForModel <- unlist(strsplit(vars1, ","))
      
    } else {
      
      indeVarsForModel <- vars1
    }
  }
    
  if (is_empty(vars2) || vars2 == "none") {
    
    gridVariablesInclude <- NULL
    
  } else {
    
    if (any(grepl(',', vars2))) {
      
      gridVariablesInclude <- unlist(strsplit(vars2, ","))
      
    } else {
      
      gridVariablesInclude <- vars2
    }
  }
    
  # TODO: Standardize "none" or NULL option from shiny app
  if (is_value_empty(priceCol) || priceCol == "none") {
    
    priceCol <- NULL
  }
    
  if (is_value_empty(startloc) || startloc == "none") {
    
    startloc <- NULL
  } 
  
  if (is_value_empty(mod.name)) {
    
    mod.name <- paste0(likelihood, Sys.time())
  } 
 
  # Alt choice ----
  # get alt choice list
  if (table_exists(paste0(project, "altmatrix"), project)) {
    
    Alt <- unserialize_table(paste0(project, "altmatrix"), project)
    
  } else {
    
    stop("Alternative Choice Matrix does not exist. Please run the ", 
         "create_alternative_choice() function.", call. = FALSE)
  }
    
  alt_var <- Alt[["alt_var"]]
  occasion <- Alt[["occasion"]]
  dataZoneTrue <- Alt[["dataZoneTrue"]]
  zone_ind <- which(dataZoneTrue == 1)
  int <- Alt[["int"]] # centroid table
  # Note: dataframe needed for choice_raw and choice? 
  choice_raw <- as.data.frame(Alt$choice)
  choice <- as.data.frame(Alt$choice[zone_ind, ])
  zoneRow <- Alt[["zoneRow"]]
  zoneID <- Alt[['zoneID']]
  
  startingloc <- if (!is.null(startloc) & all(is.na(Alt$startingloc))) {
    
    # TODO: check if this is right
    dataset[[startloc]]
    
  } else {
    
    Alt[["startingloc"]]
  }
  
  units <- Alt[["altChoiceUnits"]]

  # Expected catch ----
  # TODO: method for specifying exp to include needs to be updated
  # will exclude dummy matrices
  
  exp_select <- list()
    
  if (table_exists(paste0(project, "ExpectedCatch"), project) & likelihood == "logit_c") {
    
    ExpectedCatch <- unserialize_table(paste0(project, "ExpectedCatch"), project)
      
    if (nrow(ExpectedCatch$user_exp) != nrow(choice)) {
      
      stop('Number of observations in Expected catch matrix and catch data do not ',  
           'match. Model design file cannot be created.', call. = FALSE)
      
    } else {
      
      if (is.null(expectcatchmodels)) {
        
        stop('Expected catch matrix not defined. Model design file cannot be created.',
             call. = FALSE)
        
      } else {
        
        # prepare the ec list and expectcatchmodels for model
        exp_out <- check_exp(ec = ExpectedCatch, ec_names = expectcatchmodels)
        
        ExpectedCatch <- exp_out$exp
        exp_select <- exp_out$exp_select
      }
    }
  }
    
    
  if (!exists("ExpectedCatch")) {
    
    ExpectedCatch <- ""
    userDumV <- 1
    
    if (likelihood == "logit_c") {
      
      stop("Expected Catch Matrix does not exist. Please run the create_expectations ", 
           "function if expected catch will be included in the model.", call. = FALSE)
    }
  }

  
  if (is.list(ExpectedCatch)) {
  # Note: do this in calc_exp()? 
    if (is.null(ExpectedCatch$user_dummy)) userDumV <- 1
    else userDumV <- ExpectedCatch$user_dummy
  }
    

  # Port ----  
  # TODO: use less error-prone check for detecting port occasion
  if (any(grepl("Port", occasion, ignore.case = TRUE))) {
    
    pt <- data_pull(paste0(project, 'PortTable'), project)
    # TODO: Use a different check (this will always be TRUE unless data_pull returns an error)
    if (exists('pt')) {
      
      ptname <- pt$dat # Q: what is ptname used for? 
      port <- pt$dataset # used in create_distance_matrix()
      
    } else {
     
      stop("Port table not found in database. Check spelling and ensure port table ", 
           "is loaded into the FishSET database.", call. = FALSE)
    }
    
  } else {
    
    ptname <- NULL
    port <- NULL
  }
  
  
  # Gridded ----
  # TODO: Rename alt matrix (X is too generic)
  if (!is.null(Alt[["matrix"]])) X <- Alt[["matrix"]]
  else X <- NULL
  
 # TODO: need to pull gridded data from FSDB (should var2 be table name?) or 
 # from primary table
 # TODO: check that gridded data is in correct format ()
  if (is_empty(gridVariablesInclude)) {
    
    gridVariablesInclude <- as.data.frame(matrix(1, nrow = nrow(choice), ncol = 1))
  } 


  if (any(is_empty(indeVarsForModel))) {
    
    bCHeader <- list(units = units, gridVariablesInclude = gridVariablesInclude, 
                     userDumV = userDumV, 
                     indeVarsForModel = as.data.frame(matrix(1, nrow = nrow(choice), ncol = 1)))
    
  } else {
    # Note: doc says bCHeader doesn't include grid variables, just IVs and interactions
    if (any(indeVarsForModel %in% c("Miles * Miles", "Miles*Miles", "Miles x Miles"))) {
      
      bCHeader <- list(units = units, gridVariablesInclude = gridVariablesInclude, 
                       userDumV = userDumV, 
                       indeVarsForModel = lapply(indeVarsForModel[-1], function(x) dataset[[x]][zone_ind]))
    } else {
      
      bCHeader <- list(units = units, gridVariablesInclude = gridVariablesInclude, 
                       userDumV = userDumV, 
                       indeVarsForModel = lapply(indeVarsForModel, function(x) dataset[[x]][zone_ind]))
    }
  }
 
  # Initial parameters ----
  # need to grab inits from previous model run if required
  #  params <- list()
  #  for (i in 1:length(initparams)){
  # TODO: use better method for reading in existing initial parameters
  if (!is.numeric(initparams) & !any(grepl(',', initparams))) {
    
    x_temp <- read_dat(paste0(locoutput(project),  
                              pull_output(project, type = 'table', fun = paste0('params_', initparams))))
    
    if (!is.null(x_temp)) {
      
      initparams <- x_temp$estimate
      
    } else {
      # Q: why five? 
      initparams <- c(1, 1, 1, 1, 1) 
      warning('Model not found. Setting parameter estimates to 1.')
    }
  }
 #   }
    
  # Distance Matrix ----
  dist_out <- create_dist_matrix(dataset = dataset, alt_var = alt_var, occasion = occasion, 
                                 dataZoneTrue = dataZoneTrue, int = int, choice = choice_raw, 
                                 units = units, port = port, zoneRow = zoneRow, X = X, 
                                 zoneID = zoneID)
  
  if (is.null(dist_out)) {
    
    stop('Model design failed. Error in calculating distance matrix', call. = FALSE)
    
  } else {

    # add special terms ----
    # add only for EPM model
  
    catch <- dataset[zone_ind, ][[catchID]]
    yscale <- mean(catch, na.rm = TRUE)
     
          
    # Some models need price data
    if (is_value_empty(priceCol)) {
      
      epmDefaultPrice <- ""
      pscale <- 1
      
    } else {
      
      epmDefaultPrice <- dataset[zone_ind, as.character(priceCol)]
      pscale <- mean(epmDefaultPrice, na.rm = TRUE)
    }
  
    # scales zonal ----
    mscale <- mean(dist_out$X, na.rm = TRUE)
  
    # scales data r in
    # Note: this should be done before line 419 else first condition can't be TRUE
    if (length(bCHeader$gridVariablesInclude) == 0) r <- 1
    else {
      
      r <- as.numeric(lapply(bCHeader$gridVariablesInclude, 
                             function(x) mean(as.numeric(unlist(x)), na.rm = TRUE)))
    }
    
    if (length(bCHeader$indeVarsForModel) == 0) { 
      
      r2 <- 1 
        
    } else {
      
      r2 <- as.numeric(lapply(bCHeader$indeVarsForModel, 
                              function(x) mean(as.numeric(unlist(x)), na.rm = TRUE)))
    }
  
    # model design list ----
    modelInputData_tosave <- list(
      likelihood = likelihood,
      catch = catch,
      choice = choice,
      initparams = initparams, 
      optimOpt = optimOpt, 
      methodname = methodname, 
      mod.name  = mod.name,
      startingloc = startingloc[zone_ind],
      scales = c(catch = yscale, zonal = mscale, griddata = r, 
                 intdata = r2, pscale = pscale),
      distance = dist_out[['X']],
      instances = nrow(dist_out[['X']]),
      alts = ncol(dist_out[['X']]),
      epmDefaultPrice = epmDefaultPrice,
      dataZoneTrue = dataZoneTrue,
      typeOfNecessary = Alt[["zoneType"]],
      altChoiceType = dist_out[['altChoiceType']],
      altChoiceUnits = dist_out[['altChoiceUnits']],
      altToLocal1 = dist_out[['altToLocal1']],
      altToLocal2 = dist_out[['altToLocal2']],
      bCHeader = bCHeader,
      startloc = startloc,
      polyn = polyn,
      gridVaryingVariables = ExpectedCatch,
      expectcatchmodels = exp_select
    )
  
    single_sql <- paste0(project, "modelinputdata")
    date_sql <- paste0(project, "modelinputdata", format(Sys.Date(), format = "%Y%m%d"))
    
    if (table_exists(single_sql, project) & replace == FALSE) {
      
      modelInputData <- unserialize_table(paste0(project, "modelinputdata"), project)
      modelInputData[[length(modelInputData) + 1]] <- modelInputData_tosave
      
    } else {
      
      modelInputData <- list()
      modelInputData[[length(modelInputData) + 1]] <- modelInputData_tosave
    }
    
    if (table_exists(single_sql, project)) table_remove(single_sql, project)
    
    if (table_exists(date_sql, project)) table_remove(date_sql, project)
    
  
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
      project, catchID, replace,  likelihood,
      initparams, optimOpt, methodname, as.character(mod.name), 
      vars1, vars2, priceCol,expectcatchmodels, startloc, polyn
    )
    make_model_design_function$kwargs <- list()
    
    log_call(project, make_model_design_function)
    
    message('Model design file done')
  }
}

