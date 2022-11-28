#' Make model design file
#'
#' Create a list containing likelihood function, parameters, and data to be pass 
#' to model call function
#'
#' @param project String, name of project.
#' @param catchID  String, variable from `dat` that contains catch data.
#' @param replace Logical, should the model design file be replaced? If `FALSE`, 
#'   appends to existing model design file. Defaults to `TRUE`.
#' @param likelihood String, name of likelihood function. Details on likelihood 
#'   specific initial parameter specification can be found in 
#'   [discretefish_subroutine()] documentation.
#' \tabular{rlll}{
#'  logit_c: \tab  Conditional logit likelihood \cr
#'  logit_avgcat: \tab Average catch multinomial logit procedure \cr
#'  logit_correction: \tab Full information model with Dahl's correction function \cr
#'  epm_normal: \tab  Expected profit model with normal catch function \cr
#'  epm_weibull: \tab Expected profit model with Weibull catch function \cr
#'  epm_lognormal: \tab  Expected profit model with lognormal catch function \cr
#'  }
#' @param initparams Vector or list, initial parameter estimates for 
#'   revenue/location-specific covariates then cost/distance. The number of 
#'   parameter estimate varies by likelihood function. See Details section for 
#'   more information. The initial parameters will be set to `1` if 
#'   `initparams == NULL`. If `initparams` is a single numeric value, it will be
#'   used for each parameter. If using parameter estimates from previous model, 
#'   `initparams` should be the name of the model the parameter estimates 
#'   should come from. Examples: `initparams = 'epm_mod1'`, 
#'   `initparams = list('epm_mod1', 'epm_mod2')`.
#' @param optimOpt  String, optimization options 
#'   (max function evaluations, max iterations, (reltol) tolerance of x, trace)
#'   Note: add optim reference here?.
#' @param methodname String, optimization method (see [stats::optim()] options). 
#'   Defaults to `"BFGS"`.
#' @param mod.name String, name of model run for model result output table.
#' @param vars1  Character string, additional ‘travel-distance’ variables to 
#'   include in the model. These depend on the likelihood. See the Details section 
#'   for how to specify for each likelihood function.
#' @param vars2 Character string, additional variables to include in the model. 
#'   These depend on the likelihood. See the Details section for how to specify 
#'   for each likelihood function. For `likelihood == 'logit_c'`, `vars2` 
#'   should be the name of the gridded table saved to the FishSET Database, and
#'   should contain the string `"GridTableWide"`. See [format_grid()] for details. 
#' @param priceCol Variable in `dat` containing price information. Required 
#'   if specifying an expected profit model for the likelihood (epm_normal, 
#'   epm_weibull, epm_lognormal).
#' @param expectcatchmodels List, name of expected catch models to include in 
#'   model run. Defaults to all models. Each list item should be a string of 
#'   expected catch models to include in a model. For example, 
#'   `list(c('recent', 'older'), c('user'))` would run one model with the 
#'   medium and long expected catch matrices, and one model with just the user-defined 
#'   expected catch matrix. Choices are "recent", "older", "oldest", "logbook", 
#'   "all", and "individual". 
#'   See [create_expectations()] for details on the different models.
#'   Option "all" will run all expected catch matrices jointly. Option "individual" 
#'   will run the model for each expected catch matrix separately. The final 
#'   option is to select one more expected catch matrices to run jointly.
#' @param startloc Variable in `dat` identifying the location when choice 
#'   of where to fish next was made. Required for logit_correction likelihood.
#'   Use the [create_startingloc()] function to create the starting 
#'   location vector.
#' @param polyn Numeric, correction polynomial degree. Required for 
#'   [logit_correction()] likelihood.
#' @param spat A spatial data file containing information on fishery management 
#'   or regulatory zones boundaries. Only required if `alt_var = "nearest point"`
#'   was used in the alternative choice matrix (see [create_alternative_choice()]).
#'   Defaults to `NULL`. This should be the same spatial file used to assign
#'   observations to zones. 
#' @param spatID Variable in `spat` that identifies the individual areas or zones. 
#'   Only required if `alt_var = "nearest point"` was used in the alternative 
#'   choice matrix (see [create_alternative_choice()]). Defaults to `NULL`.
#' @param crs coordinate reference system to be assigned when creating the 
#'   distance matrix. Passed on to [create_dist_matrix()].
#' @importFrom DBI dbGetQuery dbExecute dbListTables
#' @export make_model_design
#' @md
#' @details Function creates the model matrix list that contains the data and 
#'   modeling choices. The model design list is saved to the FishSET database and 
#'   called by the [discretefish_subroutine()]. Alternative fishing 
#'   options come from the Alternative Choice list, generated from the 
#'   [create_alternative_choice()] function, and the expected catch 
#'   matrices from the [create_expectations()] function. The distance 
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
#'   called by the [discretefish_subroutine()]. Alternative fishing 
#'   options come from the `Alternative Choice` list, generated from the 
#'   [create_alternative_choice()] function, and the expected catch 
#'   matrices from the [create_expectations()] function. The distance 
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
#'     by zone such as expected catch (from [create_expectations()] function)
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
           initparams = NULL,
           optimOpt = c(100, 1.0e-08, 1, 1), # tolerance may be low
           methodname = "BFGS",
           mod.name = NULL,
           vars1 = NULL,
           vars2 = NULL,
           priceCol = NULL,
           expectcatchmodels = list('all'),
           startloc = NULL,
           polyn = NULL,
           spat = NULL,
           spatID = NULL,
           crs = NULL) {
    
  # TODO: use formula method for specifying model
  # TODO: standardize arg names: use camel-case or period-case etc.

  fishset_db <- DBI::dbConnect(RSQLite::SQLite(), locdatabase(project = project))
  on.exit(DBI::dbDisconnect(fishset_db), add = TRUE)
  
  if (!table_exists(paste0(project, "MainDataTable_final"), project)) {
    
    stop("Final dataset does not exist. Run check_model_data() to save the final",
         " dataset to the FishSET Database before modeling.")
  } 
    
  dataset <- table_view(paste0(project, "MainDataTable_final"), project)
  
  spat_out <- data_pull(spat, project)
  spatdat <- spat_out$dataset
  spat <- parse_data_name(spat, "spat", project)
  
  # check args ----
  
  if (likelihood == "logit_c") {
    
    column_check(dataset, c(catchID, vars1, priceCol, startloc))
    
    lapply(vars2, function(x) {
      
      if (!table_exists(x, project)) {
        
        stop("Gridded table '", x, "' does not exist.", call. = FALSE)
      }
    })
    
  } else {
    
    column_check(dataset, c(catchID, vars1, vars2, priceCol, startloc))
  }
  
  
  ll_funs <- c("logit_c", "logit_avgcat", "logit_correction", "epm_normal", 
               "epm_lognormal", "epm_weibull")
  
  if (!likelihood %in% ll_funs) {
    
    stop("Invalid likelihood function selected. Options are ", 
         paste0(ll_funs, collapse = ", "), call. = FALSE)
  }
  
  mod_dsn_exists <- table_exists(paste0(project, "ModelInputData"), project)
  
  if (mod_dsn_exists) mod_nms <- model_names(project)
  
  if (is_value_empty(mod.name)) {
    
    mod.name <- likelihood
    
    if (mod_dsn_exists) {
      
      if (mod.name %in% mod_nms) {
        
        n_nms <- sum(grepl(mod.name, mod_nms))
        
        mod.name <- paste0(mod.name, "_", n_nms)
      }
    }
    
  } else {
    
    if (mod_dsn_exists) {
      
      if (mod.name %in% mod_nms) {
        
        stop("Model name '", mod.name, "' exists. Enter a unique name. Current ",
             "model names are: ", paste0(model_names(project), collapse = ", "), 
             call. = FALSE)
      }
      
      file_nm_check(mod.name)
    }
  }
    
  # parameter setup ----
  # Script necessary to ensure parameters generated in shiny app are in correct format
  if (is_value_empty(vars1) || "none" %in% vars1) {
    
    indVars <- NULL
    
  } else {
    # TODO: find better way to specify vars1 and vars2 (formula method?)
    if (any(grepl(',', vars1))) {
      
      indVars <- unlist(strsplit(vars1, ","))
      
    } else {
      
      indVars <- vars1
    }
  }
    
  if (is_value_empty(vars2) || "none" %in% vars2) {
    
    gridVars <- NULL
    
  } else {
    
    if (any(grepl(',', vars2))) {
      
      gridVars <- unlist(strsplit(vars2, ","))
      
    } else {
      
      gridVars <- vars2
    }
  }
    
  # TODO: Standardize "none" or NULL option from shiny app
  if (is_value_empty(priceCol) || priceCol == "none") {
    
    priceCol <- NULL
  }
    
  if (is_value_empty(startloc) || startloc == "none") {
    
    startloc <- NULL
  } 
 
  # Alt choice ----
  # get alt choice list
  if (table_exists(paste0(project, "altmatrix"), project)) {
    
    Alt <- unserialize_table(paste0(project, "altmatrix"), project)
    
  } else {
    
    stop("Alternative Choice Matrix does not exist. Please run the ", 
         "create_alternative_choice() function.", call. = FALSE)
  }
    
  alt_var <- Alt$alt_var
  occasion <- Alt$occasion
  occasion_var <- Alt$occasion_var
  dataZoneTrue <- Alt$dataZoneTrue
  zone_ind <- which(dataZoneTrue == 1)
  
  zone_cent <- Alt$zone_cent # zonal centroid table
  fish_cent <- Alt$fish_cent # fishing centroid table
  choice_raw <- Alt$choice # as.data.frame(Alt$choice)
  choice <- Alt$choice[zone_ind] # s.data.frame(Alt$choice[zone_ind])
  alts <- length(unique(choice))
  zoneRow <- Alt$zoneRow
  zoneID <- Alt$zoneID
  
  # startingloc ----
  if (is_value_empty(startloc)) {
    
    start_loc <- rep(NA, nrow(dataset))
    
  } else {
    
    start_loc <- dataset[[startloc]]
  }
  
  units <- Alt$altChoiceUnits

  # Expected catch ----
  
  ExpectedCatch <- NULL
  exp_select <- NULL
  # TODO: Check whether ec matrices need to be rerun (necessary if primary data was filtered after ec were created)
  # use for logit_avgcat, others?   
  # if (table_exists(paste0(project, "ExpectedCatch"), project) & likelihood == "logit_c") {
  if (!is_value_empty(expectcatchmodels)) {
    
    if (!table_exists(paste0(project, "ExpectedCatch"), project)) {
      
      stop("Expected catch/revenue does not exist. Run create_expectations() ",
           "or set 'expectcatchmodels = NULL'.", call. = FALSE)
    }
    
    ExpectedCatch <- unserialize_table(paste0(project, "ExpectedCatch"), project)
      
    if (nrow(ExpectedCatch$user_exp) != length(choice)) {
      
      stop('Number of observations in Expected catch matrix and catch data do not ',  
           'match. Model design file cannot be created.', call. = FALSE)
      
    } else {
      
      if (is_value_empty(expectcatchmodels)) {
        
        stop('Expected catch matrix not defined. Model design file cannot be created.',
             call. = FALSE)
        
      } else {
        # Note: allow multiple user created ec matrices, named, select by name 
        # prepare the ec list and expectcatchmodels for model
        exp_out <- check_exp(ec = ExpectedCatch, ec_names = expectcatchmodels)
        
        ExpectedCatch <- exp_out$exp
        exp_select <- exp_out$exp_select
      }
    }
  }
    
  # Note: revisit this -- EC should be available for other likelihood funs
  if (is.null(ExpectedCatch)) {
    
    userDumV <- 1
    
    if (likelihood == "logit_c") {
      
      stop("Expected Catch Matrix does not exist. Please run the create_expectations ", 
           "function if expected catch will be included in the model.", call. = FALSE)
    }
    
  } else {
    # Note: do this in calc_exp()? 
    if (is.null(ExpectedCatch$user_dummy)) userDumV <- 1 # Note: consider removing this
    else userDumV <- ExpectedCatch$user_dummy
  }

  # Port ----  
  
  if (occasion == "port") {
    
    # check if port table needs to be merged to primary table
    if (length(occasion_var) == 1) { # port ID variable
      
      pt <- data_pull(paste0(project, 'PortTable'), project)
      ptname <- pt$dat # Note: ptname not used 
      port <- pt$dataset # used in create_distance_matrix()
      
    # } else {
    #  
    #   # update error msg
    #   stop("Port table not found in database. Check spelling and ensure port table ", 
    #        "is loaded into the FishSET database.", call. = FALSE)
    # }
    
    } else {
      
      ptname <- NULL
      port <- NULL
    }
  }
  
  # Gridded ----
  # Note: create_alternative_choice() currently cannot create a dm from a 
  # gridded dataset
  

  if (is_value_empty(gridVars)) {
    
    if (is_value_empty(expectcatchmodels)) {
      
      gridVariablesInclude <- as.data.frame(matrix(1, nrow = length(choice), ncol = 1))
      
    } else gridVariablesInclude <- NULL
    
  } else {
    
    if (likelihood == "logit_c") {

      # TODO: check if gridded table has correct # of rows, if not error out and 
      # tell user to re-run format_grid()
      gridVariablesInclude <- lapply(gridVars, function(x) {
        
        grid_tab <- table_view(x, project)
        
        grid_tab[zone_ind, names(grid_tab) %in% unique(choice)]
      })
      
    } else {
      
      gridVariablesInclude <- lapply(gridVars, function(x) dataset[[x]][zone_ind])
    }
    
    names(gridVariablesInclude) <- gridVars
  }
  
  # Ind ----
  if (is_value_empty(indVars)) {
    
    indeVarsForModel <- as.data.frame(matrix(1, nrow = length(choice), ncol = 1))
    
  } else {
    
    if (any(indVars %in% c("Miles * Miles", "Miles*Miles", "Miles x Miles"))) {
    
    indeVarsForModel <- lapply(indVars[-1], function(x) dataset[[x]][zone_ind])
    
    } else {
      
      indeVarsForModel <- lapply(indVars, function(x) dataset[[x]][zone_ind])
    }
    
    names(indeVarsForModel) <- indVars
  }
  
  bCHeader <- list(units = units, gridVariablesInclude = gridVariablesInclude, 
                   userDumV = userDumV, indeVarsForModel = indeVarsForModel)
 
  # Initial parameters ----
  # need to grab inits from previous model run if required
  # TODO: use better method for reading in existing initial parameters
 
  # set parameters
  
  init_params <- initparams
  
  if (is_value_empty(initparams)) {
    
    if (!is.numeric(initparams) & !any(grepl(',', initparams))) {
      # read in parameters from previous model
      x_temp <- read_dat(paste0(locoutput(project),  
                                pull_output(project, type = 'table', 
                                            fun = paste0('params_', initparams))))
      
      if (!is.null(x_temp)) {
        
        init_params <- x_temp$estimate
        
      } else {
        
        init_params <- 1
        warning('Model not found. Setting parameter estimates to 1.', call. = FALSE)
      }
      
    } else init_params <- 1
  }
    
  # Distance Matrix ----
  dist_out <- create_dist_matrix(dataset = dataset, spat = spatdat,
                                 spatID = spatID,  alt_var = alt_var, 
                                 occasion = occasion, occasion_var = occasion_var,
                                 dataZoneTrue = dataZoneTrue, zone_cent = zone_cent, 
                                 fish_cent = fish_cent, choice = choice_raw, 
                                 units = units, port = port, zoneRow = zoneRow, 
                                 zoneID = zoneID, crs = crs)
  
  if (is.null(dist_out)) {
    
    stop('Model design failed. Error in calculating distance matrix', 
         call. = FALSE)
    
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
      
      epmDefaultPrice <- dataset[[priceCol]][zone_ind]
      pscale <- mean(epmDefaultPrice, na.rm = TRUE)
    }
  
    # scales ----
    mscale <- mean(dist_out$distMatrix, na.rm = TRUE)
  
    # scales data r in
    # Note: this should be done before line 419 else first condition can't be TRUE
    # if (length(bCHeader$gridVariablesInclude) == 0) r <- 1
    if (is_value_empty(gridVars)) r <- 1
    else {
      
      r <- as.numeric(lapply(bCHeader$gridVariablesInclude, 
                             function(x) mean(as.numeric(unlist(x)), na.rm = TRUE)))
    }
    
    # if (length(bCHeader$indeVarsForModel) == 0) { 
    if (is_value_empty(indVars)) { 
      
      r2 <- 1 
        
    } else {
      
      r2 <- as.numeric(lapply(bCHeader$indeVarsForModel, 
                              function(x) mean(as.numeric(unlist(x)), na.rm = TRUE)))
    }
  
    # model design list ----
    
    # Note: bCHeader includes GridVariablesInclude (doc says it doesn't include grid varying vars)
    # gridVaryingVariables only contains ec matrices
    
    modelInputData_tosave <- list(
      likelihood = likelihood,
      catch = catch,
      choice = as.data.frame(choice), # consider leaving as vector
      initparams = init_params, 
      optimOpt = optimOpt, 
      methodname = methodname, 
      mod.name  = mod.name,
      mod.date = Sys.time(),
      startingloc = start_loc[zone_ind],
      scales = c(catch = yscale, zonal = mscale, griddata = r, 
                 intdata = r2, pscale = pscale),
      distance = dist_out$distMatrix,
      instances = nrow(dist_out$distMatrix),
      alts = ncol(dist_out$distMatrix),
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
  
    single_sql <- paste0(project, "ModelInputData")
    date_sql <- paste0(project, "ModelInputData", format(Sys.Date(), format = "%Y%m%d"))
    
    if (table_exists(single_sql, project) & replace == FALSE) {
      
      ModelInputData <- unserialize_table(paste0(project, "ModelInputData"), project)
      ModelInputData[[length(ModelInputData) + 1]] <- modelInputData_tosave
      
    } else {
      
      ModelInputData <- list()
      ModelInputData[[length(ModelInputData) + 1]] <- modelInputData_tosave
    }
    
    if (table_exists(single_sql, project)) table_remove(single_sql, project)
    
    if (table_exists(date_sql, project)) table_remove(date_sql, project)
    
  
    DBI::dbExecute(fishset_db, paste("CREATE TABLE IF NOT EXISTS", single_sql, "(ModelInputData MODELINPUTDATA)"))
    DBI::dbExecute(fishset_db, paste("INSERT INTO", single_sql, "VALUES (:ModelInputData)"),
                   params = list(ModelInputData = list(serialize(ModelInputData, NULL)))
    )
    DBI::dbExecute(fishset_db, paste("CREATE TABLE IF NOT EXISTS", date_sql, "(ModelInputData MODELINPUTDATA)"))
    DBI::dbExecute(fishset_db, paste("INSERT INTO", date_sql, "VALUES (:ModelInputData)"),
                   params = list(ModelInputData = list(serialize(ModelInputData, NULL)))
    )
    
    make_model_design_function <- list()
    make_model_design_function$functionID <- "make_model_design"
    make_model_design_function$args <- list(
      project, catchID, replace,  likelihood,initparams, optimOpt, 
      methodname, as.character(mod.name), vars1, vars2, priceCol,
      expectcatchmodels, startloc, polyn, spat, spatID, crs
    )
    make_model_design_function$kwargs <- list()
    
    log_call(project, make_model_design_function)
    
    message('Model design file done')
  }
}

