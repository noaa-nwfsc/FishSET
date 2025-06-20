#' Create model input data
#' 
#' Takes the structure of data compile, price (for EPM), distance, gridvarying, 
#' interaction terms
#' 
#' @param project Project name
#' @param x Optional, Model input data
#' @param mod.name Optional, name of model. Include if building model data for 
#'   specific defined model.
#' @param use.scalers Logical, should data be normalized? Defaults to \code{FALSE}. 
#'   Rescaling factors are the mean of the numeric vector unless specified with 
#'   \code{scaler.func}.
#' @param scaler.func Function to calculate rescaling factors. 
#' @param expected.catch For conditional logit. Expected catch matrices to include
#' @param exp.names The names of expected catch matrices to use in model. Specified
#'   in \code{\link{make_model_design}}. 
#' @return Returns a list of datacompile/choice matrix, dataCompile, distance, 
#'   otherdat, expname, choice, choice.table, and mod.name
#' @importFrom DBI dbConnect dbDisconnect
#' @importFrom RSQLite SQLite
#' @importFrom rlang is_bare_list
#' @export
#' @keywords internal

create_model_input <- function(project,
                               x = NULL,
                               mod.name = NULL,
                               use.scalers = FALSE,
                               scaler.func = NULL,
                               expected.catch = NULL,
                               exp.names = NULL) {
    
    # Two scenarios 
    # 1) Pulling from discrete fish subroutine  - x will be supplied
    # 2) Pulling specific model
    # Call in datasets
    
    if (is.null(x)) {
      
      x_temp <- unserialize_table(paste0(project, "ModelInputData"), project)
      
      if (!is.null(mod.name)) {
        
        x <- x_temp[[which(lapply( x_temp , "[[" , "mod.name") == mod.name)]]
        
      } else {
        
        x <- NULL
      }
    } 
    
    if (is.null(x)) stop("Model name is not defined.", call. = FALSE)
    
    if (x$likelihood == "logit_c" & is.null(expected.catch)) {
      
      stop('Expected catch matrix required but not defined. Model not run.', 
           call. = FALSE)
    }
    
    catch <- x$catch
    choice <- x$choice # Note: is dataframe necessary?
    choice.table <- choice 
    # TODO: fix non-syntactic choice column name (or leave as vector)
    choice <- as.data.frame(as.numeric(factor(as.matrix(choice))))
    # Note: ab is # of cost parameters + # of alts (shift_sort_x)
    # no interactions in create_logit_input - interact distances in likelihood function instead
    ab <- max(choice) + 1 
    distance <- data.frame(x$distance)
    startingloc <- x$startingloc
    mod.name <- unlist(x$mod.name)
    fr <- x$likelihood
    
    # TODO: updates scaler section so that 
    if (use.scalers) {
      
      if (is.null(scaler.func)) {
        
        scaler.func <- mean
      } 
      
      if (is.character(scaler.func)) {
        
        scaler_char <- rlang::parse_expr(scaler.func)
        
        if (is.function(eval(scaler_char))) {
          
          scaler.func <- eval(scaler_char)
          
        } else {
          
          stop("scaler.func is not a function.", call. = FALSE)
        }
        
      } else if (!is.function(scaler.func)) {
        
        stop("scaler.func is not a function.", call. = FALSE)
      }
      
      x$scales['catch'] <- scaler.func(x$catch)
      catch <- catch / as.numeric(x$scales['catch'])
      
      x$scales['zonal'] <- scaler.func(x$distance)
      distance <- distance / as.numeric(x$scales['zonal'])
      
      if (grepl("epm", fr)) {
        
        x$scales['pscale'] <- scaler.func(x$epmDefaultPrice)
        x$epmDefaultPrice <- x$epmDefaultPrice / x$scales['pscale']
      }
      
      if (rlang::is_bare_list(x$bCHeader$gridVariablesInclude)) {
        
        x$scales['griddata'] <- lapply(x$bCHeader$gridVariablesInclude, 
                                       function(x) scaler.func(unlist(x)))
      } else {
        
        x$scales['griddata'] <- scaler.func(x$bCHeader$gridVariablesInclude) 
      }
      
      if (rlang::is_bare_list(x$bCHeader$indeVarsForModel)) {
        
        x$scales['intdata'] <- lapply(x$bCHeader$indeVarsForModel, 
                                      function(x) scaler.func(unlist(x)))
      } else {
        
        x$scales['intdata'] <- scaler.func(x$bCHeader$indeVarsForModel) 
      }
      
      intdat <- mapply("/", x$bCHeader$indeVarsForModel, 
                       x$scales[grep('intdata', names(x$scales))], 
                       SIMPLIFY = FALSE)
      
      griddat <- mapply("/", x$bCHeader$gridVariablesInclude, 
                        x$scales[grep('griddata', names(x$scales))], 
                        SIMPLIFY = FALSE)
      
      # expected catch
      if (!is_value_empty(exp.names)) {
        # use exp.names to pull specified exp matrix
        expected.catch <- expected.catch[exp.names]
        
        expname <- paste0(names(expected.catch), collapse = '.')
        is_dummy <- grepl("_dummy", names(expected.catch))
        
        ec_grid <- lapply(seq_along(expected.catch), function(i) {
          # exclude dummies from scaling
          if (is_dummy[i]) expected.catch[[i]]
          else             expected.catch[[i]] / scaler.func(expected.catch[[i]])
        })
        
        names(ec_grid) <- names(expected.catch)
        # add ec matrices to griddat
        griddat <- c(griddat, ec_grid)
        
      } else expname <- NULL
      
    } else { # Do not scale variables
      
      intdat <- x$bCHeader$indeVarsForModel
      griddat <- x$bCHeader$gridVariablesInclude
      
      if (!is_value_empty(exp.names)) {
        # use exp.names to pull specified exp matrix
        expected.catch <- expected.catch[exp.names]
        expname <- paste0(names(expected.catch), collapse = '.')
        # add ec matrices to griddat
        griddat <- c(griddat, expected.catch)
        
      } else expname <- NULL
    }
    
    if (fr == "logit_correction" & all(is.na(startingloc))) {
      # Note: use stop() instead?
      warning("Startingloc parameter is not specified. 
              Rerun the create_alternative_choice function")
    }
    
    # Flattened identity matrices (alts x alts) in each row, and nrows = number of observations
    dataCompile <- create_logit_input(choice)
    
    # IMPORTANT NOTE: Both choice possibilities AND distances are sorted/shifted even 
    #                 though the column names for distances are not shifted.
    d <- shift_sort_x(x = dataCompile, ch = choice, y = catch, 
                      distance = distance, alts = max(choice), ab = ab)
    
    # Data needs will vary by the likelihood function
    if (grepl("epm", fr)) {
      
      otherdat <- list(
        griddat = griddat,
        intdat = intdat,
        pricedat = x$epmDefaultPrice
      )
      
    } else if (fr == "logit_correction") {
      
      otherdat <- list(
        griddat = griddat,
        intdat = intdat,
        startloc = as.data.frame(x$startloc),
        polyn = as.data.frame(x$polyn),
        distance = list(distance)
      )
      
    } else if (fr == "logit_zonal") {
      
      otherdat <- list(
        griddat = griddat,
        intdat = intdat
      )
      
    } else if (fr == "logit_c") {
      
      otherdat <- list(griddat = griddat, intdat = intdat)
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


