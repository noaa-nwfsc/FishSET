#' Linear Model for Catch
#' 
#' First stage regression model for catch. Can be used to 
#' 
#' @param dat Primary data containing information on hauls or trips. Table in FishSET 
#'   database contains the string 'MainDataTable'.
#' @param project String, name of project.
#' @param catch.formula A formula object specifying the linear model. See
#'   [stats::lm()].
#' @param zoneID zone ID Variable in `dat` that identifies the individual zones or 
#'   areas.
#' @param exp.name Name(s) of expected catch matrix to merge into `dat`.
#' @param new.name Optional, new name for `exp.name`. These should be in the same
#'   order as `exp.name`. `catch.formula` must use new names. 
#' @param output Whether to output `dat` with a single variable added (`'variable'`)
#'   or 
#' @md
#' @export
#' @importFrom stats lm
#' @importFrom rlang expr parse_expr

catch_lm <- function(dat, project, catch.formula, zoneID = NULL, exp.name = NULL, 
                     new.name = NULL, output = 'ec matrix') {

  # call in dataset
  out <- data_pull(dat, project = project)
  dataset <- out$dataset
  dat <- parse_data_name(dat, "main", project)
   
  if (!is.null(exp.name)) {
    
    # get expected catch matrices (will throw error if ec list doesn't exist)
    ecl <- expected_catch_list(project)
    
    # if (is.null(new.name)) new.name <- exp.name
    
    for (i in seq_along(exp.name)) {
      
      # add each ec matrix as a column to dat
      dat <- merge_expected_catch(dat, project, zoneID, 
                                  exp.name = exp.name[i], 
                                  # new.name = new.name[i], 
                                  new.name = NULL, 
                                  log_fun = FALSE)
    }
  }
  
  # specify linear model
  # TODO: save lm output
    
  lm_dat <- rlang::parse_expr(dat)
  lm_call <- rlang::expr(stats::lm(!!catch.formula, data = !!lm_dat)) # include ... for added options?
  
  catch_mod <- eval(lm_call)
  
  print(summary(catch_mod))
  
  cv <- all.vars(catch.formula)
  
  if (output == 'ec matrix') {
    
    zoneL <- is.null(zoneID) || !zoneID %in% cv
    
    if (is.null(exp.name) & zoneL) {
      
      stop('An expected catch matrix must be used ("exp.name") or a zone/area ', 
           'term must be used in linear model ("zoneID") for output = "ec matrix"', 
           call. = FALSE)
    }
    
    # list of referenced ec matrices
    catch_mat <- ecl[exp.name]
    nz <- ncol(catch_mat[[1]])
    
    # create a list where each entry is a data.frame containing a single zone from 
    # each chosen ec matrix, e.g. list 1 = zone 1 from exp matrices 1 and 2,  etc.
    ec_cols <- lapply(seq_len(nz), function(i) {
      
      ec_i <- lapply(seq_along(exp.name), function(j) catch_mat[[j]][, i])
      names(ec_i) <- new.name
      as.data.frame(ec_i)
    })
    
    # use catch model to estimate expected catch for each zone
    ec_pred <- lapply(seq_len(nz), function(j) {
      
      # original data w/ response and ec variable removed
      dat.x <- dat[cv[!cv %in% new.name & cv != cv[1]]]
      # add ec variables from ec_cols
      dat.x <- cbind(dat.x, ec_cols[[j]])
      
      predict(catch_mod, newdata = dat.x)
    })
    
    names(ec_pred) <- colnames(catch_mat[[1]])
    ec_matrix <- as.matrix(as.data.frame(ec_pred))
    
    # TODO: assign row names using day
    
    # TODO: save to ec list using new.name, add message. 
    
  } else {
    
    dat[[new.name]] <- catch_mod$fitted.values
    return(dat)
  }
  
  # TODO: log function
}
